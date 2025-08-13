{*************************************************************}
{                                                             }
{       Delphi Run-time Library                               }
{       UFunciones                                            }
{       Copyright (c) 2025                                    }
{       Created by Ing. Rodolfo Augusto Jiménez Bastidas      }
{                                                             }
{*************************************************************}

unit UFunciones;

interface

uses
  IniFiles,SysUtils, System.Classes, System.SyncObjs,
  FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Stan.Async,
  FireDAC.DApt, FireDAC.Stan.Param,
  FireDAC.Phys, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  Windows, PsAPI, DateUtils, System.JSON;

const
  {$IFDEF ISAPI}
  ISAPI_NAME = 'ServidorAPI.dll';
  {$ELSE}
  ISAPI_NAME = 'sa_ServidorApp';
  {$ENDIF}

type
  TErrorCode = (
    ecNone,
    ecValidation,
    ecDuplicateKey,
    ecForeignKeyViolation,
    ecInternal
  );

//1. Funciones para ejecutar API's y registro de Windows
function GetDllName: string;
function GetComputerNetName: string;
function GetMemoryUsage: UInt64;
function DateTimeToISO8601Tokyo(ADateTime: TDateTime): string;
function ISO8601TokyoToDateTime(const AISODateTime: string): TDateTime;

//2. Funciones para tratamiento y formateo de cadenas y arreglos
function QuitaEn(Cadena, Esto: string): string;
function CambiaEn(Cadena, Esto, Por: string): string;
function ExtraerMensajeSQL(const AMsg: string): string;
function ManejarErroresDB(const E: Exception; const Accion: string): TJSONObject;
function BuildResponse(CodError: Integer; const Msg: string; Data: TJSONValue = nil): TJSONObject;

//3. Funciones relacionadas con la conexion y consulta a base de datos
function CrearConexionFireDACDesdeINI(const ArchivoINI: string): TFDConnection;

var
  GloParentComponent: TComponent;
  DNSlocal: string;

implementation

uses
  Variants, FireDAC.Stan.Error, ULog;

function GetDllName: string;
var
  szFileName: array[0..MAX_PATH] of Char;
begin
  FillChar(szFileName, SizeOf(szFileName), #0);
  GetModuleFileName(hInstance, szFileName, MAX_PATH);
  Result := szFileName;
end;

function GetComputerNetName: string;
var
  buffer: array[0..255] of char;
  size: dword;
begin
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;

function GetMemoryUsage: UInt64;
var
  pmc: PROCESS_MEMORY_COUNTERS;
begin
  Result := 0;
  pmc.cb := SizeOf(pmc);
  if GetProcessMemoryInfo(GetCurrentProcess, @pmc, pmc.cb) then
    Result := pmc.WorkingSetSize; // bytes usados actualmente en RAM
end;


function DateTimeToISO8601Tokyo(ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"', ADateTime);
end;

function ISO8601TokyoToDateTime(const AISODateTime: string): TDateTime;
var
  Anio, Mes, Dia: Word;
begin
  // Espera formato:  YYYY-MM-DD
  if Length(AISODateTime) < 10 then
    raise Exception.Create('Formato ISO8601 inválido');

  Anio := StrToInt(Copy(AISODateTime, 1, 4));
  Mes  := StrToInt(Copy(AISODateTime, 6, 2));
  Dia  := StrToInt(Copy(AISODateTime, 9, 2));

  Result := EncodeDate(Anio, Mes, Dia);
end;

function QuitaEn(Cadena, Esto: string): string;
var
  aPos: Integer;
begin
  aPos := Pos(Esto, Cadena);
  Result := '';

  while (aPos > 0) do
  begin
    Result := Result + Copy(Cadena, 1, aPos - 1);
    Delete(Cadena, 1, aPos + Length(Esto) - 1);
    aPos := Pos(Esto, Cadena);
  end;
  Result := Result + Cadena;
end;

function CambiaEn(Cadena, Esto, Por: string): string;
var
  aPos: Integer;
begin
  aPos := Pos(Esto, Cadena);
  Result := '';
  while (aPos <> 0) do
  begin
    Result := Result + Copy(Cadena, 1, aPos - 1) + Por;
    Delete(Cadena, 1, aPos + Length(Esto) - 1);
    aPos := Pos(Esto, Cadena);
  end;
  Result := Result + Cadena;
end;

function ExtraerMensajeSQL(const AMsg: string): string;
var
  PosSQL: Integer;
begin
  // Busca el último corchete de cierre
  PosSQL := LastDelimiter(']', AMsg);
  if PosSQL > 0 then
    Result := Trim(Copy(AMsg, PosSQL + 1, MaxInt))
  else
    Result := AMsg;
end;

function ManejarErroresDB(const E: Exception; const Accion: string): TJSONObject;
var
  MsgLower: string;
begin
  MsgLower := LowerCase(E.Message);

  // Clave duplicada
  if Pos('duplicate key', MsgLower) > 0 then
  begin
    if Accion = 'INSERT' then
      Exit(BuildResponse(100, 'No se puede insertar, ya existe un registro con el mismo identificador.'))
    else if Accion = 'UPDATE' then
      Exit(BuildResponse(101, 'No se puede actualizar, ya existe otro registro con el mismo identificador.'));
  end;

  // Violación de FK al insertar/actualizar
  if (Pos('foreign key constraint', MsgLower) > 0) and (Accion <> 'DELETE') then
  begin
    if Accion = 'INSERT' then
      Exit(BuildResponse(102, 'No se puede insertar, el caficultor especificado no existe.'))

    else if Accion = 'UPDATE' then
      Exit(BuildResponse(103, 'No se puede actualizar, el caficultor especificado no existe.'));
  end;

  // Restricción REFERENCE en DELETE
  if (Pos('reference constraint', MsgLower) > 0) and (Accion = 'DELETE') then
    Exit(BuildResponse(104, 'No se puede eliminar porque el registro tiene datos asociados.'));

  // Error de sintaxis
  if Pos('syntax error', MsgLower) > 0 then
    Exit(BuildResponse(105, 'Error de sintaxis en la consulta SQL.'));

  // Columna no existente
  if Pos('invalid column name', MsgLower) > 0 then
    Exit(BuildResponse(106, 'Se está intentando acceder a una columna que no existe.'));

  // Mensaje genérico si no coincidió ningún patrón
  Exit(BuildResponse(999, 'Error en operación ' + Accion + ': ' + ExtraerMensajeSQL(E.Message)));
end;

function BuildResponse(CodError: Integer; const Msg: string; Data: TJSONValue = nil): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('CodError', TJSONNumber.Create(CodError));

  if CodError = 0 then
    Result.AddPair('message', Msg)
  else
    Result.AddPair('ErrorMsg', Msg);

  if Assigned(Data) then
    Result.AddPair('data', Data);
end;

function CrearConexionFireDACDesdeINI(const ArchivoINI: string): TFDConnection;
var
  IniPath: string;
  Ini: TIniFile;
  Conn: TFDConnection;
  Server, Database, User, Password: string;
begin
  Result := nil;
  IniPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + ArchivoINI;

  if not FileExists(IniPath) then
  begin
    LogDebug('ERROR: No se encontró el archivo INI: ' + IniPath);
    Exit;
  end;

  Ini := TIniFile.Create(IniPath);
  try
    Server   := Ini.ReadString('MSQLServer', 'Server', '');
    Database := Ini.ReadString('MSQLServer', 'DataBase', '');
    User     := Ini.ReadString('MSQLServer', 'User', '');
    Password := Ini.ReadString('MSQLServer', 'Password', '');

    if Server.IsEmpty or Database.IsEmpty then
    begin
      LogDebug('ERROR: Parámetros incompletos en el INI.');
      Exit;
    end;

    Conn := TFDConnection.Create(nil);
    try
      Conn.LoginPrompt := False;
      Conn.Params.Clear;
      Conn.Params.DriverID := 'MSSQL';
      Conn.Params.Values['Server']   := Server;
      Conn.Params.Values['Database'] := Database;
      Conn.Params.Values['User_Name'] := User;
      Conn.Params.Values['Password'] := Password;

      Conn.Connected := True;

      Result := Conn;
      LogDebug('Conexión exitosa a la base de datos.');
    except
      on E: Exception do
      begin
        LogDebug('Error al conectar: ' + E.Message);
        Conn.Free;
      end;
    end;
  finally
    Ini.Free;
  end;
end;


end.
