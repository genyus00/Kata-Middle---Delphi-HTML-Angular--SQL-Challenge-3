unit ServerContainerUnit1;

interface

uses
  System.SysUtils, System.Classes,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  IPPeerServer, IPPeerAPI, Datasnap.DSAuth, System.IniFiles, System.Generics.Collections;

type
  TServerContainer1 = class(TDataModule)
    DSServer1: TDSServer;
    DSAuthenticationManager1: TDSAuthenticationManager;
    DSServerClass1: TDSServerClass;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure DSAuthenticationManager1UserAuthorize(Sender: TObject;
      EventObject: TDSAuthorizeEventObject; var valid: Boolean);
    procedure DSAuthenticationManager1UserAuthenticate(Sender: TObject;
      const Protocol, Context, User, Password: string; var valid: Boolean;
      UserRoles: TStrings);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FUsers: TDictionary<string,string>; // usuario -> password
    procedure LoadUsersFromINI(const AFileName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function DSServer: TDSServer;
function DSAuthenticationManager: TDSAuthenticationManager;

implementation

{$R *.dfm}

uses
  ServerMethodsUnit1, UFunciones, ULog;

var
  FModule: TComponent;
  FDSServer: TDSServer;
  FDSAuthenticationManager: TDSAuthenticationManager;

function DSServer: TDSServer;
begin
  Result := FDSServer;
end;

function DSAuthenticationManager: TDSAuthenticationManager;
begin
  Result := FDSAuthenticationManager;
end;

constructor TServerContainer1.Create(AOwner: TComponent);
begin
  inherited;
  FUsers := TDictionary<string,string>.Create;
  FDSServer := DSServer1;
  FDSAuthenticationManager := DSAuthenticationManager1;
end;

destructor TServerContainer1.Destroy;
begin
  FUsers.Free;
  inherited;
  FDSServer := nil;
  FDSAuthenticationManager := nil;
end;

procedure TServerContainer1.DataModuleCreate(Sender: TObject);
var
  strpath: string;
begin
  strpath := ExtractFilePath(QuitaEn(GetDllName,'\\?\'));
  SetCurrentDir(strpath);

  // Cargar usuarios desde Auth.ini (archivo en la misma carpeta de la app)
  LoadUsersFromINI('Auth.ini');

  LogDebug('Módulo de servidor creado');
end;

procedure TServerContainer1.LoadUsersFromINI(const AFileName: string);
var
  IniPath: string;
  Ini: TIniFile;
  Keys: TStringList;
  i: Integer;
  keyName, keyVal: string;
begin
  IniPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + AFileName;
  if not FileExists(IniPath) then
  begin
    LogDebug('AVISO: No se encontró Auth.ini en: ' + IniPath + '. Usando autenticación por defecto.');
    Exit;
  end;

  Ini := TIniFile.Create(IniPath);
  Keys := TStringList.Create;
  try
    Ini.ReadSection('Users', Keys);
    for i := 0 to Keys.Count - 1 do
    begin
      keyName := Keys[i];
      keyVal := Ini.ReadString('Users', keyName, '');
      if (keyName <> '') and (keyVal <> '') then
        FUsers.AddOrSetValue(keyName, keyVal);
    end;
    LogDebug(Format('Usuarios cargados desde %s: %d', [IniPath, FUsers.Count]));
  finally
    Keys.Free;
    Ini.Free;
  end;
end;

procedure TServerContainer1.DSServerClass1GetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := ServerMethodsUnit1.TServerMethods1;
end;

procedure TServerContainer1.DSAuthenticationManager1UserAuthenticate(Sender: TObject;
  const Protocol, Context, User, Password: string; var valid: Boolean;
  UserRoles: TStrings);
var
  StoredPass: string;
begin
  valid := False;
  if FUsers.TryGetValue(User, StoredPass) then
  begin
    // Comparación simple; en producción usar hash + salt
    if StoredPass = Password then
      valid := True;
  end;
end;

procedure TServerContainer1.DSAuthenticationManager1UserAuthorize(Sender: TObject;
  EventObject: TDSAuthorizeEventObject; var valid: Boolean);
begin
  // Por ahora si se autenticó, autorizamos.
  valid := True;
end;

initialization
  FModule := TServerContainer1.Create(nil);
finalization
  FModule.Free;
end.
