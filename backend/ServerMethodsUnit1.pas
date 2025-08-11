unit ServerMethodsUnit1;

interface

uses  System.SysUtils, System.Classes, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.JSON,
  FireDAC.Comp.Client, Data.DB, IPPeerClient;

type
{$METHODINFO ON}
  TServerMethods1 = class(TDataModule)
  private
    { Private declarations }

  public
    { Public declarations }
    // Función de prueba
    function TestConnection: TJSONObject;

    function updateCaficultor(Adata: TJSONObject): TJSONObject;// Método POST para crear nuevo caficultor
    function GetCaficultores: TJSONObject;// Método GET para obtener todos los caficultores
    function acceptCaficultor(Id: Integer; AData: TJSONObject): TJSONObject;// Método PUT para actualizar caficultor
    function CancelCaficultor(Id: Integer): TJSONObject;// Método DELETE para eliminar caficultor

    function UpdateAbonoMonedero(AData: TJSONObject): TJSONObject;
    function GetAbonosMonedero: TJSONObject;
    function acceptAbonoMonedero(Id: Integer; AData: TJSONObject): TJSONObject;
    function CancelAbonoMonedero(Id: Integer): TJSONObject;

    function UpdateProductoCliente(AData: TJSONObject): TJSONObject;
    function GetProductosCliente: TJSONObject;
    function acceptProductoCliente(Id: Integer;AData: TJSONObject): TJSONObject;
    function CancelProductoCliente(Id: Integer): TJSONObject;

    function GetAbonosFiltrados(id_caficultor: Integer; fecha_inicio, fecha_fin: string): TJSONObject;
    function GetProductosByCaficultor(id_caficultor: Integer): TJSONObject;
    function updateRegistrarAbono(AData : TJSONObject): TJSONObject;
    function GetSaldoMonedero(id_caficultor: Integer): TJSONObject;

  end;
{$METHODINFO OFF}

implementation

{$R *.dfm}

uses
  FireDAC.Stan.Param,
  Datasnap.DSCommonServer,   // Para GetInvocationMetaData
  Datasnap.DSHTTPCommon,     // Para TDSHTTPRequest
  uDBGlobal, UFunciones, System.Variants, FireDAC.Stan.Error;

{ TServerMethods1 }
function TServerMethods1.TestConnection: TJSONObject;
var
  LData: TJSONObject;
  LConexionBD: string;
begin
  LData := TJSONObject.Create;
  try
    // Fecha y hora
    LData.AddPair('fechaHora', DateTimeToISO8601Tokyo(Now));

    // Verificación de conexión a BD
    if Assigned(GlobalConn) then
    begin
      if GlobalConn.Connected then
        LConexionBD := 'Conectada'
      else
        LConexionBD := 'Desconectada';
    end
    else
      LConexionBD := 'No configurada';

    LData.AddPair('conexionBD', LConexionBD);

    // Información adicional útil
    LData.AddPair('memoriaUsadaKB', TJSONNumber.Create(GetMemoryUsage / 1024));

    LogDebug('Prueba de conexión ejecutada');

    // Usamos BuildResponse para el formato estándar
    Result := BuildResponse(0, 'Prueba de conexión exitosa', LData);
  except
    on E: Exception do
    begin
      FreeAndNil(LData); // Libera el JSON si hubo error
      LogDebug('Error en TestConnection: ' + E.Message);
      Result := ManejarErroresDB(E, 'SELECT'); // Uso del manejador genérico
    end;
  end;
end;


{$REGION 'CAFICULTORES : CREATE - POST   - UPDATE '}
function TServerMethods1.updateCaficultor(AData: TJSONObject): TJSONObject;
var
  FDQuery: TFDQuery;
  newId: Integer;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  if not Assigned(AData) then
    Exit(BuildResponse(6, 'Body inválido.'));

  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text :=
        'INSERT INTO CAFICULTORES (nombre, identificacion, ciudad, tipo_producto) ' +
        'OUTPUT INSERTED.id ' +
        'VALUES (:nombre, :identificacion, :ciudad, :tipo_producto)';

      FDQuery.ParamByName('nombre').AsString := AData.GetValue<string>('nombre', '');
      FDQuery.ParamByName('identificacion').AsString := AData.GetValue<string>('identificacion', '');
      FDQuery.ParamByName('ciudad').AsString := AData.GetValue<string>('ciudad', '');
      FDQuery.ParamByName('tipo_producto').AsString := AData.GetValue<string>('tipo_producto', '');

      FDQuery.Open; // obtiene el ID insertado
      newId := FDQuery.Fields[0].AsInteger;

      Result := BuildResponse(0, 'Caficultor registrado correctamente.',
                              TJSONObject.Create.AddPair('newId', TJSONNumber.Create(newId)));
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'INSERT');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'CAFICULTORES : READ   - GET    - GET '}
function TServerMethods1.GetCaficultores: TJSONObject;
var
  FDQuery: TFDQuery;
  Arr: TJSONArray;
  Obj: TJSONObject;
begin
  LogDebug('Entró a GetCaficultores');
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  Arr := TJSONArray.Create;
  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text := 'SELECT id, nombre, identificacion, ciudad, tipo_producto FROM CAFICULTORES';
      FDQuery.Open;

      while not FDQuery.Eof do
      begin
        Obj := TJSONObject.Create;
        Obj.AddPair('id', TJSONNumber.Create(FDQuery.FieldByName('id').AsInteger));
        Obj.AddPair('nombre', FDQuery.FieldByName('nombre').AsString);
        Obj.AddPair('identificacion', FDQuery.FieldByName('identificacion').AsString);
        Obj.AddPair('ciudad', FDQuery.FieldByName('ciudad').AsString);
        Obj.AddPair('tipo_producto', FDQuery.FieldByName('tipo_producto').AsString);

        Arr.AddElement(Obj);
        FDQuery.Next;
      end;

      Result := BuildResponse(0, 'Consulta exitosa.', Arr);
    except
      on E: Exception do
      begin
        Arr.Free; // liberar si hay excepción antes de devolver
        Result := ManejarErroresDB(E, 'SELECT');
      end;
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'CAFICULTORES : UPDATE - PUT    - ACCEPT '}
function TServerMethods1.acceptCaficultor(Id: Integer; AData: TJSONObject): TJSONObject;
var
  FDQuery: TFDQuery;
begin
  if not Assigned(AData) then
    Exit(BuildResponse(6, 'Body inválido.'));

  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text :=
        'UPDATE CAFICULTORES ' +
        'SET nombre = :nombre, identificacion = :identificacion, ciudad = :ciudad, tipo_producto = :tipo_producto ' +
        'WHERE id = :id';

      FDQuery.ParamByName('nombre').AsString := AData.GetValue<string>('nombre', '');
      FDQuery.ParamByName('identificacion').AsString := AData.GetValue<string>('identificacion', '');
      FDQuery.ParamByName('ciudad').AsString := AData.GetValue<string>('ciudad', '');
      FDQuery.ParamByName('tipo_producto').AsString := AData.GetValue<string>('tipo_producto', '');
      FDQuery.ParamByName('id').AsInteger := Id;

      FDQuery.ExecSQL;

      if FDQuery.RowsAffected > 0 then
        Result := BuildResponse(0, 'Caficultor actualizado correctamente.')
      else
        Result := BuildResponse(3, 'No se encontró el registro o no se aplicaron cambios.');
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'UPDATE');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'CAFICULTORES : DELETE - DELETE - CANCEL '}
function TServerMethods1.CancelCaficultor(Id: Integer): TJSONObject;
var
  FDQuery: TFDQuery;
begin
  FDQuery := TFDQuery.Create(nil);
  try
    FDQuery.Connection := GlobalConn;
    FDQuery.SQL.Text := 'DELETE FROM CAFICULTORES WHERE id = :id';
    FDQuery.ParamByName('id').AsInteger := Id;

    try
      FDQuery.ExecSQL;

      if FDQuery.RowsAffected > 0 then
        Result := BuildResponse(0, 'Caficultor eliminado correctamente.')
      else
        Result := BuildResponse(3, 'No se encontró el registro para eliminar.');
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'DELETE');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'ABONOS_MONEDERO : CREATE - POST   - UPDATE '}
function TServerMethods1.UpdateAbonoMonedero(AData: TJSONObject): TJSONObject;
var
  FDQuery: TFDQuery;
  newId: Integer;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  if not Assigned(AData) then
    Exit(BuildResponse(6, 'Body inválido.'));

  FDQuery := TFDQuery.Create(nil);
  try
    FDQuery.Connection := GlobalConn;
    FDQuery.SQL.Text :=
      'INSERT INTO ABONOS_MONEDERO (id_caficultor, valor, fecha) ' +
      'OUTPUT INSERTED.id ' +
      'VALUES (:id_caficultor, :valor, :fecha)';

    FDQuery.ParamByName('id_caficultor').AsInteger := AData.GetValue<Integer>('id_caficultor', 0);
    FDQuery.ParamByName('valor').AsFloat := AData.GetValue<Double>('valor', 0);
    FDQuery.ParamByName('fecha').AsDate := ISO8601TokyoToDateTime(AData.GetValue<string>('fecha', ''));

    try
      FDQuery.Open; // obtiene el newId
      newId := FDQuery.Fields[0].AsInteger;

      Result := BuildResponse(0, 'Abono registrado correctamente.',
                              TJSONObject.Create.AddPair('newId', TJSONNumber.Create(newId)));
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'INSERT');
    end;

  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'ABONOS_MONEDERO : READ - GET ALL'}
function TServerMethods1.GetAbonosMonedero: TJSONObject;
var
  FDQuery: TFDQuery;
  Arr: TJSONArray;
  Obj: TJSONObject;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  Arr := TJSONArray.Create;

  FDQuery := TFDQuery.Create(nil);
  try
    FDQuery.Connection := GlobalConn;
    FDQuery.SQL.Text := 'SELECT id, id_caficultor, valor, fecha FROM ABONOS_MONEDERO';

    try
      FDQuery.Open;
      while not FDQuery.Eof do
      begin
        Obj := TJSONObject.Create;
        Obj.AddPair('id', TJSONNumber.Create(FDQuery.FieldByName('id').AsInteger));
        Obj.AddPair('id_caficultor', TJSONNumber.Create(FDQuery.FieldByName('id_caficultor').AsInteger));
        Obj.AddPair('valor', TJSONNumber.Create(FDQuery.FieldByName('valor').AsFloat));
        Obj.AddPair('fecha', DateTimeToISO8601Tokyo(FDQuery.FieldByName('fecha').AsDateTime));
        Arr.AddElement(Obj);
        FDQuery.Next;
      end;

      Result := BuildResponse(0, 'Consulta de abonos exitosa.', Arr);
    except
      on E: Exception do
      begin
        Arr.Free;
        Result := ManejarErroresDB(E, 'SELECT');
      end;
    end;

  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'ABONOS_MONEDERO : UPDATE - PUT    - ACCEPT '}
function TServerMethods1.acceptAbonoMonedero(Id: Integer; AData: TJSONObject): TJSONObject;
var
  FDQuery: TFDQuery;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  if not Assigned(AData) then
    Exit(BuildResponse(6, 'Body inválido.'));

  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text :=
        'UPDATE ABONOS_MONEDERO ' +
        'SET id_caficultor = :id_caficultor, valor = :valor, fecha = :fecha ' +
        'WHERE id = :id';

      FDQuery.ParamByName('id_caficultor').AsInteger := AData.GetValue<Integer>('id_caficultor', 0);
      FDQuery.ParamByName('valor').AsFloat := AData.GetValue<Double>('valor', 0);
      FDQuery.ParamByName('fecha').AsDate := ISO8601TokyoToDateTime(AData.GetValue<string>('fecha', ''));
      FDQuery.ParamByName('id').AsInteger := Id;
      FDQuery.ExecSQL;

      if FDQuery.RowsAffected > 0 then
        Result := BuildResponse(0, 'Abono actualizado correctamente.')
      else
        Result := BuildResponse(3, 'No se encontró el abono o no se aplicaron cambios.');
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'UPDATE');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'ABONOS_MONEDERO : DELETE - DELETE - CANCEL '}
function TServerMethods1.CancelAbonoMonedero(Id: Integer): TJSONObject;
var
  FDQuery: TFDQuery;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text := 'DELETE FROM ABONOS_MONEDERO WHERE id = :id';
      FDQuery.ParamByName('id').AsInteger := Id;
      FDQuery.ExecSQL;

      if FDQuery.RowsAffected > 0 then
        Result := BuildResponse(0, 'Abono eliminado correctamente.')
      else
        Result := BuildResponse(3, 'No se encontró el abono para eliminar.');
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'DELETE');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'PRODUCTOS_CLIENTE : CREATE - POST   - UPDATE '}
function TServerMethods1.UpdateProductoCliente(AData: TJSONObject): TJSONObject;
var
  FDQuery: TFDQuery;
  newId: Integer;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  if not Assigned(AData) then
    Exit(BuildResponse(6, 'Body inválido.'));

  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text :=
        'INSERT INTO PRODUCTOS_CLIENTE (id_caficultor, tipo_producto, numero_cuenta) ' +
        'OUTPUT INSERTED.id ' +
        'VALUES (:id_caficultor, :tipo_producto, :numero_cuenta)';

      FDQuery.ParamByName('id_caficultor').AsInteger := AData.GetValue<Integer>('id_caficultor', 0);
      FDQuery.ParamByName('tipo_producto').AsString := AData.GetValue<string>('tipo_producto', '');
      FDQuery.ParamByName('numero_cuenta').AsString := AData.GetValue<string>('numero_cuenta', '');

      FDQuery.Open; // ejecuta y devuelve el NewID
      newId := FDQuery.Fields[0].AsInteger;

      Result := BuildResponse(0, 'Registro creado exitosamente', TJSONObject.Create.AddPair('newId', TJSONNumber.Create(newId)));
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'INSERT');
    end;

  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'PRODUCTOS_CLIENTE : READ - GET ALL'}
function TServerMethods1.GetProductosCliente: TJSONObject;
var
  FDQuery: TFDQuery;
  Arr: TJSONArray;
  Obj: TJSONObject;
begin
  Arr := TJSONArray.Create;
  FDQuery := TFDQuery.Create(nil);

  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text := 'SELECT id, id_caficultor, tipo_producto, numero_cuenta FROM PRODUCTOS_CLIENTE';
      FDQuery.Open;

      while not FDQuery.Eof do
      begin
        Obj := TJSONObject.Create;
        Obj.AddPair('id', TJSONNumber.Create(FDQuery.FieldByName('id').AsInteger));
        Obj.AddPair('id_caficultor', TJSONNumber.Create(FDQuery.FieldByName('id_caficultor').AsInteger));
        Obj.AddPair('tipo_producto', FDQuery.FieldByName('tipo_producto').AsString);
        Obj.AddPair('numero_cuenta', FDQuery.FieldByName('numero_cuenta').AsString);
        Arr.AddElement(Obj);
        FDQuery.Next;
      end;

      Result := BuildResponse(0, 'OK', Arr);
    except
      on E: Exception do
      begin
        Arr.Free;
        Result := ManejarErroresDB(E, 'SELECT');
      end;
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'PRODUCTOS_CLIENTE : UPDATE - PUT    - ACCEPT '}
function TServerMethods1.acceptProductoCliente(Id: Integer; AData: TJSONObject): TJSONObject;
var
  FDQuery: TFDQuery;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  if not Assigned(AData) then
    Exit(BuildResponse(6, 'Body inválido.'));

  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text :=
        'UPDATE PRODUCTOS_CLIENTE ' +
        'SET id_caficultor = :id_caficultor, tipo_producto = :tipo_producto, numero_cuenta = :numero_cuenta ' +
        'WHERE id = :id';

      FDQuery.ParamByName('id_caficultor').AsInteger := AData.GetValue<Integer>('id_caficultor', 0);
      FDQuery.ParamByName('tipo_producto').AsString := AData.GetValue<string>('tipo_producto', '');
      FDQuery.ParamByName('numero_cuenta').AsString := AData.GetValue<string>('numero_cuenta', '');
      FDQuery.ParamByName('id').AsInteger := Id;

      FDQuery.ExecSQL;

      if FDQuery.RowsAffected > 0 then
        Result := BuildResponse(0, 'Registro actualizado correctamente')
      else
        Result := BuildResponse(3, 'No se encontró el registro o no se aplicaron cambios.');
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'UPDATE');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'PRODUCTOS_CLIENTE : DELETE - DELETE - CANCEL '}
function TServerMethods1.CancelProductoCliente(Id: Integer): TJSONObject;
var
  FDQuery: TFDQuery;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text := 'DELETE FROM PRODUCTOS_CLIENTE WHERE id = :id';
      FDQuery.ParamByName('id').AsInteger := Id;
      FDQuery.ExecSQL;

      if FDQuery.RowsAffected > 0 then
        Result := BuildResponse(0, 'Registro eliminado correctamente')
      else
        Result := BuildResponse(3, 'No se encontró el registro para eliminar.');
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'DELETE');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'SALDO_MONEDERO : READ   - GET    - GET '}
function TServerMethods1.GetSaldoMonedero(id_caficultor: Integer): TJSONObject;
var
  FDQuery: TFDQuery;
  DataArr: TJSONArray;
  Obj: TJSONObject;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;

      // Si id_caficultor = 0 lo tratamos como NULL para traer todos
      if id_caficultor = 0 then
        FDQuery.SQL.Text := 'EXEC sp_ConsultarSaldo NULL'
      else
        FDQuery.SQL.Text := 'EXEC sp_ConsultarSaldo :id_caficultor';

      if id_caficultor <> 0 then
        FDQuery.ParamByName('id_caficultor').AsInteger := id_caficultor;

      FDQuery.Open;

      if not FDQuery.IsEmpty then
      begin
        DataArr := TJSONArray.Create;
        while not FDQuery.Eof do
        begin
          Obj := TJSONObject.Create;
          Obj.AddPair('id_caficultor', TJSONNumber.Create(FDQuery.FieldByName('id_caficultor').AsInteger));
          Obj.AddPair('saldo', TJSONNumber.Create(FDQuery.FieldByName('saldo').AsFloat));
          DataArr.AddElement(Obj);
          FDQuery.Next;
        end;
        Result := BuildResponse(0, 'Consulta exitosa', DataArr);
      end
      else
        Result := BuildResponse(3, 'No se encontraron registros.');
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'SELECT');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'PRODUCTOS_CLIENTE : READ   - GET    - GET por caficultor'}
function TServerMethods1.GetProductosByCaficultor(id_caficultor: Integer): TJSONObject;
var
  FDQuery: TFDQuery;
  Arr: TJSONArray;
  Row: TJSONObject;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  FDQuery := TFDQuery.Create(nil);
  Arr := TJSONArray.Create;
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text := 'EXEC sp_ConsultarProductos :id_caficultor';
      FDQuery.ParamByName('id_caficultor').AsInteger := id_caficultor;
      FDQuery.Open;

      while not FDQuery.Eof do
      begin
        Row := TJSONObject.Create;
        Row.AddPair('id', TJSONNumber.Create(FDQuery.FieldByName('id').AsInteger));
        Row.AddPair('id_caficultor', TJSONNumber.Create(FDQuery.FieldByName('id_caficultor').AsInteger));
        Row.AddPair('tipo_producto', FDQuery.FieldByName('tipo_producto').AsString);
        Row.AddPair('numero_cuenta', FDQuery.FieldByName('numero_cuenta').AsString);
        Arr.AddElement(Row);
        FDQuery.Next;
      end;

      if Arr.Count > 0 then
        Result := BuildResponse(0, 'Consulta exitosa', Arr)
      else
        Result := BuildResponse(3, 'No se encontraron productos para el caficultor.');
    except
      on E: Exception do
      begin
        Arr.Free;
        Result := ManejarErroresDB(E, 'SELECT');
      end;
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'ABONOS_MONEDERO : READ   - GET    - GET por caficultor'}
function TServerMethods1.GetAbonosFiltrados(id_caficultor: Integer; fecha_inicio, fecha_fin: string): TJSONObject;
var
  FDQuery: TFDQuery;
  Arr: TJSONArray;
  Obj: TJSONObject;
begin
  if not Assigned(GlobalConn) then
    Exit(BuildResponse(2, 'Conexión a BD no inicializada.'));

  FDQuery := TFDQuery.Create(nil);
  Arr := TJSONArray.Create;
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text := 'EXEC sp_GetAbonosFiltrados :id_caficultor, :fecha_inicio, :fecha_fin';

      if id_caficultor = 0 then
      begin
        FDQuery.ParamByName('id_caficultor').DataType := ftInteger;
        FDQuery.ParamByName('id_caficultor').Clear;
      end
      else
        FDQuery.ParamByName('id_caficultor').AsInteger := id_caficultor;

      if fecha_inicio = '' then
      begin
        FDQuery.ParamByName('fecha_inicio').DataType := ftstring;
        FDQuery.ParamByName('fecha_inicio').Clear
      end
      else
        FDQuery.ParamByName('fecha_inicio').AsString := fecha_inicio;

      if fecha_fin = '' then
      begin
        FDQuery.ParamByName('fecha_fin').DataType := ftstring;
        FDQuery.ParamByName('fecha_fin').Clear
      end
      else
        FDQuery.ParamByName('fecha_fin').AsString := fecha_fin;

      FDQuery.Open;

      while not FDQuery.Eof do
      begin
        Obj := TJSONObject.Create;
        Obj.AddPair('id_caficultor', TJSONNumber.Create(FDQuery.FieldByName('id_caficultor').AsInteger));
        Obj.AddPair('valor', TJSONNumber.Create(FDQuery.FieldByName('valor').AsFloat));
        Obj.AddPair('fecha', FDQuery.FieldByName('fecha').AsString);
        Arr.AddElement(Obj);
        FDQuery.Next;
      end;

      if Arr.Count > 0 then
        Result := BuildResponse(0, 'Consulta exitosa', Arr)
      else
        Result := BuildResponse(3, 'No se encontraron abonos para el caficultor.');
    except
      on E: Exception do
      begin
        Arr.Free;
        Result := ManejarErroresDB(E, 'SELECT');
      end;
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

{$REGION 'REGISTRAR_ABONOS_MONEDERO : CREATE - POST   - UPDATE '}
function TServerMethods1.updateRegistrarAbono(AData: TJSONObject): TJSONObject;
var
  FDQuery: TFDQuery;
  newId: Integer;
  DataObj: TJSONObject;
begin
  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := GlobalConn;
      FDQuery.SQL.Text := 'EXEC sp_RegistrarAbono :id_caficultor, :valor, :fecha';
      FDQuery.ParamByName('id_caficultor').AsInteger := AData.GetValue<Integer>('id_caficultor', 0);
      FDQuery.ParamByName('valor').AsFloat := AData.GetValue<Double>('valor', 0);
      FDQuery.ParamByName('fecha').AsDate := ISO8601TokyoToDateTime(AData.GetValue<string>('fecha', ''));

      FDQuery.Open; // porque devuelve un SELECT con NewID

      if not FDQuery.IsEmpty then
      begin
        newId := FDQuery.FieldByName('NewID').AsInteger;
        DataObj := TJSONObject.Create;
        DataObj.AddPair('newId', TJSONNumber.Create(newId));
        Result := BuildResponse(0, 'Abono registrado exitosamente', DataObj);
      end
      else
        Result := BuildResponse(2, 'No se pudo obtener el ID del abono insertado.');
    except
      on E: Exception do
        Result := ManejarErroresDB(E, 'INSERT');
    end;
  finally
    FDQuery.Free;
  end;
end;
{$ENDREGION}

end.
