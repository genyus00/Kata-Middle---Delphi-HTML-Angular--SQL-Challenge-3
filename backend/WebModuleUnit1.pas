unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, Datasnap.DSHTTPCommon,
  Datasnap.DSHTTPWebBroker, Datasnap.DSServer,
  Datasnap.DSAuth, IPPeerServer, Datasnap.DSCommonServer, Datasnap.DSHTTP,
  System.JSON, Data.DBXCommon, Vcl.Graphics, Winapi.Windows;

type
  TWebModule1 = class(TWebModule)
    DSHTTPWebDispatcher1: TDSHTTPWebDispatcher;
    WebFileDispatcher1: TWebFileDispatcher;
    procedure WebModuleDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure DSHTTPWebDispatcher1FormatResult(Sender: TObject;
      var ResultVal: TJSONValue; const Command: TDBXCommand;
      var Handled: Boolean);
    procedure WebModuleWebActionItem1Action(Sender: TObject;Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleBeforeDispatch(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);

  private
    { Private declarations }
    function TextoHTML(strDato,strValor: String; colDato,colValor: TColor;bolDatoNegrilla,bolValorNegrilla: Boolean): String;
    function TColorToHex(Color: TColor): string;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses
   ServerMethodsUnit1, Web.WebReq, UFunciones, ServerContainerUnit1;

function TWebModule1.TColorToHex(Color: TColor): string;
begin
  Result :=
    IntToHex(GetRValue(Color), 2) +
    IntToHex(GetGValue(Color), 2) +
    IntToHex(GetBValue(Color), 2);
end;

function TWebModule1.TextoHTML(strDato, strValor: String; colDato,
  colValor: TColor; bolDatoNegrilla, bolValorNegrilla: Boolean): String;
var
  strTemp: String;
begin
  Result:= '<FONT color="#' + TColorToHex(colDato)+'">';

  if bolDatoNegrilla then
    Result:= '<B>' + Result + '</B>';

  Result:= Result + strDato;

  strTemp:= '<FONT color="#' + TColorToHex(colValor)+'">';

  if bolValorNegrilla then
    strTemp:= '<B>' + strTemp + '</B>';

  Result:= Result + strTemp + strValor + '</FONT>';
end;

procedure TWebModule1.WebModuleDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  strVersion: String;
  strMensajeVersionPrueba: String;
begin
  strMensajeVersionPrueba:= TextoHTML(' (exclusiva para pruebas) ','', clRed, clRed, True, True);
  strVersion:= '0.0.0.0' + strMensajeVersionPrueba;

  Response.ContentType := 'text/html';
  Response.Content :=
    '<html>' +
    '<head><title>DataSnap Server</title></head>' +
    '<body>Banco de Bogotá - DataSnap Server '+
        TextoHTML('Versión: ', strVersion, clBlack, clBlack, False, False)+
       '</body>' +
    '</html>';
  Handled := True;
end;

procedure TWebModule1.DSHTTPWebDispatcher1FormatResult(Sender: TObject;
  var ResultVal: TJSONValue; const Command: TDBXCommand; var Handled: Boolean);
var
  Temporal: TJSONValue;
begin
  Temporal := ResultVal;
  ResultVal := (Temporal as TJSONArray).Remove(0);
  Handled := True;
  Temporal.DisposeOf();
end;

procedure TWebModule1.WebModuleWebActionItem1Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '{"mensaje": "Hola mundo REST"}';
  Response.ContentType := 'application/json';
  Handled := True;
end;

procedure TWebModule1.WebModuleBeforeDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  // Habilitar CORS para todos los orígenes
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');

  // Métodos permitidos
  Response.SetCustomHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');

  // Cabeceras permitidas
  Response.SetCustomHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');

  // Soporte para solicitudes preflight (OPTIONS)
  if Request.Method = 'OPTIONS' then
  begin
    Response.StatusCode := 200;
    Response.Content := '';
    Handled := True;
  end;
end;


procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  DSHTTPWebDispatcher1.Server := DSServer;
  if DSServer.Started then
  begin
    DSHTTPWebDispatcher1.DbxContext := DSServer.DbxContext;
    DSHTTPWebDispatcher1.Start;
  end;
  DSHTTPWebDispatcher1.AuthenticationManager := DSAuthenticationManager;

  // Seguridad: limitar WebFileDispatcher (no servir todo el disco en producción)
  // Recomiendo establecer RootDirectory a una carpeta pública específica
  WebFileDispatcher1.RootDirectory := ExtractFilePath(ParamStr(0)) + 'www';
end;

initialization
finalization
  Web.WebReq.FreeWebModules;
end.

