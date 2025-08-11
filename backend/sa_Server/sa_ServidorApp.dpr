program sa_ServidorApp;

{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  UTF8ContentParser,
  WEB.ReqMulti,
  System.SysUtils,
  Dialogs,
  IPPeerServer,
  IPPeerAPI,
  Datasnap.DSHTTP,
  Datasnap.DSHTTPWebBroker,
  Datasnap.DSServer,
  Datasnap.DSCommonServer,
  IdContext,
  IdCustomHTTPServer,
  IdTCPServer,
  Winapi.Windows,
  uFrmPrincipal in 'Forms\uFrmPrincipal.pas' {Form1},
  ServerMethodsUnit1 in '..\ServerMethodsUnit1.pas' {ServerMethods1: TDataModule},
  ServerContainerUnit1 in '..\ServerContainerUnit1.pas' {ServerContainer1: TDataModule},
  WebModuleUnit1 in '..\WebModuleUnit1.pas' {WebModule1: TWebModule},
  uDBGlobal in '..\UComunes\uDBGlobal.pas',
  UFunciones in '..\UComunes\UFunciones.pas';

{$R *.res}

begin
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    Application.Initialize;
    Application.Title := 'ServerApp';

    Application.CreateForm(TForm1, Form1);
    Application.Run;

  except
    on e: Exception do
      ShowMessage(e.Message);
  end;
end.
