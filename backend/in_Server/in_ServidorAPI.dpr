library in_ServidorAPI;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  Data.DBXCommon,
  Datasnap.DSSession,
  ServerMethodsUnit1 in '..\ServerMethodsUnit1.pas' {ServerMethods1: TDataModule},
  ServerContainerUnit1 in '..\ServerContainerUnit1.pas' {ServerContainer1: TDataModule},
  WebModuleUnit1 in '..\WebModuleUnit1.pas' {WebModule1: TWebModule},
  uDBGlobal in '..\UComunes\uDBGlobal.pas',
  UFunciones in '..\UComunes\UFunciones.pas',
  ULog in '..\UComunes\ULog.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

procedure TerminateThreads;
begin
  TDSSessionManager.Instance.Free;
  Data.DBXCommon.TDBXScheduler.Instance.Free;
end;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  TISAPIApplication(Application).OnTerminate := TerminateThreads;
  Application.Run;
end.
