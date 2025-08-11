unit uFrmPrincipal;

interface

uses
  Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, Web.HTTPApp, Vcl.Menus,
  Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    Pnl_1: TPanel;
    Img_StatusBar: TImage;
    StbEstado: TStatusBar;
    TmrReloj: TTimer;
    btnStart: TButton;
    btnStop: TButton;
    btnOpenBrowser: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);

    procedure btnOpenBrowserClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure TmrRelojTimer(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FServer: TIdHTTPWebBrokerBridge;
  public
    { Public declarations }
    procedure StartServer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  WinApi.Windows, Winapi.ShellApi, Datasnap.DSSession, uDBGlobal, UFunciones;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  btnstart.Enabled := not FServer.Active;
  btnStop.Enabled := FServer.Active;
end;

procedure TForm1.btnOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  StartServer;
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
    TDSSessionManager.Instance.TerminateAllSessions;
end;

procedure TForm1.btnStartClick(Sender: TObject);
var
  Puerto: Integer;
begin
  if EditPort.Text = '' then
    raise Exception.Create('Debe especificar un puerto para el servidor.');

  if not TryStrToInt(EditPort.Text, Puerto) then
    raise Exception.Create('El puerto debe ser un número.');

  // Intentar conexión a BD
  if not Assigned(GlobalConn) then
  begin
    GlobalConn := CrearConexionFireDACDesdeINI('DBConex.ini');

    if not Assigned(GlobalConn) then
    begin
      LogDebug('ERROR: No se pudo establecer conexión a la base de datos.');
      ShowMessage('Error: No se pudo conectar a la base de datos.');
      Exit;
    end;
  end;

  StartServer;

  LogDebug('Servidor iniciado.');
  TmrReloj.Enabled := True;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  TerminateThreads;

  // Cerrar conexión a BD
  if Assigned(GlobalConn) then
  begin
    try
      GlobalConn.Connected := False;
    except
      on E: Exception do
        LogDebug('Error al cerrar conexión: ' + E.Message);
    end;
    FreeAndNil(GlobalConn);
  end;

  // Detener servidor HTTP
  FServer.Active := False;
  Sleep(200); // pequeña pausa para liberar puertos

  FServer.Bindings.Clear;
  TmrReloj.Enabled := False;
  LogDebug('Servidor detenido y conexión cerrada.');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Asegurarse de limpiar al cerrar el formulario
  if FServer.Active then
    btnStopClick(nil); // Forzar detención

end;

procedure TForm1.StartServer;
var
  Puerto: Integer;
begin
  if not TryStrToInt(EditPort.Text, Puerto) then
  begin
    ShowMessage('Puerto inválido. Ingrese un número.');
    Exit;
  end;

  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := Puerto;

    try
      FServer.Active := True;
      StbEstado.Panels[1].Text := ' Conectado a [' + GlobalConn.Params.Database +'] - Server [' + EditPort.Text+ ']';
    except
      on E: Exception do
      begin
        ShowMessage('Error al iniciar servidor: ' + E.Message);
        LogDebug('Error al iniciar servidor: ' + E.Message);
      end;
    end;
  end;
end;

procedure TForm1.TmrRelojTimer(Sender: TObject);
begin
  if Self = nil then
    TmrReloj.Enabled := false;

  StbEstado.Panels[3].Text := formatdatetime('HH:mm:ss', time);
end;

end.
