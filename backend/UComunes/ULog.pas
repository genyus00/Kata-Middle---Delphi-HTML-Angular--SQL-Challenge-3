unit ULog;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, Winapi.Windows;

procedure LogDebug(const Msg: string);
procedure GetLogContent(Dest: TStrings);
function  GetLogText: string;

implementation

var
  LogList: TStringList;
  LogLock: TCriticalSection;

procedure LogDebug(const Msg: string);
var
  Line: string;
begin
  Line := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' - ' + Msg;

  LogLock.Acquire;
  try
    LogList.Add(Line);
  finally
    LogLock.Release;
  end;

  // OutputDebugString solo en modo stand-alone o debug
  {$IFDEF DEBUG}
  OutputDebugString(PChar(Line));
  {$ENDIF}
end;

procedure GetLogContent(Dest: TStrings);
begin
  LogLock.Acquire;
  try
    Dest.Assign(LogList);
  finally
    LogLock.Release;
  end;
end;

function GetLogText: string;
begin
  LogLock.Acquire;
  try
    Result := LogList.Text;
  finally
    LogLock.Release;
  end;
end;

initialization
  LogList := TStringList.Create;
  LogLock := TCriticalSection.Create;

finalization
  LogLock.Acquire;
  try
    FreeAndNil(LogList);
  finally
    LogLock.Release;
    FreeAndNil(LogLock);
  end;

end.

