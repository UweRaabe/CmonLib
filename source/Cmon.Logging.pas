unit Cmon.Logging;

interface

uses
  System.Classes;

type
  TLog = record
  type
    TLogHandlerProc = reference to procedure(const AMessage: string);
  public
    class procedure Send(const AMessage: string); overload; static;
    class procedure Send(const Fmt: string; const Args: Array of const); overload; static;
    class function Subscribe(AProc: TLogHandlerProc): Integer; static;
    class procedure Unsubscribe(AId: Integer); static;
  end;

implementation

uses
  System.Messaging, System.SysUtils,
  Cmon.Messaging;

class procedure TLog.Send(const AMessage: string);
begin
  TLogMessage.SendMessage(nil, AMessage);
end;

class procedure TLog.Send(const Fmt: string; const Args: array of const);
begin
  Send(Format(Fmt, Args));
end;

class function TLog.Subscribe(AProc: TLogHandlerProc): Integer;
begin
  Result := TLogMessage.Subscribe(
    procedure(const Sender: TObject; const M: TMessage)
    begin
      AProc(TLogMessage(M).Value);
    end);
end;

class procedure TLog.Unsubscribe(AId: Integer);
begin
  TLogMessage.Unsubscribe(AId);
end;

end.
