unit Cmon.Logging;

interface

uses
  Cmon.Messaging;

type
  TLogMethod = procedure(const AMessage: string) of object;
  TLogProc = reference to procedure(const AMessage: string);

  TLog = record
  public
    class procedure Send(const AMessage: string); overload; static;
    class procedure Send(const Fmt: string; const Args: Array of const); overload; static;
    class function Subscribe(AProc: TLogProc): IMessageHandler; overload; static;
    class function Subscribe(AMethod: TLogMethod): IMessageHandler; overload; static;
  end;

implementation

uses
  System.SysUtils,
  Cmon.Messaging.Logging;

type
  TLogHandler = class(TLogMessageHandler)
  private
    FLogMethod: TLogMethod;
    FLogProc: TLogProc;
  protected
    procedure HandleMessage(const Sender: TObject; const Message: TLogMessage); override;
  public
    constructor Create(ALogMethod: TLogMethod); overload;
    constructor Create(ALogProc: TLogProc); overload;
  end;

constructor TLogHandler.Create(ALogMethod: TLogMethod);
begin
  inherited Create;
  FLogMethod := ALogMethod;
end;

constructor TLogHandler.Create(ALogProc: TLogProc);
begin
  inherited Create;
  FLogProc := ALogProc;
end;

procedure TLogHandler.HandleMessage(const Sender: TObject; const Message: TLogMessage);
begin
  if Assigned(FLogProc) then
    FLogProc(Message.MessageText);
  if Assigned(FLogMethod) then
    FLogMethod(Message.MessageText);
end;

class procedure TLog.Send(const AMessage: string);
begin
  TLogMessage.SendMessage(nil, AMessage);
end;

class procedure TLog.Send(const Fmt: string; const Args: array of const);
begin
  Send(Format(Fmt, Args));
end;

class function TLog.Subscribe(AMethod: TLogMethod): IMessageHandler;
begin
  Result := TLogHandler.CreateInterface(TLogHandler.Create(AMethod));
end;

class function TLog.Subscribe(AProc: TLogProc): IMessageHandler;
begin
  Result := TLogHandler.CreateInterface(TLogHandler.Create(AProc));
end;

end.
