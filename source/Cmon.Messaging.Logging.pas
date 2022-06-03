unit Cmon.Messaging.Logging;

interface

uses
  System.Messaging;

type
  TLogMessageHandler = class
  private
    FMessageID: Integer;
  strict protected
    procedure LogMessage(const Sender: TObject; const M: TMessage); virtual; abstract;
    procedure Subscribe; virtual;
    procedure Unsubscribe; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Cmon.Messaging;

constructor TLogMessageHandler.Create;
begin
  inherited;
  Subscribe;
end;

destructor TLogMessageHandler.Destroy;
begin
  Unsubscribe;
  inherited;
end;

procedure TLogMessageHandler.Subscribe;
begin
  FMessageID := TLogMessage.Subscribe(LogMessage);
end;

procedure TLogMessageHandler.Unsubscribe;
begin
  TLogMessage.Unsubscribe(FMessageID);
end;

end.
