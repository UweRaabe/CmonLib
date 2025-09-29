unit Cmon.Messaging.Logging;

interface

uses
  Cmon.Messaging;

type
  TLogMessage = class(TCommonMessage<string>)
  strict private
  class var
    FAutoRegisterHandler: Boolean;
  private
    function GetMessageText: string;
  public
    class procedure SendMessage(Sender: TObject; const AMessageText: string);
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
    property MessageText: string read GetMessageText;
  end;
  TLogMessageHandler = class(TCommonMessageHandler<TLogMessage>);

implementation

function TLogMessage.GetMessageText: string;
begin
  Result := Value;
end;

class procedure TLogMessage.SendMessage(Sender: TObject; const AMessageText: string);
begin
  inherited SendMessage(Sender, AMessageText);
end;

initialization
  TLogMessage.AutoRegisterHandler := True;
end.
