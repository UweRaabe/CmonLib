unit Cmon.Messaging.Dialogs;

interface

uses
  System.UITypes,
  Cmon.Messaging;

type
  TDlgMessage = class(TCommonMessage<string>)
  strict private
  class var
    FAutoRegisterHandler: Boolean;
  private
    FAnswer: TModalResult;
    FButtons: TMsgDlgButtons;
    FHelpContext: Integer;
    FMsgDlgType: TMsgDlgType;
    FTitle: string;
    function GetMessageText: string;
  public
    class function Execute(Sender: TObject; const ATitle, AMessageText: string; AMsgDlgType: TMsgDlgType; AButtons: TMsgDlgButtons;
        ADefault: TModalResult; AHelpContext: Integer): TModalResult;
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
    property Answer: TModalResult read FAnswer write FAnswer;
    property Buttons: TMsgDlgButtons read FButtons write FButtons;
    property HelpContext: Integer read FHelpContext write FHelpContext;
    property MessageText: string read GetMessageText;
    property MsgDlgType: TMsgDlgType read FMsgDlgType write FMsgDlgType;
    property Title: string read FTitle write FTitle;
  end;
  TDlgMessageHandler = class(TCommonMessageHandler<TDlgMessage>);

implementation

uses
  System.SysUtils;

class function TDlgMessage.Execute(Sender: TObject; const ATitle, AMessageText: string; AMsgDlgType: TMsgDlgType; AButtons:
    TMsgDlgButtons; ADefault: TModalResult; AHelpContext: Integer): TModalResult;
var
  instance: TDlgMessage;
begin
  instance := Self.Create(AMessageText);
  try
    instance.Title := ATitle;
    instance.MsgDlgType := AMsgDlgType;
    instance.Buttons := AButtons;
    instance.Answer := ADefault;
    Instance.HelpContext := AHelpContext;
    Manager.SendMessage(Sender, instance, False);
    Result := instance.Answer;
  finally
    instance.Free;
  end;
end;

function TDlgMessage.GetMessageText: string;
begin
  Result := Value;
end;

initialization
  TDlgMessage.AutoRegisterHandler := True;
end.
