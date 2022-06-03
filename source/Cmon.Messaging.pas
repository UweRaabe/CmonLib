unit Cmon.Messaging;

interface

uses
  System.Messaging, System.UITypes;

type
  TCommonMessage<T> = class(TMessage<T>)
  strict protected
    class function Manager: TMessageManager; virtual;
  public
    class procedure SendMessage(Sender: TObject; const AValue: T);
    class function Subscribe(const AListener: TMessageListener): Integer; overload;
    class function Subscribe(const AListenerMethod: TMessageListenerMethod): Integer; overload;
    class procedure Unsubscribe(const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); overload;
    class procedure Unsubscribe(Id: Integer; Immediate: Boolean = False); overload;
    class procedure Unsubscribe(const AListener: TMessageListener; Immediate: Boolean = False); overload;
  end;

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
    property Answer: TModalResult read FAnswer write FAnswer;
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
    property Buttons: TMsgDlgButtons read FButtons write FButtons;
    property HelpContext: Integer read FHelpContext write FHelpContext;
    property MessageText: string read GetMessageText;
    property MsgDlgType: TMsgDlgType read FMsgDlgType write FMsgDlgType;
    property Title: string read FTitle write FTitle;
  end;

implementation

class function TCommonMessage<T>.Manager: TMessageManager;
begin
  Result := TMessageManager.DefaultManager;
end;

class procedure TCommonMessage<T>.SendMessage(Sender: TObject; const AValue: T);
begin
  Manager.SendMessage(Sender, Self.Create(AValue));
end;

class function TCommonMessage<T>.Subscribe(const AListener: TMessageListener): Integer;
begin
  Result := Manager.SubscribeToMessage(Self, AListener);
end;

class function TCommonMessage<T>.Subscribe(const AListenerMethod: TMessageListenerMethod): Integer;
begin
  Result := Manager.SubscribeToMessage(Self, AListenerMethod);
end;

class procedure TCommonMessage<T>.Unsubscribe(const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False);
begin
  Manager.Unsubscribe(Self, AListenerMethod, Immediate);
end;

class procedure TCommonMessage<T>.Unsubscribe(Id: Integer; Immediate: Boolean);
begin
  Manager.Unsubscribe(Self, Id, Immediate);
end;

class procedure TCommonMessage<T>.Unsubscribe(const AListener: TMessageListener; Immediate: Boolean);
begin
  Manager.Unsubscribe(Self, AListener, Immediate);
end;

function TLogMessage.GetMessageText: string;
begin
  Result := Value;
end;

class procedure TLogMessage.SendMessage(Sender: TObject; const AMessageText: string);
begin
  inherited SendMessage(Sender, AMessageText);
end;

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
  TLogMessage.AutoRegisterHandler := True;
end.
