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
    procedure SetValue(const AValue: T);
    class function Subscribe(const AListener: TMessageListener): Integer; overload;
    class function Subscribe(const AListenerMethod: TMessageListenerMethod): Integer; overload;
    class procedure Unsubscribe(const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); overload;
    class procedure Unsubscribe(Id: Integer; Immediate: Boolean = False); overload;
    class procedure Unsubscribe(const AListener: TMessageListener; Immediate: Boolean = False); overload;
  end;

  TCommonMessage<T, TResult> = class(TCommonMessage<T>)
  private
    FResultValue: TResult;
  public
    class function Execute(Sender: TObject; const AValue: T; const ADefault: TResult): TResult; overload;
    class function Execute(Sender: TObject; const AValue: T): TResult; overload;
    class function Query(Sender: TObject; var AValue: T): TResult; overload;
    class function Query(Sender: TObject; var AValue: T; const ADefault: TResult): TResult; overload;
    property ResultValue: TResult read FResultValue write FResultValue;
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
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
    property Answer: TModalResult read FAnswer write FAnswer;
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

procedure TCommonMessage<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
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

class function TCommonMessage<T, TResult>.Execute(Sender: TObject; const AValue: T; const ADefault: TResult): TResult;
var
  dummy: T;
begin
  dummy := AValue;
  Result := Query(Sender, dummy, ADefault);
end;

class function TCommonMessage<T, TResult>.Execute(Sender: TObject; const AValue: T): TResult;
begin
  Result := Execute(Sender, AValue, Default(TResult));
end;

class function TCommonMessage<T, TResult>.Query(Sender: TObject; var AValue: T): TResult;
begin
  Result := Query(Sender, AValue, Default(TResult));
end;

class function TCommonMessage<T, TResult>.Query(Sender: TObject; var AValue: T; const ADefault: TResult): TResult;
begin
  var msg := Self.Create(AValue);
  try
    msg.ResultValue := ADefault;
    Manager.SendMessage(Sender, msg, False);
    AValue := msg.Value;
    Result := msg.ResultValue;
  finally
    msg.Free;
  end;
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
