unit Cmon.Messaging;

interface

uses
  System.Messaging;

type
  TCommonMessage = class(TMessage)
  strict protected
    class function Manager: TMessageManager; virtual;
  public
    class procedure SendMessage(Sender: TObject);
    class function Subscribe(const AListener: TMessageListener): Integer; overload;
    class function Subscribe(const AListenerMethod: TMessageListenerMethod): Integer; overload;
    class procedure Unsubscribe(Id: Integer; Immediate: Boolean = False); overload;
    class procedure Unsubscribe(const AListener: TMessageListener; Immediate: Boolean = False); overload;
    class procedure Unsubscribe(const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); overload;
  end;

  TCommonMessage<T> = class(TCommonMessage)
  private
    FValue: T;
    procedure SetValue(const AValue: T);
  public
    constructor Create(const AValue: T);
    class procedure SendMessage(Sender: TObject; const AValue: T);
    property Value: T read FValue write SetValue;
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
  IMessageHandler = interface
  end;

  TCommonMessageHandlerClass = class of TCommonMessageHandler;
  TCommonMessageHandler = class
  public
    constructor Create; virtual;
    class function CreateInterface(AInstance: TCommonMessageHandler = nil): IMessageHandler;
  end;

  THandleMessageEvent<T:TCommonMessage> = procedure(const Sender: TObject; const Msg: T) of object;
  THandleMessageProc<T:TCommonMessage> = reference to procedure(const Sender: TObject; const Msg: T);

  TCommonMessageHandler<TMsg: TCommonMessage> = class(TCommonMessageHandler)
  private
    FMessageID: Integer;
    FMethod: THandleMessageEvent<TMsg>;
    FProc: THandleMessageProc<TMsg>;
    procedure MessageHandler(const Sender: TObject; const M: TMessage);
  strict protected
    procedure DoHandleMethod(const Sender: TObject; const Msg: TMsg);
    procedure DoHandleProc(const Sender: TObject; const Msg: TMsg);
    procedure HandleMessage(const Sender: TObject; const Msg: TMsg); virtual;
    procedure Subscribe; virtual;
    procedure Unsubscribe; virtual;
  public
    constructor Create; overload; override;
    constructor Create(AMethod: THandleMessageEvent<TMsg>); reintroduce; overload;
    constructor Create(AProc: THandleMessageProc<TMsg>); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

type
  TMessageHandlerImpl = class(TInterfacedObject, IMessageHandler)
  private
    FInstance: TCommonMessageHandler;
  public
    constructor Create(AInstance: TCommonMessageHandler);
    destructor Destroy; override;
    property Instance: TCommonMessageHandler read FInstance implements IMessageHandler;
  end;

constructor TMessageHandlerImpl.Create(AInstance: TCommonMessageHandler);
begin
  inherited Create;
  FInstance := AInstance;
end;

destructor TMessageHandlerImpl.Destroy;
begin
  FInstance.Free;
  inherited;
end;

constructor TCommonMessage<T>.Create(const AValue: T);
begin
  FValue := AValue;
end;

class procedure TCommonMessage<T>.SendMessage(Sender: TObject; const AValue: T);
begin
  Manager.SendMessage(Sender, Self.Create(AValue));
end;

procedure TCommonMessage<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
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
var
  msg: TCommonMessage<T, TResult>;
begin
  msg := Self.Create(AValue);
  try
    msg.ResultValue := ADefault;
    Manager.SendMessage(Sender, msg, False);
    AValue := msg.Value;
    Result := msg.ResultValue;
  finally
    msg.Free;
  end;
end;

class function TCommonMessage.Manager: TMessageManager;
begin
  Result := TMessageManager.DefaultManager;
end;

class procedure TCommonMessage.SendMessage(Sender: TObject);
begin
  Manager.SendMessage(Sender, Self.Create);
end;

class function TCommonMessage.Subscribe(const AListener: TMessageListener): Integer;
begin
  Result := Manager.SubscribeToMessage(Self, AListener);
end;

class function TCommonMessage.Subscribe(const AListenerMethod: TMessageListenerMethod): Integer;
begin
  Result := Manager.SubscribeToMessage(Self, AListenerMethod);
end;

class procedure TCommonMessage.Unsubscribe(Id: Integer; Immediate: Boolean);
begin
  Manager.Unsubscribe(Self, Id, Immediate);
end;

class procedure TCommonMessage.Unsubscribe(const AListener: TMessageListener; Immediate: Boolean);
begin
  Manager.Unsubscribe(Self, AListener, Immediate);
end;

class procedure TCommonMessage.Unsubscribe(const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False);
begin
  Manager.Unsubscribe(Self, AListenerMethod, Immediate);
end;

constructor TCommonMessageHandler.Create;
begin
  inherited;
end;

class function TCommonMessageHandler.CreateInterface(AInstance: TCommonMessageHandler = nil): IMessageHandler;
begin
  if AInstance = nil then
    AInstance := Self.Create;
  Result := TMessageHandlerImpl.Create(AInstance);
end;

constructor TCommonMessageHandler<TMsg>.Create;
begin
  inherited;
  Subscribe;
end;

constructor TCommonMessageHandler<TMsg>.Create(AMethod: THandleMessageEvent<TMsg>);
begin
  Create;
  FMethod := AMethod;
end;

constructor TCommonMessageHandler<TMsg>.Create(AProc: THandleMessageProc<TMsg>);
begin
  Create;
  FProc := AProc;
end;

destructor TCommonMessageHandler<TMsg>.Destroy;
begin
  Unsubscribe;
  inherited;
end;

procedure TCommonMessageHandler<TMsg>.DoHandleMethod(const Sender: TObject; const Msg: TMsg);
begin
  if Assigned(FMethod) then FMethod(Sender, Msg);
end;

procedure TCommonMessageHandler<TMsg>.DoHandleProc(const Sender: TObject; const Msg: TMsg);
begin
  if Assigned(FProc) then FProc(Sender, Msg);
end;

procedure TCommonMessageHandler<TMsg>.HandleMessage(const Sender: TObject; const Msg: TMsg);
begin
  DoHandleMethod(Sender, Msg);
  DoHandleProc(Sender, Msg);
end;

procedure TCommonMessageHandler<TMsg>.MessageHandler(const Sender: TObject; const M: TMessage);
begin
  HandleMessage(Sender, M as TMsg);
end;

procedure TCommonMessageHandler<TMsg>.Subscribe;
begin
  FMessageID := TMsg.Subscribe(MessageHandler);
end;

procedure TCommonMessageHandler<TMsg>.Unsubscribe;
begin
  TMsg.Unsubscribe(FMessageID);
end;

end.
