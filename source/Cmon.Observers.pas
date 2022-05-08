unit Cmon.Observers;

interface

uses
  System.Classes, System.SysUtils;

type
  TControlValueObserver<T: TComponent> = class(TInterfacedObject, IObserver, IMultiCastObserver, IControlValueObserver,
      IObserverTrack)
  private
    FOnModified: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    FOnToggle: TObserverToggleEvent;
    FTarget: T;
    function GetActive: Boolean;
    function GetOnObserverToggle: TObserverToggleEvent;
    procedure Removed;
    procedure SetActive(Value: Boolean);
    procedure SetOnObserverToggle(AEvent: TObserverToggleEvent);
  strict protected
    function GetTrack: Boolean; virtual;
    procedure ValueModified; virtual;
    procedure ValueUpdate; virtual;
  protected
    procedure DoValueModified;
    procedure DoValueUpdate;
  public
    constructor Create(ATarget: T; AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil);
    property Target: T read FTarget;
  end;

type
  TControlValueObserver<T: TComponent; V> = class(TControlValueObserver<T>)
  type
    TNotifyValue = TProc<V>;
  private
    FOnNotifyValue: TNotifyValue;
  strict protected
    procedure DoNotifyValue(const AValue: V);
    procedure NotifyValue(Sender: TObject); virtual; abstract;
  protected
  public
    constructor Create(ATarget: T; AOnNotifyValue: TNotifyValue); reintroduce; overload;
  end;

implementation

constructor TControlValueObserver<T>.Create(ATarget: T; AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil);
begin
  inherited Create;
  FTarget := ATarget;
  FOnModified := AOnModified;
  FOnUpdate := AOnUpdate;
end;

procedure TControlValueObserver<T>.DoValueUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Target);
end;

procedure TControlValueObserver<T>.DoValueModified;
begin
  if Assigned(FOnModified) then
    FOnModified(Target);
end;

function TControlValueObserver<T>.GetActive: Boolean;
begin
  Result := True;
end;

function TControlValueObserver<T>.GetOnObserverToggle: TObserverToggleEvent;
begin
  Result := FOnToggle;
end;

function TControlValueObserver<T>.GetTrack: Boolean;
begin
  Result := True;
end;

procedure TControlValueObserver<T>.Removed;
begin
end;

procedure TControlValueObserver<T>.SetActive(Value: Boolean);
begin
  // (to initialize properties like ReadOnly, alignment, maxlength, etc)
  if Assigned(FOnToggle) then
    FOnToggle(Self, Value);
end;

procedure TControlValueObserver<T>.SetOnObserverToggle(AEvent: TObserverToggleEvent);
begin
  FOnToggle := AEvent;
end;

procedure TControlValueObserver<T>.ValueModified;
begin
  DoValueModified;
end;

procedure TControlValueObserver<T>.ValueUpdate;
begin
  DoValueUpdate;
end;

{ TControlValueObserver<T, V> }

constructor TControlValueObserver<T, V>.Create(ATarget: T; AOnNotifyValue: TNotifyValue);
begin
  inherited Create(ATarget, NotifyValue, nil);
  FOnNotifyValue := AOnNotifyValue;
end;

procedure TControlValueObserver<T, V>.DoNotifyValue(const AValue: V);
begin
  if Assigned(FOnNotifyValue) then
    FOnNotifyValue(AValue);
end;

end.
