unit Cmon.Observers;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  IValueObserver = interface
    ['{17C655DA-2BF8-4BFE-810F-26DD0DBD58C4}']
    function GetTracking: Boolean;
    procedure SetTracking(const Value: Boolean);
    procedure ValueChanged;
    property Tracking: Boolean read GetTracking write SetTracking;
  end;

type
{$SCOPEDENUMS ON}
  TObserverPriority = (VeryEarly, Early, Normal, Late, VeryLate);
{$SCOPEDENUMS OFF}

  IObserverPriority = interface
    ['{254BF223-348B-440A-A596-D9357BA78D32}']
    function GetPriority: TObserverPriority;
    property Priority: TObserverPriority read GetPriority;
  end;

type
  TObserversHelper = class helper for TObservers
  public
    procedure SortObservers(AID: Integer);
  end;

type
  TBaseObserver = class(TInterfacedObject, IObserver, IObserverPriority)
  private
    FActive: Boolean;
    FOnToggle: TObserverToggleEvent;
    FPriority: TObserverPriority;
  strict protected
    function GetActive: Boolean; virtual;
    function GetOnObserverToggle: TObserverToggleEvent; virtual;
    function GetPriority: TObserverPriority;
    procedure Removed; virtual;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetOnObserverToggle(AEvent: TObserverToggleEvent); virtual;
  public
    constructor Create(APriority: TObserverPriority = TObserverPriority.Normal);
  end;

type
  TControlValueObserver<T: TComponent> = class(TBaseObserver, IObserver, IMultiCastObserver, IControlValueObserver, IObserverTrack)
  private
    FOnModified: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    FTarget: T;
  strict protected
    function GetTrack: Boolean; virtual;
    procedure ValueModified; virtual;
    procedure ValueUpdate; virtual;
  protected
    procedure DoValueModified;
    procedure DoValueUpdate;
  public
    constructor Create(ATarget: T; AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil; APriority: TObserverPriority =
        TObserverPriority.Normal);
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
  public
    constructor Create(ATarget: T; AOnNotifyValue: TNotifyValue; APriority: TObserverPriority = TObserverPriority.Normal);
        reintroduce; overload;
  end;

type
  TValueObservers<T: class> = class(TObservers)
  strict private
    FKnownIDs: TList<Integer>;
  private
    FTarget: T;
  strict protected
    function MakeGetValue<V>(const APropName: string): TFunc<T, V>;
    procedure ValueChanged(AIntf: IInterface); overload;
    property KnownIDs: TList<Integer> read FKnownIDs;
  public
    constructor Create(ATarget: T);
    destructor Destroy; override;
    procedure AddObserver(const IDs: Array of Integer; const AIntf: IInterface); overload; override;
    procedure AddObserver(const Key: string; const AIntf: IInterface); overload;
    procedure AddObserver<V>(const APropName: string; AOnValueChanged: TProc<V>); overload;
    procedure AddObserver<V>(const APropName: string; AOnValueChanged: TProc<T,V>); overload;
    function CanObserve(const ID: Integer): Boolean; override;
    function GetObserverID(const Key: string): Integer;
    procedure ValueChanged(ID: Integer); overload;
    procedure ValueChanged(const Key: string); overload;
    property Target: T read FTarget;
  end;

type
  TValueObserver<T: class; V> = class(TBaseObserver, IMultiCastObserver, IValueObserver)
  type
    TNotifyValue = TProc<V>;
    TNotifyValueExt = TProc<T, V>;
    TGetValue = TFunc<T, V>;
  strict private
    function DoGetValue: V;
  private
    FOnNotifyValue: TNotifyValue;
    FOnNotifyValueExt: TNotifyValueExt;
    FOnGetValue: TGetValue;
    FTarget: T;
    FTracking: Boolean;
  strict protected
    procedure DoNotifyValue(const AValue: V);
    function GetTracking: Boolean; virtual;
    procedure SetTracking(const Value: Boolean); virtual;
    procedure ValueChanged; virtual;
  public
    constructor Create(ATarget: T; AOnNotifyValue: TNotifyValue; AOnGetValue: TGetValue; ATracking: Boolean = True; APriority:
        TObserverPriority = TObserverPriority.Normal); overload;
    constructor Create(ATarget: T; AOnNotifyValue: TNotifyValueExt; AOnGetValue: TGetValue; ATracking: Boolean = True; APriority:
        TObserverPriority = TObserverPriority.Normal); overload;
    property Target: T read FTarget;
  end;

implementation

uses
  System.Rtti;

constructor TControlValueObserver<T>.Create(ATarget: T; AOnUpdate, AOnModified: TNotifyEvent; APriority: TObserverPriority);
begin
  inherited Create(APriority);
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

function TControlValueObserver<T>.GetTrack: Boolean;
begin
  Result := True;
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

constructor TControlValueObserver<T, V>.Create(ATarget: T; AOnNotifyValue: TNotifyValue; APriority: TObserverPriority);
begin
  inherited Create(ATarget, NotifyValue, nil, APriority);
  FOnNotifyValue := AOnNotifyValue;
end;

procedure TControlValueObserver<T, V>.DoNotifyValue(const AValue: V);
begin
  if Assigned(FOnNotifyValue) then
    FOnNotifyValue(AValue);
end;

constructor TBaseObserver.Create(APriority: TObserverPriority = TObserverPriority.Normal);
begin
  inherited Create;
  FActive := True;
  FPriority := APriority;
end;

function TBaseObserver.GetActive: Boolean;
begin
  Result := FActive;
end;

function TBaseObserver.GetOnObserverToggle: TObserverToggleEvent;
begin
  Result := FOnToggle;
end;

function TBaseObserver.GetPriority: TObserverPriority;
begin
  Result := FPriority;
end;

procedure TBaseObserver.Removed;
begin
end;

procedure TBaseObserver.SetActive(Value: Boolean);
begin
  FActive := Value;
  // (to initialize properties like ReadOnly, alignment, maxlength, etc)
  if Assigned(FOnToggle) then
    FOnToggle(Self, Value);
end;

procedure TBaseObserver.SetOnObserverToggle(AEvent: TObserverToggleEvent);
begin
  FOnToggle := AEvent;
end;

constructor TValueObservers<T>.Create(ATarget: T);
begin
  inherited Create;
  FKnownIDs := TList<Integer>.Create;
  FTarget := ATarget;
end;

destructor TValueObservers<T>.Destroy;
begin
  FKnownIDs.Free;
  inherited;
end;

procedure TValueObservers<T>.AddObserver(const IDs: array of Integer; const AIntf: IInterface);
begin
  inherited AddObserver(IDs, AIntf);
  ValueChanged(AIntf);
end;

procedure TValueObservers<T>.AddObserver(const Key: string; const AIntf: IInterface);
begin
  AddObserver(GetObserverID(Key), AIntf);
end;

procedure TValueObservers<T>.AddObserver<V>(const APropName: string; AOnValueChanged: TProc<V>);
begin
  AddObserver(APropName, TValueObserver<T, V>.Create(Target, AOnValueChanged, MakeGetValue<V>(APropName)));
end;

procedure TValueObservers<T>.AddObserver<V>(const APropName: string; AOnValueChanged: TProc<T,V>);
begin
  AddObserver(APropName, TValueObserver<T, V>.Create(Target, AOnValueChanged, MakeGetValue<V>(APropName)));
end;

function TValueObservers<T>.CanObserve(const ID: Integer): Boolean;
begin
  Result := KnownIDs.Contains(ID);
end;

function TValueObservers<T>.GetObserverID(const Key: string): Integer;
begin
  Result := TObserverMapping.GetObserverID(Format('%s.%s', [T.ClassName, Key]));
  if not KnownIDs.Contains(Result) then
    KnownIDs.Add(Result);
end;

function TValueObservers<T>.MakeGetValue<V>(const APropName: string): TFunc<T, V>;
begin
  Result :=
    function(Source: T):V
    begin
      var context := TRttiContext.Create;
      try
        var rttiType := context.GetType(T);
        var rttiProp := rttiType.GetProperty(APropName);
        Result := rttiProp.GetValue(Pointer(Source)).AsType<V>;
      finally
        context.Free;
      end;
    end;
end;

procedure TValueObservers<T>.ValueChanged(AIntf: IInterface);
var
  observer: IValueObserver;
begin
  if Supports(AIntf, IValueObserver, observer) then begin
    if observer.Tracking then
      observer.ValueChanged;
  end;
end;

procedure TValueObservers<T>.ValueChanged(ID: Integer);
begin
  var list := GetMultiCastObserver(ID);
  for var I := 0 to list.Count - 1 do
    ValueChanged(list[I]);
end;

procedure TValueObservers<T>.ValueChanged(const Key: string);
begin
  ValueChanged(GetObserverID(Key));
end;

constructor TValueObserver<T, V>.Create(ATarget: T; AOnNotifyValue: TNotifyValue; AOnGetValue: TGetValue; ATracking: Boolean;
    APriority: TObserverPriority);
begin
  inherited Create(APriority);
  FTarget := ATarget;
  FOnNotifyValue := AOnNotifyValue;
  FOnGetValue := AOnGetValue;
  FTracking := ATracking;
end;

constructor TValueObserver<T, V>.Create(ATarget: T; AOnNotifyValue: TNotifyValueExt; AOnGetValue: TGetValue; ATracking: Boolean;
    APriority: TObserverPriority);
begin
  inherited Create(APriority);
  FTarget := ATarget;
  FOnNotifyValueExt := AOnNotifyValue;
  FOnGetValue := AOnGetValue;
  FTracking := ATracking;
end;

function TValueObserver<T, V>.DoGetValue: V;
begin
  Result := Default(V);
  if Assigned(FOnGetValue) then
    Result := FOnGetValue(Target);
end;

procedure TValueObserver<T, V>.DoNotifyValue(const AValue: V);
begin
  if Assigned(FOnNotifyValue) then
    FOnNotifyValue(AValue);
  if Assigned(FOnNotifyValueExt) then
    FOnNotifyValueExt(Target, AValue);
end;

function TValueObserver<T,V>.GetTracking: Boolean;
begin
  Result := FTracking;
end;

procedure TValueObserver<T,V>.SetTracking(const Value: Boolean);
begin
  FTracking := Value;
end;

procedure TValueObserver<T, V>.ValueChanged;
begin
  DoNotifyValue(DoGetValue);
end;

procedure TObserversHelper.SortObservers(AID: Integer);

  function Getpriority(AIntf: IInterface): TObserverPriority;
  var
    prio: IObserverPriority;
  begin
    Result := TObserverPriority.Normal;
    if Supports(AIntf, IObserverPriority, prio) then
      Result := prio.Priority;
  end;

var
  lastPrio: TObserverPriority;
  list: IInterfaceList;
  needsSort: Boolean;
  prioLists: array[TObserverPriority] of IInterfaceList;
begin
  needsSort := False;
  lastPrio := Low(TObserverPriority);
  list := GetMultiCastObserver(AID);
  for var I := 0 to list.Count - 1 do begin
    var prio := GetPriority(list[I]);
    if prio < lastPrio then
      needsSort := True
    else
      lastPrio := prio;
    var prioList := prioLists[prio];
    if prioList = nil then begin
      prioList := TInterfaceList.Create;
      prioLists[prio] := prioList;
    end;
    prioList.Add(list[I]);
  end;
  if needsSort then begin
    var saveEvent := OnObserverAdded;
    try
      for var I := 0 to list.Count - 1 do
        RemoveObserver(AID, list[I]);
      for var prio := Low(prioLists) to High(prioLists) do begin
        var prioList := prioLists[prio];
        if Assigned(prioList) then begin
          for var I := 0 to prioList.Count - 1 do
            AddObserver(AID, prioList[I]);
        end;
      end;
    finally
      OnObserverAdded := saveEvent;
    end;
Hurz  end;
end;

end.
