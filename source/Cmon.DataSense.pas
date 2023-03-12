unit Cmon.DataSense;

interface

uses
  System.Classes, System.Generics.Collections,
  Data.DB;

type
  TDataSenseLinkClass = class of TDataSenseLink;
  TDataSenseLink = class(TDataLink)
  private
    FFieldName: string;
    FTarget: TComponent;
    procedure SetFieldName(const Value: string);
  strict protected
    procedure DoActiveChanged(Value: Boolean); virtual;
    procedure DoEditingChanged(Value: Boolean); virtual;
    procedure DoLoadData; virtual;
    procedure DoSaveData; virtual;
    function GetEditable: Boolean; virtual;
    procedure SetEditable(const Value: Boolean); virtual;
  protected
    procedure ActiveChanged; override;
    procedure AssertTargetType<T: TComponent>(AClass: TClass);
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure Toggle(Value: Boolean); virtual;
    procedure UpdateField; virtual;
    procedure UpdateRightToLeft; virtual;
  public
    constructor Create(ATarget: TComponent); virtual;
    procedure RemoveTarget;
    property Editable: Boolean read GetEditable write SetEditable;
    property FieldName: string read FFieldName write SetFieldName;
    property Target: TComponent read FTarget;
  end;

type
  TDataSenseFieldLink = class;
  TDataSenseObserver = class(TInterfacedPersistent, IObserver, IObserverTrack, ISingleCastObserver, IEditLinkObserver)
    procedure IEditLinkObserver.Modified = ModifiedImpl;
  private
    FLink: TDataSenseFieldLink;
    FModified: Boolean;
    FOnToggle: TObserverToggleEvent;
    FUpdateCnt: Integer;
  strict protected
    FActive: Boolean;
  protected
    procedure BeginUpdate;
    function Edit: Boolean;
    procedure EndUpdate;
    function GetActive: Boolean;
{$IF CompilerVersion >= 34.0 Delphi 10.4 Sydney }
    function GetFormatLink: IEditFormatLink;
{$IFEND}
    function GetIsEditing: Boolean;
    function GetIsReadOnly: Boolean;
    function GetOnObserverToggle: TObserverToggleEvent;
    function GetTrack: Boolean;
    function GetUpdating: Boolean;
    function IsModified: Boolean;
    function IsRequired: Boolean;
    function IsValidChar(AKey: Char): Boolean;
    procedure ModifiedImpl;
    procedure Removed;
    procedure Reset;
    procedure SetActive(Value: Boolean);
    procedure SetIsReadOnly(Value: Boolean);
    procedure SetOnObserverToggle(AEvent: TObserverToggleEvent);
    procedure Toggle(Value: Boolean);
    procedure Update;
  public
    constructor Create(ALink: TDataSenseFieldLink);
    property Active: Boolean read FActive write FActive;
    property Modified: Boolean read FModified write FModified;
  end;

  TDataSenseFieldLink = class(TDataSenseLink)
  private
    FEditable: Boolean;
    FField: TField;
    FObserver: TDataSenseObserver;
    function GetCanModify: Boolean;
    function GetModified: Boolean;
    function GetObserver: TDataSenseObserver;
    procedure SetField(Value: TField);
    procedure SetModified(const Value: Boolean);
  strict protected
    function GetEditable: Boolean; override;
    procedure SetEditable(const Value: Boolean); override;
  protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure EditingChanged; override;
    function IsRequired: Boolean;
    function IsValidChar(AKey: Char): Boolean;
    procedure RecordChanged(Field: TField); override;
    procedure Reset;
    procedure Toggle(Value: Boolean); override;
    procedure UpdateData; override;
    procedure UpdateField; override;
    property CanModify: Boolean read GetCanModify;
    property Field: TField read FField;
  public
    destructor Destroy; override;
    property Modified: Boolean read GetModified write SetModified;
    property Observer: TDataSenseObserver read GetObserver;
  end;

type
  TDataSenseItem = class(TCollectionItem)
  type
    TNexus = class(TComponent)
    private
      FItem: TDataSenseItem;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AItem: TDataSenseItem); reintroduce;
    end;
  private
    FDataField: string;
    FDataLink: TDataSenseLink;
    FNexus: TNexus;
    FDataSource: TDataSource;
    function GetLinkedTo: string;
    function GetTarget: TComponent;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetTarget(const Value: TComponent);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure RemoveTarget;
  published
    property DataField: string read FDataField write SetDataField;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property LinkedTo: string read GetLinkedTo;
    property Target: TComponent read GetTarget write SetTarget;
  end;

  TDataSenseCollection = class(TOwnedCollection)
  public
    constructor Create(AOwner: TComponent);
  end;

type
  TDataSense = class(TComponent)
  strict private
  type
    TDataLinkRegistry = TDictionary<TClass, TDataSenseLinkClass>;
  class var
    FRegistry: TDataLinkRegistry;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    FDataLinks: TDataSenseCollection;
    procedure SetDataLinks(const Value: TDataSenseCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function CreateDataLink(ATarget: TComponent): TDataSenseLink;
    function FindDataSenseItem(ATarget: TComponent): TDataSenseItem;
    class function FindLinkClass(ATarget: TComponent): TDataSenseLinkClass;
    function AddDataSenseItem(ATarget: TComponent): TDataSenseItem;
    class procedure RegisterLinkClass(AClass: TComponentClass; ALinkClass: TDataSenseLinkClass);
    class function SupportsLinking(ATarget: TComponent): Boolean;
    class procedure UnregisterLinkClass(AClass: TComponentClass; ALinkClass: TDataSenseLinkClass);
  published
    property DataLinks: TDataSenseCollection read FDataLinks write SetDataLinks;
  end;

implementation

uses
  System.SysUtils,
  Cmon.Utilities;

constructor TDataSenseLink.Create(ATarget: TComponent);
begin
  inherited Create;
  FTarget := ATarget;
  { Try to add an Observer to Control. If it fails, Editable stays False. }
  Editable := True;
end;

procedure TDataSenseLink.ActiveChanged;
begin
  inherited;
  UpdateField;
  DoActiveChanged(Active);
end;

procedure TDataSenseLink.AssertTargetType<T>(AClass: TClass);
begin
  Assert(AClass.InheritsFrom(T), Format('Target must be %s', [T.ClassName]));
end;

procedure TDataSenseLink.DoActiveChanged(Value: Boolean);
begin
end;

procedure TDataSenseLink.DoEditingChanged(Value: Boolean);
begin
end;

procedure TDataSenseLink.DoLoadData;
begin
end;

procedure TDataSenseLink.DoSaveData;
begin
end;

procedure TDataSenseLink.EditingChanged;
begin
  DoEditingChanged(Editing);
end;

procedure TDataSenseLink.RemoveTarget;
begin
  FTarget := nil;
end;

function TDataSenseLink.GetEditable: Boolean;
begin
  Result := False;
end;

procedure TDataSenseLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TDataSenseLink.SetEditable(const Value: Boolean);
begin
end;

procedure TDataSenseLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then begin
    FFieldName := Value;
    UpdateField;
  end;
end;

procedure TDataSenseLink.Toggle(Value: Boolean);
begin
end;

procedure TDataSenseLink.UpdateField;
begin
end;

procedure TDataSenseLink.UpdateRightToLeft;
begin
end;

constructor TDataSenseItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FNexus := TNexus.Create(Self);
end;

destructor TDataSenseItem.Destroy;
begin
  FDataLink.Free;
  FNexus.Free;
  inherited Destroy;
end;

procedure TDataSenseItem.RemoveTarget;
begin
  if FDataLink <> nil then
    FDataLink.RemoveTarget;
end;

function TDataSenseItem.GetLinkedTo: string;
begin
  Result := '';
  if Target <> nil then
    Result := Target.Name;
end;

function TDataSenseItem.GetTarget: TComponent;
begin
  Result := nil;
  if FDataLink <> nil then begin
    Result := FDataLink.Target;
  end;
end;

procedure TDataSenseItem.SetDataField(const Value: string);
begin
  if FDataField <> Value then
  begin
    FDataField := Value;
    if FDataLink <> nil then
      FDataLink.FieldName := FDataField;
  end;
end;

procedure TDataSenseItem.SetDataSource(Value: TDataSource);
begin
  if FDataSource <> Value then
  begin
    if FDataSource <> nil then FDataSource.RemoveFreeNotification(FNexus);
    FDataSource := Value;
    if FDataSource <> nil then FDataSource.FreeNotification(FNexus);
    if FDataLink <> nil then
      FDataLink.DataSource := FDataSource;
  end;
end;

procedure TDataSenseItem.SetTarget(const Value: TComponent);
begin
  if (FDataLink <> nil) and (FDataLink.Target <> Value) then begin
    FDataLink.Free;
    FDataLink := nil;
  end;
  if (Value <> nil) and (FDataLink = nil) then begin
    FDataLink := TDataSense.CreateDataLink(Value);
    if FDataLink = nil then
      raise Exception.CreateFmt('Cannot creare DataLink for component of type %s', [Value.ClassName]);
    FDataLink.DataSource := FDataSource;
    FDataLink.FieldName := FDataField;
  end;
end;

constructor TDataSenseCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TDataSenseItem);
end;

constructor TDataSense.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLinks := TDataSenseCollection.Create(Self);
end;

class constructor TDataSense.CreateClass;
begin
  FRegistry := TDataLinkRegistry.Create;
end;

destructor TDataSense.Destroy;
begin
  FDataLinks.Free;
  inherited Destroy;
end;

class destructor TDataSense.DestroyClass;
begin
  FRegistry.Free;
end;

class function TDataSense.CreateDataLink(ATarget: TComponent): TDataSenseLink;
var
  linkClass: TDataSenseLinkClass;
begin
  Result := nil;
  linkClass := FindLinkClass(ATarget);
  if linkClass <> nil then
    Result := linkClass.Create(ATarget);
end;

function TDataSense.FindDataSenseItem(ATarget: TComponent): TDataSenseItem;
begin
  Result := nil;
  if ATarget = nil then Exit;

  for var item in DataLinks.ItemsOf<TDataSenseItem> do begin
    if item.Target = ATarget then
      Exit(item);
  end;
end;

class function TDataSense.FindLinkClass(ATarget: TComponent): TDataSenseLinkClass;
var
  linkClass: TDataSenseLinkClass;
begin
  Result := nil;
  var cls := ATarget.ClassType;
  while not cls.ClassNameIs(TComponent.ClassName) do begin
    if FRegistry.TryGetValue(cls, linkClass) then
      Exit(linkClass);
    cls := cls.ClassParent;
  end;
end;

function TDataSense.AddDataSenseItem(ATarget: TComponent): TDataSenseItem;
begin
  Result := FindDataSenseItem(ATarget);
  if Result = nil then begin
    Result := DataLinks.Add as TDataSenseItem;
    Result.Target := ATarget;
    ATarget.FreeNotification(Self);
  end;
end;

procedure TDataSense.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and not (csDestroying in ComponentState) then begin
    var link := FindDataSenseItem(AComponent);
    if link <> nil then begin
      link.RemoveTarget;
      link.Free;
    end;
  end
end;

class procedure TDataSense.RegisterLinkClass(AClass: TComponentClass; ALinkClass: TDataSenseLinkClass);
begin
  FRegistry.AddOrSetValue(AClass, ALinkClass);
end;

procedure TDataSense.SetDataLinks(const Value: TDataSenseCollection);
begin
  FDataLinks.Assign(Value);
end;

class function TDataSense.SupportsLinking(ATarget: TComponent): Boolean;
begin
  Result := FindLinkClass(ATarget) <> nil;
end;

class procedure TDataSense.UnregisterLinkClass(AClass: TComponentClass; ALinkClass: TDataSenseLinkClass);
var
  linkClass: TDataSenseLinkClass;
begin
  if FRegistry.TryGetValue(AClass, linkClass) then begin
    if linkClass = ALinkClass then
      FRegistry.Remove(AClass);
  end;
end;

constructor TDataSenseItem.TNexus.Create(AItem: TDataSenseItem);
begin
  inherited Create(nil);
  FItem := AItem;
end;

procedure TDataSenseItem.TNexus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if (AComponent = FItem.DataSource) then begin
      FItem.DataSource := nil;
    end;
  end
end;

destructor TDataSenseFieldLink.Destroy;
begin
  SetEditable(False);
  FObserver.Free;
  inherited;
end;

procedure TDataSenseFieldLink.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  inherited;
  if Event = deDisabledStateChange then begin
    if Boolean(Info) then
      UpdateField
    else
      FField := nil;
  end;
end;

procedure TDataSenseFieldLink.EditingChanged;
begin
  Observer.Modified := False;
  inherited;
end;

function TDataSenseFieldLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
end;

function TDataSenseFieldLink.GetEditable: Boolean;
begin
  Result := FEditable;
end;

function TDataSenseFieldLink.GetModified: Boolean;
begin
  Result := Observer.Modified;
end;

function TDataSenseFieldLink.GetObserver: TDataSenseObserver;
begin
  if FObserver = nil then
    FObserver := TDataSenseObserver.Create(Self);
  Result := FObserver;
end;

function TDataSenseFieldLink.IsRequired: Boolean;
begin
  Result := (Field <> nil) and Field.Required;
end;

function TDataSenseFieldLink.IsValidChar(AKey: Char): Boolean;
begin
  Result := True;
  if Field <> nil then begin
    Result := Field.IsValidChar(AKey);
  end;
end;

procedure TDataSenseFieldLink.RecordChanged(Field: TField);
begin
  inherited;
  if (Field = nil) or (Field = FField) then begin
    DoLoadData;
    Observer.Modified := False;
  end;
end;

procedure TDataSenseFieldLink.Reset;
begin
  RecordChanged(nil);
end;

procedure TDataSenseFieldLink.SetEditable(const Value: Boolean);
begin
  if (Target = nil) or (csDestroying in Target.ComponentState) then begin
    Observer.Active := False;
    FEditable := False;
    Exit;
  end;

  if Value <> FEditable then begin
    if Value then begin
      if Target.Observers.CanObserve(TObserverMapping.EditLinkID) then begin
        Target.Observers.AddObserver(TObserverMapping.EditLinkID, Observer);
        Observer.Active := True;
        FEditable := True;
      end;
    end
    else begin
      Target.Observers.RemoveObserver(TObserverMapping.EditLinkID, Observer);
      Observer.Active := False;
      FEditable := False;
    end;
  end;
end;

procedure TDataSenseFieldLink.SetField(Value: TField);
begin
  if FField <> Value then begin
    FField := Value;
    if (Dataset = nil) or not Dataset.ControlsDisabled then begin
      EditingChanged;
      RecordChanged(nil);
      UpdateRightToLeft;
    end;
  end;
end;

procedure TDataSenseFieldLink.SetModified(const Value: Boolean);
begin
  Observer.Modified := Value;
end;

procedure TDataSenseFieldLink.Toggle(Value: Boolean);
begin
  Observer.Toggle(Value);
end;

procedure TDataSenseFieldLink.UpdateData;
begin
  if Editable and Observer.Modified then begin
    DoSaveData;
    Observer.Modified := False;
  end;
end;

procedure TDataSenseFieldLink.UpdateField;
begin
  if Active and (FFieldName <> '') then begin
    FField := nil;
    if Assigned(FTarget) then
      SetField(GetFieldProperty(DataSource.Dataset, FTarget, FFieldName))
    else
      SetField(DataSource.Dataset.FieldByName(FFieldName));
  end
  else
    SetField(nil);
end;

constructor TDataSenseObserver.Create(ALink: TDataSenseFieldLink);
begin
  inherited Create;
  FLink := ALink;
end;

procedure TDataSenseObserver.BeginUpdate;
begin
  Inc(FUpdateCnt);
end;

function TDataSenseObserver.Edit: Boolean;
begin
  Result := False;
  if FLink.CanModify then
    Result := FLink.Edit;
end;

procedure TDataSenseObserver.EndUpdate;
begin
  Dec(FUpdateCnt);
end;

function TDataSenseObserver.GetActive: Boolean;
begin
  Result := FActive;
end;

{$IF CompilerVersion >= 34.0 Delphi 10.4 Sydney }
function TDataSenseObserver.GetFormatLink: IEditFormatLink;
begin
  Result := nil;
end;
{$IFEND}

function TDataSenseObserver.GetIsEditing: Boolean;
begin
  Result := FLink.Editing;
end;

function TDataSenseObserver.GetIsReadOnly: Boolean;
begin
  Result := FLink.ReadOnly;
end;

function TDataSenseObserver.GetOnObserverToggle: TObserverToggleEvent;
begin
  Result := FOnToggle;
end;

function TDataSenseObserver.GetTrack: Boolean;
begin
  Result := Active and FLink.Editing;
end;

function TDataSenseObserver.GetUpdating: Boolean;
begin
  Result := FUpdateCnt > 0;
end;

function TDataSenseObserver.IsModified: Boolean;
begin
  Result := FModified;
end;

function TDataSenseObserver.IsRequired: Boolean;
begin
  Result := FLink.IsRequired;
end;

function TDataSenseObserver.IsValidChar(AKey: Char): Boolean;
begin
  Result := FLink.IsValidChar(AKey);
end;

procedure TDataSenseObserver.ModifiedImpl;
begin
  FModified := True;
end;

procedure TDataSenseObserver.Removed;
begin
end;

procedure TDataSenseObserver.Reset;
begin
  FLink.Reset;
end;

procedure TDataSenseObserver.SetActive(Value: Boolean);
begin
  FActive := Value;
  if Assigned(FOnToggle) then
    FOnToggle(Self, Value);
end;

procedure TDataSenseObserver.SetIsReadOnly(Value: Boolean);
begin
  FLink.ReadOnly := Value;
end;

procedure TDataSenseObserver.SetOnObserverToggle(AEvent: TObserverToggleEvent);
begin
  FOnToggle := AEvent;
end;

procedure TDataSenseObserver.Toggle(Value: Boolean);
begin
  if Assigned(FOnToggle) then FOnToggle(Self, Value);
end;

procedure TDataSenseObserver.Update;
begin
  FLink.UpdateData;
  FModified := False;
end;

initialization
  RegisterClasses([TDataSense]);
finalization
  UnRegisterClasses([TDataSense]);
end.
