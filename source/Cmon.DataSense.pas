unit Cmon.DataSense;

interface

uses
  System.Classes, System.Generics.Collections,
  Data.DB;

type
  TDataSenseLinkClass = class of TDataSenseLink;
  TDataSenseLink = class(TDataLink, IInterface, IObserver)
  private
    FFieldName: string;
    FTarget: TComponent;
    FOnToggle: TObserverToggleEvent;
    procedure SetFieldName(const Value: string);
  strict protected
    FObserverActive: Boolean;
    FObserverModified: Boolean;
    procedure DoActiveChanged(Value: Boolean); virtual;
    procedure DoEditingChanged(Value: Boolean); virtual;
    procedure DoLoadData; virtual;
    procedure DoSaveData; virtual;
  protected
    procedure ActiveChanged; override;
    procedure AddObserver; virtual;
    procedure EditingChanged; override;
    function GetActive: Boolean;
    function GetOnObserverToggle: TObserverToggleEvent;
    function GetTrack: Boolean;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    procedure Removed;
    procedure RemoveObserver; virtual;
    procedure SetActive(Value: Boolean);
    procedure SetOnObserverToggle(AEvent: TObserverToggleEvent);
    procedure Toggle(Value: Boolean); virtual;
    procedure UpdateField; virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(ATarget: TComponent);
    destructor Destroy; override;
    property FieldName: string read FFieldName write SetFieldName;
    property Target: TComponent read FTarget;
  end;

  TDataSenseFieldLink = class(TDataSenseLink)
  private
    FField: TField;
    function GetCanModify: Boolean;
    procedure SetField(Value: TField);
  protected
    procedure ActiveChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    procedure UpdateField; override;
    procedure UpdateRightToLeft; virtual;
  public
    property CanModify: Boolean read GetCanModify;
    property Field: TField read FField;
  end;

  TDataSenseDisplayLink = class(TDataSenseFieldLink);

  TDataSenseEditLink = class(TDataSenseFieldLink, IInterface, IObserver, IObserverTrack, ISingleCastObserver, IEditLinkObserver)
  private
    FUpdateCnt: Integer;
  protected
    procedure AddObserver; override;
    procedure BeginUpdate;
    function Edit: Boolean;
    procedure EndUpdate;
    {$IF CompilerVersion >= 34.0 Delphi 10.4 Sydney }
    function GetFormatLink: IEditFormatLink;
    {$IFEND}
    function GetIsEditing: Boolean;
    function GetIsReadOnly: Boolean;
    function GetUpdating: Boolean;
    function IsModified: Boolean;
    function IsRequired: Boolean;
    function IsValidChar(AKey: Char): Boolean;
    procedure Modified;
    procedure RemoveObserver; override;
    procedure Reset;
    procedure SetIsReadOnly(Value: Boolean);
    procedure Update;
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
    function FindDataLink(ATarget: TComponent): TDataSenseItem;
    procedure SetDataLinks(const Value: TDataSenseCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function CreateDataLink(ATarget: TComponent): TDataSenseLink;
    function FindOrCreateDataLink(ATarget: TComponent; ADataSource: TDataSource = nil; const ADataField: string = ''): TDataSenseItem;
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
  if FTarget <> nil then begin
    AddObserver;
    FObserverActive := True;
  end;
end;

destructor TDataSenseLink.Destroy;
begin
  if FTarget <> nil then begin
    RemoveObserver;
  end;
  inherited;
end;

procedure TDataSenseLink.ActiveChanged;
begin
  inherited;
  DoActiveChanged(Active);
end;

procedure TDataSenseLink.AddObserver;
begin
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
  FObserverModified := False;
  DoEditingChanged(Editing);
end;

function TDataSenseLink.GetActive: Boolean;
begin
  Result := FObserverActive;
end;

function TDataSenseLink.GetOnObserverToggle: TObserverToggleEvent;
begin
  Result := FOnToggle;
end;

function TDataSenseLink.GetTrack: Boolean;
begin
  Result := Active and Editing;
end;

function TDataSenseLink.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

procedure TDataSenseLink.Removed;
begin
end;

procedure TDataSenseLink.RemoveObserver;
begin
end;

procedure TDataSenseLink.SetActive(Value: Boolean);
begin
  FObserverActive := Value;
  if Assigned(FOnToggle) then
    FOnToggle(Self, Value);
end;

procedure TDataSenseLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then begin
    FFieldName := Value;
    UpdateField;
  end;
end;

procedure TDataSenseLink.SetOnObserverToggle(AEvent: TObserverToggleEvent);
begin
  FOnToggle := AEvent;
end;

procedure TDataSenseLink.Toggle(Value: Boolean);
begin
  if Assigned(FOnToggle) then FOnToggle(Self, Value);
end;

procedure TDataSenseLink.UpdateField;
begin
end;

function TDataSenseLink._AddRef: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

function TDataSenseLink._Release: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

constructor TDataSenseItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FNexus := TNexus.Create(Self);
end;

destructor TDataSenseItem.Destroy;
begin
  DataSource := nil;
  Target := nil;
  FNexus.Free;
  inherited Destroy;
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
  var cls := ATarget.ClassType;
  while not cls.ClassNameIs(TComponent.ClassName) do begin
    if FRegistry.TryGetValue(cls, linkClass) then
      Exit(linkClass.Create(ATarget));
    cls := cls.ClassParent;
  end;
end;

function TDataSense.FindDataLink(ATarget: TComponent): TDataSenseItem;
begin
  Result := nil;
  if ATarget = nil then Exit;

  for var item in DataLinks.ItemsOf<TDataSenseItem> do begin
    if item.Target = ATarget then
      Exit(item);
  end;
end;

function TDataSense.FindOrCreateDataLink(ATarget: TComponent; ADataSource: TDataSource = nil; const ADataField: string = ''): TDataSenseItem;
begin
  Result := FindDataLink(ATarget);
  if Result = nil then begin
    Result := DataLinks.Add as TDataSenseItem;
    Result.Target := ATarget;
    Result.DataSource := ADataSource;
    Result.DataField := ADataField;
    ATarget.FreeNotification(Self);
  end;
end;

procedure TDataSense.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    var link := FindDataLink(AComponent);
    link.Free;
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
  Result := True;
  var cls := ATarget.ClassType;
  while not cls.ClassNameIs(TComponent.ClassName) do begin
    if FRegistry.ContainsKey(cls) then
      Exit;
    cls := cls.ClassParent;
  end;
  Result := False;
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

procedure TDataSenseEditLink.AddObserver;
begin
  Target.Observers.AddObserver(TObserverMapping.EditLinkID, Self);
end;

procedure TDataSenseEditLink.BeginUpdate;
begin
  Inc(FUpdateCnt);
end;

function TDataSenseEditLink.Edit: Boolean;
begin
  if CanModify then
    inherited Edit;
  Result := Editing;
end;

procedure TDataSenseEditLink.EndUpdate;
begin
  Dec(FUpdateCnt);
end;

{$IF CompilerVersion >= 34.0 Delphi 10.4 Sydney }
function TDataSenseEditLink.GetFormatLink: IEditFormatLink;
begin
  Result := nil;
end;
{$IFEND}

function TDataSenseEditLink.GetIsEditing: Boolean;
begin
  Result := Editing;
end;

function TDataSenseEditLink.GetIsReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

function TDataSenseEditLink.GetUpdating: Boolean;
begin
  Result := FUpdateCnt > 0;
end;

function TDataSenseEditLink.IsModified: Boolean;
begin
  Result := FObserverModified;
end;

function TDataSenseEditLink.IsRequired: Boolean;
begin
  Result := (Field <> nil) and Field.Required;
end;

function TDataSenseEditLink.IsValidChar(AKey: Char): Boolean;
begin
  Result := True;
  if Field <> nil then begin
    Result := Field.IsValidChar(AKey);
  end;
end;

procedure TDataSenseEditLink.Modified;
begin
  FObserverModified := True;
end;

procedure TDataSenseEditLink.RemoveObserver;
begin
  Target.Observers.RemoveObserver(TObserverMapping.EditLinkID, Self);
end;

procedure TDataSenseEditLink.Reset;
begin
  RecordChanged(nil);
end;

procedure TDataSenseEditLink.SetIsReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TDataSenseEditLink.Update;
begin
  UpdateData;
  FObserverModified := False;
end;

procedure TDataSenseFieldLink.ActiveChanged;
begin
  UpdateField;
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

function TDataSenseFieldLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
end;

procedure TDataSenseFieldLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TDataSenseFieldLink.RecordChanged(Field: TField);
begin
  inherited;
  if (Field = nil) or (Field = FField) then begin
    DoLoadData;
    FObserverModified := False;
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

procedure TDataSenseFieldLink.UpdateData;
begin
  if FObserverModified then begin
    if (Field <> nil) then
      DoSaveData;
    FObserverModified := False;
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

procedure TDataSenseFieldLink.UpdateRightToLeft;
begin
end;

initialization
  RegisterClasses([TDataSense]);
finalization
  UnRegisterClasses([TDataSense]);
end.
