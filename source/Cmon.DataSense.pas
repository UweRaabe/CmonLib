unit Cmon.DataSense;

interface

uses
  System.Classes, System.Generics.Collections,
  Data.DB;

type
  TCustomObserverDataLinkClass = class of TCustomObserverDataLink;
  TCustomObserverDataLink = class(TDataLink, IInterface, IObserver, IObserverTrack, ISingleCastObserver, IEditLinkObserver)
  private
    FTarget: TComponent;
    FField: TField;
    FFieldName: string;
    FObserverActive: Boolean;
    FObserverModified: Boolean;
    FOnToggle: TObserverToggleEvent;
    FUpdateCnt: Integer;
    procedure SetField(Value: TField);
    procedure UpdateField;
    function GetCanModify: Boolean;
    procedure SetFieldName(const Value: string);
  strict protected
    procedure DoActiveChanged(Value: Boolean); virtual;
    procedure DoEditingChanged(Value: Boolean); virtual;
    procedure DoLoadData; virtual;
    procedure DoSaveData; virtual;
  protected
    procedure ActiveChanged; override;
    procedure BeginUpdate;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    function Edit: Boolean;
    procedure EditingChanged; override;
    procedure EndUpdate;
    function GetActive: Boolean;
    function GetIsEditing: Boolean;
    function GetIsReadOnly: Boolean;
    function GetOnObserverToggle: TObserverToggleEvent;
    function GetTrack: Boolean;
    function GetUpdating: Boolean;
    {$IF CompilerVersion >= 34.0 Delphi 10.4 Sydney }
    function GetFormatLink: IEditFormatLink;
    {$IFEND}
    function IsModified: Boolean;
    function IsRequired: Boolean;
    function IsValidChar(AKey: Char): Boolean;
    procedure LayoutChanged; override;
    procedure Modified;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    procedure RecordChanged(Field: TField); override;
    procedure Removed;
    procedure Reset;
    procedure SetActive(Value: Boolean);
    procedure SetIsReadOnly(Value: Boolean);
    procedure SetOnObserverToggle(AEvent: TObserverToggleEvent);
    procedure Toggle(Value: Boolean); virtual;
    procedure Update;
    procedure UpdateData; override;
    procedure UpdateRightToLeft; virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(ATarget: TComponent);
    destructor Destroy; override;
    property CanModify: Boolean read GetCanModify;
    property Target: TComponent read FTarget;
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;
  end;

type
  TDataLinkSupport = class
  strict private
  type
    TDataLinkRegistry = TDictionary<TClass, TCustomObserverDataLinkClass>;
  class var
    FRegistry: TDataLinkRegistry;
    class constructor CreateClass;
    class destructor DestroyClass;
  public
    class function CreateDataLink(ATarget: TComponent): TCustomObserverDataLink;
    class procedure RegisterLinkClass(AClass: TComponentClass; ALinkClass: TCustomObserverDataLinkClass);
    class function SupportsLinking(ATarget: TComponent): Boolean;
    class procedure UnregisterLinkClass(AClass: TComponentClass; ALinkClass: TCustomObserverDataLinkClass);
  end;

type
  TDataLinkItem = class(TCollectionItem)
  type
    TNexus = class(TComponent)
    private
      FItem: TDataLinkItem;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AItem: TDataLinkItem); reintroduce;
    end;
  private
    FDataField: string;
    FDataLink: TCustomObserverDataLink;
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

  TDataLinkCollection = class(TOwnedCollection)
  public
    constructor Create(AOwner: TComponent);
  end;

  TDataLinkContainer = class(TComponent)
  private
    FDataLinks: TDataLinkCollection;
    function FindDataLink(ATarget: TComponent): TDataLinkItem;
    procedure SetDataLinks(const Value: TDataLinkCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindOrCreateDataLink(ATarget: TComponent; ADataSource: TDataSource = nil; const ADataField: string = ''): TDataLinkItem;
  published
    property DataLinks: TDataLinkCollection read FDataLinks write SetDataLinks;
  end;

implementation

uses
  System.SysUtils,
  Cmon.Utilities;

constructor TCustomObserverDataLink.Create(ATarget: TComponent);
begin
  inherited Create;
  FTarget := ATarget;
  if FTarget <> nil then begin
    FTarget.Observers.AddObserver(TObserverMapping.EditLinkID, Self);
    FObserverActive := True;
  end;
end;

destructor TCustomObserverDataLink.Destroy;
begin
  if FTarget <> nil then begin
    FTarget.Observers.RemoveObserver(TObserverMapping.EditLinkID, Self);
  end;
  inherited;
end;

procedure TCustomObserverDataLink.ActiveChanged;
begin
  UpdateField;
  DoActiveChanged(Active);
end;

procedure TCustomObserverDataLink.BeginUpdate;
begin
  Inc(FUpdateCnt);
end;

procedure TCustomObserverDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  inherited;
  if Event = deDisabledStateChange then begin
    if Boolean(Info) then
      UpdateField
    else
      FField := nil;
  end;
end;

procedure TCustomObserverDataLink.DoActiveChanged(Value: Boolean);
begin
end;

procedure TCustomObserverDataLink.DoEditingChanged(Value: Boolean);
begin
end;

procedure TCustomObserverDataLink.DoLoadData;
begin
end;

procedure TCustomObserverDataLink.DoSaveData;
begin
end;

function TCustomObserverDataLink.Edit: Boolean;
begin
  if CanModify then
    inherited Edit;
  Result := Editing;
end;

procedure TCustomObserverDataLink.EditingChanged;
begin
  FObserverModified := False;
  DoEditingChanged(Editing);
end;

procedure TCustomObserverDataLink.EndUpdate;
begin
  Dec(FUpdateCnt);
end;

function TCustomObserverDataLink.GetActive: Boolean;
begin
  Result := FObserverActive;
end;

function TCustomObserverDataLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
end;

{$IF CompilerVersion >= 34.0 Delphi 10.4 Sydney }
function TCustomObserverDataLink.GetFormatLink: IEditFormatLink;
begin
  Result := nil;
end;
{$IFEND}

function TCustomObserverDataLink.GetIsEditing: Boolean;
begin
  Result := Editing;
end;

function TCustomObserverDataLink.GetIsReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

function TCustomObserverDataLink.GetOnObserverToggle: TObserverToggleEvent;
begin
  Result := FOnToggle;
end;

function TCustomObserverDataLink.GetTrack: Boolean;
begin
  Result := Active and Editing;
end;

function TCustomObserverDataLink.GetUpdating: Boolean;
begin
  Result := FUpdateCnt > 0;
end;

function TCustomObserverDataLink.IsModified: Boolean;
begin
  Result := FObserverModified;
end;

function TCustomObserverDataLink.IsRequired: Boolean;
begin
  Result := (Field <> nil) and Field.Required;
end;

function TCustomObserverDataLink.IsValidChar(AKey: Char): Boolean;
begin
  Result := True;
  if Field <> nil then begin
    Result := Field.IsValidChar(AKey);
  end;
end;

procedure TCustomObserverDataLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TCustomObserverDataLink.Modified;
begin
  FObserverModified := True;
end;

function TCustomObserverDataLink.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

procedure TCustomObserverDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FField) then begin
    DoLoadData;
    FObserverModified := False;
  end;
end;

procedure TCustomObserverDataLink.Removed;
begin

end;

procedure TCustomObserverDataLink.Reset;
begin
  RecordChanged(nil);
end;

procedure TCustomObserverDataLink.SetActive(Value: Boolean);
begin
  FObserverActive := Value;
  if Assigned(FOnToggle) then
    FOnToggle(Self, Value);
end;

procedure TCustomObserverDataLink.SetField(Value: TField);
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

procedure TCustomObserverDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then begin
    FFieldName := Value;
    UpdateField;
  end;
end;

procedure TCustomObserverDataLink.SetIsReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TCustomObserverDataLink.SetOnObserverToggle(AEvent: TObserverToggleEvent);
begin
  FOnToggle := AEvent;
end;

procedure TCustomObserverDataLink.Toggle(Value: Boolean);
begin
  if Assigned(FOnToggle) then FOnToggle(Self, Value);
end;

procedure TCustomObserverDataLink.Update;
begin
  UpdateData;
  FObserverModified := False;
end;

procedure TCustomObserverDataLink.UpdateData;
begin
  if FObserverModified then begin
    if (Field <> nil) then
      DoSaveData;
    FObserverModified := False;
  end;
end;

procedure TCustomObserverDataLink.UpdateField;
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

procedure TCustomObserverDataLink.UpdateRightToLeft;
begin
end;

function TCustomObserverDataLink._AddRef: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

function TCustomObserverDataLink._Release: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

constructor TDataLinkContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLinks := TDataLinkCollection.Create(Self);
end;

destructor TDataLinkContainer.Destroy;
begin
  FDataLinks.Free;
  inherited Destroy;
end;

function TDataLinkContainer.FindDataLink(ATarget: TComponent): TDataLinkItem;
begin
  Result := nil;
  if ATarget = nil then Exit;

  for var item in DataLinks.ItemsOf<TDataLinkItem> do begin
    if item.Target = ATarget then
      Exit(item);
  end;
end;

function TDataLinkContainer.FindOrCreateDataLink(ATarget: TComponent; ADataSource: TDataSource = nil; const ADataField: string = ''): TDataLinkItem;
begin
  Result := FindDataLink(ATarget);
  if Result = nil then begin
    Result := DataLinks.Add as TDataLinkItem;
    Result.Target := ATarget;
    Result.DataSource := ADataSource;
    Result.DataField := ADataField;
    ATarget.FreeNotification(Self);
  end;
end;

procedure TDataLinkContainer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    var link := FindDataLink(AComponent);
    link.Free;
  end
end;

procedure TDataLinkContainer.SetDataLinks(const Value: TDataLinkCollection);
begin
  FDataLinks.Assign(Value);
end;

constructor TDataLinkItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FNexus := TNexus.Create(Self);
end;

destructor TDataLinkItem.Destroy;
begin
  DataSource := nil;
  Target := nil;
  FNexus.Free;
  inherited Destroy;
end;

function TDataLinkItem.GetLinkedTo: string;
begin
  Result := '';
  if Target <> nil then
    Result := Target.Name;
end;

function TDataLinkItem.GetTarget: TComponent;
begin
  Result := nil;
  if FDataLink <> nil then begin
    Result := FDataLink.Target;
  end;
end;

procedure TDataLinkItem.SetDataField(const Value: string);
begin
  if FDataField <> Value then
  begin
    FDataField := Value;
    if FDataLink <> nil then
      FDataLink.FieldName := FDataField;
  end;
end;

procedure TDataLinkItem.SetDataSource(Value: TDataSource);
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

procedure TDataLinkItem.SetTarget(const Value: TComponent);
begin
  if (FDataLink <> nil) and (FDataLink.Target <> Value) then begin
    FDataLink.Free;
    FDataLink := nil;
  end;
  if (Value <> nil) and (FDataLink = nil) then begin
    FDataLink := TDataLinkSupport.CreateDataLink(Value);
    if FDataLink = nil then
      raise Exception.CreateFmt('Cannot creare DataLink for component of type %s', [Value.ClassName]);
    FDataLink.DataSource := FDataSource;
    FDataLink.FieldName := FDataField;
  end;
end;

constructor TDataLinkCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TDataLinkItem);
end;

class constructor TDataLinkSupport.CreateClass;
begin
  FRegistry := TDataLinkRegistry.Create;
end;

class destructor TDataLinkSupport.DestroyClass;
begin
  FRegistry.Free;
end;

class function TDataLinkSupport.CreateDataLink(ATarget: TComponent): TCustomObserverDataLink;
var
  linkClass: TCustomObserverDataLinkClass;
begin
  Result := nil;
  var cls := ATarget.ClassType;
  while not cls.ClassNameIs(TComponent.ClassName) do begin
    if FRegistry.TryGetValue(cls, linkClass) then
      Exit(linkClass.Create(ATarget));
    cls := cls.ClassParent;
  end;
end;

class procedure TDataLinkSupport.RegisterLinkClass(AClass: TComponentClass; ALinkClass: TCustomObserverDataLinkClass);
begin
  FRegistry.AddOrSetValue(AClass, ALinkClass);
end;

class function TDataLinkSupport.SupportsLinking(ATarget: TComponent): Boolean;
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

class procedure TDataLinkSupport.UnregisterLinkClass(AClass: TComponentClass; ALinkClass: TCustomObserverDataLinkClass);
var
  linkClass: TCustomObserverDataLinkClass;
begin
  if FRegistry.TryGetValue(AClass, linkClass) then begin
    if linkClass = ALinkClass then
      FRegistry.Remove(AClass);
  end;
end;

constructor TDataLinkItem.TNexus.Create(AItem: TDataLinkItem);
begin
  inherited Create(nil);
  FItem := AItem;
end;

procedure TDataLinkItem.TNexus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if (AComponent = FItem.DataSource) then begin
      FItem.DataSource := nil;
    end;
  end
end;

initialization
  RegisterClasses([TDataLinkContainer]);
finalization
  UnRegisterClasses([TDataLinkContainer]);
end.
