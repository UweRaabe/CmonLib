unit Cmon.DbAware.ObserverLink;

interface

uses
  System.Classes,
  Data.DB;

type
  TCustomObserverDataLink = class(TDataLink, IInterface, IObserver, IObserverTrack, ISingleCastObserver, IEditLinkObserver)
  private
    FControl: TComponent;
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
    {$ENDIF}
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
    constructor Create(AControl: TComponent);
    destructor Destroy; override;
    property CanModify: Boolean read GetCanModify;
    property Control: TComponent read FControl;
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;
  end;

implementation

constructor TCustomObserverDataLink.Create(AControl: TComponent);
begin
  inherited Create;
  FControl := AControl;
  if FControl <> nil then begin
    FControl.Observers.AddObserver(TObserverMapping.EditLinkID, Self);
    FObserverActive := True;
  end;
end;

destructor TCustomObserverDataLink.Destroy;
begin
  if FControl <> nil then begin
    FControl.Observers.RemoveObserver(TObserverMapping.EditLinkID, Self);
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

function TCustomObserverDataLink.GetFormatLink: IEditFormatLink;
begin
  Result := nil;
end;

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
    if Assigned(FControl) then
      SetField(GetFieldProperty(DataSource.Dataset, FControl, FFieldName))
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

end.
