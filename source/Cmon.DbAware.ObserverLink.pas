unit Cmon.DbAware.ObserverLink;

interface

uses
  System.Classes,
  Data.DB,
  Vcl.Controls;

type
  TObserverDataLink = class(TDataLink, IInterface, IObserver, IObserverTrack, ISingleCastObserver, IEditLinkObserver)
  private
    FControl: TWinControl;
    FField: TField;
    FFieldName: string;
    FObserverActive: Boolean;
    FObserverModified: Boolean;
    FOnToggle: TObserverToggleEvent;
    FUpdateCnt: Integer;
    procedure SetField(Value: TField);
    procedure UpdateField;
    procedure UpdateRightToLeft;
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
    procedure FocusControl(Field: TFieldRef); override;
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
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AControl: TWinControl);
    destructor Destroy; override;
    property CanModify: Boolean read GetCanModify;
    property Control: TWinControl read FControl;
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;
  end;

  TObserverDataLink<T:TWinControl> = class(TObserverDataLink)
  private
    function GetControl: T;
  public
    property Control: T read GetControl;
  end;

implementation

uses
  Winapi.Windows,
  Vcl.DBCtrls;

constructor TObserverDataLink.Create(AControl: TWinControl);
begin
  inherited Create;
  VisualControl := True;
  FControl := AControl;
  if FControl <> nil then begin
    FControl.Observers.AddObserver(TObserverMapping.EditLinkID, Self);
    FObserverActive := True;
  end;
end;

destructor TObserverDataLink.Destroy;
begin
  if FControl <> nil then begin
    FControl.Observers.RemoveObserver(TObserverMapping.EditLinkID, Self);
  end;
  inherited;
end;

procedure TObserverDataLink.ActiveChanged;
begin
  UpdateField;
  DoActiveChanged(Active);
end;

procedure TObserverDataLink.BeginUpdate;
begin
  Inc(FUpdateCnt);
end;

procedure TObserverDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  inherited;
  if Event = deDisabledStateChange then begin
    if Boolean(Info) then
      UpdateField
    else
      FField := nil;
  end;
end;

procedure TObserverDataLink.DoActiveChanged(Value: Boolean);
begin
end;

procedure TObserverDataLink.DoEditingChanged(Value: Boolean);
begin
end;

procedure TObserverDataLink.DoLoadData;
begin
end;

procedure TObserverDataLink.DoSaveData;
begin
end;

function TObserverDataLink.Edit: Boolean;
begin
  if CanModify then
    inherited Edit;
  Result := Editing;
end;

procedure TObserverDataLink.EditingChanged;
begin
  FObserverModified := False;
  DoEditingChanged(Editing);
end;

procedure TObserverDataLink.EndUpdate;
begin
  Dec(FUpdateCnt);
end;

procedure TObserverDataLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = FField) and (FControl <> nil) then
    if FControl.CanFocus then begin
      Field^ := nil;
      FControl.SetFocus;
    end;
end;

function TObserverDataLink.GetActive: Boolean;
begin
  Result := FObserverActive;
end;

function TObserverDataLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
end;

{$IF CompilerVersion >= 34.0 Delphi 10.4 Sydney }
function TObserverDataLink.GetFormatLink: IEditFormatLink;
begin
  Result := nil;
end;
{$ENDIF}

function TObserverDataLink.GetIsEditing: Boolean;
begin
  Result := Editing;
end;

function TObserverDataLink.GetIsReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

function TObserverDataLink.GetOnObserverToggle: TObserverToggleEvent;
begin
  Result := FOnToggle;
end;

function TObserverDataLink.GetTrack: Boolean;
begin
  Result := Active and Editing;
end;

function TObserverDataLink.GetUpdating: Boolean;
begin
  Result := FUpdateCnt > 0;
end;

function TObserverDataLink.IsModified: Boolean;
begin
  Result := FObserverModified;
end;

function TObserverDataLink.IsRequired: Boolean;
begin
  Result := (Field <> nil) and Field.Required;
end;

function TObserverDataLink.IsValidChar(AKey: Char): Boolean;
begin
  Result := True;
  if Field <> nil then begin
    Result := Field.IsValidChar(AKey);
  end;
end;

procedure TObserverDataLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TObserverDataLink.Modified;
begin
  FObserverModified := True;
end;

function TObserverDataLink.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

procedure TObserverDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FField) then begin
    DoLoadData;
    FObserverModified := False;
  end;
end;

procedure TObserverDataLink.Removed;
begin

end;

procedure TObserverDataLink.Reset;
begin
  RecordChanged(nil);
end;

procedure TObserverDataLink.SetActive(Value: Boolean);
begin
  FObserverActive := Value;
  if Assigned(FOnToggle) then
    FOnToggle(Self, Value);
end;

procedure TObserverDataLink.SetField(Value: TField);
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

procedure TObserverDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then begin
    FFieldName := Value;
    UpdateField;
  end;
end;

procedure TObserverDataLink.SetIsReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TObserverDataLink.SetOnObserverToggle(AEvent: TObserverToggleEvent);
begin
  FOnToggle := AEvent;
end;

procedure TObserverDataLink.Toggle(Value: Boolean);
begin
  if Assigned(FOnToggle) then FOnToggle(Self, Value);
end;

procedure TObserverDataLink.Update;
begin
  UpdateData;
  FObserverModified := False;
end;

procedure TObserverDataLink.UpdateData;
begin
  if FObserverModified then begin
    if (Field <> nil) then
      DoSaveData;
    FObserverModified := False;
  end;
end;

procedure TObserverDataLink.UpdateField;
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

procedure TObserverDataLink.UpdateRightToLeft;
var
  isRightAligned: Boolean;
  useRightToLeftAlignment: Boolean;
begin
  if Assigned(FControl) then
    if FControl.IsRightToLeft then begin
      isRightAligned := (GetWindowLong(FControl.Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
      useRightToLeftAlignment := DBUseRightToLeftAlignment(FControl, Field);
      if isRightAligned xor useRightToLeftAlignment then begin
        FControl.Perform(CM_RECREATEWND, 0, 0);
      end;
    end;
end;

function TObserverDataLink._AddRef: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

function TObserverDataLink._Release: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

function TObserverDataLink<T>.GetControl: T;
begin
  Result := inherited Control as T;
end;

end.
