unit Cmon.DataSense.FMX;

interface

uses
  System.Classes,
  Data.DB,
  FMX.Controls, FMX.Edit, FMX.Memo,
  Cmon.DataSense;

type
  TDataSenseLinkFMX = class(TDataSenseLink);

  TControlDataSenseLink = class(TDataSenseFieldLink)
  private
    function GetControl: TControl;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateRightToLeft; override;
  public
    constructor Create(ATarget: TComponent); override;
    property Control: TControl read GetControl;
  end;

  TControlDataSenseLink<T: TControl> = class(TControlDataSenseLink)
  private
    function GetControl: T;
  public
    constructor Create(ATarget: TComponent); override;
    property Control: T read GetControl;
  end;

type
  TCustomEditDataSenseLink = class(TControlDataSenseLink<TCustomEdit>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

  TCustomEditDataSenseLink<T: TCustomEdit> = class(TCustomEditDataSenseLink)
  private
    function GetControl: T;
  public
    constructor Create(ATarget: TComponent); override;
    property Control: T read GetControl;
  end;

type
  TCustomMemoDataSenseLink = class(TControlDataSenseLink<TCustomMemo>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

  TCustomMemoDataSenseLink<T: TCustomEdit> = class(TCustomMemoDataSenseLink)
  private
    function GetControl: T;
  public
    constructor Create(ATarget: TComponent); override;
    property Control: T read GetControl;
  end;

implementation

procedure TCustomEditDataSenseLink.DoLoadData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then
    Control.Text := Field.DisplayText
  else
    Control.Text := '';
end;

procedure TCustomEditDataSenseLink.DoSaveData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then
    Field.Text := Control.Text;
end;

procedure TCustomMemoDataSenseLink.DoLoadData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then
    Control.Text := Field.DisplayText
  else
    Control.Text := '';
end;

procedure TCustomMemoDataSenseLink.DoSaveData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then
    Field.Text := Control.Lines.Text;
end;

constructor TControlDataSenseLink<T>.Create(ATarget: TComponent);
begin
  AssertTargetType<T>(ATarget.ClassType);
  inherited Create(ATarget);
end;

function TControlDataSenseLink<T>.GetControl: T;
begin
  Result := inherited Control as T;
end;

constructor TControlDataSenseLink.Create(ATarget: TComponent);
begin
  AssertTargetType<TControl>(ATarget.ClassType);
  inherited Create(ATarget);
  VisualControl := True;
end;

procedure TControlDataSenseLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = Self.Field) and (Control <> nil) then
    if Control.CanFocus then begin
      Field^ := nil;
      Control.SetFocus;
    end;
end;

function TControlDataSenseLink.GetControl: TControl;
begin
  Result := Target as TControl;
end;

procedure TControlDataSenseLink.UpdateRightToLeft;
{ I haven't yet found out how this is handled in FMX - if it is even supported at all. }

//var
//  isRightAligned: Boolean;
//  useRightToLeftAlignment: Boolean;
begin
//  if Assigned(Control) then
//    if Control.IsRightToLeft then begin
//      isRightAligned := (GetWindowLong(Control.Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
//      useRightToLeftAlignment := DBUseRightToLeftAlignment(Control, Field);
//      if isRightAligned xor useRightToLeftAlignment then begin
//        Control.Perform(CM_RECREATEWND, 0, 0);
//      end;
//    end;
end;

constructor TCustomEditDataSenseLink<T>.Create(ATarget: TComponent);
begin
  AssertTargetType<T>(ATarget.ClassType);
  inherited Create(ATarget);
end;

function TCustomEditDataSenseLink<T>.GetControl: T;
begin
  Result := inherited Control as T;
end;

constructor TCustomMemoDataSenseLink<T>.Create(ATarget: TComponent);
begin
  AssertTargetType<T>(ATarget.ClassType);
  inherited Create(ATarget);
end;

function TCustomMemoDataSenseLink<T>.GetControl: T;
begin
  Result := inherited Control as T;
end;

initialization
  TDataSense.RegisterLinkClass(TCustomEdit, TCustomEditDataSenseLink);
  TDataSense.RegisterLinkClass(TCustomMemo, TCustomMemoDataSenseLink);
finalization
  TDataSense.UnregisterLinkClass(TCustomEdit, TCustomEditDataSenseLink);
  TDataSense.UnregisterLinkClass(TCustomMemo, TCustomMemoDataSenseLink);
end.
