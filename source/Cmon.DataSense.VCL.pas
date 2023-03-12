unit Cmon.DataSense.VCL;

interface

uses
  System.Classes,
  Data.DB,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Mask,
  Cmon.DataSense;

type
  TDataSenseLinkVCL = class(TDataSenseLink);

type
  TControlDataSenseLink = class(TDataSenseFieldLink)
  private
    function GetControl: TControl;
  strict protected
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  public
    constructor Create(ATarget: TComponent); override;
  published
    property Control: TControl read GetControl;
  end;

type
  TControlDataSenseLink<T: TControl> = class(TControlDataSenseLink)
  private
    function GetControl: T;
  public
    constructor Create(ATarget: TComponent); override;
    property Control: T read GetControl;
  end;

type
  TWinControlDataSenseLink<T: TWinControl> = class(TControlDataSenseLink<T>)
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateRightToLeft; override;
  end;

type
  TCustomEditDataSenseLink<T: TCustomEdit> = class(TWinControlDataSenseLink<T>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

type
  TCustomMaskEditDataSenseLink = class(TCustomEditDataSenseLink<TCustomMaskEdit>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

type
  TCustomMaskEditDataSenseLink<T: TCustomMaskEdit> = class(TCustomMaskEditDataSenseLink)
  private
    function GetControl: T;
  protected
  public
    constructor Create(ATarget: TComponent); override;
    property Control: T read GetControl;
  end;

type
  TCustomLabeledEditDataSenseLink<T: TCustomLabeledEdit> = class(TCustomMaskEditDataSenseLink<T>)
  protected
    procedure DoLoadData; override;
  public
    destructor Destroy; override;
  end;

type
  TDateTimePickerDataSenseLink = class(TWinControlDataSenseLink<TDateTimePicker>)
  protected
    procedure DoActiveChanged(Value: Boolean); override;
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

implementation

uses
  Winapi.Windows,
  System.StrUtils, System.SysUtils,
  Vcl.DBCtrls;

type
  TControlHack = class(TControl);
  TCustomMaskEditHack = class(TCustomMaskEdit);

constructor TControlDataSenseLink.Create(ATarget: TComponent);
begin
  AssertTargetType<TControl>(ATarget.ClassType);
  inherited Create(ATarget);
  VisualControl := True;
end;

procedure TControlDataSenseLink.DoLoadData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then
    TControlHack(Control).Text := Field.DisplayText
  else
    TControlHack(Control).Text := '';
end;

procedure TControlDataSenseLink.DoSaveData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then
    Field.Text := TControlHack(Control).Text;
end;

function TControlDataSenseLink.GetControl: TControl;
begin
  Result := Target as TControl;
end;

constructor TControlDataSenseLink<T>.Create(ATarget: TComponent);
begin
  AssertTargetType<T>(ATarget.ClassType);
  inherited;
  VisualControl := True;
end;

function TControlDataSenseLink<T>.GetControl: T;
begin
  Result := Target as T;
end;

procedure TWinControlDataSenseLink<T>.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = Self.Field) and (Control <> nil) then
    if Control.CanFocus then begin
      Field^ := nil;
      Control.SetFocus;
    end;
end;

procedure TWinControlDataSenseLink<T>.UpdateRightToLeft;
var
  isRightAligned: Boolean;
  useRightToLeftAlignment: Boolean;
begin
  if Assigned(Control) then
    if Control.IsRightToLeft then begin
      isRightAligned := (GetWindowLong(Control.Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
      useRightToLeftAlignment := DBUseRightToLeftAlignment(Control, Field);
      if isRightAligned xor useRightToLeftAlignment then begin
        Control.Perform(CM_RECREATEWND, 0, 0);
      end;
    end;
end;

destructor TCustomLabeledEditDataSenseLink<T>.Destroy;
begin
  if (Target <> nil) and not (csDestroying in Target.ComponentState) then begin
    var EditLabel := Control.EditLabel;
    if not EditLabel.IsLabelModified then
      EditLabel.Caption := '';
  end;

  inherited;
end;

procedure TCustomLabeledEditDataSenseLink<T>.DoLoadData;
begin
  inherited;
  if Control = nil then Exit;

  var EditLabel := Control.EditLabel;
  if not EditLabel.IsLabelModified or (EditLabel.GetTextLen = 0) then
  begin
    if Field <> nil then
      EditLabel.Caption := Field.DisplayLabel
    else if csDesigning in Control.ComponentState then
      EditLabel.Caption := Control.Name
    else
      EditLabel.Caption := '';
    EditLabel.IsLabelModified := False;
  end;
end;

procedure TDateTimePickerDataSenseLink.DoActiveChanged(Value: Boolean);
begin
  if Control <> nil then begin
    { To clear the entry when inactive use a Format that produces an empty string. The same is used for a NULL value. }
    Control.Format := IfThen(Value, '', ' ');
  end;
end;

procedure TDateTimePickerDataSenseLink.DoLoadData;
begin
  inherited;
  { Hide Null }
  Control.Format := IfThen(Field.IsNull, ' ', '');
  if not Field.IsNull then
    Control.DateTime := Field.AsDateTime;
end;

procedure TDateTimePickerDataSenseLink.DoSaveData;
begin
  inherited;
  Field.AsDateTime := Control.DateTime;
end;

procedure TCustomEditDataSenseLink<T>.DoLoadData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then begin
    if Control.Focused and CanModify then
      Control.Text := Field.Text
    else
      Control.Text := Field.DisplayText;
  end
  else
    Control.Text := '';
end;

procedure TCustomEditDataSenseLink<T>.DoSaveData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then
    Field.Text := Control.Text;
end;

constructor TCustomMaskEditDataSenseLink<T>.Create(ATarget: TComponent);
begin
  AssertTargetType<T>(ATarget.ClassType);
  inherited;
end;

function TCustomMaskEditDataSenseLink<T>.GetControl: T;
begin
  Result := Target as T;
end;

procedure TCustomMaskEditDataSenseLink.DoLoadData;
begin
  inherited;
  if Control = nil then Exit;

  if Field <> nil then begin
    if Control.Alignment <> Field.Alignment then begin
      Control.EditText := '';  {forces update}
      Control.Alignment := Field.Alignment;
    end;
    TCustomMaskEditHack(Control).EditMask := Field.EditMask;
    if not (csDesigning in Control.ComponentState) then begin
      if (Field.DataType in [ftString, ftWideString]) and (TCustomMaskEditHack(Control).MaxLength = 0) then
        TCustomMaskEditHack(Control).MaxLength := Field.Size;
    end;
    if Control.Focused and CanModify then
      Control.Text := Field.Text
    else begin
      Control.EditText := Field.DisplayText;
      if Editing and Modified then
        Modified := True;
    end;
  end
  else
    Control.Text := '';
end;

procedure TCustomMaskEditDataSenseLink.DoSaveData;
begin
  inherited;
  if Control = nil then Exit;

  Control.ValidateEdit;
  if Field <> nil then
    Field.Text := Control.Text;
end;

initialization
  TDataSense.RegisterLinkClass(TCustomLabel, TControlDataSenseLink<TCustomLabel>);
  TDataSense.RegisterLinkClass(TBoundLabel, nil);
  TDataSense.RegisterLinkClass(TCustomEdit, TCustomEditDataSenseLink<TCustomEdit>);
  TDataSense.RegisterLinkClass(TCustomMaskEdit, TCustomMaskEditDataSenseLink);
  TDataSense.RegisterLinkClass(TCustomLabeledEdit, TCustomLabeledEditDataSenseLink<TCustomLabeledEdit>);
  TDataSense.RegisterLinkClass(TDateTimePicker, TDateTimePickerDataSenseLink);
finalization
  TDataSense.UnregisterLinkClass(TCustomLabel, TControlDataSenseLink<TCustomLabel>);
  TDataSense.UnregisterLinkClass(TBoundLabel, nil);
  TDataSense.UnregisterLinkClass(TCustomEdit, TCustomEditDataSenseLink<TCustomEdit>);
  TDataSense.UnregisterLinkClass(TCustomMaskEdit, TCustomMaskEditDataSenseLink);
  TDataSense.UnregisterLinkClass(TCustomLabeledEdit, TCustomLabeledEditDataSenseLink<TCustomLabeledEdit>);
  TDataSense.UnregisterLinkClass(TDateTimePicker, TDateTimePickerDataSenseLink);
end.
