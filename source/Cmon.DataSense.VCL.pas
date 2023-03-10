unit Cmon.DataSense.VCL;

interface

uses
  System.Classes,
  Data.DB,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  Cmon.DataSense;

type
  TDataSenseLinkVCL = class(TDataSenseLink);

  TControlDataSenseEditLink = class(TDataSenseEditLink)
  private
    function GetControl: TControl;
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  public
    constructor Create(AControl: TControl);
  published
    property Control: TControl read GetControl;
  end;

  TControlDataSenseEditLink<T: TControl> = class(TControlDataSenseEditLink)
  private
    function GetControl: T;
  public
    constructor Create(AControl: T);
    property Control: T read GetControl;
  end;

  TControlDataSenseLink = class(TDataSenseDisplayLink)
  private
    function GetControl: TControl;
  protected
    procedure DoLoadData; override;
  public
    constructor Create(AControl: TControl);
    property Control: TControl read GetControl;
  end;

  TControlDataSenseLink<T: TControl> = class(TControlDataSenseLink)
  private
    function GetControl: T;
  protected
  public
    constructor Create(AControl: T);
    property Control: T read GetControl;
  end;

type
  TWinControlDataSenseLink<T: TWinControl> = class(TControlDataSenseLink<T>)
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateRightToLeft; override;
  end;

  TWinControlDataSenseEditLink<T: TWinControl> = class(TControlDataSenseEditLink<T>)
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateRightToLeft; override;
  end;

type
  TCustomLabeledEditDataSenseLink = class(TWinControlDataSenseEditLink<TCustomLabeledEdit>)
  protected
    procedure DoLoadData; override;
  end;

type
  TDateTimePickerDataSenseLink = class(TWinControlDataSenseEditLink<TDateTimePicker>)
  protected
    procedure DoActiveChanged(Value: Boolean); override;
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

implementation

uses
  Winapi.Windows,
  System.StrUtils,
  Vcl.DBCtrls;

type
  TControlHelper = class helper for TControl
  public
    function GetText: string;
    procedure SetText(const Value: string);
    property Text: string read GetText write SetText;
  end;

constructor TControlDataSenseLink<T>.Create(AControl: T);
begin
  inherited Create(AControl);
  VisualControl := True;
end;

function TControlDataSenseLink<T>.GetControl: T;
begin
  Result := Target as T;
end;

constructor TControlDataSenseEditLink<T>.Create(AControl: T);
begin
  inherited Create(AControl);
  VisualControl := True;
end;

function TControlDataSenseEditLink<T>.GetControl: T;
begin
  Result := Target as T;
end;

procedure TWinControlDataSenseEditLink<T>.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = Self.Field) and (Control <> nil) then
    if Control.CanFocus then begin
      Field^ := nil;
      Control.SetFocus;
    end;
end;

procedure TWinControlDataSenseEditLink<T>.UpdateRightToLeft;
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

procedure TCustomLabeledEditDataSenseLink.DoLoadData;
begin
  inherited;
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
  if (Control <> nil) and (Field <> nil) then begin
    { Hide Null }
    Control.Format := IfThen(Field.IsNull, ' ', '');
    if not Field.IsNull then
      Control.DateTime := Field.AsDateTime;
  end;
end;

procedure TDateTimePickerDataSenseLink.DoSaveData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Field.AsDateTime := Control.DateTime;
  end;
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

function TControlHelper.GetText: string;
begin
  Result := inherited Text;
end;

procedure TControlHelper.SetText(const Value: string);
begin
  inherited Text := Value;
end;

constructor TControlDataSenseEditLink.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

procedure TControlDataSenseEditLink.DoLoadData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Control.Text := Field.AsString;
  end;
end;

procedure TControlDataSenseEditLink.DoSaveData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Field.AsString := Control.Text;
  end;
end;

function TControlDataSenseEditLink.GetControl: TControl;
begin
  Result := Target as TControl;
end;

constructor TControlDataSenseLink.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

procedure TControlDataSenseLink.DoLoadData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Control.Text := Field.AsString;
  end;
end;

function TControlDataSenseLink.GetControl: TControl;
begin
  Result := Target as TControl;
end;

initialization
  TDataSense.RegisterLinkClass(TControl, TControlDataSenseLink);
  TDataSense.RegisterLinkClass(TWinControl, TWinControlDataSenseLink<TWinControl>);
  TDataSense.RegisterLinkClass(TCustomLabeledEdit, TCustomLabeledEditDataSenseLink);
  TDataSense.RegisterLinkClass(TDateTimePicker, TDateTimePickerDataSenseLink);
finalization
  TDataSense.UnregisterLinkClass(TControl, TControlDataSenseLink);
  TDataSense.UnregisterLinkClass(TWinControl, TWinControlDataSenseLink<TWinControl>);
  TDataSense.UnregisterLinkClass(TCustomLabeledEdit, TCustomLabeledEditDataSenseLink);
  TDataSense.UnregisterLinkClass(TDateTimePicker, TDateTimePickerDataSenseLink);
end.
