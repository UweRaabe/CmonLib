unit Cmon.DataSense.FMX;

interface

uses
  System.Classes,
  Data.DB,
  FMX.Controls, FMX.Edit, FMX.Memo,
  Cmon.DataSense;

type
  TDataSenseLinkFMX = class(TDataSenseLink);

  TControlDataSenseEditLink = class(TDataSenseEditLink)
  private
    function GetControl: TControl;
  public
    constructor Create(AControl: TControl);
    property Control: TControl read GetControl;
  end;

  TControlDataSenseEditLink<T: TControl> = class(TControlDataSenseEditLink)
  private
    function GetControl: T;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateRightToLeft; override;
  public
    constructor Create(AControl: T);
    property Control: T read GetControl;
  end;

type
  TCustomEditDataSenseLink = class(TControlDataSenseEditLink<TCustomEdit>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

type
  TCustomMemoDataSenseLink = class(TControlDataSenseEditLink<TCustomMemo>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

implementation

procedure TCustomEditDataSenseLink.DoLoadData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Control.Text := Field.AsString;
  end;
end;

procedure TCustomEditDataSenseLink.DoSaveData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Field.AsString := Control.Text;
  end;
end;

procedure TCustomMemoDataSenseLink.DoLoadData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Control.Text := Field.AsString;
  end;
end;

procedure TCustomMemoDataSenseLink.DoSaveData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Field.AsString := Control.Text;
  end;
end;

constructor TControlDataSenseEditLink<T>.Create(AControl: T);
begin
  inherited Create(AControl);
  VisualControl := True;
end;

procedure TControlDataSenseEditLink<T>.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = Self.Field) and (Control <> nil) then
    if Control.CanFocus then begin
      Field^ := nil;
      Control.SetFocus;
    end;
end;

function TControlDataSenseEditLink<T>.GetControl: T;
begin
  Result := Target as T;
end;

procedure TControlDataSenseEditLink<T>.UpdateRightToLeft;
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

constructor TControlDataSenseEditLink.Create(AControl: TControl);
begin
  inherited Create(AControl);
  VisualControl := True;
end;

function TControlDataSenseEditLink.GetControl: TControl;
begin
  Result := Target as TControl;
end;

initialization
  TDataSense.RegisterLinkClass(TCustomEdit, TCustomEditDataSenseLink);
  TDataSense.RegisterLinkClass(TCustomMemo, TCustomMemoDataSenseLink);
finalization
  TDataSense.UnregisterLinkClass(TCustomEdit, TCustomEditDataSenseLink);
  TDataSense.UnregisterLinkClass(TCustomMemo, TCustomMemoDataSenseLink);
end.
