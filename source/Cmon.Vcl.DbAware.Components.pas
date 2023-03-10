unit Cmon.Vcl.DbAware.Components;

interface

uses
  System.Classes,
  Data.DB,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls,
  Cmon.DbAware.ObserverLink;

type
  TObserverDataLinkVCL = class(TCustomObserverDataLink)
  private
    function GetControl: TWinControl;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateRightToLeft; override;
  public
    constructor Create(AControl: TWinControl);
    property Control: TWinControl read GetControl;
  end;

type
  TObserverDataLinkVCL<T: TWinControl> = class(TObserverDataLinkVCL)
  private
    function GetControl: T;
  public
    constructor Create(AControl: T);
    property Control: T read GetControl;
  end;

type
  TDateTimePickerDataLink = class(TObserverDataLinkVCL<TDateTimePicker>)
  protected
    procedure DoActiveChanged(Value: Boolean); override;
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

type
  TCustomEditDataLink = class(TObserverDataLinkVCL<TCustomEdit>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

implementation

uses
  Winapi.Windows,
  System.StrUtils,
  Vcl.DBCtrls;

procedure TDateTimePickerDataLink.DoActiveChanged(Value: Boolean);
begin
  if Control <> nil then begin
    Control.Format := IfThen(Value, '', ' ');
  end;
end;

procedure TDateTimePickerDataLink.DoLoadData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    { Hide Null }
    Control.Format := IfThen(Field.IsNull, ' ', '');
    if not Field.IsNull then
      Control.DateTime := Field.AsDateTime;
  end;
end;

procedure TDateTimePickerDataLink.DoSaveData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Field.AsDateTime := Control.DateTime;
  end;
end;

procedure TCustomEditDataLink.DoLoadData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Control.Text := Field.AsString;
  end;
end;

procedure TCustomEditDataLink.DoSaveData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Field.AsString := Control.Text;
  end;
end;

constructor TObserverDataLinkVCL<T>.Create(AControl: T);
begin
  inherited Create(AControl);
end;

function TObserverDataLinkVCL<T>.GetControl: T;
begin
  Result := inherited Control as T;
end;

constructor TObserverDataLinkVCL.Create(AControl: TWinControl);
begin
  inherited Create(AControl);
  VisualControl := True;
end;

procedure TObserverDataLinkVCL.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = Self.Field) and (Control <> nil) then
    if Control.CanFocus then begin
      Field^ := nil;
      Control.SetFocus;
    end;
end;

function TObserverDataLinkVCL.GetControl: TWinControl;
begin
  Result := Target as TWinControl;
end;

procedure TObserverDataLinkVCL.UpdateRightToLeft;
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

initialization
  TDataLinkSupport.RegisterLinkClass(TCustomEdit, TCustomEditDataLink);
  TDataLinkSupport.RegisterLinkClass(TDateTimePicker, TDateTimePickerDataLink);
finalization
  TDataLinkSupport.UnregisterLinkClass(TCustomEdit, TCustomEditDataLink);
  TDataLinkSupport.UnregisterLinkClass(TDateTimePicker, TDateTimePickerDataLink);
end.
