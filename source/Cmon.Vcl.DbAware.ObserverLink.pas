unit Cmon.Vcl.DbAware.ObserverLink;

interface

uses
  System.Classes,
  Data.DB,
  Vcl.Controls,
  Cmon.DbAware.ObserverLink;

type
  TObserverDataLink = class(TCustomObserverDataLink)
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
  TObserverDataLink<T: TWinControl> = class(TObserverDataLink)
  private
    function GetControl: T;
  public
    constructor Create(AControl: T);
    property Control: T read GetControl;
  end;

implementation

uses
  Winapi.Windows,
  Vcl.DBCtrls;

{$IF CompilerVersion >= 34.0 Delphi 10.4 Sydney }
{$ENDIF}

constructor TObserverDataLink<T>.Create(AControl: T);
begin
  inherited Create(AControl);
end;

function TObserverDataLink<T>.GetControl: T;
begin
  Result := inherited Control as T;
end;

constructor TObserverDataLink.Create(AControl: TWinControl);
begin
  inherited Create(AControl);
  VisualControl := True;
end;

procedure TObserverDataLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = Self.Field) and (Control <> nil) then
    if Control.CanFocus then begin
      Field^ := nil;
      Control.SetFocus;
    end;
end;

function TObserverDataLink.GetControl: TWinControl;
begin
  Result := inherited Control as TWinControl;
end;

procedure TObserverDataLink.UpdateRightToLeft;
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

end.
