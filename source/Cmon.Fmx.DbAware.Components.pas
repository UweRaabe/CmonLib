unit Cmon.Fmx.DbAware.Components;

interface

uses
  System.Classes,
  Data.DB,
  FMX.Controls, FMX.Edit, FMX.Memo,
  Cmon.DbAware.ObserverLink;

type
  TObserverDataLinkFMX = class(TCustomObserverDataLink)
  private
    function GetControl: TControl;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateRightToLeft; override;
  public
    constructor Create(AControl: TControl);
    property Control: TControl read GetControl;
  end;

type
  TObserverDataLinkFMX<T: TControl> = class(TObserverDataLinkFMX)
  private
    function GetControl: T;
  public
    constructor Create(AControl: T);
    property Control: T read GetControl;
  end;

type
  TCustomEditDataLink = class(TObserverDataLinkFMX<TCustomEdit>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

type
  TCustomMemoDataLink = class(TObserverDataLinkFMX<TCustomMemo>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

implementation

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

procedure TCustomMemoDataLink.DoLoadData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Control.Text := Field.AsString;
  end;
end;

procedure TCustomMemoDataLink.DoSaveData;
begin
  inherited;
  if (Control <> nil) and (Field <> nil) then begin
    Field.AsString := Control.Text;
  end;
end;

constructor TObserverDataLinkFMX<T>.Create(AControl: T);
begin
  inherited Create(AControl);
end;

function TObserverDataLinkFMX<T>.GetControl: T;
begin
  Result := inherited Control as T;
end;

constructor TObserverDataLinkFMX.Create(AControl: TControl);
begin
  inherited Create(AControl);
  VisualControl := True;
end;

procedure TObserverDataLinkFMX.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = Self.Field) and (Control <> nil) then
    if Control.CanFocus then begin
      Field^ := nil;
      Control.SetFocus;
    end;
end;

function TObserverDataLinkFMX.GetControl: TControl;
begin
  Result := Target as TControl;
end;

procedure TObserverDataLinkFMX.UpdateRightToLeft;
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

initialization
  TDataLinkSupport.RegisterLinkClass(TCustomEdit, TCustomEditDataLink);
  TDataLinkSupport.RegisterLinkClass(TCustomMemo, TCustomMemoDataLink);
finalization
  TDataLinkSupport.UnregisterLinkClass(TCustomEdit, TCustomEditDataLink);
  TDataLinkSupport.UnregisterLinkClass(TCustomMemo, TCustomMemoDataLink);
end.
