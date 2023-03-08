unit Cmon.Vcl.DbAware.Components;

interface

uses
  System.Classes,
  Data.DB,
  Vcl.Controls,
  Cmon.Vcl.DbAware.ObserverLink;

type
  TDataComponent = class(TComponent)
  private
    FDataField: string;
    FDataLink: TObserverDataLink;
    FDataSource: TDataSource;
    function GetControl: TWinControl;
    procedure SetControl(const Value: TWinControl);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    function CreateDataLink(AControl: TWinControl): TObserverDataLink;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    class function CreateInstance(AOwner: TComponent; AControl: TWinControl; ADataSource: TDataSource; const ADataField: string): TDataComponent;
  published
    property Control: TWinControl read GetControl write SetControl;
    property DataField: string read FDataField write SetDataField;
    property DataSource: TDataSource read FDataSource write SetDataSource;
  end;

implementation

uses
  System.StrUtils,
  Vcl.ComCtrls, Vcl.StdCtrls;

type
  TDateTimePickerDataLink = class(TObserverDataLink<TDateTimePicker>)
  protected
    procedure DoActiveChanged(Value: Boolean); override;
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;

  TCustomEditDataLink = class(TObserverDataLink<TCustomEdit>)
  protected
    procedure DoLoadData; override;
    procedure DoSaveData; override;
  end;


function TDataComponent.CreateDataLink(AControl: TWinControl): TObserverDataLink;
begin
  if AControl.InheritsFrom(TDateTimePicker) then
    Result := TDateTimePickerDataLink.Create(TDateTimePicker(AControl))
  else if AControl.InheritsFrom(TCustomEdit) then
    Result := TCustomEditDataLink.Create(TCustomEdit(AControl))
  else
    Result := TObserverDataLink<TWinControl>.Create(AControl);
end;

class function TDataComponent.CreateInstance(AOwner: TComponent; AControl: TWinControl; ADataSource: TDataSource; const ADataField: string): TDataComponent;
begin
  Result := Self.Create(AOwner);
  Result.Control := AControl;
  Result.DataSource := ADataSource;
  Result.DataField := ADataField;
end;

function TDataComponent.GetControl: TWinControl;
begin
  Result := nil;
  if FDataLink <> nil then begin
    Result := FDataLink.Control;
  end;
end;

procedure TDataComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if (AComponent = DataSource) then begin
      DataSource := nil;
    end
    else if AComponent = Control then begin
      Control := nil;
    end;
  end
end;

procedure TDataComponent.SetControl(const Value: TWinControl);
begin
  if (FDataLink <> nil) and (FDataLink.Control <> Value) then begin
    if FDataLink.Control <> nil then FDataLink.Control.RemoveFreeNotification(Self);
    FDataLink.Free;
    FDataLink := nil;
  end;
  if (Value <> nil) and (FDataLink = nil) then begin
    FDataLink := CreateDataLink(Value);
    if FDataLink.Control <> nil then FDataLink.Control.FreeNotification(Self);
    FDataLink.DataSource := FDataSource;
    FDataLink.FieldName := FDataField;
  end;
end;

procedure TDataComponent.SetDataField(const Value: string);
begin
  if FDataField <> Value then
  begin
    FDataField := Value;
    if FDataLink <> nil then begin
      FDataLink.FieldName := FDataField;
    end;
  end;
end;

procedure TDataComponent.SetDataSource(Value: TDataSource);
begin
  if FDataSource <> Value then
  begin
    if FDataSource <> nil then FDataSource.RemoveFreeNotification(Self);
    FDataSource := Value;
    if FDataSource <> nil then FDataSource.FreeNotification(Self);
    if FDataLink <> nil then begin
      FDataLink.DataSource := FDataSource;
    end;
  end;
end;

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

end.
