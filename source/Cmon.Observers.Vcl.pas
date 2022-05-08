unit Cmon.Observers.Vcl;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.StdCtrls,
  Cmon.Observers;

type
  TCustomEditHelper = class helper for TCustomEdit
  public
    procedure AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil); overload;
    procedure AddObserver(AOnUpdateValue: TProc<string>); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<string>);
  end;

type
  TCustomMemoHelper = class helper for TCustomMemo
  public
    procedure AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil); overload;
    procedure AddObserver(AOnUpdateValue: TProc<TStrings>); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<TStrings>);
  end;

type
  TCustomComboBoxHelper = class helper for TCustomComboBox
  public
    procedure AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil); overload;
    procedure AddObserver(AOnUpdateValue: TProc<string>); overload;
    procedure AddObserver(AOnUpdateValue: TProc<Integer>); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<string>); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<Integer>); overload;
  end;

type
  TCustomListBoxHelper = class helper for TCustomListBox
  public
    procedure AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil); overload;
    procedure AddObserver(AOnUpdateValue: TProc<string>); overload;
    procedure AddObserver(AOnUpdateValue: TProc<Integer>); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<string>); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<Integer>); overload;
  end;

implementation

{ we dont need the methods of the interfaced helpers, so we can safely declare these internal ones }

type
  TInternalCustomEditHelper = class helper for TCustomEdit
  public
    procedure ReSelectAll;
  end;

type
  TInternalCustomComboBoxHelper = class helper for TCustomComboBox
  public
    function GetValue: string;
  end;

type
  TInternalCustomListBoxHelper = class helper for TCustomListBox
  protected
    function GetValue: string;
  end;

type
  TValidator = record
  public
    class function Create<T>(AOnValidateValue: TPredicate<T>): TProc<T>; static;
  end;

type
  TCustomEditObserver = class(TControlValueObserver<TCustomEdit, string>)
  strict protected
    procedure ValueUpdate; override;
  protected
    procedure NotifyValue(Sender: TObject); override;
  end;

type
  TCustomMemoObserver = class(TControlValueObserver<TCustomMemo, TStrings>)
  strict protected
    function GetTrack: Boolean; override;
  protected
    procedure NotifyValue(Sender: TObject); override;
  end;

type
  TCustomComboBoxObserver = class(TControlValueObserver<TCustomComboBox, string>)
  protected
    procedure NotifyValue(Sender: TObject); override;
  end;

type
  TCustomComboBoxIndexObserver = class(TControlValueObserver<TCustomComboBox, Integer>)
  protected
    procedure NotifyValue(Sender: TObject); override;
  end;

type
  TCustomListBoxObserver = class(TControlValueObserver<TCustomListBox, string>)
  protected
    procedure NotifyValue(Sender: TObject); override;
  end;

type
  TCustomListBoxIndexObserver = class(TControlValueObserver<TCustomListBox, Integer>)
  protected
    procedure NotifyValue(Sender: TObject); override;
  end;

class function TValidator.Create<T>(AOnValidateValue: TPredicate<T>): TProc<T>;
begin
  Result :=
    procedure(AValue: T)
    begin
      if not AOnValidateValue(AValue) then
        Abort;
    end;
end;

procedure TCustomEditObserver.NotifyValue(Sender: TObject);
begin
  DoNotifyValue(Target.Text);
end;

procedure TCustomEditObserver.ValueUpdate;
begin
  inherited;
  Target.ReSelectAll;
end;

procedure TCustomEditHelper.AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomEditObserver.Create(Self, AOnUpdate, AOnModified));
end;

procedure TCustomEditHelper.AddObserver(AOnUpdateValue: TProc<string>);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomEditObserver.Create(Self, AOnUpdateValue));
end;

procedure TCustomEditHelper.AddValidator(AOnValidateValue: TPredicate<string>);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<string>(AOnValidateValue));
end;

procedure TCustomComboBoxHelper.AddObserver(AOnUpdate, AOnModified: TNotifyEvent);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomComboBoxObserver.Create(Self, AOnUpdate, AOnModified));
end;

procedure TCustomComboBoxHelper.AddObserver(AOnUpdateValue: TProc<string>);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomComboBoxObserver.Create(Self, AOnUpdateValue));
end;

procedure TCustomComboBoxHelper.AddObserver(AOnUpdateValue: TProc<Integer>);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomComboBoxIndexObserver.Create(Self, AOnUpdateValue));
end;

procedure TCustomComboBoxHelper.AddValidator(AOnValidateValue: TPredicate<Integer>);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<Integer>(AOnValidateValue));
end;

procedure TCustomComboBoxHelper.AddValidator(AOnValidateValue: TPredicate<string>);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<string>(AOnValidateValue));
end;

procedure TCustomListBoxHelper.AddObserver(AOnUpdate, AOnModified: TNotifyEvent);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomListBoxObserver.Create(Self, AOnUpdate, AOnModified));
end;

procedure TCustomListBoxHelper.AddObserver(AOnUpdateValue: TProc<string>);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomListBoxObserver.Create(Self, AOnUpdateValue));
end;

procedure TCustomListBoxHelper.AddObserver(AOnUpdateValue: TProc<Integer>);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomListBoxIndexObserver.Create(Self, AOnUpdateValue));
end;

procedure TCustomListBoxHelper.AddValidator(AOnValidateValue: TPredicate<Integer>);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<Integer>(AOnValidateValue));
end;

procedure TCustomListBoxHelper.AddValidator(AOnValidateValue: TPredicate<string>);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<string>(AOnValidateValue));
end;

procedure TCustomMemoHelper.AddObserver(AOnUpdateValue: TProc<TStrings>);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomMemoObserver.Create(Self, AOnUpdateValue));
end;

procedure TCustomMemoHelper.AddValidator(AOnValidateValue: TPredicate<TStrings>);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<TStrings>(AOnValidateValue));
end;

procedure TCustomMemoHelper.AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomMemoObserver.Create(Self, AOnUpdate, AOnModified));
end;

function TCustomMemoObserver.GetTrack: Boolean;
begin
  Result := False;
end;

procedure TCustomMemoObserver.NotifyValue(Sender: TObject);
begin
  DoNotifyValue(Target.Lines);
end;

procedure TCustomComboBoxObserver.NotifyValue(Sender: TObject);
begin
  DoNotifyValue(Target.GetValue);
end;

procedure TCustomListBoxObserver.NotifyValue(Sender: TObject);
begin
  DoNotifyValue(Target.GetValue);
end;

procedure TInternalCustomEditHelper.ReSelectAll;
begin
  if AutoSelect and Focused then
    SelectAll;
end;

function TInternalCustomComboBoxHelper.GetValue: string;
begin
  Result := Text;
end;

function TInternalCustomListBoxHelper.GetValue: string;
begin
  if ItemIndex < 0 then
    Result := ''
  else
    Result := Items[ItemIndex];
end;

procedure TCustomComboBoxIndexObserver.NotifyValue(Sender: TObject);
begin
  DoNotifyValue(Target.ItemIndex);
end;

procedure TCustomListBoxIndexObserver.NotifyValue(Sender: TObject);
begin
  DoNotifyValue(Target.ItemIndex);
end;

end.
