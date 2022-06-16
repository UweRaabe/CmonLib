unit Cmon.Observers.Vcl;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.StdCtrls,
  Cmon.Observers;

type
  TCustomEditHelper = class helper for TCustomEdit
  public
    procedure AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil; APriority: TObserverPriority =
        TObserverPriority.Normal); overload;
    procedure AddObserver(AOnUpdateValue: TProc<string>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<string>; APriority: TObserverPriority = TObserverPriority.Normal);
  end;

type
  TCustomMemoHelper = class helper for TCustomMemo
  public
    procedure AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil; APriority: TObserverPriority =
        TObserverPriority.Normal); overload;
    procedure AddObserver(AOnUpdateValue: TProc<TStrings>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<TStrings>; APriority: TObserverPriority = TObserverPriority.Normal);
  end;

type
  TCustomComboBoxHelper = class helper for TCustomComboBox
  public
    procedure AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil; APriority: TObserverPriority =
        TObserverPriority.Normal); overload;
    procedure AddObserver(AOnUpdateValue: TProc<string>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
    procedure AddObserver(AOnUpdateValue: TProc<Integer>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<string>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<Integer>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
  end;

type
  TCustomListBoxHelper = class helper for TCustomListBox
  public
    procedure AddObserver(AOnUpdate: TNotifyEvent; AOnModified: TNotifyEvent = nil; APriority: TObserverPriority =
        TObserverPriority.Normal); overload;
    procedure AddObserver(AOnUpdateValue: TProc<string>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
    procedure AddObserver(AOnUpdateValue: TProc<Integer>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<string>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
    procedure AddValidator(AOnValidateValue: TPredicate<Integer>; APriority: TObserverPriority = TObserverPriority.Normal); overload;
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

procedure TCustomEditHelper.AddObserver(AOnUpdate, AOnModified: TNotifyEvent; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomEditObserver.Create(Self, AOnUpdate, AOnModified, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomEditHelper.AddObserver(AOnUpdateValue: TProc<string>; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomEditObserver.Create(Self, AOnUpdateValue, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomEditHelper.AddValidator(AOnValidateValue: TPredicate<string>; APriority: TObserverPriority);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<string>(AOnValidateValue), APriority);
end;

procedure TCustomComboBoxHelper.AddObserver(AOnUpdate, AOnModified: TNotifyEvent; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomComboBoxObserver.Create(Self, AOnUpdate, AOnModified, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomComboBoxHelper.AddObserver(AOnUpdateValue: TProc<string>; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomComboBoxObserver.Create(Self, AOnUpdateValue, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomComboBoxHelper.AddObserver(AOnUpdateValue: TProc<Integer>; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomComboBoxIndexObserver.Create(Self, AOnUpdateValue, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomComboBoxHelper.AddValidator(AOnValidateValue: TPredicate<Integer>; APriority: TObserverPriority);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<Integer>(AOnValidateValue), APriority);
end;

procedure TCustomComboBoxHelper.AddValidator(AOnValidateValue: TPredicate<string>; APriority: TObserverPriority);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<string>(AOnValidateValue), APriority);
end;

procedure TCustomListBoxHelper.AddObserver(AOnUpdate, AOnModified: TNotifyEvent; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomListBoxObserver.Create(Self, AOnUpdate, AOnModified, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomListBoxHelper.AddObserver(AOnUpdateValue: TProc<string>; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomListBoxObserver.Create(Self, AOnUpdateValue, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomListBoxHelper.AddObserver(AOnUpdateValue: TProc<Integer>; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomListBoxIndexObserver.Create(Self, AOnUpdateValue, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomListBoxHelper.AddValidator(AOnValidateValue: TPredicate<Integer>; APriority: TObserverPriority);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<Integer>(AOnValidateValue), APriority);
end;

procedure TCustomListBoxHelper.AddValidator(AOnValidateValue: TPredicate<string>; APriority: TObserverPriority);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<string>(AOnValidateValue), APriority);
end;

procedure TCustomMemoHelper.AddObserver(AOnUpdateValue: TProc<TStrings>; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomMemoObserver.Create(Self, AOnUpdateValue, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
end;

procedure TCustomMemoHelper.AddValidator(AOnValidateValue: TPredicate<TStrings>; APriority: TObserverPriority);
begin
  if not Assigned(AOnValidateValue) then Exit;
  AddObserver(TValidator.Create<TStrings>(AOnValidateValue), APriority);
end;

procedure TCustomMemoHelper.AddObserver(AOnUpdate, AOnModified: TNotifyEvent; APriority: TObserverPriority);
begin
  Observers.AddObserver(TObserverMapping.ControlValueID, TCustomMemoObserver.Create(Self, AOnUpdate, AOnModified, APriority));
  Observers.SortObservers(TObserverMapping.ControlValueID);
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
