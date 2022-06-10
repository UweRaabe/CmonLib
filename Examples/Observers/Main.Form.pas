unit Main.Form;

interface

uses
  System.Classes, System.Messaging,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  DataTypes;

type
  TDemoMainForm = class(TForm)
    LogMemo: TMemo;
    MyStringEdit: TEdit;
    MyLinesMemo: TMemo;
    MySelectedComboBox: TComboBox;
    MyListItemListBox: TListBox;
  private
    FData: TData;
    FLogHandlerId: Integer;
    procedure DoLogMessage(const AMessage: string);
    procedure SetData(const Value: TData);
  strict protected
    function CreateData(AData: TData = nil): TData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Data: TData read FData write SetData;
  end;

var
  DemoMainForm: TDemoMainForm;

implementation

{$R *.dfm}

uses
  System.SysUtils,
  Cmon.Logging, Cmon.Observers.Vcl,
  ObservableData;

var
  GlobalData: TData = nil;

constructor TDemoMainForm.Create(AOwner: TComponent);
begin
  inherited;

  { register to receive log messages }
  LogMemo.Clear;
  FLogHandlerId := TLog.Subscribe(DoLogMessage);

//  FData := CreateData;
  Data := GlobalData;

  { link observers }
  MyStringEdit.AddValidator(Data.IsMyStringValid);
  MyStringEdit.AddObserver(procedure(AValue: string) begin Data.MyString := AValue end);
  MyLinesMemo.AddObserver(procedure(AValue: TStrings) begin Data.MyLines := AValue end);
  MySelectedComboBox.AddObserver(procedure(AValue: Integer) begin Data.MySelectedIndex := AValue end);
  MySelectedComboBox.AddObserver(procedure(AValue: string) begin Data.MySelected := AValue end);
  MyListItemListBox.AddObserver(procedure(AValue: Integer) begin Data.MyListItemIndex := AValue end);
  MyListItemListBox.AddObserver(procedure(AValue: string) begin Data.MyListItem := AValue end);
end;

destructor TDemoMainForm.Destroy;
begin
  TLog.Unsubscribe(FLogHandlerId);
  FData.Free;
  inherited Destroy;
end;

function TDemoMainForm.CreateData(AData: TData = nil): TData;
var
  tmp: TObservableData;
begin
  if AData = nil then
    tmp := TObservableData.Create
  else
    tmp := TObservableDataWrapper.Create(AData);
  tmp.AddObserver<string>(tmp.cMyString, procedure(AValue: string) begin MyStringEdit.Text := AValue end);
  tmp.AddObserver<TStrings>(tmp.cMyLines, procedure(AValue: TStrings) begin MyLinesMemo.Lines := AValue end);
  tmp.AddObserver<Integer>(tmp.cMySelectedIndex, procedure(AValue: Integer) begin MySelectedComboBox.ItemIndex := AValue end);
  tmp.AddObserver<string>(tmp.cMySelected, procedure(AValue: string) begin MySelectedComboBox.Text := AValue end);
  tmp.AddObserver<Integer>(tmp.cMyListItemIndex, procedure(AValue: Integer) begin MyListItemListBox.ItemIndex := AValue end);

  tmp.AddObserver<string>(tmp.cMySelected,
    procedure(AData: TData; AValue: string)
    begin
      if (AData.MySelectedIndex < 0) and AData.IsMyStringValidNoMsg(AValue) then
        AData.MyString := AValue
    end);

  Result := tmp;
end;

procedure TDemoMainForm.DoLogMessage(const AMessage: string);
begin
  LogMemo.Lines.Add(AMessage);
end;

procedure TDemoMainForm.SetData(const Value: TData);
begin
  if FData <> Value then
  begin
    FData.Free;
    FData := CreateData(Value);
  end;
end;

initialization
  GlobalData := TData.Create;
finalization
  GlobalData.Free;
end.
