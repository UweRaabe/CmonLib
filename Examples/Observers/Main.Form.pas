unit Main.Form;

interface

uses
  System.Classes, System.Messaging,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  DataTypes;

type
  TForm702 = class(TForm)
    LogMemo: TMemo;
    MyStringEdit: TEdit;
    MyLinesMemo: TMemo;
    MySelectedComboBox: TComboBox;
    MyListItemListBox: TListBox;
  private
    FData: TData;
    FLogHandlerId: Integer;
    procedure DoLogMessage(const AMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Data: TData read FData;
  end;

var
  Form702: TForm702;

implementation

{$R *.dfm}

uses
  System.SysUtils,
  Vcl.Dialogs,
  Cmon.Observers.Vcl, Cmon.Logging;

constructor TForm702.Create(AOwner: TComponent);
begin
  inherited;

  { register to receive log messages }
  LogMemo.Clear;
  FLogHandlerId := TLog.Subscribe(DoLogMessage);

  FData := TData.Create;

  { load initial data }
  MyStringEdit.Text := Data.MyString;
  MyLinesMemo.Lines := Data.MyLines;
  MySelectedComboBox.Text := Data.MySelected;
  MySelectedComboBox.ItemIndex := Data.MySelectedIndex;
  MyListItemListBox.ItemIndex := Data.MyListItemIndex;

  { link observers }
  MyStringEdit.AddValidator(Data.IsMyStringValid);
  MyStringEdit.AddObserver(procedure(AValue: string) begin Data.MyString := AValue end);
  MyLinesMemo.AddObserver(procedure(AValue: TStrings) begin Data.MyLines := AValue end);
  MySelectedComboBox.AddObserver(procedure(AValue: string) begin Data.MySelected := AValue end);
  MySelectedComboBox.AddObserver(procedure(AValue: Integer) begin Data.MySelectedIndex := AValue end);
  MyListItemListBox.AddObserver(procedure(AValue: string) begin Data.MyListItem := AValue end);
  MyListItemListBox.AddObserver(procedure(AValue: Integer) begin Data.MyListItemIndex := AValue end);
end;

destructor TForm702.Destroy;
begin
  TLog.Unsubscribe(FLogHandlerId);
  FData.Free;
  inherited Destroy;
end;

procedure TForm702.DoLogMessage(const AMessage: string);
begin
  LogMemo.Lines.Add(AMessage);
end;

end.
