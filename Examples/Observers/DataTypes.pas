unit DataTypes;

interface

uses
  System.Classes;

type
  TData = class
  private
    FMyLines: TStrings;
    FMyListItem: string;
    FMyListItemIndex: Integer;
    FMySelected: string;
    FMySelectedIndex: Integer;
    FMyString: string;
    function GetMyLines: TStrings;
    procedure MyLinesChanged;
    procedure MyListItemChanged;
    procedure MyListItemIndexChanged;
    procedure MySelectedChanged;
    procedure MySelectedIndexChanged;
    procedure MyStringChanged;
    procedure SetMyLines(Value: TStrings);
    procedure SetMyListItem(const Value: string);
    procedure SetMyListItemIndex(const Value: Integer);
    procedure SetMySelected(const Value: string);
    procedure SetMySelectedIndex(const Value: Integer);
    procedure SetMyString(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function IsMyStringValid(AValue: string): Boolean;
    property MyLines: TStrings read GetMyLines write SetMyLines;
    property MyListItem: string read FMyListItem write SetMyListItem;
    property MyListItemIndex: Integer read FMyListItemIndex write SetMyListItemIndex;
    property MySelected: string read FMySelected write SetMySelected;
    property MySelectedIndex: Integer read FMySelectedIndex write SetMySelectedIndex;
    property MyString: string read FMyString write SetMyString;
  end;

implementation

uses
  System.SysUtils,
  Cmon.Logging, Cmon.Dialogs;

constructor TData.Create;
begin
  inherited;
  FMyListItemIndex := -1;
  FMySelectedIndex := -1;
end;

destructor TData.Destroy;
begin
  FMyLines.Free;
  inherited Destroy;
end;

function TData.GetMyLines: TStrings;
begin
  if FMyLines = nil then begin
    FMyLines := TStringList.Create;
  end;
  Result := FMyLines;
end;

function TData.IsMyStringValid(AValue: string): Boolean;
begin
  Result := AValue.StartsWith('Hello');
  if not Result then
    TMessageDlg.Error(nil, 'Validation Error', 'MyString has to start with "Hello"!');
end;

procedure TData.MyLinesChanged;
begin
  TLog.Send('MyLines changed to: %s', [MyLines.CommaText]);
end;

procedure TData.MyListItemChanged;
begin
  TLog.Send('MyListItem changed to: %s', [MyListItem]);
end;

procedure TData.MyListItemIndexChanged;
begin
  TLog.Send('MyListItemIndex changed to: %d', [FMyListItemIndex]);
end;

procedure TData.MySelectedChanged;
begin
  TLog.Send('MySelected changed to: %s', [MySelected])
end;

procedure TData.MySelectedIndexChanged;
begin
  TLog.Send('MySelctedIndex changed to: %d', [FMySelectedIndex]);
end;

procedure TData.MyStringChanged;
begin
  TLog.Send('MySelected changed to: %s', [MyString]);
end;

procedure TData.SetMyLines(Value: TStrings);
begin
  if not MyLines.Equals(Value) then
  begin
    MyLines.Assign(Value);
    MyLinesChanged;
  end;
end;

procedure TData.SetMyListItem(const Value: string);
begin
  if FMyListItem <> Value then
  begin
    FMyListItem := Value;
    MyListItemChanged;
  end;
end;

procedure TData.SetMyListItemIndex(const Value: Integer);
begin
  if FMyListItemIndex <> Value then
  begin
    FMyListItemIndex := Value;
    MyListItemIndexChanged;
  end;
end;

procedure TData.SetMySelected(const Value: string);
begin
  if FMySelected <> Value then
  begin
    FMySelected := Value;
    MySelectedChanged;
  end;
end;

procedure TData.SetMySelectedIndex(const Value: Integer);
begin
  if FMySelectedIndex <> Value then
  begin
    FMySelectedIndex := Value;
    MySelectedIndexChanged;
  end;
end;

procedure TData.SetMyString(const Value: string);
begin
  if (FMyString <> Value) then
  begin
    FMyString := Value;
    MyStringChanged;
  end;
end;

end.
