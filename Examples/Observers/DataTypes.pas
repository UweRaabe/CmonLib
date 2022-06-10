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
    procedure SetMyLines(Value: TStrings);
    procedure SetMyListItem(const Value: string);
    procedure SetMyListItemIndex(const Value: Integer);
    procedure SetMySelected(const Value: string);
    procedure SetMySelectedIndex(const Value: Integer);
    procedure SetMyString(const Value: string);
  protected
    procedure Changed; virtual;
    procedure MyLinesChanged; virtual;
    procedure MyListItemChanged; virtual;
    procedure MyListItemIndexChanged; virtual;
    procedure MySelectedChanged; virtual;
    procedure MySelectedIndexChanged; virtual;
    procedure MyStringChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TData); virtual;
    function IsMyStringValid(AValue: string): Boolean;
    function IsMyStringValidNoMsg(AValue: string): Boolean;
    property MyLines: TStrings read FMyLines write SetMyLines;
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
  FMyLines := TStringList.Create;

  FMyLines.AddStrings(['Hello', 'World']);
  FMyListItemIndex := 2;
  FMySelectedIndex := -1;
  FMySelected := 'Hurz';
  FMyString := 'Hello World';
end;

destructor TData.Destroy;
begin
  FMyLines.Free;
  inherited Destroy;
end;

procedure TData.Assign(Source: TData);
begin
  FMyLines.Assign(Source.FMyLines);
  FMyListItem := Source.FMyListItem;
  FMyListItemIndex := Source.FMyListItemIndex;
  FMySelected := Source.FMySelected;
  FMySelectedIndex := Source.FMySelectedIndex;
  FMyString := Source.FMyString;
end;

procedure TData.Changed;
begin
end;

function TData.IsMyStringValid(AValue: string): Boolean;
begin
  Result := IsMyStringValidNoMsg(AValue);
  if not Result then
    TMessageDlg.Error(nil, 'Validation Error', 'MyString has to start with "Hello"!');
end;

function TData.IsMyStringValidNoMsg(AValue: string): Boolean;
begin
  Result := AValue.IsEmpty or AValue.StartsWith('Hello');
end;

procedure TData.MyLinesChanged;
begin
  TLog.Send('MyLines changed to: %s', [MyLines.CommaText]);
  Changed;
end;

procedure TData.MyListItemChanged;
begin
  TLog.Send('MyListItem changed to: %s', [MyListItem]);
  Changed;
end;

procedure TData.MyListItemIndexChanged;
begin
  TLog.Send('MyListItemIndex changed to: %d', [FMyListItemIndex]);
  Changed;
end;

procedure TData.MySelectedChanged;
begin
  TLog.Send('MySelected changed to: %s', [MySelected]);
  Changed;
end;

procedure TData.MySelectedIndexChanged;
begin
  TLog.Send('MySelectedIndex changed to: %d', [FMySelectedIndex]);
  Changed;
end;

procedure TData.MyStringChanged;
begin
  TLog.Send('MyString changed to: %s', [MyString]);
  Changed;
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
