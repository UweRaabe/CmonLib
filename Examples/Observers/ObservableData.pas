unit ObservableData;

interface

uses
  System.SysUtils,
  Cmon.Observers,
  DataTypes;

type
  TObservableData = class(TData)
  public const
    { string representation of TData property names.
      I am eagerly waiting for the implementation of the top most voted feature request in QP:
      RSP-13290 "NameOf(T) compiler (magic) function" }
    cMyLines = 'MyLines';
    cMyListItem = 'MyListItem';
    cMyListItemIndex = 'MyListItemIndex';
    cMySelected = 'MySelected';
    cMySelectedIndex = 'MySelectedIndex';
    cMyString = 'MyString';
  private
    FObservers: TValueObservers<TData>;
  protected
    procedure MyLinesChanged; override;
    procedure MyListItemChanged; override;
    procedure MyListItemIndexChanged; override;
    procedure MySelectedChanged; override;
    procedure MySelectedIndexChanged; override;
    procedure MyStringChanged; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObserver<T>(const APropName: string; AOnValueChanged: TProc<T>); overload;
    procedure AddObserver<T>(const APropName: string; AOnValueChanged: TProc<TData,T>); overload;
    property Observers: TValueObservers<TData> read FObservers;
  end;

type
  TObservableDataWrapper = class(TObservableData)
  private
    FData: TData;
    procedure SetData(const Value: TData);
  protected
    procedure Changed; override;
  public
    constructor Create(AData: TData);
    property Data: TData read FData write SetData;
  end;

implementation

constructor TObservableData.Create;
begin
  inherited;
  FObservers := TValueObservers<TData>.Create(Self);
end;

destructor TObservableData.Destroy;
begin
  FObservers.Free;
  inherited Destroy;
end;

procedure TObservableData.AddObserver<T>(const APropName: string; AOnValueChanged: TProc<T>);
begin
  Observers.AddObserver<T>(APropName, AOnValueChanged);
end;

procedure TObservableData.AddObserver<T>(const APropName: string; AOnValueChanged: TProc<TData,T>);
begin
  Observers.AddObserver<T>(APropName, AOnValueChanged);
end;

procedure TObservableData.MyLinesChanged;
begin
  inherited;
  Observers.ValueChanged(cMyLines);
end;

procedure TObservableData.MyListItemChanged;
begin
  inherited;
  Observers.ValueChanged(cMyListItem);
end;

procedure TObservableData.MyListItemIndexChanged;
begin
  inherited;
  Observers.ValueChanged(cMyListItemIndex);
end;

procedure TObservableData.MySelectedChanged;
begin
  inherited;
  Observers.ValueChanged(cMySelected);
end;

procedure TObservableData.MySelectedIndexChanged;
begin
  inherited;
  Observers.ValueChanged(cMySelectedIndex);
end;

procedure TObservableData.MyStringChanged;
begin
  inherited;
  Observers.ValueChanged(cMyString);
end;

constructor TObservableDataWrapper.Create(AData: TData);
begin
  inherited Create;
  Data := AData;
end;

procedure TObservableDataWrapper.Changed;
begin
  inherited;
  FData.Assign(Self);
end;

procedure TObservableDataWrapper.SetData(const Value: TData);
begin
  if (FData <> Value) and (Self <> Value) then begin
    FData := Value;
    Assign(FData);
  end;
end;

end.
