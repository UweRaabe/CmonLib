unit Common.Form;

interface

uses
  Vcl.Forms,
  Cmon.DataStorage;

type
  TCommonForm = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  strict protected
    function GetDefaultDataStorage: TDataStorage; virtual;
    procedure PrepareStorage(DataStorage: TDataStorage); virtual;
  protected
    function GetStorageKey(DataStorage: TDataStorage): string; virtual;
    procedure InternalLoadFromStorage(DataStorage: TDataStorage); virtual;
    procedure InternalSaveToStorage(DataStorage: TDataStorage); virtual;
    procedure InternalInitDefaults(DataStorage: TDataStorage); virtual;
    procedure InternalPrepareStorage(DataStorage: TDataStorage); virtual;
  public
    procedure InitDefaults; overload; virtual;
    procedure InitDefaults(DataStorage: TDataStorage); overload; virtual;
    procedure LoadFromStorage; overload; virtual;
    procedure LoadFromStorage(DataStorage: TDataStorage); overload; virtual;
    procedure LoadFromStorage(ATarget: IStorageTarget); overload;
    procedure SaveToStorage; overload; virtual;
    procedure SaveToStorage(DataStorage: TDataStorage); overload; virtual;
    procedure SaveToStorage(ATarget: IStorageTarget); overload;
    property DefaultDataStorage: TDataStorage read GetDefaultDataStorage;
  end;

implementation

uses
  Cmon.Utilities, Common.Frame;

{$R *.dfm}

procedure TCommonForm.FormDestroy(Sender: TObject);
begin
  SaveToStorage;
  inherited;
end;

procedure TCommonForm.FormCreate(Sender: TObject);
begin
  inherited;
  InitDefaults;
  LoadFromStorage;
end;

function TCommonForm.GetDefaultDataStorage: TDataStorage;
begin
  Result := TDataStorage.DefaultInstance;
end;

function TCommonForm.GetStorageKey(DataStorage: TDataStorage): string;
begin
  result := Name;
end;

procedure TCommonForm.InternalLoadFromStorage(DataStorage: TDataStorage);
begin
end;

procedure TCommonForm.InternalSaveToStorage(DataStorage: TDataStorage);
begin
end;

procedure TCommonForm.InternalInitDefaults(DataStorage: TDataStorage);
begin
end;

procedure TCommonForm.InternalPrepareStorage(DataStorage: TDataStorage);
begin
end;

procedure TCommonForm.InitDefaults;
begin
  InitDefaults(DefaultDataStorage);
end;

procedure TCommonForm.InitDefaults(DataStorage: TDataStorage);
begin
  DataStorage.InitDefaults(Self);
  for var frame in ComponentsOf<TCommonFrame> do
    frame.InitDefaults(DataStorage);
  InternalInitDefaults(DataStorage);
end;

procedure TCommonForm.LoadFromStorage;
begin
  LoadFromStorage(DefaultDataStorage);
end;

procedure TCommonForm.LoadFromStorage(DataStorage: TDataStorage);
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage(DataStorage);
    DataStorage.LoadFromStorage(Self);
    for var frame in ComponentsOf<TCommonFrame> do
      frame.LoadFromStorage(DataStorage);
    InternalLoadFromStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

procedure TCommonForm.LoadFromStorage(ATarget: IStorageTarget);
var
  tmpStorage: TDataStorage;
begin
  tmpStorage := TDataStorage.Create;
  try
    tmpStorage.StorageTarget := ATarget;
    LoadFromStorage(tmpStorage);
  finally
    tmpStorage.Free;
  end;
end;

procedure TCommonForm.PrepareStorage(DataStorage: TDataStorage);
begin
  DataStorage.StorageKey := GetStorageKeyFromAttribute(Self, GetStorageKey(DataStorage));
  InternalPrepareStorage(DataStorage);
end;

procedure TCommonForm.SaveToStorage;
begin
  SaveToStorage(DefaultDataStorage);
end;

procedure TCommonForm.SaveToStorage(DataStorage: TDataStorage);
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage(DataStorage);
    DataStorage.SaveToStorage(Self);
    for var frame in ComponentsOf<TCommonFrame> do
      frame.SaveToStorage(DataStorage);
    InternalSaveToStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

procedure TCommonForm.SaveToStorage(ATarget: IStorageTarget);
var
  tmpStorage: TDataStorage;
begin
  tmpStorage := TDataStorage.Create;
  try
    tmpStorage.StorageTarget := ATarget;
    SaveToStorage(tmpStorage);
  finally
    tmpStorage.Free;
  end;
end;

end.
