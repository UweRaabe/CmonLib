unit Common.DM;

interface

uses
  System.Classes,
  Cmon.DataStorage;

type
  TdmCommon = class(TDataModule)
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  strict protected
    function GetDefaultDataStorage: TDataStorage; virtual;
    function GetStorageKey(DataStorage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(DataStorage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(DataStorage: TDataStorage); virtual;
    procedure InternalPrepareStorage(DataStorage: TDataStorage); virtual;
    procedure InternalSaveToStorage(DataStorage: TDataStorage); virtual;
    procedure PrepareStorage(DataStorage: TDataStorage); virtual;
    property DefaultDataStorage: TDataStorage read GetDefaultDataStorage;
  public
    procedure InitDefaults; overload; virtual;
    procedure InitDefaults(DataStorage: TDataStorage); overload; virtual;
    procedure LoadFromStorage; overload; virtual;
    procedure LoadFromStorage(DataStorage: TDataStorage); overload; virtual;
    procedure LoadFromStorage(ATarget: IStorageTarget); overload;
    procedure SaveToStorage; overload; virtual;
    procedure SaveToStorage(DataStorage: TDataStorage); overload; virtual;
    procedure SaveToStorage(ATarget: IStorageTarget); overload;
  end;

implementation

uses
  Cmon.Utilities;

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

procedure TdmCommon.DataModuleDestroy(Sender: TObject);
begin
  SaveToStorage;
  inherited;
end;

procedure TdmCommon.DataModuleCreate(Sender: TObject);
begin
  inherited;
  InitDefaults;
  LoadFromStorage;
end;

function TdmCommon.GetDefaultDataStorage: TDataStorage;
begin
  Result := TDataStorage.DefaultInstance;
end;

function TdmCommon.GetStorageKey(DataStorage: TDataStorage): string;
begin
  Result := Name;
end;

procedure TdmCommon.InitDefaults;
begin
  InitDefaults(DefaultDataStorage);
end;

procedure TdmCommon.InitDefaults(DataStorage: TDataStorage);
begin
  DataStorage.InitDefaults(Self);
  InternalInitDefaults(DataStorage);
end;

procedure TdmCommon.InternalInitDefaults(DataStorage: TDataStorage);
begin
end;

procedure TdmCommon.InternalLoadFromStorage(DataStorage: TDataStorage);
begin
end;

procedure TdmCommon.InternalPrepareStorage(DataStorage: TDataStorage);
begin
end;

procedure TdmCommon.InternalSaveToStorage(DataStorage: TDataStorage);
begin
end;

procedure TdmCommon.LoadFromStorage;
begin
  LoadFromStorage(DefaultDataStorage);
end;

procedure TdmCommon.LoadFromStorage(DataStorage: TDataStorage);
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage(DataStorage);
    DataStorage.LoadFromStorage(Self);
    InternalLoadFromStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

procedure TdmCommon.LoadFromStorage(ATarget: IStorageTarget);
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

procedure TdmCommon.PrepareStorage(DataStorage: TDataStorage);
begin
  DataStorage.StorageKey := GetStorageKeyFromAttribute(Self, GetStorageKey(DataStorage));
  InternalPrepareStorage(DataStorage);
end;

procedure TdmCommon.SaveToStorage;
begin
  SaveToStorage(DefaultDataStorage);
end;

procedure TdmCommon.SaveToStorage(DataStorage: TDataStorage);
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage(DataStorage);
    DataStorage.SaveToStorage(Self);
    InternalSaveToStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

procedure TdmCommon.SaveToStorage(ATarget: IStorageTarget);
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
