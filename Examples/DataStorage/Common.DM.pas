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
    function GetDataStorage: TDataStorage; virtual;
    function GetStorageKey(DataStorage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(DataStorage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(DataStorage: TDataStorage); virtual;
    procedure InternalPrepareStorage(DataStorage: TDataStorage); virtual;
    procedure InternalSaveToStorage(DataStorage: TDataStorage); virtual;
    procedure PrepareStorage;
    property DataStorage: TDataStorage read GetDataStorage;
  public
    procedure InitDefaults;
    procedure LoadFromStorage;
    procedure SaveToStorage;
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

function TdmCommon.GetDataStorage: TDataStorage;
begin
  Result := TDataStorage.DefaultInstance;
end;

function TdmCommon.GetStorageKey(DataStorage: TDataStorage): string;
begin
  Result := Name;
end;

procedure TdmCommon.InitDefaults;
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
  DataStorage.PushStorageKey;
  try
    PrepareStorage;
    DataStorage.LoadFromStorage(Self);
    InternalLoadFromStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

procedure TdmCommon.PrepareStorage;
begin
  DataStorage.StorageKey := GetStorageKeyFromAttribute(Self, GetStorageKey(DataStorage));
  InternalPrepareStorage(DataStorage);
end;

procedure TdmCommon.SaveToStorage;
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage;
    DataStorage.SaveToStorage(Self);
    InternalSaveToStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

end.
