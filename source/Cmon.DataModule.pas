unit Cmon.DataModule;

interface

uses
  System.Classes,
  Cmon.DataStorage;

type
  TdmCommon = class(TDataModule)
  strict protected
    function AutoDataStorage: Boolean; virtual;
    function GetDefaultDataStorage: TDataStorage; virtual;
    function GetStorageKey(DataStorage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(DataStorage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(DataStorage: TDataStorage); virtual;
    procedure InternalPrepareStorage(DataStorage: TDataStorage); virtual;
    procedure InternalSaveToStorage(DataStorage: TDataStorage); virtual;
    procedure PrepareStorage(DataStorage: TDataStorage); virtual;
    property DefaultDataStorage: TDataStorage read GetDefaultDataStorage;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
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
  
type
  { This allows to inherit a TDataModule without using visual inheritance keeping the IDE designer happy.
    Just use this unit in the interface part after System.Classes. }
  TDataModule = TdmCommon;

implementation

uses
  Cmon.Utilities;

function TdmCommon.AutoDataStorage: Boolean;
begin
  Result := True;
end;

procedure TdmCommon.DoCreate;
begin
  if AutoDataStorage then begin
    InitDefaults;
    LoadFromStorage;
  end;
  inherited;
end;

procedure TdmCommon.DoDestroy;
begin
  inherited;
  if AutoDataStorage then begin
    SaveToStorage;
  end;
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
