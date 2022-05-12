unit Common.Frame;

interface

uses
  Vcl.Forms,
  Cmon.DataStorage;

type
  TCommonFrame = class(TFrame)
  strict protected
    function GetStorageKey(DataStorage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(DataStorage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(DataStorage: TDataStorage); virtual;
    procedure InternalPrepareStorage(DataStorage: TDataStorage); virtual;
    procedure InternalSaveToStorage(DataStorage: TDataStorage); virtual;
  public
    procedure InitDefaults(DataStorage: TDataStorage); virtual;
    procedure LoadFromStorage(DataStorage: TDataStorage); virtual;
    procedure PrepareStorage(DataStorage: TDataStorage); virtual;
    procedure SaveToStorage(DataStorage: TDataStorage); virtual;
  end;

implementation

uses
  Cmon.Utilities;

{$R *.dfm}

function TCommonFrame.GetStorageKey(DataStorage: TDataStorage): string;
begin
  result := DataStorage.MakeStorageSubKey(Name);
end;

procedure TCommonFrame.InitDefaults(DataStorage: TDataStorage);
begin
  DataStorage.InitDefaults(Self);
  for var frame in ComponentsOf<TCommonFrame> do
    frame.InitDefaults(DataStorage);
  InternalInitDefaults(DataStorage);
end;

procedure TCommonFrame.InternalInitDefaults(DataStorage: TDataStorage);
begin
end;

procedure TCommonFrame.InternalLoadFromStorage(DataStorage: TDataStorage);
begin
end;

procedure TCommonFrame.InternalPrepareStorage(DataStorage: TDataStorage);
begin
end;

procedure TCommonFrame.InternalSaveToStorage(DataStorage: TDataStorage);
begin
end;

procedure TCommonFrame.LoadFromStorage(DataStorage: TDataStorage);
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

procedure TCommonFrame.PrepareStorage(DataStorage: TDataStorage);
begin
  DataStorage.StorageKey := GetStorageKeyFromAttribute(Self, GetStorageKey(DataStorage));
  InternalPrepareStorage(DataStorage);
end;

procedure TCommonFrame.SaveToStorage(DataStorage: TDataStorage);
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

end.
