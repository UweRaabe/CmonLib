unit Common.Frame;

interface

uses
  Vcl.Forms,
  Cmon.DataStorage;

type
  TCommonFrame = class(TFrame)
  protected
    function GetStorageKey(Storage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(Storage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(Storage: TDataStorage); virtual;
    procedure InternalPrepareStorage(Storage: TDataStorage); virtual;
    procedure InternalSaveToStorage(Storage: TDataStorage); virtual;
  public
    procedure InitDefaults(Storage: TDataStorage); virtual;
    procedure LoadFromStorage(Storage: TDataStorage); virtual;
    procedure PrepareStorage(Storage: TDataStorage); virtual;
    procedure SaveToStorage(Storage: TDataStorage); virtual;
  end;
  
type
  TFrame = TCommonFrame;

implementation

uses
  Cmon.Utilities;

function TCommonFrame.GetStorageKey(Storage: TDataStorage): string;
begin
  Result := Storage.MakeStorageSubKey(Name);
end;

procedure TCommonFrame.InitDefaults(Storage: TDataStorage);
begin
  Storage.InitDefaults(Self);
  for var frame in ComponentsOf<TCommonFrame> do
    frame.InitDefaults(Storage);
  InternalInitDefaults(Storage);
end;

procedure TCommonFrame.InternalInitDefaults(Storage: TDataStorage);
begin
end;

procedure TCommonFrame.InternalLoadFromStorage(Storage: TDataStorage);
begin
end;

procedure TCommonFrame.InternalPrepareStorage(Storage: TDataStorage);
begin
end;

procedure TCommonFrame.InternalSaveToStorage(Storage: TDataStorage);
begin
end;

procedure TCommonFrame.LoadFromStorage(Storage: TDataStorage);
begin
  Storage.PushStorageKey;
  try
    PrepareStorage(Storage);
    Storage.LoadFromStorage(Self);
    for var frame in ComponentsOf<TCommonFrame> do
      frame.LoadFromStorage(Storage);
    InternalLoadFromStorage(Storage);
  finally
    Storage.PopStorageKey;
  end;
end;

procedure TCommonFrame.PrepareStorage(Storage: TDataStorage);
begin
  Storage.StorageKey := GetStorageKeyFromAttribute(Self, GetStorageKey(Storage));
  InternalPrepareStorage(Storage);
end;

procedure TCommonFrame.SaveToStorage(Storage: TDataStorage);
begin
  Storage.PushStorageKey;
  try
    PrepareStorage(Storage);
    Storage.SaveToStorage(Self);
    for var frame in ComponentsOf<TCommonFrame> do
      frame.SaveToStorage(Storage);
    InternalSaveToStorage(Storage);
  finally
    Storage.PopStorageKey;
  end;
end;

end.
