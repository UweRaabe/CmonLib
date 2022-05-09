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
    procedure InitDefaults(DataStorage: TDataStorage);
    procedure LoadFromStorage(DataStorage: TDataStorage);
    procedure PrepareStorage(DataStorage: TDataStorage);
    procedure SaveToStorage(DataStorage: TDataStorage);
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
  ForAllComponentsOf<TCommonFrame>(
    procedure(Arg: TCommonFrame)
    begin
      Arg.InitDefaults(DataStorage);
    end);
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
    ForAllComponentsOf<TCommonFrame>(
      procedure(Arg: TCommonFrame)
      begin
        Arg.LoadFromStorage(DataStorage);
      end);
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
    ForAllComponentsOf<TCommonFrame>(
      procedure(Arg: TCommonFrame)
      begin
        Arg.SaveToStorage(DataStorage);
      end);
    InternalSaveToStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

end.
