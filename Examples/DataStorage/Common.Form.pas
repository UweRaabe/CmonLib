unit Common.Form;

interface

uses
  Vcl.Forms,
  Cmon.DataStorage;

type
  TCommonForm = class(TForm)
  strict protected
    function AutoDataStorage: Boolean; virtual;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
    function GetDefaultDataStorage: TDataStorage; virtual;
    function GetStorageKey(Storage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(Storage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(Storage: TDataStorage); virtual;
    procedure InternalPrepareStorage(Storage: TDataStorage); virtual;
    procedure InternalSaveToStorage(Storage: TDataStorage); virtual;
    procedure PrepareStorage(Storage: TDataStorage); virtual;
  public
    procedure InitDefaults; overload; virtual;
    procedure InitDefaults(Storage: TDataStorage); overload; virtual;
    procedure LoadFromStorage; overload; virtual;
    procedure LoadFromStorage(Storage: TDataStorage); overload; virtual;
    procedure LoadFromStorage(ATarget: IStorageTarget); overload;
    procedure LoadFromStorage(const AFileName: string); overload;
    procedure SaveToStorage; overload; virtual;
    procedure SaveToStorage(Storage: TDataStorage); overload; virtual;
    procedure SaveToStorage(ATarget: IStorageTarget); overload;
    procedure SaveToStorage(const AFileName: string); overload;
    property DefaultDataStorage: TDataStorage read GetDefaultDataStorage;
  end;
  
type
  { This allows to inherit a TForm without using visual inheritance keeping the IDE designer happy.
    Just use this unit in the interface part after Vcl.Forms. }
  TForm = TCommonForm;

implementation

uses
  Cmon.Utilities,
  Common.Frame;

function TCommonForm.AutoDataStorage: Boolean;
begin
  Result := True;
end;

procedure TCommonForm.DoCreate;
begin
  if AutoDataStorage then begin
    InitDefaults;
    LoadFromStorage;
  end;
  inherited;
end;

procedure TCommonForm.DoDestroy;
begin
  inherited;
  if AutoDataStorage then begin
    SaveToStorage;
  end;
end;

function TCommonForm.GetDefaultDataStorage: TDataStorage;
begin
  Result := TDataStorage.DefaultInstance;
end;

function TCommonForm.GetStorageKey(Storage: TDataStorage): string;
begin
  Result := Name;
end;

procedure TCommonForm.InitDefaults;
begin
  InitDefaults(DefaultDataStorage);
end;

procedure TCommonForm.InitDefaults(Storage: TDataStorage);
begin
  Storage.InitDefaults(Self);
  for var frame in ComponentsOf<TCommonFrame> do
    frame.InitDefaults(Storage);
  InternalInitDefaults(Storage);
end;

procedure TCommonForm.InternalInitDefaults(Storage: TDataStorage);
begin
end;

procedure TCommonForm.InternalLoadFromStorage(Storage: TDataStorage);
begin
end;

procedure TCommonForm.InternalPrepareStorage(Storage: TDataStorage);
begin
end;

procedure TCommonForm.InternalSaveToStorage(Storage: TDataStorage);
begin
end;

procedure TCommonForm.LoadFromStorage;
begin
  LoadFromStorage(DefaultDataStorage);
end;

procedure TCommonForm.LoadFromStorage(Storage: TDataStorage);
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

procedure TCommonForm.LoadFromStorage(const AFileName: string);
begin
  LoadFromStorage(TDataStorage.CreateStorageTarget(Self, AFileName));
end;

procedure TCommonForm.PrepareStorage(Storage: TDataStorage);
begin
  Storage.StorageKey := GetStorageKeyFromAttribute(Self, GetStorageKey(Storage));
  InternalPrepareStorage(Storage);
end;

procedure TCommonForm.SaveToStorage;
begin
  SaveToStorage(DefaultDataStorage);
end;

procedure TCommonForm.SaveToStorage(Storage: TDataStorage);
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

procedure TCommonForm.SaveToStorage(const AFileName: string);
begin
  SaveToStorage(TDataStorage.CreateStorageTarget(Self, AFileName));
end;

end.
