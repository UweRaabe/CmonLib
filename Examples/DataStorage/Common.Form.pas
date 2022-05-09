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
    function GetDataStorage: TDataStorage; virtual;
    procedure PrepareStorage;
    property DataStorage: TDataStorage read GetDataStorage;
  protected
    function GetStorageKey(DataStorage: TDataStorage): string; virtual;
    procedure InternalLoadFromStorage(DataStorage: TDataStorage); virtual;
    procedure InternalSaveToStorage(DataStorage: TDataStorage); virtual;
    procedure InternalInitDefaults(DataStorage: TDataStorage); virtual;
    procedure InternalPrepareStorage(DataStorage: TDataStorage); virtual;
  public
    procedure InitDefaults;
    procedure LoadFromStorage;
    procedure SaveToStorage;
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

function TCommonForm.GetDataStorage: TDataStorage;
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
  DataStorage.InitDefaults(Self);
  ForAllComponentsOf<TCommonFrame>(
    procedure(Arg: TCommonFrame)
    begin
      Arg.InitDefaults(DataStorage);
    end);
  InternalInitDefaults(DataStorage);
end;

procedure TCommonForm.LoadFromStorage;
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage;
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

procedure TCommonForm.PrepareStorage;
begin
  DataStorage.StorageKey := GetStorageKeyFromAttribute(Self, GetStorageKey(DataStorage));
  InternalPrepareStorage(DataStorage);
end;

procedure TCommonForm.SaveToStorage;
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage;
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
