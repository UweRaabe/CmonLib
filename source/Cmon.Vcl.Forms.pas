unit Cmon.Vcl.Forms;

interface

uses
  System.Classes,
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
    procedure AfterConstruction; override;
    procedure InitDefaults(Storage: TDataStorage); virtual;
    procedure LoadFromStorage(Storage: TDataStorage); virtual;
    procedure PrepareStorage(Storage: TDataStorage); virtual;
    procedure SaveToStorage(Storage: TDataStorage); virtual;
  end;

type
  INotifyOwner = interface
    ['{08BC84AD-F50A-46F8-8E0A-4D87D8872794}']
    procedure NotifyLoaded(Sender: TCommonFrame);
  end;

type
  TCommonForm = class(TForm, INotifyOwner)
  strict private
    procedure NotifyLoaded(Sender: TCommonFrame);
  private
    FAutoDataStorage: Boolean;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
    function GetDefaultDataStorage: TDataStorage; virtual;
    function GetStorageKey(Storage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(Storage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(Storage: TDataStorage); virtual;
    procedure InternalPrepareStorage(Storage: TDataStorage); virtual;
    procedure InternalSaveToStorage(Storage: TDataStorage); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PrepareStorage(Storage: TDataStorage); virtual;
  public
    procedure InitDefaults; overload; virtual;
    procedure InitDefaults(Storage: TDataStorage); overload; virtual;
    procedure InitDefaults(AFrame: TCommonFrame); overload; virtual;
    procedure InitDefaults(AFrame: TCommonFrame; Storage: TDataStorage); overload; virtual;
    procedure LoadFromStorage; overload; virtual;
    procedure LoadFromStorage(Storage: TDataStorage); overload; virtual;
    procedure LoadFromStorage(ATarget: IStorageTarget); overload;
    procedure LoadFromStorage(const AFileName: string); overload;
    procedure LoadFromStorage(AFrame: TCommonFrame); overload; virtual;
    procedure LoadFromStorage(AFrame: TCommonFrame; Storage: TDataStorage); overload; virtual;
    procedure SaveToStorage; overload; virtual;
    procedure SaveToStorage(Storage: TDataStorage); overload; virtual;
    procedure SaveToStorage(ATarget: IStorageTarget); overload;
    procedure SaveToStorage(const AFileName: string); overload;
    procedure SaveToStorage(AFrame: TCommonFrame); overload; virtual;
    procedure SaveToStorage(AFrame: TCommonFrame; Storage: TDataStorage); overload; virtual;
    property AutoDataStorage: Boolean read FAutoDataStorage write FAutoDataStorage;
    property DefaultDataStorage: TDataStorage read GetDefaultDataStorage;
  end;

type
  { This allows to inherit a TForm or TFrame without using visual inheritance keeping the IDE designer happy.
    Just use this unit in the interface part after Vcl.Forms. }
  TForm = TCommonForm;
  TFrame = TCommonFrame;

implementation

uses
  System.SysUtils,
  Cmon.Utilities;

procedure TCommonForm.DoCreate;
begin
  inherited;
  if AutoDataStorage then begin
    InitDefaults;
    LoadFromStorage;
  end;
end;

procedure TCommonForm.DoDestroy;
begin
  if AutoDataStorage then begin
    SaveToStorage;
  end;
  AutoDataStorage := False;
  inherited;
end;

function TCommonForm.GetDefaultDataStorage: TDataStorage;
begin
  Result := TDataStorage.DefaultInstance;
end;

function TCommonForm.GetStorageKey(Storage: TDataStorage): string;
begin
  var key: string := Name;
  if key.IsEmpty then
    key := ClassName.Substring(1);

  Result := Storage.MakeStorageSubKey(key);
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

procedure TCommonForm.InitDefaults(AFrame: TCommonFrame);
begin
  InitDefaults(AFrame, DefaultDataStorage);
end;

procedure TCommonForm.InitDefaults(AFrame: TCommonFrame; Storage: TDataStorage);
begin
  AFrame.InitDefaults(Storage);
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

procedure TCommonForm.LoadFromStorage(AFrame: TCommonFrame);
begin
  LoadFromStorage(AFrame, DefaultDataStorage);
end;

procedure TCommonForm.LoadFromStorage(AFrame: TCommonFrame; Storage: TDataStorage);
begin
  Storage.PushStorageKey;
  try
    PrepareStorage(Storage);
    AFrame.LoadFromStorage(Storage);
  finally
    Storage.PopStorageKey;
  end;
end;

procedure TCommonForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if AComponent is TCommonFrame then begin
    var frame := TCommonFrame(AComponent);
    case Operation of
      opInsert: begin
        { This is too early for InitDefaults and LoadFromStorage of the frame.
          We have to wait for the frame to notify us when loaded. }
      end;
      opRemove: begin
        if AutoDataStorage then
          SaveToStorage(frame);
      end;
    end;
  end;
end;

procedure TCommonForm.NotifyLoaded(Sender: TCommonFrame);
begin
  if AutoDataStorage then begin
    InitDefaults(Sender);
    LoadFromStorage(Sender);
  end;
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

procedure TCommonForm.SaveToStorage(AFrame: TCommonFrame);
begin
  SaveToStorage(AFrame, DefaultDataStorage);
end;

procedure TCommonForm.SaveToStorage(AFrame: TCommonFrame; Storage: TDataStorage);
begin
  Storage.PushStorageKey;
  try
    PrepareStorage(Storage);
    AFrame.SaveToStorage(Storage);
  finally
    Storage.PopStorageKey;
  end;
end;

procedure TCommonFrame.AfterConstruction;
var
  intf: INotifyOwner;
begin
  inherited;
  if Supports(Owner, INotifyOwner, intf) then
    intf.NotifyLoaded(Self);
end;

function TCommonFrame.GetStorageKey(Storage: TDataStorage): string;
begin
  var
    key: string := Name;
  if key.IsEmpty then
    key := ClassName.Substring(1);

  Result := Storage.MakeStorageSubKey(key);
end;

procedure TCommonFrame.InitDefaults(Storage: TDataStorage);
begin
  Storage.InitDefaults(Self);
  for var Frame in ComponentsOf<TCommonFrame> do
    Frame.InitDefaults(Storage);
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
    for var Frame in ComponentsOf<TCommonFrame> do
      Frame.LoadFromStorage(Storage);
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
    for var Frame in ComponentsOf<TCommonFrame> do
      Frame.SaveToStorage(Storage);
    InternalSaveToStorage(Storage);
  finally
    Storage.PopStorageKey;
  end;
end;

end.
