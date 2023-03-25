unit Cmon.Vcl.Forms;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls,
  Cmon.DataStorage;

type
{$SCOPEDENUMS ON}
  /// <summary>
  ///   Declares the possible values to defines if and when loading and storing
  ///   data is executed.
  /// </summary>
  /// <seealso cref="TCommonForm.AutoDataStorage">
  ///   AutoDataStorage
  /// </seealso>
  TAutoDataStorage = (
    /// <summary>
    ///   No automatic loading or storeing.
    /// </summary>
    none,
    /// <summary>
    ///   Loading is done after all OnCreate events, while storing happens
    ///   before any OnDestroy event.
    /// </summary>
    callInside,
    /// <summary>
    ///   Loading is done before any OnCreate event, while storing happens
    ///   after all OnDestroy events. <br />
    /// </summary>
    callOutside);
{$SCOPEDENUMS OFF}

type
  TCommonFrame = class(TFrame)
  protected
    function GetStorageKey(Storage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(Storage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(Storage: TDataStorage); virtual;
    procedure InternalPrepareStorage(Storage: TDataStorage); virtual;
    procedure InternalSaveToStorage(Storage: TDataStorage); virtual;
    procedure SetParent(AParent: TWinControl); override;
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
  IRedirectReferences = interface
    ['{346E9829-E9A8-4E1A-BAF1-9B1FD033562E}']
    procedure RedirectReferences;
  end;

type
  TDataModuleClass = class of TDataModule;
  TCommonForm = class(TForm, INotifyOwner, IRedirectReferences)
  strict private
    procedure NotifyLoaded(Sender: TCommonFrame);
  private
  class var
    FDefaultAutoDataStorage: TAutoDataStorage;
    FDefaultHandleGlobalVirtualImageLists: Boolean;
  var
    FAutoDataStorage: TAutoDataStorage;
    FHandledReferences: TStrings;
    FHandleGlobalVirtualImageLists: Boolean;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
    function GetAutoDataStorage: TAutoDataStorage; virtual;
    function GetDefaultDataStorage: TDataStorage; virtual;
    function GetStorageKey(Storage: TDataStorage): string; virtual;
    procedure GrabVirtualImageLists(AClass: TDataModuleClass);
    procedure InternalInitDefaults(Storage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(Storage: TDataStorage); virtual;
    procedure InternalPrepareStorage(Storage: TDataStorage); virtual;
    procedure InternalSaveToStorage(Storage: TDataStorage); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PrepareStorage(Storage: TDataStorage); virtual;
    procedure RedirectReferences;
    procedure ResolveReferences;
    property HandledReferences: TStrings read FHandledReferences;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;
    procedure InitDefaults; overload; virtual;
    procedure InitDefaults(AFrame: TCommonFrame); overload; virtual;
    procedure InitDefaults(AFrame: TCommonFrame; Storage: TDataStorage); overload; virtual;
    procedure InitDefaults(Storage: TDataStorage); overload; virtual;
    procedure LoadFromStorage; overload; virtual;
    procedure LoadFromStorage(const AFileName: string); overload;
    procedure LoadFromStorage(ATarget: IStorageTarget); overload;
    procedure LoadFromStorage(AFrame: TCommonFrame); overload; virtual;
    procedure LoadFromStorage(AFrame: TCommonFrame; Storage: TDataStorage); overload; virtual;
    procedure LoadFromStorage(Storage: TDataStorage); overload; virtual;
    procedure SaveToStorage; overload; virtual;
    procedure SaveToStorage(const AFileName: string); overload;
    procedure SaveToStorage(ATarget: IStorageTarget); overload;
    procedure SaveToStorage(AFrame: TCommonFrame); overload; virtual;
    procedure SaveToStorage(AFrame: TCommonFrame; Storage: TDataStorage); overload; virtual;
    procedure SaveToStorage(Storage: TDataStorage); overload; virtual;
    /// <summary>
    ///   The value <i>AutoDataStorage</i> is initialized with in a new form
    ///   instance
    /// </summary>
    class property DefaultAutoDataStorage: TAutoDataStorage read FDefaultAutoDataStorage write FDefaultAutoDataStorage;
    class property DefaultHandleGlobalVirtualImageLists: Boolean read FDefaultHandleGlobalVirtualImageLists write FDefaultHandleGlobalVirtualImageLists;
    /// <summary>
    ///   Defines if and when loading and storing happens automatically
    /// </summary>
    /// <remarks>
    ///   The value is initialized with <i>DefaultAutoDataStorage</i>, but it
    ///   can be adjusted in the forms constructor. Note that setting it in the
    ///   OnCreate event will have no effect if it was initialised with <i>
    ///   TAutoDataStorage.callOutside</i> or going to be set to that value in
    ///   the event.
    /// </remarks>
    property AutoDataStorage: TAutoDataStorage read GetAutoDataStorage write FAutoDataStorage;
    property DefaultDataStorage: TDataStorage read GetDefaultDataStorage;
    property HandleGlobalVirtualImageLists: Boolean read FHandleGlobalVirtualImageLists write FHandleGlobalVirtualImageLists;
  end;

type
  { This allows to inherit a TForm or TFrame without using visual inheritance keeping the IDE designer happy.
    Just use this unit in the interface part after Vcl.Forms. }
  TForm = TCommonForm;
  TFrame = TCommonFrame;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.VirtualImageList,
  Cmon.Utilities;

constructor TCommonForm.Create(AOwner: TComponent);
begin
  FHandledReferences := TStringList.Create(dupIgnore, True, False);
  inherited Create(AOwner);
end;

destructor TCommonForm.Destroy;
begin
  inherited Destroy;
  FHandledReferences.Free;
end;

class function TCommonForm.NewInstance: TObject;
begin
  Result := inherited;
  TCommonForm(Result).FAutoDataStorage := DefaultAutoDataStorage;
  TCommonForm(Result).FHandleGlobalVirtualImageLists := DefaultHandleGlobalVirtualImageLists;
end;

procedure TCommonForm.AfterConstruction;
begin
  RedirectReferences;
  inherited;
end;

procedure TCommonForm.DoCreate;
begin
  InitDefaults;
  if AutoDataStorage = TAutoDataStorage.callOutside then
    LoadFromStorage;
  inherited;
  if AutoDataStorage = TAutoDataStorage.callInside then
    LoadFromStorage;
end;

procedure TCommonForm.DoDestroy;
begin
  if AutoDataStorage = TAutoDataStorage.callInside then
    SaveToStorage;
  inherited;
  if AutoDataStorage = TAutoDataStorage.callOutside then
    SaveToStorage;
end;

function TCommonForm.GetAutoDataStorage: TAutoDataStorage;
begin
  Result := FAutoDataStorage;
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

procedure TCommonForm.GrabVirtualImageLists(AClass: TDataModuleClass);
var
  cmp: TComponent;
  instance: TDataModule;
begin
  { just creating the instance will resolve all pending references to it }
  instance := AClass.Create(nil);
  try
    var dmPPI := instance.PixelsPerInch;
    if dmPPI = 0 then
      dmPPI := Screen.DefaultPixelsPerInch; { should be 96 }
    if HandledReferences.IndexOf(instance.Name) < 0 then
    begin
      HandledReferences.Add(instance.Name);
      for var I := instance.ComponentCount - 1 downto 0 do
      begin
        cmp := instance.Components[I];
        if cmp is TVirtualImageList then
        begin
          { Moving the component to Self will keep all references intact.
            That is why a clone won't work here. }
          InsertComponent(cmp);
          if CurrentPPI <> dmPPI then begin
            var img := TVirtualImageList(cmp);
            var W := MulDiv(img.Width, CurrentPPI, dmPPI);
            var H := MulDiv(img.Height, CurrentPPI, dmPPI);
            img.SetSize(W, H);
          end;
        end;
      end;
    end;
  finally
    instance.Free;
  end;
end;

procedure TCommonForm.InitDefaults;
begin
  InitDefaults(DefaultDataStorage);
end;

procedure TCommonForm.InitDefaults(AFrame: TCommonFrame);
begin
  InitDefaults(AFrame, DefaultDataStorage);
end;

procedure TCommonForm.InitDefaults(AFrame: TCommonFrame; Storage: TDataStorage);
begin
  AFrame.InitDefaults(Storage);
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

procedure TCommonForm.LoadFromStorage(const AFileName: string);
begin
  LoadFromStorage(TDataStorage.CreateStorageTarget(Self, AFileName));
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
        if (AutoDataStorage <> TAutoDataStorage.none) and not (csDestroying in frame.ComponentState) then
          SaveToStorage(frame);
      end;
    end;
  end;
end;

procedure TCommonForm.NotifyLoaded(Sender: TCommonFrame);
begin
  if AutoDataStorage <> TAutoDataStorage.none then begin
    InitDefaults(Sender);
    LoadFromStorage(Sender);
  end;
end;

procedure TCommonForm.PrepareStorage(Storage: TDataStorage);
begin
  Storage.StorageKey := TDataStorage.GetStorageKeyFromAttribute(Self, GetStorageKey(Storage));
  InternalPrepareStorage(Storage);
end;

procedure TCommonForm.RedirectReferences;
{ static frames call this when their parent is set during loading }
begin
  if not HandleGlobalVirtualImageLists then Exit;

  { first resolve all existing references }
  for var reference in HandledReferences do
    RedirectFixupReferences(nil, reference, Name);
  { now resolve any remaining references by cloning the necessary image lists }
  ResolveReferences;
end;

procedure TCommonForm.ResolveReferences;
var
  lst: TStringList;
begin
  lst := TStringList.Create();
  try
    GetFixupReferenceNames(nil, lst);
    for var reference in lst do begin
      var cls := GetClass('T' + reference);
      if (cls <> nil) and cls.InheritsFrom(TDataModule) then
        GrabVirtualImageLists(TDataModuleClass(cls));
    end;
  finally
    lst.Free;
  end;
end;

procedure TCommonForm.SaveToStorage;
begin
  SaveToStorage(DefaultDataStorage);
end;

procedure TCommonForm.SaveToStorage(const AFileName: string);
begin
  SaveToStorage(TDataStorage.CreateStorageTarget(Self, AFileName));
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
  Storage.StorageKey := TDataStorage.GetStorageKeyFromAttribute(Self, GetStorageKey(Storage));
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

procedure TCommonFrame.SetParent(AParent: TWinControl);
var
  intf: IRedirectReferences;
begin
  inherited;
  if Supports(GetParentForm(Self), IRedirectReferences, intf) then
    intf.RedirectReferences;
end;

function FindGlobalComponent(const Name: string): TComponent;
begin
  Result := nil;
  var idx := Name.IndexOf('.');
  if idx > 0 then begin
    Result := System.Classes.FindGlobalComponent(Name.Remove(idx));
    if Result <> nil then begin
      Result := FindNestedComponent(Result, Name.Substring(idx + 1));
    end;
  end;
end;

initialization
  System.Classes.RegisterFindGlobalComponentProc(FindGlobalComponent);
finalization
  System.Classes.UnregisterFindGlobalComponentProc(FindGlobalComponent);
end.
