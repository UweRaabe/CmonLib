unit Cmon.DataStorage;

interface

uses
  System.Rtti, System.Classes, System.SysUtils, System.Generics.Collections, System.TypInfo,
  Cmon.Messaging, Cmon.DataStorage.Types;

type
  IStorageTarget = Cmon.DataStorage.Types.IStorageTarget;
  TCustomStorageAttribute = Cmon.DataStorage.Types.TCustomStorageAttribute;
  TCustomDefaultAttribute = Cmon.DataStorage.Types.TCustomDefaultAttribute;
  TStorageTargetDescriptor = Cmon.DataStorage.Types.TStorageTargetDescriptor;
  TStorageTargets = TArray<TStorageTargetDescriptor>;

type
  DefaultAttribute = class(TCustomDefaultAttribute);
  StorageAttribute = class(TCustomStorageAttribute);
  StorageKeyAttribute = class(TCustomStorageKeyAttribute);
  NoStorageAttribute = class(TCustomAttribute);
  AutoStorageFieldsAttribute = class(TCustomVisibilitiesAttribute);
  AutoStoragePropertiesAttribute = class(TCustomVisibilitiesAttribute);
  SkipFieldNameFAttribute = class(TCustomVisibilitiesAttribute);
  SkipStorageAttribute = class(TCustomDefaultAttribute);
  SkipNullDateAttribute = class(SkipStorageAttribute)
  public
    constructor Create;
  end;

type
{$SCOPEDENUMS ON}
  TStorageAction = (init, load, save);

  /// <summary>
  ///   Declares the possible values to defines if and when loading and storing
  ///   data is executed.
  /// </summary>
  TAutoDataStorage = (
    /// <summary>
    ///   No automatic loading or storing.
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

  TDataStorage = class
  strict private
  const
    cKeySeparator: string = '\';
  class var
    FAutoRegisterHandler: Boolean;
    FDefaultInstance: TDataStorage;
    FStorageTargetFactory: TStorageTargetFactory;
    class destructor Destruct;
    class function GetDefaultInstance: TDataStorage; static;
  private type
    TStorageKeyStack = TStack<string>;
  private
    FDefaultStorageKey: string;
    FStorageKey: string;
    FStorageKeyStack: TStorageKeyStack;
    FStorageTarget: IStorageTarget;
    FStorageTargetExt: IStorageTargetExt;
    function GetFormatSettings: TFormatSettings;
    class function GetStorageTargets: TStorageTargets; static;
    procedure SetFormatSettings(const Value: TFormatSettings);
    procedure SetStorageTarget(const Value: IStorageTarget);
  protected
    class procedure ExecuteInit(AInstance, ATypeInfo: Pointer; AAttribute: TCustomAttributeClass);
    procedure ExecuteStorageAction(Action: TStorageAction; AInstance, ATypeInfo: Pointer; const AStorageKey: string; AAttribute: TCustomAttributeClass);
    function RetrieveStorageKey(Instance: TObject): string; overload;
    function RetrieveStorageKey<T>: string; overload;
  public
    constructor Create; overload;
    constructor Create(const AFileName: string; AReadOnly: Boolean = False); overload;
    destructor Destroy; override;
    { StorageTargets }
    function CreateStorageTarget(const AFileName: string = ''): IStorageTarget; overload;
    class function CreateStorageTarget(Sender: TObject; const AFileName: string = ''): IStorageTarget; overload;
    class function IsSupportedTargetExtension(const AExtension: string): Boolean;
    class procedure ListStorageTargets(Target: TStorageTargetDescriptorList);
    class function MakeStorageTargetFileFilter: string;
    { InitDefaults }
    class procedure InitDefaults(Instance: TObject); overload; static;
    class procedure InitDefaults<T: record>(var Instance: T); overload; static;
    class procedure InitDefaults<T: record; A: TCustomDefaultAttribute>(var Instance: T); overload; static;
    class procedure InitDefaults<A: TCustomDefaultAttribute>(Instance: TObject); overload; static;
    { LoadFromStorage }
    procedure LoadFromStorage(Instance: TObject); overload;
    procedure LoadFromStorage<A: TCustomStorageAttribute>(Instance: TObject); overload;
    procedure LoadFromStorage<T: record>(var Instance: T); overload;
    procedure LoadFromStorage<T: record; A: TCustomStorageAttribute>(var Instance: T); overload;
    { SaveToStorage }
    procedure SaveToStorage(Instance: TObject); overload;
    procedure SaveToStorage<A: TCustomStorageAttribute>(Instance: TObject); overload;
    procedure SaveToStorage<T: record>(var Instance: T); overload;
    procedure SaveToStorage<T: record; A: TCustomStorageAttribute>(var Instance: T); overload;
    { StorageKey handling }
    class function GetStorageKeyFromAttribute(AInstance: TObject; const ADefault: string = ''): string; overload;
    class function GetStorageKeyFromAttribute<T>(const ADefault: string = ''): string; overload;
    class function SplitStorageKey(const AStorageKey: string): TArray<string>;
    function MakeStorageSubKey(const ASubKey: string): string;
    procedure PopStorageKey;
    procedure PushStorageKey; overload;
    procedure PushStorageKey(const NewKey: string); overload;
    { Reading, Writing, Key and Section handling }
    procedure EraseStorageKey;
    procedure DeleteKey(const Ident: string);
    procedure ReadKey(Target: TStrings);
    function ReadBoolean(const Ident: string; const Default: Boolean): Boolean;
    function ReadDateTime(const Ident: string; const Default: TDateTime): TDateTime;
    function ReadFloat(const Ident: string; const Default: Double): Double;
    procedure ReadChild(Instance: TObject; const Ident: string = ''); overload;
    procedure ReadChild<A: TCustomStorageAttribute>(Instance: TObject; const Ident: string = ''); overload;
    function ReadInteger(const Ident: string; const Default: Integer): Integer;
    function ReadString(const Ident, Default: string): string;
    procedure ReadStrings(const Ident: string; Target: TStrings);
    function ReadValue(const Ident: string; const Default: TValue): TValue;
    function ValueExists(const Ident: string): Boolean;
    procedure WriteBoolean(const Ident: string; const Value: Boolean);
    procedure WriteDateTime(const Ident: string; const Value: TDateTime);
    procedure WriteFloat(const Ident: string; const Value: Double);
    procedure WriteChild(Instance: TObject; const Ident: string = ''); overload;
    procedure WriteChild<A: TCustomStorageAttribute>(Instance: TObject; const Ident: string = ''); overload;
    procedure WriteInteger(const Ident: string; const Value: Integer);
    procedure WriteString(const Ident: string; const Value: string);
    procedure WriteStrings(const Ident: string; Source: TStrings);
    procedure WriteValue(const Ident: string; const Value: TValue);
    { Properties }
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
    class property DefaultInstance: TDataStorage read GetDefaultInstance;
    property DefaultStorageKey: string read FDefaultStorageKey write FDefaultStorageKey;
    property FormatSettings: TFormatSettings read GetFormatSettings write SetFormatSettings;
    class property StorageTargetFactory: TStorageTargetFactory read FStorageTargetFactory write FStorageTargetFactory;
    class property StorageTargets: TStorageTargets read GetStorageTargets;
    property StorageKey: string read FStorageKey write FStorageKey;
    property StorageTarget: IStorageTarget read FStorageTarget write SetStorageTarget;
  end;

type
  IStoredData = interface
  ['{744EAFDC-EFB9-491F-9280-B803B39D1472}']
    procedure InternalInitDefaults(DataStorage: TDataStorage);
    procedure InternalLoadFromStorage(DataStorage: TDataStorage);
    procedure InternalPrepareStorage(DataStorage: TDataStorage);
    procedure InternalSaveToStorage(DataStorage: TDataStorage);
  end;

  IStorageKey = interface
  ['{AF9DF9C3-C201-4902-B78A-AE55621BC44B}']
    function GetStorageKey(DataStorage: TDataStorage): string;
  end;

type
  TCustomStoredClass = class
  private
    [NoStorage]
    FDataStorage: TDataStorage;
  strict protected
    function GetDataStorage: TDataStorage; virtual;
    function GetTarget: TObject; virtual;
    property Target: TObject read GetTarget;
  protected
    function InternalGetStorageKey(AStorage: TDataStorage): string; virtual;
    procedure InternalInitDefaults(AStorage: TDataStorage); virtual;
    procedure InternalLoadFromStorage(AStorage: TDataStorage); virtual;
    procedure InternalSaveToStorage(AStorage: TDataStorage); virtual;
    procedure InternalPrepareStorage(AStorage: TDataStorage); virtual;
    procedure PrepareStorage;
    [NoStorage]
    property DataStorage: TDataStorage read GetDataStorage write FDataStorage;
  public
    procedure Finalize; virtual;
    procedure InitDefaults;
    procedure Initialize; virtual;
    procedure LoadFromStorage; overload;
    procedure LoadFromStorage(const AFileName: string); overload;
    procedure SaveToStorage; overload;
    procedure SaveToStorage(const AFileName: string); overload;
  end;

type
  TStoredWrapper = class(TCustomStoredClass)
  private
    FTarget: TObject;
  strict protected
    function GetTarget: TObject; override;
  public
    constructor Create(ATarget: TObject);
    property Target: TObject read GetTarget;
  end;

type
  TStoredClass = class(TCustomStoredClass);

function GetStorageKeyFromAttribute(AInstance: TObject; const ADefault: string = ''): string; deprecated 'Use TDataStorage.GetStorageKeyFromAttribute';

implementation

type
  TDefaultStorageTarget = class(TAbstractStorageTarget)
  private
    FStorageTarget: IStorageTarget;
  strict protected
    procedure DeleteKey(const Key, Ident: string); override;
    procedure EraseStorageKey(const Key: string); override;
  protected
    procedure ReadKey(const Key: string; Target: TStrings); override;
    function ReadString(const Key: string; const Ident: string; const Default: string): string; override;
    procedure WriteString(const Key: string; const Ident: string; const Value: string); override;
    function ValueExists(const Key, Ident: string): Boolean; override;
  public
    constructor Create(AStorageTarget: IStorageTarget = nil);
  end;

constructor TDefaultStorageTarget.Create(AStorageTarget: IStorageTarget = nil);
begin
  inherited Create;
  FStorageTarget := AStorageTarget;
end;

procedure TDefaultStorageTarget.DeleteKey(const Key, Ident: string);
begin
  if FStorageTarget <> nil then
    FStorageTarget.DeleteKey(Key, Ident);
end;

procedure TDefaultStorageTarget.EraseStorageKey(const Key: string);
begin
  if FStorageTarget <> nil then
    FStorageTarget.EraseStorageKey(Key);
end;

procedure TDefaultStorageTarget.ReadKey(const Key: string; Target: TStrings);
begin
  if FStorageTarget <> nil then
    FStorageTarget.ReadKey(Key, Target);
end;

function TDefaultStorageTarget.ReadString(const Key, Ident, Default: string): string;
begin
  Result := Default;
  if FStorageTarget <> nil then
    Result := FStorageTarget.ReadString(Key, Ident, Default);
end;

function TDefaultStorageTarget.ValueExists(const Key, Ident: string): Boolean;
begin
  Result := False;
  if FStorageTarget <> nil then
    Result := FStorageTarget.ValueExists(Key, Ident);
end;

procedure TDefaultStorageTarget.WriteString(const Key, Ident, Value: string);
begin
  if FStorageTarget <> nil then
    FStorageTarget.WriteString(Key, Ident, Value);
end;

function GetStorageKeyFromAttribute(AInstance: TObject; const ADefault: string = ''): string;
begin
  Result := TDataStorage.GetStorageKeyFromAttribute(AInstance, ADefault);
end;

type
  TDataStorageHelper = class helper for TDataStorage
  protected
    procedure ReadInstance(const Ident: string; Instance: TObject; AAttribute: TCustomAttributeClass);
    procedure WriteInstance(const Ident: string; Instance: TObject; AAttribute: TCustomAttributeClass);
  end;

  TCustomWorker = class
  private
    FContext: TRttiContext;
    FInstanceInfo: Pointer;
  strict protected
    Instance: Pointer;
    InstanceType: TRttiType;
    SkipFAttribute: SkipFieldNameFAttribute;
    Storage: TDataStorage;
    AttributeClass: TCustomAttributeClass;
  protected
    function GetStoredName(Attribute: TCustomStorageAttribute; const Default: string): string; overload;
    function GetStoredName(AField: TRttiField): string; overload;
    function GetStoredName(AProp: TRttiProperty): string; overload;
    function SkipF(AVisibility: TMemberVisibility): Boolean;
  public
    constructor Create(ADataStorage: TDataStorage; AInstance, AInstanceType: Pointer; AAttributeClass: TCustomAttributeClass);
  end;

  TMemberHandlerClass = class of TMemberHandler;
  TMemberHandler = class(TCustomWorker)
  protected
    procedure HandleMember(AField: TRttiField); overload; virtual; abstract;
    procedure HandleMember(AProp: TRttiProperty); overload; virtual; abstract;
  public
  end;

  TInitHandler = class(TMemberHandler)
  protected
    procedure HandleMember(AField: TRttiField); overload; override;
    procedure HandleMember(AProp: TRttiProperty); overload; override;
  end;

  TLoadHandler = class(TMemberHandler)
  protected
    procedure HandleMember(AField: TRttiField); overload; override;
    procedure HandleMember(AProp: TRttiProperty); overload; override;
  end;

  TSaveHandler = class(TMemberHandler)
  strict protected
  protected
    procedure HandleMember(AField: TRttiField); overload; override;
    procedure HandleMember(AProp: TRttiProperty); overload; override;
  end;

  TMemberIterator = class(TCustomWorker)
  private
    AutoFields: TCustomVisibilitiesAttribute;
    AutoProps: TCustomVisibilitiesAttribute;
  protected
    FHandler: TMemberHandler;
    function CheckMember(AField: TRttiField): Boolean; overload;
    function CheckMember(AProp: TRttiProperty): Boolean; overload;
    procedure Execute(Action: TStorageAction); overload;
    procedure IterateFields;
    procedure IterateProperties;
    property Handler: TMemberHandler read FHandler;
  public
    class procedure Execute(Action: TStorageAction; AStorage: TDataStorage; AInstance, AInstanceType: Pointer; AAttribute: TCustomAttributeClass); overload;
  end;

constructor TDataStorage.Create;
begin
  inherited Create;
  FStorageKeyStack := TStorageKeyStack.Create();
  SetStorageTarget(nil); // initialize with default target
end;

constructor TDataStorage.Create(const AFileName: string; AReadOnly: Boolean = False);
begin
  Create;
  StorageTarget := CreateStorageTarget(AFileName);
  FStorageTargetExt.ReadOnly := AReadOnly;
end;

destructor TDataStorage.Destroy;
begin
  FStorageKeyStack.Free;
  inherited;
end;

class destructor TDataStorage.Destruct;
begin
  FDefaultInstance.Free;
end;

class function TDataStorage.CreateStorageTarget(Sender: TObject; const AFileName: string = ''): IStorageTarget;
begin
  Result := TStorageTargetMessage.Execute(Sender, AFileName);
end;

function TDataStorage.CreateStorageTarget(const AFileName: string = ''): IStorageTarget;
begin
  Result := CreateStorageTarget(Self, AFileName);
end;

procedure TDataStorage.EraseStorageKey;
begin
  FStorageTarget.EraseStorageKey(StorageKey);
end;

procedure TDataStorage.DeleteKey(const Ident: string);
begin
  FStorageTarget.DeleteKey(StorageKey, Ident);
end;

class procedure TDataStorage.ExecuteInit(AInstance, ATypeInfo: Pointer; AAttribute: TCustomAttributeClass);
begin
  TMemberIterator.Execute(TStorageAction.init, nil, AInstance, ATypeInfo, AAttribute);
end;

procedure TDataStorage.ExecuteStorageAction(Action: TStorageAction; AInstance, ATypeInfo: Pointer; const AStorageKey: string; AAttribute:
    TCustomAttributeClass);
begin
  PushStorageKey(AStorageKey);
  try
    TMemberIterator.Execute(Action, Self, AInstance, ATypeInfo, AAttribute);
  finally
    PopStorageKey;
  end;
end;

class function TDataStorage.GetDefaultInstance: TDataStorage;
begin
  if FDefaultInstance = nil then begin
    FDefaultInstance := TDataStorage.Create;
    FDefaultInstance.StorageTarget := FDefaultInstance.CreateStorageTarget();
  end;
  Result := FDefaultInstance;
end;

function TDataStorage.GetFormatSettings: TFormatSettings;
begin
  Result := FStorageTargetExt.FormatSettings;
end;

class function TDataStorage.GetStorageKeyFromAttribute(AInstance: TObject; const ADefault: string = ''): string;
begin
  Result := ADefault;
  var context := TRTTIContext.Create;
  var myType := context.GetType(AInstance.ClassType);
  if myType <> nil then begin
    var attr := myType.GetAttribute<StorageKeyAttribute>;
    if attr <> nil then
      Result := attr.Key;
  end;
end;

class function TDataStorage.GetStorageKeyFromAttribute<T>(const ADefault: string = ''): string;
begin
  Result := ADefault;
  var context := TRTTIContext.Create;
  var myType := context.GetType(TypeInfo(T));
  if myType <> nil then begin
    var attr := myType.GetAttribute<StorageKeyAttribute>;
    if attr <> nil then
      Result := attr.Key;
  end;
end;

class procedure TDataStorage.InitDefaults(Instance: TObject);
begin
  InitDefaults<DefaultAttribute>(Instance);
end;

class procedure TDataStorage.InitDefaults<T>(var Instance: T);
begin
  InitDefaults<T, DefaultAttribute>(Instance);
end;

class procedure TDataStorage.InitDefaults<T, A>(var Instance: T);
begin
  ExecuteInit(@Instance, TypeInfo(T), A);
end;

class procedure TDataStorage.InitDefaults<A>(Instance: TObject);
begin
  ExecuteInit(Instance, Instance.ClassInfo, A);
end;

class function TDataStorage.GetStorageTargets: TStorageTargets;
begin
  var targets := TStorageTargetDescriptorList.Create;
  try
    ListStorageTargets(targets);
    Result := targets.ToArray;
  finally
    targets.Free;
  end;
end;

class function TDataStorage.IsSupportedTargetExtension(const AExtension: string): Boolean;
var
  targets: TStorageTargetDescriptorList;
begin
  targets := TStorageTargetDescriptorList.Create;
  try
    ListStorageTargets(targets);
    for var target in targets do
      if SameText(AExtension, target.FileExtension) then
        Exit(True);
    Result := False;
  finally
    targets.Free;
  end;
end;

class procedure TDataStorage.ListStorageTargets(Target: TStorageTargetDescriptorList);
begin
  TStorageTargetListMessage.SendMessage(nil, Target);
end;

procedure TDataStorage.LoadFromStorage(Instance: TObject);
begin
  LoadFromStorage<StorageAttribute>(Instance);
end;

procedure TDataStorage.LoadFromStorage<A>(Instance: TObject);
begin
  if Instance = nil then Exit;

  ExecuteStorageAction(TStorageAction.load, Instance, Instance.ClassInfo, RetrieveStorageKey(Instance), A);
end;

procedure TDataStorage.LoadFromStorage<T>(var Instance: T);
begin
  LoadFromStorage<T, StorageAttribute>(Instance);
end;

procedure TDataStorage.LoadFromStorage<T, A>(var Instance: T);
begin
  ExecuteStorageAction(TStorageAction.load, @Instance, TypeInfo(T), RetrieveStorageKey<T>, A);
end;

function TDataStorage.MakeStorageSubKey(const ASubKey: string): string;
begin
  if StorageKey.IsEmpty or StorageKey.Equals(DefaultStorageKey) then
    Result := ASubKey
  else
    Result := StorageKey + cKeySeparator + ASubKey;
end;

class function TDataStorage.MakeStorageTargetFileFilter: string;
var
  lst: TStringList;
  targets: TStorageTargetDescriptorList;
begin
  targets := TStorageTargetDescriptorList.Create;
  try
    ListStorageTargets(targets);
    lst := TStringList.Create;
    try
      for var target in Targets do begin
        lst.Add(Format('%s (*%s)', [target.Description, target.FileExtension]));
        lst.Add(Format('*%s', [target.FileExtension]));
      end;
      Result := string.Join('|', lst.ToStringArray);
    finally
      lst.Free;
    end;
  finally
    targets.Free;
  end;
end;

procedure TDataStorage.PopStorageKey;
begin
  StorageKey := FStorageKeyStack.Pop;
end;

procedure TDataStorage.PushStorageKey;
begin
  FStorageKeyStack.Push(StorageKey);
end;

procedure TDataStorage.PushStorageKey(const NewKey: string);
begin
  PushStorageKey;
  StorageKey := NewKey;
end;

function TDataStorage.ReadBoolean(const Ident: string; const Default: Boolean): Boolean;
begin
  Result := FStorageTargetExt.ReadBoolean(StorageKey, Ident, Default);
end;

function TDataStorage.ReadDateTime(const Ident: string; const Default: TDateTime): TDateTime;
begin
  Result := FStorageTargetExt.ReadDateTime(StorageKey, Ident, Default);
end;

function TDataStorage.ReadFloat(const Ident: string; const Default: Double): Double;
begin
  Result := FStorageTargetExt.ReadFloat(StorageKey, Ident, Default);
end;

procedure TDataStorage.ReadChild(Instance: TObject; const Ident: string = '');
begin
  ReadChild<StorageAttribute>(Instance, Ident);
end;

procedure TDataStorage.ReadChild<A>(Instance: TObject; const Ident: string = '');
begin
  var key := Ident;
  if key.IsEmpty then
    key := GetStorageKeyFromAttribute(Instance);
  PushStorageKey(MakeStorageSubKey(key));
  try
    LoadFromStorage<A>(Instance);
  finally
    PopStorageKey;
  end;
end;

function TDataStorage.ReadInteger(const Ident: string; const Default: Integer): Integer;
begin
  Result := FStorageTargetExt.ReadInteger(StorageKey, Ident, Default);
end;

procedure TDataStorage.ReadKey(Target: TStrings);
begin
  FStorageTarget.ReadKey(StorageKey, Target);
end;

function TDataStorage.ReadString(const Ident, Default: string): string;
begin
  Result := FStorageTarget.ReadString(StorageKey, Ident, Default);
end;

procedure TDataStorage.ReadStrings(const Ident: string; Target: TStrings);
begin
  FStorageTargetExt.ReadStrings(StorageKey, Ident, Target);
end;

function TDataStorage.ReadValue(const Ident: string; const Default: TValue): TValue;
begin
  Result := FStorageTargetExt.ReadValue(StorageKey, Ident, Default);
end;

function TDataStorage.RetrieveStorageKey(Instance: TObject): string;
begin
  Result := StorageKey;
  if Result.IsEmpty then
    Result := GetStorageKeyFromAttribute(Instance, Instance.ClassName);
end;

function TDataStorage.RetrieveStorageKey<T>: string;
begin
  Result := StorageKey;
  if Result.IsEmpty then
    Result := GetStorageKeyFromAttribute<T>(string(PTypeInfo(TypeInfo(T)).Name));
end;

procedure TDataStorage.SaveToStorage(Instance: TObject);
begin
  SaveToStorage<StorageAttribute>(Instance);
end;

procedure TDataStorage.SaveToStorage<A>(Instance: TObject);
begin
  ExecuteStorageAction(TStorageAction.save, Instance, Instance.ClassInfo, RetrieveStorageKey(Instance), A);
end;

procedure TDataStorage.SaveToStorage<T>(var Instance: T);
begin
  SaveToStorage<T, StorageAttribute>(Instance);
end;

procedure TDataStorage.SaveToStorage<T, A>(var Instance: T);
begin
  ExecuteStorageAction(TStorageAction.save, @Instance, TypeInfo(T), RetrieveStorageKey<T>, A);
end;

procedure TDataStorage.SetFormatSettings(const Value: TFormatSettings);
begin
  FStorageTargetExt.FormatSettings := Value;
end;

procedure TDataStorage.SetStorageTarget(const Value: IStorageTarget);
begin
  FStorageTargetExt := nil;
  FStorageTarget := Value;
  if FStorageTarget = nil then
    FStorageTarget := TDefaultStorageTarget.Create;
  if not Supports(FStorageTarget, IStorageTargetExt, FStorageTargetExt) then
    FStorageTargetExt := TDefaultStorageTarget.Create(FStorageTarget);
end;

class function TDataStorage.SplitStorageKey(const AStorageKey: string): TArray<string>;
begin
  Result := AStorageKey.Split([cKeySeparator]);
end;

function TDataStorage.ValueExists(const Ident: string): Boolean;
begin
  Result := FStorageTarget.ValueExists(StorageKey, Ident);
end;

procedure TDataStorage.WriteBoolean(const Ident: string; const Value: Boolean);
begin
  FStorageTargetExt.WriteBoolean(StorageKey, Ident, Value);
end;

procedure TDataStorage.WriteDateTime(const Ident: string; const Value: TDateTime);
begin
  FStorageTargetExt.WriteDateTime(StorageKey, Ident, Value);
end;

procedure TDataStorage.WriteFloat(const Ident: string; const Value: Double);
begin
  FStorageTargetExt.WriteFloat(StorageKey, Ident, Value);
end;

procedure TDataStorage.WriteChild(Instance: TObject; const Ident: string = '');
begin
  WriteChild<StorageAttribute>(Instance, Ident);
end;

procedure TDataStorage.WriteChild<A>(Instance: TObject; const Ident: string = '');
begin
  var key := Ident;
  if key.IsEmpty then
    key := GetStorageKeyFromAttribute(Instance);
  PushStorageKey(MakeStorageSubKey(key));
  try
    SaveToStorage<A>(Instance);
  finally
    PopStorageKey;
  end;
end;

procedure TDataStorage.WriteInteger(const Ident: string; const Value: Integer);
begin
  FStorageTargetExt.WriteInteger(StorageKey, Ident, Value);
end;

procedure TDataStorage.WriteString(const Ident: string; const Value: string);
begin
  FStorageTarget.WriteString(StorageKey, Ident, Value);
end;

procedure TDataStorage.WriteStrings(const Ident: string; Source: TStrings);
begin
  FStorageTargetExt.WriteStrings(StorageKey, Ident, Source);
end;

procedure TDataStorage.WriteValue(const Ident: string; const Value: TValue);
begin
  FStorageTargetExt.WriteValue(StorageKey, Ident, Value);
end;

procedure TCustomStoredClass.Finalize;
begin
  SaveToStorage;
end;

function TCustomStoredClass.GetDataStorage: TDataStorage;
begin
  Result := FDataStorage;
  if Result = nil then
    Result := TDataStorage.DefaultInstance;
end;

function TCustomStoredClass.GetTarget: TObject;
begin
  Result := Self;
end;

procedure TCustomStoredClass.InitDefaults;
begin
  InternalInitDefaults(DataStorage);
end;

procedure TCustomStoredClass.Initialize;
begin
  InitDefaults;
  LoadFromStorage;
end;

function TCustomStoredClass.InternalGetStorageKey(AStorage: TDataStorage): string;
var
  intf: IStorageKey;
begin
  { do we have a StorageKey attribute? }
  Result := TDataStorage.GetStorageKeyFromAttribute(Target);
  if Result.IsEmpty then begin
    { no, then we look for IStorageKey support }
    if Supports(Target, IStorageKey, intf) then
      Result := intf.GetStorageKey(AStorage);
  end;
end;

procedure TCustomStoredClass.InternalInitDefaults(AStorage: TDataStorage);
var
  intf: IStoredData;
begin
  AStorage.InitDefaults(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalInitDefaults(AStorage);
end;

procedure TCustomStoredClass.InternalLoadFromStorage(AStorage: TDataStorage);
var
  intf: IStoredData;
begin
  AStorage.LoadFromStorage(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalLoadFromStorage(AStorage);
end;

procedure TCustomStoredClass.InternalPrepareStorage(AStorage: TDataStorage);
var
  intf: IStoredData;
begin
  if Supports(Target, IStoredData, intf) then
    intf.InternalPrepareStorage(AStorage);
end;

procedure TCustomStoredClass.InternalSaveToStorage(AStorage: TDataStorage);
var
  intf: IStoredData;
begin
  AStorage.SaveToStorage(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalSaveToStorage(AStorage);
end;

procedure TCustomStoredClass.LoadFromStorage;
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage;
    InternalLoadFromStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

procedure TCustomStoredClass.LoadFromStorage(const AFileName: string);
begin
  var storage := TDataStorage.Create(AFileName, True); // ReadOnly
  try
    DataStorage := storage;
    LoadFromStorage;
  finally
    DataStorage := nil;
    storage.Free;
  end;
end;

procedure TCustomStoredClass.PrepareStorage;
var
  key: string;
begin
  key := InternalGetStorageKey(DataStorage);
  if not key.IsEmpty then
    DataStorage.StorageKey := key;
  InternalPrepareStorage(DataStorage);
end;

procedure TCustomStoredClass.SaveToStorage;
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage;
    InternalSaveToStorage(DataStorage);
  finally
    DataStorage.PopStorageKey;
  end;
end;

procedure TCustomStoredClass.SaveToStorage(const AFileName: string);
begin
  var storage := TDataStorage.Create(AFileName);
  try
    DataStorage := storage;
    SaveToStorage;
  finally
    DataStorage := nil;
    storage.Free;
  end;
end;

constructor TStoredWrapper.Create(ATarget: TObject);
begin
  inherited Create;
  FTarget := ATarget;
end;

function TStoredWrapper.GetTarget: TObject;
begin
  Result := FTarget;
end;

function TMemberIterator.CheckMember(AField: TRttiField): Boolean;
begin
  if AttributeClass.InheritsFrom(TCustomDefaultAttribute) or (AutoFields = nil) then
    Result := AField.HasAttribute(AttributeClass)
  else
    Result := AutoFields.CheckVisibility(AField.Visibility) and not AField.HasAttribute<NoStorageAttribute>;
end;

function TMemberIterator.CheckMember(AProp: TRttiProperty): Boolean;
begin
  if AttributeClass.InheritsFrom(TCustomDefaultAttribute) or (AutoProps = nil) then
    Result := AProp.HasAttribute(AttributeClass)
  else
    Result := AutoProps.CheckVisibility(AProp.Visibility) and not AProp.HasAttribute<NoStorageAttribute>;
end;

procedure TMemberIterator.Execute(Action: TStorageAction);
var
  cls: TMemberHandlerClass;
begin
  case Action of
    TStorageAction.init: cls := TInitHandler;
    TStorageAction.load: cls := TLoadHandler;
    TStorageAction.save: cls := TSaveHandler;
  else
    Exit;
  end;
  FHandler := cls.Create(Storage, Instance, FInstanceInfo, AttributeClass);
  try
    AutoFields := InstanceType.GetAttribute<AutoStorageFieldsAttribute>;
    AutoProps := InstanceType.GetAttribute<AutoStoragePropertiesAttribute>;
    IterateFields;
    IterateProperties;
  finally
    FHandler.Free;
  end;
end;

procedure TMemberIterator.IterateFields;
begin
  for var field in InstanceType.GetFields do
    if CheckMember(field) then
      Handler.HandleMember(field);
end;

procedure TMemberIterator.IterateProperties;
begin
  for var prop in InstanceType.GetProperties do
    if CheckMember(prop) then
      Handler.HandleMember(prop);
end;

class procedure TMemberIterator.Execute(Action: TStorageAction; AStorage: TDataStorage; AInstance, AInstanceType: Pointer; AAttribute: TCustomAttributeClass);
begin
  var instance := Self.Create(AStorage, AInstance, AInstanceType, AAttribute);
  try
    instance.Execute(Action);
  finally
    instance.Free;
  end;
end;

procedure TInitHandler.HandleMember(AField: TRttiField);
begin
  var attr := AField.GetAttribute(AttributeClass) as TCustomDefaultAttribute;
  if (attr = nil) or attr.Value.IsEmpty then Exit;
  AField.SetValue(Instance, attr.value);
end;

procedure TInitHandler.HandleMember(AProp: TRttiProperty);
begin
  var attr := AProp.GetAttribute(AttributeClass) as TCustomDefaultAttribute;
  if (attr = nil) or attr.Value.IsEmpty then Exit;
  AProp.SetValue(Instance, attr.value);
end;

procedure TLoadHandler.HandleMember(AField: TRttiField);
begin
  var storedName := GetStoredName(AField);
  if AField.FieldType.IsInstance then begin
    Storage.ReadInstance(storedName, AField.GetValue(Instance).AsObject, AttributeClass);
  end
  else begin
    var value := Storage.ReadValue(storedName, AField.GetValue(Instance));
    if not value.IsEmpty then
      AField.SetValue(Instance, value);
  end;
end;

procedure TLoadHandler.HandleMember(AProp: TRttiProperty);
begin
  var storedName := GetStoredName(AProp);
  if AProp.PropertyType.IsInstance then begin
    Storage.ReadInstance(storedName, AProp.GetValue(Instance).AsObject, AttributeClass);
  end
  else begin
    var value := Storage.ReadValue(storedName, AProp.GetValue(Instance));
    if not value.IsEmpty then
      AProp.SetValue(Instance, value);
  end;
end;

procedure TSaveHandler.HandleMember(AField: TRttiField);

  function SameValue(const A, B: TValue): Boolean;
  begin
    Result := False;
    if A.DataSize = B.DataSize then begin
      Result := CompareMem(A.GetReferenceToRawData, B.GetReferenceToRawData, A.DataSize);
    end;
  end;

begin
  var storedName := GetStoredName(AField);
  if AField.FieldType.IsInstance then begin
    Storage.WriteInstance(storedName, AField.GetValue(Instance).AsObject, AttributeClass);
  end
  else begin
    var value := AField.GetValue(Instance);
    var attr := AField.GetAttribute<SkipStorageAttribute>;
    if (attr = nil) or not SameValue(attr.Value, value) then
      Storage.WriteValue(storedName, value);
  end;
end;

procedure TSaveHandler.HandleMember(AProp: TRttiProperty);
begin
  var storedName := GetStoredName(AProp);
  if AProp.PropertyType.IsInstance then
    Storage.WriteInstance(storedName, AProp.GetValue(Instance).AsObject, AttributeClass)
  else
    Storage.WriteValue(storedName, AProp.GetValue(Instance));
end;

constructor TCustomWorker.Create(ADataStorage: TDataStorage; AInstance, AInstanceType: Pointer; AAttributeClass: TCustomAttributeClass);
begin
  inherited Create;
  Storage := ADataStorage;
  Instance := AInstance;
  FInstanceInfo := AInstanceType;
  AttributeClass := AAttributeClass;
  FContext := TRttiContext.Create;
  InstanceType := FContext.GetType(AInstanceType);
  SkipFAttribute := InstanceType.GetAttribute<SkipFieldNameFAttribute>;
end;

function TCustomWorker.GetStoredName(Attribute: TCustomStorageAttribute; const Default: string): string;
begin
  Result := Default;
  if (Attribute <> nil) and not Attribute.Name.IsEmpty then
    Result := Attribute.Name;
end;

function TCustomWorker.GetStoredName(AField: TRttiField): string;
begin
  var S := AField.Name;
  if SkipF(AField.Visibility) then
    S := S.Substring(1);
  Result := GetStoredName(AField.GetAttribute(AttributeClass) as TCustomStorageAttribute, S);
end;

function TCustomWorker.GetStoredName(AProp: TRttiProperty): string;
begin
  Result := GetStoredName(AProp.GetAttribute(AttributeClass) as TCustomStorageAttribute, AProp.Name);
end;

function TCustomWorker.SkipF(AVisibility: TMemberVisibility): Boolean;
begin
  Result := (SkipFAttribute = nil) or SkipFAttribute.CheckVisibility(AVisibility);
end;

procedure TDataStorageHelper.ReadInstance(const Ident: string; Instance: TObject; AAttribute: TCustomAttributeClass);
var
  dataIntf: IStoredData;
  keyIntf: IStorageKey;
  stored: TCustomStoredClass;
begin
  Supports(Instance, IStoredData, dataIntf);
  Supports(Instance, IStorageKey, keyIntf);
  stored := nil;
  if Instance is TCustomStoredClass then
    stored := TCustomStoredClass(Instance);
  var key := Ident;
  if keyIntf <> nil then
    key := keyIntf.GetStorageKey(Self);
  PushStorageKey(MakeStorageSubKey(key));
  try
    if Supports(Instance, IStoredData, dataIntf) then
      dataIntf.InternalPrepareStorage(Self);
    if stored <> nil then
      stored.InternalPrepareStorage(Self);
    ExecuteStorageAction(TStorageAction.load, Instance, Instance.ClassInfo, StorageKey, AAttribute);
    if stored <> nil then
      stored.InternalLoadFromStorage(Self);
    if dataIntf <> nil then
      dataIntf.InternalLoadFromStorage(Self);
  finally
    PopStorageKey;
  end;
end;

procedure TDataStorageHelper.WriteInstance(const Ident: string; Instance: TObject; AAttribute: TCustomAttributeClass);
var
  dataIntf: IStoredData;
  keyIntf: IStorageKey;
  stored: TCustomStoredClass;
begin
  Supports(Instance, IStoredData, dataIntf);
  Supports(Instance, IStorageKey, keyIntf);
  stored := nil;
  if Instance is TCustomStoredClass then
    stored := TCustomStoredClass(Instance);
  var key := Ident;
  if keyIntf <> nil then
    key := keyIntf.GetStorageKey(Self);
  PushStorageKey(MakeStorageSubKey(key));
  try
    if dataIntf <> nil then
      dataIntf.InternalPrepareStorage(Self);
    if stored <> nil then
      stored.InternalPrepareStorage(Self);
    ExecuteStorageAction(TStorageAction.save, Instance, Instance.ClassInfo, StorageKey, AAttribute);
    if stored <> nil then
      stored.InternalSaveToStorage(Self);
    if dataIntf <> nil then
      dataIntf.InternalSaveToStorage(Self);
  finally
    PopStorageKey;
  end;
end;

constructor SkipNullDateAttribute.Create;
begin
  inherited Create(TDateTime(0));
end;

initialization
  TDataStorage.AutoRegisterHandler := True;
end.
