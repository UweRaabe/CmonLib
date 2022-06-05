unit Cmon.DataStorage;

interface

uses
  System.Rtti, System.Classes, System.SysUtils, System.Generics.Collections,
  Cmon.Messaging;

type
  { DefaultAttribute from System.Classes uses a Variant internally and casts the type Boolean to Integer. Thus the original
    type information gets lost, while we do rely on it here. Therefore we declare our own DefaultAttribute based on TValue
    and provide constructors for all types we need. Other types can be added in special derived classes if necessary.
  }
  DefaultAttribute = class(TCustomAttribute)
  private
    FValue: TValue;
  public
    constructor Create(AValue: Boolean); overload;
    constructor Create(AValue: Integer); overload;
    constructor Create(const AValue: string); overload;
    constructor Create(AValue: Double); overload;
    constructor Create(AValue: TValue); overload;
    property Value: TValue read FValue;
  end;

  StoredAttribute = System.Classes.StoredAttribute;

  StorageKeyAttribute = class(TCustomAttribute)
  private
    FKey: string;
  public
    constructor Create(const AKey: string);
    property Key: string read FKey;
  end;

type
  IStorageTarget = interface
  ['{13CF0202-A7DB-4452-A48C-58B1EEE804BD}']
    function ReadString(const Key, Ident, Default: string): string;
    procedure WriteString(const Key, Ident, Value: string);
  end;
  TStorageTargetFactory = TFunc<IStorageTarget>;

  TStorageTargetMessage = class(TCommonMessage<string>)
  private
    FStorageTarget: IStorageTarget;
    function GetFileName: string;
  public
    class function Execute(Sender: TObject; const AFileName: string): IStorageTarget;
    property FileName: string read GetFileName;
    property StorageTarget: IStorageTarget read FStorageTarget write FStorageTarget;
  end;

  TStorageTargetDescriptor = record
    Description: string;
    FileExtension: string;
  public
    constructor Create(const ADescription, AFileExtension: string);
  end;
  TStorageTargetDescriptorList = TList<TStorageTargetDescriptor>;
  TStorageTargetListMessage = class(TCommonMessage<TStorageTargetDescriptorList>);

type
  TDataStorage = class
  strict private
  const
    cKeySeparator: string = '\';
    cBool: array[Boolean] of Integer = (0, 1);
  class var
    FAutoRegisterHandler: Boolean;
    FDefaultInstance: TDataStorage;
    FStorageTargetFactory: TStorageTargetFactory;
    class destructor Destruct;
    class function GetDefaultInstance: TDataStorage; static;
  private type
    TStorageKeyStack = TStack<string>;
  private
    FStorageKey: string;
    FStorageKeyStack: TStorageKeyStack;
    FStorageTarget: IStorageTarget;
    procedure SetStorageTarget(const Value: IStorageTarget);
  strict protected
    class procedure InitDefaults(Instance: TObject; AField: TRttiField); overload;
    class procedure InitDefaults(Instance: TObject; AProp: TRttiProperty); overload;
    function InternalReadString(const Ident, Default: string): string; virtual;
    procedure InternalWriteString(const Ident, Value: string); virtual;
    procedure LoadFromStorage<T: StoredAttribute>(Instance: TObject; AField: TRttiField); overload;
    procedure LoadFromStorage<T: StoredAttribute>(Instance: TObject; AProp: TRttiProperty); overload;
    procedure ReadInstance<T: StoredAttribute>(const Ident: string; Instance: TObject);
    procedure SaveToStorage<T: StoredAttribute>(Instance: TObject; AField: TRttiField); overload;
    procedure SaveToStorage<T: StoredAttribute>(Instance: TObject; AProp: TRttiProperty); overload;
    procedure WriteInstance<T: StoredAttribute>(const Ident: string; Instance: TObject);
  protected
    function GetStoredName(const Value, Default: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateStorageTarget(const AFileName: string = ''): IStorageTarget; overload;
    class function CreateStorageTarget(Sender: TObject; const AFileName: string = ''): IStorageTarget; overload;
    class procedure InitDefaults(Instance: TObject); overload; static;
    class function IsSupportedTargetExtension(const AExtension: string): Boolean;
    class procedure ListStorageTargets(Target: TStorageTargetDescriptorList);
    procedure LoadFromStorage(Instance: TObject); overload;
    procedure LoadFromStorage<T: StoredAttribute>(Instance: TObject); overload;
    function MakeStorageSubKey(const ASubKey: string): string;
    class function MakeStorageTargetFileFilter: string;
    procedure PopStorageKey;
    procedure PushStorageKey; overload;
    procedure PushStorageKey(const NewKey: string); overload;
    function ReadBoolean(const Ident: string; const Default: Boolean): Boolean;
    function ReadDateTime(const Ident: string; const Default: TDateTime): TDateTime;
    function ReadInteger(const Ident: string; const Default: Integer): Integer;
    function ReadString(const Ident, Default: string): string;
    procedure ReadStrings(const Ident: string; Target: TStrings);
    function ReadValue(const Ident: string; const Default: TValue): TValue;
    procedure SaveToStorage(Instance: TObject); overload;
    procedure SaveToStorage<T: StoredAttribute>(Instance: TObject); overload;
    class function SplitStorageKey(const AStorageKey: string): TArray<string>;
    procedure WriteBoolean(const Ident: string; const Value: Boolean);
    procedure WriteDateTime(const Ident: string; const Value: TDateTime);
    procedure WriteInteger(const Ident: string; const Value: Integer);
    procedure WriteString(const Ident: string; const Value: string);
    procedure WriteStrings(const Ident: string; Source: TStrings);
    procedure WriteValue(const Ident: string; const Value: TValue);
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
    class property DefaultInstance: TDataStorage read GetDefaultInstance;
    class property StorageTargetFactory: TStorageTargetFactory read FStorageTargetFactory write FStorageTargetFactory;
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
  strict protected
    function GetDataStorage: TDataStorage; virtual;
  protected
    function InternalGetStorageKey: string; virtual;
    procedure InternalInitDefaults; virtual;
    procedure InternalLoadFromStorage; virtual;
    procedure InternalSaveToStorage; virtual;
    procedure InternalPrepareStorage; virtual;
    procedure PrepareStorage;
    property DataStorage: TDataStorage read GetDataStorage;
  public
    procedure Finalize; virtual;
    procedure InitDefaults;
    procedure Initialize; virtual;
    procedure LoadFromStorage;
    procedure SaveToStorage;
  end;

type
  TStoredWrapper = class(TCustomStoredClass)
  private
    FTarget: TObject;
  protected
    function InternalGetStorageKey: string; override;
    procedure InternalInitDefaults; override;
    procedure InternalLoadFromStorage; override;
    procedure InternalPrepareStorage; override;
    procedure InternalSaveToStorage; override;
  public
    constructor Create(ATarget: TObject);
    property Target: TObject read FTarget;
  end;

function GetStorageKeyFromAttribute(AInstance: TObject; const ADefault: string = ''): string;

implementation

uses
  System.TypInfo;

function GetStorageKeyFromAttribute(AInstance: TObject; const ADefault: string = ''): string;
begin
  Result := ADefault;
  var context := TRTTIContext.Create;
  try
    var myType := context.GetType(AInstance.ClassType);
    if myType <> nil then begin
      var attr := myType.GetAttribute<StorageKeyAttribute>;
      if attr <> nil then
        Result := attr.Key;
    end;
  finally
    context.Free;
  end;
end;

constructor DefaultAttribute.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

constructor DefaultAttribute.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

constructor DefaultAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

constructor DefaultAttribute.Create(AValue: Double);
begin
  inherited Create;
  FValue := AValue;
end;

constructor DefaultAttribute.Create(AValue: TValue);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TDataStorage.Create;
begin
  inherited Create;
  FStorageKeyStack := TStorageKeyStack.Create();
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

class function TDataStorage.GetDefaultInstance: TDataStorage;
begin
  if FDefaultInstance = nil then begin
    FDefaultInstance := TDataStorage.Create;
    FDefaultInstance.StorageTarget := FDefaultInstance.CreateStorageTarget();
  end;
  Result := FDefaultInstance;
end;

function TDataStorage.GetStoredName(const Value, Default: string): string;
begin
  Result := Value;
  if Result.IsEmpty then
    Result := Default;
end;

class procedure TDataStorage.InitDefaults(Instance: TObject);
var
  context: TRttiContext;
begin
  if Instance = nil then Exit;

  context := TRttiContext.Create;
  try
    var myType := context.GetType(Instance.ClassType);
    if myType <> nil then begin
      { initialize all fields having a Default attribute }
      for var field in myType.GetFields do
        InitDefaults(Instance, field);

      { initialize all properties having a Default attribute }
      for var prop in myType.GetProperties do
        InitDefaults(Instance, prop);
    end;
  finally
    context.Free;
  end;
end;

class procedure TDataStorage.InitDefaults(Instance: TObject; AField: TRttiField);
begin
  var attr := AField.GetAttribute<DefaultAttribute>;
  if (attr = nil) then Exit;
  if AField.FieldType.IsInstance then begin
    InitDefaults(AField.GetValue(Instance).AsObject);
  end
  else begin
    if attr.Value.IsEmpty then Exit;
    AField.SetValue(Instance, attr.value);
  end;
end;

class procedure TDataStorage.InitDefaults(Instance: TObject; AProp: TRttiProperty);
begin
  var attr := AProp.GetAttribute<DefaultAttribute>;
  if (attr = nil) or attr.Value.IsEmpty then Exit;
  AProp.SetValue(Instance, attr.value);
end;

function TDataStorage.InternalReadString(const Ident, Default: string): string;
begin
  Result := Default;
  if Assigned(FStorageTarget) then
    Result := FStorageTarget.ReadString(StorageKey, Ident, Default);
end;

procedure TDataStorage.InternalWriteString(const Ident, Value: string);
begin
  if Assigned(FStorageTarget) then
    FStorageTarget.WriteString(StorageKey, Ident, Value);
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
  LoadFromStorage<StoredAttribute>(Instance);
end;

procedure TDataStorage.LoadFromStorage<T>(Instance: TObject);
var
  context: TRttiContext;
begin
  var newKey := StorageKey;
  if newKey.IsEmpty then
    newKey := GetStorageKeyFromAttribute(Instance, Instance.ClassName);
  PushStorageKey(newKey);
  try
    context := TRttiContext.Create;
    try
      var myType := context.GetType(Instance.ClassType);
      if myType <> nil then begin
        for var field in myType.GetFields do
          LoadFromStorage<T>(Instance, field);

        for var prop in myType.GetProperties do
          LoadFromStorage<T>(Instance, prop);
      end;
    finally
      context.Free;
    end;
  finally
    PopStorageKey;
  end;
end;

procedure TDataStorage.LoadFromStorage<T>(Instance: TObject; AField: TRttiField);
begin
  var attr := AField.GetAttribute<T>;
  if (attr = nil) or not attr.Flag then Exit;
  var storedName := GetStoredName(attr.Name, AField.Name.Substring(1));
  if AField.FieldType.IsInstance then begin
    ReadInstance<T>(storedName, AField.GetValue(Instance).AsObject);
  end
  else begin
    var value := ReadValue(storedName, AField.GetValue(Instance));
    if not value.IsEmpty then
      AField.SetValue(Instance, value);
  end;
end;

procedure TDataStorage.LoadFromStorage<T>(Instance: TObject; AProp: TRttiProperty);
begin
  var attr := AProp.GetAttribute<T>;
  if (attr = nil) or not attr.Flag then Exit;
  var storedName := GetStoredName(attr.Name, AProp.Name);
  if AProp.PropertyType.IsInstance then begin
    ReadInstance<T>(storedName, AProp.GetValue(Instance).AsObject);
  end
  else begin
    var value := ReadValue(storedName, AProp.GetValue(Instance));
    if not value.IsEmpty then
      AProp.SetValue(Instance, value);
  end;
end;

function TDataStorage.MakeStorageSubKey(const ASubKey: string): string;
begin
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
  Result := (ReadInteger(Ident, cBool[Default]) = cBool[true]);
end;

function TDataStorage.ReadDateTime(const Ident: string; const Default: TDateTime): TDateTime;
var
  S: string;
begin
  Result := Default;
  S := ReadString(Ident, '');
  if not S.IsEmpty then begin
    Result := StrToDateTimeDef(S, Default, TFormatSettings.Invariant);
  end;
end;

procedure TDataStorage.ReadInstance<T>(const Ident: string; Instance: TObject);
begin
  PushStorageKey(MakeStorageSubKey(Ident));
  try
    LoadFromStorage<T>(Instance);
  finally
    PopStorageKey;
  end;
end;

function TDataStorage.ReadInteger(const Ident: string; const Default: Integer): Integer;
var
  S: string;
begin
  S := ReadString(Ident, '');
  if (S.Length > 2) and (S.StartsWith('0x',true)) then begin
    S := '$' + S.Substring(2);
  end;
  Result := StrToIntDef(S, Default);
end;

function TDataStorage.ReadString(const Ident, Default: string): string;
begin
  Result := InternalReadString(Ident, Default);
end;

procedure TDataStorage.ReadStrings(const Ident: string; Target: TStrings);
begin
  Target.CommaText := ReadString(Ident, Target.CommaText);
end;

function TDataStorage.ReadValue(const Ident: string; const Default: TValue): TValue;
var
  S: string;
begin
  S := ReadString(Ident, '');
  if S = '' then begin
    Exit(Default);
  end;

  case Default.Kind of
    tkEnumeration: begin
      Result := TValue.FromOrdinal(Default.TypeInfo, GetEnumValue(Default.TypeInfo, S));
    end;
    tkFloat: begin { independent from current FormatSettings! }
      case Default.TypeData.FloatType of
        ftDouble: begin { handle special Double cases }
          if Default.TypeInfo = TypeInfo(TDate) then
            Result := TValue.From<TDate>(StrToDateDef(S, Default.AsType<TDate>, TFormatSettings.Invariant))
          else if Default.TypeInfo = TypeInfo(TTime) then
            Result := TValue.From<TTime>(StrToTimeDef(S, Default.AsType<TTime>, TFormatSettings.Invariant))
          else if Default.TypeInfo = TypeInfo(TDateTime) then
            Result := TValue.From<TDateTime>(StrToDateTimeDef(S, Default.AsType<TDateTime>, TFormatSettings.Invariant))
          else
            Result := TValue.From<Double>(StrToFloatDef(S, Default.AsType<Double>, TFormatSettings.Invariant));
        end;
        ftSingle: Result := TValue.From<Single>(StrToFloatDef(S, Default.AsType<Single>, TFormatSettings.Invariant));
        ftExtended: Result := TValue.From<Extended>(StrToFloatDef(S, Default.AsType<Extended>, TFormatSettings.Invariant));
        ftComp: Result := TValue.From<Comp>(StrToInt64Def(S, Default.AsInt64));
        ftCurr: Result := TValue.From<Currency>(StrToCurrDef(S, Default.AsCurrency, TFormatSettings.Invariant));
      end;
    end;
    tkInteger: Result := StrToIntDef(S, Default.AsInteger);
    tkInt64: Result := StrToInt64Def(S, Default.AsInt64);
    tkWChar,
    tkChar: Result := TValue.From<Char>(S.Chars[0]); { an empty string S has already be handled at the start of this method }
    tkString,
    tkLString,
    tkWString,
    tkUString: Result := S;
  else
    { tkSet, tkClass, tkMethod, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray, tkClassRef, tkPointer, tkProcedure }
    raise ENotImplemented.CreateFmt('Stored type "%s" not supported!', [GetTypeName(Default.TypeInfo)]);
  end;
end;

procedure TDataStorage.SaveToStorage(Instance: TObject);
begin
  SaveToStorage<StoredAttribute>(Instance);
end;

procedure TDataStorage.SaveToStorage<T>(Instance: TObject);
var
  context: TRttiContext;
begin
  var newKey := StorageKey;
  if newKey.IsEmpty then
    newKey := GetStorageKeyFromAttribute(Instance, Instance.ClassName);
  PushStorageKey(newKey);
  try
    context := TRttiContext.Create;
    try
      var myType := context.GetType(Instance.ClassType);
      if myType <> nil then begin
        for var field in myType.GetFields do
          SaveToStorage<T>(Instance, field);

        for var prop in myType.GetProperties do
          SaveToStorage<T>(Instance, prop);
      end;
    finally
      context.Free;
    end;
  finally
    PopStorageKey;
  end;
end;

procedure TDataStorage.SaveToStorage<T>(Instance: TObject; AField: TRttiField);
begin
  var attr := AField.GetAttribute<T>;
  if (attr = nil) or not attr.Flag then Exit;
  var storedName := GetStoredName(attr.name, AField.Name.Substring(1));
  if AField.FieldType.IsInstance then
    WriteInstance<T>(storedName, AField.GetValue(Instance).AsObject)
  else
    WriteValue(storedName, AField.GetValue(Instance));
end;

procedure TDataStorage.SaveToStorage<T>(Instance: TObject; AProp: TRttiProperty);
begin
  var attr := AProp.GetAttribute<T>;
  if (attr = nil) or not attr.Flag then Exit;
  var storedName := GetStoredName(attr.name, AProp.Name);
  if AProp.PropertyType.IsInstance then
    WriteInstance<T>(storedName, AProp.GetValue(Instance).AsObject)
  else
    WriteValue(storedName, AProp.GetValue(Instance));
end;

procedure TDataStorage.SetStorageTarget(const Value: IStorageTarget);
begin
  FStorageTarget := Value;
end;

class function TDataStorage.SplitStorageKey(const AStorageKey: string): TArray<string>;
begin
  Result := AStorageKey.Split([cKeySeparator]);
end;

procedure TDataStorage.WriteBoolean(const Ident: string; const Value: Boolean);
begin
  WriteInteger(Ident, cBool[Value]);
end;

procedure TDataStorage.WriteDateTime(const Ident: string; const Value: TDateTime);
begin
  WriteString(Ident, DateTimeToStr(Value, TFormatSettings.Invariant));
end;

procedure TDataStorage.WriteInstance<T>(const Ident: string; Instance: TObject);
begin
  PushStorageKey(MakeStorageSubKey(Ident));
  try
    SaveToStorage<T>(Instance);
  finally
    PopStorageKey;
  end;
end;

procedure TDataStorage.WriteInteger(const Ident: string; const Value: Integer);
begin
  WriteString(Ident, IntToStr(Value));
end;

procedure TDataStorage.WriteString(const Ident: string; const Value: string);
begin
  InternalWriteString(Ident, Value);
end;

procedure TDataStorage.WriteStrings(const Ident: string; Source: TStrings);
begin
  WriteString(Ident, Source.CommaText);
end;

procedure TDataStorage.WriteValue(const Ident: string; const Value: TValue);
begin
  var S := Value.ToString; { ToString already handle most of the cases correct }
  case Value.Kind of
    tkFloat: begin { independent from current FormatSettings! }
      case Value.TypeData.FloatType of
        ftDouble: begin { handle special Double cases }
          if Value.TypeInfo = TypeInfo(TDate) then
            S := DateToStr(Value.AsType<TDate>, TFormatSettings.Invariant)
          else if Value.TypeInfo = TypeInfo(TTime) then
            S := TimeToStr(Value.AsType<TTime>, TFormatSettings.Invariant)
          else if Value.TypeInfo = TypeInfo(TDateTime) then
            S := DateTimeToStr(Value.AsType<TDateTime>, TFormatSettings.Invariant)
          else
            S := FloatToStr(Value.AsType<Double>, TFormatSettings.Invariant);
        end;
        ftSingle,
        ftExtended: begin
          S := FloatToStr(Value.AsExtended, TFormatSettings.Invariant);
        end;
        ftComp: S := IntToStr(Value.AsInt64);
        ftCurr: S := CurrToStr(Value.AsCurrency, TFormatSettings.Invariant);
      end;
    end;
    tkEnumeration,
    tkInteger,
    tkInt64,
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString: ; { Value.ToString already did the job }
  else
    { tkSet, tkClass, tkMethod, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray, tkClassRef, tkPointer, tkProcedure }
    raise ENotImplemented.CreateFmt('Stored type "%s" not supported!', [GetTypeName(Value.TypeInfo)]);
  end;
  WriteString(Ident, S);
end;

procedure TCustomStoredClass.Finalize;
begin
  SaveToStorage;
end;

function TCustomStoredClass.GetDataStorage: TDataStorage;
begin
  Result := TDataStorage.DefaultInstance;
end;

procedure TCustomStoredClass.InitDefaults;
begin
  InternalInitDefaults;
end;

procedure TCustomStoredClass.Initialize;
begin
  InitDefaults;
  LoadFromStorage;
end;

function TCustomStoredClass.InternalGetStorageKey: string;
begin
  Result := '';
end;

procedure TCustomStoredClass.InternalInitDefaults;
begin
end;

procedure TCustomStoredClass.InternalLoadFromStorage;
begin
end;

procedure TCustomStoredClass.InternalPrepareStorage;
begin
end;

procedure TCustomStoredClass.InternalSaveToStorage;
begin
end;

procedure TCustomStoredClass.LoadFromStorage;
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage;
    InternalLoadFromStorage;
  finally
    DataStorage.PopStorageKey;
  end;
end;

procedure TCustomStoredClass.PrepareStorage;
var
  key: string;
begin
  key := InternalGetStorageKey;
  if not key.IsEmpty then
    DataStorage.StorageKey := key;
  InternalPrepareStorage;
end;

procedure TCustomStoredClass.SaveToStorage;
begin
  DataStorage.PushStorageKey;
  try
    PrepareStorage;
    InternalSaveToStorage;
  finally
    DataStorage.PopStorageKey;
  end;
end;

constructor TStoredWrapper.Create(ATarget: TObject);
begin
  inherited Create;
  FTarget := ATarget;
end;

function TStoredWrapper.InternalGetStorageKey: string;
var
  intf: IStorageKey;
begin
  { do we have a StorageKey attribute? }
  Result := GetStorageKeyFromAttribute(Target);
  if Result.IsEmpty then begin
    { no, then we look for IStorageKey support }
    if Supports(Target, IStorageKey, intf) then
      Result := intf.GetStorageKey(DataStorage);
  end;
end;

procedure TStoredWrapper.InternalInitDefaults;
var
  intf: IStoredData;
begin
  DataStorage.InitDefaults(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalInitDefaults(DataStorage);
end;

procedure TStoredWrapper.InternalLoadFromStorage;
var
  intf: IStoredData;
begin
  DataStorage.LoadFromStorage(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalLoadFromStorage(DataStorage);
end;

procedure TStoredWrapper.InternalPrepareStorage;
var
  intf: IStoredData;
begin
  if Supports(Target, IStoredData, intf) then
    intf.InternalPrepareStorage(DataStorage);
end;

procedure TStoredWrapper.InternalSaveToStorage;
var
  intf: IStoredData;
begin
  DataStorage.SaveToStorage(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalSaveToStorage(DataStorage);
end;

constructor StorageKeyAttribute.Create(const AKey: string);
begin
  inherited Create;
  FKey := AKey;
end;

class function TStorageTargetMessage.Execute(Sender: TObject; const AFileName: string): IStorageTarget;
var
  instance: TStorageTargetMessage;
begin
  instance := Self.Create(AFileName);
  try
    Manager.SendMessage(Sender, instance, False);
    Result := instance.StorageTarget;
  finally
    instance.Free;
  end;
end;

function TStorageTargetMessage.GetFileName: string;
begin
  Result := Value;
end;

constructor TStorageTargetDescriptor.Create(const ADescription, AFileExtension: string);
begin
  Description := ADescription;
  FileExtension := AFileExtension;
end;

initialization
  TDataStorage.AutoRegisterHandler := True;
end.
