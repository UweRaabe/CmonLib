unit Cmon.DataStorage;

interface

uses
  System.Rtti, System.Classes, System.SysUtils, System.Generics.Collections, System.TypInfo,
  Cmon.Messaging;

type
  { DefaultAttribute from System.Classes uses a Variant internally and casts the type Boolean to Integer. Thus the original
    type information gets lost, while we do rely on it here. Therefore we declare our own DefaultAttribute based on TValue
    and provide constructors for all types we need. Other types can be added in special derived classes if necessary.
  }
  TCustomDefaultAttribute = class(TCustomAttribute)
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

type
  DefaultAttribute = class(TCustomDefaultAttribute);

type
  TCustomStorageAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string = '');
    property Name: string read FName;
  end;

type
  StorageAttribute = class(TCustomStorageAttribute);

type
  TCustomAutoStorageAttribute = class(TCustomAttribute)
  private
    FVisibilities: TVisibilityClasses;
  public
    constructor Create(AVisibilities: TVisibilityClasses);
    function CheckVisibility(AVisibility: TMemberVisibility): Boolean;
  end;

type
  AutoStorageFieldsAttribute = class(TCustomAutoStorageAttribute);
  AutoStoragePropertiesAttribute = class(TCustomAutoStorageAttribute);
  NoAutoStorageAttribute = class(TCustomAttribute);

type
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
    procedure EraseStorageKey(const Key: string);
    procedure DeleteKey(const Key, Ident: string);
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
  TStorageTargetMissingMessage = class(TCommonMessage<string>);

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
    function LoadAutoStorage(Instance: TObject): Boolean; overload;
    procedure LoadAutoStorage(Instance: TObject; AField: TRttiField); overload;
    procedure LoadAutoStorage(Instance: TObject; AProp: TRttiProperty); overload;
    function SaveAutoStorage(Instance: TObject): Boolean; overload;
    procedure SaveAutoStorage(Instance: TObject; AField: TRttiField); overload;
    procedure SaveAutoStorage(Instance: TObject; AProp: TRttiProperty); overload;
    procedure SetStorageTarget(const Value: IStorageTarget);
  strict protected
    class procedure ErrorNotImplemented(const ATypeName: string);
    class procedure InitDefaults<T: TCustomDefaultAttribute>(Instance: TObject; AField: TRttiField); overload;
    class procedure InitDefaults<T: TCustomDefaultAttribute>(Instance: TObject; AProp: TRttiProperty); overload;
    procedure InternalDeleteKey(const Ident: string); virtual;
    procedure InternalEraseStorageKey; virtual;
    function InternalReadString(const Ident, Default: string): string; virtual;
    procedure InternalWriteString(const Ident, Value: string); virtual;
    procedure LoadFromStorage<T: TCustomStorageAttribute>(Instance: TObject; AProp: TRttiProperty); overload;
    procedure LoadFromStorage<T: TCustomStorageAttribute>(Instance: TObject; AField: TRttiField); overload;
    procedure ReadInstance(const Ident: string; Instance: TObject); overload;
    procedure ReadInstance<T: TCustomStorageAttribute>(const Ident: string; Instance: TObject); overload;
    procedure SaveToStorage<T: TCustomStorageAttribute>(Instance: TObject; AField: TRttiField); overload;
    procedure SaveToStorage<T: TCustomStorageAttribute>(Instance: TObject; AProp: TRttiProperty); overload;
    procedure WriteInstance(const Ident: string; Instance: TObject); overload;
    procedure WriteInstance<T: TCustomStorageAttribute>(const Ident: string; Instance: TObject); overload;
  protected
    function GetStoredName(const Value, Default: string): string; overload;
    function GetStoredName(Attribute: TCustomStorageAttribute; const Default: string): string; overload;
    function GetStoredName(AField: TRttiField): string; overload;
    function GetStoredName(AProp: TRttiProperty): string; overload;
    function RetrieveStorageKey(Instance: TObject): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateStorageTarget(const AFileName: string = ''): IStorageTarget; overload;
    class function CreateStorageTarget(Sender: TObject; const AFileName: string = ''): IStorageTarget; overload;
    procedure EraseStorageKey;
    procedure DeleteKey(const Ident: string);
    class procedure InitDefaults(Instance: TObject); overload; static;
    class procedure InitDefaults<T: TCustomDefaultAttribute>(Instance: TObject); overload; static;
    class function IsSupportedTargetExtension(const AExtension: string): Boolean;
    class procedure ListStorageTargets(Target: TStorageTargetDescriptorList);
    procedure LoadFromStorage(Instance: TObject); overload;
    procedure LoadFromStorage<T: TCustomStorageAttribute>(Instance: TObject); overload;
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
    procedure SaveToStorage<T: TCustomStorageAttribute>(Instance: TObject); overload;
    class function SplitStorageKey(const AStorageKey: string): TArray<string>;
    class function StringToValue(const AString: string; const Default: TValue): TValue;
    class function ValueToString(const Value: TValue): string;
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
    function GetTarget: TObject; virtual;
    property Target: TObject read GetTarget;
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
  strict protected
    function GetTarget: TObject; override;
  public
    constructor Create(ATarget: TObject);
    property Target: TObject read GetTarget;
  end;

type
  TStoredClass = class(TCustomStoredClass);

function GetStorageKeyFromAttribute(AInstance: TObject; const ADefault: string = ''): string;

implementation

function GetDynArrayElType(ATypeInfo: PTypeInfo): PTypeInfo;
var
  ref: PPTypeInfo;
begin
  // Get real element type of dynamic array, even if it's array of array of etc.
  ref := GetTypeData(ATypeInfo).DynArrElType;
  if ref = nil then
    Exit(nil);
  Result := ref^;
end;

function GetArrayElType(ATypeInfo: PTypeInfo): PTypeInfo;
var
  ref: PPTypeInfo;
begin
  if ATypeInfo^.Kind = tkArray then
  begin
    ref := GetTypeData(ATypeInfo)^.ArrayData.ElType;
    if ref = nil then
      Result := nil
    else
      Result := ref^;
  end
  else if ATypeInfo^.Kind = tkDynArray then
    Exit(GetDynArrayElType(ATypeInfo))
  else
    Exit(nil);
end;

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

constructor TCustomDefaultAttribute.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TCustomDefaultAttribute.Create(AValue: Double);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TCustomDefaultAttribute.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TCustomDefaultAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TCustomDefaultAttribute.Create(AValue: TValue);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TCustomStorageAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
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

procedure TDataStorage.EraseStorageKey;
begin
  InternalEraseStorageKey;
end;

procedure TDataStorage.DeleteKey(const Ident: string);
begin
  InternalDeleteKey(Ident);
end;

class procedure TDataStorage.ErrorNotImplemented(const ATypeName: string);
begin
  raise ENotImplemented.CreateFmt('Storage type "%s" not supported!', [ATypeName]);
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

function TDataStorage.GetStoredName(Attribute: TCustomStorageAttribute; const Default: string): string;
begin
  Result := Default;
  if (Attribute <> nil) and not Attribute.Name.IsEmpty then
    Result := Attribute.Name;
end;

function TDataStorage.GetStoredName(AField: TRttiField): string;
begin
  Result := GetStoredName(AField.GetAttribute<StorageAttribute>, AField.Name.Substring(1));
end;

function TDataStorage.GetStoredName(AProp: TRttiProperty): string;
begin
  Result := GetStoredName(AProp.GetAttribute<StorageAttribute>, AProp.Name);
end;

class procedure TDataStorage.InitDefaults(Instance: TObject);
begin
  InitDefaults<DefaultAttribute>(Instance);
end;

class procedure TDataStorage.InitDefaults<T>(Instance: TObject);
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
        InitDefaults<T>(Instance, field);

      { initialize all properties having a Default attribute }
      for var prop in myType.GetProperties do
        InitDefaults<T>(Instance, prop);
    end;
  finally
    context.Free;
  end;
end;

class procedure TDataStorage.InitDefaults<T>(Instance: TObject; AField: TRttiField);
begin
  var attr := AField.GetAttribute<T>;
  if (attr = nil) then Exit;
  if AField.FieldType.IsInstance then begin
    InitDefaults<T>(AField.GetValue(Instance).AsObject);
  end
  else begin
    if attr.Value.IsEmpty then Exit;
    AField.SetValue(Instance, attr.value);
  end;
end;

class procedure TDataStorage.InitDefaults<T>(Instance: TObject; AProp: TRttiProperty);
begin
  var attr := AProp.GetAttribute<T>;
  if (attr = nil) or attr.Value.IsEmpty then Exit;
  AProp.SetValue(Instance, attr.value);
end;

procedure TDataStorage.InternalEraseStorageKey;
begin
  if Assigned(FStorageTarget) then
    FStorageTarget.EraseStorageKey(StorageKey);
end;

procedure TDataStorage.InternalDeleteKey(const Ident: string);
begin
  if Assigned(FStorageTarget) then
    FStorageTarget.DeleteKey(StorageKey, Ident);
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

function TDataStorage.LoadAutoStorage(Instance: TObject): Boolean;
var
  attr: TCustomAutoStorageAttribute;
begin
  Result := False;
  PushStorageKey(RetrieveStorageKey(Instance));
  try
    var context := TRTTIContext.Create;
    try
      var myType := context.GetType(Instance.ClassType);
      if myType <> nil then begin
        attr := myType.GetAttribute<AutoStorageFieldsAttribute>;
        if attr <> nil then begin
          Result := True;
          for var field in myType.GetFields do
            if attr.CheckVisibility(field.Visibility) and not field.HasAttribute<NoAutoStorageAttribute> then
              LoadAutoStorage(Instance, field);
        end;

        attr := myType.GetAttribute<AutoStoragePropertiesAttribute>;
        if attr <> nil then begin
          Result := True;
          for var prop in myType.GetProperties do
            if attr.CheckVisibility(prop.Visibility) and not prop.HasAttribute<NoAutoStorageAttribute> then
              LoadAutoStorage(Instance, prop);
        end;
      end;
    finally
      context.Free;
    end;
  finally
    PopStorageKey;
  end;
end;

procedure TDataStorage.LoadFromStorage(Instance: TObject);
begin
  if not LoadAutoStorage(Instance) then
    LoadFromStorage<StorageAttribute>(Instance);
end;

procedure TDataStorage.LoadAutoStorage(Instance: TObject; AField: TRttiField);
begin
  var storedName := GetStoredName(AField);
  if AField.FieldType.IsInstance then begin
    ReadInstance(storedName, AField.GetValue(Instance).AsObject);
  end
  else begin
    var value := ReadValue(storedName, AField.GetValue(Instance));
    if not value.IsEmpty then
      AField.SetValue(Instance, value);
  end;
end;

procedure TDataStorage.LoadAutoStorage(Instance: TObject; AProp: TRttiProperty);
begin
  var storedName := GetStoredName(AProp);
  if AProp.PropertyType.IsInstance then begin
    ReadInstance(storedName, AProp.GetValue(Instance).AsObject);
  end
  else begin
    var value := ReadValue(storedName, AProp.GetValue(Instance));
    if not value.IsEmpty then
      AProp.SetValue(Instance, value);
  end;
end;

procedure TDataStorage.LoadFromStorage<T>(Instance: TObject);
var
  context: TRttiContext;
begin
  PushStorageKey(RetrieveStorageKey(Instance));
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
  if (attr = nil) then Exit;
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
  if (attr = nil) then Exit;
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
  if StorageKey.IsEmpty then
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

procedure TDataStorage.ReadInstance(const Ident: string; Instance: TObject);
begin
  PushStorageKey(MakeStorageSubKey(Ident));
  try
    LoadFromStorage(Instance);
  finally
    PopStorageKey;
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

  Result := StringToValue(S, Default);
end;

function TDataStorage.RetrieveStorageKey(Instance: TObject): string;
begin
  Result := StorageKey;
  if Result.IsEmpty then
    Result := GetStorageKeyFromAttribute(Instance, Instance.ClassName);
end;

function TDataStorage.SaveAutoStorage(Instance: TObject): Boolean;
var
  attr: TCustomAutoStorageAttribute;
begin
  Result := False;

  PushStorageKey(RetrieveStorageKey(Instance));
  try
    var context := TRTTIContext.Create;
    try
      var myType := context.GetType(Instance.ClassType);
      if myType <> nil then begin
        attr := myType.GetAttribute<AutoStorageFieldsAttribute>;
        if attr <> nil then begin
          Result := True;
          for var field in myType.GetFields do
            if attr.CheckVisibility(field.Visibility) and not field.HasAttribute<NoAutoStorageAttribute> then
              SaveAutoStorage(Instance, field);
        end;

        attr := myType.GetAttribute<AutoStoragePropertiesAttribute>;
        if attr <> nil then begin
          Result := True;
          for var prop in myType.GetProperties do
            if attr.CheckVisibility(prop.Visibility) and not prop.HasAttribute<NoAutoStorageAttribute> then
              SaveAutoStorage(Instance, prop);
        end;
      end;
    finally
      context.Free;
    end;
  finally
    PopStorageKey;
  end;
end;

procedure TDataStorage.SaveAutoStorage(Instance: TObject; AField: TRttiField);
begin
  var storedName := GetStoredName(AField);
  if AField.FieldType.IsInstance then
    WriteInstance(storedName, AField.GetValue(Instance).AsObject)
  else
    WriteValue(storedName, AField.GetValue(Instance));
end;

procedure TDataStorage.SaveAutoStorage(Instance: TObject; AProp: TRttiProperty);
begin
  var storedName := GetStoredName(AProp);
  if AProp.PropertyType.IsInstance then
    WriteInstance(storedName, AProp.GetValue(Instance).AsObject)
  else
    WriteValue(storedName, AProp.GetValue(Instance));
end;

procedure TDataStorage.SaveToStorage(Instance: TObject);
begin
  if not SaveAutoStorage(Instance) then
    SaveToStorage<StorageAttribute>(Instance);
end;

procedure TDataStorage.SaveToStorage<T>(Instance: TObject);
var
  context: TRttiContext;
begin
  PushStorageKey(RetrieveStorageKey(Instance));
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
  if (attr = nil) then Exit;
  var storedName := GetStoredName(attr.name, AField.Name.Substring(1));
  if AField.FieldType.IsInstance then
    WriteInstance<T>(storedName, AField.GetValue(Instance).AsObject)
  else
    WriteValue(storedName, AField.GetValue(Instance));
end;

procedure TDataStorage.SaveToStorage<T>(Instance: TObject; AProp: TRttiProperty);
begin
  var attr := AProp.GetAttribute<T>;
  if (attr = nil) then Exit;
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

class function TDataStorage.StringToValue(const AString: string; const Default: TValue): TValue;
begin
  Result := Default;

  case Default.Kind of
    tkEnumeration: begin
      var valInt64: Int64;
      if TryStrToInt64(AString, valInt64) then begin
        if Default.TypeInfo = TypeInfo(Boolean) then
          Result := (valInt64 = cBool[True])
        else
          Result := TValue.FromOrdinal(Default.TypeInfo, valInt64);
      end
      else begin
        var ordinal := GetEnumValue(Default.TypeInfo, AString);
        if ordinal < 0 then
          Result := Default
        else
          Result := TValue.FromOrdinal(Default.TypeInfo, ordinal);
      end;
    end;
    tkSet: begin
      StringToSet(Result.TypeInfo, AString, Result.GetReferenceToRawData);
    end;
    tkFloat: begin { independent from current FormatSettings! }
      case Default.TypeData.FloatType of
        ftDouble: begin { handle special Double cases }
          if Default.TypeInfo = TypeInfo(TDate) then
            Result := TValue.From<TDate>(StrToDateDef(AString, Default.AsType<TDate>, TFormatSettings.Invariant))
          else if Default.TypeInfo = TypeInfo(TTime) then
            Result := TValue.From<TTime>(StrToTimeDef(AString, Default.AsType<TTime>, TFormatSettings.Invariant))
          else if Default.TypeInfo = TypeInfo(TDateTime) then
            Result := TValue.From<TDateTime>(StrToDateTimeDef(AString, Default.AsType<TDateTime>, TFormatSettings.Invariant))
          else
            Result := TValue.From<Double>(StrToFloatDef(AString, Default.AsType<Double>, TFormatSettings.Invariant));
        end;
        ftSingle: Result := TValue.From<Single>(StrToFloatDef(AString, Default.AsType<Single>, TFormatSettings.Invariant));
        ftExtended: Result := TValue.From<Extended>(StrToFloatDef(AString, Default.AsType<Extended>, TFormatSettings.Invariant));
        ftComp: Result := TValue.From<Comp>(StrToInt64Def(AString, Default.AsInt64));
        ftCurr: Result := TValue.From<Currency>(StrToCurrDef(AString, Default.AsCurrency, TFormatSettings.Invariant));
      end;
    end;
    tkInteger: Result := StrToIntDef(AString, Default.AsInteger);
    tkInt64: Result := StrToInt64Def(AString, Default.AsInt64);
    tkArray,
    tkDynArray: begin
      if AString.StartsWith('[') and AString.EndsWith(']') then begin
        var lst := TStringList.Create();
        try
          lst.CommaText := AString.Substring(1, AString.Length - 2);
          var dummy: TValue;
          TValue.Make(nil, GetArrayElType(Default.TypeInfo), dummy);
          var arr: TArray<TValue>;
          SetLength(arr, lst.Count);
          for var I := 0 to lst.Count - 1 do
            arr[I] := StringToValue(lst[I], dummy);
          Result := TValue.FromArray(Default.TypeInfo, arr);
        finally
          lst.Free;
        end;
      end;
    end;
    tkRecord,
    tkMRecord: begin
      if AString.StartsWith('(') and AString.EndsWith(')') then begin
        var lst := TStringList.Create();
        try
          lst.NameValueSeparator := ':';
          for var S in AString.Substring(1, AString.Length - 2).Split([';'], '"') do
            lst.Add(S);
          var context := TRttiContext.Create;
          var recType := context.GetType(Default.TypeInfo) as TRttiRecordType;
          for var field in recType.GetFields do begin
            var idx := lst.IndexOfName(field.Name);
            if idx >= 0 then begin
              var S := lst.ValueFromIndex[idx];
              if field.FieldType.TypeKind in [tkString, tkLString, tkWString, tkWideString, tkUString, tkChar, tkWChar, tkWideChar, tkShortString] then
                S := S.DeQuotedString('"');
              field.SetValue(Result.GetReferenceToRawData, StringToValue(S, field.GetValue(Default.GetReferenceToRawData)));
            end;
          end;
        finally
          lst.Free;
        end;
      end;
    end;
    tkWChar,
    tkChar: Result := TValue.From<Char>(AString.Chars[0]); { an empty string AString has already be handled at the start of this method }
    tkString,
    tkLString,
    tkWString,
    tkUString: Result := AString;
  else
    { tkClass, tkMethod, tkVariant, tkRecord, tkInterface, tkClassRef, tkPointer, tkProcedure }
    ErrorNotImplemented(GetTypeName(Default.TypeInfo));
  end;
end;

class function TDataStorage.ValueToString(const Value: TValue): string;
begin
  Result := Value.ToString; { ToString already handle most of the cases correct }
  case Value.Kind of
    tkFloat: begin { independent from current FormatSettings! }
      case Value.TypeData.FloatType of
        ftDouble: begin { handle special Double cases }
          if Value.TypeInfo = TypeInfo(TDate) then
            Result := DateToStr(Value.AsType<TDate>, TFormatSettings.Invariant)
          else if Value.TypeInfo = TypeInfo(TTime) then
            Result := TimeToStr(Value.AsType<TTime>, TFormatSettings.Invariant)
          else if Value.TypeInfo = TypeInfo(TDateTime) then
            Result := DateTimeToStr(Value.AsType<TDateTime>, TFormatSettings.Invariant)
          else
            Result := FloatToStr(Value.AsType<Double>, TFormatSettings.Invariant);
        end;
        ftSingle,
        ftExtended: begin
          Result := FloatToStr(Value.AsExtended, TFormatSettings.Invariant);
        end;
        ftComp: Result := IntToStr(Value.AsInt64);
        ftCurr: Result := CurrToStr(Value.AsCurrency, TFormatSettings.Invariant);
      end;
    end;
    tkArray,
    tkDynArray: begin
      var lst := TStringList.Create;
      try
        for var I := 0 to Value.GetArrayLength - 1 do begin
          lst.Add(ValueToString(Value.GetArrayElement(I)));
        end;
        Result := '[' + lst.CommaText + ']';
      finally
        lst.Free;
      end;
    end;
    tkRecord,
    tkMRecord: begin
      var lst := TStringList.Create;
      try
        lst.NameValueSeparator := ':';
        var context := TRttiContext.Create;
        var recType := context.GetType(Value.TypeInfo) as TRttiRecordType;
        for var field in recType.GetFields do begin
          var S := ValueToString(field.GetValue(Value.GetReferenceToRawData));
          if field.FieldType.TypeKind in [tkString, tkLString, tkWString, tkWideString, tkUString, tkChar, tkWChar, tkWideChar, tkShortString] then
            S := S.QuotedString('"');
          lst.AddPair(field.Name, S);
        end;
        Result := '(' + string.Join(';', lst.ToStringArray) + ')';
      finally
        lst.Free;
      end;
    end;
    tkEnumeration,
    tkSet,
    tkInteger,
    tkInt64,
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString: ; { Value.ToString already did the job }
  else
    { tkClass, tkMethod, tkVariant, tkRecord, tkInterface, tkClassRef, tkPointer, tkProcedure }
    ErrorNotImplemented(GetTypeName(Value.TypeInfo));
  end;
end;

procedure TDataStorage.WriteBoolean(const Ident: string; const Value: Boolean);
begin
  WriteInteger(Ident, cBool[Value]);
end;

procedure TDataStorage.WriteDateTime(const Ident: string; const Value: TDateTime);
begin
  WriteString(Ident, DateTimeToStr(Value, TFormatSettings.Invariant));
end;

procedure TDataStorage.WriteInstance(const Ident: string; Instance: TObject);
begin
  PushStorageKey(MakeStorageSubKey(Ident));
  try
    SaveToStorage(Instance);
  finally
    PopStorageKey;
  end;
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
  WriteString(Ident, ValueToString(Value));
end;

procedure TCustomStoredClass.Finalize;
begin
  SaveToStorage;
end;

function TCustomStoredClass.GetDataStorage: TDataStorage;
begin
  Result := TDataStorage.DefaultInstance;
end;

function TCustomStoredClass.GetTarget: TObject;
begin
  Result := Self;
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

procedure TCustomStoredClass.InternalInitDefaults;
var
  intf: IStoredData;
begin
  DataStorage.InitDefaults(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalInitDefaults(DataStorage);
end;

procedure TCustomStoredClass.InternalLoadFromStorage;
var
  intf: IStoredData;
begin
  DataStorage.LoadFromStorage(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalLoadFromStorage(DataStorage);
end;

procedure TCustomStoredClass.InternalPrepareStorage;
var
  intf: IStoredData;
begin
  if Supports(Target, IStoredData, intf) then
    intf.InternalPrepareStorage(DataStorage);
end;

procedure TCustomStoredClass.InternalSaveToStorage;
var
  intf: IStoredData;
begin
  DataStorage.SaveToStorage(Target);
  if Supports(Target, IStoredData, intf) then
    intf.InternalSaveToStorage(DataStorage);
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

function TStoredWrapper.GetTarget: TObject;
begin
  Result := FTarget;
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

constructor TCustomAutoStorageAttribute.Create(AVisibilities: TVisibilityClasses);
begin
  inherited Create;
  FVisibilities := AVisibilities;
end;

function TCustomAutoStorageAttribute.CheckVisibility(AVisibility: TMemberVisibility): Boolean;
begin
  Result := False;
  case AVisibility of
    mvPrivate  : Result := vcPrivate in FVisibilities;
    mvProtected: Result := vcProtected in FVisibilities;
    mvPublic   : Result := vcPublic in FVisibilities;
    mvPublished: Result := vcPublished in FVisibilities;
  end;
end;

initialization
  TDataStorage.AutoRegisterHandler := True;
end.
