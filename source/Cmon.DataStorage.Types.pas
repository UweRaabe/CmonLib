unit Cmon.DataStorage.Types;

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
  TCustomStorageAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string = '');
    property Name: string read FName;
  end;

type
  TCustomVisibilitiesAttribute = class(TCustomAttribute)
  private
    FVisibilities: TVisibilityClasses;
  public
    constructor Create(AVisibilities: TVisibilityClasses);
    function CheckVisibility(AVisibility: TMemberVisibility): Boolean;
  end;

type
  TCustomStorageKeyAttribute = class(TCustomAttribute)
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
    procedure ReadKey(const Key: string; Target: TStrings);
    function ReadString(const Key, Ident, Default: string): string;
    function ValueExists(const Key, Ident: string): Boolean;
    procedure WriteString(const Key, Ident, Value: string);
  end;

  IStorageTargetExt = interface(IStorageTarget)
  ['{6C9F0927-5705-41FE-9992-4AD984DE42C2}']
    function GetFormatSettings: TFormatSettings;
    function GetReadOnly: Boolean;
    function ReadBoolean(const Key, Ident: string; const Default: Boolean): Boolean;
    function ReadDateTime(const Key, Ident: string; const Default: TDateTime): TDateTime;
    function ReadFloat(const Key, Ident: string; const Default: Double): Double;
    function ReadInteger(const Key, Ident: string; const Default: Integer): Integer;
    procedure ReadStrings(const Key, Ident: string; Target: TStrings);
    function ReadValue(const Key, Ident: string; const Default: TValue): TValue;
    procedure SetFormatSettings(const Value: TFormatSettings);
    procedure SetReadOnly(const Value: Boolean);
    procedure WriteBoolean(const Key, Ident: string; const Value: Boolean);
    procedure WriteDateTime(const Key, Ident: string; const Value: TDateTime);
    procedure WriteFloat(const Key, Ident: string; const Value: Double);
    procedure WriteInteger(const Key, Ident: string; const Value: Integer);
    procedure WriteStrings(const Key, Ident: string; Source: TStrings);
    procedure WriteValue(const Key, Ident: string; const Value: TValue);
    property FormatSettings: TFormatSettings read GetFormatSettings write SetFormatSettings;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
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
  { TValue <-> string conversion }
  TValueConverter = class
  private
    FFormatSettings: TFormatSettings;
  strict protected
    procedure ErrorNotImplemented(const ATypeName: string); virtual;
  protected
    function RemoveIsoTimePart(const AString: string): string;
  public
    function ConvertStringToDate(const AString: string; const ADefault: TDate): TDate; virtual;
    function ConvertStringToDateTime(const AString: string; const ADefault: TDateTime): TDateTime; virtual;
    function ConvertStringToTime(const AString: string; const ADefault: TTime): TTime; virtual;
    function ConvertToString(const ADate: TDate): string; overload; virtual;
    function ConvertToString(const ADateTime: TDateTime): string; overload; virtual;
    function ConvertToString(const ATime: TTime): string; overload; virtual;
    function StringToValue(const AString: string; const Default: TValue): TValue; virtual;
    function ValueToString(const Value: TValue): string; virtual;
    property FormatSettings: TFormatSettings read FFormatSettings write FFormatSettings;
  end;

type
  TAbstractStorageTarget = class abstract(TInterfacedObject, IStorageTarget, IStorageTargetExt)
  const
    cBool: array[Boolean] of Integer = (0, 1);
  private
    FConverter: TValueConverter;
    FFormatSettings: TFormatSettings;
    FReadOnly: Boolean;
    function GetConverter: TValueConverter;
    function GetFormatSettings: TFormatSettings;
    function GetReadOnly: Boolean;
    procedure SetFormatSettings(const Value: TFormatSettings);
    procedure SetReadOnly(const Value: Boolean);
  strict protected
    function CreateConverter: TValueConverter; virtual;
    procedure DeleteKey(const Key, Ident: string); virtual; abstract;
    procedure EraseStorageKey(const Key: string); virtual; abstract;
    procedure ReadOnlyChanged; virtual;
    procedure ReadKey(const Key: string; Target: TStrings); virtual; abstract;
    function ReadBoolean(const Key, Ident: string; const Default: Boolean): Boolean; virtual;
    function ReadDateTime(const Key, Ident: string; const Default: TDateTime): TDateTime; virtual;
    function ReadFloat(const Key, Ident: string; const Default: Double): Double; virtual;
    function ReadInteger(const Key, Ident: string; const Default: Integer): Integer; virtual;
    function ReadString(const Key, Ident, Default: string): string; virtual; abstract;
    procedure ReadStrings(const Key, Ident: string; Target: TStrings); virtual;
    function ReadValue(const Key, Ident: string; const Default: TValue): TValue; virtual;
    procedure WriteBoolean(const Key, Ident: string; const Value: Boolean); virtual;
    procedure WriteDateTime(const Key, Ident: string; const Value: TDateTime); virtual;
    procedure WriteFloat(const Key, Ident: string; const Value: Double); virtual;
    procedure WriteInteger(const Key, Ident: string; const Value: Integer); virtual;
    procedure WriteString(const Key, Ident, Value: string); virtual; abstract;
    procedure WriteStrings(const Key, Ident: string; Source: TStrings); virtual;
    procedure WriteValue(const Key, Ident: string; const Value: TValue); virtual;
    function ValueExists(const Key, Ident: string): Boolean; virtual; abstract;
  protected
    property Converter: TValueConverter read GetConverter;
  public
    constructor Create;
    destructor Destroy; override;
    property FormatSettings: TFormatSettings read GetFormatSettings write SetFormatSettings;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

implementation

uses
  System.DateUtils;

function StringToDate(const AString: string; const ADefault: TDate): TDate;
begin
  Result := ADefault;
  if not AString.IsEmpty then begin
    var dt: TDateTime;
    if TryISO8601ToDate(AString, dt) then begin
      if not TryStrToDate(AString, dt, TFormatSettings.Invariant) then
        dt := StrToDateDef(AString, ADefault);
    end;
    Result := DateOf(dt);
  end;
end;

function StringToDateTime(const AString: string; const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
  if not AString.IsEmpty then begin
    if not TryISO8601ToDate(AString, Result) then begin
      if not TryStrToDateTime(AString, Result, TFormatSettings.Invariant) then
        Result := StrToDateTimeDef(AString, ADefault);
    end;
  end;
end;

function StringToTime(const AString: string; const ADefault: TTime): TTime;
begin
  Result := ADefault;
  if not AString.IsEmpty then begin
    var dt: TDateTime;
    { ISO8601 time only is not supported }
    if not TryStrToTime(AString, dt, TFormatSettings.Invariant) then
      dt := StrToTimeDef(AString, ADefault);
    Result := TimeOf(dt);
  end;
end;

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

constructor TCustomVisibilitiesAttribute.Create(AVisibilities: TVisibilityClasses);
begin
  inherited Create;
  FVisibilities := AVisibilities;
end;

function TCustomVisibilitiesAttribute.CheckVisibility(AVisibility: TMemberVisibility): Boolean;
begin
  Result := False;
  case AVisibility of
    mvPrivate  : Result := vcPrivate in FVisibilities;
    mvProtected: Result := vcProtected in FVisibilities;
    mvPublic   : Result := vcPublic in FVisibilities;
    mvPublished: Result := vcPublished in FVisibilities;
  end;
end;

constructor TCustomStorageKeyAttribute.Create(const AKey: string);
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

constructor TAbstractStorageTarget.Create;
begin
  inherited Create;
  FFormatSettings := TFormatSettings.Invariant;
end;

destructor TAbstractStorageTarget.Destroy;
begin
  FConverter.Free;
  inherited Destroy;
end;

function TAbstractStorageTarget.CreateConverter: TValueConverter;
begin
  Result := TValueConverter.Create;
end;

function TAbstractStorageTarget.GetConverter: TValueConverter;
begin
  if FConverter = nil then begin
    FConverter := CreateConverter;
    FConverter.FormatSettings := FormatSettings;
  end;
  Result := FConverter;
end;

function TAbstractStorageTarget.GetFormatSettings: TFormatSettings;
begin
  Result := FFormatSettings;
end;

function TAbstractStorageTarget.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TAbstractStorageTarget.ReadOnlyChanged;
begin
end;

function TAbstractStorageTarget.ReadBoolean(const Key, Ident: string; const Default: Boolean): Boolean;
begin
  var def := TValue.From(Default);
  Result := Converter.StringToValue(ReadString(Key, Ident, Converter.ValueToString(def)), def).AsBoolean;
end;

function TAbstractStorageTarget.ReadDateTime(const Key, Ident: string; const Default: TDateTime): TDateTime;
begin
  var S := ReadString(Key, Ident, '');
  Result := Converter.ConvertStringToDateTime(S, Default);
end;

function TAbstractStorageTarget.ReadFloat(const Key, Ident: string; const Default: Double): Double;
begin
  Result := Default;
  var S := ReadString(Key, Ident, '');
  if not S.IsEmpty then
    Result := StrToFloatDef(S, Default, FormatSettings);
end;

function TAbstractStorageTarget.ReadInteger(const Key, Ident: string; const Default: Integer): Integer;
var
  S: string;
begin
  S := ReadString(Key, Ident, '');
  if (S.Length > 2) and (S.StartsWith('0x',true)) then
    S := '$' + S.Substring(2);
  Result := StrToIntDef(S, Default);
end;

procedure TAbstractStorageTarget.ReadStrings(const Key, Ident: string; Target: TStrings);
begin
  Target.CommaText := ReadString(Key, Ident, Target.CommaText);
end;

function TAbstractStorageTarget.ReadValue(const Key, Ident: string; const Default: TValue): TValue;
var
  S: string;
begin
  case Default.Kind of
    tkEnumeration: begin
      if Default.TypeInfo = TypeInfo(Boolean) then
        Exit(ReadBoolean(Key, Ident, Default.AsBoolean));
    end;
    tkInteger: begin
      Exit(ReadInteger(Key, Ident, Default.AsInteger));
    end;
    tkFloat: begin
      case Default.TypeData.FloatType of
        ftDouble: begin
          { handle special Double cases }
          if Default.TypeInfo = TypeInfo(TDate) then
            Exit(TDate(ReadDateTime(Key, Ident, Default.AsType<TDate>)))
          else if Default.TypeInfo = TypeInfo(TTime) then
            Exit(TTime(ReadDateTime(Key, Ident, Default.AsType<TTime>)))
          else if Default.TypeInfo = TypeInfo(TDateTime) then
            Exit(ReadDateTime(Key, Ident, Default.AsType<TTime>))
          else
            Exit(ReadFloat(Key, Ident, Default.AsType<Double>))
        end;
        ftSingle: begin
          Exit(Single(ReadFloat(Key, Ident, Default.AsType<Double>)));
        end;
        ftExtended: begin
          Exit(Extended(ReadFloat(Key, Ident, Default.AsExtended)));
        end;
      end;
    end;
    tkRecord: begin
      if Default.TypeInfo = TypeInfo(TValue) then begin
        Exit(ReadValue(Key, Ident, Default.AsType<TValue>));
      end;
    end;
  end;
  S := ReadString(Key, Ident, '');
  if S = '' then
    Exit(Default);
  Result := Converter.StringToValue(S, Default);
end;

procedure TAbstractStorageTarget.SetFormatSettings(const Value: TFormatSettings);
begin
  FFormatSettings := Value;
  if FConverter <> nil then
    FConverter.FormatSettings := FFormatSettings;
end;

procedure TAbstractStorageTarget.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    ReadOnlyChanged;
  end;
end;

procedure TAbstractStorageTarget.WriteBoolean(const Key, Ident: string; const Value: Boolean);
begin
  WriteInteger(Key, Ident, cBool[Value]);
end;

procedure TAbstractStorageTarget.WriteDateTime(const Key, Ident: string; const Value: TDateTime);
begin
  WriteString(Key, Ident, Converter.ConvertToString(Value));
end;

procedure TAbstractStorageTarget.WriteFloat(const Key, Ident: string; const Value: Double);
begin
  WriteString(Key, Ident, FloatToStr(Value, FormatSettings));
end;

procedure TAbstractStorageTarget.WriteInteger(const Key, Ident: string; const Value: Integer);
begin
  WriteString(Key, Ident, IntToStr(Value));
end;

procedure TAbstractStorageTarget.WriteStrings(const Key, Ident: string; Source: TStrings);
begin
  WriteString(Key, Ident, Source.CommaText);
end;

procedure TAbstractStorageTarget.WriteValue(const Key, Ident: string; const Value: TValue);
begin
  case Value.Kind of
    tkEnumeration: begin
      if Value.TypeInfo = TypeInfo(Boolean) then begin
        WriteBoolean(Key, Ident, Value.AsBoolean);
        Exit;
      end;
    end;
    tkInteger: begin
      WriteInteger(Key, Ident, Value.AsInteger);
      Exit;
    end;
    tkFloat: begin
      if Value.TypeData.FloatType = ftDouble then begin
        { handle special Double cases }
        if Value.TypeInfo = TypeInfo(TDate) then
          WriteDateTime(Key, Ident, Value.AsType<TDate>)
        else if Value.TypeInfo = TypeInfo(TTime) then
          WriteDateTime(Key, Ident, Value.AsType<TTime>)
        else if Value.TypeInfo = TypeInfo(TDateTime) then
          WriteDateTime(Key, Ident, Value.AsType<TDateTime>)
        else
          WriteFloat(Key, Ident, Value.AsType<Double>);
        Exit;
      end
      else if Value.TypeData.FloatType in [ftSingle, ftExtended] then begin
        WriteFloat(Key, Ident, Value.AsExtended);
        Exit;
      end;
    end;
    tkRecord: begin
      if Value.TypeInfo = TypeInfo(TValue) then begin
        WriteValue(Key, Ident, Value.AsType<TValue>);
        Exit;
      end;
    end;
  end;
  WriteString(Key, Ident, Converter.ValueToString(Value));
end;

procedure TValueConverter.ErrorNotImplemented(const ATypeName: string);
begin
  raise ENotImplemented.CreateFmt('Storage type "%s" not supported!', [ATypeName]);
end;

function TValueConverter.ConvertStringToDate(const AString: string; const ADefault: TDate): TDate;
begin
  Result := ADefault;
  if not AString.IsEmpty then begin
    var dt: TDateTime;
    if TryISO8601ToDate(AString, dt) then begin
      if not TryStrToDate(AString, dt, TFormatSettings.Invariant) then
        dt := StrToDateDef(AString, ADefault);
    end;
    Result := DateOf(dt);
  end;
end;

function TValueConverter.ConvertStringToDateTime(const AString: string; const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
  if not AString.IsEmpty then begin
    if not TryISO8601ToDate(AString, Result) then begin
      if not TryStrToDateTime(AString, Result, TFormatSettings.Invariant) then
        Result := StrToDateTimeDef(AString, ADefault);
    end;
  end;
end;

function TValueConverter.ConvertStringToTime(const AString: string; const ADefault: TTime): TTime;
begin
  Result := ADefault;
  if not AString.IsEmpty then begin
    var dt: TDateTime;
    { ISO8601 time only is not supported }
    if not TryStrToTime(AString, dt, TFormatSettings.Invariant) then
      dt := StrToTimeDef(AString, ADefault);
    Result := TimeOf(dt);
  end;
end;

function TValueConverter.ConvertToString(const ADate: TDate): string;
begin
  Result := RemoveIsoTimePart(DateToISO8601(ADate));
end;

function TValueConverter.ConvertToString(const ADateTime: TDateTime): string;
const
  cZeroZulu = 'T00:00:00.000Z';
begin
  Result := DateToISO8601(ADateTime);
  if Result.EndsWith(cZeroZulu) then
    Result := RemoveIsoTimePart(Result);
end;

function TValueConverter.ConvertToString(const ATime: TTime): string;
begin
  Result := TimeToStr(ATime, TFormatSettings.Invariant);
end;

function TValueConverter.RemoveIsoTimePart(const AString: string): string;
begin
  Result := AString;
  var timeSep := Result.IndexOf('T');
  if timeSep > 0 then
    Result := Result.Remove(timeSep);
end;

function TValueConverter.StringToValue(const AString: string; const Default: TValue): TValue;
begin
  Result := Default;

  case Default.Kind of
    tkEnumeration: begin
      var valInt64: Int64;
      if TryStrToInt64(AString, valInt64) then begin
        if Default.TypeInfo = TypeInfo(Boolean) then
          Result := (valInt64 <> 0)
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
            Result := TValue.From<TDate>(ConvertStringToDate(AString, Default.AsType<TDate>))
          else if Default.TypeInfo = TypeInfo(TTime) then
            Result := TValue.From<TTime>(ConvertStringToTime(AString, Default.AsType<TTime>))
          else if Default.TypeInfo = TypeInfo(TDateTime) then
            Result := TValue.From<TDateTime>(ConvertStringToDateTime(AString, Default.AsType<TDateTime>))
          else
            Result := TValue.From<Double>(StrToFloatDef(AString, Default.AsType<Double>, FormatSettings));
        end;
        ftSingle: Result := TValue.From<Single>(StrToFloatDef(AString, Default.AsType<Single>, FormatSettings));
        ftExtended: Result := TValue.From<Extended>(StrToFloatDef(AString, Default.AsType<Extended>, FormatSettings));
        ftComp: Result := TValue.From<Comp>(StrToInt64Def(AString, Default.AsInt64));
        ftCurr: Result := TValue.From<Currency>(StrToCurrDef(AString, Default.AsCurrency, FormatSettings));
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
    { tkClass, tkMethod, tkVariant, tkInterface, tkClassRef, tkPointer, tkProcedure }
    ErrorNotImplemented(GetTypeName(Default.TypeInfo));
  end;
end;

function TValueConverter.ValueToString(const Value: TValue): string;
begin
  Result := Value.ToString; { ToString already handle most of the cases correct }
  case Value.Kind of
    tkFloat: begin { independent from current FormatSettings! }
      case Value.TypeData.FloatType of
        ftDouble: begin { handle special Double cases }
          if Value.TypeInfo = TypeInfo(TDate) then
            Result := ConvertToString(Value.AsType<TDate>)
          else if Value.TypeInfo = TypeInfo(TTime) then
            Result := ConvertToString(Value.AsType<TTime>)
          else if Value.TypeInfo = TypeInfo(TDateTime) then
            Result := ConvertToString(Value.AsType<TDateTime>)
          else
            Result := FloatToStr(Value.AsType<Double>, FormatSettings);
        end;
        ftSingle,
        ftExtended: begin
          Result := FloatToStr(Value.AsExtended, FormatSettings);
        end;
        ftComp: Result := IntToStr(Value.AsInt64);
        ftCurr: Result := CurrToStr(Value.AsCurrency, FormatSettings);
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
    { tkClass, tkMethod, tkVariant, tkInterface, tkClassRef, tkPointer, tkProcedure }
    ErrorNotImplemented(GetTypeName(Value.TypeInfo));
  end;
end;

end.
