unit Cmon.DataSetHelper.Generator;

interface

uses
  System.Classes, System.Generics.Collections,
  Data.DB,
  Cmon.DataSetHelper;

type
  TCreateMode = (cmRecord, cmClass);
  TNameDetection = (ndStripPrefix, ndRegEx);

type
  TDataSetHelperGenerator = class
  type
    TDataSets = TList<TDataSet>;
    TTypeNames = TDictionary<string, string>;
  private
    FBaseTypeName: string;
    FCode: TStringList;
    FCreateFields: Boolean;
    FCreateMode: TCreateMode;
    FDataSets: TDataSets;
    FIndent: Integer;
    FMapMode: TDBFieldsMapping;
    FNameDetection: TNameDetection;
    FPrefixes: string;
    FRegEx: string;
    FTypeNames: TTypeNames;
    FTypesCreated: TStrings;
    FUseNameConstants: Boolean;
    procedure SetNameDetection(const Value: TNameDetection);
    procedure SetPrefixes(const Value: string);
    procedure SetRegEx(const Value: string);
  protected
    procedure BeginIndent;
    procedure CreateMappingType(ADataSet: TDataSet);
    procedure CreateConstants(ADataSet: TDataSet);
    procedure CreateRecordFields(ADataSet: TDataSet);
    procedure EndIndent;
    function ExtractName(const AName: string): string;
    function ExtractTypeName(const ADataSetName: string): string;
    function GetTypeFromFieldType(AFieldType: TFieldType): string;
    procedure RefreshTypeNames;
    function StripPrefix(const AName: string): string;
    procedure WriteAttribute(const AFieldName: string);
    procedure WriteLine(const AText: string);
    procedure WriteMapMode;
    property BaseTypeName: string read FBaseTypeName write FBaseTypeName;
    property Code: TStringList read FCode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDataSet(ADataSet: TDataSet);
    procedure CopyToClipboard; virtual;
    procedure Execute;
    property CreateFields: Boolean read FCreateFields write FCreateFields;
    property CreateMode: TCreateMode read FCreateMode write FCreateMode;
    property DataSets: TDataSets read FDataSets;
    property MapMode: TDBFieldsMapping read FMapMode write FMapMode;
    property NameDetection: TNameDetection read FNameDetection write SetNameDetection;
    property Prefixes: string read FPrefixes write SetPrefixes;
    property RegEx: string read FRegEx write SetRegEx;
    property TypeNames: TTypeNames read FTypeNames;
    property TypesCreated: TStrings read FTypesCreated;
    property UseNameConstants: Boolean read FUseNameConstants write FUseNameConstants;
  end;

implementation

uses
  System.SysUtils, System.RegularExpressions,
  Vcl.Clipbrd;

constructor TDataSetHelperGenerator.Create;
begin
  inherited Create;
  FCode := TStringList.Create();
  FTypesCreated := TStringList.Create;
  FDataSets := TDataSets.Create();
  FTypeNames := TTypeNames.Create();
end;

destructor TDataSetHelperGenerator.Destroy;
begin
  FTypeNames.Free;
  FDataSets.Free;
  FTypesCreated.Free;
  FCode.Free;
  inherited Destroy;
end;

procedure TDataSetHelperGenerator.AddDataSet(ADataSet: TDataSet);
begin
  DataSets.Add(ADataSet);
  var dsName := ADataSet.Name;
  TypeNames.Add(dsName, ExtractTypeName(dsName));
end;

procedure TDataSetHelperGenerator.BeginIndent;
begin
  Inc(FIndent);
end;

procedure TDataSetHelperGenerator.CreateConstants(ADataSet: TDataSet);
begin
  WriteLine('public type');
  BeginIndent;
  WriteLine('Consts = record');
  WriteLine('public const');
  BeginIndent;
  for var fld in ADataSet.Fields do
    WriteLine(Format('%s = %s;', [fld.FieldName, fld.FieldName.QuotedString]));
  EndIndent;
  WriteLine('end;');
  EndIndent;
end;

procedure TDataSetHelperGenerator.CopyToClipboard;
begin
  Clipboard.Open;
  Clipboard.AsText := Code.Text;
  Clipboard.Close;
end;

procedure TDataSetHelperGenerator.CreateMappingType(ADataSet: TDataSet);
const
  cModes: array[TCreateMode] of string = ('record', 'class');
  cFieldFmt: array[TCreateMode] of string = ('%s: %s;', 'F%s: %s;');
begin
  WriteLine('type');
  BeginIndent;
  WriteMapMode;
  WriteLine(Format('%s = %s', [BaseTypeName, cModes[CreateMode]]));
  if UseNameConstants then
    CreateConstants(ADataSet);
  if CreateFields then
    CreateRecordFields(ADataSet);
  case CreateMode of
    cmRecord: WriteLine('public');
    cmClass: WriteLine('private');
  end;
  BeginIndent;
  for var fld in ADataSet.Fields do begin
    WriteAttribute(fld.FieldName);
    var typeName := GetTypeFromFieldType(fld.DataType);
    WriteLine(Format(cFieldFmt[CreateMode], [fld.FieldName, typeName]));
  end;
  EndIndent;
  if CreateMode = cmClass then begin
    WriteLine('public');
    BeginIndent;
    for var fld in ADataSet.Fields do begin
      var typeName := GetTypeFromFieldType(fld.DataType);
      WriteLine(Format('property %0:s: %1:s read F%0:s write F%0:s;', [fld.FieldName, typeName]));
    end;
    EndIndent;
  end;
  WriteLine('end;');
  if UseNameConstants then begin
    WriteLine(Format('%0:sConsts = %0:s.Consts;', [BaseTypeName]));
    TypesCreated.Add(BaseTypeName + 'Consts');
  end;
  if CreateFields then begin
    WriteLine(Format('%0:sFields = %0:s.Fields;', [BaseTypeName]));
    TypesCreated.Add(BaseTypeName + 'Fields');
  end;
  EndIndent;
  WriteLine('');
end;

procedure TDataSetHelperGenerator.CreateRecordFields(ADataSet: TDataSet);
begin
  WriteLine('public type');
  BeginIndent;
  WriteMapMode;
  WriteLine('Fields = class(TRecordFields)');
  WriteLine('private');
  BeginIndent;
  for var fld in ADataSet.Fields do begin
    WriteAttribute(fld.FieldName);
    WriteLine(Format('F%s: TField;', [fld.FieldName]));
  end;
  EndIndent;
  WriteLine('public');
  BeginIndent;
  for var fld in ADataSet.Fields do begin
    WriteLine(Format('property %0:s: TField read F%0:s;', [fld.FieldName]));
  end;
  EndIndent;
  WriteLine('end;');
  EndIndent;
end;

procedure TDataSetHelperGenerator.EndIndent;
begin
  Dec(FIndent);
end;

procedure TDataSetHelperGenerator.Execute;
begin
  for var dataSet in DataSets do begin
    var wasActive := dataSet.Active;
    try
      dataSet.Active := True;
      if not dataSet.Active then Exit;
      if dataSet.FieldCount = 0 then Exit;
      BaseTypeName := TypeNames[dataSet.Name];
      TypesCreated.Add(BaseTypeName);
      CreateMappingType(dataSet);
    finally
      dataSet.Active := wasActive;
    end;
  end;
end;

function TDataSetHelperGenerator.ExtractName(const AName: string): string;
begin
  var match := TRegEx.Match(AName, RegEx);
  if match.Success then
    Result := match.Value
  else
    Result := AName;
end;

function TDataSetHelperGenerator.ExtractTypeName(const ADataSetName: string): string;
begin
  case NameDetection of
    ndStripPrefix: Result := StripPrefix(ADataSetName);
    ndRegEx: Result := ExtractName(ADataSetName);
  else
    Result := ADataSetName;
  end;
  Result := 'T' + Result;
end;

function TDataSetHelperGenerator.GetTypeFromFieldType(AFieldType: TFieldType): string;
begin
  case AFieldType of
    ftString         : Result := 'string';
    ftSmallint       : Result := 'SmallInt';
    ftInteger        : Result := 'Integer';
    ftWord           : Result := 'Word';
    ftBoolean        : Result := 'Boolean';
    ftFloat          : Result := 'Double';
    ftCurrency       : Result := 'Currency';
    ftBCD            : Result := 'Double';
    ftDate           : Result := 'TDate';
    ftTime           : Result := 'TTime';
    ftDateTime       : Result := 'TDateTime';
    ftBytes          : Result := 'TBytes';
    ftVarBytes       : Result := 'TBytes';
    ftAutoInc        : Result := 'Integer';
    ftBlob           : Result := 'TBytes';
    ftMemo           : Result := 'string';
    ftGraphic        : Result := 'TBytes';
    ftFmtMemo        : Result := 'string';
    ftTypedBinary    : Result := 'TBytes';
    ftFixedChar      : Result := 'string';
    ftWideString     : Result := 'string';
    ftLargeint       : Result := 'LargeInt';
    ftVariant        : Result := 'Variant';
    ftGuid           : Result := 'TGUID';
    ftTimeStamp      : Result := 'TDateTime';
    ftFMTBcd         : Result := 'Double';
    ftFixedWideChar  : Result := 'string';
    ftWideMemo       : Result := 'string';
    ftLongWord       : Result := 'LongWord';
    ftShortint       : Result := 'ShortInt';
    ftByte           : Result := 'Byte';
    ftExtended       : Result := 'Extended';
    ftTimeStampOffset: Result := 'TDateTime';
    ftSingle         : Result := 'Single';
  else
    Result := '<unknown>';
  end;
end;

procedure TDataSetHelperGenerator.RefreshTypeNames;
begin
  for var dsName in TypeNames.Keys do
    TypeNames[dsName] := ExtractTypeName(dsName);
end;

procedure TDataSetHelperGenerator.SetNameDetection(const Value: TNameDetection);
begin
  if FNameDetection <> Value then
  begin
    FNameDetection := Value;
    RefreshTypeNames;
  end;
end;

procedure TDataSetHelperGenerator.SetPrefixes(const Value: string);
begin
  if FPrefixes <> Value then
  begin
    FPrefixes := Value;
    if NameDetection = ndStripPrefix then
      RefreshTypeNames;
  end;
end;

procedure TDataSetHelperGenerator.SetRegEx(const Value: string);
begin
  if FRegEx <> Value then
  begin
    FRegEx := Value;
    if NameDetection = ndRegEx then
      RefreshTypeNames;
  end;
end;

function TDataSetHelperGenerator.StripPrefix(const AName: string): string;
begin
  Result := AName;
  var lst := TStringList.Create;
  try
    lst.CommaText := Prefixes;
    for var pre in lst do begin
      if Result.StartsWith(pre, True) then begin
        Result := Result.Substring(pre.Length);
        Break;
      end;
    end;
  finally
    lst.Free;
  end;
end;

procedure TDataSetHelperGenerator.WriteAttribute(const AFieldName: string);
begin
  if MapMode = mapManual then begin
    var fldName := AFieldName;
    if UseNameConstants then
      fldName := 'Consts.' + fldName
    else
      fldName := fldName.QuotedString;
    WriteLine(Format('[DBField(%s)]', [fldName]));
  end;
end;

procedure TDataSetHelperGenerator.WriteLine(const AText: string);
begin
  Code.Add(string.Create(' ', FIndent*2) + AText);
end;

procedure TDataSetHelperGenerator.WriteMapMode;
const
  cMapMode: array[TDBFieldsMapping] of string = ('mapAuto', 'mapManual');
begin
  WriteLine(Format('[DBFields(%s)]', [cMapMode[MapMode]]));
end;

end.
