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
  private
    FBaseTypeName: string;
    FCode: TStringList;
    FCreateFields: Boolean;
    FCreateMode: TCreateMode;
    FDataSets: TList<TDataSet>;
    FIndent: Integer;
    FMapMode: TDBFieldsMapping;
    FNameDetection: TNameDetection;
    FPrefixes: string;
    FRegEx: string;
    FTypesCreated: TStrings;
    FUseNameConstants: Boolean;
  protected
    procedure BeginIndent;
    procedure CreateMappingType(ADataSet: TDataSet);
    procedure CreateConstants(ADataSet: TDataSet);
    procedure CreateRecordFields(ADataSet: TDataSet);
    procedure EndIndent;
    function ExtractName(const AName: string): string;
    function ExtractTypeName(ADataSet: TDataSet): string;
    function GetTypeFromFieldType(AFieldType: TFieldType): string;
    function StripPrefix(const AName: string): string;
    procedure WriteAttribute(const AFieldName: string);
    procedure WriteLine(const AText: string);
    procedure WriteMapMode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CopyToClipboard; virtual;
    procedure Execute;
    property BaseTypeName: string read FBaseTypeName write FBaseTypeName;
    property Code: TStringList read FCode;
    property CreateFields: Boolean read FCreateFields write FCreateFields;
    property CreateMode: TCreateMode read FCreateMode write FCreateMode;
    property DataSets: TList<TDataSet> read FDataSets;
    property MapMode: TDBFieldsMapping read FMapMode write FMapMode;
    property NameDetection: TNameDetection read FNameDetection write FNameDetection;
    property Prefixes: string read FPrefixes write FPrefixes;
    property RegEx: string read FRegEx write FRegEx;
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
  FDataSets := TList<TDataSet>.Create();
end;

destructor TDataSetHelperGenerator.Destroy;
begin
  FDataSets.Free;
  FTypesCreated.Free;
  FCode.Free;
  inherited Destroy;
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
  for var dataset in DataSets do begin
    var wasActive := dataSet.Active;
    try
      dataSet.Active := True;
      if not dataSet.Active then Exit;
      if dataSet.FieldCount = 0 then Exit;
      BaseTypeName := ExtractTypeName(dataSet);
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

function TDataSetHelperGenerator.ExtractTypeName(ADataSet: TDataSet): string;
begin
  Result := ADataSet.Name;
  case NameDetection of
    ndStripPrefix: Result := StripPrefix(ADataSet.Name);
    ndRegEx: Result := ExtractName(ADataSet.Name);
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

function TDataSetHelperGenerator.StripPrefix(const AName: string): string;
begin
  Result := AName;
  for var pre in ['qry', 'qu', 'tbl', 'tb'] do begin
    if Result.StartsWith(pre, True) then begin
      Result := Result.Substring(pre.Length);
      Break;
    end;
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
