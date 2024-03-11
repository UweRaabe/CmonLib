unit Cmon.DataSetHelper.Generator;

interface

uses
  System.Classes,
  Data.DB;

type
  TDataSetHelperGenerator = class
  private
    FCode: TStringList;
    FTypesCreated: TStrings;
  protected
    function ExtractTypeName(ADataSet: TDataSet): string;
    function GetTypeFromFieldType(AFieldType: TFieldType): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CopyToClipboard; virtual;
    procedure CreateAccessClass(ADataSet: TDataSet);
    procedure CreateAccessRecord(ADataSet: TDataSet);
    procedure CreateRecordFields(ADataSet: TDataSet);
    property Code: TStringList read FCode;
    property TypesCreated: TStrings read FTypesCreated;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Clipbrd;

constructor TDataSetHelperGenerator.Create;
begin
  inherited Create;
  FCode := TStringList.Create();
  FTypesCreated := TStringList.Create;
end;

destructor TDataSetHelperGenerator.Destroy;
begin
  FTypesCreated.Free;
  FCode.Free;
  inherited Destroy;
end;

procedure TDataSetHelperGenerator.CopyToClipboard;
begin
  Clipboard.Open;
  Clipboard.AsText := Code.Text;
  Clipboard.Close;
end;

procedure TDataSetHelperGenerator.CreateAccessClass(ADataSet: TDataSet);
begin
  if not ADataSet.Active then Exit;
  if ADataSet.FieldCount = 0 then Exit;
  var typeName := ExtractTypeName(ADataSet);
  TypesCreated.Add(typename);
  Code.Add('type');
  Code.Add(Format('  %s = class', [typeName]));
  Code.Add('  private');
  for var fld in ADataSet.Fields do begin
    typeName := GetTypeFromFieldType(fld.DataType);
    Code.Add(Format('    F%s: %s;', [fld.FieldName, typeName]));
  end;
  Code.Add('  public');
  for var fld in ADataSet.Fields do begin
    typeName := GetTypeFromFieldType(fld.DataType);
    Code.Add(Format('    property %0:s: %1:s read F%0:s write F%0:s;', [fld.FieldName, typeName]));
  end;
  Code.Add('  end;');
  Code.Add('');
end;

procedure TDataSetHelperGenerator.CreateAccessRecord(ADataSet: TDataSet);
begin
  if not ADataSet.Active then Exit;
  if ADataSet.FieldCount = 0 then Exit;
  var typeName := ExtractTypeName(ADataSet);
  TypesCreated.Add(typename);
  Code.Add('type');
  Code.Add(Format('  %s = record', [typeName]));
  for var fld in ADataSet.Fields do begin
    typeName := GetTypeFromFieldType(fld.DataType);
    Code.Add(Format('    %s: %s;', [fld.FieldName, typeName]));
  end;
  Code.Add('  end;');
  Code.Add('');
end;

procedure TDataSetHelperGenerator.CreateRecordFields(ADataSet: TDataSet);
begin
  if not ADataSet.Active then Exit;
  if ADataSet.FieldCount = 0 then Exit;
  var typeName := ExtractTypeName(ADataSet) + 'Fields';
  TypesCreated.Add(typename);
  Code.Add('type');
  Code.Add(Format('  %s = class(TRecordFields)', [typeName]));
  Code.Add('  private');
  for var fld in ADataSet.Fields do begin
    Code.Add(Format('    F%s: TField;', [fld.FieldName]));
  end;
  Code.Add('  public');
  for var fld in ADataSet.Fields do begin
    Code.Add(Format('    property %0:s: TField read F%0:s;', [fld.FieldName]));
  end;
  Code.Add('  end;');
  Code.Add('');
end;

function TDataSetHelperGenerator.ExtractTypeName(ADataSet: TDataSet): string;
begin
  Result := ADataSet.Name;
  for var pre in ['qry', 'qu', 'tbl', 'tb'] do begin
    if Result.StartsWith(pre, True) then begin
      Result := Result.Substring(pre.Length);
      Break;
    end;
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

end.
