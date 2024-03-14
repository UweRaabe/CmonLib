unit Cmon.DataSetHelper.Design;

interface

uses
  DesignIntf, DesignEditors;

type
  TDataSetHelperEditor = class(TSelectionEditor)
  public
    procedure ExecuteVerb(Index: Integer; const List: IDesignerSelections); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  ToolsAPI,
  Winapi.Windows,
  System.Win.Registry,
  System.SysUtils, System.Rtti,
  Data.DB,
  Vcl.Dialogs,
  Cmon.DataSetHelper.Generator, Cmon.DataSetHelper.Generator.Form, Cmon.DataSetHelper;

resourcestring
  SCreateMappings = 'Create mappings';
  STypesCopiedToClipboard = 'Types copied to clipboard:';

procedure Register;
begin
  RegisterSelectionEditor(TDataSet, TDataSetHelperEditor);
end;

type
  TCreateModeHelper = record helper for TCreateMode
  public
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    property AsString: string read GetAsString write SetAsString;
  end;

type
  TDBFieldsMappingHelper = record helper for TDBFieldsMapping
  public
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    property AsString: string read GetAsString write SetAsString;
  end;

type
  TNameDetectionHelper = record helper for TNameDetection
  public
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    property AsString: string read GetAsString write SetAsString;
  end;

type
  TGeneratorHelper = class helper for TDataSetHelperGenerator
  const
    cDataSetMappingsGenerator = 'DataSetMappingsGenerator';
    cCreateFields = 'CreateFields';
    cCreateMode = 'CreateMode';
    cMapMode = 'MapMode';
    cNameDetection = 'NameDetection';
    cPrefixes = 'Prefixes';
    cRegEx = 'RegEx';
    cUseNameConstants = 'UseNameConstants';
  public
    procedure LoadSettings;
    procedure SaveSettings;
  end;

procedure TGeneratorHelper.LoadSettings;
begin
  var BDSKey := (BorlandIDEServices.GetService(IOTAServices) as IOTAServices).GetBaseRegistryKey;
  var ini := TRegistryIniFile.Create(BDSKey);
  try
    var section := cDataSetMappingsGenerator;
    CreateFields := ini.ReadBool(section, cCreateFields, CreateFields);
    CreateMode.AsString := ini.ReadString(section, cCreateMode, CreateMode.AsString);
    MapMode.AsString := ini.ReadString(section, cMapMode, MapMode.AsString);
    NameDetection.AsString := ini.ReadString(section, cNameDetection, NameDetection.AsString);
    Prefixes := ini.ReadString(section, cPrefixes, Prefixes);
    RegEx := ini.ReadString(section, cRegEx, RegEx);
    UseNameConstants := ini.ReadBool(section, cUseNameConstants, UseNameConstants);
  finally
    ini.Free;
  end;
end;

procedure TGeneratorHelper.SaveSettings;
begin
  var BDSKey := (BorlandIDEServices.GetService(IOTAServices) as IOTAServices).GetBaseRegistryKey;
  var ini := TRegistryIniFile.Create(BDSKey);
  try
    var section := cDataSetMappingsGenerator;
    ini.WriteBool(section, cCreateFields, CreateFields);
    ini.WriteString(section, cCreateMode, CreateMode.AsString);
    ini.WriteString(section, cMapMode, MapMode.AsString);
    ini.WriteString(section, cNameDetection, NameDetection.AsString);
    ini.WriteString(section, cPrefixes, Prefixes);
    ini.WriteString(section, cRegEx, RegEx);
    ini.WriteBool(section, cUseNameConstants, UseNameConstants);
  finally
    ini.Free;
  end;
end;

procedure TDataSetHelperEditor.ExecuteVerb(Index: Integer; const List: IDesignerSelections);
begin
  var instance := TDataSetHelperGenerator.Create;
  try
    instance.LoadSettings;
    for var I := 0 to List.Count - 1 do begin
      var dataSet := List[I] as TDataSet;
      instance.AddDataSet(dataSet);
    end;
    if TMappingsGeneratorForm.Execute(instance) then begin
      instance.Execute;
      instance.SaveSettings;
      instance.CopyToClipboard;
      TaskMessageDlg(STypesCopiedToClipboard, instance.TypesCreated.Text, mtInformation, [mbOK], 0);
    end;
  finally
    instance.Free;
  end;
end;

function TDataSetHelperEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SCreateMappings;
  else
    Result := '';
  end;
end;

function TDataSetHelperEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TCreateModeHelper.GetAsString: string;
begin
  Result := TRttiEnumerationType.GetName(Self);
end;

procedure TCreateModeHelper.SetAsString(const Value: string);
begin
  Self := TRttiEnumerationType.GetValue<TCreateMode>(Value);
end;

function TDBFieldsMappingHelper.GetAsString: string;
begin
  Result := TRttiEnumerationType.GetName(Self);
end;

procedure TDBFieldsMappingHelper.SetAsString(const Value: string);
begin
  Self := TRttiEnumerationType.GetValue<TDBFieldsMapping>(Value);
end;

function TNameDetectionHelper.GetAsString: string;
begin
  Result := TRttiEnumerationType.GetName(Self);
end;

procedure TNameDetectionHelper.SetAsString(const Value: string);
begin
  Self := TRttiEnumerationType.GetValue<TNameDetection>(Value);
end;

end.
