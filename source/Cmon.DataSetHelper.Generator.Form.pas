unit Cmon.DataSetHelper.Generator.Form;

interface

uses
  System.Classes, System.Actions,
  Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ValEdit,
  Cmon.DataSetHelper.Generator;

type
  TMappingsGeneratorForm = class(TForm)
    pnlMain: TPanel;
    grpCreateMappings: TGroupBox;
    selCreateRecord: TRadioButton;
    selCreateClass: TRadioButton;
    MainActionList: TActionList;
    actCreateRecord: TAction;
    actCreateClass: TAction;
    actCreateFields: TAction;
    grpOptions: TGroupBox;
    selUseNameConstants: TCheckBox;
    actMapAuto: TAction;
    actMapManual: TAction;
    actUseNameConstants: TAction;
    selMapAuto: TRadioButton;
    selMapManual: TRadioButton;
    grpTypeName: TGroupBox;
    selStripPrefix: TRadioButton;
    selUseRegEx: TRadioButton;
    actStripPrefix: TAction;
    actUseRegex: TAction;
    edtPrefixes: TEdit;
    edtRegex: TEdit;
    selCreateFields: TCheckBox;
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    actOK: TAction;
    actCancel: TAction;
    tkey: TRadioGroup;
    pnlTop: TPanel;
    edtTypeNames: TValueListEditor;
    selSorted: TCheckBox;
    actSorted: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actCreateClassExecute(Sender: TObject);
    procedure actCreateClassUpdate(Sender: TObject);
    procedure actCreateFieldsExecute(Sender: TObject);
    procedure actCreateFieldsUpdate(Sender: TObject);
    procedure actCreateRecordExecute(Sender: TObject);
    procedure actCreateRecordUpdate(Sender: TObject);
    procedure actMapAutoExecute(Sender: TObject);
    procedure actMapAutoUpdate(Sender: TObject);
    procedure actMapManualExecute(Sender: TObject);
    procedure actMapManualUpdate(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actSortedExecute(Sender: TObject);
    procedure actSortedUpdate(Sender: TObject);
    procedure actStripPrefixExecute(Sender: TObject);
    procedure actStripPrefixUpdate(Sender: TObject);
    procedure actUseNameConstantsExecute(Sender: TObject);
    procedure actUseNameConstantsUpdate(Sender: TObject);
    procedure actUseRegexExecute(Sender: TObject);
    procedure actUseRegexUpdate(Sender: TObject);
  private
    FGenerator: TDataSetHelperGenerator;
    procedure SetGenerator(const Value: TDataSetHelperGenerator);
  protected
    procedure LoadTypeNames;
    procedure SaveTypeNames;
  public
    class function Execute(AGenerator: TDataSetHelperGenerator): Boolean;
    procedure LoadValues;
    procedure SaveValues;
    property Generator: TDataSetHelperGenerator read FGenerator write SetGenerator;
  end;

implementation

uses
  System.Generics.Collections, System.SysUtils,
  Cmon.DataSetHelper, Cmon.Observers.Vcl;

{$R *.dfm}

procedure TMappingsGeneratorForm.FormCreate(Sender: TObject);
begin
  edtPrefixes.AddObserver(
    procedure (Value: string)
    begin
      Generator.Prefixes := Value;
      if Generator.NameDetection = ndStripPrefix then
        LoadTypeNames;
    end);
  edtRegex.AddObserver(
    procedure (Value: string)
    begin
      Generator.RegEx := Value;
      if Generator.NameDetection = ndRegEx then
        LoadTypeNames;
    end);
end;

procedure TMappingsGeneratorForm.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TMappingsGeneratorForm.actCreateClassExecute(Sender: TObject);
begin
  Generator.CreateMode := cmClass;
end;

procedure TMappingsGeneratorForm.actCreateClassUpdate(Sender: TObject);
begin
  actCreateClass.Checked := (Generator.CreateMode = cmClass);
end;

procedure TMappingsGeneratorForm.actCreateFieldsExecute(Sender: TObject);
begin
  Generator.CreateFields := not Generator.CreateFields;
end;

procedure TMappingsGeneratorForm.actCreateFieldsUpdate(Sender: TObject);
begin
  actCreateFields.Checked := Generator.CreateFields;
end;

procedure TMappingsGeneratorForm.actCreateRecordExecute(Sender: TObject);
begin
  Generator.CreateMode := cmClass;
end;

procedure TMappingsGeneratorForm.actCreateRecordUpdate(Sender: TObject);
begin
  actCreateRecord.Checked := (Generator.CreateMode = cmRecord);
end;

procedure TMappingsGeneratorForm.actMapAutoExecute(Sender: TObject);
begin
  Generator.MapMode := mapAuto;
end;

procedure TMappingsGeneratorForm.actMapAutoUpdate(Sender: TObject);
begin
  actMapAuto.Checked := (Generator.MapMode = mapAuto);
end;

procedure TMappingsGeneratorForm.actMapManualExecute(Sender: TObject);
begin
  Generator.MapMode := mapManual;
end;

procedure TMappingsGeneratorForm.actMapManualUpdate(Sender: TObject);
begin
  actMapManual.Checked := (Generator.MapMode = mapManual);
end;

procedure TMappingsGeneratorForm.actOKExecute(Sender: TObject);
begin
  SaveValues;
  ModalResult := mrOK;
end;

procedure TMappingsGeneratorForm.actSortedExecute(Sender: TObject);
begin
  Generator.CreateSorted := not Generator.CreateSorted;
end;

procedure TMappingsGeneratorForm.actSortedUpdate(Sender: TObject);
begin
  actSorted.Checked := Generator.CreateSorted;
end;

procedure TMappingsGeneratorForm.actStripPrefixExecute(Sender: TObject);
begin
  if Generator.NameDetection <> ndStripPrefix then begin
    Generator.NameDetection := ndStripPrefix;
    LoadTypeNames;
  end;
end;

procedure TMappingsGeneratorForm.actStripPrefixUpdate(Sender: TObject);
begin
  actStripPrefix.Checked := (Generator.NameDetection = ndStripPrefix);
end;

procedure TMappingsGeneratorForm.actUseNameConstantsExecute(Sender: TObject);
begin
  Generator.UseNameConstants := not Generator.UseNameConstants;
end;

procedure TMappingsGeneratorForm.actUseNameConstantsUpdate(Sender: TObject);
begin
  actUseNameConstants.Checked := Generator.UseNameConstants;
end;

procedure TMappingsGeneratorForm.actUseRegexExecute(Sender: TObject);
begin
  if Generator.NameDetection <> ndRegEx then begin
    Generator.NameDetection := ndRegEx;
    LoadTypeNames;
  end;
end;

procedure TMappingsGeneratorForm.actUseRegexUpdate(Sender: TObject);
begin
  actUseRegex.Checked := (Generator.NameDetection = ndRegEx);
  actUseRegex.Enabled := not Generator.RegEx.IsEmpty;
end;

class function TMappingsGeneratorForm.Execute(AGenerator: TDataSetHelperGenerator): Boolean;
begin
  var instance := Self.Create(nil);
  try
    instance.Generator := AGenerator;
    Result := (instance.ShowModal = mrOk);
  finally
    instance.Free;
  end;
end;

procedure TMappingsGeneratorForm.LoadTypeNames;
begin
  edtTypeNames.BeginUpdate;
  try
    edtTypeNames.Strings.Clear;
    var lst := TStringList.Create;
    try
      for var pair in Generator.TypeNames do
        lst.AddPair(pair.Key, pair.Value);
      lst.Sort;
      for var I := 0 to lst.Count - 1 do
        edtTypeNames.Strings.AddPair(lst.Names[I], lst.ValueFromIndex[I]);
    finally
      lst.Free;
    end;
  finally
    edtTypeNames.EndUpdate;
  end;
end;

procedure TMappingsGeneratorForm.LoadValues;
begin
  edtPrefixes.Text := Generator.Prefixes;
  edtRegex.Text := Generator.RegEx;
  LoadTypeNames;
end;

procedure TMappingsGeneratorForm.SaveTypeNames;
begin
  for var I := 0 to edtTypeNames.Strings.Count - 1 do begin
    var key := edtTypeNames.Strings.Names[I];
    var value := edtTypeNames.Strings.ValueFromIndex[I];
    Generator.TypeNames[key] := value;
  end;
end;

procedure TMappingsGeneratorForm.SaveValues;
begin
  Generator.Prefixes := edtPrefixes.Text;
  Generator.RegEx := edtRegex.Text;
  SaveTypeNames;
end;

procedure TMappingsGeneratorForm.SetGenerator(const Value: TDataSetHelperGenerator);
begin
  FGenerator := Value;
  LoadValues;
end;

end.
