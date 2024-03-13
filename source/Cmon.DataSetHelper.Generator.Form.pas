unit Cmon.DataSetHelper.Generator.Form;

interface

uses
  System.Classes, System.Actions,
  Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.ActnList, Vcl.StdCtrls,
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
    procedure actStripPrefixExecute(Sender: TObject);
    procedure actStripPrefixUpdate(Sender: TObject);
    procedure actUseNameConstantsExecute(Sender: TObject);
    procedure actUseNameConstantsUpdate(Sender: TObject);
    procedure actUseRegexExecute(Sender: TObject);
    procedure actUseRegexUpdate(Sender: TObject);
  private
    FGenerator: TDataSetHelperGenerator;
  public
    class function Execute(AGenerator: TDataSetHelperGenerator): Boolean;
    property Generator: TDataSetHelperGenerator read FGenerator write FGenerator;
  end;

implementation

uses
  Cmon.DataSetHelper;

{$R *.dfm}

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
  ModalResult := mrOK;
end;

procedure TMappingsGeneratorForm.actStripPrefixExecute(Sender: TObject);
begin
  Generator.NameDetection := ndStripPrefix;
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
  Generator.NameDetection := ndRegEx;
end;

procedure TMappingsGeneratorForm.actUseRegexUpdate(Sender: TObject);
begin
  actUseRegex.Checked := (Generator.NameDetection = ndRegEx);
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

end.
