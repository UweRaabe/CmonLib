program DataStorageDemo;

uses
  Vcl.Forms,
  Cmon.DataStorage.Inifile,
  Common.DM in 'Common.DM.pas' {dmCommon: TDataModule},
  Common.Frame in 'Common.Frame.pas' {CommonFrame: TFrame},
  Common.Form in 'Common.Form.pas' {CommonForm},
  Main.Form in 'Main.Form.pas' {DemoMainForm},
  Main.Frame in 'Main.Frame.pas' {DemoFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoMainForm, DemoMainForm);
  Application.Run;
end.
