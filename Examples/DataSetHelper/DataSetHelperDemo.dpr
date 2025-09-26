program DataSetHelperDemo;

uses
  Vcl.Forms,
  Main.Form in 'Main.Form.pas' {MainForm},
  Mapping.Types in 'Mapping.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
