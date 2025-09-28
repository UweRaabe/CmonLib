program FactoryDemo;

uses
  Vcl.Forms,
  Main.Form in 'Main.Form.pas' {MainForm},
  MyInterface.Types in 'MyInterface.Types.pas',
  MyInterface.Impl in 'MyInterface.Impl.pas',
  MyInterface.Factory in 'MyInterface.Factory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
