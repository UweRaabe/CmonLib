program DbAwareDemoFmx;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main.FmxForm in 'Main.FmxForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
