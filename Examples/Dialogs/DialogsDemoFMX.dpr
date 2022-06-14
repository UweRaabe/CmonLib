program DialogsDemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Cmon.Messaging.Dialogs.Fmx,
  Utilities in 'Utilities.pas',
  Main.FmxForm in 'Main.FmxForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
