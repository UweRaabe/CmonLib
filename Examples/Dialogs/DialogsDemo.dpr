program DialogsDemo;

uses
  Vcl.Forms,
  Cmon.Messaging.Dialogs.Vcl,
  Main.Form in 'Main.Form.pas' {MainForm},
  Utilities in 'Utilities.pas';

{$R *.res}

procedure PreInitialize;
begin
  { Per default using Cmon.Messaging.Dialogs.Vcl will automatically register the containing message handler during
    Application.Initialize. To optionally disable auto registering of all DlgMessage handlers this is one place to do.
    You need to add Cmon.Messaging to the uses to make it compile. }
//  TDlgMessage.AutoRegisterHandler := False;

  { You can as well disable automatic registration of indivudual handlers. That implies to leave the corresponding setting
    above at True of course. }
//  TDlgMessageHandlerVcl.AutoRegisterHandler := False;
end;

begin
  PreInitialize;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

