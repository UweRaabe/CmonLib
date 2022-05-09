program ObserversDemo;

uses
  Vcl.Forms,
  Cmon.Messaging.Dialogs.Vcl,
  DataTypes in 'DataTypes.pas',
  Main.Form in 'Main.Form.pas' {Form702};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm702, Form702);
  Application.Run;
end.
