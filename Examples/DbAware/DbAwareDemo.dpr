program DbAwareDemo;

uses
  Vcl.Forms,
  Main.Form in 'Main.Form.pas' {MainForm},
  Images.DM in 'Images.DM.pas' {dmImages: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmImages, dmImages);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
