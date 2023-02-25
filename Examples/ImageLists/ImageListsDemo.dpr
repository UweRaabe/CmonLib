program ImageListsDemo;

uses
  Vcl.Forms,
  Cmon.Vcl.Forms,
  Images.DM in 'Images.DM.pas' {dmImages: TDataModule},
  DesignImages.DM in 'DesignImages.DM.pas' {dmDesignImages: TDataModule},
  Main.Frame in 'Main.Frame.pas' {MainFrame: TFrame},
  Main.Form in 'Main.Form.pas' {MainForm},
  Base.Form in 'Base.Form.pas' {BaseForm},
  Simple.Form in 'Simple.Form.pas' {SimpleForm},
  StaticFrame.Form in 'StaticFrame.Form.pas' {StaticFrameForm};

{$R *.res}

procedure PreInitialize;
begin
  TCommonForm.DefaultHandleGlobalVirtualImageLists := True;
end;

begin
  PreInitialize;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmImages, dmImages);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
