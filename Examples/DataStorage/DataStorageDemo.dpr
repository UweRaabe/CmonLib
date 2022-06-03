﻿program DataStorageDemo;

uses
  Vcl.Forms,
  Cmon.Messaging.Dialogs.Vcl,
  Cmon.DataStorage.Target,
  Cmon.DataStorage.JSON,
  Cmon.DataStorage.Inifile,
  Common.DM in 'Common.DM.pas' {dmCommon: TDataModule},
  Common.Frame in 'Common.Frame.pas' {CommonFrame: TFrame},
  Common.Form in 'Common.Form.pas' {CommonForm},
  Main.Frame in 'Main.Frame.pas' {DemoFrame: TFrame},
  Main.Form in 'Main.Form.pas' {DemoMainForm};

{$R *.res}

procedure PreInitialize;
begin
  { Per default using Cmon.Messaging.Dialogs.Vcl, Cmon.DataStorage.JSON and Cmon.DataStorage.Inifile will automatically
    register the containing message handlers during Application.Initialize. To optionally disable auto registering of all
    DlgMessage handlers and/or TDataStorageTarget handlers this is one place to do.
    You need to add Cmon.Messaging and/or Cmon.DataStorage to the uses to make it compile. }
//  TDlgMessage.AutoRegisterHandler := False;
//  TDataStorage.AutoRegisterHandler := False;

  { You can as well disable automatic registration of individual handlers. That implies to leave the corresponding settings
    above at True of course. }
//  TDlgMessageHandlerVcl.AutoRegisterHandler := False;
//  TIniStorageTargetHandler.AutoRegisterHandler := False;
//  TJSONStorageTargetHandler.AutoRegisterHandler := False;

  { This sets the extension (and thus the handler) which is used for automatic load and Save of storage data }
  TCustomStorageTarget.DefaultFileExtension := TIniStorageTarget.FileExtension;
  { This sets the default extension for the settings file used for manual Load and Save of storage data in DemoMainForm }
  TDemoMainForm.SettingsFileExtension := TJSONStorageTarget.FileExtension;
end;

begin
  PreInitialize;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoMainForm, DemoMainForm);
  Application.Run;
end.