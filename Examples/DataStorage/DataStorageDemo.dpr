program DataStorageDemo;

uses
  Vcl.Forms,
  Cmon.DataStorage,
  Cmon.DataStorage.Target,
  Cmon.DataStorage.Inifile,
  Cmon.DataStorage.JSON,
  Cmon.Vcl.Forms,
  Main.Data.Types in 'Main.Data.Types.pas',
  Main.Frame in 'Main.Frame.pas' {DemoFrame: TFrame},
  Main.Form in 'Main.Form.pas' {DemoMainForm};

{$R *.res}

procedure PreInitialize;
begin
  { Per default using  Cmon.DataStorage.JSON and Cmon.DataStorage.Inifile will automatically
    register the containing message handlers during Application.Initialize. To optionally disable auto registering of all
    TDataStorageTarget handlers this is one place to do.
    You need to Cmon.DataStorage to the uses to make it compile. }
//  TDataStorage.AutoRegisterHandler := False;

  { You can as well disable automatic registration of individual handlers. That implies to leave the corresponding settings
    above at True of course. }
//  TIniStorageTargetHandler.AutoRegisterHandler := False;
//  TJSONStorageTargetHandler.AutoRegisterHandler := False;

  { This sets the extension (and thus the handler) which is used for automatic load and Save of storage data }
  TCustomStorageTarget.DefaultFileExtension := TIniStorageTarget.FileExtension;
  { This sets the default extension for the settings file used for manual Load and Save of storage data in DemoMainForm }
  TDemoMainForm.SettingsFileExtension := TJSONStorageTarget.FileExtension;

  { Here we activate the DataStorage for all TCommonForm descendants.

    TAutoDataStorage.callOutside loads from the storage before any OnCreate event and stores to storage after all OnDestroy events.
    TAutoDataStorage.callInside loads from the storage after all OnCreate events and stores to storage before any OnDestroy event.
  }
  TCommonForm.DefaultAutoDataStorage := TAutoDataStorage.callInside;
end;

begin
  PreInitialize;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoMainForm, DemoMainForm);
  Application.Run;
end.
