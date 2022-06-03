unit Cmon.DataStorage.Inifile;

interface

uses
  System.IniFiles, System.Messaging,
  Cmon.DataStorage, Cmon.DataStorage.Target;

type
  TIniStorageTarget = class(TCustomStorageTarget)
  private
    FIniFile: TMemIniFile;
    procedure SetIniFile(const Value: TMemIniFile);
  strict protected
    function ReadString(const Key: string; const Ident: string; const Default: string): string; override;
    procedure WriteString(const Key: string; const Ident: string; const Value: string); override;
    property IniFile: TMemIniFile read FIniFile write SetIniFile;
  public
    destructor Destroy; override;
    class function Description: string; override;
    class function FileExtension: string; override;
    procedure LoadFromFile(const AFileName: string); override;
  end;

type
  TIniStorageTargetHandler = class(TStorageTargetHandler<TIniStorageTarget>)
  strict private
  class var
    FAutoRegisterHandler: Boolean;
  public
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
  end;

implementation

uses
  System.IOUtils, System.SysUtils,
  Cmon.Utilities, Cmon.Messaging, Cmon.Initializing;

resourcestring
  SINIFiles = 'INI files';

class function TIniStorageTarget.Description: string;
begin
  Result := SINIFiles;
end;

destructor TIniStorageTarget.Destroy;
begin
  FIniFile.Free;
  inherited Destroy;
end;

class function TIniStorageTarget.FileExtension: string;
begin
  Result := '.ini';
end;

procedure TIniStorageTarget.LoadFromFile(const AFileName: string);
begin
  IniFile := TMemIniFile.Create(AFileName);
end;

function TIniStorageTarget.ReadString(const Key: string; const Ident: string; const Default: string): string;
begin
  Result := IniFile.ReadString(Key, Ident, Default);
end;

procedure TIniStorageTarget.SetIniFile(const Value: TMemIniFile);
begin
  if FIniFile <> Value then
  begin
    FIniFile.Free;
    FIniFile := Value;
    if FIniFile <> nil then
      FIniFile.AutoSave := True;
  end;
end;

procedure TIniStorageTarget.WriteString(const Key: string; const Ident: string; const Value: string);
begin
  IniFile.WriteString(Key, Ident, Value);
end;

var
  Instance: TIniStorageTargetHandler = nil;

{ will be called in Application.Initialize after all other initialization code has been executed }
procedure InitHandler;
begin
  if TDataStorage.AutoRegisterHandler and TIniStorageTargetHandler.AutoRegisterHandler then
    Instance := TIniStorageTargetHandler.Create;
end;

initialization
  TIniStorageTargetHandler.AutoRegisterHandler := True;
  TInitialize.AddInitProc(InitHandler);
finalization
  Instance.Free;
end.
