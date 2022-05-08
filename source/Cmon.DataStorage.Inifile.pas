unit Cmon.DataStorage.Inifile;

interface

uses
  System.IniFiles,
  Cmon.DataStorage;

type
  TIniStorageTarget = class(TInterfacedObject, IStorageTarget)
  private
    FFileName: string;
    FIniFile: TMemIniFile;
  class var
    FDefaultFileName: string;
    FDefaultFilePath: string;
    class function GetDefaultFileName: string; static;
  strict protected
    property IniFile: TMemIniFile read FIniFile implements IStorageTarget;
  public
    constructor Create(const AFileName: string = '');
    destructor Destroy; override;
    class property DefaultFileName: string read GetDefaultFileName write FDefaultFileName;
    class property DefaultFilePath: string read FDefaultFilePath write FDefaultFilePath;
    property FileName: string read FFileName;
  end;

implementation

uses
  System.IOUtils, System.SysUtils,
  Cmon.Utilities;

constructor TIniStorageTarget.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  if FFileName.IsEmpty then begin
    FFileName := DefaultFileName;
    if not DefaultFilePath.IsEmpty then
      FFileName := TPath.Combine(DefaultFilePath, TPath.GetFileName(FFileName));
  end;
  FIniFile := TMemIniFile.Create(FFileName);
  FIniFile.AutoSave := True;
end;

destructor TIniStorageTarget.Destroy;
begin
  FIniFile.Free;
  inherited Destroy;
end;

class function TIniStorageTarget.GetDefaultFileName: string;
begin
  Result := FDefaultFileName;
  if Result.IsEmpty then
    Result := TPath.ChangeExtension(TUtilities.GetExeName, '.ini');
end;

function CreateStorageTarget: IStorageTarget;
begin
  result := TIniStorageTarget.Create;
end;

initialization
  TDataStorage.StorageTargetFactory := CreateStorageTarget;
end.
