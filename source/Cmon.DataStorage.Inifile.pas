unit Cmon.DataStorage.Inifile;

interface

uses
  System.IniFiles, System.Messaging, System.Classes, System.SysUtils,
  Cmon.DataStorage, Cmon.DataStorage.Target;

type
  TIniStorageTarget = class(TCustomStorageTarget)
  private
    FIniFile: TMemIniFile;
    FStream: TBytesStream;
    function GetData: TBytes;
    procedure SetData(const Value: TBytes);
    procedure SetIniFile(const Value: TMemIniFile);
    procedure SetStream(const Value: TBytesStream);
  strict protected
    procedure EraseStorageKey(const Key: string); override;
    procedure DeleteKey(const Key, Ident: string); override;
    function InternalLoadBytes(const AFileName: string): TBytes; virtual;
    procedure InternalSaveBytes(const AFileName: string; const ABytes: TBytes); virtual;
    procedure ReadKey(const Key: string; Target: TStrings); override;
    function ReadString(const Key: string; const Ident: string; const Default: string): string; override;
    procedure UpdateData;
    procedure WriteString(const Key: string; const Ident: string; const Value: string); override;
    function ValueExists(const Key, Ident: string): Boolean; override;
    property IniFile: TMemIniFile read FIniFile write SetIniFile;
    property Stream: TBytesStream read FStream write SetStream;
  protected
    property Data: TBytes read GetData write SetData;
  public
    constructor Create(const AFileName: string = ''); override;
    destructor Destroy; override;
    class function Description: string; override;
    class function FileExtension: string; override;
    procedure LoadFromFile(const AFileName: string); override;
    procedure SaveToFile(const AFileName: string); override;
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
  System.IOUtils,
  Cmon.Utilities, Cmon.Messaging, Cmon.Initializing;

resourcestring
  SINIFiles = 'INI files';

constructor TIniStorageTarget.Create(const AFileName: string = '');
begin
  inherited;
  Stream := TBytesStream.Create(nil);
end;

class function TIniStorageTarget.Description: string;
begin
  Result := SINIFiles;
end;

destructor TIniStorageTarget.Destroy;
begin
  FIniFile.Free;
  FStream.Free;
  inherited Destroy;
end;

procedure TIniStorageTarget.EraseStorageKey(const Key: string);
begin
  var lst := TStringList.Create;
  try
    IniFile.ReadSections(Key, lst);
    for var S in lst do
      IniFile.EraseSection(S);
  finally
    lst.Free;
  end;
end;

procedure TIniStorageTarget.DeleteKey(const Key, Ident: string);
begin
  IniFile.DeleteKey(Key, Ident);
end;

class function TIniStorageTarget.FileExtension: string;
begin
  Result := '.ini';
end;

function TIniStorageTarget.GetData: TBytes;
begin
  Result := Copy(Stream.Bytes, 0, Stream.Size);
end;

function TIniStorageTarget.InternalLoadBytes(const AFileName: string): TBytes;
begin
  Result := nil;
  if TFile.Exists(AFileName) then
    Result := TFile.ReadAllBytes(AFileName);
end;

procedure TIniStorageTarget.InternalSaveBytes(const AFileName: string; const ABytes: TBytes);
begin
  { make sure the folder exists }
  TDirectory.CreateDirectory(TPath.GetDirectoryName(AFileName));
  TFile.WriteAllBytes(AFileName, ABytes);
end;

procedure TIniStorageTarget.LoadFromFile(const AFileName: string);
begin
  Data := InternalLoadBytes(AFileName);
  inherited;
end;

procedure TIniStorageTarget.ReadKey(const Key: string; Target: TStrings);
begin
  IniFile.ReadSectionValues(Key, Target);
end;

function TIniStorageTarget.ReadString(const Key: string; const Ident: string; const Default: string): string;
begin
  Result := IniFile.ReadString(Key, Ident, Default);
end;

procedure TIniStorageTarget.SaveToFile(const AFileName: string);
begin
  inherited;
  UpdateData;
  InternalSaveBytes(AFileName, Data);
end;

procedure TIniStorageTarget.SetData(const Value: TBytes);
begin
  Stream := TBytesStream.Create(Value);
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

procedure TIniStorageTarget.SetStream(const Value: TBytesStream);
begin
  if FStream <> Value then
  begin
    FStream.Free;
    FStream := Value;
    if FStream = nil then
      FStream := TBytesStream.Create(nil);
    IniFile := TMemIniFile.Create(FStream);
  end;
end;

procedure TIniStorageTarget.UpdateData;
begin
  { assures that all changes are written to Stream, even when only the encoding has to be changed }
  if IniFile.Modified or (Inifile.Encoding <> TEncoding.UTF8) then begin
    IniFile.Encoding := TEncoding.UTF8;
    IniFile.UpdateFile;
  end;
end;

function TIniStorageTarget.ValueExists(const Key, Ident: string): Boolean;
begin
  Result := IniFile.ValueExists(Key, Ident);
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
