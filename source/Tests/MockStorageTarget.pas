unit MockStorageTarget;

interface

uses
  System.Classes, System.IniFiles,
  Cmon.DataStorage.Types, Cmon.DataStorage.Inifile;

type
  TMockIniStorageTarget = class(TIniStorageTarget)
  private
    FIniFile: TMemIniFile;
    FLines: TStrings;
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
  protected
    function ReadString(const Key: string; const Ident: string; const Default: string): string; override;
    procedure WriteString(const Key: string; const Ident: string; const Value: string); override;
  public
    constructor Create(const AFileName: string = ''); override;
    destructor Destroy; override;
    property Lines: TStrings read GetLines write SetLines;
  end;

implementation

constructor TMockIniStorageTarget.Create;
begin
  inherited;
  FLines := TStringList.Create;
  FIniFile := TMemIniFile.Create('');
end;

destructor TMockIniStorageTarget.Destroy;
begin
  FIniFile.Free;
  FLines.Free;
  inherited Destroy;
end;

function TMockIniStorageTarget.GetLines: TStrings;
begin
  FIniFile.GetStrings(FLines);
  Result := FLines;
end;

function TMockIniStorageTarget.ReadString(const Key, Ident, Default: string): string;
begin
  Result := FIniFile.ReadString(Key, Ident, Default);
end;

procedure TMockIniStorageTarget.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
  FIniFile.SetStrings(FLines);
end;

procedure TMockIniStorageTarget.WriteString(const Key, Ident, Value: string);
begin
  FIniFile.WriteString(Key, Ident, Value);
end;

end.
