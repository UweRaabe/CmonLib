unit Cmon.CmdLineHandler;

interface

uses
  System.Classes, System.SysUtils;

resourcestring
  SPressEnterToContinue = 'press <enter> to continue...';

type
  TProgressEvent = TProc<Integer>;
  TProgressHandler = class
  private
    FCount: Integer;
    FCurrent: Integer;
    FOnProgress: TProgressEvent;
    FProgress: Integer;
    procedure SetCount(const Value: Integer);
    procedure SetProgress(const Value: Integer);
  protected
    procedure DoProgress; virtual;
  public
    class function ConsoleProgressEvent: TProgressEvent;
    procedure Finished;
    procedure LinkToConsole;
    procedure Reset;
    procedure StepIt;
    property Count: Integer read FCount write SetCount;
    property Progress: Integer read FProgress write SetProgress;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

type
  TCmdLineHandler = class
  private
    FLogFileName: string;
    FLogs: TStringList;
    FOEMEncoding: TEncoding;
    FOutPath: string;
    FParams: TStringList;
    FWaitForEnter: Boolean;
    function GetSwitch(const Name: string): Boolean;
    procedure SaveLogFile(const FileName: string);
  protected
    function ChangeChar(const Value: string; Source, Target: Char): string;
    function ChangePath(const FileName, Path: string): string;
    function CleanPathName(const Value: string): string;
    function ExeName: string;
    function ExePath: string;
    procedure FinishHandler; virtual;
    function GetFileEncoding(const AFileName: string; ADefaultEncoding: TEncoding): TEncoding;
    procedure HandleCmdLine;
    procedure InternalHandleCmdLine; virtual; abstract;
    procedure LogError(const Value: string); virtual;
    procedure LogLine(const Value: string); virtual;
    function MakeLogProc: TProc<string>;
    procedure PrepareHandler; virtual;
    procedure ShowCmdLine; virtual; abstract;
    procedure ShowVersion; virtual;
    function VersionString: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class procedure Execute; virtual;
    class function NeedsHelp: Boolean; virtual;
    function SwitchValue(const Name: string; const Default: string = ''): string;
    property LogFileName: string read FLogFileName write FLogFileName;
    property Logs: TStringList read FLogs;
    property OEMEncoding: TEncoding read FOEMEncoding;
    property OutPath: string read FOutPath write FOutPath;
    property Params: TStringList read FParams;
    property Switch[const Name: string]: Boolean read GetSwitch;
    property WaitForEnter: Boolean read FWaitForEnter write FWaitForEnter;
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils, System.Diagnostics;

function GetFileVersion(const AFileName: string; var AMajor, AMinor, ARelease, ABuild: Cardinal): Boolean;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := False;
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          AMajor := HiWord(FI.dwFileVersionMS);
          AMinor := LoWord(FI.dwFileVersionMS);
          ARelease := HiWord(FI.dwFileVersionLS);
          ABuild := LoWord(FI.dwFileVersionLS);
          Result:= True;
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

constructor TCmdLineHandler.Create;
begin
  inherited;
  FOEMEncoding := TMBCSEncoding.Create(850);
  FParams := TStringList.Create();

  {$IFDEF DEBUG}
  FWaitForEnter := true;
  {$ENDIF}
end;

destructor TCmdLineHandler.Destroy;
begin
  FParams.Free;
  FOEMEncoding.Free;
  inherited Destroy;
end;

function TCmdLineHandler.ChangeChar(const Value: string; Source, Target: Char): string;
var
  I: Integer;
begin
  result := Value;
  for I := 1 to Length(result) do
    if result[I] = Source then
      result[I] := Target;
end;

function TCmdLineHandler.ChangePath(const FileName, Path: string): string;
begin
  Result := ChangeFilePath(FileName, Path);
end;

function TCmdLineHandler.CleanPathName(const Value: string): string;
begin
  result := ChangeChar(Value, TPath.AltDirectorySeparatorChar, TPath.DirectorySeparatorChar);
end;

class procedure TCmdLineHandler.Execute;
var
  instance: TCmdLineHandler;
begin
  instance := Self.Create;
  try
    instance.ShowVersion;
    if NeedsHelp then
      instance.ShowCmdLine
    else
      instance.HandleCmdLine;
  finally
    instance.Free;
  end;
end;

procedure TCmdLineHandler.LogLine(const Value: string);
begin
  if Logs <> nil then
    Logs.Add(Value);
  Writeln(Value);
end;

procedure TCmdLineHandler.SaveLogFile(const FileName: string);
begin
  if (FileName > '') and (Logs <> nil) and (Logs.Count > 0) then
    Logs.SaveToFile(FileName);
end;

function TCmdLineHandler.ExeName: string;
begin
  result := TPath.GetFileNameWithoutExtension(ParamStr(0));
end;

function TCmdLineHandler.ExePath: string;
begin
  result := TPath.GetDirectoryName(ParamStr(0));
end;

procedure TCmdLineHandler.FinishHandler;
begin
  SaveLogFile(LogFileName);
  FreeAndNil(FLogs);
end;

function TCmdLineHandler.GetFileEncoding(const AFileName: string; ADefaultEncoding: TEncoding): TEncoding;
const
  maxLengthPreamble = 3;
var
  buffer: TBytes;
  stream: TStream;
begin
  result := nil;
  SetLength(buffer, maxLengthPreamble);
  stream := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    stream.ReadBuffer(buffer[0], Length(buffer));
  finally
    stream.Free;
  end;
  TEncoding.GetBufferEncoding(buffer, result, ADefaultEncoding);
end;

function TCmdLineHandler.GetSwitch(const Name: string): Boolean;
begin
  Result := FindCmdLineSwitch(Name);
end;

procedure TCmdLineHandler.HandleCmdLine;
var
  sw: TStopwatch;
begin
  try
    sw := TStopwatch.StartNew;
    PrepareHandler;
    try
      InternalHandleCmdLine;
    finally
      FinishHandler;
    end;
    LogLine(Format('elapsed time: %d ms', [sw.ElapsedMilliseconds]));
  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
  if WaitForEnter then begin
    Writeln(SPressEnterToContinue);
    Readln;
  end;
end;

procedure TCmdLineHandler.LogError(const Value: string);
begin
  LogLine('Error: ' + Value);
  Writeln(SPressEnterToContinue);
  Readln;
end;

function TCmdLineHandler.MakeLogProc: TProc<string>;
begin
  Result :=
    procedure(Arg1: string)
    begin
      LogLine(Arg1);
    end;
end;

class function TCmdLineHandler.NeedsHelp: Boolean;
begin
  Result := (ParamCount = 0) or FindCmdLineSwitch('?') or FindCmdLineSwitch('help');
end;

procedure TCmdLineHandler.PrepareHandler;
var
  S: string;
  I: Integer;
begin
  for I := 1 to ParamCount do begin
    S := ParamStr(I);
    if not CharInSet(S[1], SwitchChars) then
      Params.Add(S);
  end;

  if Switch['p'] then begin
    WaitForEnter := true;
  end;

  if FindCmdLineSwitch('o', S) then
    S := TPath.GetFullPath(CleanPathName(S))
  else
    S := '';
  OutPath := S;

  if FindCmdLineSwitch('l', S) then
    S := CleanPathName(S)
  else
    S := '';
  LogFileName := S;
  if LogFileName > '' then
    FLogs := TStringList.Create;
end;

procedure TCmdLineHandler.ShowVersion;
begin
  Writeln(Format('%s %s', [ExeName, VersionString])
  {$IFDEF DEBUG}
    , ' (Debug Version)'
  {$ENDIF}
  );
end;

function TCmdLineHandler.SwitchValue(const Name: string; const Default: string): string;
begin
  if not FindCmdLineSwitch(Name, result) then
    Result := Default;
end;

function TCmdLineHandler.VersionString: string;
var
  build: Cardinal;
  major: Cardinal;
  minor: Cardinal;
  release: Cardinal;
begin
  if GetFileVersion(ParamStr(0), major, minor, release, build) then
    Result := Format('Version %d.%d.%d.%d', [major, minor, release, build])
  else
    Result := '<no version info>';
end;

procedure TProgressHandler.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Progress);
end;

class function TProgressHandler.ConsoleProgressEvent: TProgressEvent;
begin
  Result :=
    procedure(Percent: Integer)
    begin
      Write(Format('%.2d%%', [Percent]) + #8#8#8);
    end;
end;

procedure TProgressHandler.Finished;
begin
  Progress := 100;
end;

procedure TProgressHandler.LinkToConsole;
begin
  OnProgress := ConsoleProgressEvent();
end;

procedure TProgressHandler.Reset;
begin
  FCurrent := 0;
  FProgress := 0;
  DoProgress;
end;

procedure TProgressHandler.SetCount(const Value: Integer);
begin
  FCount := Value;
  Reset;
end;

procedure TProgressHandler.SetProgress(const Value: Integer);
begin
  if FProgress <> Value then begin
    FProgress := Value;
    DoProgress;
  end;
end;

procedure TProgressHandler.StepIt;
begin
  Inc(FCurrent);
  Progress := Round(100*FCurrent/FCount);
end;

end.
