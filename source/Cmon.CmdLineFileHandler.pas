unit Cmon.CmdLineFileHandler;

interface

uses
  System.Classes, System.SysUtils,
  Cmon.CmdLineHandler;

resourcestring
  SMayContainWildcards = 'may contain wildcards';
  SUsage = 'usage';
  SConverting = 'converting';
  SCanNotCreateFolder = 'can not create folder';

type
  TFileHandlerCmdLine = class(TCmdLineHandler)
  private
    FLogPrefix: string;
  protected
    procedure InternalHandleCmdLine; override;
    procedure HandleFile(const ASourceName, ATargetName: string); virtual;
    function MakeTargetName(const SourceName, BasePath, TargetPath: string): string; virtual;
    procedure ShowCmdLine; override;
    procedure SortFiles(AFiles: TStringList); virtual;
  public
    constructor Create; override;
    property LogPrefix: string read FLogPrefix write FLogPrefix;
  end;

implementation

uses
  System.Types, System.IOUtils;

procedure TFileHandlerCmdLine.InternalHandleCmdLine;
var
  FileMask: string;
  FilePath: string;
  files: TStringList;
  i: Integer;
  s: string;
  searchOption: TSearchOption;
  SourceName: string;
  TargetName: string;
  TargetPath: string;
begin
  if Switch['s'] then
    searchOption := TSearchOption.soAllDirectories
  else
    searchOption := TSearchOption.soTopDirectoryOnly;

  files := TStringList.Create;
  try
    for i := 0 to Params.Count - 1 do begin
      s := Params[i];
      try
        s := TPath.GetFullPath(CleanPathName(s));
        FileMask := TPath.GetFileName(s);
        FilePath := TPath.GetDirectoryName(s);
        files.Clear;
        for SourceName in TDirectory.GetFiles(FilePath, FileMask, searchOption) do begin
          files.Add(SourceName);
        end;
        SortFiles(files);
        for SourceName in files do begin
          TargetName := MakeTargetName(SourceName, IncludeTrailingPathDelimiter(FilePath), OutPath);
          if TargetName > '' then begin
            TargetPath := TPath.GetDirectoryName(TargetName);
            if TargetPath.IsEmpty or ForceDirectories(TargetPath) then
              HandleFile(SourceName, TargetName)
            else
              LogError(SCanNotCreateFolder + ': ' + TargetPath);
          end
          else begin
            LogLine('skipped ' + SourceName + ' (no target)');
          end;
        end;
      except
        on E: Exception do
          LogError(E.Message);
      end;
    end;
  finally
    files.Free;
  end;
end;

constructor TFileHandlerCmdLine.Create;
begin
  inherited;
  FLogPrefix := SConverting;
end;

procedure TFileHandlerCmdLine.HandleFile(const ASourceName, ATargetName: string);
begin
  LogLine(LogPrefix + ': ' + ASourceName);
end;

function TFileHandlerCmdLine.MakeTargetName(const SourceName, BasePath, TargetPath: string): string;
begin
  Result := SourceName;
  if TargetPath > '' then begin
    result := IncludeTrailingPathDelimiter(TargetPath) + ExtractRelativePath(BasePath, SourceName);
  end;
end;

procedure TFileHandlerCmdLine.ShowCmdLine;
begin
  Writeln(SUsage, ': ', ExeName, ' [<filepath>]<filename> [-o:<outputpath>] [-l:<logfile>] [-s]');
  Writeln('       <filename> ', SMayContainWildcards);
  Writeln;
end;

procedure TFileHandlerCmdLine.SortFiles(AFiles: TStringList);
begin
end;

end.
