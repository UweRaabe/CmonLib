unit AsyncSearch;

interface

uses
  Cmon.AsyncGuard;

type
  ISearchTarget = interface
    procedure AddFiles(const AFiles: TArray<string>);
    procedure BeginSearch;
    procedure EndSearch;
  end;

type
  TSearch = class(TAsyncTask<ISearchTarget>)
  private
    FPath: string;
    FSearchPattern: string;
    procedure SearchFolder(const APath, ASearchPattern: string);
  strict protected
    procedure AddFiles(const AFiles: TArray<string>); virtual;
    procedure BeginSearch; virtual;
    procedure EndSearch; virtual;
    procedure InternalExecute; override;
  public
    constructor Create(ATarget: ISearchTarget; const APath, ASearchPattern: string);
    class procedure Execute(ATarget: ISearchTarget; const APath, ASearchPattern: string; out ACancel: ICancel);
  end;

type
  TAsyncSearch = class(TSearch)
  strict protected
    procedure AddFiles(const AFiles: TArray<string>); override;
    procedure BeginSearch; override;
    procedure EndSearch; override;
  public
    class procedure Execute(ATarget: ISearchTarget; const APath, ASearchPattern: string; out ACancel: ICancel);
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils;

procedure TAsyncSearch.AddFiles(const AFiles: TArray<string>);
begin
  TThread.Synchronize(nil, procedure begin inherited; end);
end;

procedure TAsyncSearch.BeginSearch;
begin
  TThread.Synchronize(nil, procedure begin inherited; end);
end;

procedure TAsyncSearch.EndSearch;
begin
  TThread.Synchronize(nil, procedure begin inherited; end);
end;

class procedure TAsyncSearch.Execute(ATarget: ISearchTarget; const APath, ASearchPattern: string; out ACancel: ICancel);
begin
  TAsyncGuard.ExecuteAsync(Self.Create(ATarget, APath, ASearchPattern), ACancel);
end;

constructor TSearch.Create(ATarget: ISearchTarget; const APath, ASearchPattern: string);
begin
  inherited Create(ATarget);
  FPath := APath;
  FSearchPattern := ASearchPattern;
end;

procedure TSearch.AddFiles(const AFiles: TArray<string>);
var
  Target: ISearchTarget;
begin
  if Length(AFiles) = 0 then Exit;
  if HasTarget(Target) then
    Target.AddFiles(AFiles);
end;

procedure TSearch.BeginSearch;
var
  Target: ISearchTarget;
begin
  if HasTarget(Target) then
    Target.BeginSearch;
end;

procedure TSearch.EndSearch;
var
  Target: ISearchTarget;
begin
  if HasTarget(Target) then
    Target.EndSearch;
end;

procedure TSearch.InternalExecute;
begin
  BeginSearch;
  SearchFolder(FPath, FSearchPattern);
  EndSearch;
end;

class procedure TSearch.Execute(ATarget: ISearchTarget; const APath, ASearchPattern: string; out ACancel: ICancel);
begin
  TAsyncGuard.ExecuteSync(Self.Create(ATarget, APath, ASearchPattern), ACancel);
end;

procedure TSearch.SearchFolder(const APath, ASearchPattern: string);
var
  arr: TArray<string>;
  dir: string;
begin
  arr := TDirectory.GetFiles(APath, ASearchPattern);
  AddFiles(arr);
  { release memory as early as possible }
  arr := nil;
  for dir in TDirectory.GetDirectories(APath) do begin
    if IsCanceled then Exit;
    if not TDirectory.Exists(dir) then Continue;
    SearchFolder(dir, ASearchPattern);
  end;
end;

end.
