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
  TSearch = class(TAsyncGuard)
  private
    FPath: string;
    FSearchPattern: string;
    FTarget: ISearchTarget;
    procedure SearchFolder(const APath, ASearchPattern: string);
  strict protected
    procedure AddFiles(const AFiles: TArray<string>); virtual;
    procedure BeginSearch; virtual;
    procedure CheckCancelled;
    procedure EndSearch; virtual;
    procedure Execute; override;
  public
    constructor Create(ATarget: ISearchTarget; const APath, ASearchPattern: string);
    class procedure Execute(ATarget: ISearchTarget; const APath, ASearchPattern: string; out ACancel: ICancel); reintroduce; overload;
  end;

type
  TAsyncSearch = class(TSearch)
  strict protected
    procedure AddFiles(const AFiles: TArray<string>); override;
    procedure BeginSearch; override;
    procedure EndSearch; override;
    procedure Execute(ACancel: ICancel); override;
  public
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

procedure TAsyncSearch.Execute(ACancel: ICancel);
begin
  ExecuteAsync(ACancel);
end;

constructor TSearch.Create(ATarget: ISearchTarget; const APath, ASearchPattern: string);
begin
  inherited Create;
  FTarget := ATarget;
  FPath := APath;
  FSearchPattern := ASearchPattern;
end;

procedure TSearch.AddFiles(const AFiles: TArray<string>);
begin
  CheckCancelled;
  if Length(AFiles) = 0 then Exit;
  if FTarget <> nil then
    FTarget.AddFiles(AFiles);
end;

procedure TSearch.BeginSearch;
begin
  CheckCancelled;
  if FTarget <> nil then
    FTarget.BeginSearch;
end;

procedure TSearch.CheckCancelled;
begin
  if IsCancelled then
    FTarget := nil;
end;

procedure TSearch.EndSearch;
begin
  CheckCancelled;
  if FTarget <> nil then
    FTarget.EndSearch;
end;

procedure TSearch.Execute;
begin
  BeginSearch;
  SearchFolder(FPath, FSearchPattern);
  EndSearch;
end;

class procedure TSearch.Execute(ATarget: ISearchTarget; const APath, ASearchPattern: string; out ACancel: ICancel);
begin
  Execute(Self.Create(ATarget, APath, ASearchPattern), ACancel);
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
    if IsCancelled then Exit;
    if not TDirectory.Exists(dir) then Continue;
    SearchFolder(dir, ASearchPattern);
  end;
end;

end.
