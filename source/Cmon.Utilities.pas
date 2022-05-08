unit Cmon.Utilities;

interface

uses
  System.Classes, System.SysUtils, System.Rtti;

type
  EValidationError = class(Exception);

type
  TComponentHelper = class helper for TComponent
  public
    function FindComponentOf<T: class>(const AName: string): T;
    procedure ForAllComponentsOf<T: class>(DoProc: TProc<T>);
  end;

type
  TRttiHelper = record
  public
    class function FindAttribute<T: TCustomAttribute>(Source: TClass): T; overload; static;
    class function FindAttribute<T: TCustomAttribute>(Source: TRttiObject): T; overload; static;
  end;

type
  TUtilities = record
  private
  class var
    FAppName: string;
  public
    class function GetExeName: string; static;
    class procedure Postpone(AProc: TThreadMethod; ADelayMS: Cardinal = 0); overload; static;
    class procedure Postpone(AProc: TThreadProcedure; ADelayMS: Cardinal = 0); overload; static;
    class function UserAppDataPath: string; static;
    class function UserDocumentsPath: string; static;
    class property AppName: string read FAppName write FAppName;
  end;

implementation

uses
  System.Threading, System.IOUtils;

function TComponentHelper.FindComponentOf<T>(const AName: string): T;
begin
  result := FindComponent(AName) as T;
end;

procedure TComponentHelper.ForAllComponentsOf<T>(DoProc: TProc<T>);
var
  cmp: TComponent;
begin
  for cmp in Self do
    if cmp is T then
      DoProc(T(cmp));
end;

class function TRttiHelper.FindAttribute<T>(Source: TClass): T;
var
  context: TRttiContext;
  myType: TRttiType;
begin
  Result := nil;
  context := TRttiContext.Create;
  try
    myType := context.GetType(Source);
    if myType <> nil then begin
      Result := FindAttribute<T>(myType);
    end;
  finally
    context.Free;
  end;
end;

class function TRttiHelper.FindAttribute<T>(Source: TRttiObject): T;
var
  attr: TCustomAttribute;
  attributes: TArray<TCustomAttribute>;
begin
  Result := nil;
  attributes := Source.GetAttributes;
  for attr in attributes do begin
    if attr is T then begin
      Result := T(attr);
      Break;
    end;
  end;
end;

class function TUtilities.GetExeName: string;
begin
  Result := ParamStr(0);
end;

class procedure TUtilities.Postpone(AProc: TThreadMethod; ADelayMS: Cardinal = 0);
begin
{ while ForceQueue was already introduced in 10.2 it misses the delay parameter before 10.4 }
{$IF CompilerVersion < 34.0 Delphi 10.4 Sydney }
  TTask.Run(
    procedure
    begin
      if ADelayMS > 0 then begin
        Sleep(ADelayMS);
      end;
      TThread.Queue(nil, AProc);
    end);
{$ELSE}
  TThread.ForceQueue(nil, AProc, ADelayMS);
{$ENDIF}
end;

class procedure TUtilities.Postpone(AProc: TThreadProcedure; ADelayMS: Cardinal = 0);
begin
{$IF CompilerVersion < 34.0 Delphi 10.4 Sydney }
  TTask.Run(
    procedure
    begin
      if ADelayMS > 0 then begin
        Sleep(ADelayMS);
      end;
      TThread.Queue(nil, AProc);
    end);
{$ELSE}
  TThread.ForceQueue(nil, AProc, ADelayMS);
{$ENDIF}
end;

class function TUtilities.UserAppDataPath: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, AppName);
end;

class function TUtilities.UserDocumentsPath: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, AppName);
end;

end.
