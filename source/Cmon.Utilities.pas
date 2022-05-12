unit Cmon.Utilities;

interface

uses
  System.Classes, System.SysUtils, System.Rtti;

type
  EValidationError = class(Exception);

type
  TEnumWrapper<T: class> = record
  type
    TGetItemFunc = TFunc<Integer, TObject>;
    TEnumerator = record
    private
      FIndex: Integer;
      FCount: Integer;
      FCurrent: T;
      FGetItem: TGetItemFunc;
    public
      function MoveNext: Boolean; inline;
      property Current: T read FCurrent;
    end;
  private
    FCount: Integer;
    FGetItem: TGetItemFunc;
  public
    constructor Create(ACount: Integer; AGetItem: TGetItemFunc);
    function GetEnumerator: TEnumerator; inline;
  end;

type
  TComponentHelper = class helper for TComponent
  public
    function ComponentsOf<T: TComponent>: TEnumWrapper<T>; inline;
    function FindComponentOf<T: TComponent>(const AName: string): T; inline;
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
    class function GetExeName: string; static; inline;
    class procedure Postpone(AProc: TThreadMethod; ADelayMS: Cardinal = 0); overload; static;
    class procedure Postpone(AProc: TThreadProcedure; ADelayMS: Cardinal = 0); overload; static;
    class function UserAppDataPath: string; static; inline;
    class function UserDocumentsPath: string; static; inline;
    class property AppName: string read FAppName write FAppName;
  end;

implementation

uses
  System.Threading, System.IOUtils;

{ TRttiHelper }

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

{ TUtilities }

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

{ TEnumWrapper<T> }

constructor TEnumWrapper<T>.Create(ACount: Integer; AGetItem: TGetItemFunc);
begin
  FCount := ACount;
  FGetItem := AGetItem;
end;

function TEnumWrapper<T>.GetEnumerator: TEnumerator;
begin
  Result.FCount := FCount;
  Result.FGetItem := FGetItem;
  Result.FIndex := -1;
end;

function TEnumWrapper<T>.TEnumerator.MoveNext: Boolean;
var
  cmp: TObject;
begin
  repeat
    Inc(FIndex);
    if FIndex < FCount then
    begin
      cmp := FGetItem(FIndex);
      if cmp.InheritsFrom(T) then
      begin
        FCurrent := T(cmp);
        Exit(True);
      end;
      Continue;
    end;
  until True;
  Result := False;
end;

{ TComponentHelper }

function TComponentHelper.ComponentsOf<T>: TEnumWrapper<T>;
begin
  Result := TEnumWrapper<T>.Create(ComponentCount,
    function(Index: Integer): TObject
    begin
      Result := Components[Index];
    end);
end;

function TComponentHelper.FindComponentOf<T>(const AName: string): T;
begin
  result := FindComponent(AName) as T;
end;

end.
