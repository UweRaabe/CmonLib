unit Cmon.Utilities;

interface

uses
  System.Classes, System.SysUtils;

type
  EValidationError = class(Exception);

type
{$SCOPEDENUMS ON}
  TEnumDirection = (Normal, Reverse);
{$SCOPEDENUMS OFF}
  TEnumWrapper<T: class> = record
  type
    TGetItemFunc = TFunc<Integer, TObject>;
    TEnumerator = record
    private
      FIndex: Integer;
      FCount: Integer;
      FCurrent: T;
      FDirection: TEnumDirection;
      FGetItem: TGetItemFunc;
    public
      function MoveNext: Boolean; inline;
      property Current: T read FCurrent;
    end;
  private
    FCount: Integer;
    FDirection: TEnumDirection;
    FGetItem: TGetItemFunc;
  public
    constructor Create(ACount: Integer; AGetItem: TGetItemFunc; ADirection: TEnumDirection);
    function GetEnumerator: TEnumerator; inline;
  end;

type
  TComponentHelper = class helper for TComponent
  public
    function ComponentsOf<T: TComponent>(ADirection: TEnumDirection = TEnumDirection.Normal): TEnumWrapper<T>; inline;
    function FindComponentOf<T: TComponent>(const AName: string): T; inline;
  end;

type
  TCollectionHelper = class helper for TCollection
  public
    function ItemsOf<T: TCollectionItem>(ADirection: TEnumDirection = TEnumDirection.Normal): TEnumWrapper<T>; inline;
  end;

type
  TUtilities = record
  private
  class var
    FAppName: string;
    FCompanyName: string;
  public
    class function CommonAppDataPath: string; overload; static; inline;
    class function CommonAppDataPath(const AFileName: string): string; overload; static; inline;
    class function CommonDocumentsPath: string; overload; static; inline;
    class function CommonDocumentsPath(const AFileName: string): string; overload; static; inline;
    class function GetCompanyAppSubPath: string; static;
    class function GetExeName: string; static; inline;
    class procedure Postpone(AProc: TThreadMethod; ADelayMS: Cardinal = 0); overload; static;
    class procedure Postpone(AProc: TThreadProcedure; ADelayMS: Cardinal = 0); overload; static;
    class function StripFolder(const AFolder, Value: string): string; overload; static;
    class function StripFolder(const AFolder: string; const Values: TArray<string>): TArray<string>; overload; static;
    class function UserAppDataPath: string; overload; static; inline;
    class function UserAppDataPath(const AFileName: string): string; overload; static; inline;
    class function UserDocumentsPath: string; overload; static; inline;
    class function UserDocumentsPath(const AFileName: string): string; overload; static; inline;
    class property AppName: string read FAppName write FAppName;
    class property CompanyName: string read FCompanyName write FCompanyName;
  end;

implementation

uses
  System.Threading, System.IOUtils;

{ TUtilities }

class function TUtilities.CommonAppDataPath: string;
begin
  Result := TPath.Combine(TPath.GetPublicPath, GetCompanyAppSubPath);
end;

class function TUtilities.CommonAppDataPath(const AFileName: string): string;
begin
  Result := TPath.Combine(CommonAppDataPath, AFileName);
end;

class function TUtilities.CommonDocumentsPath: string;
begin
  Result := TPath.Combine(TPath.GetSharedDocumentsPath, GetCompanyAppSubPath);
end;

class function TUtilities.CommonDocumentsPath(const AFileName: string): string;
begin
  Result := TPath.Combine(CommonDocumentsPath, AFileName);
end;

class function TUtilities.GetCompanyAppSubPath: string;
begin
  Result := TPath.Combine(CompanyName, AppName);
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

class function TUtilities.StripFolder(const AFolder, Value: string): string;
begin
  if Value.StartsWith(AFolder, True) then
    Result := Value.Remove(0, AFolder.Length + 1);
end;

class function TUtilities.StripFolder(const AFolder: string; const Values: TArray<string>): TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, Length(Values));
  for I := 0 to Length(Values) - 1 do
    Result[I] := StripFolder(AFolder, Values[I]);
end;

class function TUtilities.UserAppDataPath: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, GetCompanyAppSubPath);
end;

class function TUtilities.UserAppDataPath(const AFileName: string): string;
begin
  Result := TPath.Combine(UserAppDataPath, AFileName);
end;

class function TUtilities.UserDocumentsPath: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, GetCompanyAppSubPath);
end;

class function TUtilities.UserDocumentsPath(const AFileName: string): string;
begin
  Result := TPath.Combine(UserDocumentsPath, AFileName);
end;

{ TEnumWrapper<T> }

constructor TEnumWrapper<T>.Create(ACount: Integer; AGetItem: TGetItemFunc; ADirection: TEnumDirection);
begin
  FCount := ACount;
  FGetItem := AGetItem;
  FDirection := ADirection;
end;

function TEnumWrapper<T>.GetEnumerator: TEnumerator;
begin
  Result.FCount := FCount;
  Result.FDirection := FDirection;
  Result.FGetItem := FGetItem;
  Result.FIndex := -1;
end;

function TEnumWrapper<T>.TEnumerator.MoveNext: Boolean;
var
  cmp: TObject;
  idx: Integer;
begin
  while True do begin
    Inc(FIndex);
    if FIndex < FCount then
    begin
      case FDirection of
        TEnumDirection.Normal: idx := FIndex;
        TEnumDirection.Reverse: idx := FCount - FIndex - 1;
      else
        raise EProgrammerNotFound.Create('unhandled case');
      end;
      cmp := FGetItem(idx);
      if cmp.InheritsFrom(T) then
      begin
        FCurrent := T(cmp);
        Exit(True);
      end;
    end
    else
      Exit(False);
  End;
end;

{ TComponentHelper }

function TComponentHelper.ComponentsOf<T>(ADirection: TEnumDirection = TEnumDirection.Normal): TEnumWrapper<T>;
begin
  Result := TEnumWrapper<T>.Create(ComponentCount,
    function(Index: Integer): TObject
    begin
      Result := Components[Index];
    end,
    ADirection);
end;

function TComponentHelper.FindComponentOf<T>(const AName: string): T;
begin
  result := FindComponent(AName) as T;
end;

{ TCollectionHelper }

function TCollectionHelper.ItemsOf<T>(ADirection: TEnumDirection = TEnumDirection.Normal): TEnumWrapper<T>;
begin
  Result := TEnumWrapper<T>.Create(Count,
    function(Index: Integer): TObject
    begin
      Result := Items[Index];
    end,
    ADirection);
end;

end.
