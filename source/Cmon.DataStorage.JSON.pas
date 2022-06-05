unit Cmon.DataStorage.JSON;

interface

uses
  System.JSON,
  Cmon.DataStorage, Cmon.DataStorage.Target;

type
  TJSONStorageTarget = class(TCustomStorageTarget)
  private
    FJson: TJSONObject;
    FModified: Boolean;
    procedure SetJson(const Value: TJSONObject);
  strict protected
    function ReadString(const Key: string; const Ident: string; const Default: string): string; override;
    procedure WriteString(const Key: string; const Ident: string; const Value: string); override;
  protected
  public
    destructor Destroy; override;
    class function Description: string; override;
    class function FileExtension: string; override;
    procedure LoadFromFile(const AFileName: string); override;
    procedure SaveToFile(const AFileName: string); override;
    property Json: TJSONObject read FJson write SetJson;
    property Modified: Boolean read FModified write FModified;
  end;

type
  TJSONStorageTargetHandler = class(TStorageTargetHandler<TJSONStorageTarget>)
  strict private
  class var
    FAutoRegisterHandler: Boolean;
  public
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  Cmon.Utilities, Cmon.Messaging, Cmon.Initializing;

resourcestring
  SJSONFiles = 'JSON files';

type
  TJsonObjectHelper = class helper for TJSONObject
  public
    function FindIdent(const AIdent: string): TJSONString;
    procedure WriteIdent(const AIdent, AValue: string);
    function FindKey(const AKey: string): TJSONObject;
    function FindOrCreateKey(const AKey: string): TJSONObject;
    function FindPair<T: class>(const AName: string): TJSONPair;
  end;

destructor TJSONStorageTarget.Destroy;
begin
  FJson.Free;
  FJson := nil;
  inherited Destroy;
end;

class function TJSONStorageTarget.Description: string;
begin
  Result := SJSONFiles;
end;

class function TJSONStorageTarget.FileExtension: string;
begin
  Result := '.json';
end;

procedure TJSONStorageTarget.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
    Json := TJSONValue.ParseJSONValue(TFile.ReadAllText(AFileName)) as TJSONObject
  else
    Json := TJSONObject.Create;
  Modified := False;
  inherited;
end;

function TJSONStorageTarget.ReadString(const Key, Ident, Default: string): string;
begin
  Result := Default;
  var node := Json;
  for var subKey in TDataStorage.SplitStorageKey(Key) do begin
    node := node.FindKey(subKey);
    if node = nil then Exit;
  end;
  if node <> nil then begin
    var temp := node.FindIdent(Ident);
    if temp <> nil then
      Result := temp.Value;
  end;
end;

procedure TJSONStorageTarget.SaveToFile(const AFileName: string);
begin
  if Modified and (Json <> nil) then
    TFile.WriteAllText(AFileName, Json.Format(4), TEncoding.UTF8);
  Modified := False;
  inherited;
end;

procedure TJSONStorageTarget.SetJson(const Value: TJSONObject);
begin
  if FJson <> Value then begin
    FJson.Free;
    FJson := Value;
  end;
end;

procedure TJSONStorageTarget.WriteString(const Key, Ident, Value: string);
begin
  var node := Json;
  for var subKey in TDataStorage.SplitStorageKey(Key) do
    node := node.FindOrCreateKey(subKey);
  node.WriteIdent(Ident, Value);
  Modified := True;
end;

function TJsonObjectHelper.FindIdent(const AIdent: string): TJSONString;
begin
  Result := nil;
  var pair := FindPair<TJSONString>(AIdent);
  if pair <> nil then
    Result := pair.JsonValue as TJSONString;
end;

procedure TJsonObjectHelper.WriteIdent(const AIdent, AValue: string);
begin
  var pair := FindPair<TJSONString>(AIdent);
  if pair = nil then
    AddPair(AIdent, AValue)
  else
    pair.JsonValue := TJSONString.Create(AValue);
end;

function TJsonObjectHelper.FindKey(const AKey: string): TJSONObject;
begin
  Result := nil;
  var pair := FindPair<TJSONObject>(AKey);
  if pair <> nil then
    Result := pair.JsonValue as TJSONObject;
end;

function TJsonObjectHelper.FindOrCreateKey(const AKey: string): TJSONObject;
begin
  Result := FindKey(AKey);
  if Result = nil then begin
    Result := TJSONObject.Create;
    AddPair(AKey, Result);
  end;
end;

function TJsonObjectHelper.FindPair<T>(const AName: string): TJSONPair;
begin
  Result := nil;
  for var I := 0 to Count - 1 do begin
    var pair := Pairs[I];
    if pair.JsonString.Equals(AName) and (pair.JsonValue is T) then
      Exit(pair);
  end;
end;

var
  Instance: TJSONStorageTargetHandler = nil;

{ will be called in Application.Initialize after all other initialization code has been executed }
procedure InitHandler;
begin
  if TDataStorage.AutoRegisterHandler and TJSONStorageTargetHandler.AutoRegisterHandler then
    Instance := TJSONStorageTargetHandler.Create;
end;

initialization
  TJSONStorageTargetHandler.AutoRegisterHandler := True;
  TInitialize.AddInitProc(InitHandler);
finalization
  Instance.Free;
end.
