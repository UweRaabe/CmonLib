unit Cmon.DataStorage.JSON;

interface

uses
  System.JSON, System.Classes, System.SysUtils,
  Cmon.DataStorage, Cmon.DataStorage.Target;

type
  TJSONStorageTarget = class(TCustomStorageTarget)
  private
    FJson: TJSONObject;
    FModified: Boolean;
    function FindOrCreateKey(const Key: string): TJSONObject;
    function FindKey(const Key: string): TJSONObject;
    function FindValue(const Key, Ident: string): TJSONValue;
    procedure SetJson(const Value: TJSONObject);
  strict protected
    procedure DeleteKey(const Key, Ident: string); override;
    procedure EraseStorageKey(const Key: string); override;
    function InternalLoadBytes(const AFileName: string): TBytes; virtual;
    procedure InternalSaveBytes(const AFileName: string; const ABytes: TBytes); virtual;
    procedure ReadKey(const Key: string; Target: TStrings); override;
    function ReadString(const Key: string; const Ident: string; const Default: string): string; override;
    function ReadBoolean(const Key: string; const Ident: string; const Default: Boolean): Boolean; override;
    function ReadFloat(const Key: string; const Ident: string; const Default: Double): Double; override;
    function ReadInteger(const Key: string; const Ident: string; const Default: Integer): Integer; override;
    procedure WriteBoolean(const Key: string; const Ident: string; const Value: Boolean); override;
    procedure WriteFloat(const Key: string; const Ident: string; const Value: Double); override;
    procedure WriteInteger(const Key: string; const Ident: string; const Value: Integer); override;
    procedure WriteString(const Key: string; const Ident: string; const Value: string); override;
    function ValueExists(const Key, Ident: string): Boolean; override;
  public
    destructor Destroy; override;
    class function Description: string; override;
    class function FileExtension: string; override;
    procedure LoadFromFile(const AFileName: string); override;
    procedure SaveToFile(const AFileName: string); override;
    property JSON: TJSONObject read FJson write SetJson;
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
  System.IOUtils, System.Generics.Collections,
  Cmon.Utilities, Cmon.Messaging, Cmon.Initializing;

resourcestring
  SJSONFiles = 'JSON files';

type
  TJsonObjectHelper = class helper for TJSONObject
  public
    procedure WriteIdent(const AIdent, AValue: string); overload;
    function FindOrCreateKey(const AKey: string): TJSONObject;
    function FindKey(const AKey: string): TJSONObject;
    procedure WriteIdent(const AIdent: string; const AValue: Boolean); overload;
    procedure WriteIdent(const AIdent: string; const AValue: Integer); overload;
    procedure WriteIdent(const AIdent: string; const AValue: Double); overload;
  end;

destructor TJSONStorageTarget.Destroy;
begin
  FJson.Free;
  FJson := nil;
  inherited Destroy;
end;

procedure TJSONStorageTarget.DeleteKey(const Key, Ident: string);
begin
  var node := FindKey(Key);
  if node <> nil then begin
    var temp := node.Get(Ident);
    if temp <> nil then begin
      temp.Free;
      Modified := True;
    end;
  end;
end;

class function TJSONStorageTarget.Description: string;
begin
  Result := SJSONFiles;
end;

procedure TJSONStorageTarget.EraseStorageKey(const Key: string);
begin
  var node := FindKey(Key);
  if node <> nil then begin
    node.Free;
    Modified := True;
  end;
end;

class function TJSONStorageTarget.FileExtension: string;
begin
  Result := '.json';
end;

function TJSONStorageTarget.FindOrCreateKey(const Key: string): TJSONObject;
begin
  if Json = nil then
    Json := TJSONObject.Create;
  var node := Json;
  for var subKey in TDataStorage.SplitStorageKey(Key) do
    node := node.FindOrCreateKey(subKey);
  Result := node;
end;

function TJSONStorageTarget.FindKey(const Key: string): TJSONObject;
begin
  Result := nil;
  var node := Json;
  if node = nil then Exit;
  for var subKey in TDataStorage.SplitStorageKey(Key) do begin
    node := node.FindKey(subKey);
    if node = nil then Exit;
  end;
  Result := node;
end;

function TJSONStorageTarget.FindValue(const Key, Ident: string): TJSONValue;
begin
  Result := nil;
  var node := FindKey(Key);
  if node <> nil then
    Result := node.Values[Ident];
end;

function TJSONStorageTarget.InternalLoadBytes(const AFileName: string): TBytes;
begin
  Result := TFile.ReadAllBytes(AFileName);
end;

procedure TJSONStorageTarget.InternalSaveBytes(const AFileName: string; const ABytes: TBytes);
begin
  TFile.WriteAllBytes(AFileName, ABytes);
end;

procedure TJSONStorageTarget.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
    Json := TJSONValue.ParseJSONValue(InternalLoadBytes(AFileName), 0) as TJSONObject
  else
    Json := TJSONObject.Create;
  Modified := False;
  inherited;
end;

function TJSONStorageTarget.ReadBoolean(const Key, Ident: string; const Default: Boolean): Boolean;
begin
  Result := Default;
  var node := FindValue(Key, Ident);
  if node <> nil then
    Result := node.AsType<Boolean>;
end;

function TJSONStorageTarget.ReadFloat(const Key, Ident: string; const Default: Double): Double;
begin
  Result := Default;
  var node := FindValue(Key, Ident);
  if node <> nil then
    Result := node.AsType<Double>;
end;

function TJSONStorageTarget.ReadInteger(const Key, Ident: string; const Default: Integer): Integer;
begin
  Result := Default;
  var node := FindValue(Key, Ident);
  if node <> nil then
    Result := node.AsType<Integer>;
end;

procedure TJSONStorageTarget.ReadKey(const Key: string; Target: TStrings);
begin
  inherited;
  var node := FindKey(Key);
  if node <> nil then begin
    for var pair in node do
      Target.AddPair(pair.JsonString.Value, pair.JsonValue.Value);
  end;
end;

function TJSONStorageTarget.ReadString(const Key, Ident, Default: string): string;
begin
  Result := Default;
  var node := FindValue(Key, Ident);
  if node <> nil then
    Result := node.AsType<string>;
end;

procedure TJSONStorageTarget.SaveToFile(const AFileName: string);
begin
  if Modified and (Json <> nil) then
    InternalSaveBytes(AFileName, TEncoding.UTF8.GetBytes(Json.Format(4)));
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

function TJSONStorageTarget.ValueExists(const Key, Ident: string): Boolean;
begin
  Result := FindValue(Key, Ident) <> nil;
end;

procedure TJSONStorageTarget.WriteBoolean(const Key, Ident: string; const Value: Boolean);
begin
  var node := FindOrCreateKey(Key);
  node.WriteIdent(Ident, Value);
  Modified := True;
end;

procedure TJSONStorageTarget.WriteFloat(const Key, Ident: string; const Value: Double);
begin
  var node := FindOrCreateKey(Key);
  node.WriteIdent(Ident, Value);
  Modified := True;
end;

procedure TJSONStorageTarget.WriteInteger(const Key, Ident: string; const Value: Integer);
begin
  var node := FindOrCreateKey(Key);
  node.WriteIdent(Ident, Value);
  Modified := True;
end;

procedure TJSONStorageTarget.WriteString(const Key, Ident, Value: string);
begin
  var node := FindOrCreateKey(Key);
  node.WriteIdent(Ident, Value);
  Modified := True;
end;

procedure TJsonObjectHelper.WriteIdent(const AIdent, AValue: string);
begin
  var pair := Get(AIdent);
  if pair = nil then
    AddPair(AIdent, AValue)
  else
    pair.JsonValue := TJSONString.Create(AValue);
end;

function TJsonObjectHelper.FindOrCreateKey(const AKey: string): TJSONObject;
begin
  var pair := Get(AKey);
  if pair = nil then begin
    Result := TJSONObject.Create;
    AddPair(AKey, Result);
  end
  else if not (pair.JsonValue is TJSONObject) then begin
    Result := TJSONObject.Create;
    pair.JsonValue := Result;
  end
  else
    Result := pair.JsonValue as TJSONObject
end;

function TJsonObjectHelper.FindKey(const AKey: string): TJSONObject;
begin
  Result := nil;
  var pair := Get(AKey);
  if (pair <> nil) and (pair.JsonValue is TJSONObject) then
    Result := pair.JsonValue as TJSONObject;
end;

procedure TJsonObjectHelper.WriteIdent(const AIdent: string; const AValue: Boolean);
begin
  var pair := Get(AIdent);
  if pair = nil then
    AddPair(AIdent, AValue)
  else
    pair.JsonValue := TJSONBool.Create(AValue);
end;

procedure TJsonObjectHelper.WriteIdent(const AIdent: string; const AValue: Integer);
begin
  var pair := Get(AIdent);
  if pair = nil then
    AddPair(AIdent, AValue)
  else
    pair.JsonValue := TJSONNumber.Create(AValue);
end;

procedure TJsonObjectHelper.WriteIdent(const AIdent: string; const AValue: Double);
begin
  var pair := Get(AIdent);
  if pair = nil then
    AddPair(AIdent, AValue)
  else
    pair.JsonValue := TJSONNumber.Create(AValue);
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
