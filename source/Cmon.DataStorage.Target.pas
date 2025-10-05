unit Cmon.DataStorage.Target;
{$IF CompilerVersion < 35.0 Delphi 11 Alexandria }
  I'm sorry, but this unit is currently not compatible with your Delphi version
{$ENDIF}

interface

uses
  Cmon.Messaging, Cmon.DataStorage.Types;

type
  TCustomStorageTarget = class(TAbstractStorageTarget)
  private
    FFileName: string;
  class var
    FDefaultFileExtension: string;
    FDefaultFileName: string;
    FDefaultFilePath: string;
  public
    constructor Create(const AFileName: string = ''); virtual;
    class function Description: string; virtual; abstract;
    class function FileExtension: string; virtual; abstract;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function EffectiveFileName(const AFileName: string): string;
    procedure LoadFromFile(const AFileName: string); virtual;
    procedure SaveToFile(const AFileName: string); virtual;
    class property DefaultFileExtension: string read FDefaultFileExtension write FDefaultFileExtension;
    class property DefaultFileName: string read FDefaultFileName write FDefaultFileName;
    class property DefaultFilePath: string read FDefaultFilePath write FDefaultFilePath;
    property FileName: string read FFileName;
  end;

type
  TStorageTargetHandler<T: TCustomStorageTarget> = class(TCommonMessageHandler<TStorageTargetMessage>)
  private
    FListHandler: TCommonMessageHandler;
  strict protected
    procedure HandleMessage(const Sender: TObject; const Msg: TStorageTargetMessage); override;
    procedure HandleListMessage(const Sender: TObject; const Msg: TStorageTargetListMessage);
  protected
    function CheckFileName(var AFileName: string): Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  Cmon.Utilities;

procedure TCustomStorageTarget.AfterConstruction;
begin
  inherited;
  LoadFromFile(FileName);
end;

procedure TCustomStorageTarget.BeforeDestruction;
begin
  if not ReadOnly then
    SaveToFile(FileName);
  inherited;
end;

constructor TCustomStorageTarget.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := EffectiveFileName(AFileName);
end;

class function TCustomStorageTarget.EffectiveFileName(const AFileName: string): string;
begin
  Result := AFileName;
  if Result.IsEmpty or Result.Contains('*') then begin
    Result := DefaultFileName;
    if Result.IsEmpty then
      Result := TPath.ChangeExtension(TUtilities.GetExeName, FileExtension);
    if not DefaultFilePath.IsEmpty then
      Result := TPath.Combine(DefaultFilePath, TPath.GetFileName(Result));
  end;
end;

procedure TCustomStorageTarget.LoadFromFile(const AFileName: string);
begin
end;

procedure TCustomStorageTarget.SaveToFile(const AFileName: string);
begin
end;

constructor TStorageTargetHandler<T>.Create;
begin
  inherited;
  FListHandler := TCommonMessageHandler<TStorageTargetListMessage>.Create(HandleListMessage);
end;

destructor TStorageTargetHandler<T>.Destroy;
begin
  FListHandler.Free;
  inherited;
end;

function TStorageTargetHandler<T>.CheckFileName(var AFileName: string): Boolean;
begin
  AFileName := T.EffectiveFileName(AFileName);
  Result := TFile.Exists(AFileName);
end;

procedure TStorageTargetHandler<T>.HandleMessage(const Sender: TObject; const Msg: TStorageTargetMessage);
begin
  { if we already have a result skip this handler }
  if Msg.StorageTarget <> nil then Exit;

  { get the extension to check if this handler is suitable }
  var ext := TPath.GetExtension(Msg.FileName);
  if ext.IsEmpty then
    ext := TCustomStorageTarget.DefaultFileExtension;
  if ext.IsEmpty or SameText(ext, T.FileExtension) then begin
    var fileName := Msg.Value;
    if not CheckFileName(fileName) then
      TStorageTargetMissingMessage.SendMessage(Sender, fileName);

    Msg.StorageTarget := T.Create(fileName);
  end;
end;

procedure TStorageTargetHandler<T>.HandleListMessage(const Sender: TObject; const Msg: TStorageTargetListMessage);
begin
  Msg.Value.Add(TStorageTargetDescriptor.Create(T.Description, T.FileExtension));
end;

end.
