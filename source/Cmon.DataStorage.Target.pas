unit Cmon.DataStorage.Target;

interface

uses
  System.Messaging,
  Cmon.DataStorage.Types;

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
  TStorageTargetHandler<T: TCustomStorageTarget> = class
  private
    FMessageID: Integer;
    FListMessageID: Integer;
  strict protected
    procedure StorageTargetMessage(const Sender: TObject; const M: TMessage);
    procedure StorageTargetListMessage(const Sender: TObject; const M: TMessage);
    procedure Subscribe; virtual;
    procedure Unsubscribe; virtual;
  protected
    function CheckFileName(var AFileName: string): Boolean; virtual;
  public
    constructor Create;
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
  Subscribe;
end;

destructor TStorageTargetHandler<T>.Destroy;
begin
  Unsubscribe;
  inherited;
end;

function TStorageTargetHandler<T>.CheckFileName(var AFileName: string): Boolean;
begin
  AFileName := T.EffectiveFileName(AFileName);
  Result := TFile.Exists(AFileName);
end;

procedure TStorageTargetHandler<T>.StorageTargetMessage(const Sender: TObject; const M: TMessage);
begin
  var msg := M as TStorageTargetMessage;

  { if we already have a result skip this handler }
  if msg.StorageTarget <> nil then Exit;

  { get the extension to check if this handler is suitable }
  var ext := TPath.GetExtension(msg.FileName);
  if ext.IsEmpty then
    ext := TCustomStorageTarget.DefaultFileExtension;
  if ext.IsEmpty or SameText(ext, T.FileExtension) then begin
    var fileName := msg.Value;
    if not CheckFileName(fileName) then
      TStorageTargetMissingMessage.SendMessage(Sender, fileName);

    msg.StorageTarget := T.Create(fileName);
  end;
end;

procedure TStorageTargetHandler<T>.StorageTargetListMessage(const Sender: TObject; const M: TMessage);
begin
  var msg := M as TStorageTargetListMessage;
  msg.Value.Add(TStorageTargetDescriptor.Create(T.Description, T.FileExtension));
end;

procedure TStorageTargetHandler<T>.Subscribe;
begin
  FMessageID := TStorageTargetMessage.Subscribe(StorageTargetMessage);
  FListMessageID := TStorageTargetListMessage.Subscribe(StorageTargetListMessage);
end;

procedure TStorageTargetHandler<T>.Unsubscribe;
begin
  TStorageTargetMessage.Unsubscribe(FListMessageID);
  TStorageTargetMessage.Unsubscribe(FMessageID);
end;

end.
