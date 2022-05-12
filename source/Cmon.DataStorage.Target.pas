unit Cmon.DataStorage.Target;

interface

uses
  System.Messaging,
  Cmon.DataStorage;

type
  TCustomStorageTarget = class(TInterfacedObject, IStorageTarget)
  private
    FFileName: string;
  class var
    FDefaultFileExtension: string;
    FDefaultFileName: string;
    FDefaultFilePath: string;
  strict protected
    function ReadString(const Key: string; const Ident: string; const Default: string): string; virtual; abstract;
    procedure WriteString(const Key: string; const Ident: string; const Value: string); virtual; abstract;
  public
    constructor Create(const AFileName: string = ''); virtual;
    class function Description: string; virtual; abstract;
    class function FileExtension: string; virtual; abstract;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
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
  SaveToFile(FileName);
  inherited;
end;

constructor TCustomStorageTarget.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  if FFileName.IsEmpty or FFileName.Contains('*') then begin
    FFileName := DefaultFileName;
    if FFileName.IsEmpty then
      FFileName := TPath.ChangeExtension(TUtilities.GetExeName, FileExtension);
    if not DefaultFilePath.IsEmpty then
      FFileName := TPath.Combine(DefaultFilePath, TPath.GetFileName(FFileName));
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

procedure TStorageTargetHandler<T>.StorageTargetMessage(const Sender: TObject; const M: TMessage);
begin
  var msg := M as TStorageTargetMessage;

  { if we already have a result skip this handler }
  if msg.StorageTarget <> nil then Exit;

  { get the extension to check if this handler is suitable }
  var ext := TPath.GetExtension(msg.FileName);
  if ext.IsEmpty then
    ext := TCustomStorageTarget.DefaultFileExtension;
  if ext.IsEmpty or SameText(ext, T.FileExtension) then
    msg.StorageTarget := T.Create(msg.Value);
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
