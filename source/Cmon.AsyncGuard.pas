unit Cmon.AsyncGuard;

interface

uses
  System.SyncObjs;

type
  ICancel = interface
    procedure Cancel;
    function IsCanceled: Boolean;
  end;

type
  TAsyncTask = class
  private
    FIsCanceled: Boolean;
  strict protected
    procedure Canceled; virtual;
    procedure InternalExecute; virtual; abstract;
    function IsMainThread: Boolean;
  public
    procedure Cancel;
    procedure Execute;
    property IsCanceled: Boolean read FIsCanceled;
  end;

type
  TAsyncTask<T: IInterface> = class(TAsyncTask)
  strict private
    FMRSW: TLightweightMREW;
    FTarget: T;
  strict protected
    procedure Canceled; override;
    procedure ClearTarget;
    function GetTarget: T;
    function HasTarget(out ATarget: T): Boolean;
  public
    constructor Create(ATarget: T);
  end;

type
  TAsyncGuard = class
  strict private
  type
    TCancel = class(TInterfacedObject, ICancel)
    private
      FGuard: TAsyncGuard;
    strict protected
      property Guard: TAsyncGuard read FGuard implements ICancel;
    public
      constructor Create(AGuard: TAsyncGuard);
      destructor Destroy; override;
    end;
  strict private
    FTask: TAsyncTask;
  strict protected
    procedure Cancel;
    procedure ExecuteSync(ACancel: ICancel); overload;
    procedure ExecuteAsync(ACancel: ICancel); overload;
    function IsCanceled: Boolean;
    property Task: TAsyncTask read FTask;
  public
    constructor Create(ATask: TAsyncTask);
    destructor Destroy; override;
    class procedure ExecuteSync(ATask: TAsyncTask; out ACancel: ICancel); overload;
    class procedure ExecuteAsync(ATask: TAsyncTask; out ACancel: ICancel); overload;
  end;

implementation

uses
  System.Classes, System.Threading, System.SysUtils;

constructor TAsyncGuard.Create(ATask: TAsyncTask);
begin
  inherited Create;
  if ATask = nil then
    raise EArgumentException.Create('Task must not be nil!');
  FTask := ATask;
end;

destructor TAsyncGuard.Destroy;
begin
  FTask.Free;
  inherited Destroy;
end;

procedure TAsyncGuard.Cancel;
begin
  Task.Cancel;
end;

class procedure TAsyncGuard.ExecuteSync(ATask: TAsyncTask; out ACancel: ICancel);
begin
  var instance := Self.Create(ATask);
  { TCancel is responsible for destroing instance }
  ACancel := TCancel.Create(instance);
  instance.ExecuteSync(ACancel);
end;

procedure TAsyncGuard.ExecuteSync(ACancel: ICancel);
begin
  Task.Execute;
end;

procedure TAsyncGuard.ExecuteAsync(ACancel: ICancel);
begin
  TTask.Run(
    procedure
    begin
      { capture ACancel to keep the current during the lifetime of the async method }
      if not ACancel.IsCanceled then
        Task.Execute;
    end);
end;

class procedure TAsyncGuard.ExecuteAsync(ATask: TAsyncTask; out ACancel: ICancel);
begin
  var instance := Self.Create(ATask);
  { TCancel is responsible for destroing instance }
  ACancel := TCancel.Create(instance);
  instance.ExecuteAsync(ACancel);
end;

function TAsyncGuard.IsCanceled: Boolean;
begin
  Result := Task.IsCanceled;
end;

constructor TAsyncGuard.TCancel.Create(AGuard: TAsyncGuard);
begin
  inherited Create;
  FGuard := AGuard;
end;

destructor TAsyncGuard.TCancel.Destroy;
begin
  FGuard.Free;
  inherited Destroy;
end;

procedure TAsyncTask.Cancel;
begin
  if not FIsCanceled then begin
    FIsCanceled := True;
    Canceled;
  end;
end;

procedure TAsyncTask.Canceled;
begin
end;

procedure TAsyncTask.Execute;
begin
  if not IsCanceled then
    InternalExecute;
end;

function TAsyncTask.IsMainThread: Boolean;
begin
  Result := (TThread.CurrentThread.ThreadID = MainThreadID);
end;

constructor TAsyncTask<T>.Create(ATarget: T);
begin
  inherited Create;
  FTarget := ATarget;
end;

procedure TAsyncTask<T>.Canceled;
begin
  ClearTarget;
end;

procedure TAsyncTask<T>.ClearTarget;
begin
  FMRSW.BeginWrite;
  try
    { TODO : This can execute destructor code inside the current thread! }
    FTarget := nil;
  finally
    FMRSW.EndWrite;
  end;
end;

function TAsyncTask<T>.GetTarget: T;
begin
  FMRSW.BeginRead;
  try
    Result := FTarget;
  finally
    FMRSW.EndRead;
  end;
end;

function TAsyncTask<T>.HasTarget(out ATarget: T): Boolean;
begin
  if IsCanceled then begin
    ClearTarget;
    Exit(False);
  end;
  ATarget := GetTarget;
  Result := (ATarget <> nil);
end;

end.
