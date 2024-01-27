unit Cmon.AsyncGuard;

interface

type
  ICancel = interface
    procedure Cancel;
    function IsCancelled: Boolean;
  end;

type
  TAsyncGuard = class
  type
    TCancel = class(TInterfacedObject, ICancel)
    private
      FTask: TAsyncGuard;
    strict protected
      property Task: TAsyncGuard read FTask implements ICancel;
    public
      constructor Create(ATask: TAsyncGuard);
      destructor Destroy; override;
    end;
  strict private
    FIsCancelled: Boolean;
  strict protected
    procedure Cancel;
    procedure Execute(ACancel: ICancel); overload; virtual;
    procedure Execute; overload; virtual; abstract;
    procedure ExecuteAsync(ACancel: ICancel);
    function IsCancelled: Boolean;
  public
    class procedure Execute<T: TAsyncGuard>(AInstance: T; out ACancel: ICancel); overload;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Threading;

procedure TAsyncGuard.Cancel;
begin
  FIsCancelled := True;
end;

class procedure TAsyncGuard.Execute<T>(AInstance: T; out ACancel: ICancel);
begin
  { TCancel is responsible for destroing instance }
  ACancel := TCancel.Create(AInstance);
  Ainstance.Execute(ACancel);
end;

procedure TAsyncGuard.Execute(ACancel: ICancel);
begin
  Execute;
end;

procedure TAsyncGuard.ExecuteAsync(ACancel: ICancel);
begin
  TTask.Run(
    procedure
    begin
      { capture ACancel to keep the current during the lifetime of the async method }
      if not ACancel.IsCancelled then
        Execute;
    end);
end;

function TAsyncGuard.IsCancelled: Boolean;
begin
  Result := FIsCancelled;
end;

constructor TAsyncGuard.TCancel.Create(ATask: TAsyncGuard);
begin
  inherited Create;
  FTask := ATask;
end;

destructor TAsyncGuard.TCancel.Destroy;
begin
  FTask.Free;
  inherited Destroy;
end;

end.
