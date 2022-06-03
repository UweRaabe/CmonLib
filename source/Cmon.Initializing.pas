unit Cmon.Initializing;

interface

uses
  System.SysUtils;

type
  /// <summary>
  ///   The order in which each group of procedures with the same priority is
  ///   called.
  /// </summary>
  TInitPriority = (VeryEarly, Early, Normal, Late, VeryLate);
  TInitialize = record
  public
    /// <summary>
    ///   Adds a procedure to be called during Application.Initialize.
    /// </summary>
    /// <param name="AProc">
    ///   The procedure to be called
    /// </param>
    /// <param name="APriority">
    ///   Determines when the procedure is called.
    /// </param>
    /// <remarks>
    ///   The procedures added with this method are called inside
    ///   Application.Initialize in the order of the given priority. Procedures
    ///   with the same priority are called in the order they were added.
    /// </remarks>
    class procedure AddInitProc(AProc: TProc; APriority: TInitPriority = TInitPriority.Normal); static;
  end;

implementation

uses
  System.Generics.Collections;

type
  TInitProcHandler = class
  type
    TProcList = TList<TProc>;
  private
    FInitProcs: array[TInitPriority] of TProcList;
  public
    destructor Destroy; override;
    procedure AddInitProc(AProc: TProc; APriority: TInitPriority);
    procedure Execute;
  end;

var
  InitProcHandler: TInitProcHandler = nil;

class procedure TInitialize.AddInitProc(AProc: TProc; APriority: TInitPriority);
begin
  InitProcHandler.AddInitProc(AProc, APriority);
end;

destructor TInitProcHandler.Destroy;
begin
  for var prio := Low(FInitProcs) to High(FInitProcs) do
    FInitProcs[prio].Free;
  inherited;
end;

procedure TInitProcHandler.AddInitProc(AProc: TProc; APriority: TInitPriority);
begin
  if FInitProcs[APriority] = nil then
    FInitProcs[APriority] := TProcList.Create;

  FInitProcs[APriority].Add(AProc);
end;

procedure TInitProcHandler.Execute;
begin
  for var prio := Low(FInitProcs) to High(FInitProcs) do begin
    var list := FInitProcs[prio];
    if list = nil then Continue;
    for var proc in list do
      proc();
    FInitProcs[prio].Free;
    FInitProcs[prio] := nil;
  end;
end;

var
  SaveInitProc: Pointer = nil;

{ will be called in Application.Initialize after all other initialization code has been executed }
procedure InitApplication;
begin
  if SaveInitProc <> nil then TProcedure(SaveInitProc);
  InitProcHandler.Execute;
end;

initialization
  InitProcHandler := TInitProcHandler.Create;
  SaveInitProc := InitProc;
  InitProc := @InitApplication;
finalization
  InitProcHandler.Free;
end.
