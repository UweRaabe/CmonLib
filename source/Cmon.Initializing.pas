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
  strict private
  class var
    FAutoExecute: Boolean;
  public
    /// <summary>
    ///   Adds a procedure to be called during <i>Application.Initialize</i>.
    /// </summary>
    /// <param name="AProc">
    ///   The procedure to be called
    /// </param>
    /// <param name="APriority">
    ///   Determines when the procedure is called.
    /// </param>
    /// <remarks>
    ///   The procedures added with this method are called either manually with
    ///   <see cref="Cmon.Initializing|TInitialize.Execute">Execute</see> or
    ///   automatically inside <i>Application.Initialize</i> (controlled by the
    ///   setting of <see cref="Cmon.Initializing|TInitialize.AutoExecute">
    ///   AutoExecute</see>) in the order of the given priority. Procedures
    ///   with the same priority are called in the order they were added.
    /// </remarks>
    class procedure AddInitProc(AProc: TProc; APriority: TInitPriority = TInitPriority.Normal); static;
    /// <summary>
    ///   Calls all registered procedures.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     If <see cref="Cmon.Initializing|TInitialize.AutoExecute">
    ///     AutoExecute</see> is True (default), this is automatically done
    ///     during <i>Application.Initialize</i>.
    ///   </para>
    ///   <para>
    ///     A call to <i>Execute</i> removes any called procedure, so calling
    ///     it multiple times makes no difference.
    ///   </para>
    /// </remarks>
    class procedure Execute; static;
    /// <summary>
    ///   Controls whether all registered procedures are called automatically
    ///   during <i>Application.Initialize</i>
    /// </summary>
    /// <remarks>
    ///   Default is True. If set to False before <i>Application.Initialize</i>
    ///   , <see cref="Cmon.Initializing|TInitialize.Execute">Execute</see> has
    ///   to be called manually when suitable.
    /// </remarks>
    class property AutoExecute: Boolean read FAutoExecute write FAutoExecute;
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

class procedure TInitialize.Execute;
begin
  InitProcHandler.Execute;
end;

destructor TInitProcHandler.Destroy;
begin
  { This should never be necessary when Execute was called before }
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
  if TInitialize.AutoExecute then
    TInitialize.Execute;
end;

initialization
  TInitialize.AutoExecute := True;
  InitProcHandler := TInitProcHandler.Create;
  SaveInitProc := InitProc;
  InitProc := @InitApplication;
finalization
  InitProcHandler.Free;
end.
