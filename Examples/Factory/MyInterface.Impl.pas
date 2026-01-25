unit MyInterface.Impl;

interface

uses
  Cmon.Factory,
  MyInterface.Types;

{$DEFINE USEWRAPPER}

type
  { Using the wrapper allows the implementing class not to support reference counting.
    That may be helpful if it has to be derived from some other class. }
  TMyImplementor = class{$IFNDEF UseWrapper}(TInterfacedObject, IMyInterface){$ENDIF}
  public
    function ConvertNumber(Number: Integer): string;
  end;

{$IFDEF UseWrapper}
type
  TMyImplementorWrapper = class(TImplementorWrapper<TMyImplementor>, IMyInterface)
  strict protected
    property Implementor: TMyImplementor read GetInstance implements IMyInterface;
  end;
  TMyInterfaceImpl = TMyImplementorWrapper;
{$ELSE}
  TMyInterfaceImpl = TMyImplementor;
{$ENDIF}

type
  TMyContextClass = class(TInterfacedObject, IMyContextInterface)
  private
    FContext: TMyContext;
  strict protected
    function GetContextName: string;
  public
    constructor Create(AContext: TMyContext);
    property Context: TMyContext read FContext write FContext;
  end;

  TMySimpleContext = class(TMyContextClass)
  public
    constructor Create;
  end;

  TMyComplexContext = class(TMyContextClass)
  public
    constructor Create;
  end;

implementation

uses
  System.SysUtils, System.Rtti,
  MyInterface.Factory;

constructor TMyContextClass.Create(AContext: TMyContext);
begin
  inherited Create;
  FContext := AContext;
end;

function TMyContextClass.GetContextName: string;
begin
  Result := TRttiEnumerationType.GetName(Context);
end;

constructor TMySimpleContext.Create;
begin
  inherited Create(TMyContext.simple);
end;

constructor TMyComplexContext.Create;
begin
  inherited Create(TMyContext.complex);
end;

function TMyImplementor.ConvertNumber(Number: Integer): string;
begin
  Result := Number.ToString;
end;

initialization
  TMyInterfaceFactory.Register<TMyInterfaceImpl>;
  TMyContextInterfaceFactory.Register<TMySimpleContext>(TMyContext.simple);
  TMyContextInterfaceFactory.Register<TMyComplexContext>(TMyContext.complex);
finalization
  TMyContextInterfaceFactory.Unregister<TMyComplexContext>(TMyContext.complex);
  TMyContextInterfaceFactory.Unregister<TMySimpleContext>(TMyContext.simple);
  TMyInterfaceFactory.Unregister<TMyInterfaceImpl>;
end.
