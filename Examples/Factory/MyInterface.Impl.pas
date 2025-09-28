unit MyInterface.Impl;

interface

uses
  MyInterface.Types;

type
  TMyInterface = class(TInterfacedObject, IMyInterface)
  strict private
  strict protected
    function ConvertNumber(Number: Integer): string;
  end;

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

function TMyInterface.ConvertNumber(Number: Integer): string;
begin
  Result := Number.ToString;
end;

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

initialization
  TMyInterfaceFactory.Register<TMyInterface>;
  TMyContextInterfaceFactory.Register<TMySimpleContext>(TMyContext.simple);
  TMyContextInterfaceFactory.Register<TMyComplexContext>(TMyContext.complex);
finalization
  TMyContextInterfaceFactory.Unregister<TMyComplexContext>(TMyContext.complex);
  TMyContextInterfaceFactory.Unregister<TMySimpleContext>(TMyContext.simple);
  TMyInterfaceFactory.Unregister<TMyInterface>;
end.
