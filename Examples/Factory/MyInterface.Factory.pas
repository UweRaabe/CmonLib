unit MyInterface.Factory;

interface

uses
  Cmon.Factory,
  MyInterface.Types;

type
  TMyInterfaceFactory = class(TInterfaceFactory<IMyInterface>);
  TMyContextInterfaceFactory = class(TInterfaceFactory<IMyContextInterface,TMyContext>);

implementation

end.
