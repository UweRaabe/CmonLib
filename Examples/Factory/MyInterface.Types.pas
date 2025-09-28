unit MyInterface.Types;

interface

type
  IMyInterface = interface
    function ConvertNumber(Number: Integer): string;
  end;

type
{$SCOPEDENUMS ON}
  TMyContext = (simple, complex);
{$SCOPEDENUMS OFF}
  IMyContextInterface = interface
    function GetContextName: string;
  end;

implementation

end.
