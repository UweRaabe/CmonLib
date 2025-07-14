unit Cmon.Vcl.Design;

interface

procedure Register;

implementation

uses
  System.Classes,
  Cmon.MRU.Vcl,
  DsnConst;

procedure Register;
begin
  RegisterComponents(srAdditional, [TMRUFiles]);
end;

end.
