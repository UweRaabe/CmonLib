unit Cmon.Utilities.Vcl;

interface

uses
  Vcl.Controls,
  Cmon.Utilities;

type
  TWinControlHelper = class helper for TWinControl
  public
    function ControlsOf<T: TControl>(ADirection: TEnumDirection = TEnumDirection.Normal): TEnumWrapper<T>; inline;
  end;

implementation

function TWinControlHelper.ControlsOf<T>(ADirection: TEnumDirection): TEnumWrapper<T>;
begin
  Result := TEnumWrapper<T>.Create(ControlCount,
    function(Index: Integer): TObject
    begin
      Result := Controls[Index];
    end,
    ADirection);
end;

end.
