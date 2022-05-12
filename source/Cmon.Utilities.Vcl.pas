unit Cmon.Utilities.Vcl;

interface

uses
  Vcl.Controls,
  Cmon.Utilities;

type
  TWinControlHelper = class helper for TWinControl
  public
    function ControlsOf<T: TControl>: TEnumWrapper<T>; inline;
  end;

implementation

{ TWinControlHelper }

function TWinControlHelper.ControlsOf<T>: TEnumWrapper<T>;
begin
  Result := TEnumWrapper<T>.Create(ControlCount,
    function(Index: Integer): TObject
    begin
      Result := Controls[Index];
    end);
end;

end.
