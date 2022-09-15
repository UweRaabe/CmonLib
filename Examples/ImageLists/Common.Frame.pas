unit Common.Frame;

interface

uses
  Vcl.Forms, Vcl.Controls;

type
  TCommonFrame = class(TFrame)
  protected
    procedure SetParent(AParent: TWinControl); override;
  end;

type
  TFrame = TCommonFrame;

implementation

uses
  System.SysUtils,
  Common.Types;

procedure TCommonFrame.SetParent(AParent: TWinControl);
var
  intf: IRedirectReferences;
begin
  inherited;
  if Supports(GetParentForm(Self), IRedirectReferences, intf) then
    intf.RedirectReferences;
end;

end.
