unit Main.Frame;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.ComCtrls, Vcl.ToolWin,
  Cmon.Vcl.Forms;

type
  TMainFrame = class(TFrame)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
  public
    constructor CreateAnonym(AOwner: TComponent);
  end;

implementation

{$R *.dfm}

uses
  DesignImages.DM;

constructor TMainFrame.CreateAnonym(AOwner: TComponent);
begin
  Create(nil);
  Name := '';
  AOwner.InsertComponent(Self);
end;

end.
