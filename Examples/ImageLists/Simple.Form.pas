unit Simple.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Base.Form, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin;

type
  TSimpleForm = class(TBaseForm)
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
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  SimpleForm: TSimpleForm;

implementation

{$R *.dfm}

end.
