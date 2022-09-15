unit Main.Form;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ComCtrls, Vcl.ToolWin,
  Common.Form,
  Base.Form, Main.Frame, Simple.Form;

type
  TMainForm = class(TSimpleForm)
    StaticFrame: TMainFrame;
    NewFormButton: TButton;
    procedure NewFormButtonClick(Sender: TObject);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DesignImages.DM;

procedure TMainForm.NewFormButtonClick(Sender: TObject);
begin
  TBaseForm.Create(Application).Show;
end;

end.
