unit Main.VclForm;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    TextEdit: TMemo;
    LoadButton: TButton;
    SaveButton: TButton;
    OpenDialog: TOpenTextFileDialog;
    SaveDialog: TSaveTextFileDialog;
    pnlTop: TPanel;
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.UITypes,
  Utilities;

{$R *.dfm}

procedure TMainForm.LoadButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadStrings(OpenDialog.FileName, TextEdit.Lines);
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SaveStrings(SaveDialog.FileName, TextEdit.Lines);
end;

end.
