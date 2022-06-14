unit Main.FmxForm;

interface

uses
  System.Classes,
  FMX.Forms, FMX.Memo, FMX.Memo.Types, FMX.Dialogs, FMX.ScrollBox, FMX.StdCtrls, FMX.Types, FMX.Controls,
  FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    pnlTop: TPanel;
    LoadButton: TButton;
    SaveButton: TButton;
    TextEdit: TMemo;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  Utilities;

{$R *.fmx}

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
