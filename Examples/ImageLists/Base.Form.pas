unit Base.Form;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls,
  Cmon.Vcl.Forms;

type
  TBaseForm = class(TForm)
    AddFrameButton: TButton;
    InfoLabel: TLabel;
    procedure AddFrameButtonClick(Sender: TObject);
  private
  public
    procedure AfterConstruction; override;
    procedure UpdateInfo;
  end;

var
  SecondaryForm: TBaseForm;

implementation

{$R *.dfm}

uses
  System.SysUtils,
  Vcl.VirtualImageList,
  Cmon.Utilities,
  Main.Frame;

procedure TBaseForm.AddFrameButtonClick(Sender: TObject);
begin
  var frame := TMainFrame.CreateAnonym(Self);
  frame.Parent := Self;
  frame.Align := alTop;
  frame.AutoSize := true;
  UpdateInfo;
end;

procedure TBaseForm.AfterConstruction;
begin
  inherited;
  UpdateInfo;
end;

procedure TBaseForm.UpdateInfo;
begin
  var cnt := 0;
  for var img in ComponentsOf<TVirtualImageList> do
    Inc(cnt);

  InfoLabel.Caption := Format('TVirtualImageList instances: %d', [cnt]);
end;

end.
