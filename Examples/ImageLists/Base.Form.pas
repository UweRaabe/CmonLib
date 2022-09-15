unit Base.Form;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls,
  Common.Form;

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
  InfoLabel.Caption := Format('TVirtualImageList instances: %d', [VirtualImageListCount]);
end;

end.
