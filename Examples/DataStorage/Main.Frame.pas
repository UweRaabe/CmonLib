unit Main.Frame;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask,
  Cmon.DataStorage,
  Common.Frame;

type
  TDemoFrame = class(TFrame)
    SomeTextEdit: TLabeledEdit;
    SomeIndexSelector: TRadioGroup;
    TitleLabel: TLabel;
  private
    function GetSomeIndex: Integer;
    function GetSomeText: string;
    procedure SetSomeIndex(const Value: Integer);
    procedure SetSomeText(const Value: string);
  protected
  public
    procedure UpdateTitle;
    [Stored, Default(-1)]
    property SomeIndex: Integer read GetSomeIndex write SetSomeIndex;
    [Stored, Default('')]
    property SomeText: string read GetSomeText write SetSomeText;
  end;

implementation

{$R *.dfm}

function TDemoFrame.GetSomeIndex: Integer;
begin
  Result := SomeIndexSelector.ItemIndex;
end;

function TDemoFrame.GetSomeText: string;
begin
  Result := SomeTextEdit.Text;
end;

procedure TDemoFrame.SetSomeIndex(const Value: Integer);
begin
  SomeIndexSelector.ItemIndex := Value;
end;

procedure TDemoFrame.SetSomeText(const Value: string);
begin
  SomeTextEdit.Text := Value;
end;

procedure TDemoFrame.UpdateTitle;
begin
  TitleLabel.Caption := Name;
end;

end.
