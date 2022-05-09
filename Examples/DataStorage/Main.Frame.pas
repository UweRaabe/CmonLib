unit Main.Frame;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask,
  Cmon.DataStorage,
  Common.Frame;

type
  TDemoFrame = class(TCommonFrame)
    SomeTextEdit: TLabeledEdit;
    SomeIndexSelector: TRadioGroup;
    TitleLabel: TLabel;
  private
    function GetSomeIndex: Integer;
    function GetSomeText: string;
    procedure SetSomeIndex(const Value: Integer);
    procedure SetSomeText(const Value: string);
  public
    procedure UpdateTitle;
    [Stored]
    property SomeIndex: Integer read GetSomeIndex write SetSomeIndex;
    [Stored]
    property SomeText: string read GetSomeText write SetSomeText;
  end;

var
  DemoFrame: TDemoFrame;

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
