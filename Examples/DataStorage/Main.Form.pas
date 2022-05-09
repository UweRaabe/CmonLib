unit Main.Form;

interface

uses
  System.Classes, System.Rtti,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask,
  Cmon.DataStorage,
  Common.Form, Common.Frame, Main.Frame;

type
{$SCOPEDENUMS ON}
  TMyEnum = (none, enum1, enum2, enum3, enum4);
{$SCOPEDENUMS OFF}

type
  DefaultMyEnumAttribute = class(DefaultAttribute)
  public
    constructor Create(AValue: TMyEnum); overload;
  end;

type
  TDemoMainForm = class(TCommonForm)
    SomeTextEdit: TLabeledEdit;
    SomeIndexSelector: TRadioGroup;
    DemoFrame1: TDemoFrame;
    DemoFrame2: TDemoFrame;
    TitleLabel: TLabel;
    SomeEnumSelector: TRadioGroup;
    SomeBooleanCheck: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    function GetSomeBoolean: Boolean;
    function GetSomeEnum: TMyEnum;
    function GetSomeIndex: Integer;
    function GetSomeText: string;
    procedure SetSomeBoolean(const Value: Boolean);
    procedure SetSomeEnum(const Value: TMyEnum);
    procedure SetSomeIndex(const Value: Integer);
    procedure SetSomeText(const Value: string);
  public
    procedure UpdateTitle;
    [Stored, Default(True)]
    property SomeBoolean: Boolean read GetSomeBoolean write SetSomeBoolean;
    [Stored, DefaultMyEnum(TMyEnum.None)]
    property SomeEnum: TMyEnum read GetSomeEnum write SetSomeEnum;
    [Stored, Default(1)]
    property SomeIndex: Integer read GetSomeIndex write SetSomeIndex;
    [Stored, Default('Hello World')]
    property SomeText: string read GetSomeText write SetSomeText;
  end;

var
  DemoMainForm: TDemoMainForm;

implementation

{$R *.dfm}

type
  TMyEnumHelper = record helper for TMyEnum
  public
    function GetAsIndex: Integer;
    class procedure ListNames(Target: TStrings); static;
    procedure SetAsIndex(const Value: Integer);
    function ToString: string;
    property AsIndex: Integer read GetAsIndex write SetAsIndex;
  end;

function TMyEnumHelper.GetAsIndex: Integer;
begin
  Result := Ord(Self);
end;

class procedure TMyEnumHelper.ListNames(Target: TStrings);
var
  enum: TMyEnum;
begin
  Target.BeginUpdate;
  try
    Target.Clear;
    for enum := Low(enum) to High(enum) do
      Target.Add(enum.ToString);
  finally
    Target.EndUpdate;
  end;
end;

procedure TMyEnumHelper.SetAsIndex(const Value: Integer);
begin
  Self := TMyEnum(Value);
end;

function TMyEnumHelper.ToString: string;
begin
  Result := TRttiEnumerationType.GetName(Self);
end;

procedure TDemoMainForm.FormCreate(Sender: TObject);
begin
  TMyEnum.ListNames(SomeEnumSelector.Items);
  inherited;
  UpdateTitle;
  DemoFrame1.UpdateTitle;
  DemoFrame2.UpdateTitle;
end;

function TDemoMainForm.GetSomeBoolean: Boolean;
begin
  Result := SomeBooleanCheck.Checked;
end;

function TDemoMainForm.GetSomeEnum: TMyEnum;
begin
  Result.AsIndex := SomeEnumSelector.ItemIndex;
end;

function TDemoMainForm.GetSomeIndex: Integer;
begin
  Result := SomeIndexSelector.ItemIndex;
end;

function TDemoMainForm.GetSomeText: string;
begin
  Result := SomeTextEdit.Text;
end;

procedure TDemoMainForm.SetSomeBoolean(const Value: Boolean);
begin
  SomeBooleanCheck.Checked := Value;
end;

procedure TDemoMainForm.SetSomeEnum(const Value: TMyEnum);
begin
  SomeEnumSelector.ItemIndex := Value.AsIndex;
end;

procedure TDemoMainForm.SetSomeIndex(const Value: Integer);
begin
  SomeIndexSelector.ItemIndex := Value;
end;

procedure TDemoMainForm.SetSomeText(const Value: string);
begin
  SomeTextEdit.Text := Value;
end;

procedure TDemoMainForm.UpdateTitle;
begin
  TitleLabel.Caption := Name;
end;

constructor DefaultMyEnumAttribute.Create(AValue: TMyEnum);
begin
  inherited Create(TValue.From(AValue));
end;

end.
