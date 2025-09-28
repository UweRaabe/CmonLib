unit Main.Form;

interface

uses
  System.Classes, System.Generics.Collections,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.NumberBox, Vcl.ExtCtrls,
  MyInterface.Types, MyInterface.Factory;

type
  TMainForm = class(TForm)
    edtNumber: TNumberBox;
    btnDoSomething: TButton;
    dspSomething: TMemo;
    pnlTop: TPanel;
    lblNumber: TLabel;
    pnlMain: TPanel;
    procedure btnDoSomethingClick(Sender: TObject);
  public
  type
    TMyContextList = TList<IMyContextInterface>;
  private
    FMyComplex: IMyContextInterface;
    FMyInterface: IMyInterface;
    FMyContextList: TMyContextList;
    FMySimple: IMyContextInterface;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MyInterface: IMyInterface read FMyInterface;
    property MySimple: IMyContextInterface read FMySimple;
    property MyComplex: IMyContextInterface read FMyComplex;
    property MyContextList: TMyContextList read FMyContextList;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FMyContextList := TMyContextList.Create;

  FMySimple := TMyContextInterfaceFactory.CreateInstance(TMyContext.simple);
  FMyComplex := TMyContextInterfaceFactory.CreateInstance(TMyContext.complex);
  FMyInterface := TMyInterfaceFactory.CreateInstance;

  MyContextList.Add(MySimple);
  MyContextList.Add(MyComplex);
end;

destructor TMainForm.Destroy;
begin
  FMyContextList.Free;
  inherited;
end;

procedure TMainForm.btnDoSomethingClick(Sender: TObject);
begin
  dspSomething.Lines.Add(MyInterface.ConvertNumber(edtNumber.ValueInt));
  for var ctx in MyContextList do
    dspSomething.Lines.Add(ctx.GetContextName);
end;

end.
