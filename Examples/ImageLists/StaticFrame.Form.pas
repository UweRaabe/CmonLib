unit StaticFrame.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Base.Form, Vcl.StdCtrls, Main.Frame;

type
  TStaticFrameForm = class(TBaseForm)
    StaticFrame: TMainFrame;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  StaticFrameForm: TStaticFrameForm;

implementation

{$R *.dfm}

end.
