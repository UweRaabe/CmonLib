unit Images.DM;

interface

uses
  System.SysUtils, System.Classes, System.Messaging,
  Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  TdmImages = class(TDataModule)
    MainImageCollection: TImageCollection;
  private
  public
  end;

var
  dmImages: TdmImages;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
