unit DesignImages.DM;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, System.Messaging,
  Vcl.ImgList, Vcl.VirtualImageList;

type
  TdmDesignImages = class(TDataModule)
    MainImageList: TVirtualImageList;
  private
  public
  end;

var
  dmDesignImages: TdmDesignImages;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  Images.DM;

{$R *.dfm}

initialization
  RegisterClass(TdmDesignImages);
finalization
  UnRegisterClass(TdmDesignImages);
end.
