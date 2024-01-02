unit Main.Form;

interface

uses
  System.ImageList, System.Classes,
  Data.DB,
  Datasnap.DBClient,
  Vcl.Forms, Vcl.ImgList, Vcl.VirtualImageList, Vcl.ExtCtrls, Vcl.Controls, Vcl.StdCtrls, Vcl.Mask, Vcl.DBCtrls, Vcl.Grids,
  Vcl.DBGrids, Vcl.Buttons,
  Cmon.DataSense, Cmon.DataSense.VCL;

type
  TMainForm = class(TForm)
    edtCity: TButtonedEdit;
    SmallImages: TVirtualImageList;
    QuCustomer: TClientDataSet;
    DsCustomer: TDataSource;
    QuCustomerCustNo: TFloatField;
    QuCustomerCompany: TStringField;
    QuCustomerAddr1: TStringField;
    QuCustomerAddr2: TStringField;
    QuCustomerCity: TStringField;
    QuCustomerState: TStringField;
    QuCustomerZip: TStringField;
    QuCustomerCountry: TStringField;
    QuCustomerPhone: TStringField;
    QuCustomerFAX: TStringField;
    QuCustomerTaxRate: TFloatField;
    QuCustomerContact: TStringField;
    QuCustomerLastInvoiceDate: TDateTimeField;
    dspCity: TLabel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    DBNavigator1: TDBNavigator;
    pnlMain: TPanel;
    DBGrid1: TDBGrid;
    edtCompany: TDBEdit;
    lblCompany: TLabel;
    lblCity: TLabel;
    LabeledEdit1: TLabeledEdit;
    DataSense: TDataSense;
    procedure edtCityRightButtonClick(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Images.DM;

procedure TMainForm.edtCityRightButtonClick(Sender: TObject);
begin
  dspCity.Caption := edtCity.Text;
end;

end.
