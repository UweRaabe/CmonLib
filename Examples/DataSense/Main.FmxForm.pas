unit Main.FmxForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions, System.ImageList, System.Rtti,
  System.Bindings.Outputs,
  Data.DB, Data.Bind.Controls, Data.Bind.Components, Data.Bind.DBScope, Data.Bind.EngExt, Data.Bind.Grid,
  Datasnap.DBClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  Fmx.Bind.Navigator, FMX.ActnList, FMX.ImgList, FMX.Grid.Style, Fmx.Bind.DBEngExt, Fmx.Bind.Grid, Fmx.Bind.Editors,
  FMX.ScrollBox, FMX.Grid,
  Cmon.DataSense, Cmon.DataSense.FMX;

type
  TMainForm = class(TForm)
    DsCustomer: TDataSource;
    QuCustomer: TClientDataSet;
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
    edtCompany: TEdit;
    edtCity: TEdit;
    lblCompany: TLabel;
    lblCity: TLabel;
    BindNavigator1: TBindNavigator;
    BindSourceDB1: TBindSourceDB;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    DataSense1: TDataSense;
    procedure actFirstExecute(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.actFirstExecute(Sender: TObject);
begin
  QuCustomer.First;
end;

end.
