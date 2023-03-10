unit Main.FmxForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Data.DB, Datasnap.DBClient, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls,
  Cmon.DbAware.ObserverLink, Cmon.Fmx.DbAware.Components, Data.Bind.Controls, FMX.Layouts, Fmx.Bind.Navigator, Data.Bind.Components, Data.Bind.DBScope,
  System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Grid, FMX.ScrollBox, FMX.Grid;

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
    DataLinkContainer1: TDataLinkContainer;
    BindNavigator1: TBindNavigator;
    BindSourceDB1: TBindSourceDB;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
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
