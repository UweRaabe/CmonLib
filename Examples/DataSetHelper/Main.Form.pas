unit Main.Form;

interface

{ Example showing the use of the Cmon.DataSetHelper unit }

uses
  System.Classes,
  Data.DB,
  Datasnap.DBClient,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Grids, Vcl.DBGrids,
  Cmon.DataSetHelper,
  Mapping.Types;

type
  [DBFields(mapManual)]
  TSomeManuallyMappedClass = class
  private
    [DBField('SomeDBFieldName')]
    FSomeField: Integer;
    [DBField('SomeOtherDBFieldName')]
    FSomeOtherField: string;
    FSomeNotMappedField: Double;
  public
    property SomeField: Integer read FSomeField write FSomeField;
    property SomeNotMappedField: Double read FSomeNotMappedField write FSomeNotMappedField;
    property SomeOtherField: string read FSomeOtherField write FSomeOtherField;
  end;

type
  TMainForm = class(TForm)
    MemOutput: TMemo;
    PnlTop: TPanel;
    PnlMain: TPanel;
    BtnListEmployees: TButton;
    BtnListCustomers: TButton;
    GrdEmployee: TDBGrid;
    DsEmployee: TDataSource;
    GrdCustomer: TDBGrid;
    DsCustomer: TDataSource;
    QuEmployee: TClientDataSet;
    QuCustomer: TClientDataSet;
    QuEmployeeFullName: TStringField;
    QuCustomerFullAddress: TStringField;
    procedure FormCreate(Sender: TObject);
    procedure BtnListEmployeesClick(Sender: TObject);
    procedure BtnListCustomersClick(Sender: TObject);
    procedure GrdCustomerDblClick(Sender: TObject);
    procedure GrdEmployeeDblClick(Sender: TObject);
  private
    FCustomerFields: TCustomerFields;
    FEmployeeFields: TEmployeeFields;
  public
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.SysUtils,
  Vcl.Dialogs,
  MidasLib;

{$R *.dfm}

destructor TMainForm.Destroy;
begin
  FCustomerFields.Free;
  FEmployeeFields.Free;
  inherited Destroy;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FEmployeeFields := TEmployeeFields.Create;
  FEmployeeFields.DataSet := QuEmployee;
  FCustomerFields := TCustomerFields.Create;
  FCustomerFields.DataSet := QuCustomer;
  QuEmployee.Active := true;
  QuCustomer.Active := true;
end;

procedure TMainForm.BtnListEmployeesClick(Sender: TObject);
var
  Employee: TEmployee;
  S: string;
begin
  { List all emplyoees with First- and LastName in the Memo. Employees hired
    before 01/01/1991 are marked with an * in front of their names. }
  MemOutput.Lines.BeginUpdate;
  try
    MemOutput.Lines.Clear;
    for Employee in QuEmployee.Records<TEmployee> do begin
      S := Trim(Format('%s %s', [Employee.FirstName, Employee.LastName]));
      if Employee.HireDate < EncodeDate(1991, 1, 1) then
        S := '*' + S;
      MemOutput.Lines.Add(S);
    end;
  finally
    MemOutput.Lines.EndUpdate;
  end;
end;

procedure TMainForm.BtnListCustomersClick(Sender: TObject);
var
  Customer: TCustomer;
  S: string;
begin
  { List all Customers with their Name and City in the Memo. Customers outside
    the US have their Country enclosed in brackets appended. }
  MemOutput.Lines.BeginUpdate;
  try
    MemOutput.Lines.Clear;
    Customer := TCustomer.Create;
    try
      for Customer in QuCustomer.Records(Customer) do begin
        S := Format('%d: %s - %s %s', [Customer.CustNo, Customer.Company, Customer.ZipCode, Customer.City]);
        if Customer.Country <> 'US' then
          S := S + ' (' + Customer.Country + ')';
        MemOutput.Lines.Add(S);
      end;
    finally
      Customer.Free;
    end;
  finally
    MemOutput.Lines.EndUpdate;
  end;
end;

procedure TMainForm.GrdCustomerDblClick(Sender: TObject);
var
  Customer: TCustomer;
begin
  { Set LastInvoiceDate to current date/time }
  Customer := TCustomer.Create;
  try
    QuCustomer.LoadInstanceFromCurrent(Customer);
    Customer.LastInvoiceDate := Now;
    QuCustomer.Edit;
    QuCustomer.StoreInstanceToCurrent(Customer);
    QuCustomer.Post;
  finally
    Customer.Free;
  end;
end;

procedure TMainForm.GrdEmployeeDblClick(Sender: TObject);
var
  Employee: TEmployee;
begin
  { Show the employee's name and the hire date. }
  Employee := QuEmployee.GetCurrentRec<TEmployee>;
  ShowMessage(Format('%s %s was hired on %s', [Employee.FirstName, Employee.LastName, FormatDateTime('dddddd', Employee.HireDate)]));
end;

end.
