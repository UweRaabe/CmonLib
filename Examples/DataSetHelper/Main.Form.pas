unit Main.Form;

interface

{ Example showing the use of the Cmon.DataSetHelper unit }

uses
  System.Classes,
  Data.DB,
  Datasnap.DBClient,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Grids, Vcl.DBGrids,
  Cmon.DataSetHelper;

type
  TEmployee = record
    EmpNo: Integer;
    LastName: string;
    FirstName: string;
    FullName: string;
    PhoneExt: string;
    HireDate: TDateTime;
    Salary: Double;
  end;

  TCustomer = class
  private
    [DBField('CustNo')]
    FCustNo: Double;
    FCompany: string;
    FAddress1: string;
    FAddress2: string;
    FCity: string;
    FState: string;
    [DBField('Zip')]
    FZipCode: string;
    FCountry: string;
    FPhone: string;
    FFAX: string;
    FTaxRate: Double;
    FContact: string;
    FLastInvoiceDate: TDateTime;
    function GetCustNo: Integer;
    procedure SetCustNo(const Value: Integer);
  public
    [DBField('Addr1')]
    property Address1: string read FAddress1 write FAddress1;
    [DBField('Addr2')]
    property Address2: string read FAddress2 write FAddress2;
    property City: string read FCity write FCity;
    property Company: string read FCompany write FCompany;
    property Contact: string read FContact write FContact;
    property Country: string read FCountry write FCountry;
    [DBField(false)]
    property CustNo: Integer read GetCustNo write SetCustNo;
    property FAX: string read FFAX write FFAX;
    property LastInvoiceDate: TDateTime read FLastInvoiceDate write FLastInvoiceDate;
    property Phone: string read FPhone write FPhone;
    property State: string read FState write FState;
    property TaxRate: Double read FTaxRate write FTaxRate;
    property ZipCode: string read FZipCode write FZipCode;
  end;

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
  TEmployeeFields = class(TRecordFields)
  private
    FEmpNo: TField;
    FFirstName: TField;
    FFullName: TField;
    FHireDate: TField;
    FLastName: TField;
    FPhoneExt: TField;
    FSalary: TField;
  protected
    procedure InternalCalcFields; override;
  public
    property EmpNo: TField read FEmpNo;
    property FirstName: TField read FFirstName;
    property FullName: TField read FFullName;
    property HireDate: TField read FHireDate;
    property LastName: TField read FLastName;
    property PhoneExt: TField read FPhoneExt;
    property Salary: TField read FSalary;
  end;

type
  TCustomerFields = class(TRecordFields)
  private
    FCustNo: TField;
    FCompany: TField;
    [DBField('Addr1')]
    FAddress1: TField;
    [DBField('Addr2')]
    FAddress2: TField;
    FCity: TField;
    FState: TField;
    [DBField('Zip')]
    FZipCode: TField;
    FCountry: TField;
    FPhone: TField;
    FFAX: TField;
    FTaxRate: TField;
    FContact: TField;
    FFullAddress: TField;
    FLastInvoiceDate: TField;
  protected
    procedure InternalCalcFields; override;
  public
    property Address1: TField read FAddress1;
    property Address2: TField read FAddress2;
    property City: TField read FCity;
    property Company: TField read FCompany;
    property Contact: TField read FContact;
    property Country: TField read FCountry;
    property CustNo: TField read FCustNo;
    property FAX: TField read FFAX;
    property FullAddress: TField read FFullAddress;
    property LastInvoiceDate: TField read FLastInvoiceDate;
    property Phone: TField read FPhone;
    property State: TField read FState;
    property TaxRate: TField read FTaxRate;
    property ZipCode: TField read FZipCode;
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
    procedure QuCustomerCalcFields(DataSet: TDataSet);
    procedure QuEmployeeCalcFields(DataSet: TDataSet);
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


function TCustomer.GetCustNo: Integer;
begin
  result := Round(FCustNo);
end;

procedure TCustomer.SetCustNo(const Value: Integer);
begin
  FCustNo := Value;
end;

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

procedure TMainForm.QuCustomerCalcFields(DataSet: TDataSet);
begin
  if FCustomerFields <> nil then
    FCustomerFields.CalcFields;
end;

procedure TMainForm.QuEmployeeCalcFields(DataSet: TDataSet);
begin
  if FEmployeeFields <> nil then
    FEmployeeFields.CalcFields;
end;

procedure TEmployeeFields.InternalCalcFields;
begin
  inherited;
  FullName.AsString := FirstName.AsString + ' ' + LastName.AsString;
end;

procedure TCustomerFields.InternalCalcFields;
var
  lst: TStringList;
begin
  inherited;
  lst := TStringList.Create;
  try
    lst.Add(Company.AsString);
    lst.Add(Address1.AsString);
    lst.Add(Address2.AsString);
    lst.Add(Format('%s %s', [ZipCode.AsString, City.AsString]));
    lst.Add(State.AsString);
    lst.Add(Country.AsString);

    FullAddress.AsString := lst.CommaText;
  finally
    lst.Free;
  end;
end;

end.
