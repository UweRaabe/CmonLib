unit Mapping.Types;

interface

{
  Example of mapping record and class, field name constants.
  Some fields and properties have specific mapped names different from the DB field name.
  The TRecordFields descendants implement InternalCalcFields.
}

uses
  Data.DB,
  Cmon.DataSetHelper;

type
  [DBFields(mapAuto)]
  TEmployee = record
  public type
    Consts = record
    public const
      EmpNo = 'EmpNo';
      LastName = 'LastName';
      FirstName = 'FirstName';
      PhoneExt = 'PhoneExt';
      HireDate = 'HireDate';
      Salary = 'Salary';
      FullName = 'FullName';
    end;
  public type
    [DBFields(mapAuto)]
    Fields = class(TRecordFields)
    private
      FEmpNo: TField;
      FLastName: TField;
      FFirstName: TField;
      FPhoneExt: TField;
      FHireDate: TField;
      FSalary: TField;
      FFullName: TField;
    protected
      procedure InternalCalcFields; override;
    public
      property EmpNo: TField read FEmpNo;
      property LastName: TField read FLastName;
      property FirstName: TField read FFirstName;
      property PhoneExt: TField read FPhoneExt;
      property HireDate: TField read FHireDate;
      property Salary: TField read FSalary;
      property FullName: TField read FFullName;
    end;
  public
    EmpNo: Integer;
    LastName: string;
    FirstName: string;
    PhoneExt: string;
    HireDate: TDateTime;
    Salary: Double;
    FullName: string;
  end;
  TEmployeeConsts = TEmployee.Consts;
  TEmployeeFields = TEmployee.Fields;

type
  [DBFields(mapAuto)]
  TCustomer = class
  public type
    Consts = record
    public const
      CustNo = 'CustNo';
      Company = 'Company';
      Address1 = 'Addr1';
      Address2 = 'Addr2';
      City = 'City';
      State = 'State';
      ZipCode = 'Zip';
      Country = 'Country';
      Phone = 'Phone';
      FAX = 'FAX';
      TaxRate = 'TaxRate';
      Contact = 'Contact';
      LastInvoiceDate = 'LastInvoiceDate';
      FullAddress = 'FullAddress';
    end;
  public type
    [DBFields(mapAuto)]
    Fields = class(TRecordFields)
    private
      FCustNo: TField;
      FCompany: TField;
      [DBField(Consts.Address1)]
      FAddress1: TField;
      [DBField(Consts.Address2)]
      FAddress2: TField;
      FCity: TField;
      FState: TField;
      [DBField(Consts.ZipCode)]
      FZipCode: TField;
      FCountry: TField;
      FPhone: TField;
      FFAX: TField;
      FTaxRate: TField;
      FContact: TField;
      FLastInvoiceDate: TField;
      FFullAddress: TField;
    protected
      procedure InternalCalcFields; override;
    public
      property CustNo: TField read FCustNo;
      property Company: TField read FCompany;
      property Address1: TField read FAddress1;
      property Address2: TField read FAddress2;
      property City: TField read FCity;
      property State: TField read FState;
      property ZipCode: TField read FZipCode;
      property Country: TField read FCountry;
      property Phone: TField read FPhone;
      property FAX: TField read FFAX;
      property TaxRate: TField read FTaxRate;
      property Contact: TField read FContact;
      property LastInvoiceDate: TField read FLastInvoiceDate;
      property FullAddress: TField read FFullAddress;
    end;
  private
    [DBField(Consts.CustNo)]
    FCustNo: Double;
    FCompany: string;
    FAddress1: string;
    FAddress2: string;
    FCity: string;
    FState: string;
    [DBField(Consts.ZipCode)]
    FZipCode: string;
    FCountry: string;
    FPhone: string;
    FFAX: string;
    FTaxRate: Double;
    FContact: string;
    FLastInvoiceDate: TDateTime;
    FFullAddress: string;
    function GetCustNo: Integer;
    procedure SetCustNo(const Value: Integer);
  public
    [DBField(false)]
    property CustNo: Integer read GetCustNo write SetCustNo;
    property Company: string read FCompany write FCompany;
    [DBField(Consts.Address1)]
    property Address1: string read FAddress1 write FAddress1;
    [DBField(Consts.Address2)]
    property Address2: string read FAddress2 write FAddress2;
    property City: string read FCity write FCity;
    property State: string read FState write FState;
    property ZipCode: string read FZipCode write FZipCode;
    property Country: string read FCountry write FCountry;
    property Phone: string read FPhone write FPhone;
    property FAX: string read FFAX write FFAX;
    property TaxRate: Double read FTaxRate write FTaxRate;
    property Contact: string read FContact write FContact;
    property LastInvoiceDate: TDateTime read FLastInvoiceDate write FLastInvoiceDate;
    property FullAddress: string read FFullAddress write FFullAddress;
  end;
  TCustomerConsts = TCustomer.Consts;
  TCustomerFields = TCustomer.Fields;

implementation

uses
  System.Classes, System.SysUtils;

procedure TEmployee.Fields.InternalCalcFields;
begin
  inherited;
  FullName.AsString := FirstName.AsString + ' ' + LastName.AsString;
end;

function TCustomer.GetCustNo: Integer;
begin
  result := Round(FCustNo);
end;

procedure TCustomer.SetCustNo(const Value: Integer);
begin
  FCustNo := Value;
end;

procedure TCustomer.Fields.InternalCalcFields;
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
