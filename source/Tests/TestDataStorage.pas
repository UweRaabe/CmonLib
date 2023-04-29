unit TestDataStorage;

interface

uses
  DUnitX.TestFramework,
  Cmon.DataStorage,
  Cmon.DataStorage.Types;

type
  [TestFixture]
  TValueConverterTest = class
  private
    FConverter: TValueConverter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestArrayToString;
    [Test]
    procedure TestRecordToString;
    [Test]
    procedure TestSetToString;
    [Test]
    procedure TestStringToArray;
    [Test]
    procedure TestStringToRecord;
    [Test]
    procedure TestStringToSet;
  end;

type
  [TestFixture]
  TDataStorageTest = class
  public
    [Test]
    procedure TestString;
  end;

implementation

uses
  System.Rtti, System.IOUtils, System.SysUtils,
  Cmon.DataStorage.Inifile;

procedure TValueConverterTest.Setup;
begin
  FConverter := TValueConverter.Create;
end;

procedure TValueConverterTest.TearDown;
begin
  FConverter.Free;
end;

procedure TValueConverterTest.TestArrayToString;
begin
  var source := TArray<Integer>.Create(1, 1, 2, 3, 5, 8, 13, 21);
  var target := '[1,1,2,3,5,8,13,21]';
  var res := FConverter.ValueToString(TValue.From(source));
  Assert.AreEqual(target, res);
end;

procedure TValueConverterTest.TestRecordToString;
type
  TMyRecord = record
    Field1: Integer;
    Field2: Boolean;
    Field3: string;
  end;
begin
  var source := Default(TMyRecord);
  source.Field1 := 42;
  source.Field3 := 'Hello World';
  var target := '(Field1:42;Field2:False;Field3:"Hello World")';
  var res := FConverter.ValueToString(TValue.From(source));
  Assert.AreEqual(target, res);
end;

procedure TValueConverterTest.TestSetToString;
type
  TEnum = (enum1, enum2, enum3);
  TEnumSet = set of TEnum;
begin
  var source: TEnumSet := [enum1, enum2];
  var target := '[enum1,enum2]';
  var res := FConverter.ValueToString(TValue.From(source));
  Assert.AreEqual(target, res);
end;

procedure TValueConverterTest.TestStringToArray;
type
  TArr = TArray<Integer>;
begin
  var source := '[1,1,2,3,5,8,13,21]';
  var target := TArr.Create(1, 1, 2, 3, 5, 8, 13, 21);
  var res := FConverter.StringToValue(source, TValue.From(TArr.Create())).AsType<TArr>;
  Assert.AreEqual(target, res);
end;

procedure TValueConverterTest.TestStringToRecord;
type
  TMyRecord = record
    Field1: Integer;
    Field2: Boolean;
    Field3: string;
  end;
begin
  var source := '(Field1:42;Field2:False;Field3:"Hello World")';
  var target := Default(TMyRecord);
  target.Field1 := 42;
  target.Field2 := False;
  target.Field3 := 'Hello World';
  var res := FConverter.StringToValue(source, TValue.From(Default(TMyRecord))).AsType<TMyRecord>;
  Assert.AreEqual(target.Field1, res.Field1);
  Assert.AreEqual(target.Field2, res.Field2);
  Assert.AreEqual(target.Field3, res.Field3);
end;

procedure TValueConverterTest.TestStringToSet;
type
  TEnum = (enum1, enum2, enum3);
  TEnumSet = set of TEnum;
begin
  var source := '[enum1,enum3]';
  var target: TEnumSet := [enum1, enum3];
  var res := FConverter.StringToValue(source, TValue.From<TEnumSet>([])).AsType<TEnumSet>;
  Assert.AreEqual(target, res);
end;

procedure TDataStorageTest.TestString;
const
  exp = '[Test]' + sLineBreak +
        'String=Hello World' + sLineBreak +
        sLineBreak;
begin
  var FileName := TPath.ChangeExtension(TPath.GetTempFileName, '.ini');
  try
    var Instance := TDataStorage.Create;
    try
      Instance.StorageTarget := TIniStorageTarget.Create(FileName);
      Instance.StorageKey := 'Test';
      Instance.WriteString('String', 'Hello World');
    finally
      Instance.Free;
    end;
    var act := TFile.ReadAllText(FileName);
    Assert.AreEqual(exp, act);
  finally
    TFile.Delete(FileName);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TValueConverterTest);
  TDUnitX.RegisterTestFixture(TDataStorageTest);

end.
