unit Main.Data.Types;

interface

uses
  Cmon.DataStorage;

type
  [AutoStorageFields([vcPrivate])]
  TSubData = class
  private
    [Default(100)]
    FSomeInteger: Integer;
    [Default('Bar')]
    FSomeString: string;
  public
    property SomeInteger: Integer read FSomeInteger write FSomeInteger;
    property SomeString: string read FSomeString write FSomeString;
  end;

type
  [StorageKey('Settings'), AutoStorageProperties([vcPublic])]
  TMainData = class
  private
    FDontStoreMe: Integer;
    [Default(10)]
    FSomeInteger: Integer;
    [Default('Foo')]
    FSomeString: string;
    [Default]
    FSubData: TSubData;
  public
    constructor Create;
    destructor Destroy; override;
    [NoAutoStorage]
    property DontStoreMe: Integer read FDontStoreMe write FDontStoreMe;
    property SomeInteger: Integer read FSomeInteger write FSomeInteger;
    property SomeString: string read FSomeString write FSomeString;
    [Storage('Sub')]
    property SubData: TSubData read FSubData;
  end;

type
  [StorageKey('SettingsRec'), AutoStorageFields([vcPrivate, vcPublic]), SkipFieldNameF([vcPrivate])]
  TMainDataRec = record
  private
    [Default('Foo')]
    FSomeString: string;
  public
    [NoAutoStorage, Default(5)]
    DontStoreMe: Integer;
    [Default(10)]
    SomeInteger: Integer;
    property SomeString: string read FSomeString write FSomeString;
  end;

var
  MainData: TMainData = nil;
  MainDataRec: TMainDataRec;

implementation

constructor TMainData.Create;
begin
  inherited Create;
  FSubData := TSubData.Create();
end;

destructor TMainData.Destroy;
begin
  FSubData.Free;
  inherited Destroy;
end;

initialization
  MainData := TMainData.Create;
finalization
  MainData.Free;
  MainData := nil;
end.
