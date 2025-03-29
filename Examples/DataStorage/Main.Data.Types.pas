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
    [NoStorage]
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
{$IF (CompilerVersion > 36.0) or RTLVersion123 }
  {$MESSAGE HINT 'String fields in records used with generic record constraints break the 12.3 compiler.'}
{$ELSE}
    [Default('Foo')]
    FSomeString: string;
{$ENDIF}
    function GetSomeString: string;
    procedure SetSomeString(const Value: string);
  public
    [NoStorage, Default(5)]
    DontStoreMe: Integer;
    [Default(10)]
    SomeInteger: Integer;
    { AutoStorage for record properties will not work until RTTI supports it }
    property SomeString: string read GetSomeString write SetSomeString;
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

function TMainDataRec.GetSomeString: string;
begin
{$IF (CompilerVersion > 36.0) or RTLVersion123 }
  Result := 'Feature not supported!';
{$ELSE}
  Result := FSomeString;
{$ENDIF}
end;

procedure TMainDataRec.SetSomeString(const Value: string);
begin
{$IF (CompilerVersion > 36.0) or RTLVersion123 }
{$ELSE}
  FSomeString := Value;
{$ENDIF}
end;

initialization
  MainData := TMainData.Create;
finalization
  MainData.Free;
  MainData := nil;
end.
