unit Cmon.DataSetHelper;

interface

uses
  System.Rtti, System.Generics.Collections,
  Data.DB;

type
  TDBFieldsMapping = (mapAuto, mapManual);

var
  DefaultDBFieldsMapping: TDBFieldsMapping = mapAuto;

type
  DBFieldsAttribute = class(TCustomAttribute)
  strict protected
    FMode: TDBFieldsMapping;
  public
    constructor Create(AMode: TDBFieldsMapping);
    property Mode: TDBFieldsMapping read FMode;
  end;

  DBFieldAttribute = class(TCustomAttribute)
  strict protected
    FIsStored: Boolean;
    FFieldName: String;
  public
    constructor Create; overload;
    constructor Create(const AIsStored: Boolean); overload;
    constructor Create(const AFieldName: string); overload;
    property IsStored: Boolean read FIsStored;
    property FieldName: String read FFieldName;
  end;

  TDataSetHelper = class helper for TDataSet
  private type
    TDataSetRecord<T> = class
    private type
      TMapping = class
      private
        FField: TField;
        FIsInstanceType: Boolean;
      protected
        function GetPointer(var Target: T): Pointer;
        property IsInstanceType: Boolean read FIsInstanceType;
      public
        constructor Create(AField: TField; AIsInstanceType: Boolean);
        procedure LoadFromField(var Target: T); virtual; abstract;
        procedure StoreToField(var Source: T); virtual; abstract;
      end;

      TFieldMapping = class(TMapping)
      private
        FRTTIField: TRTTIField;
      public
        constructor Create(AField: TField; ARTTIField: TRTTIField; AIsInstanceType: Boolean);
        procedure StoreToField(var Source: T); override;
        procedure LoadFromField(var Target: T); override;
      end;

      TPropMapping = class(TMapping)
      private
        FRTTIProp: TRttiProperty;
      public
        constructor Create(AField: TField; ARTTIProp: TRttiProperty; AIsInstanceType: Boolean);
        procedure StoreToField(var Source: T); override;
        procedure LoadFromField(var Target: T); override;
      end;

    private
      FDataSet: TDataSet;
      FInstance: T;
      FMapMode: TDBFieldsMapping;
      FMappings: TObjectList<TMapping>;
      FRTTIContext: TRTTIContext;
      function CheckAttributes(obj: TRttiNamedObject): string;
      function GetCurrent: T;
      function GetMapMode(obj: TRttiNamedObject): TDBFieldsMapping;
      procedure LoadMappings;
    protected
      procedure Initialize; virtual;
      property Instance: T read FInstance;
    public
      constructor Create(ADataSet: TDataSet); overload;
      constructor Create(ADataSet: TDataSet; const AInstance: T); overload;
      destructor Destroy; override;
      procedure LoadFromCurrent(var Target: T);
      procedure StoreToCurrent(var Source: T);
      property Current: T read GetCurrent;
    end;

    TDataSetEnumerator<T> = class(TDataSetRecord<T>)
    private
      FBookmark: TBookmark;
      FMoveToFirst: Boolean;
      FWasActive: Boolean;
    protected
      procedure Initialize; override;
    public
      destructor Destroy; override;
      function MoveNext: Boolean;
    end;

    IRecords<T> = interface
      function GetEnumerator: TDataSetEnumerator<T>;
    end;

    TRecords<T> = class(TInterfacedObject, IRecords<T>)
    private
      FDataSet: TDataSet;
    public
      constructor Create(ADataSet: TDataSet);
      function GetEnumerator: TDataSetEnumerator<T>; virtual;
    end;

    TRecordsInstance<T: class> = class(TRecords<T>)
    private
      FInstance: T;
    public
      constructor Create(ADataSet: TDataSet; AInstance: T);
      function GetEnumerator: TDataSetEnumerator<T>; override;
    end;

  public
    function GetCurrentRec<T: record>: T;
    procedure SetCurrentRec<T: record>(AInstance: T);
    procedure LoadInstanceFromCurrent<T: class>(AInstance: T);
    procedure StoreInstanceToCurrent<T: class>(AInstance: T);
    function Records<T: class>(AInstance: T): IRecords<T>; overload;
    function Records<T: record>: IRecords<T>; overload;
  end;

implementation

uses
  System.Sysutils;

constructor DBFieldsAttribute.Create(AMode: TDBFieldsMapping);
begin
  inherited Create;
  FMode := AMode;
end;

constructor DBFieldAttribute.Create;
begin
  inherited Create;
  FIsStored := true;
  FFieldName := '';
end;

constructor DBFieldAttribute.Create(const AIsStored: Boolean);
begin
  inherited Create;
  FIsStored := AIsStored;
  FFieldName := '';
end;

constructor DBFieldAttribute.Create(const AFieldName: string);
begin
  inherited Create;
  FIsStored := true;
  FFieldName := AFieldName;
end;

procedure TDataSetHelper.LoadInstanceFromCurrent<T>(AInstance: T);
var
  tmp: TDataSetRecord<T>;
begin
  tmp := TDataSetRecord<T>.Create(Self, AInstance);
  try
    tmp.LoadFromCurrent(AInstance);
  finally
    tmp.Free;
  end;
end;

function TDataSetHelper.GetCurrentRec<T>: T;
var
  tmp: TDataSetRecord<T>;
begin
  tmp := TDataSetRecord<T>.Create(Self);
  try
    result := tmp.Current;
  finally
    tmp.Free;
  end;
end;

procedure TDataSetHelper.StoreInstanceToCurrent<T>(AInstance: T);
var
  tmp: TDataSetRecord<T>;
begin
  tmp := TDataSetRecord<T>.Create(Self, AInstance);
  try
    tmp.StoreToCurrent(AInstance);
  finally
    tmp.Free;
  end;
end;

function TDataSetHelper.Records<T>(AInstance: T): IRecords<T>;
begin
  Result := TRecordsInstance<T>.Create(Self, AInstance);
end;

function TDataSetHelper.Records<T>: IRecords<T>;
begin
  Result := TRecords<T>.Create(Self);
end;

procedure TDataSetHelper.SetCurrentRec<T>(AInstance: T);
var
  tmp: TDataSetRecord<T>;
begin
  tmp := TDataSetRecord<T>.Create(Self, AInstance);
  try
    tmp.StoreToCurrent(AInstance);
  finally
    tmp.Free;
  end;
end;

destructor TDataSetHelper.TDataSetEnumerator<T>.Destroy;
{ Restore the DataSet to its previous state. }
begin
  if FWasActive then begin
    { if we have a valid bookmark, use it }
    if FDataSet.BookmarkValid(FBookmark) then
      FDataSet.GotoBookmark(FBookmark);
    { I'm not sure, if FreeBokmark can handle nil pointers - so to be safe }
    if FBookmark <> nil then
      FDataSet.FreeBookmark(FBookmark);
  end
  else
    FDataSet.Active := false;
  { don't forget this one! }
  FDataSet.EnableControls;
  inherited;
end;

constructor TDataSetHelper.TDataSetRecord<T>.Create(ADataSet: TDataSet);
begin
  Create(ADataSet, Default(T));
end;

constructor TDataSetHelper.TDataSetRecord<T>.Create(ADataSet: TDataSet; const AInstance: T);
begin
  inherited Create;
  FDataSet := ADataSet;
  FInstance := AInstance;
  Initialize;
end;

destructor TDataSetHelper.TDataSetRecord<T>.Destroy;
begin
  FMappings.Free;
  inherited;
end;

function TDataSetHelper.TDataSetRecord<T>.CheckAttributes(obj: TRttiNamedObject): string;
var
  attr: TCustomAttribute;
  storedAttr: DBFieldAttribute;
begin
  case FMapMode of
    mapAuto: result := obj.Name;
    mapManual: result := '';
  end;
  for attr in obj.GetAttributes do begin
    if attr is DBFieldAttribute then begin
      storedAttr := attr as DBFieldAttribute;
      if storedAttr.IsStored then begin
        if storedAttr.FieldName > '' then begin
          Result := storedAttr.FieldName;
        end;
      end
      else begin
        Result := '';
      end;
      Break;
    end;
  end;
end;

function TDataSetHelper.TDataSetRecord<T>.GetCurrent: T;
begin
  Result := Instance;
  LoadFromCurrent(Result);
end;

function TDataSetHelper.TDataSetRecord<T>.GetMapMode(obj: TRttiNamedObject): TDBFieldsMapping;
var
  attr: TCustomAttribute;
begin
  result := DefaultDBFieldsMapping;
  for attr in obj.GetAttributes do begin
    if attr is DBFieldsAttribute then begin
      Exit((attr as DBFieldsAttribute).Mode);
    end;
  end;
end;

procedure TDataSetHelper.TDataSetRecord<T>.Initialize;
begin
  LoadMappings;
end;

procedure TDataSetHelper.TDataSetRecord<T>.LoadFromCurrent(var Target: T);
var
  mapping: TMapping;
begin
  for mapping in FMappings do begin
    mapping.LoadFromField(Target);
  end;
end;

procedure TDataSetHelper.TDataSetRecord<T>.LoadMappings;
var
  field: TRttiField;
  fld: TField;
  prop: TRttiProperty;
  rttyType: TRTTIType;
begin
  FMappings := TObjectList<TMapping>.Create;
  FRTTIContext := TRTTIContext.Create;
  rttyType := FRTTIContext.GetType(TypeInfo(T));
  if rttyType.IsRecord then begin
    FInstance := Default(T);
  end
  else if rttyType.IsInstance then begin
    if TValue.From<T>(Instance).AsObject = nil then begin
      raise Exception.Create('No instance provided');
    end;
  end
  else begin
    raise Exception.Create('Only records and classes allowed');
  end;
  FMapMode := GetMapMode(rttyType);
  for field in rttyType.GetFields do begin
    fld := FDataSet.FindField(CheckAttributes(field));
    if fld = nil then Continue;
    FMappings.Add(TFieldMapping.Create(fld, field, rttyType.IsInstance));
  end;
  for prop in rttyType.GetProperties do begin
    fld := FDataSet.FindField(CheckAttributes(prop));
    if fld = nil then Continue;
    FMappings.Add(TPropMapping.Create(fld, prop, rttyType.IsInstance));
  end;
end;

procedure TDataSetHelper.TDataSetRecord<T>.StoreToCurrent(var Source: T);
var
  mapping: TMapping;
begin
  for mapping in FMappings do begin
    mapping.StoreToField(Source);
  end;
end;

procedure TDataSetHelper.TDataSetEnumerator<T>.Initialize;
{ The enumerator is automatically created and destroyed in the for-in loop.
  So we remember the active state and set a flag that the first MoveNext will
  not move to the next record, but stays on the first one instead. }
begin
  { save the Active state }
  FWasActive := FDataSet.Active;
  { avoid flickering }
  FDataSet.DisableControls;
  if FWasActive then begin
    { get a bookmark of the Current position - even if it is invalid }
    FBookmark := FDataSet.GetBookmark;
    FDataSet.First;
  end
  else begin
    { FBookmark is initialized to nil anyway, so no need to set it here }
    FDataSet.Active := true;
  end;
  FMoveToFirst := true;
  inherited;
end;

function TDataSetHelper.TDataSetEnumerator<T>.MoveNext: Boolean;
begin
  { Check if we have to move to the first record, which has been done already
    during Create. }
  if FMoveToFirst then
    FMoveToFirst := false
  else
    FDataSet.Next;
  Result := not FDataSet.EoF;
end;

constructor TDataSetHelper.TRecords<T>.Create(ADataSet: TDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

function TDataSetHelper.TRecords<T>.GetEnumerator: TDataSetEnumerator<T>;
begin
  Result := TDataSetEnumerator<T>.Create(FDataSet);
end;

constructor TDataSetHelper.TDataSetRecord<T>.TMapping.Create(AField: TField; AIsInstanceType: Boolean);
begin
  inherited Create;
  FField := AField;
  FIsInstanceType := AIsInstanceType;
end;

function TDataSetHelper.TDataSetRecord<T>.TMapping.GetPointer(var Target: T): Pointer;
begin
  if IsInstanceType then begin
    result := TValue.From<T>(Target).AsObject;
  end
  else begin
    result := @Target;
  end;
end;

constructor TDataSetHelper.TDataSetRecord<T>.TFieldMapping.Create(AField: TField; ARTTIField: TRTTIField;
    AIsInstanceType: Boolean);
begin
  inherited Create(AField, AIsInstanceType);
  FRTTIField := ARTTIField;
end;

procedure TDataSetHelper.TDataSetRecord<T>.TFieldMapping.LoadFromField(var Target: T);
var
  val: TValue;
begin
  if FField.IsNull then
    val := TValue.Empty
  else
    val := TValue.FromVariant(FField.Value);
  FRTTIField.SetValue(GetPointer(Target), val);
end;

procedure TDataSetHelper.TDataSetRecord<T>.TFieldMapping.StoreToField(var Source: T);
begin
  FField.Value := FRTTIField.GetValue(GetPointer(Source)).AsVariant;
end;

constructor TDataSetHelper.TDataSetRecord<T>.TPropMapping.Create(AField: TField; ARTTIProp: TRttiProperty;
    AIsInstanceType: Boolean);
begin
  inherited Create(AField, AIsInstanceType);
  FRTTIProp := ARTTIProp;
end;

procedure TDataSetHelper.TDataSetRecord<T>.TPropMapping.StoreToField(var Source: T);
begin
  FField.Value := FRTTIProp.GetValue(GetPointer(Source)).AsVariant;
end;

procedure TDataSetHelper.TDataSetRecord<T>.TPropMapping.LoadFromField(var Target: T);
var
  val: TValue;
begin
  if FField.IsNull then
    val := TValue.Empty
  else
    val := TValue.FromVariant(FField.Value);
  FRTTIProp.SetValue(GetPointer(Target), val);
end;

constructor TDataSetHelper.TRecordsInstance<T>.Create(ADataSet: TDataSet; AInstance: T);
begin
  inherited Create(ADataSet);
  FInstance := AInstance;
end;

function TDataSetHelper.TRecordsInstance<T>.GetEnumerator: TDataSetEnumerator<T>;
begin
  Result := TDataSetEnumerator<T>.Create(FDataSet, FInstance);
end;

end.
