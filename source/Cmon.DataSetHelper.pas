unit Cmon.DataSetHelper;

{ Note: TValue doesn't handle BCD data types, so we convert them to/from Double.
  As this might affect precision in some cases, it is suggested to handle these fields manually to avoid that.
  In addition a SQLTimeStampField value is treated as TDateTime.
}

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
        FType: TRttiType;
        function GetFieldValue: TValue;
        procedure SetFieldValue(const Value: TValue);
      protected
        function GetPointer(var Target: T): Pointer;
        property FieldValue: TValue read GetFieldValue write SetFieldValue;
        property IsInstanceType: Boolean read FIsInstanceType;
      public
        constructor Create(AField: TField; AType: TRttiType; AIsInstanceType: Boolean);
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
    function AssureEdit: Boolean;
    procedure AssurePost;
    procedure ExportToCSV(const AFileName: string);
    function GetCurrentRec<T: record>: T;
    procedure SetCurrentRec<T: record>(AInstance: T);
    procedure LoadInstanceFromCurrent<T: class>(AInstance: T);
    procedure StoreInstanceToCurrent<T: class>(AInstance: T);
    function Records<T: class>(AInstance: T): IRecords<T>; overload;
    function Records<T: record>: IRecords<T>; overload;
    procedure TriggerCalcFields;
  end;

type
  TRecordFields = class
  type
    TFieldsDataLink = class(TDataLink)
    private
      FOwner: TRecordFields;
      procedure UpdateField;
    protected
      procedure ActiveChanged; override;
      procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
      procedure LayoutChanged; override;
      property Owner: TRecordFields read FOwner;
    public
      constructor Create(AOwner: TRecordFields);
    end;
  private
    FDataSource: TDataSource;
    FDataLink: TFieldsDataLink;
    FFieldsAreLinked: Boolean;
    FMapMode: TDBFieldsMapping;
    FOnCalcFields: TDataSetNotifyEvent;
    FRTTIContext: TRTTIContext;
    function CheckAttributes(obj: TRttiNamedObject; const ADefault: string = ''): string;
    procedure DoCalcFields(ADataSet: TDataSet);
    function GetDataSet: TDataSet;
    function GetMapMode(obj: TRttiNamedObject): TDBFieldsMapping;
    procedure LinkFields;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetFieldsAreLinked(const Value: Boolean);
    procedure UnlinkFields;
  protected
    procedure InternalCalcFields; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CalcFields;
    property DataSet: TDataSet read GetDataSet write SetDataSet;
    property FieldsAreLinked: Boolean read FFieldsAreLinked write SetFieldsAreLinked;
    property OnCalcFields: TDataSetNotifyEvent read FOnCalcFields write FOnCalcFields;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.StrUtils, System.TypInfo;

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

function TDataSetHelper.AssureEdit: Boolean;
begin
  Result := False;
  if not (State in [dsEdit, dsInsert]) then begin
    Edit;
    Result := True;
  end;
end;

procedure TDataSetHelper.AssurePost;
begin
  if State in [dsEdit, dsInsert] then begin
    Post;
  end;
end;

procedure TDataSetHelper.ExportToCSV(const AFileName: string);
var
  fld: TField;
  lst: TStringList;
  wasActive: Boolean;
  writer: TTextWriter;
begin
  writer := TStreamWriter.Create(AFileName);
  try
    lst := TStringList.Create;
    try
      lst.QuoteChar := '"';
      lst.Delimiter := ';';
      wasActive := Active;
      try
        Active := true;
        GetFieldNames(lst);
        writer.WriteLine(lst.DelimitedText);
        First;
        while not Eof do begin
          lst.Clear;
          for fld in Fields do
            lst.Add(fld.Text);
          writer.WriteLine(lst.DelimitedText);
          Next;
        end;
      finally
        Active := wasActive;
      end;
    finally
      lst.Free;
    end;
  finally
    writer.Free;
  end;
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

procedure TDataSetHelper.TriggerCalcFields;
begin
  GetCalcFields(ActiveBuffer);
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

constructor TDataSetHelper.TDataSetRecord<T>.TMapping.Create(AField: TField; AType: TRttiType; AIsInstanceType: Boolean);
begin
  inherited Create;
  FField := AField;
  FIsInstanceType := AIsInstanceType;
  FType := AType;
end;

function TDataSetHelper.TDataSetRecord<T>.TMapping.GetFieldValue: TValue;
begin
  Result := TValue.Empty;
  if not FField.IsNull then begin
    case FType.TypeKind of
      tkUnknown: ;
      tkInteger: Result := FField.AsInteger;
      tkChar,
      tkWChar: begin
        var S := FField.AsString;
        if not S.IsEmpty then
          Result := S[1];
      end;
      tkEnumeration: begin
        if SameText(FType.Name, 'Boolean') then
          Result := FField.AsBoolean;
      end;
      tkFloat: begin
        { handle special Double cases }
        if MatchText(FType.Name, ['TDate', 'TTime', 'TDateTime']) then
          Result := FField.AsDateTime
        else
          Result := FField.AsFloat;
      end;
      tkLString,
      tkWString,
      tkUString,
      tkString: Result := FField.AsString;
      tkSet: ;
      tkClass: ;
      tkMethod: ;
      tkArray: ;
      tkRecord: ;
      tkInterface: ;
      tkInt64: Result := FField.AsLargeInt;
      tkDynArray: begin
        if SameText(FType.Name, 'TBytes') then
          Result := TValue.From<TBytes>(FField.AsBytes);
      end;
      tkClassRef: ;
      tkPointer: ;
      tkProcedure: ;
      tkMRecord: ;
    else
      Result := TValue.FromVariant(FField.Value);
    end;
  end;
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

procedure TDataSetHelper.TDataSetRecord<T>.TMapping.SetFieldValue(const Value: TValue);
begin
  if FField.DataType = ftAutoInc then Exit;
  if FField.ReadOnly then Exit;

  if Value.IsEmpty then begin
    FField.Clear;
  end
  else begin
    case FType.TypeKind of
      tkUnknown: ;
      tkInteger: FField.AsInteger := Value.AsInteger;
      tkChar,
      tkWChar: FField.AsString := Value.AsString;
      tkEnumeration: begin
        if SameText(FType.Name, 'Boolean') then
          FField.AsBoolean := Value.AsBoolean;
      end;
      tkFloat: begin
        { handle special Double cases }
        if MatchText(FType.Name, ['TDate', 'TTime', 'TDateTime']) then
          FField.AsDateTime := Value.AsType<TDateTime>
        else
          FField.AsFloat := Value.AsExtended;
      end;
      tkLString,
      tkWString,
      tkUString,
      tkString: FField.AsString := Value.AsString;
      tkSet: ;
      tkClass: ;
      tkMethod: ;
      tkArray: ;
      tkRecord: ;
      tkInterface: ;
      tkInt64: FField.AsLargeInt := Value.AsInt64;
      tkDynArray: begin
        if SameText(FType.Name, 'TBytes') then
          FField.AsBytes := Value.AsType<TBytes>;
      end;
      tkClassRef: ;
      tkPointer: ;
      tkProcedure: ;
      tkMRecord: ;
    else
      FField.Value := Value.AsVariant;
    end;
  end;
end;

constructor TDataSetHelper.TDataSetRecord<T>.TFieldMapping.Create(AField: TField; ARTTIField: TRTTIField;
    AIsInstanceType: Boolean);
begin
  inherited Create(AField, ARTTIField.FieldType, AIsInstanceType);
  FRTTIField := ARTTIField;
end;

procedure TDataSetHelper.TDataSetRecord<T>.TFieldMapping.LoadFromField(var Target: T);
begin
  FRTTIField.SetValue(GetPointer(Target), FieldValue);
end;

procedure TDataSetHelper.TDataSetRecord<T>.TFieldMapping.StoreToField(var Source: T);
begin
  FieldValue := FRTTIField.GetValue(GetPointer(Source));
end;

constructor TDataSetHelper.TDataSetRecord<T>.TPropMapping.Create(AField: TField; ARTTIProp: TRttiProperty;
    AIsInstanceType: Boolean);
begin
  inherited Create(AField, ARTTIProp.PropertyType, AIsInstanceType);
  FRTTIProp := ARTTIProp;
end;

procedure TDataSetHelper.TDataSetRecord<T>.TPropMapping.StoreToField(var Source: T);
begin
  FieldValue := FRTTIProp.GetValue(GetPointer(Source));
end;

procedure TDataSetHelper.TDataSetRecord<T>.TPropMapping.LoadFromField(var Target: T);
begin
  FRTTIProp.SetValue(GetPointer(Target), FieldValue);
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

constructor TRecordFields.Create;
begin
  inherited;
  FDataSource := TDataSource.Create(nil);
  FDataLink := TFieldsDataLink.Create(Self);
  FDataLink.DataSource := FDataSource;
end;

destructor TRecordFields.Destroy;
begin
  FDataLink.Free;
  FDataSource.Free;
  inherited Destroy;
end;

procedure TRecordFields.CalcFields;
begin
  if (DataSet <> nil) and (FDataSource.State <> dsInactive) then
    InternalCalcFields;
end;

function TRecordFields.CheckAttributes(obj: TRttiNamedObject; const ADefault: string = ''): string;
var
  attr: TCustomAttribute;
  storedAttr: DBFieldAttribute;
begin
  case FMapMode of
    mapAuto: begin
      if ADefault > '' then
        result := ADefault
      else
        result := obj.Name;
    end;
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

procedure TRecordFields.DoCalcFields(ADataSet: TDataSet);
begin
  if (ADataSet = DataSet) then begin
    FieldsAreLinked := True;
    if Assigned(FOnCalcFields) then
      FOnCalcFields(DataSet)
    else
      CalcFields;
  end;
end;

function TRecordFields.GetDataSet: TDataSet;
begin
  Result := FDataSource.DataSet;
end;

function TRecordFields.GetMapMode(obj: TRttiNamedObject): TDBFieldsMapping;
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

procedure TRecordFields.LinkFields;
var
  field: TRttiField;
  fld: TField;
  prop: TRttiProperty;
  rttyType: TRTTIType;
begin
  if (DataSet = nil) or not DataSet.Active then Exit;

  FRTTIContext := TRTTIContext.Create;
  rttyType := FRTTIContext.GetType(ClassType);
  FMapMode := GetMapMode(rttyType);
  for field in rttyType.GetFields do begin
    var def := field.Name;
    if def.StartsWith('F', True) then
      def := def.Remove(0, 1);
    fld := DataSet.FindField(CheckAttributes(field, def));
    if fld = nil then Continue;
    field.SetValue(Self, TValue.From<TField>(fld));
  end;
  for prop in rttyType.GetProperties do begin
    if not prop.IsWritable then Continue;
    fld := DataSet.FindField(CheckAttributes(prop));
    if fld = nil then Continue;
    prop.SetValue(Self, TValue.From<TField>(fld));
  end;
end;

procedure TRecordFields.UnlinkFields;
var
  field: TRttiField;
  fld: TField;
  prop: TRttiProperty;
  rttyType: TRTTIType;
begin
  if (DataSet = nil) or not DataSet.Active then Exit;

  FRTTIContext := TRTTIContext.Create;
  rttyType := FRTTIContext.GetType(ClassType);
  FMapMode := GetMapMode(rttyType);
  for field in rttyType.GetFields do begin
    fld := DataSet.FindField(CheckAttributes(field));
    if fld = nil then Continue;
    if field.GetValue(Self).AsObject = fld then
      field.SetValue(Self, TValue.From<TField>(nil));
  end;
  for prop in rttyType.GetProperties do begin
    if not prop.IsWritable then Continue;
    fld := DataSet.FindField(CheckAttributes(prop));
    if fld = nil then Continue;
    if prop.GetValue(Self).AsObject = fld then
      prop.SetValue(Self, TValue.From<TField>(nil));
  end;
end;

procedure TRecordFields.InternalCalcFields;
begin
end;

procedure TRecordFields.SetDataSet(const Value: TDataSet);
begin
  FDataSource.DataSet := Value;
  if (Value <> nil) then begin
    if not Assigned(Value.OnCalcFields) then
      Value.OnCalcFields := DoCalcFields;
    if Value.Active then
      Value.TriggerCalcFields;
  end;
end;

procedure TRecordFields.SetFieldsAreLinked(const Value: Boolean);
begin
  if FFieldsAreLinked <> Value then
  begin
    if FFieldsAreLinked then
      UnlinkFields;
    if Value then
      LinkFields;
    FFieldsAreLinked := Value;
  end;
end;

constructor TRecordFields.TFieldsDataLink.Create(AOwner: TRecordFields);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TRecordFields.TFieldsDataLink.ActiveChanged;
begin
  UpdateField;
end;

procedure TRecordFields.TFieldsDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  inherited;
  if Event = deDisabledStateChange then
    UpdateField;
end;

procedure TRecordFields.TFieldsDataLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TRecordFields.TFieldsDataLink.UpdateField;
begin
  Owner.FieldsAreLinked := Active;
end;

end.
