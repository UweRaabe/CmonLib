unit Cmon.DataSense.Design;

interface

uses
  System.Classes,
  Cmon.DataSense,
  DesignIntf, DesignEditors, DesignMenus;

type
  TDataSenseEditor = class(TComponentEditor)
  private
  class var
    class function GetAutoDataSense: Boolean; static;
    class procedure SetAutoDataSense(const Value: Boolean); static;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
    class property AutoDataSense: Boolean read GetAutoDataSense write SetAutoDataSense;
  end;

type
  TLinkedPropertyFilter = class(TSelectionEditor, ISelectionPropertyFilter)
  const
    cDataSource = 'DataSource';
    cDataField = 'DataField';
  private
    function FindContainer: TDataSense;
  protected
    procedure FilterProperties(const ASelection: IDesignerSelections; const ASelectionProperties: IInterfaceList);
    procedure InternalRequiresUnits(Proc: TGetStrProc); virtual;
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

type
  TLinkedPropertyFilterVCL = class(TLinkedPropertyFilter)
  protected
    procedure InternalRequiresUnits(Proc: TGetStrProc); override;
  public
  end;

type
  TLinkedPropertyFilterFMX = class(TLinkedPropertyFilter)
  protected
    procedure InternalRequiresUnits(Proc: TGetStrProc); override;
  public
  end;

procedure Register;

implementation

uses
  System.Win.Registry,
  System.SysUtils, System.TypInfo,
  Vcl.Controls,
  FMX.Controls,
  Cmon.Utilities, Cmon.DataSense.VCL, Cmon.DataSense.FMX,
  DsnDBCst, DBReg, ColnEdit, ToolsAPI;

procedure Register;
begin
  RegisterComponents(srDControls, [TDataSense]);

  RegisterComponentEditor(TDataSense, TDataSenseEditor);

  RegisterSelectionEditor(TComponent, TLinkedPropertyFilter);
  RegisterSelectionEditor(Vcl.Controls.TControl, TLinkedPropertyFilterVCL);
  RegisterSelectionEditor(FMX.Controls.TControl, TLinkedPropertyFilterFMX);

  { we won't this to be editable in the Object Inspector, but we need the streaming }
  UnlistPublishedProperty(TDataSenseItem, 'Target');
end;

type
  TPropertyEditorHelper = class helper for TPropertyEditor
  public
    procedure LinkProperty(AInstance: TPersistent; const APropName: string);
  end;

procedure TPropertyEditorHelper.LinkProperty(AInstance: TPersistent; const APropName: string);
begin
  SetPropEntry(0, AInstance, System.TypInfo.GetPropInfo(AInstance, APropName));
end;

procedure TLinkedPropertyFilter.FilterProperties(const ASelection: IDesignerSelections; const ASelectionProperties: IInterfaceList);

  function HasDataSourceProp(Target: TComponent): Boolean;
  begin
    Result := GetPropInfo(Target, cDataSource) <> nil;
  end;

  procedure InsertProperty(AProp: IProperty);
  begin
    for var idx := 0 to ASelectionProperties.Count - 1 do begin
      var prop := ASelectionProperties[idx] as IProperty;
      if AProp.GetName < prop.GetName then begin
        ASelectionProperties.Insert(idx, AProp);
        Break;
      end;
    end;
  end;

begin
  { if we already have a DataSource property there is nothing to do for us here }
  for var idx := 0 to ASelectionProperties.Count - 1 do begin
    var prop := ASelectionProperties[idx] as IProperty;
    if prop.GetName.Equals(cDataSource) then Exit;
  end;

  { Check if the selection contains TComponents only.
    Also these components must be supported or already have a DataSource property.
    If f.i. a TDBEdit and TEdit are selected, they don't share a DataSource property by default,
    but we can add one to the TEdit and use the built-in from the TDBEdit to set both to the same TDataSource.
  }
  for var I := 0 to ASelection.Count - 1 do begin
    var item := ASelection[I];
    { only components support the observer pattern }
    if not item.InheritsFrom(TComponent) then Exit;
    var target := item as TComponent;
    if not (TDataSense.SupportsLinking(target) or HasDataSourceProp(target)) then Exit;
  end;

  { check if we can find a TDataLinkContainer }
  var container := FindContainer;
  if (container = nil) then Exit;

  for var I := 0 to ASelection.Count - 1 do
  begin
    var target := ASelection[I] as TComponent;
    if HasDataSourceProp(target) then begin
      var prop := TComponentProperty.Create(Designer, 1);
      prop.LinkProperty(target, cDataSource);
    end
    else begin
      var data := container.FindDataSenseItem(target);
      if data = nil then begin
        container.AddDataSenseItem(target);
        Designer.Modified;
        Exit;
      end;

      if ASelection.Count = 1 then begin
        var fldProp := TDataFieldProperty.Create(Designer, 1);
        fldProp.LinkProperty(Data, cDataField);
        InsertProperty(fldProp);
      end;

      var srcProp := TComponentProperty.Create(Designer, 1);
      srcProp.LinkProperty(Data, cDataSource);
      InsertProperty(srcProp);
    end;
  end;
end;

function TLinkedPropertyFilter.FindContainer: TDataSense;
begin
  Result := nil;
  for var cmp in Designer.Root.ComponentsOf<TDataSense> do
    Exit(cmp);
  if TDataSenseEditor.AutoDataSense then begin
    var list := CreateSelectionList;
    Designer.GetSelections(list);
    Result := Designer.CreateComponent(TDataSense, Designer.Root, 0, 0, 0, 0) as TDataSense;
    Designer.SetSelections(list);
  end;
end;

procedure TLinkedPropertyFilter.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  if FindContainer <> nil then
    InternalRequiresUnits(Proc);
end;

procedure TLinkedPropertyFilter.InternalRequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc(TDataSense.UnitName);
end;

procedure TLinkedPropertyFilterFMX.InternalRequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc(TDataSenseLinkFMX.UnitName);
end;

procedure TLinkedPropertyFilterVCL.InternalRequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc(TDataSenseLinkVCL.UnitName);
end;

procedure TDataSenseEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowCollectionEditor(Designer, GetComponent, TDataSense(GetComponent).DataLinks, 'TDataLinks');
    1: AutoDataSense := not AutoDataSense;
  end;
end;

class function TDataSenseEditor.GetAutoDataSense: Boolean;
begin
  Result := False;
  var reg := TRegistry.Create;
  try
    var svc := BorlandIDEServices.GetService(IOTAServices) as IOTAServices;
    var key := svc.GetBaseRegistryKey + '\DataSense';
    if reg.OpenKeyReadOnly(key) then begin
      if reg.ValueExists('AutoDataSense') then
        Result := reg.ReadBool('AutoDataSense');
    end;
  finally
    reg.Free;
  end;
end;

function TDataSenseEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit DataLinks...';
    1: Result := 'Auto DataSense';
  else
    Result := '';
  end;
end;

function TDataSenseEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TDataSenseEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  case Index of
    1: AItem.Checked := AutoDataSense;
  end;
end;

class procedure TDataSenseEditor.SetAutoDataSense(const Value: Boolean);
begin
  var reg := TRegistry.Create;
  try
    var svc := BorlandIDEServices.GetService(IOTAServices) as IOTAServices;
    var key := svc.GetBaseRegistryKey + '\DataSense';
    if reg.OpenKey(key, True) then begin
      reg.WriteBool('AutoDataSense', Value);
    end;
  finally
    reg.Free;
  end;
end;

end.
