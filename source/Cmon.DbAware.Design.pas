unit Cmon.DbAware.Design;

interface

uses
  System.Classes,
  DesignIntf, DesignEditors;

type
  TLinkedPropertyFilter = class(TSelectionEditor, ISelectionPropertyFilter)
  const
    cDataSource = 'DataSource';
    cDataField = 'DataField';
  protected
    procedure FilterProperties(const ASelection: IDesignerSelections; const ASelectionProperties: IInterfaceList);
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

type
  TLinkedPropertyFilterVCL = class(TLinkedPropertyFilter)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

type
  TLinkedPropertyFilterFMX = class(TLinkedPropertyFilter)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  System.SysUtils, System.TypInfo,
  Vcl.Controls,
  FMX.Controls,
  Cmon.Utilities, Cmon.DbAware.ObserverLink, Cmon.Vcl.DbAware.Components, Cmon.Fmx.DbAware.Components,
  DsnDBCst, DBReg;

procedure Register;
begin
  RegisterComponents(srDControls, [TDataLinkContainer]);

  RegisterSelectionEditor(TComponent, TLinkedPropertyFilter);
  RegisterSelectionEditor(Vcl.Controls.TControl, TLinkedPropertyFilterVCL);
  RegisterSelectionEditor(FMX.Controls.TControl, TLinkedPropertyFilterFMX);

  { we won't this to be editable in the Object Inspector, but we need the streaming }
  UnlistPublishedProperty(TDataLinkItem, 'Target');
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

  function FindContainer: TDataLinkContainer;
  begin
    for var cmp in Designer.Root.ComponentsOf<TDataLinkContainer> do
      Exit(cmp);
    Result := nil;
//    Result := Designer.CreateComponent(TDataLinkContainer, Designer.Root, 0, 0, 48, 48) as TDataLinkContainer;
  end;

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
  { check if we can find a TDataLinkContainer }
  var container := FindContainer;
  if (container = nil) then Exit;

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
    if not (TDataLinkSupport.SupportsLinking(target) or HasDataSourceProp(target)) then Exit;
  end;

  for var I := 0 to ASelection.Count - 1 do
  begin
    var target := ASelection[I] as TComponent;
    if HasDataSourceProp(target) then begin
      var prop := TComponentProperty.Create(Designer, 1);
      prop.LinkProperty(target, cDataSource);
    end
    else begin
      var data := container.FindOrCreateDataLink(target);

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

procedure TLinkedPropertyFilter.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc(TDataLinkSupport.UnitName);
end;

procedure TLinkedPropertyFilterFMX.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc(TObserverDataLinkFMX.UnitName);
end;

procedure TLinkedPropertyFilterVCL.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc(TObserverDataLinkVCL.UnitName);
end;

end.
