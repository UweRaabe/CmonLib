unit Common.Form;

interface

uses
  System.Classes,
  Vcl.Forms,
  Common.Types;

type
  TDataModuleClass = class of TDataModule;
  TCommonForm = class(TForm, IRedirectReferences)
  private
    FHandledReferences: TStrings;
    function GetVirtualImageListCount: Integer;
  protected
    procedure GrabVirtualImageLists(AClass: TDataModuleClass);
    procedure RedirectReferences;
    procedure ResolveReferences;
    property HandledReferences: TStrings read FHandledReferences;
    property VirtualImageListCount: Integer read GetVirtualImageListCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

type
  TForm = TCommonForm;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.VirtualImageList,
  Cmon.Utilities;

constructor TCommonForm.Create(AOwner: TComponent);
begin
  FHandledReferences := TStringList.Create(dupIgnore, True, False);
  inherited Create(AOwner);
end;

destructor TCommonForm.Destroy;
begin
  inherited Destroy;
  FHandledReferences.Free;
end;

procedure TCommonForm.AfterConstruction;
begin
  RedirectReferences;
  inherited;
end;

function TCommonForm.GetVirtualImageListCount: Integer;
begin
  Result := 0;
  for var img in ComponentsOf<TVirtualImageList> do
    Inc(Result);
end;

procedure TCommonForm.GrabVirtualImageLists(AClass: TDataModuleClass);
var
  cmp: TComponent;
  instance: TDataModule;
begin
  { just creating the instance will resolve all pending references to it }
  instance := AClass.Create(nil);
  try
    var dmPPI := instance.PixelsPerInch;
    if dmPPI = 0 then
      dmPPI := Screen.DefaultPixelsPerInch; { should be 96 }
    if HandledReferences.IndexOf(instance.Name) < 0 then
    begin
      HandledReferences.Add(instance.Name);
      for var I := instance.ComponentCount - 1 downto 0 do
      begin
        cmp := instance.Components[I];
        if cmp is TVirtualImageList then
        begin
          { Moving the component to Self will keep all references intact.
            That is why a clone won't work here. }
          InsertComponent(cmp);
          if CurrentPPI <> dmPPI then begin
            var img := TVirtualImageList(cmp);
            var W := MulDiv(img.Width, CurrentPPI, dmPPI);
            var H := MulDiv(img.Height, CurrentPPI, dmPPI);
            img.SetSize(W, H);
          end;
        end;
      end;
    end;
  finally
    instance.Free;
  end;
end;

procedure TCommonForm.RedirectReferences;
{ static frames call this when their parent is set during loading }
begin
  { first resolve all existing references }
  for var reference in FHandledReferences do
    RedirectFixupReferences(nil, reference, Name);
  { now resolve any remaining references by cloning the necessary image lists }
  ResolveReferences;
end;

procedure TCommonForm.ResolveReferences;
var
  lst: TStringList;
begin
  lst := TStringList.Create();
  try
    GetFixupReferenceNames(nil, lst);
    for var reference in lst do begin
      var cls := GetClass('T' + reference);
      if (cls <> nil) and cls.InheritsFrom(TDataModule) then
        GrabVirtualImageLists(TDataModuleClass(cls));
    end;
  finally
    lst.Free;
  end;
end;

function FindGlobalComponent(const Name: string): TComponent;
begin
  Result := nil;
  var idx := Name.IndexOf('.');
  if idx > 0 then begin
    Result := System.Classes.FindGlobalComponent(Name.Remove(idx));
    if Result <> nil then begin
      Result := FindNestedComponent(Result, Name.Substring(idx + 1));
    end;
  end;
end;

initialization
  System.Classes.RegisterFindGlobalComponentProc(FindGlobalComponent);
finalization
  System.Classes.UnregisterFindGlobalComponentProc(FindGlobalComponent);
end.
