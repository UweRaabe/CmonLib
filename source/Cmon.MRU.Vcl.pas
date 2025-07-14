unit Cmon.MRU.Vcl;

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes,
  Vcl.Menus;

type
  TMRUMenuItem = class(TMenuItem);
  TMRUMenuItemClass = class of TMRUMenuItem;

type
  TMRUClickEvent = procedure(Sender: TObject; const FileName: string) of object;
  TMRUAdjustMenuItemEvent = procedure(Sender: TObject; MenuItem: TMRUMenuItem) of object;

type
  TCustomMRUFiles = class(TComponent)
  private
    FItems: TStringList;
    FMaxItems: Integer;
    FMaxMenuWidth: Integer;
    FOnAdjustMenuItem: TMRUAdjustMenuItemEvent;
    FOnClick: TMRUClickEvent;
    FParentMenuItem: TMenuItem;
    FPopupMenu: TPopupMenu;
    FShowAccelerators: Boolean;
    FShowFullPath: boolean;
    procedure ClearParentMenu(AParentItem: TMenuItem); overload;
    procedure ClearParentMenu; overload;
    function CurrentParentMenuItem: TMenuItem;
    function FixupDisplayString(const AFileName: string; N: Integer): string;
    procedure GenerateMenuItems;
    procedure ItemsChange(Sender: TObject);
    procedure RemoveParentMenuItem;
    procedure RemovePopupMenu;
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    procedure SetMaxItems(Value: Integer);
    procedure SetMaxMenuWidth(const Value: Integer);
    procedure SetParentMenuItem(const Value: TMenuItem);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetShowAccelerators(Value: Boolean);
    procedure SetShowFullPath(const Value: boolean);
  protected
    procedure DoAdjustMenuItem(MenuItem: TMRUMenuItem); virtual;
    procedure DoClick(Sender: TObject); virtual;
    function GetMenuItemClass: TMRUMenuItemClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(FileName: string);
    procedure BoundToMaxItems;
    function RemoveItem(const FileName: string): boolean;
    property Items: TStrings read GetItems write SetItems;
    property MaxItems: Integer read FMaxItems write SetMaxItems default 8;
    property MaxMenuWidth: Integer read FMaxMenuWidth write SetMaxMenuWidth default 0;
    property ParentMenuItem: TMenuItem read FParentMenuItem write SetParentMenuItem;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShowAccelerators: Boolean read FShowAccelerators write SetShowAccelerators default True;
    property ShowFullPath: boolean read FShowFullPath write SetShowFullPath default True;
    property OnAdjustMenuItem: TMRUAdjustMenuItemEvent read FOnAdjustMenuItem write FOnAdjustMenuItem;
    property OnClick: TMRUClickEvent read FOnClick write FOnClick;
  end;

type
  TMRUFiles = class(TCustomMRUFiles)
  published
    property MaxItems;
    property MaxMenuWidth;
    property OnAdjustMenuItem;
    property OnClick;
    property ParentMenuItem;
    property PopupMenu;
    property ShowAccelerators;
    property ShowFullPath;
  end;

implementation

uses
  Winapi.ShLwApi,
  Vcl.Graphics;

function PathEllipsis(const Path: string; MaxWidth: Integer): string;
begin
  if MaxWidth = 0 then
    Exit(Path);

  var BM := TBitmap.Create;
  try
    { select menu font }
    var NCM: TNonClientMetrics;
    NCM.cbSize := SizeOf(NCM);
    SystemParametersInfo(SPI_GETNONCLIENTMETRICS, NCM.cbSize, @NCM, 0);
    BM.Canvas.Font.Handle := CreateFontIndirect(NCM.lfMenuFont);

    var buf := StrAlloc(Length(Path) + 4); // because of DT_MODIFYSTRING
    try
      StrPCopy(buf, Path);
      var R := Rect(0, 0, MaxWidth, 255);
      DrawText(BM.Canvas.Handle, buf, -1, R, DT_SINGLELINE or DT_PATH_ELLIPSIS or DT_MODIFYSTRING or DT_CALCRECT);
      Result := StrPas(buf);
    finally
      StrDispose(buf);
    end;
  finally
    BM.Free;
  end;
end;

constructor TCustomMRUFiles.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;
  FMaxItems := 8;
  FShowFullPath := True;
  FShowAccelerators := True;
end;

destructor TCustomMRUFiles.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TCustomMRUFiles.AddItem(FileName: string);
begin
  FileName := Trim(FileName);
  if FileName <> '' then begin
    Items.BeginUpdate;
    try
      var I := Items.IndexOf(FileName);
      if I < 0 then begin
        Items.Insert(0, FileName);
        BoundToMaxItems;
      end
      else if I > 0 then
        Items.Move(I, 0);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TCustomMRUFiles.BoundToMaxItems;
begin
  Items.BeginUpdate;
  try
    while Items.Count > MaxItems do
      Items.Delete(Items.Count - 1);
  finally
    Items.EndUpdate;
  end;
end;

procedure TCustomMRUFiles.ClearParentMenu(AParentItem: TMenuItem);
begin
  if AParentItem = nil then Exit;

  { backwards because of Delete }
  for var I := AParentItem.Count-1 downto 0 do begin
    if AParentItem.Items[I] is TMRUMenuItem then
      AParentItem.Delete(I);
  end;
end;

procedure TCustomMRUFiles.ClearParentMenu;
begin
  ClearParentMenu(CurrentParentMenuItem);
end;

function TCustomMRUFiles.CurrentParentMenuItem: TMenuItem;
begin
  if PopupMenu <> nil then
    result := PopupMenu.Items
  else
    result := ParentMenuItem
end;

procedure TCustomMRUFiles.DoAdjustMenuItem(MenuItem: TMRUMenuItem);
begin
  if Assigned(FOnAdjustMenuItem) then FOnAdjustMenuItem(Self, MenuItem);
end;

procedure TCustomMRUFiles.DoClick(Sender: TObject);
begin
  if Assigned(FOnClick) and (Sender is TMRUMenuItem) then
    FOnClick(Self, Items[TMRUMenuItem(Sender).Tag]);
end;

function TCustomMRUFiles.FixupDisplayString(const AFileName: string; N: Integer): string;
begin
  Result := AFileName;
  if not ShowFullPath then
    Result := ExtractFileName(Result);
  Result := StringReplace(Result, '&', '&&', [rfReplaceAll]);
  Result := PathEllipsis(Result, MaxMenuWidth);

  if ShowAccelerators and (N < 36) then begin // 0..9, A..Z
    if N < 10 then begin
      { Add the numeric hotkey identifier. }
      Result := Format('&%d %s', [N, Result])
    end
    else begin
      var Ch := Chr(Ord('A') + N - 10);
      { Can't go beyond Z }
      if Ch <= 'Z' then begin // should already be covered, but...
        { Add an alphabetic hotkey identifier. }
        Result := Format('&%s %s', [Chr(N), Result]);
      end;
    end;
  end;
end;

procedure TCustomMRUFiles.GenerateMenuItems;
begin
  if csLoading in ComponentState then Exit;

  var parentItem := CurrentParentMenuItem;
  if parentItem = nil then Exit;

  ClearParentMenu(parentItem);
  for var I := 0 to Items.Count - 1 do begin
    var NewMenuItem := GetMenuItemClass.Create(Self);
    NewMenuItem.Caption := FixupDisplayString(Items[I], I+1);
    NewMenuItem.Tag := I;
    NewMenuItem.OnClick := DoClick;
    DoAdjustMenuItem(NewMenuItem);
    parentItem.Add(NewMenuItem);
  end;
end;

procedure TCustomMRUFiles.ItemsChange(Sender: TObject);
begin
  GenerateMenuItems;
end;

procedure TCustomMRUFiles.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then begin
    if AComponent = FPopupMenu then
      FPopupMenu := nil
    else if (AComponent = FParentMenuItem) then
      FParentMenuItem := nil;
  end;
  inherited;
end;

function TCustomMRUFiles.RemoveItem(const FileName: string): boolean;
var
  I: Integer;
begin
  I := Items.IndexOf(FileName);
  result := (I >= 0);
  if result then
    Items.Delete(I);
end;

procedure TCustomMRUFiles.RemoveParentMenuItem;
begin
  if FParentMenuItem <> nil then begin
    ClearParentMenu;
    FParentMenuItem.RemoveFreeNotification(Self);
  end;
end;

procedure TCustomMRUFiles.RemovePopupMenu;
begin
  if FPopupMenu <> nil then begin
    ClearParentMenu;
    FPopupMenu.RemoveFreeNotification(Self);
  end;
end;

function TCustomMRUFiles.GetItems: TStrings;
begin
  Result := Items;
end;

function TCustomMRUFiles.GetMenuItemClass: TMRUMenuItemClass;
begin
  Result := TMRUMenuItem;
end;

procedure TCustomMRUFiles.SetItems(const Value: TStrings);
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    Items.AddStrings(Value);
    BoundToMaxItems;
  finally
    Items.EndUpdate;
  end;
end;

procedure TCustomMRUFiles.SetMaxItems(Value: Integer);
begin
  if Value < 1 then
    Value := 1;

  if Value <> FMaxItems then begin
    FMaxItems := Value;
    BoundToMaxItems;
  end;
end;

procedure TCustomMRUFiles.SetMaxMenuWidth(const Value: Integer);
begin
  if FMaxMenuWidth <> Value then begin
    FMaxMenuWidth := Value;
    GenerateMenuItems;
  end;
end;

procedure TCustomMRUFiles.SetParentMenuItem(const Value: TMenuItem);
begin
  if FParentMenuItem <> Value then begin
    if Value <> nil then
      RemovePopupMenu;
    RemoveParentMenuItem;
    FParentMenuItem := Value;
    if FParentMenuItem <> nil then begin
      FParentMenuItem.FreeNotification(Self);
      GenerateMenuItems;
    end;
  end;
end;

procedure TCustomMRUFiles.SetPopupMenu(Value: TPopupMenu);
begin
  if FPopupMenu <> Value then begin
    if Value <> nil then
      RemoveParentMenuItem;
    RemovePopupMenu;
    FPopupMenu := Value;
    if FPopupMenu <> nil then begin
      FPopupMenu.FreeNotification(Self);
      GenerateMenuItems;
    end;
  end;
end;

procedure TCustomMRUFiles.SetShowAccelerators(Value: Boolean);
begin
  if FShowAccelerators <> Value then begin
    FShowAccelerators := Value;
    GenerateMenuItems;
  end;
end;

procedure TCustomMRUFiles.SetShowFullPath(const Value: boolean);
begin
  if FShowFullPath <> Value then begin
    FShowFullPath := Value;
    GenerateMenuItems;
  end;
end;

end.
