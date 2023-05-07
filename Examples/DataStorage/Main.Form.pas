unit Main.Form;

interface

uses
  Winapi.Windows,
  System.Classes, System.IniFiles, System.Types,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.Dialogs, Vcl.ComCtrls, Vcl.NumberBox,
  Cmon.DataStorage, Cmon.Vcl.Forms,
  Main.Frame;

{ DefaultAttribute from Cmon.DataStorage supports only basic types.
  If we have a custom enumeration like below, we need a special descendant. }
type
{$SCOPEDENUMS ON}
  TMyEnum = (none, enum1, enum2, enum3, enum4);
{$SCOPEDENUMS OFF}

type
  DefaultMyEnumAttribute = class(DefaultAttribute)
  public
    constructor Create(AValue: TMyEnum); overload;
  end;

{ We can make use of TDataStorage even for our own Storage and Default attributes. }
type
  LayoutAttribute = class(TCustomStorageAttribute);
  LayoutDefaultAttribute = class(TCustomDefaultAttribute);

type
  TDemoMainForm = class(TForm)
    SomeTextEdit: TLabeledEdit;
    SomeIndexSelector: TRadioGroup;
    DemoFrame1: TDemoFrame;
    DemoFrame2: TDemoFrame;
    TitleLabel: TLabel;
    SomeEnumSelector: TRadioGroup;
    SomeBooleanCheck: TCheckBox;
    SaveSettingsButton: TButton;
    LoadSettingsButton: TButton;
    LoadSettingsDialog: TFileOpenDialog;
    SaveSettingsDialog: TFileSaveDialog;
    RestoreDefaultsButton: TButton;
    MainDataTree: TTreeView;
    pnlBottom: TPanel;
    pnlLeft: TPanel;
    pnlCenter: TPanel;
    pnlFrames: TPanel;
    splFrames: TSplitter;
    splMainData: TSplitter;
    splLeft: TSplitter;
    LoadLayoutButton: TButton;
    SaveLayoutButton: TButton;
    RestoreLayoutButton: TButton;
    SomeStringEdit: TLabeledEdit;
    SomeIntegerEdit: TNumberBox;
    procedure FormCreate(Sender: TObject);
    procedure LoadLayoutButtonClick(Sender: TObject);
    procedure LoadSettingsButtonClick(Sender: TObject);
    procedure MainDataTreeDblClick(Sender: TObject);
    procedure MainDataTreeEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure MainDataTreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure MainDataTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RestoreDefaultsButtonClick(Sender: TObject);
    procedure RestoreLayoutButtonClick(Sender: TObject);
    procedure SaveLayoutButtonClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
  strict private
  class var
    FSettingsFileExtension: string;
    FSettingsFileName: string;
  private
    FLayoutFileName: string;
    function GetLayoutFileName: string;
    function GetLayoutRect: TRect;
    class function GetSettingsFileName: string; static;
    function GetSomeBoolean: Boolean;
    function GetSomeEnum: TMyEnum;
    function GetSomeIndex: Integer;
    function GetSomeText: string;
    function GetSplitterFrames: Integer;
    function GetSplitterLeft: Integer;
    function GetSplitterMainData: Integer;
    procedure LoadMainData;
    procedure PrepareFileDialog(ADialog: TCustomFileDialog);
    procedure SaveMainData;
    procedure SetLayoutRect(const Value: TRect);
    procedure SetSomeBoolean(const Value: Boolean);
    procedure SetSomeEnum(const Value: TMyEnum);
    procedure SetSomeIndex(const Value: Integer);
    procedure SetSomeText(const Value: string);
    procedure SetSplitterFrames(const Value: Integer);
    procedure SetSplitterLeft(const Value: Integer);
    procedure SetSplitterMainData(const Value: Integer);
  protected
    procedure LoadLayout; overload;
    procedure LoadSettings;
    procedure RestoreDefaults;
    procedure RestoreLayout;
    procedure SaveLayout; overload;
    procedure SaveSettings;
    function UnscaledValue(Value: Integer): Integer;
    function ScaledValue(Value: Integer): Integer;
  public
    procedure InitDefaults(Storage: TDataStorage); overload; override;
    procedure LoadFromStorage(Storage: TDataStorage); overload; override;
    procedure LoadLayout(const AFileName: string); overload;
    procedure SaveLayout(const AFileName: string); overload;
    procedure SaveToStorage(Storage: TDataStorage); overload; override;
    procedure UpdateTitle;
    property LayoutFileName: string read GetLayoutFileName write FLayoutFileName;
    class property SettingsFileExtension: string read FSettingsFileExtension write FSettingsFileExtension;
    class property SettingsFileName: string read GetSettingsFileName write FSettingsFileName;
    [Storage, Default(True)]
    property SomeBoolean: Boolean read GetSomeBoolean write SetSomeBoolean;
    [Storage, DefaultMyEnum(TMyEnum.none)]
    property SomeEnum: TMyEnum read GetSomeEnum write SetSomeEnum;
    [Storage, Default(1)]
    property SomeIndex: Integer read GetSomeIndex write SetSomeIndex;
    [Storage, Default('Hello World')]
    property SomeText: string read GetSomeText write SetSomeText;
    [Layout]
    property LayoutRect: TRect read GetLayoutRect write SetLayoutRect;
    [Layout, LayoutDefault(241)]
    property SplitterFrames: Integer read GetSplitterFrames write SetSplitterFrames;
    [Layout, LayoutDefault(249)]
    property SplitterLeft: Integer read GetSplitterLeft write SetSplitterLeft;
    [Layout, LayoutDefault(217)]
    property SplitterMainData: Integer read GetSplitterMainData write SetSplitterMainData;
  end;

var
  DemoMainForm: TDemoMainForm;

implementation

uses
  System.SysUtils, System.IOUtils, System.Rtti, System.UITypes,
  Vcl.Consts,
  Cmon.Utilities,
  Main.Data.Types;

{$R *.dfm}

resourcestring
  SLoadSettings = 'Load settings';
  SSaveSettings = 'Save settings';

constructor DefaultMyEnumAttribute.Create(AValue: TMyEnum);
begin
  inherited Create(TValue.From(AValue));
end;

type
  TMyEnumHelper = record helper for TMyEnum
  private
    function GetAsIndex: Integer;
    function GetAsString: string;
    procedure SetAsIndex(const Value: Integer);
    procedure SetAsString(const Value: string);
  public
    class function FromIndex(Value: Integer): TMyEnum; static;
    class function FromString(const Value: string): TMyEnum; static;
    class procedure ListNames(Target: TStrings); static;
    property AsIndex: Integer read GetAsIndex write SetAsIndex;
    property AsString: string read GetAsString write SetAsString;
  end;

function TMyEnumHelper.GetAsIndex: Integer;
begin
  Result := Ord(Self);
end;

function TMyEnumHelper.GetAsString: string;
begin
  Result := TRttiEnumerationType.GetName(Self);
end;

procedure TMyEnumHelper.SetAsIndex(const Value: Integer);
begin
  Self := TMyEnum(Value);
end;

procedure TMyEnumHelper.SetAsString(const Value: string);
begin
  Self := TRttiEnumerationType.GetValue<TMyEnum>(Value);
end;

class function TMyEnumHelper.FromIndex(Value: Integer): TMyEnum;
begin
  Result.AsIndex := Value;
end;

class function TMyEnumHelper.FromString(const Value: string): TMyEnum;
begin
  Result.AsString := Value;
end;

class procedure TMyEnumHelper.ListNames(Target: TStrings);
var
  enum: TMyEnum;
begin
  Target.BeginUpdate;
  try
    Target.Clear;
    for enum := Low(enum) to High(enum) do
      Target.Add(enum.AsString);
  finally
    Target.EndUpdate;
  end;
end;

procedure TDemoMainForm.FormCreate(Sender: TObject);
begin
  TMyEnum.ListNames(SomeEnumSelector.Items);
  inherited;
  LoadSettingsDialog.Title := SLoadSettings;
  SaveSettingsDialog.Title := SSaveSettings;
  PrepareFileDialog(LoadSettingsDialog);
  PrepareFileDialog(SaveSettingsDialog);
  UpdateTitle;
  for var frame in ComponentsOf<TDemoFrame> do
    frame.UpdateTitle;
end;

function TDemoMainForm.GetLayoutFileName: string;
begin
  Result := FLayoutFileName;
  if Result.IsEmpty then
    Result := TPath.Combine(TUtilities.UserDocumentsPath, TPath.ChangeExtension('Layout', SettingsFileExtension));
end;

function TDemoMainForm.GetLayoutRect: TRect;
begin
  Result := BoundsRect;
end;

class function TDemoMainForm.GetSettingsFileName: string;
begin
  if FSettingsFileName.IsEmpty then
    Result := TPath.ChangeExtension(TUtilities.GetExeName, SettingsFileExtension)
  else if TDataStorage.IsSupportedTargetExtension(TPath.GetExtension(FSettingsFileName)) then
    Result := FSettingsFileName
  else
    Result := TPath.ChangeExtension(FSettingsFileName, SettingsFileExtension);
end;

procedure TDemoMainForm.LoadSettingsButtonClick(Sender: TObject);
begin
  inherited;
  LoadSettings;
end;

procedure TDemoMainForm.RestoreDefaultsButtonClick(Sender: TObject);
begin
  inherited;
  RestoreDefaults;
end;

procedure TDemoMainForm.SaveSettingsButtonClick(Sender: TObject);
begin
  inherited;
  SaveSettings;
end;

function TDemoMainForm.GetSomeBoolean: Boolean;
begin
  Result := SomeBooleanCheck.Checked;
end;

function TDemoMainForm.GetSomeEnum: TMyEnum;
begin
  Result.AsIndex := SomeEnumSelector.ItemIndex;
end;

function TDemoMainForm.GetSomeIndex: Integer;
begin
  Result := SomeIndexSelector.ItemIndex;
end;

function TDemoMainForm.GetSomeText: string;
begin
  Result := SomeTextEdit.Text;
end;

function TDemoMainForm.GetSplitterFrames: Integer;
begin
  Result := UnscaledValue(DemoFrame1.Width);
end;

function TDemoMainForm.GetSplitterLeft: Integer;
begin
  Result := UnscaledValue(pnlLeft.Width);
end;

function TDemoMainForm.GetSplitterMainData: Integer;
begin
  Result := UnscaledValue(pnlFrames.Height);
end;

procedure TDemoMainForm.InitDefaults(Storage: TDataStorage);
begin
  inherited;
  Storage.InitDefaults(MainData);
  Storage.InitDefaults<TMainDataRec>(MainDataRec);
  LoadMainData;
end;

procedure TDemoMainForm.LoadFromStorage(Storage: TDataStorage);
begin
  inherited;
  Storage.LoadFromStorage(MainData);
  Storage.LoadFromStorage<TMainDataRec>(MainDataRec);
  LoadMainData;
end;

procedure TDemoMainForm.LoadLayout(const AFileName: string);
begin
  var storage := TDataStorage.Create(AFileName);
  try
    storage.LoadFromStorage<LayoutAttribute>(Self);
  finally
    storage.Free;
  end;
end;

procedure TDemoMainForm.LoadLayout;
begin
  LoadSettingsDialog.FileName := LayoutFileName;
  if LoadSettingsDialog.Execute then begin
    LayoutFileName := LoadSettingsDialog.FileName;
    LoadLayout(LayoutFileName);
  end;
end;

procedure TDemoMainForm.LoadLayoutButtonClick(Sender: TObject);
begin
  LoadLayout;
end;

procedure TDemoMainForm.LoadMainData;
begin
  MainDataTree.Items.Clear;

  var mainNode := MainDataTree.Items.AddChild(nil, 'MainData');
  MainDataTree.Items.AddChild(mainNode, MainData.SomeInteger.ToString);
  MainDataTree.Items.AddChild(mainNode, MainData.SomeString);

  var subNode := MainDataTree.Items.AddChild(mainNode, 'SubData');
  MainDataTree.Items.AddChild(subNode, MainData.SubData.SomeInteger.ToString);
  MainDataTree.Items.AddChild(subNode, MainData.SubData.SomeString);

  MainDataTree.FullExpand;

  SomeIntegerEdit.ValueInt := MainDataRec.SomeInteger;
  SomeStringEdit.Text  := MainDataRec.SomeString;
end;

procedure TDemoMainForm.PrepareFileDialog(ADialog: TCustomFileDialog);
begin
  var defaultExt := SettingsFileExtension;
  ADialog.FileTypes.Clear;
  for var target in TDataStorage.StorageTargets do begin
    var fileType := ADialog.FileTypes.Add;
    fileType.DisplayName := Format('%s (*%s)', [target.Description, target.FileExtension]);
    fileType.FileMask := Format('*%s', [target.FileExtension]);
    if SameText(defaultExt, target.FileExtension) then
      ADialog.FileTypeIndex := ADialog.FileTypes.Count;
  end;

  var arr := SDefaultFilter.Split(['|']);
  var fileType := ADialog.FileTypes.Add;
  fileType.DisplayName := arr[0];
  fileType.FileMask := arr[1];
  ADialog.DefaultExtension := defaultExt.Substring(1);
end;

procedure TDemoMainForm.SetSomeBoolean(const Value: Boolean);
begin
  SomeBooleanCheck.Checked := Value;
end;

procedure TDemoMainForm.SetSomeEnum(const Value: TMyEnum);
begin
  SomeEnumSelector.ItemIndex := Value.AsIndex;
end;

procedure TDemoMainForm.SetSomeIndex(const Value: Integer);
begin
  SomeIndexSelector.ItemIndex := Value;
end;

procedure TDemoMainForm.SetSomeText(const Value: string);
begin
  SomeTextEdit.Text := Value;
end;

procedure TDemoMainForm.LoadSettings;
begin
  LoadSettingsDialog.FileName := SettingsFileName;
  if LoadSettingsDialog.Execute then
    LoadFromStorage(LoadSettingsDialog.FileName);
end;

procedure TDemoMainForm.MainDataTreeDblClick(Sender: TObject);
begin
  inherited;
  if MainDataTree.Selected <> nil then
    MainDataTree.Selected.EditText;
end;

procedure TDemoMainForm.MainDataTreeEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  inherited;
  case Node.Level of
    1: case Node.Index of
         0: MainData.SomeInteger := S.ToInteger;
         1: MainData.SomeString := S;
       end;
    2: case Node.Index of
         0: MainData.SubData.SomeInteger := S.ToInteger;
         1: MainData.SubData.SomeString := S;
       end;
  end;
end;

procedure TDemoMainForm.MainDataTreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
  inherited;
  AllowEdit := not Node.HasChildren;
end;

procedure TDemoMainForm.MainDataTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ((Key = vkF2) or (Key=vkReturn)) and (Shift = []) then begin
    if MainDataTree.Selected <> nil then
      MainDataTree.Selected.EditText;
    Key := 0;
  end;
end;

procedure TDemoMainForm.RestoreDefaults;
begin
  InitDefaults;
end;

procedure TDemoMainForm.RestoreLayout;
begin
  TDataStorage.InitDefaults<LayoutDefaultAttribute>(Self);
end;

procedure TDemoMainForm.RestoreLayoutButtonClick(Sender: TObject);
begin
  RestoreLayout;
end;

procedure TDemoMainForm.SaveLayout(const AFileName: string);
begin
  var storage := TDataStorage.Create(AFileName);
  try
    storage.SaveToStorage<LayoutAttribute>(Self);
  finally
    storage.Free;
  end;
end;

procedure TDemoMainForm.SaveLayout;
begin
  SaveSettingsDialog.FileName := LayoutFileName;
  if SaveSettingsDialog.Execute then begin
    LayoutFileName := SaveSettingsDialog.FileName;
    SaveLayout(LayoutFileName);
  end;
end;

procedure TDemoMainForm.SaveLayoutButtonClick(Sender: TObject);
begin
  SaveLayout;
end;

procedure TDemoMainForm.SaveMainData;
begin
  MainDataRec.SomeInteger := SomeIntegerEdit.ValueInt;
  MainDataRec.SomeString := SomeStringEdit.Text;
end;

procedure TDemoMainForm.SaveSettings;
begin
  SaveSettingsDialog.FileName := SettingsFileName;
  if SaveSettingsDialog.Execute then
    SaveToStorage(SaveSettingsDialog.FileName);
end;

procedure TDemoMainForm.SaveToStorage(Storage: TDataStorage);
begin
  inherited;
  SaveMainData;
  Storage.SaveToStorage(MainData);
  Storage.SaveToStorage<TMainDataRec>(MainDataRec);
end;

procedure TDemoMainForm.SetLayoutRect(const Value: TRect);
begin
  BoundsRect := Value;
end;

procedure TDemoMainForm.SetSplitterFrames(const Value: Integer);
begin
  DemoFrame1.Width := ScaledValue(Value);
end;

procedure TDemoMainForm.SetSplitterLeft(const Value: Integer);
begin
  pnlLeft.Width := ScaledValue(Value);
end;

procedure TDemoMainForm.SetSplitterMainData(const Value: Integer);
begin
  pnlFrames.Height := ScaledValue(Value);
end;

function TDemoMainForm.UnscaledValue(Value: Integer): Integer;
begin
  Result := MulDiv(Value, Screen.DefaultPixelsPerInch, GetCurrentPPI);
end;

function TDemoMainForm.ScaledValue(Value: Integer): Integer;
begin
  Result := MulDiv(Value, GetCurrentPPI, Screen.DefaultPixelsPerInch);
end;

procedure TDemoMainForm.UpdateTitle;
begin
  TitleLabel.Caption := Name;
end;

end.
