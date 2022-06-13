unit Main.Form;

interface

uses
  System.Classes, System.IniFiles,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.Dialogs, Vcl.ComCtrls,
  Cmon.DataStorage,
  Common.Frame, Common.Form,
  Main.Frame;

type
{$SCOPEDENUMS ON}
  TMyEnum = (none, enum1, enum2, enum3, enum4);
{$SCOPEDENUMS OFF}

type
  DefaultMyEnumAttribute = class(DefaultAttribute)
  public
    constructor Create(AValue: TMyEnum); overload;
  end;

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
    procedure FormCreate(Sender: TObject);
    procedure LoadSettingsButtonClick(Sender: TObject);
    procedure MainDataTreeDblClick(Sender: TObject);
    procedure MainDataTreeEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure MainDataTreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure MainDataTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RestoreDefaultsButtonClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
  strict private
  class var
    FSettingsFileExtension: string;
    FSettingsFileName: string;
  private
    class function GetSettingsFileName: string; static;
    function GetSomeBoolean: Boolean;
    function GetSomeEnum: TMyEnum;
    function GetSomeIndex: Integer;
    function GetSomeText: string;
    procedure LoadMainData;
    procedure PrepareFileDialog(ADialog: TCustomFileDialog);
    procedure SetSomeBoolean(const Value: Boolean);
    procedure SetSomeEnum(const Value: TMyEnum);
    procedure SetSomeIndex(const Value: Integer);
    procedure SetSomeText(const Value: string);
  protected
    procedure LoadSettings;
    procedure RestoreDefaults;
    procedure SaveSettings;
  public
    procedure InitDefaults(Storage: TDataStorage); overload; override;
    procedure LoadFromStorage(Storage: TDataStorage); overload; override;
    procedure SaveToStorage(Storage: TDataStorage); overload; override;
    procedure UpdateTitle;
    class property SettingsFileExtension: string read FSettingsFileExtension write FSettingsFileExtension;
    class property SettingsFileName: string read GetSettingsFileName write FSettingsFileName;
    [Stored, Default(True)]
    property SomeBoolean: Boolean read GetSomeBoolean write SetSomeBoolean;
    [Stored, DefaultMyEnum(TMyEnum.none)]
    property SomeEnum: TMyEnum read GetSomeEnum write SetSomeEnum;
    [Stored, Default(1)]
    property SomeIndex: Integer read GetSomeIndex write SetSomeIndex;
    [Stored, Default('Hello World')]
    property SomeText: string read GetSomeText write SetSomeText;
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

procedure TDemoMainForm.InitDefaults(Storage: TDataStorage);
begin
  inherited;
  Storage.InitDefaults(MainData);
  LoadMainData;
end;

procedure TDemoMainForm.LoadFromStorage(Storage: TDataStorage);
begin
  inherited;
  Storage.LoadFromStorage(MainData);
  LoadMainData;
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
end;

procedure TDemoMainForm.PrepareFileDialog(ADialog: TCustomFileDialog);
begin
  var defaultExt := SettingsFileExtension;
  var targets := TStorageTargetDescriptorList.Create;
  try
    TDataStorage.ListStorageTargets(targets);
    ADialog.FileTypes.Clear;
    for var target in targets do begin
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
  finally
    targets.Free;
  end;
  ADialog.DefaultExtension := defaultExt.Substring(1);
  ADialog.FileName := SettingsFileName;
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

procedure TDemoMainForm.SaveSettings;
begin
  if SaveSettingsDialog.Execute then
    SaveToStorage(SaveSettingsDialog.FileName);
end;

procedure TDemoMainForm.SaveToStorage(Storage: TDataStorage);
begin
  inherited;
  Storage.SaveToStorage(MainData);
end;

procedure TDemoMainForm.UpdateTitle;
begin
  TitleLabel.Caption := Name;
end;

end.
