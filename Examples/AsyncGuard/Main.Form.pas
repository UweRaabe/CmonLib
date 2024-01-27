unit Main.Form;

interface

uses
  System.ImageList, System.Classes, System.Types, System.Diagnostics,
  Vcl.Forms, Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.WinXCtrls, Vcl.Controls, Vcl.ComCtrls,
  Cmon.AsyncGuard,
  AsyncSearch;

type
  TSearchForm = class(TForm, ISearchTarget)
    dspFiles: TListView;
    pnlTop: TPanel;
    edtSearchPattern: TSearchBox;
    lblSearchPattern: TLabel;
    lblRootFolder: TLabel;
    edtRootFolder: TButtonedEdit;
    imgCollection: TImageCollection;
    imgList: TVirtualImageList;
    btnNewSearchWindow: TButton;
    StatusBar: TStatusBar;
    procedure btnNewSearchWindowClick(Sender: TObject);
    procedure dspFilesData(Sender: TObject; Item: TListItem);
    procedure dspFilesDataFind(Sender: TObject; Find: TItemFind; const FindString: string; const FindPosition: TPoint;
        FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean; var Index: Integer);
    procedure edtRootFolderRightButtonClick(Sender: TObject);
    procedure edtSearchPatternInvokeSearch(Sender: TObject);
  private
    FFiles: TStringList;
    FSearch: ICancel;
    FStopwatch: TStopwatch;
  class var
    FormIndex: Integer;
    function GetFiles: TStringList;
    procedure SelectRootFolder;
    procedure SetSearch(const Value: ICancel);
    procedure AddFiles(const AFiles: TArray<string>);
    procedure BeginSearch;
    procedure CreateNewSearchWindow;
    procedure EndSearch;
    procedure StartSearch;
  protected
    property Files: TStringList read GetFiles;
    property Search: ICancel read FSearch write SetSearch;
  public
    destructor Destroy; override;
  end;

var
  SearchForm: TSearchForm;

implementation

uses
  System.Threading, System.IOUtils, System.SysUtils,
  Vcl.FileCtrl;

{$R *.dfm}

destructor TSearchForm.Destroy;
begin
  Search := nil;
  FFiles.Free;
  inherited;
end;

procedure TSearchForm.AddFiles(const AFiles: TArray<string>);
begin
  Files.AddStrings(AFiles);
  dspFiles.Items.Count := Files.Count;
  StatusBar.SimpleText := Format('%d files found', [Files.Count]);
  if Files.Count >= 10000 then begin
    EndSearch;  // cancels search and releases interface
  end;
end;

procedure TSearchForm.BeginSearch;
begin
  FStopWatch := TStopwatch.StartNew;
  dspFiles.Cursor := crHourGlass;
end;

procedure TSearchForm.btnNewSearchWindowClick(Sender: TObject);
begin
  CreateNewSearchWindow;
end;

procedure TSearchForm.CreateNewSearchWindow;
var
  form: TSearchForm;
begin
  Inc(FormIndex);
  form := TSearchForm.Create(Application);
  form.Caption := Format('Search Window %d', [FormIndex]);
  form.Show;
end;

procedure TSearchForm.StartSearch;
begin
  { cancel and release any active search }
  Search := nil;
  StatusBar.SimpleText := '';
  dspFiles.Clear;
  Files.Clear;
  TAsyncSearch.Execute(Self, edtRootFolder.Text, edtSearchPattern.Text, FSearch);
end;

procedure TSearchForm.dspFilesData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := Files[Item.Index];
end;

procedure TSearchForm.dspFilesDataFind(Sender: TObject; Find: TItemFind; const FindString: string; const FindPosition: TPoint;
    FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean; var Index: Integer);
begin
  Index := -1;
  case Find of
    ifData: ;
    ifPartialString: ;
    ifExactString: Index := Files.IndexOf(FindString);
    ifNearest: ;
  end;
end;

procedure TSearchForm.edtRootFolderRightButtonClick(Sender: TObject);
begin
  SelectRootFolder;
end;

procedure TSearchForm.edtSearchPatternInvokeSearch(Sender: TObject);
begin
  StartSearch;
end;

procedure TSearchForm.EndSearch;
begin
  Search := nil; // cancel and release interface when search is complete or interrupted
  StatusBar.SimpleText := Format('%d files found (%d ms)', [Files.Count, FStopwatch.ElapsedMilliseconds]);
  dspFiles.Cursor := crDefault;
end;

function TSearchForm.GetFiles: TStringList;
begin
  if FFiles = nil then begin
    FFiles := TStringList.Create;
  end;
  Result := FFiles;
end;

procedure TSearchForm.SelectRootFolder;
var
  path: string;
begin
  path := edtRootFolder.Text;
  if SelectDirectory('Select Root Folder', '', path) then
    edtRootFolder.Text := path;
end;

procedure TSearchForm.SetSearch(const Value: ICancel);
begin
  if FSearch <> nil then
    FSearch.Cancel;
  FSearch := Value;
end;

end.
