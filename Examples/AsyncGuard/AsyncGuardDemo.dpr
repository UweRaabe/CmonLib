program AsyncGuardDemo;

uses
  Vcl.Forms,
  Main.Form in 'Main.Form.pas' {SearchForm},
  AsyncSearch in 'AsyncSearch.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSearchForm, SearchForm);
  Application.Run;
end.
