unit Utilities;

interface

uses
  System.Classes;

procedure LoadStrings(const FileName: string; Target: TStrings);
procedure SaveStrings(const FileName: string; Source: TStrings);

implementation

uses
  System.IOUtils, System.SysUtils,
  Cmon.Dialogs;

resourcestring
  SOverwriteFile = 'Datei "%s" bereits vorhanden. Überschreiben?';
  SFileNotFound = 'Datei "%s" nicht gefunden';

procedure LoadStrings(const FileName: string; Target: TStrings);
begin
  if TFile.Exists(FileName) then
    Target.LoadFromFile(FileName)
  else
    TMessageDlg.Error(nil, 'LoadStrings', Format(SFileNotFound, [FileName]));
end;

procedure SaveStrings(const FileName: string; Source: TStrings);
begin
  if TFile.Exists(FileName) then begin
    if not TMessageDlg.Confirm(nil, 'SaveStrings', Format(SOverwriteFile, [FileName])) then
      Exit;
  end;

  Source.SaveToFile(FileName);
end;

end.
