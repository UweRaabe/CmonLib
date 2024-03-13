unit Cmon.DataSetHelper.Design;

interface

uses
  DesignIntf, DesignEditors;

type
  TDataSetHelperEditor = class(TSelectionEditor)
  public
    procedure ExecuteVerb(Index: Integer; const List: IDesignerSelections); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  Data.DB,
  Vcl.Dialogs,
  Cmon.DataSetHelper.Generator, Cmon.DataSetHelper.Generator.Form;

resourcestring
  SCreateMappings = 'Create mappings';
  STypesCopiedToClipboard = 'Types copied to clipboard:';

procedure Register;
begin
  RegisterSelectionEditor(TDataSet, TDataSetHelperEditor);
end;

procedure TDataSetHelperEditor.ExecuteVerb(Index: Integer; const List: IDesignerSelections);
begin
  var instance := TDataSetHelperGenerator.Create;
  try
    for var I := 0 to List.Count - 1 do begin
      var dataSet := List[I] as TDataSet;
      instance.DataSets.Add(dataSet);
    end;
    if TMappingsGeneratorForm.Execute(instance) then begin
      instance.Execute;
      instance.CopyToClipboard;
      var msg := STypesCopiedToClipboard + sLineBreak + instance.TypesCreated.Text;
      ShowMessage(msg);
    end;
  finally
    instance.Free;
  end;
end;

function TDataSetHelperEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SCreateMappings;
  else
    Result := '';
  end;
end;

function TDataSetHelperEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
