unit Cmon.DataSetHelper.Design;

interface

uses
  DesignIntf, DesignEditors;

resourcestring
  SCreateAccessRecord = 'Create mapped record';
  SCreateAccessClass = 'Create mapped class';
  SCreateTRecordFieldsClass = 'Create TRecordFields class';
  STypesCopiedToClipboard = 'Types copied to clipboard: ';

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
  Cmon.DataSetHelper.Generator;

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
      var wasActive := dataSet.Active;
      try
        dataSet.Active := True;
        case Index of
          0: instance.CreateAccessRecord(dataSet);
          1: instance.CreateAccessClass(dataSet);
          2: instance.CreateRecordFields(dataSet);
        end;
      finally
        dataSet.Active := wasActive;
      end;
    end;
    instance.CopyToClipboard;
    var msg := STypesCopiedToClipboard + instance.TypesCreated.CommaText;
    ShowMessage(msg);
  finally
    instance.Free;
  end;
end;

function TDataSetHelperEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SCreateAccessRecord;
    1: Result := SCreateAccessClass;
    2: Result := SCreateTRecordFieldsClass;
  else
    Result := '';
  end;
end;

function TDataSetHelperEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

end.
