unit Cmon.Dialogs;

interface

uses
  System.UITypes;

const
  mbYesNo = [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo];
  mbYesNoCancel = [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel];
  mbYesAllNoAllCancel = [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbYesToAll, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbNoToAll, TMsgDlgBtn.mbCancel];
  mbOKCancel = [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel];
  mbAbortRetryIgnore = [TMsgDlgBtn.mbAbort, TMsgDlgBtn.mbRetry, TMsgDlgBtn.mbIgnore];
  mbAbortIgnore = [TMsgDlgBtn.mbAbort, TMsgDlgBtn.mbIgnore];

type
  {$SCOPEDENUMS ON}
  TYesNoCancel = (Yes, No, Cancel);
  {$SCOPEDENUMS OFF}

type
  TMessageDlg = class
  public
    class function Execute(Sender: TObject; const ATitle, AMessageText: string; AMsgDlgType: TMsgDlgType; AButtons: TMsgDlgButtons;
        ADefault: TModalResult; AHelpContext: Integer = 0): TModalResult;
    class function Confirm(Sender: TObject; const ATitle, AMessageText: string; ADefault: Boolean = True; AHelpContext: Integer
        = 0): Boolean;
    class function ConfirmCancel(Sender: TObject; const ATitle, AMessageText: string; ADefault: TYesNoCancel = TYesNoCancel.Yes;
        AHelpContext: Integer = 0): TYesNoCancel;
    class procedure Error(Sender: TObject; const ATitle, AMessageText: string; AHelpContext: Integer = 0);
    class procedure Info(Sender: TObject; const ATitle, AMessageText: string; AHelpContext: Integer = 0);
    class procedure Warning(Sender: TObject; const ATitle, AMessageText: string; AHelpContext: Integer = 0);
  end;

implementation

uses
  System.SysUtils,
  Cmon.Messaging;

class function TMessageDlg.Confirm(Sender: TObject; const ATitle, AMessageText: string; ADefault: Boolean = True; AHelpContext:
    Integer = 0): Boolean;
const
  cDefaultResult: array[Boolean] of TModalResult = (mrNo, mrYes);
var
  res: TModalResult;
begin
  res := Execute(Sender, ATitle, AMessageText, TMsgDlgType.mtConfirmation, mbYesNo, cDefaultResult[ADefault], AHelpContext);
  Result := IsPositiveResult(res);
end;

class function TMessageDlg.ConfirmCancel(Sender: TObject; const ATitle, AMessageText: string; ADefault: TYesNoCancel =
    TYesNoCancel.Yes; AHelpContext: Integer = 0): TYesNoCancel;
const
  cDefaultResult: array[TYesNoCancel] of TModalResult = (mrNo, mrYes, mrCancel);
var
  res: TModalResult;
begin
  res := Execute(Sender, ATitle, AMessageText, TMsgDlgType.mtConfirmation, mbYesNoCancel, cDefaultResult[ADefault], AHelpContext);
  if IsPositiveResult(res) then
    Result := TYesNoCancel.Yes
  else if IsNegativeResult(res) then
    Result := TYesNoCancel.No
  else if IsAbortResult(res) then
    Result := TYesNoCancel.Cancel
  else
    raise Exception.CreateFmt('unexpected ModalResult: %d', [res]);
end;

class procedure TMessageDlg.Error(Sender: TObject; const ATitle, AMessageText: string; AHelpContext: Integer = 0);
begin
  Execute(Sender, ATitle, AMessageText, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], mrOK, AHelpContext);
end;

class function TMessageDlg.Execute(Sender: TObject; const ATitle, AMessageText: string; AMsgDlgType: TMsgDlgType; AButtons:
    TMsgDlgButtons; ADefault: TModalResult; AHelpContext: Integer = 0): TModalResult;
begin
  Result := TDlgMessage.Execute(Sender, ATitle, AMessageText, AMsgDlgType, AButtons, ADefault, AHelpContext);
end;

class procedure TMessageDlg.Info(Sender: TObject; const ATitle, AMessageText: string; AHelpContext: Integer = 0);
begin
  Execute(Sender, ATitle, AMessageText, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], mrOK, AHelpContext);
end;

class procedure TMessageDlg.Warning(Sender: TObject; const ATitle, AMessageText: string; AHelpContext: Integer = 0);
begin
  Execute(Sender, ATitle, AMessageText, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], mrOK, AHelpContext);
end;

end.
