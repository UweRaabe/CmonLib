unit Cmon.Messaging.Dialogs.Vcl;

interface

uses
  System.Messaging,
  Cmon.Messaging.Dialogs;

type
  TDlgMessageHandlerVcl = class(TDlgMessageHandler)
  strict protected
    procedure DlgMessage(const Sender: TObject; const M: TMessage); override;
  end;

implementation

uses
  System.UITypes, System.SysUtils,
  Vcl.Dialogs,
  Cmon.Messaging;

procedure TDlgMessageHandlerVcl.DlgMessage(const Sender: TObject; const M: TMessage);
begin
  var msg := M as TDlgMessage;
  if msg.Title.IsEmpty then
    msg.Answer := MessageDlg(msg.MessageText, msg.MsgDlgType, msg.Buttons, msg.HelpContext)
  else
    msg.Answer := TaskMessageDlg(msg.Title, msg.MessageText, msg.MsgDlgType, msg.Buttons, msg.HelpContext);
end;

{$IFNDEF DontRegisterHandler}
var
  Instance: TDlgMessageHandlerVcl = nil;

initialization
  Instance := TDlgMessageHandlerVcl.Create;
finalization
  Instance.Free;
{$ENDIF}
end.
