unit Cmon.Messaging.Dialogs.Vcl;

interface

uses
  Cmon.Messaging.Dialogs;

type
  TDlgMessageHandlerVcl = class(TDlgMessageHandler)
  strict private
  class var
    FAutoRegisterHandler: Boolean;
  strict protected
    procedure HandleMessage(const Sender: TObject; const Msg: TDlgMessage); override;
  public
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
  end;

implementation

uses
  System.UITypes, System.SysUtils,
  Vcl.Dialogs,
  Cmon.Initializing, Cmon.Messaging;

procedure TDlgMessageHandlerVcl.HandleMessage(const Sender: TObject; const Msg: TDlgMessage);
begin
  if Msg.Title.IsEmpty then
    Msg.Answer := MessageDlg(Msg.MessageText, Msg.MsgDlgType, Msg.Buttons, Msg.HelpContext)
  else
    Msg.Answer := TaskMessageDlg(Msg.Title, Msg.MessageText, Msg.MsgDlgType, Msg.Buttons, Msg.HelpContext);
end;

var
  Instance: IMessageHandler;

{ will be called in Application.Initialize after all other initialization code has been executed }
procedure InitHandler;
begin
  if TDlgMessage.AutoRegisterHandler and TDlgMessageHandlerVcl.AutoRegisterHandler then
    Instance := TDlgMessageHandlerVcl.CreateInterface;
end;

initialization
  TDlgMessageHandlerVcl.AutoRegisterHandler := True;
  TInitialize.AddInitProc(InitHandler);
end.
