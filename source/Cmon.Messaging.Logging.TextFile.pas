unit Cmon.Messaging.Logging.TextFile;

interface

uses
  Cmon.Messaging.Logging;

type
  TLogMessageTextFile = class(TLogMessageHandler)
  strict private
  class var
    FAutoRegisterHandler: Boolean;
    FDefaultLogFileName: string;
  private
    FLogFileName: string;
  strict protected
    procedure HandleMessage(const Sender: TObject; const Msg: TLogMessage); override;
  public
    constructor Create; override;
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
    class property DefaultLogFileName: string read FDefaultLogFileName write FDefaultLogFileName;
    property LogFileName: string read FLogFileName write FLogFileName;
  end;

implementation

uses
  System.IOUtils, System.SysUtils,
  Cmon.Utilities, Cmon.Initializing, Cmon.Messaging;

const
  cLogExtension = '.log';

constructor TLogMessageTextFile.Create;
begin
  inherited Create;
  var fileName := TLogMessageTextFile.DefaultLogFileName;
  if fileName.IsEmpty then begin
    if not TUtilities.AppName.IsEmpty then
      fileName := TUtilities.AppName + cLogExtension
    else
      fileName := TPath.ChangeExtension(TUtilities.GetExeName, cLogExtension);
  end;
  FLogFileName := TPath.Combine(TUtilities.UserAppDataPath, fileName);
end;

procedure TLogMessageTextFile.HandleMessage(const Sender: TObject; const Msg: TLogMessage);
begin
  TFile.AppendAllText(LogFileName, Msg.MessageText);
end;

var
  Instance: IMessageHandler;

{ will be called in Application.Initialize after all other initialization code has been executed }
procedure InitHandler;
begin
  if TLogMessage.AutoRegisterHandler and TLogMessageTextFile.AutoRegisterHandler then
    Instance := TLogMessageTextFile.CreateInterface;
end;

initialization
  TLogMessageTextFile.AutoRegisterHandler := True;
  TInitialize.AddInitProc(InitHandler);
end.
