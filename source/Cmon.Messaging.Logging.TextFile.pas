unit Cmon.Messaging.Logging.TextFile;

interface

uses
  System.Messaging,
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
    procedure LogMessage(const Sender: TObject; const M: TMessage); override;
  public
    constructor Create;
    class property AutoRegisterHandler: Boolean read FAutoRegisterHandler write FAutoRegisterHandler;
    class property DefaultLogFileName: string read FDefaultLogFileName write FDefaultLogFileName;
    property LogFileName: string read FLogFileName write FLogFileName;
  end;

implementation

uses
  System.IOUtils, System.SysUtils,
  Cmon.Utilities, Cmon.Messaging, Cmon.Initializing;

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

procedure TLogMessageTextFile.LogMessage(const Sender: TObject; const M: TMessage);
begin
  var msg := M as TLogMessage;
  TFile.AppendAllText(LogFileName, msg.MessageText);
end;

var
  Instance: TLogMessageTextFile = nil;

{ will be called in Application.Initialize after all other initialization code has been executed }
procedure InitHandler;
begin
  if TLogMessage.AutoRegisterHandler and TLogMessageTextFile.AutoRegisterHandler then
    Instance := TLogMessageTextFile.Create;
end;

initialization
  TLogMessageTextFile.AutoRegisterHandler := True;
  TInitialize.AddInitProc(InitHandler);
finalization
  Instance.Free;
end.
