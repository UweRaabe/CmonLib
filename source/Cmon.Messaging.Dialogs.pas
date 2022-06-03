unit Cmon.Messaging.Dialogs;

interface

uses
  System.Messaging;

type
  TDlgMessageHandler = class
  private
    FMessageID: Integer;
  strict protected
    procedure DlgMessage(const Sender: TObject; const M: TMessage); virtual; abstract;
    procedure Subscribe; virtual;
    procedure Unsubscribe; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Cmon.Messaging;

constructor TDlgMessageHandler.Create;
begin
  inherited;
  Subscribe;
end;

destructor TDlgMessageHandler.Destroy;
begin
  Unsubscribe;
  inherited;
end;

procedure TDlgMessageHandler.Subscribe;
begin
  FMessageID := TDlgMessage.Subscribe(DlgMessage);
end;

procedure TDlgMessageHandler.Unsubscribe;
begin
  TDlgMessage.Unsubscribe(FMessageID);
end;

end.
