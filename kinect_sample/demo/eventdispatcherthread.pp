{$mode objfpc}
{$H+}
unit EventDispatcherThread;

interface

uses Windows, Classes;

Const
  MsgSkeleton  = 1;  // message to send when skeleton event occurs
  MsgDepth     = 2;  // Message to send when depth event occurs

type
  TEventDispatcherThread = class(TThread)
  private
    FESkeleton,     // Skeleton event to watch
    FEDepth,        // Depth event to watch
    FHWnd : Handle; // Window handle to send message (FMsg) to
  protected
    procedure Execute; override;
  public
    Constructor CreateDispatcher(HWindow, ESkeleton, EDepth : Handle);
  end;

implementation

uses Messages;

{ TEventDispatcherThread }

constructor TEventDispatcherThread.CreateDispatcher(HWindow, ESkeleton, EDepth : Handle);
begin
  FESkeleton:=ESkeleton;
  FEDepth:=EDepth;
  FHWnd:= HWindow;
  FreeOnTerminate:=True;
  inherited Create(False);
end;

procedure TEventDispatcherThread.Execute;
begin
  if (FHWnd=INVALID_HANDLE_VALUE) or (FESkeleton=INVALID_HANDLE_VALUE) then
    exit;
  While not terminated do
    begin
    if (WaitForSingleObject(FESkeleton,50)=WAIT_OBJECT_0) then
      begin
      SendMessage(FHWnd,WM_USER,MsgSkeleton,0);
      ResetEvent(FESkeleton);
      end;
    if (WaitForSingleObject(FEDepth,50)=WAIT_OBJECT_0) then
      begin
      SendMessage(FHWnd,WM_USER,MsgDepth,0);
      ResetEvent(FEDepth);
      end;
    end;
end;

end.
