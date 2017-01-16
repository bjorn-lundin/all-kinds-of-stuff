unit frmmain;
{$mode delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, libkinect10, EventDispatcherThread, StdCtrls, ExtCtrls, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CBPlayer1: TColorButton;
    CBPlayer2: TColorButton;
    CBPlayer3: TColorButton;
    CBPlayer4: TColorButton;
    CBPlayer5: TColorButton;
    CBPlayer6: TColorButton;
    lbl3: TLabel;
    lbl2: TLabel;
    lbl1: TLabel;
    lD2: TLabel;
    lD1: TLabel;
    LPlayer1: TLabel;
    LPlayer2: TLabel;
    LElevation: TLabel;
    LImageFrames: TLabel;
    LPlayer3: TLabel;
    LPlayer4: TLabel;
    LPlayer5: TLabel;
    LPlayer6: TLabel;
    LSkeletonFrames: TLabel;
    LLSkeletonFrames: TLabel;
    LLImageFrames: TLabel;
    PControls: TPanel;
    PIMage: TPanel;
    SHead: TShape;
    SLeft: TShape;
    SRight: TShape;
    TBAngle: TTrackBar;
    procedure CBPlayer1ColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TBAngleChange(Sender: TObject);
  private
    FTEvents : TEventDispatcherThread;
    FKinect : INuiSensor;
    FESkeleton,
    FEDepth,
    FSDepth : Handle;
    FLastTick : TDateTime;
    FFLastFrameCountS,
    FFrameCountS : Integer;
    FFLastFrameCountD,
    FFrameCountD : Integer;
    FBDepth : TBitmap;
    FPlayerColors : Array[0..6] of TColor;
    FLastPosition : Integer;
    Procedure DoTick(IsDepth : Boolean);
    Function InitKinect(EnableNear : Boolean = False) : boolean;
    procedure eventDispatcher(var msg : TMessage); message WM_USER;
    procedure OnNewSkeletonFrame;
    procedure OnNewDepthFrame;
    procedure ShowJoint(S: TShape; PSD: PNUI_SKELETON_DATA; HI: Integer);
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses dateutils;

procedure TMainForm.eventDispatcher(var msg: TMessage);
begin
  if (msg.WParam=msgSkeleton) then
    OnNewSkeletonFrame
  else if (msg.WParam=msgDepth) then
    OnNewDepthFrame;
  DoTick(msg.WParam=msgDepth);
end;

procedure TMainForm.FormCreate(Sender: TObject);

Var
  I : Integer;

begin
  FESkeleton:=INVALID_HANDLE_VALUE;
  FEDepth:=INVALID_HANDLE_VALUE;
  FSDepth:=INVALID_HANDLE_VALUE;
  LoadNuiLibrary;
  TBAngle.Max:=NUI_CAMERA_ELEVATION_MAXIMUM;
  TBAngle.Min:=NUI_CAMERA_ELEVATION_MINIMUM;
  if not InitKinect then
    ShowMessage('Could not initialize kinect!');
  For I:=1 to 6 do
    FPlayerColors[i]:=clWhite;

end;

procedure TMainForm.CBPlayer1ColorChanged(Sender: TObject);
begin
  With (Sender as TColorButton) do
    FPlayerColors[Tag]:=ButtonColor;
end;

procedure TMainForm.FormDestroy(Sender: TObject);

  Procedure CH(Var H : Handle);

  begin
    if (H<>INVALID_HANDLE_VALUE) then
      begin
      CloseHandle(H);
      H:=INVALID_HANDLE_VALUE
      end;
  end;

begin
  if assigned(FKinect) then
    begin
    FKinect.NuiSkeletonTrackingDisable;
    FKinect.NuiShutdown;
    end;
  CH(FESkeleton);
  CH(FEDepth);
  if assigned(FTEvents) then
    FTEvents.Terminate;
end;

procedure TMainForm.TBAngleChange(Sender: TObject);

Const
  SErrSetAngle = 'Failed to set camera elevation angle to %d';
  SErrGetAngle = 'Failed to get camera elevation angle.';

Var
  A : Longint;

begin
  If TBAngle.Position=FLastPosition then
    Exit;
  if Not Failed (FKinect.NuiCameraElevationGetAngle(@A)) then
    begin
    if (A<>-TBAngle.Position) then
      begin
      A:=-TBAngle.Position;
      if Failed (FKinect.NuiCameraElevationSetAngle(A)) then
        ShowMessage(Format(SErrSetAngle,[A]));
      FLastPosition:=-A;
      end;
    end
  else
    ShowMessage(SErrGetAngle);
end;

procedure TMainForm.DoTick(IsDepth: Boolean);

Var
  FPS,FPD : Integer;
  D : TDateTime;
begin
  If IsDepth then
    Inc(FFrameCountD)
  else
    Inc(FFrameCountS);
  D:=Now;
  If MilliSecondsBetween(D,FLastTick)>1000 then
    begin
    FPD:=FFrameCountD-FFlastFrameCountD;
    FPS:=FFrameCountS-FFlastFrameCountS;
    LSKeletonFrames.Caption:=Format('%d (%d/sec)',[FFrameCountS,FPS]);
    LImageFrames.Caption:=Format('%d (%d/sec)',[FFrameCountD,FPD]);
    FFlastFrameCountD:=FFrameCountD;
    FFlastFrameCountS:=FFrameCountS;
    FlastTick:=D;
    end;
end;

procedure TMainForm.OnNewDepthFrame;

var
  IFDepth : NUI_IMAGE_FRAME;
  FT      : INuiFrameTexture;
  lck     : NUI_LOCKED_RECT;

Const
  DS   = NUI_IMAGE_PLAYER_INDEX_SHIFT;
  MINS = NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE shr DS;
  MAXS = NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE shr DS;
Var
  depth  : pword;
  CD     : Word;
  p      : byte;
  x, y   : integer;
  w, h   : cardinal;
  GS     : Cardinal;
  C      : TCanvas;
  GS2     : Cardinal;

begin

  if (FSDepth=INVALID_HANDLE_VALUE) then
    Exit;
  if Failed(FKinect.NuiImageStreamGetNextFrame(FSDepth,0,@IFDepth)) then
    Exit;
  NuiImageResolutionToSize(IFDepth.eResolution, w, h);
  try
    FT:=IFDepth.pFrameTexture;
    if not assigned(FT) then
      Exit;
    if Failed(FT.LockRect(0,@lck,nil,0)) then
      Exit;
    try
      if lck.Pitch<>(2*w) then
        Exit;
      depth:=lck.pBits;
      C:=FBDepth.Canvas;
      C.Lock;
      for y:=0 to h-1 do
        for x:=0 to w-1 do
          begin
          CD:=Depth^;
          P:=(CD and NUI_IMAGE_PLAYER_INDEX_MASK);
          if (P<>0) then
            begin
              C.Pixels[X,Y]:=FPlayerColors[p];
              lbl1.Caption:= IntToStr(p);
              lbl2.Caption:= IntToStr(Cd);
              GS2:=Round(((CD shr ds) - MINS) / MAXS * 255);
              GS2:=GS2 and $FF;
              GS2:=GS2  or (GS2 shl 8) or (GS2 shl 16);
              lbl3.Caption :=IntToStr(GS2);
              C.Pixels[X,Y]:=GS2;
            end
          else if (CD>=NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE) and
                  (CD<=NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE) then
              begin
              GS:=Round(((CD shr ds) - MINS) / MAXS * 255);
              GS:=GS and $FF;
              GS:=GS  or (GS shl 8) or (GS shl 16);
              //C.Pixels[X,Y]:=GS;
              C.Pixels[X,Y]:=clWhite;
              end
          else
            C.Pixels[X,Y]:=clBLack;
          Inc(depth);
          end;
      C.Unlock;
    finally
      FT.UnlockRect(0);
    end;
  finally
    FKinect.NuiImageStreamReleaseFrame(FSDepth, @IFDepth);
  end;
  PImage.Canvas.Draw(0,0,FBDepth);
  For X:=0 to PImage.ControlCount-1 do
    if PImage.Controls[x] is TShape then
      PImage.Controls[x].Repaint;
end;


Procedure TMainForm.ShowJoint(S : TShape; PSD : PNUI_SKELETON_DATA; HI : Integer);

Var
  x, y : single;

begin
  S.Visible:=PSD^.eSkeletonPositionTrackingState[HI]=
              NUI_SKELETON_POSITION_TRACKED;
  if S.Visible then
    begin
    NuiTransformSkeletonToDepthImage(PSD^.SkeletonPositions[HI],
                                     x,y,
                                     NUI_IMAGE_RESOLUTION_640x480);
    S.Left:=Round(x)-S.Width shr 1;
    S.Top:=Round(y)-S.Height shr 1;
    end;
end;

procedure TMainForm.OnNewSkeletonFrame;

var
  i : integer;
  fr : NUI_SKELETON_FRAME;
  PSD : PNUI_SKELETON_DATA;
  tsp : NUI_TRANSFORM_SMOOTH_PARAMETERS;


begin
  FillChar(fr,sizeof(NUI_SKELETON_FRAME),0);
  if Failed(FKinect.NuiSkeletonGetNextFrame(0,@fr)) then
    Exit;
  PSD:=Nil;
  I:=0;
  While (PSD=Nil) and (i<NUI_SKELETON_COUNT) do
    begin
    if (fr.SkeletonData[i].eTrackingState=NUI_SKELETON_TRACKED) then
      PSD:=@fr.SkeletonData[i];
    Inc(I);
    end;
  if Not Assigned(PSD) then
    Exit;
  With tsp do
    begin
    fCorrection:=0.3;
    fJitterRadius:=1.0;
    fMaxDeviationRadius:=0.5;
    fPrediction:=0.4;
    fSmoothing:=0.7;
    end;
  if Failed(FKinect.NuiTransformSmooth(@fr,@tsp)) then
    Exit;
  ShowJoint(SHead,PSD,NUI_SKELETON_POSITION_HEAD);
  ShowJoint(SLeft,PSD,NUI_SKELETON_POSITION_HAND_LEFT);
  ShowJoint(SRight,PSD,NUI_SKELETON_POSITION_HAND_RIGHT);
end;

function TMainForm.InitKinect (EnableNear : Boolean = False): boolean;

Const
  NUIOptions =
    NUI_INITIALIZE_FLAG_USES_SKELETON or
    NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX;
  SkeletonOptions =
    NUI_SKELETON_TRACKING_FLAG_ENABLE_SEATED_SUPPORT or
    NUI_SKELETON_TRACKING_FLAG_ENABLE_IN_NEAR_RANGE;
  ImageOptions =
    NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX;
  ImageResolution =
    NUI_IMAGE_RESOLUTION_640x480;
  ImageStreamOptions =
    NUI_IMAGE_STREAM_FLAG_ENABLE_NEAR_MODE;

var
  w,h : DWord;
  C,i : integer;
  A : Longint;
  NS : INuiSensor;
  E : Int64;

begin
  Result:=false;
  FKinect := nil;
  if Failed(NuiGetSensorCount(C)) then
    exit;
  I:=0;
  While (FKinect=Nil) and (i<C) do
    begin
    if Not Failed(NuiCreateSensorByIndex(i,NS)) then
      if (NS.NuiStatus=S_OK) then
         FKinect:=NS;
    Inc(I);
    end;
  if not Assigned(FKinect) then
    exit;
  if Failed(FKinect.NuiInitialize(NUIOptions)) then
    FKinect:=Nil;
  // Skeleton event
  FESkeleton:=CreateEvent(nil,True,False,nil);
  FKinect.NuiSkeletonTrackingEnable(FESkeleton,SkeletonOptions);
  // Depth event
  FEDepth:=CreateEvent(nil,true,false,nil);
  if Failed(FKinect.NuiImageStreamOpen(ImageOptions,
    ImageResolution,0,2,FEDepth, FSDepth)) then
      Exit;
  if EnableNear then
    if Failed(FKinect.NuiImageStreamSetImageFrameFlags(FSDepth,ImageStreamOptions)) then
      Exit;
  // Set up event dispatcher thread
  FTEvents:=TEventDispatcherThread.CreateDispatcher(Handle,
                                                    FESkeleton,
                                                    FEDEpth);
  // Set up bitmap for depth image.
  NuiImageResolutionToSize(ImageResolution,w, h);
  FBDepth:=TBitmap.Create;
  FBDepth.Width:=w;
  FBDepth.Height:=h;
  ClientWidth:=PControls.Width+W;
  ClientHeight:=h;
  Result:=true;
  // Show current elevation angle
  if Not Failed (FKinect.NuiCameraElevationGetAngle(@A)) then
    TBAngle.Position:=A;
end;

end.
