

procedure TMainForm.FormCreate(Sender: TObject);
begin
FESkeleton:=INVALID_HANDLE_VALUE;
FEDepth:=INVALID_HANDLE_VALUE;
FSDepth:=INVALID_HANDLE_VALUE;
LoadNuiLibrary;
TBAngle.Min:=NUI_CAMERA_ELEVATION_MINIMUM;
TBAngle.Max:=NUI_CAMERA_ELEVATION_MAXIMUM;
if not InitKinect then
ShowMessage(’Could not initialize kinect!’);
For I:=1 to 6 do
FPlayerColors[i]:=clWhite;
end;


function TMainForm.InitKinect (EnableNear : Boolean = False): boolean;
var
w,h : DWord;
C,i : integer;
NS : INuiSensor;
E : Int64;
begin
Result:=false;
9
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
begin
FKinect:=Nil;
Exit;
end;

FESkeleton:=CreateEvent(nil,True,False,nil);
FKinect.NuiSkeletonTrackingEnable(FESkeleton,SkeletonOptions);

FEDepth:=CreateEvent(nil,true,false,nil);
if Failed(FKinect.NuiImageStreamOpen(ImageOptions,
ImageResolution,0,2,FEDepth, FSDepth)) then
Exit;
if EnableNear then
if Failed(FKinect.NuiImageStreamSetImageFrameFlags(FSDepth,ImageStreamOptions)) then Exit;



FTEvents:=TEventDispatcherThread.CreateDispatcher(Handle,
FESkeleton,
FEDEpth);

NuiImageResolutionToSize(ImageResolution,w, h);
ClientWidth:=w;
ClientHeight:=h;
FBDepth:=TBitmap.Create;
FBDepth.Width:=w;
FBDepth.Height:=h;
Result:=true;
if Not Failed (FKinect.NuiCameraElevationGetAngle(@A)) then
TBAngle.Position:=A;
end;

procedure TEventDispatcherThread.Execute;
begin
if (FHWnd=INVALID_HANDLE_VALUE) or
(FESkeleton=INVALID_HANDLE_VALUE) then
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


procedure TMainForm.eventDispatcher(var msg: TMessage);
begin
if (msg.WParam=msgSkeleton) then
OnNewSkeletonFrame
else if (msg.WParam=msgDepth) then

OnNewDepthFrame;
DoTick(msg.WParam=msgDepth);
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
//This code fetches the next NUI_SKELETON_FRAME structure from the kinect, and initializes
//a pointer to the first skeleton (PSD). The following step is ’smoothing out’ the received
//coordinates:
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
//And finally, the 3 shapes are positioned:
ShowJoint(SHead,PSD,NUI_SKELETON_POSITION_HEAD);
ShowJoint(SLeft,PSD,NUI_SKELETON_POSITION_HAND_LEFT);
ShowJoint(SRight,PSD,NUI_SKELETON_POSITION_HAND_RIGHT);
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
S.Left:=Round(x)-(S.Width div 2);
S.Top:=Round(y)-(S.Height div 2);
end;
end;



procedure TMainForm.OnNewDepthFrame;
  Const DS = NUI_IMAGE_PLAYER_INDEX_SHIFT;
  MINS = NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE shr DS;
  MAXS = NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE shr DS;
var
  IFDepth : NUI_IMAGE_FRAME;
  FT : INuiFrameTexture;
  lck : NUI_LOCKED_RECT;
  depth : pword;
  CD : Word;
  p : byte;
  x, y : integer;
  w, h, GS : cardinal;
  C : TCanvas;
begin
  if (FSDepth=INVALID_HANDLE_VALUE) then Exit;
  if Failed(FKinect.NuiImageStreamGetNextFrame(FSDepth,0,@IFDepth)) then Exit;
  NuiImageResolutionToSize(IFDepth.eResolution, w, h);
  //The above code retrieves the image from the kinect and calculates a width and heigh with it.
  //The next step is to retrieve the image data from the INuiFrameTexture interface:
  try
    FT:=IFDepth.pFrameTexture;
    if not assigned(FT) then Exit;
    if Failed(FT.LockRect(0,@lck,nil,0)) then Exit;
    try
      if lck.Pitch<>(2*w) then Exit;
      depth:=lck.pBits;
      //The following steps transfer are a loop over the depth data, 
      //transferring it as a grayscale to the bitmap. 
      //The depths that have a player index in them are transferred 
      //to the bitmap using the player’s color. 
      //if there is no player index, the depth value is 
      //transformed to a grayscale value ranging from 0 to 255.
      // Note that the bitmap canvas is locked for better performance:
      C:=FBDepth.Canvas;
      C.Lock;
      for y:=0 to h-1 do
        for x:=0 to w-1 do begin
          CD:=Depth^;
          P:=(CD and NUI_IMAGE_PLAYER_INDEX_MASK);
          if (P<>0) then 
            C.Pixels[X,Y]:=FPlayerColors[p]
          else 
            if (CD>=NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE) and
              (CD<=NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE) then begin
              GS:=Round(((CD shr ds) - MINS) / MAXS * 255);
              GS:=GS and $FF;
              GS:=GS or (GS shl 8) or (GS shl 16);
              C.Pixels[X,Y]:=GS;
            end
            else
              C.Pixels[X,Y]:=clBLack;
          Inc(depth);
        end;
      C.Unlock;
     //Lastly, the retrieved depth image data is released, and the bitmap is drawn on the panel:
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



procedure TMainForm.TBAngleChange(Sender: TObject);
Var
A : Longint;
begin
If TBAngle.Position=FLastPosition then
Exit;
if Not Failed (FKinect.NuiCameraElevationGetAngle(@A)) then
begin
if (A<>-TBAngle.Position) then
A:=-TBAngle.Position;
begin
if Failed (FKinect.NuiCameraElevationSetAngle(A)) then
ShowMessage(Format(SErrSetAngle,[A]));
FLastPosition:=-A;
end;
end
else
ShowMessage(SErrGetAngle);
end;




