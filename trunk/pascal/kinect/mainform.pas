unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,libkinect10 ;

type

  { TMainForm }

  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    TBAngle: TScrollBar;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

//var
//  MainForm: TMainForm;

const
NUIOptions  = NUI_INITIALIZE_FLAG_USES_SKELETON or
              NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX;
implementation

{$R *.lfm}

{ TMainForm }


Procedure TMainForm.FormCreate(Sender: TObject);
Begin
  FESkeleton := INVALID_HANDLE_VALUE;
  FEDepth := INVALID_HANDLE_VALUE;
  FSDepth := INVALID_HANDLE_VALUE;
  LoadNuiLibrary;
  TBAngle.Min := NUI_CAMERA_ELEVATION_MINIMUM;
  TBAngle.Max := NUI_CAMERA_ELEVATION_MAXIMUM;
  If Not InitKinect Then
    ShowMessage(’Could Not initialize kinect!’);
  For I:=1 To 6 Do
    FPlayerColors[i] := clWhite;
End;


Function TMainForm.InitKinect (EnableNear : Boolean = False): boolean;

Var
  w,h : DWord;
  C,i : integer;
  NS : INuiSensor;
  E : Int64;
Begin
  Result := false;
  9
  FKinect := Nil;
  If Failed(NuiGetSensorCount(C)) Then
    exit;
  I := 0;
  While (FKinect=Nil) And (i<C) Do
    Begin
      If Not Failed(NuiCreateSensorByIndex(i,NS)) Then
        If (NS.NuiStatus=S_OK) Then
          FKinect := NS;
      Inc(I);
    End;
  If Not Assigned(FKinect) Then
    exit;
  If Failed(FKinect.NuiInitialize(NUIOptions)) Then
    Begin
      FKinect := Nil;
      Exit;
    End;

  FESkeleton := CreateEvent(Nil,True,False,Nil);
  FKinect.NuiSkeletonTrackingEnable(FESkeleton,SkeletonOptions);

  FEDepth := CreateEvent(Nil,true,false,Nil);
  If Failed(FKinect.NuiImageStreamOpen(ImageOptions,
     ImageResolution,0,2,FEDepth, FSDepth)) Then
    Exit;
  If EnableNear Then
    If Failed(FKinect.NuiImageStreamSetImageFrameFlags(FSDepth,ImageStreamOptions)) Then Exit;

  FTEvents := TEventDispatcherThread.CreateDispatcher(Handle,
              FESkeleton,
              FEDEpth);

  NuiImageResolutionToSize(ImageResolution,w, h);
  ClientWidth := w;
  ClientHeight := h;
  FBDepth := TBitmap.Create;
  FBDepth.Width := w;
  FBDepth.Height := h;
  Result := true;
  If Not Failed (FKinect.NuiCameraElevationGetAngle(@A)) Then
    TBAngle.Position := A;
End;

Procedure TEventDispatcherThread.Execute;
Begin
  If (FHWnd=INVALID_HANDLE_VALUE) Or
     (FESkeleton=INVALID_HANDLE_VALUE) Then
    exit;
  While Not terminated Do
    Begin
      If (WaitForSingleObject(FESkeleton,50)=WAIT_OBJECT_0) Then
        Begin
          SendMessage(FHWnd,WM_USER,MsgSkeleton,0);
          ResetEvent(FESkeleton);
        End;
      If (WaitForSingleObject(FEDepth,50)=WAIT_OBJECT_0) Then
        Begin
          SendMessage(FHWnd,WM_USER,MsgDepth,0);
          ResetEvent(FEDepth);
        End;
    End;
End;


Procedure TMainForm.eventDispatcher(Var msg: TMessage);
Begin
  If (msg.WParam=msgSkeleton) Then
    OnNewSkeletonFrame
  Else If (msg.WParam=msgDepth) Then

         OnNewDepthFrame;
  DoTick(msg.WParam=msgDepth);
End;




Procedure TMainForm.OnNewSkeletonFrame;

Var
  i : integer;
  fr : NUI_SKELETON_FRAME;
  PSD : PNUI_SKELETON_DATA;
  tsp : NUI_TRANSFORM_SMOOTH_PARAMETERS;
Begin
  FillChar(fr,sizeof(NUI_SKELETON_FRAME),0);
  If Failed(FKinect.NuiSkeletonGetNextFrame(0,@fr)) Then
    Exit;
  PSD := Nil;
  I := 0;
  While (PSD=Nil) And (i<NUI_SKELETON_COUNT) Do
    Begin
      If (fr.SkeletonData[i].eTrackingState=NUI_SKELETON_TRACKED) Then
        PSD := @fr.SkeletonData[i];
      Inc(I);
    End;
  If Not Assigned(PSD) Then
    Exit;
  //This code fetches the next NUI_SKELETON_FRAME structure from the kinect, and initializes
  //a pointer to the first skeleton (PSD). The following step is ’smoothing out’ the received
  //coordinates:
  With tsp Do
    Begin
      fCorrection := 0.3;
      fJitterRadius := 1.0;
      fMaxDeviationRadius := 0.5;
      fPrediction := 0.4;
      fSmoothing := 0.7;
    End;
  If Failed(FKinect.NuiTransformSmooth(@fr,@tsp)) Then
    Exit;
  //And finally, the 3 shapes are positioned:
  ShowJoint(SHead,PSD,NUI_SKELETON_POSITION_HEAD);
  ShowJoint(SLeft,PSD,NUI_SKELETON_POSITION_HAND_LEFT);
  ShowJoint(SRight,PSD,NUI_SKELETON_POSITION_HAND_RIGHT);
End;


Procedure TMainForm.ShowJoint(S : TShape; PSD : PNUI_SKELETON_DATA; HI : Integer);

Var
  x, y : single;
Begin
  S.Visible := PSD^.eSkeletonPositionTrackingState[HI]=
               NUI_SKELETON_POSITION_TRACKED;
  If S.Visible Then
    Begin
      NuiTransformSkeletonToDepthImage(PSD^.SkeletonPositions[HI],
                                       x,y,
                                       NUI_IMAGE_RESOLUTION_640x480);
      S.Left := Round(x)-(S.Width Div 2);
      S.Top := Round(y)-(S.Height Div 2);
    End;
End;



Procedure TMainForm.OnNewDepthFrame;

Const DS = NUI_IMAGE_PLAYER_INDEX_SHIFT;
  MINS = NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE shr DS;
  MAXS = NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE shr DS;

Var
  IFDepth : NUI_IMAGE_FRAME;
  FT : INuiFrameTexture;
  lck : NUI_LOCKED_RECT;
  depth : pword;
  CD : Word;
  p : byte;
  x, y : integer;
  w, h, GS : cardinal;
  C : TCanvas;
Begin
  If (FSDepth=INVALID_HANDLE_VALUE) Then Exit;
  If Failed(FKinect.NuiImageStreamGetNextFrame(FSDepth,0,@IFDepth)) Then Exit;
  NuiImageResolutionToSize(IFDepth.eResolution, w, h);
  //The above code retrieves the image from the kinect and calculates a width and heigh with it.
  //The next step is to retrieve the image data from the INuiFrameTexture interface:
  Try
    FT := IFDepth.pFrameTexture;
    If Not assigned(FT) Then Exit;
    If Failed(FT.LockRect(0,@lck,Nil,0)) Then Exit;
    Try
      If lck.Pitch<>(2*w) Then Exit;
      depth := lck.pBits;
      //The following steps transfer are a loop over the depth data,
      //transferring it as a grayscale to the bitmap.
      //The depths that have a player index in them are transferred
      //to the bitmap using the player’s color.
      //if there is no player index, the depth value is
      //transformed to a grayscale value ranging from 0 to 255.
      // Note that the bitmap canvas is locked for better performance:
      C := FBDepth.Canvas;
      C.Lock;
      For y:=0 To h-1 Do
        For x:=0 To w-1 Do
          Begin
            CD := Depth^;
            P := (CD And NUI_IMAGE_PLAYER_INDEX_MASK);
            If (P<>0) Then
              C.Pixels[X,Y] := FPlayerColors[p]
            Else
              If (CD>=NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE) And
                 (CD<=NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE) Then
                Begin
                  GS := Round(((CD shr ds) - MINS) / MAXS * 255);
                  GS := GS And $FF;
                  GS := GS Or (GS shl 8) Or (GS shl 16);
                  C.Pixels[X,Y] := GS;
                End
            Else
              C.Pixels[X,Y] := clBLack;
            Inc(depth);
          End;
      C.Unlock;
      //Lastly, the retrieved depth image data is released, and the bitmap is drawn on the panel:
    Finally
      FT.UnlockRect(0);
    End;
  Finally
    FKinect.NuiImageStreamReleaseFrame(FSDepth, @IFDepth);
  End;
  PImage.Canvas.Draw(0,0,FBDepth);
  For X:=0 To PImage.ControlCount-1 Do
    If PImage.Controls[x] is TShape Then
      PImage.Controls[x].Repaint;
End;



Procedure TMainForm.TBAngleChange(Sender: TObject);

Var
  A : Longint;
Begin
  If TBAngle.Position=FLastPosition Then
    Exit;
  If Not Failed (FKinect.NuiCameraElevationGetAngle(@A)) Then
    Begin
      If (A<>-TBAngle.Position) Then
        A := -TBAngle.Position;
      Begin
        If Failed (FKinect.NuiCameraElevationSetAngle(A)) Then
          ShowMessage(Format(SErrSetAngle,[A]));
        FLastPosition := -A;
      End;
    End
  Else
    ShowMessage(SErrGetAngle);
End;


end.

