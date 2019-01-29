import cv2
import time
import numpy as np

from picamera.array import PiRGBArray
from picamera import PiCamera

font = cv2.FONT_HERSHEY_SIMPLEX
while (True):
  cam = cv2.VideoCapture(0)
  ret , frame = cam.read()
  
  camera = PiCamera()
  camera.resolution = (640, 480)
  rawCapture = PiRGBArray(camera)
   
  # allow the camera to warmup
  time.sleep(0.1)
   
  # grab an image from the camera
  camera.capture(rawCapture, format="bgr")
  frame = rawCapture.array  
   
 
  #frame = cv2.imread('images/1.png')
  height, width, channels = frame.shape   
  print "height, width, channels", height, width, channels
#  cv2.imshow('Detected',frame)
#  cv2.imwrite('images/1.png',frame)
  print "1"
#  time.sleep(10)
  grey = cv2.cvtColor(frame,cv2.COLOR_BGR2GRAY)
#  cv2.imshow('Detected',grey)
  cv2.imwrite('images/2.png',grey)
  print "2"
  blur = cv2.blur(grey,(5,5))
#  cv2.imshow('Detected',blur)
  cv2.imwrite('images/3.png',blur)
  print "3"
  
  circles = cv2.HoughCircles(blur,method=cv2.HOUGH_GRADIENT,dp=1,minDist=20,param1=50,param2=30,minRadius=0,maxRadius=40)
  if circles is not None:
    for i in circles [0,:]:
      print "0/1/2",i[0],i[1],i[2]
      cv2.putText(grey, str(int(i[0])), (int(i[0]-8),int(i[1]-5)), font, 0.2, (255, 255, 255), 1, cv2.LINE_AA)
      cv2.putText(grey, str(int(i[1])), (int(i[0]-8),int(i[1]+5)), font, 0.2, (255, 255, 255), 1, cv2.LINE_AA)
      #circle is (x,y) based, frams is heigh-width based (y,x) taht is .. so switch them
      cl=0
      
      for y in range(int(i[0]-5),int(i[0]+5)):
        for x in range(int(i[1]-5),int(i[1]+5)):
          #c=frame[x,y]
          #g=grey[x,y]
          b=blur[x,y]
          if b > 70 :
            cl = b
            
          #print "color", x,y,"|",c,"|",g,"|",b           
      cv2.circle(frame,(i[0],i[1]),i[2],(0,255,0),2)
      cv2.circle(frame,(i[0],i[1]),i[2],(0,255,0),2)
      if cl == 0:
        cv2.putText(blur, 'S', (int(i[0]-5),int(i[1])), font, 0.5, (255, 255, 255), 1, cv2.LINE_AA)
      else:     
        cv2.putText(blur, 'V', (int(i[0]-5),int(i[1])), font, 0.5, (255, 255, 255), 1, cv2.LINE_AA)
      
      #cv2.circle(frame,(i[0],i[1]),2,(0,0,255),3)
#  cv2.imshow('Detected',frame)
  cv2.imwrite('images/4.jpg',frame)  
  cv2.imwrite('images/5.jpg',blur)  
  cv2.imwrite('images/6.png',grey)
  print "4"
#  time.sleep(10)
#  if cv2.waitKey(5) == 27:
#    break
  cv2.destroyAllWindows()
#  cam.release()
  break
  
  
