import datetime
import cv2
import time
import numpy as np

from picamera.array import PiRGBArray
from picamera import PiCamera

USB=False




def get_top_left(src):
   # must have BLACK marker 
   left = 0
   top = 0
   new_width = 10
   height = 480-1
   cnt = 0
   keep_going = True
   while (keep_going) :
     new_width += 10
     cnt += 1
     print "cnt", cnt,left,new_width,top,height
     # swap args x,y !!!
     cropped = src[top:height,left:new_width]     
     circles = cv2.HoughCircles(cropped,method=cv2.HOUGH_GRADIENT,dp=1,minDist=20,param1=50,param2=30,minRadius=0,maxRadius=40)
     if circles is not None:
       if len(circles) == 1:
     
         for i in circles [0,:]:
           print "0/1/2",i[0],i[1],i[2]
           cmin = 99999
           cmax = 0
           for y in range(int(i[0]-5),int(i[0]+5)):
             for x in range(int(i[1]-5),int(i[1]+5)):
               b=src[x,y]
               if b > cmax :
                 cmax = b
               if b < cmin :
                 cmin = b
                 
           print "get_top_left",i[0],i[1],i[2],cmin,cmax
           keep_going = False
           break       
   
   return (i[0],i[1],i[2],cmax)
   
def get_bottom_right(src,r):
   # must have WHITE marker 
   # Bottom right is x=(20~22) * r, y=(10~12)*r
   x1tl = int(r*25)
   y1tl = int(r*13)
   x2tl = int(r*30) 
   y2tl = int(r*18)
   cnt = 0
   keep_going = True
   while (keep_going) :
     keep_going = False
     cnt += 1
     print "cnt", cnt,x1tl,y1tl,x2tl,y2tl
     # swap args x,y !!!
     print "y1tl:y2tl,x1tl:x2tl", y1tl,y2tl,x1tl,x2tl
     cropped = src[y1tl:y2tl,x1tl:x2tl]          
     cv2.imwrite('/dev/shm/8.png',cropped)
     circles = cv2.HoughCircles(cropped,method=cv2.HOUGH_GRADIENT,dp=1,minDist=20,param1=50,param2=30,minRadius=0,maxRadius=40)
     if circles is not None:
       if len(circles) == 1:
     
         for i in circles [0,:]:
           print "0/1/2",i[0],i[1],i[2]
           cmin = 99999
           cmax = 0341
           for y in range(int(i[0]-5),int(i[0]+5)):
             for x in range(int(i[1]-5),int(i[1]+5)):
               b=cropped[x,y]
               if b > cmax :
                 cmax = b
               if b < cmin :
                 cmin = b
                 
           print "get_bottom_right", x1tl+i[0],y1tl+i[1],i[2],cmin,cmax
           break       
   
   return (x1tl+i[0],y1tl+i[1],cmin)

   


def get_x_index(x,tl,br):
  xpct = 100.0 * (float(x)-tl[0])/(br[0] - tl[0])
  if xpct < 5:
    ret = -1
  elif xpct < 27+10:
    ret = 0
  elif xpct < 47+10:
    ret = 1
  elif xpct < 61+10:
    ret = 2
  elif xpct < 75+10:
    ret = 3
  else:
    ret = 4000
  #print "get_x_index", "tl", tl, "br", br
  print "get_x_index", x, "->", int(xpct), "->", ret
  return ret
  
def get_y_index(y,tl,br):
  ypct = 100.0 * (float(y) - tl[1]) /(br[1] - tl[1]) 
  if ypct < 0+20:
    ret = 0
  elif ypct < 27+10:
    ret = 1
  elif ypct < 55+10:
    ret = 2
  else:
    ret = 3000
  print "get_y_index", y, "->", int(ypct) , "->", ret
  return ret
    
    
if USB:
  camera = cv2.VideoCapture(0)
else:
  camera = PiCamera()
  camera.resolution = (640, 480)
  
font = cv2.FONT_HERSHEY_SIMPLEX
while (True):
 
  grey = None
  blur = None
  frame = None
  
  if USB:
    ret , frame = camera.read()
  else:
    rawCapture = PiRGBArray(camera)   
    # allow the camera to warmup
    time.sleep(0.1)
    # grab an image from the camera
    camera.capture(rawCapture, format="bgr")
    frame = rawCapture.array    
  
 
  #frame = cv2.imread('/dev/shm/1.png')
  height, width, channels = frame.shape   
  print "height, width, channels", height, width, channels
  
  grey = cv2.cvtColor(frame,cv2.COLOR_BGR2GRAY)
  cv2.imwrite('/dev/shm/2.png',grey)
  blur = cv2.blur(grey,(5,5))
  cv2.imwrite('/dev/shm/3.png',blur)
  
  top_left=get_top_left(blur)
  #add radius to call
  bottom_right=get_bottom_right(blur,top_left[2])
  print "top_left",top_left
  print "bottom_right", bottom_right
  
  
  board = [['-','-','-','-'],['-','-','-','-'],['-','-','-','-'],['-','-','-','-']]
  print board  
  
  
  circles = cv2.HoughCircles(blur,method=cv2.HOUGH_GRADIENT,dp=1,minDist=20,param1=50,param2=30,minRadius=0,maxRadius=40)
  if circles is not None:
    for i in circles [0,:]:
      #print "0/1/2",i[0],i[1],i[2]
      cv2.putText(grey, str(int(i[0])), (int(i[0]-8),int(i[1]-5)), font, 0.2, (255, 255, 255), 1, cv2.LINE_AA)
      cv2.putText(grey, str(int(i[1])), (int(i[0]-8),int(i[1]+5)), font, 0.2, (255, 255, 255), 1, cv2.LINE_AA)
      #circle is (x,y) based, frams is heigh-width based (y,x) that is .. so switch them
      cl='S'
      
      for y in range(int(i[0]-5),int(i[0]+5)):
        for x in range(int(i[1]-5),int(i[1]+5)):
          #c=frame[x,y]
          #g=grey[x,y]
          b=blur[x,y]
          if b > top_left[3]*1.5 :
            cl = 'V'
            
          #print "color", x,y,"|",c,"|",g,"|",b           
      cv2.circle(frame,(i[0],i[1]),i[2],(0,255,0),2)
      cv2.circle(frame,(i[0],i[1]),i[2],(0,255,0),2)
      cv2.putText(blur, cl, (int(i[0]-5),int(i[1])), font, 0.5, (255, 255, 255), 1, cv2.LINE_AA)

      x=get_x_index(i[0],top_left,bottom_right)  
      y=get_y_index(i[1],top_left,bottom_right)  
      #print x,y,i[0],i[1],cl
      if x >=1 and x <=3 and y >=0 and y <=3 :
        #board[y][x]= cl + " " + str(int(i[0])) + "," + str(int(i[1]))
        board[y][x]= cl
      
      #cv2.circle(frame,(i[0],i[1]),2,(0,0,255),3)
#  cv2.imshow('Detected',frame)
  cv2.imwrite('/dev/shm/4.jpg',frame)  
  cv2.imwrite('/dev/shm/5.jpg',blur)  
  cv2.imwrite('/dev/shm/6.png',grey)
  cv2.destroyAllWindows()
  if USB:
    camera.release()
  
  print board  
  print "-------"
  print "|" + board[0][1] + "|" + board[0][2] +"|" + board[0][3] + "|" 
  print "-------"
  print "|" + board[1][1] + "|" + board[1][2] +"|" + board[1][3] + "|" 
  print "-------"
  print "|" + board[2][1] + "|" + board[2][2] +"|" + board[2][3] + "|" 
  print "-------"
  #break
  


#print "-------------------"
##print board[0][0]
##print board[1][0]
##print board[2][0]
##print board[3][0]
#print "-------------------"
#print board[0][1]
#print board[1][1]
#print board[2][1]
##print board[3][1]
#print "-------------------"
#print board[0][2]
#print board[1][2]
#print board[2][2]
##print board[3][2]
#print "-------------------"
#print board[0][3]
#print board[1][3]
#print board[2][3]
##print board[3][3]
#print "-------------------"

print "-------"
print "|" + board[0][1] + "|" + board[0][2] +"|" + board[0][3] + "|" 
print "-------"
print "|" + board[1][1] + "|" + board[1][2] +"|" + board[1][3] + "|" 
print "-------"
print "|" + board[2][1] + "|" + board[2][2] +"|" + board[2][3] + "|" 
print "-------"





