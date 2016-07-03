import cv2
import time
#cam = cv2.VideoCapture(1)

while (True):
  #ret , frame = cam.read()
  #print ret
  frame = cv2.imread("coins.jpeg")
  cv2.imshow('Detected',frame)
  print "1"
  time.sleep(10)
  grey = cv2.cvtColor(frame,cv2.COLOR_BGR2GRAY)
  cv2.imshow('Detected',grey)
  print "2"
  blur = cv2.blur(grey,(5,5))
  cv2.imshow('Detected',blur)
  print "3"
  circles = cv2.HoughCircles(blur,method=cv2.cv.CV_HOUGH_GRADIENT,dp=1,minDist=200,param1=50,param2=13,minRadius=30,maxRadius=175)
  if circles is not None:
    for i in circles [0,:]:
      cv2.circle(frame,(i[0],i[1]),i[2],(0,255,0),2)
      cv2.circle(frame,(i[0],i[1]),2,(0,0,255),3)
  cv2.imshow('Detected',frame)
  print "4"
  time.sleep(10)
  if cv2.waitKey(5) == 27:
    break
  cv2.destroyAllWindows()
  cam.release()
  
  
