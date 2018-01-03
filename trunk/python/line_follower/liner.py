
# -*- coding: iso-8859-15 -*-
import PID
import numpy as np
import cv2
from BrickPi import *
 
video_capture = cv2.VideoCapture(0)
video_capture.set(3, 160)
video_capture.set(4, 120)
 
#pid = PID(3.0,0.4,1.2)
#pid.setPoint(80) 

Full_Speed_Forward = 200 

BrickPiSetup()  # setup the serial port for communication

BrickPi.MotorEnable[PORT_B] = 1 
BrickPi.MotorEnable[PORT_C] = 1 

BrickPiSetupSensors()   #Send the properties of sensors to BrickPi

while(True):
 
    # Capture the frames
    ret, frame = video_capture.read()
 
    # Crop the image
    crop_img = frame[60:120, 0:160]
 
    # Convert to grayscale
    gray = cv2.cvtColor(crop_img, cv2.COLOR_BGR2GRAY)
 
    # Gaussian blur
    blur = cv2.GaussianBlur(gray,(5,5),0)
 
    # Color thresholding
    ret,thresh = cv2.threshold(blur,60,255,cv2.THRESH_BINARY_INV)

    #cv2.imshow('frame2',thresh)
    #break 
    # Find the contours of the frame
    #_ , contours,hierarchy = cv2.findContours(thresh.copy(), cv2.RETR_LIST, cv2.CHAIN_APPROX_NONE)
    contours,hierarchy = cv2.findContours(thresh.copy(), 1, cv2.CHAIN_APPROX_NONE)
    # Find the biggest contour (if detected)
    if len(contours) > 0:
        c = max(contours, key=cv2.contourArea)
        M = cv2.moments(c)
 
        cx = int(M['m10']/M['m00'])
        cy = int(M['m01']/M['m00'])
 
        cv2.line(crop_img,(cx,0),(cx,720),(255,0,0),1)
        cv2.line(crop_img,(0,cy),(1280,cy),(255,0,0),1)
 
        cv2.drawContours(crop_img, contours, -1, (0,255,0), 1)
        
      #  ctrl = pid.update(cx) 
      #  print str(ctrl),str(cx),"ctrl-value/cx"
        
        if cx >= 120:
            print str(cx),"Turn Left!"
            BrickPi.MotorSpeed[PORT_B] = 0
            BrickPi.MotorSpeed[PORT_C] = Full_Speed_Forward
 
        elif cx < 120 and cx > 40:
            print str(cx),"On Track!"
            BrickPi.MotorSpeed[PORT_B] = Full_Speed_Forward
            BrickPi.MotorSpeed[PORT_C] = Full_Speed_Forward

        elif cx <= 40:
            print str(cx),"Turn Right"
            BrickPi.MotorSpeed[PORT_B] = Full_Speed_Forward
            BrickPi.MotorSpeed[PORT_C] = 0 
    else:
        print "I don't see the line"
        BrickPi.MotorSpeed[PORT_B] = 0  #(-255 to 255)
        BrickPi.MotorSpeed[PORT_C] = 0  # (-255 to 255) 
 
    #Display the resulting frame
    BrickPiUpdateValues() # Ask BrickPi to update values for sensors/motors
    cv2.imshow('frame',crop_img)
    if cv2.waitKey(100) & 0xFF == ord('q'):
        break

#cv2.waitKey(10000)
