import numpy as np
import cv2


#More information: http://en.wikipedia.org/wiki/PID_controller
#
#cnr437@gmail.com
#
#######	Example	#########
#
#p=PID(3.0,0.4,1.2)
#p.setPoint(5.0)
#while True:
#     pid = p.update(measurement_value)
#
#


class PID:
	"""
	Discrete PID control
	"""

	def __init__(self, P=2.0, I=0.0, D=1.0, Derivator=0, Integrator=0, Integrator_max=500, Integrator_min=-500):

		self.Kp=P
		self.Ki=I
		self.Kd=D
		self.Derivator=Derivator
		self.Integrator=Integrator
		self.Integrator_max=Integrator_max
		self.Integrator_min=Integrator_min

		self.set_point=0.0
		self.error=0.0

	def update(self,current_value):
		"""
		Calculate PID output value for given reference input and feedback
		"""

		self.error = self.set_point - current_value

		self.P_value = self.Kp * self.error
		self.D_value = self.Kd * ( self.error - self.Derivator)
		self.Derivator = self.error

		self.Integrator = self.Integrator + self.error

		if self.Integrator > self.Integrator_max:
			self.Integrator = self.Integrator_max
		elif self.Integrator < self.Integrator_min:
			self.Integrator = self.Integrator_min

		self.I_value = self.Integrator * self.Ki

		PID = self.P_value + self.I_value + self.D_value

		return PID

	def setPoint(self,set_point):
		"""
		Initilize the setpoint of PID
		"""
		self.set_point = set_point
		self.Integrator=0
		self.Derivator=0

	def setIntegrator(self, Integrator):
		self.Integrator = Integrator

	def setDerivator(self, Derivator):
		self.Derivator = Derivator

	def setKp(self,P):
		self.Kp=P

	def setKi(self,I):
		self.Ki=I

	def setKd(self,D):
		self.Kd=D

	def getPoint(self):
		return self.set_point

	def getError(self):
		return self.error

	def getIntegrator(self):
		return self.Integrator

	def getDerivator(self):
		return self.Derivator


# enc class pid


 
video_capture = cv2.VideoCapture(0)
video_capture.set(3, 160)
video_capture.set(4, 120)
 
pid = PID(3.0,0.4,1.2)
pid.setPoint(80) 
 
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
    _ , contours,hierarchy = cv2.findContours(thresh.copy(), cv2.RETR_LIST, cv2.CHAIN_APPROX_NONE)
 
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
 
        if cx < 120 and cx > 50:
            print str(cx),"On Track!"
 
        if cx <= 50:
            print str(cx),"Turn Right"
 
    else:
        print "I don't see the line"
 
    #Display the resulting frame
    cv2.imshow('frame',crop_img)
    if cv2.waitKey(100) & 0xFF == ord('q'):
        break

#cv2.waitKey(10000)
