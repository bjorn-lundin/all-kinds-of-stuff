# import the necessary packages
import argparse
import imutils
import cv2
import time


# construct the argument parse and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-i", "--image", required=True,
	help="path to the input image")
args = vars(ap.parse_args())

# load the image, convert it to grayscale, blur it slightly,
# and threshold it
image = cv2.imread(args["image"])
gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
cv2.imshow("Imagegray", gray)

blurred = cv2.GaussianBlur(gray, (5, 5), 0)
cv2.imshow("blurred", blurred)

t=140
threshold = cv2.threshold(blurred, t, 255, cv2.THRESH_BINARY)[1]
cv2.imshow("threshold" + str(t), threshold)


# find contours in the thresholded image
contours, hierarchy = cv2.findContours(image=threshold.copy(),mode=cv2.RETR_EXTERNAL,method=cv2.CHAIN_APPROX_NONE)

min_area = 10 
# loop over the contours
image_r = image.copy()
image_c = image.copy()
for contour in contours:        
	print('---------------')
	# compute the center of the contour
	area = cv2.contourArea(contour)
	print('Area',area)
	M = cv2.moments(contour)
	if M["m00"] > min_area:
		cX = int(M["m10"] / M["m00"])
		cY = int(M["m01"] / M["m00"])
	else:
		print("too small, min_area",min_area,"real_area",M["m00"], "skipping")
		continue
	
	# draw the contour and center of the shape on the image
	cv2.drawContours(image,contour,-1,color=(0,0,0),thickness=3)
	cv2.circle(image, (cX, cY), 7, (255, 255, 255), -1)
	cv2.putText(image, "center", (cX - 20, cY - 20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 255), 2)
	print('cX',cX,'cY',cY)

	(x,y),radius = cv2.minEnclosingCircle(contour)
	center = (int(x),int(y))
	radius = int(radius)
	cv2.circle(image_c,center,radius,(0,0,255),2)	
	print('xc',int(x),'yc',int(y))

	perimeter = cv2.arcLength(contour, closed=True)
	borders = cv2.approxPolyDP(curve=contour,epsilon=0.05*perimeter,closed=True)
	x,y,w,h = cv2.boundingRect(borders)
	cv2.rectangle(image_r,(x,y),(x+w,y+h),(255,0,0),2)
	print('xb',int(x+(w/2)),'yb',int(y+(h/2)))

	cv2.imshow("Image_r", image_r)
	cv2.imshow("Image_c", image_c)
	cv2.imshow("Image", image)
print('done')
cv2.waitKey(0)

