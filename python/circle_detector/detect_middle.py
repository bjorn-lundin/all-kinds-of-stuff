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
print('1')
cv2.imshow("Imagegray", gray)
print('2')

blurred = cv2.GaussianBlur(gray, (5, 5), 0)
cv2.imshow("blurred", blurred)
print('3')


#for t in range(141,141) :
t=140
threshold = cv2.threshold(blurred, t, 255, cv2.THRESH_BINARY)[1]
cv2.imshow("thresh old " + str(t), threshold)
print('4')

#for s in range(5,1) :
#  if s % 2 == 1 and s > 1:
#    for c in range(0,5) :

s=5
c=1
#thresh = cv2.adaptiveThreshold(blurred,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY,s,c)
#cv2.imshow("thresh gauss s=" + str(s) + " c=" + str(c), thresh)
#(T, threshInv) = cv2.threshold(blurred, 141, 255,cv2.THRESH_BINARY_INV)
#cv2.imshow("threshinv " + str(T), threshInv)

print('5')

c_list = [] 
#time.sleep(3)
circles=None
circles = cv2.HoughCircles(
        threshold,
        method=cv2.HOUGH_GRADIENT,
        dp=1,
        minDist=50,
        param1=100,
        param2=100,
        minRadius=0,
        maxRadius=0,
        )
print('6')
print( 'circles', circles)
        
if circles is not None:
  for i in circles[0, :]:
    print( 'circle', 1)
    cv2.circle(threshold, (i[0], i[1]), i[2], (0, 255, 0), 2)
    print( 'circle', i[0], i[1], i[2])
    #cmin = 99999
    #cmax = 0
    #cmean = 0
    #cnt = 0
            # circle is (x,y) based, frams is heigh-width based (y,x) that is .. so switch them
    #for y in range(int(i[0] - 5), int(i[0] + 5)):
    #  for x in range(int(i[1] - 5), int(i[1] + 5)):
    #    b = blur[x, y]
    #    cnt +=1
    #    cmean += b 
    #    if b > cmax:
    #      cmax = b
    #    if b < cmin:
    #      cmin = b
    #    cmean /= cnt  
    #    print 'circle', i[0], i[1], i[2], cmin, cmax, cmean
    #    c_list.append(( i[0], i[1], i[2], cmin, cmax, cmean))
    #   cv2.circle(thresh, (i[0], i[1]), i[2], (0, 255, 0), 2)

print('7')

cv2.imshow("thresh gauss circles", threshold)
print('8')

# find contours in the thresholded image
cnts1 = cv2.findContours(threshold.copy(), cv2.RETR_EXTERNAL,
	cv2.CHAIN_APPROX_SIMPLE)
#print('cnts1',cnts)
	
cnts = imutils.grab_contours(cnts1)
#print('cnts2',cnts)

 
# loop over the contours
for c in cnts:        
	#print('c',c)
	# compute the center of the contour
	M = cv2.moments(c)
#	print ("M",M)
	cX = int(M["m10"] / M["m00"])
	cY = int(M["m01"] / M["m00"])
	# draw the contour and center of the shape on the image
	cv2.drawContours(image, [c], -1, (0, 255, 0), 2)
	cv2.circle(image, (cX, cY), 7, (255, 255, 255), -1)
	cv2.putText(image, "center", (cX - 20, cY - 20),
		cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 255), 2)
	# show the image
	print('9')
	cv2.imshow("Image", image)
	print('10')
cv2.waitKey(0)

