# TechVidvan Object detection of similar color

import cv2
import numpy as np

# Reading the image
img = cv2.imread('IMG_2191_small2.jpg')

# Showing the output

cv2.imshow("Image", img)
print('1')



# convert to hsv colorspace
hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
print('2')

# lower bound and upper bound for Green color
lower_bound = np.array([50, 20, 20])   
upper_bound = np.array([100, 255, 255])

# find the colors within the boundaries
mask = cv2.inRange(hsv, lower_bound, upper_bound)
print('3')


#define kernel size  
kernel = np.ones((7,7),np.uint8)

# Remove unnecessary noise from mask

mask = cv2.morphologyEx(mask, cv2.MORPH_CLOSE, kernel)
mask = cv2.morphologyEx(mask, cv2.MORPH_OPEN, kernel)
print('4')


# Segment only the detected region
segmented_img = cv2.bitwise_and(img, img, mask=mask)


# Find contours from the mask

contours, hierarchy = cv2.findContours(mask.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
print('5')

output = cv2.drawContours(segmented_img, contours, -1, (0, 0, 255), 3)

# Showing the output

cv2.imshow("Output", output)

print('6')


# lower bound and upper bound for Yellow color

lower_bound = np.array([20, 80, 80])   
upper_bound = np.array([30, 255, 255])


# Draw contour on original image

output = cv2.drawContours(img, contours, -1, (0, 0, 255), 3)
print('7')
cv2.imshow("Output final", output)
print('8')

cv2.waitKey(0)
cv2.destroyAllWindows()

print('done')


