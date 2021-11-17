
import cv2
from pydexarm import Dexarm


def X(n):
  return -70+n*46

def Y(n):
  return 230+n*46


'''mac & linux'''
arm = Dexarm("/dev/ttyACM0")

cap = cv2.VideoCapture(0)
# Check if the webcam is opened correctly
if not cap.isOpened():
    raise IOError("Cannot open webcam")
   
arm.go_home()

arm.move_to(X(5), Y(0), 0)
ret, frame = cap.read()
print(frame.shape)
cv2.imwrite('r1_' + str(5) + '_' + str(0) + '.png',frame)
cv2.imwrite('r1_' + str(5) + '_' + str(0) + '.jpg',frame)



cap.release()
cv2.destroyAllWindows()

