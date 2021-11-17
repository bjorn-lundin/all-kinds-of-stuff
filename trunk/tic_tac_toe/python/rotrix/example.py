
import cv2

from pydexarm import Dexarm

'''mac & linux'''
arm = Dexarm("/dev/ttyACM0")

cap = cv2.VideoCapture(0)

# Check if the webcam is opened correctly
if not cap.isOpened():
    raise IOError("Cannot open webcam")
    
    
arm.go_home()


def X(n):
  return -70+n*46

def Y(n):
  return 230+n*46


for x in [0,1,2,3] : 
  for y in [0,1,2,3] : 
    print("x=",str(x),"y=",str(y))
    arm.move_to(X(x), Y(y), 0)
    ret, frame = cap.read()
    print(frame.shape)
    #frame = cv2.resize(frame, None, fx=0.5, fy=0.5, interpolation=cv2.INTER_AREA)
    cv2.imwrite('r1_' + str(x) + '_' + str(y) + '.png',frame)
    cv2.imshow('image', frame)
    arm.move_to(X(x), Y(y), -60)
    if y==0 or y==2:
      arm.air_picker_pick()

    if y==1 or y==3:
      arm.air_picker_nature()
      arm.delay_s(1)

    arm.move_to(X(x), Y(y), 0)

arm.air_picker_stop()
arm.go_home()

cap.release()
cv2.destroyAllWindows()

