import cv2

cap = cv2.VideoCapture(0)

# Check if the webcam is opened correctly
if not cap.isOpened():
    raise IOError("Cannot open webcam")

Keep_Going = True
while Keep_Going:
    try:
        ret, frame = cap.read()
        frame = cv2.resize(frame, None, fx=0.5, fy=0.5, interpolation=cv2.INTER_AREA)
        cv2.imshow('Input', frame)
        c = cv2.waitKey(1)
        print(c)
        if c == 27:
            Keep_Going = False
    except KeyboardInterrupt:
        Keep_Going = False

cap.release()
cv2.destroyAllWindows()
