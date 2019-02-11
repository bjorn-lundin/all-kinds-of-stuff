from time import sleep
import RPi.GPIO as GPIO

GPIO.setwarnings(False)

try: 
    MAX_CNT = 2
    DIR = 24      # Direction GPIO Pin
    STEP = 23      # Step GPIO Pin
    ENABLE = 4
    CW = 1         # Clockwise Rotation
    CCW = 0        # Counterclockwise Rotation
    SPR = 200       # Steps per Revolution (360 / 1.8)

    RELAY = 21

    GPIO.setmode(GPIO.BCM)
    GPIO.setup(DIR, GPIO.OUT)
    GPIO.setup(STEP, GPIO.OUT)
    GPIO.setup(ENABLE, GPIO.OUT)
    GPIO.setup(RELAY, GPIO.OUT)


    for x in range(10):
            GPIO.output(RELAY, GPIO.HIGH)
            sleep(1)
            GPIO.output(RELAY, GPIO.LOW)
            sleep(1)


    GPIO.output(DIR, CW)

#MODE = (14, 15, 18) # Microstep Resolution GPIO Pins
#GPIO.setup(MODE, GPIO.OUT)
#RESOLUTION = {'Full': (0, 0, 0),
#              'Half': (1, 0, 0),
#              '1/4': (0, 1, 0),
#              '1/8': (1, 1, 0),
#              '1/16': (0, 0, 1),
#              '1/32': (1, 0, 1)}
#
#GPIO.output(MODE, RESOLUTION['1/32'])
    SPR = 200
    step_count = SPR 
    print "stepcount", step_count
    delay = 0.005 
    cnt = 0

    GPIO.output(ENABLE, GPIO.LOW)
    while True:
        cnt = cnt +1
        GPIO.output(DIR, CW)
        for x in range(step_count):
            GPIO.output(STEP, GPIO.HIGH)
            sleep(delay)
            GPIO.output(STEP, GPIO.LOW)
            sleep(delay)

        sleep(1)
        print "other dir"
        GPIO.output(DIR, CCW)

        for x in range(step_count):
            GPIO.output(STEP, GPIO.HIGH)
            sleep(delay)
            GPIO.output(STEP, GPIO.LOW)
            sleep(delay)
        if cnt >= MAX_CNT:
            break
        sleep(1)
except:  
    # this catches ALL other exceptions including errors.  
    # You won't get any error messages for debugging  
    # so only use it once your code is working  
    print "Other error or exception occurred!"  

finally:
    print "will cleanup"
    GPIO.output(ENABLE, GPIO.HIGH)
#    GPIO.cleanup()
    print "did cleanup"
