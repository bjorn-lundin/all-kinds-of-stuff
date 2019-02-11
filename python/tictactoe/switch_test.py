from time import sleep
import RPi.GPIO as GPIO

GPIO.setwarnings(False)

#try: 
ENABLE = 5

GPIO.setmode(GPIO.BCM)
GPIO.setup(ENABLE, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)

for x in range(1000):
        print GPIO.input(ENABLE)
        sleep(1)

