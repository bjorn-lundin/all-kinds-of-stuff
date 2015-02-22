import time
import RPi.GPIO as io
#import picamera
import subprocess

io.setmode(io.BCM)

pir_pin = 18
#camera = picamera.PiCamera()

io.setup(pir_pin, io.IN)         # activate input

Take_Phote = True 

while True:
    if io.input(pir_pin):
        print("PIR ALARM!")
        if Take_Phote :
            print("would take photo now!")
            #camera.capture('/tmp/pic.jpg')
            subprocess.call(['/usr/bin/raspistill', '-o', '/tmp/pic.jpg'], shell=True)
            #subprocess.call(['/usr/bin/raspistill --nopreview -w640 -h480 -q5 -o/tmp/pic.jpg'], shell=True)
            Take_Phote = False 
    else:
        Take_Phote = True 
        print("nothing!")
    time.sleep(1.5)

