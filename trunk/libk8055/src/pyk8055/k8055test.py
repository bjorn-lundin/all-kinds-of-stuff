#!/usr/bin/env python
#
# Sample pyton test program for pyk8055 wrapper
#
from time import sleep
from pyk8055 import *

try:
	# Open device port 0
	k = k8055(0)

	# set analog output channel 1 to 200/255*5v = 3.9V
	k.OutputAnalogChannel(1,200)
	sleep(1)
	# set analog output channel low again = 0V
	k.OutputAnalogChannel(1,0)

	# read both analog inputs
	# note: this returns a list
	res = k.ReadAllAnalog()
	print res[1:]

	# Test class string function
	print str(k)

	# Set debounce time on counter 1
	k.SetCounterDebounceTime(1, 100)

	# reset counter 1
	k.ResetCounter(1)

	# create a rotating display of digital outputs
	# while waiting for keypress on digital input 1
	# (longer than .2sec :) 
	a,d = 0,1
	k.WriteAllDigital(1)
	while a == 0:		# is key 1 down (input 1 high)
		a = k.ReadDigitalChannel(1)
		sleep(0.2)	# wait .2 sec
		d *=2		# and continue rotating digital outputs
		if d > 128: d=1;
		k.WriteAllDigital(d)

	print "Digital input 1=",a

	# read the counter on input 1
	c1 = k.ReadCounter(1)
	print "Counter(1)=",c1
	# read the counter on input 2
	c2 = k.ReadCounter(2)
	print "Counter(2)=",c2

	# set even bits on digital outputs
	k.WriteAllDigital(170)
	sleep(1)
	# set odd bits on digital outputs
	k.WriteAllDigital(85)
	sleep(1)
	# Clear all digital outputs
	k.WriteAllDigital(0)

	# and close
	k.CloseDevice()

except IOError:
	print "Could not open Device"


