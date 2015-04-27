###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################


C_DIRECTORY := $(HOME)/svn/brickpi_c/trunk/Sensor_Examples
C_OBJECT := ../objects/bnlbrickpi.o
C_SOURCE := bnlbrickpi.c

.PHONY : all clean

all : 
	 $(MAKE) $(C_OBJECT)

objects :
	mkdir ../objects

    
$(C_OBJECT) : ../objects tick.h BrickPi.h bnlbrickpi.h bnlbrickpi.c
	gcc -c -Wall $(C_SOURCE) -o $(C_OBJECT) -L/usr/local/lib -lwiringPi -lm -lrt

BrickPi.h : $(C_DIRECTORY)/BrickPi.h
	cp -f $(C_DIRECTORY)/BrickPi.h ./

tick.h : $(C_DIRECTORY)/tick.h
	cp -f $(C_DIRECTORY)/tick.h ./

clean :
	rm -f $(C_OBJECT)
