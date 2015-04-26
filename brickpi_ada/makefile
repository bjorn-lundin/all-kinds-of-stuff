###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################


C_DIRECTORY := $(HOME)/svn/brickpi_c/trunk/Sensor_Examples
C_OBJECT := c/bnlbrickpi.o
C_SOURCE := c/bnlbrickpi.c


.PHONY : all clean

all :
	$(MAKE) brick 2>&1 | tee make.log

brick : $(C_OBJECT)
	gnatmake -p -P brickpi.gpr

$(C_OBJECT) : c/bnlbrickpi.h c/bnlbrickpi.c c/BrickPi.h c/tick.h
	cd c && gcc -c -Wall bnlbrickpi.c -o bnlbrickpi.o -L/usr/local/lib -lwiringPi -lm -lrt


c/BrickPi.h : $(C_DIRECTORY)/BrickPi.h
	cp -f $(C_DIRECTORY)/BrickPi.h c/

c/tick.h : $(C_DIRECTORY)/tick.h
	cp -f $(C_DIRECTORY)/tick.h c/

clean :
	rm -f  c/BrickPi.h c/tick.h c/*.o
	gnatclean -P brickpi.gpr