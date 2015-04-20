###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################


C_DIRECTORY := $(HOME)/svn/brickpi_c/trunk/Sensor_Examples
C_OBJECT := $(C_DIRECTORY)/bnlbrickpi.o
C_SOURCE := $(C_DIRECTORY)/bnlbrickpi.c


.PHONY : all clean

all :
	$(MAKE) brick 2>&1 | tee make.log

brick : $(C_OBJECT)
	gnatmake -p -P brickpi.gpr

$(C_OBJECT) : $(C_SOURCE)
	gcc -c -Wall  $(C_SOURCE) -o $(C_OBJECT)

clean :
	rm -f $(C_OBJECT)
	gnatclean -P brickpi.gpr