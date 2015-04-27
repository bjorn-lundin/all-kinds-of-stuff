###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

.PHONY : all clean auto

all : 
	cd c && $(MAKE) 
	gnatmake -p -P brickpi.gpr 

auto : brickpi-constants.ads
    
bin/create_constants : create_constants.adb    
	gnatmake -p -P brickpi.gpr create_constants
 
brickpi-constants.ads : bin/create_constants
	bin/create_constants > brickpi-constants.ads

clean :
	cd c && $(MAKE) clean
	gnatclean -P brickpi.gpr
	rm brickpi-constants.ads
	