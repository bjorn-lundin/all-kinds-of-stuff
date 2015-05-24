###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

.PHONY : all clean auto lob

all : 
	gprbuild -p -P brickpi.gpr 
lib :
	cd c && gprbuild -p -P cbrickpi.gpr 

auto : brickpi-constants.ads c_constants.ads
    
bin/create_constants : create_constants.adb    
	gprbuild -p -P brickpi.gpr create_constants
 
brickpi-constants.ads : bin/create_constants
	bin/create_constants --brick > brickpi-constants.ads

c_constants.ads : bin/create_constants
	bin/create_constants --io > c_constants.ads
    
clean :
	cd c && gnat clean -P cbrickpi.gpr
	gnat clean -P brickpi.gpr
	rm brickpi-constants.ads  c_constants.ads
	