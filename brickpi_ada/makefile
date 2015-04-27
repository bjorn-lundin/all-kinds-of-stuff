###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

.PHONY : all clean

all :
	cd c && $(MAKE) 
	gnatmake -p -P brickpi.gpr 

clean :
	cd c && $(MAKE) clean
	gnatclean -P brickpi.gpr