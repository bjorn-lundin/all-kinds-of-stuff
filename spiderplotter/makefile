###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

.PHONY : all clean 

all : 
	gprbuild -p -P plotter.gpr 

clean :
	gnat clean -P plotter.gpr
	rm -f *.o *.ali plotter_main
	