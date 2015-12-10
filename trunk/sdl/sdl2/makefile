
SDL2_LIBS := -L/usr/lib/x86_64-linux-gnu -lSDL2 -lpthread -Wl,--no-undefined -lm -ldl -lasound -lm -ldl -lpthread -lpulse-simple -lpulse -lX11 -lXext -lXcursor -lXinerama -lXi -lXrandr -lXss -lXxf86vm -lwayland-egl -lwayland-client -lwayland-cursor -lxkbcommon -lpthread -lrt -lSDL2_ttf
# -lSDL2_image



.PHONY : all clean

all:
#	echo $(SDL2_LIBS)
	gprbuild -p -P sdl2test.gpr -largs $(SDL2_LIBS) 

clean:
	gnatclean -P sdl2test.gpr  
