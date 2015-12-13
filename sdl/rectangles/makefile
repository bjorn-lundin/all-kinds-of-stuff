
SDL2_LIBS := -L/usr/lib/x86_64-linux-gnu -lSDL2 -lpthread -Wl,--no-undefined -lm -ldl -lasound -lm -ldl -lpthread -lpulse-simple -lpulse -lX11 -lXext -lXcursor -lXinerama -lXi -lXrandr -lXss -lXxf86vm -lwayland-egl -lwayland-client -lwayland-cursor -lxkbcommon -lpthread -lrt -lSDL2_ttf
# -lSDL2_image



.PHONY : all clean cpp_sample

all:
#	echo $(SDL2_LIBS)
	gprbuild -p -P rectangles.gpr -largs $(SDL2_LIBS) 

cpp_sample :
	cd cpp_sample && g++ sdl2.cpp -std=c++1y -lSDL2 -lSDL2_ttf -o ../exe/fonttest 


clean:
	gnatclean -P rectangles.gpr  
