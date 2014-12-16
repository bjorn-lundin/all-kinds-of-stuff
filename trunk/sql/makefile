
PATH_TO_LIBPQ := C:/bnl/tools/pgadmin/1.18/1.18
.PHONY : all clean

all : sql_test.adb
	gnatmake -gnat12 sql_test.adb -largs -L$(PATH_TO_LIBPQ) -lpq
    
clean :
	rm -f *.o *.exe *.ali
    