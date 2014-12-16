cc = gcc
exec = k8055
syspref = /usr/local
bindir = $(syspref)/bin
libdir = $(syspref)/lib
pylibdir = pyk8055
includedir = $(syspref)/include
mandir = $(syspref)/man/man1
OBJS = main.o libk8055.o
CFLAGS = -DDAEMON -Wall
libs = -lusb -L/usr/lib -lm

libk8055.so.0.2: libk8055.o
	$(cc) -Wall -o libk8055.so.0.2 -shared libk8055.o

libk8055.o: libk8055.c
	$(cc) -Wall -O -c -g -fPIC libk8055.c
	-ln -sf libk8055.so.0.2 libk8055.so

libk8055.c: k8055.h

all: k8055_prog libk8055.so.0.2

%.o: %.c
	$(cc) $(CFLAGS) -c $<

k8055_prog: $(OBJS)
	$(cc) $(OBJS) -o $(exec) $(libs)
	strip $(exec)
    
pylib: $(pylibdir)/libk8055.i libk8055.c
	@$(MAKE) -C $(pylibdir)

clean:	
	rm -f *.o libk8055.so libk8055.so.0.2 $(exec)
	@$(MAKE) -C $(pylibdir) clean
    
install: k8055_prog libk8055.so.0.2
	cp -f $(exec) $(bindir)/
	if !(test -d $(libdir)); then \
	  mkdir $(libdir); \
	fi
	if !(test -d $(includedir)); then \
	  mkdir $(includedir); \
	fi
	cp -Pf lib* $(libdir)
	cp -f k8055.h $(includedir)
	if !(test -d $(mandir)); then \
	  mkdir $(mandir); \
	fi
	cp -f man/k8055.1.gz $(mandir)/

pyinstall: $(pylibdir)/libk8055.i
	@$(MAKE) -C $(pylibdir) install
    
uninstall:
	rm -f $(bindir)/$(exec) $(libdir)/libk8055* $(includedir)/k8055.h
    
test: k8055_prog
	killall $(exec)
	./$(exec)
