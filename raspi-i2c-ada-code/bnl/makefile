
all:

all: test_i2c

i2c_wrapper.ads: i2c_wrapper.c
	rm -f autogen/* ;\
	cp i2c_wrapper.c autogen/ ; \
	cd autogen ; \
	gnatgcc -c -fdump-ada-spec -C i2c_wrapper.c \
	rm -f autogen/i2c_wrapper.c ;\

test_i2c : test_i2c.adb i2c_wrapper.ads
	gprbuild -p -P i2c.gpr


.PHONY: all