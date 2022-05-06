
SHELL := /bin/bash

export CPU := $(shell uname -m)
export OS := $(shell uname)


.PHONYE :all clean test
all:
	gprbuild -p -P telescope.gpr
clean:
	gprclean -P telescope.gpr
test:
	cd bin && sudo ./telescope
