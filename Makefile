# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include vsn.mk
VSN = $(MOODLE_VSN)

all:
	cd src; make all

clean:
	cd src; make clean
