# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include vsn.mk

SUB_DIRECTORIES = src

include ../meadow/priv/Makefile.subdir

install:
	sudo cp -p priv/moodle.service /etc/init.d/moodle
	sudo update-rc.d moodle defaults
