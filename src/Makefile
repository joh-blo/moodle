include ../vsn.mk

# ----------------------------------------------------
# VARIABLES
# ----------------------------------------------------

APP_NAME = moodle

MODULES = \
	moodle \
	moodle_app \
	moodle_sup \
	moodle_manager \
	moodle_alarm \
	moodle_lib


TARGET_FILES =$(EMAKE_TARGET) $(APP_TARGET)

# Erlang include directories. 
INCLUDES = \
	../include \


include ../../meadow/priv/Makefile.vars

# ----------------------------------------------------
# BUILD TARGETS
# ----------------------------------------------------

all: $(TARGET_FILES) 
	erl -make

clean:
	rm -f $(APP_TARGET) $(EMAKE_TARGET) ../ebin/*.beam

doc: edoc

include ../../meadow/priv/Makefile.rules

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
