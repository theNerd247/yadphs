##############################
# @file Makefile for yadp
##############################
#
## PROJECT ##############################

SHELL=/bin/sh
PROJECT=yadp
VERSION=0.1

## END PROJECT ############################
#
## DIRS ##############################

PREFIX=.
BINDIR=../bin
IDIR=$(PREFIX)/include
SRCDIR=$(PREFIX)/src
LIBDIR=$(PREFIX)/lib
CLEAN=$(LIBDIR) 
REQUIRED_DIRS=$(BINDIR) $(LIBDIR)

SRCTYPE=hs
OBJTYPE=o

## END DIRS ############################
#
## FILES ##############################
DOXYGEN_CONFIG_FILE=$(PREFIX)/doxygen.conf
## END FILES ############################
#
## FLAGS ##############################

LLIBS=
LLIBS:=$(patsubst %, -l%, $(LLIBS))

export CC=ghc
export CFLAGS=
export LFLAGS=$(LLIBS) 

SOFLAGS=

## END FLAGS ############################
#
## OBJECTS ##############################

SRCS=$(SRCDIR)/*.$(SRCTYPE)
OBJS:=$(patsubst %.$(SRCTYPE), %.$(OBJTYPE), $(wildcard $(SRCDIR)/*.$(SRCTYPE)))
LIBOBJS=
LIBOBJS:=$(patsubst %.$(SRCTYPE), $(SRCDIR)/%.$(OBJTYPE), $(LIBOBJS))
LIBOBJECTS=$(filter $(LIBOBJS), $(OBJS))
LIBNAME=$(PROJECT)
OBJECTS:=$(filter-out $(LIBOBJECTS), $(OBJS))

## END OBJECTS ############################
#
## TARGETS ##############################

TARGET=$(BINDIR)/$(PROJECT)
LIBTARGET=

.PHONY: all $(TARGET) setup clean package doc

all: setup $(TARGET)

#for debuging this make file
debugmk: 
	@echo $(OBJECTS)
	@echo $(LIBOBJECTS)

#create directories that are needed before building
setup:
	@mkdir -p $(REQUIRED_DIRS)

$(LIBTARGET): $(LIBOBJECTS)
	$(CC) -o $(LIBTARGET) $(LIBOBJECTS) $(SOFLAGS) $(LFLAGS)

$(TARGET):
	cd $(SRCDIR);\
	$(CC) $(PROJECT);\
	mv $(PROJECT) $(TARGET)

#perform a recursive build 
#build: $(SUBDIRS)

#perform a recursive clean
#clean: $(SUBDIRS)
clean: CLEAN += $(OBJECTS) $(LIBOBJECTS)
clean: 
	rm -rf $(CLEAN)

doc:
	doxygen $(DOXYGEN_CONFIG_FILE)

## END TARGETS ############################
