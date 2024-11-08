# tight - loseless file compression program/library
# See 'tight.h' for copyright and licence details.

include config.mk

CSCRIPT_A = libcscript.a
CORE_O = src/capi.o src/carray.o src/ccode.o src/cdebug.o src/cfunction.o\
	 src/cgc.o src/chashtable.o src/clexer.o src/cmem.o src/cmeta.o\
	 src/cobject.o src/cparser.o src/cvm.o src/cprotected.o src/creader.o\
	 src/cscript.o src/cstate.o src/cstring.o src/ctrace.o
LIB_O = src/cauxlib.o src/ccorelib.o src/cslib.c
BASE_O = $(CORE_O) $(LIB_O) $(MYOBJS)

CSCRIPT_T = cscript		# target
CSCRIPT_O = src/cscript.o	# target object

ALL_O= $(BASE_O) $(CSCRIPT_O)		# all object files
ALL_T= $(CSCRIPT_A) $(CSCRIPT_T)	# all targets
ALL_A= $(CSCRIPT_A)			# all archives


default: 	$(PLATFORM)

all:		$(ALL_T)

o: 		$(ALL_O)

a: 		$(ALL_A)

$(CSCRIPT_A): 	$(BASE_O)
	$(AR) $@ $(BASE_O)
	$(RANLIB) $@

$(CSCRIPT_T): 	$(CSCRIPT_O) $(CSCRIPT_A)
	$(CC) -o $@ $(LDFLAGS) $(CSCRIPT_O) $(CSCRIPT_A) $(LIBS)

test:
	./$(CSCRIPT_T) -v

clean:
	$(RM) $(ALL_T) $(ALL_O)

depend:
	@$(CC) $(CFLAGS) -MM src/c*.c

buildecho:
	@echo "PLATFORM = $(PLATFORM)"
	@echo "CC = $(CC)"
	@echo "CFLAGS = $(CFLAGS)"
	@echo "LDFLAGS = $(LDFLAGS)"
	@echo "LIBS = $(LIBS)"
	@echo "AR = $(AR)"
	@echo "RANLIB = $(RANLIB)"
	@echo "RM = $(RM)"
	@echo "UNAME = $(UNAME)"


ALL = all

help:
	@echo "Do 'make PLATFORM' where PLATFORM is one of these:"
	@echo "    $(PLATFORMS)"

guess:
	@echo Guessing `$(UNAME)`
	@$(MAKE) `$(UNAME)`

generic: $(ALL)

Linux linux:
	$(MAKE) $(ALL) SYSLIBS="-Wl,-E"

mingw:
	$(MAKE) "CSCRIPT_A=cscript1.dll" "CSCRIPT_T=cscript.exe" \
	"AR=$(CC) -shared -o" "RANLIB=strip --strip-unneeded" \
	"SYSCFLAGS=-DCS_BUILD_AS_DLL" "SYSLIBS=" "SYSLDFLAGS=-s" cscript.exe


# What to install.
TO_BIN = cscript
TO_INC = cscript.h csconf.h cslib.h cauxlib.h cscript.hpp
TO_LIB = libcscript.a
TO_MAN = cscript.1

install: dummy
	cd src && $(MKDIR) $(INSTALL_BIN) $(INSTALL_INC) $(INSTALL_LIB)\
		  $(INSTALL_MAN) $(INSTALL_CSMOD) $(INSTALL_CMOD)
	cd src && $(INSTALL_EXEC) $(TO_BIN) $(INSTALL_BIN)
	cd src && $(INSTALL_DATA) $(TO_INC) $(INSTALL_INC)
	cd src && $(INSTALL_DATA) $(TO_LIB) $(INSTALL_LIB)
	cd doc && $(INSTALL_DATA) $(TO_MAN) $(INSTALL_MAN)

uninstall:
	cd src && cd $(INSTALL_BIN) && $(RM) $(TO_BIN)
	cd src && cd $(INSTALL_INC) && $(RM) $(TO_INC)
	cd src && cd $(INSTALL_LIB) && $(RM) $(TO_LIB)
	cd doc && cd $(INSTALL_MAN) && $(RM) $(TO_MAN)

local:
	$(MAKE) install INSTALL_TOP=../install

# make may get confused with install/ if it does not support .PHONY.
dummy:

# Echo all config parameters.
echo: 	buildecho
	@cd src && $(MAKE) -s echo
	@echo "PLAT= $(PLAT)"
	@echo "V= $V"
	@echo "R= $R"
	@echo "TO_BIN= $(TO_BIN)"
	@echo "TO_INC= $(TO_INC)"
	@echo "TO_LIB= $(TO_LIB)"
	@echo "TO_MAN= $(TO_MAN)"
	@echo "INSTALL_TOP= $(INSTALL_TOP)"
	@echo "INSTALL_BIN= $(INSTALL_BIN)"
	@echo "INSTALL_INC= $(INSTALL_INC)"
	@echo "INSTALL_LIB= $(INSTALL_LIB)"
	@echo "INSTALL_MAN= $(INSTALL_MAN)"
	@echo "INSTALL_LMOD= $(INSTALL_LMOD)"
	@echo "INSTALL_CMOD= $(INSTALL_CMOD)"
	@echo "INSTALL_EXEC= $(INSTALL_EXEC)"
	@echo "INSTALL_DATA= $(INSTALL_DATA)"

# Echo pkg-config data.
pc:
	@echo "version=$R"
	@echo "prefix=$(INSTALL_TOP)"
	@echo "libdir=$(INSTALL_LIB)"
	@echo "includedir=$(INSTALL_INC)"

# Targets that do not create files
.PHONY: all $(PLATFORMS) help test clean default install uninstall local dummy\
	echo pc o a depend buildecho
