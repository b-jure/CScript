# Makefile for installing CSript

include config.mk

CSCRIPT_A = libcscript.a
CORE_O = src/capi.o src/carray.o src/ccode.o src/cdebug.o src/cfunction.o\
	 src/cgc.o src/ctable.o src/clexer.o src/cmem.o src/cmeta.o\
	 src/cobject.o src/cparser.o src/cvm.o src/cprotected.o src/creader.o\
	 src/cscript.o src/cstate.o src/cstring.o src/ctrace.o
LIB_O = src/cauxlib.o src/cbaselib.o src/cslib.o
BASE_O = $(CORE_O) $(LIB_O) $(MYOBJS)

CSCRIPT_T = cscript
CSCRIPT_O = src/cscript.o

ALL_O= $(BASE_O) $(CSCRIPT_O)
ALL_T= $(CSCRIPT_A) $(CSCRIPT_T)
ALL_A= $(CSCRIPT_A)


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
	$(MAKE) install INSTALL_ROOT=../install

# make may get confused with install/ if it does not support .PHONY.
dummy:

# Echo all config parameters.
echo: 	buildecho
	@echo "PLATFORM = $(PLATFORM)"
	@echo "V = $V"
	@echo "R = $R"
	@echo "TO_BIN = $(TO_BIN)"
	@echo "TO_INC = $(TO_INC)"
	@echo "TO_LIB = $(TO_LIB)"
	@echo "TO_MAN = $(TO_MAN)"
	@echo "INSTALL_ROOT = $(INSTALL_ROOT)"
	@echo "INSTALL_BIN = $(INSTALL_BIN)"
	@echo "INSTALL_INC = $(INSTALL_INC)"
	@echo "INSTALL_LIB = $(INSTALL_LIB)"
	@echo "INSTALL_MAN = $(INSTALL_MAN)"
	@echo "INSTALL_CSMOD = $(INSTALL_CSMOD)"
	@echo "INSTALL_CMOD = $(INSTALL_CMOD)"
	@echo "INSTALL_EXEC = $(INSTALL_EXEC)"
	@echo "INSTALL_DATA = $(INSTALL_DATA)"

# Echo pkg-config data.
pc:
	@echo "version = $R"
	@echo "prefix = $(INSTALL_ROOT)"
	@echo "libdir = $(INSTALL_LIB)"
	@echo "includedir = $(INSTALL_INC)"

# Targets that do not create files
.PHONY: all $(PLATFORMS) help test clean default install uninstall local dummy\
	echo pc o a depend buildecho


# DO NOT MODIFY

capi.o: src/capi.c src/carray.h src/cobject.h src/cscript.h src/csconf.h \
 src/climits.h src/cdebug.h src/cstate.h src/cfunction.h src/ccode.h \
 src/cbits.h src/cparser.h src/clexer.h src/creader.h src/cmem.h \
 src/cgc.h src/cmeta.h src/cprotected.h src/ctable.h src/cstring.h \
 src/cvm.h src/capi.h src/ctrace.h
carray.o: src/carray.c src/cdebug.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cstate.h src/carray.h src/cgc.h \
 src/cbits.h src/cmem.h
cauxlib.o: src/cauxlib.c src/cauxlib.h src/cscript.h src/csconf.h
cbaselib.o: src/cbaselib.c src/cauxlib.h src/cscript.h src/csconf.h
ccode.o: src/ccode.c src/ccode.h src/cbits.h src/cparser.h src/clexer.h \
 src/creader.h src/cscript.h src/csconf.h src/cmem.h src/climits.h \
 src/cobject.h src/ctable.h src/cdebug.h src/cstate.h src/cvm.h \
 src/cgc.h
cdebug.o: src/cdebug.c src/cdebug.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cstate.h src/capi.h src/ccode.h \
 src/cbits.h src/cparser.h src/clexer.h src/creader.h src/cmem.h \
 src/cfunction.h src/cstring.h src/cprotected.h src/cmeta.h src/cvm.h \
 src/ctrace.h src/cgc.h
cfunction.o: src/cfunction.c src/cfunction.h src/ccode.h src/cbits.h \
 src/cparser.h src/clexer.h src/creader.h src/cscript.h src/csconf.h \
 src/cmem.h src/climits.h src/cobject.h src/cstate.h src/cdebug.h \
 src/cgc.h src/cmeta.h src/cvm.h
cgc.o: src/cgc.c src/cgc.h src/cbits.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cstate.h src/carray.h src/cfunction.h \
 src/ccode.h src/cparser.h src/clexer.h src/creader.h src/cmem.h \
 src/cmeta.h src/ctable.h src/cstring.h src/cvm.h src/cprotected.h
ctable.o: src/ctable.c src/cstring.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cstate.h src/ctable.h src/cbits.h \
 src/cgc.h src/cmem.h src/cdebug.h
clexer.o: src/clexer.c src/cmeta.h src/csconf.h src/cscript.h \
 src/cobject.h src/climits.h src/ctypes.h src/cgc.h src/cbits.h \
 src/cstate.h src/clexer.h src/creader.h src/cmem.h src/cdebug.h \
 src/cprotected.h src/ctable.h src/cstring.h
cmem.o: src/cmem.c src/cgc.h src/cbits.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cstate.h src/cdebug.h src/cmem.h \
 src/cprotected.h src/creader.h
cmeta.o: src/cmeta.c src/cmeta.h src/csconf.h src/cscript.h src/cobject.h \
 src/climits.h src/clexer.h src/creader.h src/cmem.h src/cstring.h \
 src/cstate.h src/cdebug.h src/ctable.h src/cbits.h src/cgc.h \
 src/cvm.h
cobject.o: src/cobject.c src/climits.h src/cscript.h src/csconf.h \
 src/cobject.h src/cvm.h src/cstate.h
cparser.o: src/cparser.c src/ccode.h src/cbits.h src/cparser.h \
 src/clexer.h src/creader.h src/cscript.h src/csconf.h src/cmem.h \
 src/climits.h src/cobject.h src/cgc.h src/cstate.h src/cstring.h \
 src/ctrace.h src/cvm.h src/cfunction.h src/ctable.h
cprotected.o: src/cprotected.c src/cprotected.h src/creader.h \
 src/cscript.h src/csconf.h src/cmem.h src/climits.h src/cparser.h \
 src/clexer.h src/cobject.h src/cfunction.h src/ccode.h src/cbits.h \
 src/cstate.h src/cgc.h src/ctrace.h
creader.o: src/creader.c src/creader.h src/cscript.h src/csconf.h \
 src/cmem.h src/climits.h
cscript.o: src/cscript.c src/cscript.h src/csconf.h src/cauxlib.h \
 src/cslib.h
cslib.o: src/cslib.c src/cslib.h src/cscript.h src/csconf.h src/cauxlib.h
cstate.o: src/cstate.c src/ctable.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cbits.h src/carray.h src/cstate.h \
 src/capi.h src/cdebug.h src/cfunction.h src/ccode.h src/cparser.h \
 src/clexer.h src/creader.h src/cmem.h src/cgc.h src/cmeta.h \
 src/cprotected.h src/cstring.h src/ctrace.h
cstring.o: src/cstring.c src/cstate.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cstring.h src/cgc.h src/cbits.h \
 src/ctypes.h src/cdebug.h src/cmem.h src/cvm.h src/cprotected.h \
 src/creader.h
ctrace.o: src/ctrace.c src/ctrace.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cmeta.h src/ccode.h src/cbits.h \
 src/cparser.h src/clexer.h src/creader.h src/cmem.h src/cdebug.h \
 src/cstate.h src/cstring.h
cvm.o: src/cvm.c src/capi.h src/carray.h src/cobject.h src/cscript.h \
 src/csconf.h src/climits.h src/cfunction.h src/ccode.h src/cbits.h \
 src/cparser.h src/clexer.h src/creader.h src/cmem.h src/cstate.h \
 src/cgc.h src/ctable.h src/cdebug.h src/cvm.h src/cmeta.h \
 src/cstring.h src/ctrace.h src/cjmptable.h
