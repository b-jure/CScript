# Makefile for installing CSript

include config.mk

CSCRIPT_A = libcscript.a
CORE_O = src/capi.o src/clist.o src/ccode.o src/cdebug.o src/cfunction.o\
	 src/cgc.o src/ctable.o src/clexer.o src/cmem.o src/cmeta.o\
	 src/cobject.o src/cparser.o src/cvm.o src/cprotected.o src/creader.o\
	 src/cscript.o src/cstate.o src/cstring.o src/ctrace.o
LIB_O = src/cscriptaux.o src/cbaselib.o src/cloadlib.o src/cscriptlib.o src/cstrlib.o\
	src/cmathlib.o src/ciolib.o src/coslib.o src/creglib.o src/cdblib.o src/clstlib.o\
	src/cutf8lib.o
BASE_O = $(CORE_O) $(LIB_O) $(MYOBJS)

CSCRIPT_T = cscript
CSCRIPT_O = src/cscript.o

ALL_O= $(BASE_O) $(CSCRIPT_O)
ALL_T= $(CSCRIPT_A) $(CSCRIPT_T)
ALL_A= $(CSCRIPT_A)

# What to install.
TO_BIN = cscript
TO_INC = cscript.h cscriptconf.h cscriptlib.h cscriptaux.h cscriptlimits.h cscript.hpp
TO_LIB = libcscript.a
TO_MAN = cscript.1

default: 	$(PLATFORM)

all:		$(ALL_T)

o: 		$(ALL_O)

a: 		$(ALL_A)

$(CSCRIPT_A): $(BASE_O)
	$(AR) $@ $(BASE_O)
	$(RANLIB) $@

$(CSCRIPT_T): $(CSCRIPT_O) $(CSCRIPT_A)
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


# Convenience targets for popular platforms.
ALL = all

help:
	@echo "Do 'make PLATFORM' where PLATFORM is one of these:"
	@echo "    $(PLATFORMS)"

guess:
	@echo Guessing `$(UNAME)`
	@$(MAKE) `$(UNAME)`

AIX aix:
	$(MAKE) $(ALL) CC="xlc" CFLAGS="-O2 -DCS_USE_POSIX -DCS_USE_DLOPEN" SYSLIBS="-ldl" SYSLDFLAGS="-brtl -bexpall"

bsd:
	$(MAKE) $(ALL) SYSCFLAGS="-DCS_USE_POSIX -DCS_USE_DLOPEN" SYSLIBS="-Wl,-E"

FreeBSD NetBSD OpenBSD freebsd:
	$(MAKE) $(ALL) SYSCFLAGS="-DCS_USE_LINUX -DCS_USE_READLINE -I/usr/include/edit" SYSLIBS="-Wl,-E -ledit" CC="cc"

generic: $(ALL)

ios:
	$(MAKE) $(ALL) SYSCFLAGS="-DCS_USE_IOS"

Linux linux: linux-noreadline

linux-noreadline:
	$(MAKE) $(ALL) SYSLIBS="-Wl,-E"

linux-readline:
	$(MAKE) $(ALL) SYSCFLAGS="-DCS_USE_LINUX -DCS_USE_READLINE" SYSLIBS="-Wl,-E -ldl -lreadline"

Darwin macos macosx:
	$(MAKE) $(ALL) SYSCFLAGS="-DLUA_USE_MACOSX -DLUA_USE_READLINE" SYSLIBS="-lreadline"

mingw:
	$(MAKE) "CSCRIPT_A=cscript1.dll" "CSCRIPT_T=cscript.exe" \
	"AR=$(CC) -shared -o" "RANLIB=strip --strip-unneeded" \
	"SYSCFLAGS=-DCS_BUILD_AS_DLL" "SYSLIBS=" "SYSLDFLAGS=-s" cscript.exe

posix:
	$(MAKE) $(ALL) SYSCFLAGS="-DCS_USE_POSIX"

SunOS solaris:
	$(MAKE) $(ALL) SYSCFLAGS="-DCS_USE_POSIX -DCS_USE_DLOPEN -D_REENTRANT" SYSLIBS="-ldl"


install: dummy
	cd src && $(MKDIR) $(INSTALL_BIN) $(INSTALL_INC) $(INSTALL_LIB) \
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

# Compiler modules may use special flags.
clexer.o:
	$(CC) $(CFLAGS) $(CMCFLAGS) -c clexer.c

cparser.o:
	$(CC) $(CFLAGS) $(CMCFLAGS) -c cparser.c

ccode.o:
	$(CC) $(CFLAGS) $(CMCFLAGS) -c ccode.c

# DO NOT DELETE
capi.o: src/capi.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/clist.h \
 src/cdebug.h src/cstate.h src/cfunction.h src/ccode.h src/cbits.h \
 src/cparser.h src/clexer.h src/creader.h src/cmem.h src/cgc.h \
 src/cmeta.h src/cprotected.h src/ctable.h src/cstring.h src/cvm.h \
 src/capi.h
cbaselib.o: src/cbaselib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
ccode.o: src/ccode.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/ccode.h \
 src/cbits.h src/cparser.h src/clexer.h src/creader.h src/cmem.h \
 src/ctable.h src/cdebug.h src/cstate.h src/clist.h src/cvm.h src/cgc.h
cdblib.o: src/cdblib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
cdebug.o: src/cdebug.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cdebug.h \
 src/cstate.h src/clist.h src/capi.h src/ccode.h src/cbits.h \
 src/cparser.h src/clexer.h src/creader.h src/cmem.h src/cfunction.h \
 src/cstring.h src/cprotected.h src/cmeta.h src/cvm.h src/cgc.h \
 src/ctable.h
cfunction.o: src/cfunction.c src/cscriptprefix.h src/ctrace.h \
 src/cobject.h src/cscript.h src/cscriptconf.h src/cscriptlimits.h \
 src/cfunction.h src/ccode.h src/cbits.h src/cparser.h src/clexer.h \
 src/creader.h src/cmem.h src/cstate.h src/clist.h src/cdebug.h src/cgc.h \
 src/cmeta.h src/cvm.h src/cprotected.h
cgc.o: src/cgc.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cgc.h \
 src/cbits.h src/cstate.h src/clist.h src/cfunction.h src/ccode.h \
 src/cparser.h src/clexer.h src/creader.h src/cmem.h src/cmeta.h \
 src/ctable.h src/cstring.h src/cvm.h src/cprotected.h
ciolib.o: src/ciolib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
clexer.o: src/clexer.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/ctypes.h \
 src/cgc.h src/cbits.h src/cstate.h src/clist.h src/clexer.h \
 src/creader.h src/cmem.h src/cdebug.h src/cprotected.h src/ctable.h \
 src/cstring.h
clist.o: src/clist.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/clist.h \
 src/cstring.h src/cstate.h src/clexer.h src/creader.h src/cmem.h \
 src/cgc.h src/cbits.h src/cmeta.h src/cdebug.h
cloadlib.o: src/cloadlib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
clstlib.o: src/clstlib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
cmathlib.o: src/cmathlib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
cmem.o: src/cmem.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cgc.h \
 src/cbits.h src/cstate.h src/clist.h src/cdebug.h src/cmem.h \
 src/cprotected.h src/creader.h
cmeta.o: src/cmeta.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cmeta.h \
 src/clist.h src/clexer.h src/creader.h src/cmem.h src/cstring.h \
 src/cstate.h src/cdebug.h src/ctable.h src/cbits.h src/cgc.h src/cvm.h \
 src/cprotected.h
cobject.o: src/cobject.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cvm.h \
 src/cstate.h src/clist.h
coslib.o: src/coslib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
cparser.o: src/cparser.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/ccode.h \
 src/cbits.h src/cparser.h src/clexer.h src/creader.h src/cmem.h \
 src/cfunction.h src/cstate.h src/clist.h src/cgc.h src/cstring.h \
 src/ctable.h src/cvm.h
cprotected.o: src/cprotected.c src/cscriptprefix.h src/ctrace.h \
 src/cobject.h src/cscript.h src/cscriptconf.h src/cscriptlimits.h \
 src/cprotected.h src/creader.h src/cmem.h src/cparser.h src/clexer.h \
 src/cfunction.h src/ccode.h src/cbits.h src/cstate.h src/clist.h \
 src/cstring.h src/cgc.h
creader.o: src/creader.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/creader.h \
 src/cmem.h
creglib.o: src/creglib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cstrlib.h \
 src/cscriptaux.h src/cscriptlib.h
cscriptaux.o: src/cscriptaux.c src/cscriptprefix.h src/ctrace.h \
 src/cobject.h src/cscript.h src/cscriptconf.h src/cscriptlimits.h \
 src/cscriptaux.h
cscript.o: src/cscript.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
cscriptlib.o: src/cscriptlib.c src/cscriptprefix.h src/ctrace.h \
 src/cobject.h src/cscript.h src/cscriptconf.h src/cscriptlimits.h \
 src/cscriptlib.h src/cscriptaux.h
cstate.o: src/cstate.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/ctable.h \
 src/cbits.h src/clist.h src/cstate.h src/capi.h src/cdebug.h \
 src/cfunction.h src/ccode.h src/cparser.h src/clexer.h src/creader.h \
 src/cmem.h src/cgc.h src/cmeta.h src/cprotected.h src/cstring.h
cstring.o: src/cstring.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cstate.h \
 src/clist.h src/cstring.h src/clexer.h src/creader.h src/cmem.h \
 src/cgc.h src/cbits.h src/ctypes.h src/cdebug.h src/cvm.h \
 src/cprotected.h
cstrlib.o: src/cstrlib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cstrlib.h \
 src/cscriptaux.h src/cscriptlib.h
ctable.o: src/ctable.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cstring.h \
 src/cstate.h src/clist.h src/clexer.h src/creader.h src/cmem.h \
 src/ctable.h src/cbits.h src/cgc.h src/cdebug.h
ctrace.o: src/ctrace.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cmeta.h \
 src/ccode.h src/cbits.h src/cparser.h src/clexer.h src/creader.h \
 src/cmem.h src/cdebug.h src/cstate.h src/clist.h src/cstring.h
cutf8lib.o: src/cutf8lib.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/cscriptaux.h \
 src/cscriptlib.h
cvm.o: src/cvm.c src/cscriptprefix.h src/ctrace.h src/cobject.h \
 src/cscript.h src/cscriptconf.h src/cscriptlimits.h src/capi.h \
 src/cstate.h src/clist.h src/cfunction.h src/ccode.h src/cbits.h \
 src/cparser.h src/clexer.h src/creader.h src/cmem.h src/cgc.h \
 src/ctable.h src/cdebug.h src/cvm.h src/cmeta.h src/cstring.h \
 src/cprotected.h src/cjmptable.h
