# Makefile for installing Tokudae

include config.mk

TOKUDAE_A = libtokudae.a
CORE_O = src/tapi.o src/tlist.o src/tcode.o src/tdebug.o src/tfunction.o\
	 src/tgc.o src/ttable.o src/tlexer.o src/tmem.o src/tmeta.o\
	 src/tobject.o src/tparser.o src/tvm.o src/tprotected.o src/treader.o\
	 src/tokudae.o src/tstate.o src/tstring.o src/ttrace.o
LIB_O = src/tokudaeaux.o src/tbaselib.o src/tloadlib.o src/tokudaelib.o src/tstrlib.o\
	src/tmathlib.o src/tiolib.o src/toslib.o src/treglib.o src/tdblib.o src/tlstlib.o\
	src/tutf8lib.o
BASE_O = $(CORE_O) $(LIB_O) $(MYOBJS)

TOKUDAE_T = tokudae
TOKUDAE_O = src/tokudae.o

ALL_O= $(BASE_O) $(TOKUDAE_O)
ALL_T= $(TOKUDAE_A) $(TOKUDAE_T)
ALL_A= $(TOKUDAE_A)

# What to install.
TO_BIN = tokudae
TO_INC = tokudae.h tokudaeconf.h tokudaelib.h tokudaeaux.h tokudaelimits.h tokudae.hpp
TO_LIB = libtokudae.a
TO_MAN = tokudae.1

default: $(PLATFORM)

all: clean $(ALL_T) 

o: $(ALL_O)

a: $(ALL_A)

$(TOKUDAE_A): $(BASE_O)
	$(AR) $@ $(BASE_O)
	$(RANLIB) $@

$(TOKUDAE_T): $(TOKUDAE_O) $(TOKUDAE_A)
	$(CC) -o $@ $(LDFLAGS) $(TOKUDAE_O) $(TOKUDAE_A) $(LIBS)

test:
	./$(TOKUDAE_T) -v

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
	$(MAKE) $(ALL) CC="xlc" CFLAGS="-O2 -DTOKU_USE_POSIX -DTOKU_USE_DLOPEN" SYSLIBS="-ldl" SYSLDFLAGS="-brtl -bexpall"

bsd:
	$(MAKE) $(ALL) SYSCFLAGS="-DTOKU_USE_POSIX -DTOKU_USE_DLOPEN" SYSLIBS="-Wl,-E"

FreeBSD NetBSD OpenBSD freebsd:
	$(MAKE) $(ALL) SYSCFLAGS="-DTOKU_USE_LINUX -DTOKU_USE_READLINE -I/usr/include/edit" SYSLIBS="-Wl,-E -ledit" CC="cc"

generic: $(ALL)

ios:
	$(MAKE) $(ALL) SYSCFLAGS="-DTOKU_USE_IOS"

Linux linux: linux-noreadline

linux-noreadline:
	$(MAKE) $(ALL) SYSLIBS="-Wl,-E"

linux-readline: 	clean
	$(MAKE) $(ALL) SYSCFLAGS="-DTOKU_USE_LINUX -DTOKU_USE_READLINE" SYSLIBS="-Wl,-E -ldl -lreadline"

Darwin macos macosx:
	$(MAKE) $(ALL) SYSCFLAGS="-DTOKU_USE_MACOSX -DTOKU_USE_READLINE" SYSLIBS="-lreadline"

mingw:
	$(MAKE) "TOKUDAE_A=tokudae1.dll" "TOKUDAE_T=tokudae.exe" \
	"AR=$(CC) -shared -o" "RANLIB=strip --strip-unneeded" \
	"SYSCFLAGS=-DTOKU_BUILD_AS_DLL" "SYSLIBS=" "SYSLDFLAGS=-s" tokudae.exe

posix:
	$(MAKE) $(ALL) SYSCFLAGS="-DTOKU_USE_POSIX"

SunOS solaris:
	$(MAKE) $(ALL) SYSCFLAGS="-DTOKU_USE_POSIX -DTOKU_USE_DLOPEN -D_REENTRANT" SYSLIBS="-ldl"


install: dummy
	cd src && $(MKDIR) $(INSTALL_BIN) $(INSTALL_INC) $(INSTALL_LIB) \
		  $(INSTALL_MAN) $(INSTALL_TMOD) $(INSTALL_CMOD)
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
echo: buildecho
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
	@echo "INSTALL_TMOD = $(INSTALL_TMOD)"
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
	$(CC) $(CFLAGS) $(CMCFLAGS) -c tlexer.c

cparser.o:
	$(CC) $(CFLAGS) $(CMCFLAGS) -c tparser.c

ccode.o:
	$(CC) $(CFLAGS) $(CMCFLAGS) -c tcode.c

# DO NOT DELETE
