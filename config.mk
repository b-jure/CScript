# Configuration file for building and installing CScript

# { -------------------------------------------------------------------------
# 			    Version and release
V = 1.0
R = $V.0
# } -------------------------------------------------------------------------


# { -------------------------------------------------------------------------
# 				Paths & Installing

PLATFORMS = linux mingw generic guess 	# list of all supported platforms
PLATFORM = guess 			# target platform

# Install paths
ROOT_PATH = /usr/local
INSTALL_BIN = $(ROOT_PATH)/bin
INSTALL_INC = $(ROOT_PATH)/include
INSTALL_LIB = $(ROOT_PATH)/lib
INSTALL_MAN = $(ROOT_PATH)/man/man1
INSTALL_CSMOD = $(ROOT_PATH)/share/cscript/$V
INSTALL_CMOD = $(ROOT_PATH)/lib/cscript/$V

# Install tool
INSTALL = install -p
INSTALL_EXEC = $(INSTALL) -m 0755
INSTALL_DATA = $(INSTALL) -m 0644
#
# If you don't have "install" you can use "cp" instead.
# INSTALL= cp -p
# INSTALL_EXEC= $(INSTALL)
# INSTALL_DATA= $(INSTALL)
# } -------------------------------------------------------------------------


# { -------------------------------------------------------------------------
# 			Compiler and Linker Flags 
CC = gcc -std=c99
OPTS = -O2
CFLAGS = -Wpedantic -Wall -Wextra ${OPTS} ${SYSCFLAGS} ${MYCFLAGS}
LDFLAGS = ${SYSLDFLAGS} ${MYLDFLAGS}
LIBS = -lm ${SYSLIBS} ${MYLIBS}

# system flags
SYSCFLAGS =
SYSLDFLAGS =
SYSLIBS =

# user flags
MYCFLAGS = -fsanitize=address -fsanitize=undefined -ggdb
MYLDFLAGS = -fsanitize=address -fsanitize=undefined
MYLIBS =
MYOBJS =

# special flags for compiler modules
CMCFLAGS =
# } -------------------------------------------------------------------------


# { -------------------------------------------------------------------------
# 		   	Archiver and Other Utilities
AR = ar rcu
RANLIB = ranlib
RM = rm -f
MKDIR = mkdir -p
UNAME = uname
# } -------------------------------------------------------------------------
