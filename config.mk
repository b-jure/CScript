# Configuration file for building and installing CScript

# { -------------------------------------------------------------------------
# 			    Version and release
V = 1.0
R = $V.0
# } -------------------------------------------------------------------------


# { -------------------------------------------------------------------------
# 				Paths & Installing
PLATFORMS = linux mingw generic guess
PLATFORM = guess

# Install paths
INSTALL_ROOT = /usr/local
INSTALL_BIN = $(INSTALL_ROOT)/bin
INSTALL_INC = $(INSTALL_ROOT)/include
INSTALL_LIB = $(INSTALL_ROOT)/lib
INSTALL_MAN = $(INSTALL_ROOT)/man/man1
INSTALL_CSMOD = $(INSTALL_ROOT)/share/cscript/$V
INSTALL_CMOD = $(INSTALL_ROOT)/lib/cscript/$V

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
OPTS = -O0
CFLAGS = -Wall -Wextra ${OPTS} ${SYSCFLAGS} ${MYCFLAGS}
LDFLAGS = ${SYSLDFLAGS} ${MYLDFLAGS}
LIBS = -lm ${SYSLIBS} ${MYLIBS}

# system flags
SYSCFLAGS =
SYSLDFLAGS =
SYSLIBS =

# user flags
MYCFLAGS = -fsanitize=address -fsanitize=undefined -ggdb -DCSI_ASSERT -DCSI_TRACE_API -DTRACEMEMORY
MYLDFLAGS = -fsanitize=address -fsanitize=undefined
MYLIBS =
MYOBJS =
# } -------------------------------------------------------------------------


# { -------------------------------------------------------------------------
# 		   	Archiver and Other Utilities
AR = ar rcu
RANLIB = ranlib
RM = rm -f
MKDIR = mkdir -p
UNAME = uname
# } -------------------------------------------------------------------------
