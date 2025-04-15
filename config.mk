# Configuration file for building and installing CScript


# {=========================================================================
# 			    Version and release
# ==========================================================================
V = 1.0
R = $V.0
# }=========================================================================


# {=========================================================================
# 				Paths & Installing
# ==========================================================================
# Your platform (see PLATFORMS for possible values).
PLATFORM = guess
PLATFORMS = guess aix bsd freebsd ios macosx posix solaris linux mingw generic 

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
# }=========================================================================


# {=========================================================================
# 			Compiler and Linker Flags
# ==========================================================================
# Internal defines used for testing (all of these slow down operations a lot):
# -DCSI_ASSERT => enables internal asserts
# -DCSI_TRACE_EXEC => traces bytecode execution (stack and instructions)
# -DCSI_DISASSEMBLE_BYTECODE => disassembles and outputs compiled chunks

CC = gcc -std=c99
OPTS = -O0
CFLAGS = -Wall -Wextra $(OPTS) $(SYSCFLAGS) $(MYCFLAGS)
LDFLAGS = $(SYSLDFLAGS) $(MYLDFLAGS)
LIBS = -lm $(SYSLIBS) $(MYLIBS)

# system flags
SYSCFLAGS =
SYSLDFLAGS =
SYSLIBS =

# Flags for production
# MYCFLAGS = -DCS_USE_APICHECK
# MYLDFLAGS =
# MYLIBS =
# MYOBJS =

# Flags for testing
ASANFLAGS = -fsanitize=address -fsanitize=undefined
MYCFLAGS = $(ASANFLAGS) -ggdb -DCS_USE_APICHECK -DCSI_ASSERT
	   #-DCSI_TRACE_EXEC #-DCSI_DISASSEMBLE_BYTECODE
MYLDFLAGS = $(ASANFLAGS)
MYLIBS =
MYOBJS =

# Special flags for compiler modules; -Os reduces code size.
CMCFLAGS= 
# }=========================================================================


# {=========================================================================
# 			Archiver and Other Utilities
# ==========================================================================
AR = ar rcu
RANLIB = ranlib
RM = rm -f
MKDIR = mkdir -p
UNAME = uname
# }=========================================================================
