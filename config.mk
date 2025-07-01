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
# -DCSI_ASSERT => Enables all internal asserts inside CScript.
# -DCSI_TRACE_EXEC => Traces bytecode execution (including stack state).
# -DCSI_DISASSEMBLE_BYTECODE => Disassembles precompiled chunks.
# -DEMERGENCYGCTESTS => Forces an emergency collection at every single
# allocation.
# -DHARDMEMTESTS => Forces a full collection at all points where the collector
# can run.
# -DHARDSTACKTESTS => forces a reallocation of the stack at every point where
# the stack can be reallocated.
#
# Address Sanitizer stuff:
# ASAN_OPTIONS => environment variable that holds Address Sanitizer options
# detect_invalid_pointer_pairs=2

CC = gcc
CFLAGS = -std=c99 -Wfatal-errors -Wall -Wextra $(SYSCFLAGS) $(MYCFLAGS)
LDFLAGS = $(SYSLDFLAGS) $(MYLDFLAGS)
LIBS = -lm $(SYSLIBS) $(MYLIBS)

# system flags
SYSCFLAGS =
SYSLDFLAGS =
SYSLIBS =

# Release flags
# MYCFLAGS = -O2 -march=native -fno-stack-protector -fno-common 
# MYLDFLAGS =
# MYLIBS =
# MYOBJS =

# Testing flags
ASANFLAGS = -fsanitize=address -fsanitize=undefined -fsanitize=pointer-subtract -fsanitize=pointer-compare
MYCFLAGS = $(ASANFLAGS) -O0 -g3 -DCS_USE_APICHECK -DCSI_ASSERT
	   #-DCSI_DISASSEMBLE_BYTECODE #-DCSI_TRACE_EXEC
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
