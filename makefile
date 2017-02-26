# ------------------------------------------------------------------------
# Makefile
# ------------------------------------------------------------------------
# Based in the Almake project as used in KOF91 V1.49.
# Visit http://almake.sf.net/ for almake information.
# Visit http://kof91.sf.net/  for KOF91 information.

# This file defines the tarjet platform, and is modified by fix.bat or
# fix.sh script. See it's sources.
include target.os

# Suggested by "GNU Coding Stardards"
SHELL = /bin/sh

# ===============================================
# Project name
PROJECT = Z80 Pascal

# ===============================================

# --------------------------------------
# -- Platform dependent configuration --
# --------------------------------------

# ------------------
# DOS
# ------------------
ifeq ($(TARGET),DOS)
	# Platform name
	PLATFORM=DOS
	# Binary sufix
	BINSUF = .exe
	# Extra flags
	EFLAGS = 

	# File management.
	DELETE = del
	COPY   = xcopy
endif

# ------------------
# Windows
# ------------------
ifeq ($(TARGET),WIN)
	# Platform name
	PLATFORM=Windows
	# Binary sufix
	BINSUF = .exe
	# Extra flags.
	EFLAGS = -WG

	# File management
	# TODO: Detect MSys, Cywing and such...
	DELETE = del
	COPY   = copy
endif

# ------------------
# Linux
# ------------------
ifeq ($(TARGET),LINUX)
	# Platform name
	PLATFORM=GNU/Linux
	# Binary sufix
	BINSUF = 
	# Extra flags.
	EFLAGS = 

	# File management
	DELETE = rm -rf
	COPY   = cp
endif



# ----------------------------
# -- Optimization specifics --
# ----------------------------

# Optimization options, including "smart linking".
OPTOPT = -O3 -Xs -XX

# Next can be used to optimize for almost any current 32bit PC with Linux or
# Windows, doing it pretty well.  Of course, it will prevent your executable to
# run in anything older than PentiumIII.
# OPTOPT += -CpPENTIUM3

# Next one can be used to optimize for 64bit PC with Linux or Windows.
# OPTOPT += -CpATHLON64



# ---------------------
# -- Debug specifics --
# ---------------------

# Debugging opetions.
# Not only adds GDB information to the executable, but also tells the compiler
# to show ALL warnings and hints.
DBGOPT = -O- -gl -vh -vw



# --------------------------
# -- No platform specific --
# --------------------------

# Binary/executable name
BINARY = z80pas

# Sufix for main unit.  See "makefile.list" and "makefile.all".
MAINSUF = .pp

# Directories
SRCDIR = src/compiler/
OBJDIR = obj/compiler/
BINDIR = bin/
DOCDIR = docs/src/

PFLAGS = -Mobjfpc -Sh -Si

# Optimized compilation
# FLAGS = $(OPTOPT) $(PFLAGS) $(EFLAGS)
# Use next line instead to activate debug.
FLAGS = $(DBGOPT) $(PFLAGS) $(EFLAGS)

# -- Source files list --
include makefile.list

# -- Build rules  --
include makefile.all

