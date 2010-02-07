# ------------------------------------------------------------------------
# Archivo makefile genérico
# ------------------------------------------------------------------------
# Este es un archivo "makefile" diseñado para ajustarse a la mayor cantidad de
# proyectos posibles.  Está basado, principalmente, en el proyecto "almake"
# según la versión que aparece en el proyecto "KOF91 V1.49".
# Visite http://almake.sf.net/ para conocer más cosas sobre "almake".
# Visite http://kof91.sf.net/  para conocer más cosas sobre "KOF91".

# Este archivo define la plataforma de destino, modificado por el guión fix.bat
# o por fix.sh, según lo indicado como parámetro.
include target.os

# Sugerido por "GNU Coding Stardards"
SHELL = /bin/sh

# ===============================================
# Nombre del proyecto
PROJECT = Z80 Pascal

# Nombre del archivo binario sin extensión
BINARY = z80pas

# ===============================================
# ---------------------------------------------
# -- Elementos dependientes de la plataforma --
# ---------------------------------------------

# ------------------
# DJGPP/DOS
# ------------------
ifeq ($(TARGET),DJGPP)
	# Nombre de la plataforma.
	PLATFORM=DOS/DJGPP
	# Sufijo del binario
	BINSUF = .exe
	# Sufijo de los objetos
	OBJSUF = .o

	# Manipulación de archivos.
	DELETE = del
	COPY   = xcopy
endif

# ------------------
# MinGW32/Win32
# ------------------
ifeq ($(TARGET),WIN32)
	# Nombre de la plataforma.
	PLATFORM=Windows
	# Sufijo del binario
	BINSUF = .exe
	OBJSUF = .o
	# Extra flags.
	EFLAGS = -WG

	# Manipulación de archivos.
	DELETE = del
	COPY   = copy
endif

# ------------------
# Linux
# ------------------
ifeq ($(TARGET),LINUX)
	# Nombre de la plataforma.
	PLATFORM=GNU/Linux
	# Sufijo del binario
	BINSUF = 
	OBJSUF = .o
	# Extra flags.
	EFLAGS = 

	# manipulación de archivos.
	DELETE = rm -rf
	COPY   = cp
endif



# ------------------------------------
# -- No específico de la plataforma --
# ------------------------------------

# Sufix for main unit.  See "makefile.list" and "makefile.all".
MAINSUF = .pp

# Directories
SRCDIR = src/compiler/
OBJDIR = obj/compiler/
BINDIR = bin

FLAGS = -Mobjfpc -Nu -O3 -Sh -Si -Xs -XX $(EFLAGS)
# Use next line instead to activate debug.
#FLAGS = -Mobjfpc -g -O- -pg -Sh $(EFLAGS)

# -- La lista de archivos fuente --
include makefile.list

# -- Las normas para construir el proyecto --
include makefile.all

