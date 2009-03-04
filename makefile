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

# Directorio de las fuentes
SRCDIR = src/compiler/

# Directorio de los archivos de cabecera
INCDIR = 

# Sufijo de las fuentes (.c, .cpp, .pas, etc).
SRCSUF = .pas

# ---------------------------------------------
# -- Elementos dependientes de la plataforma --
# ---------------------------------------------
#  Aquí se definirá de qué modo se compilará el proyecto.
#  En cierto modo está pensado para C/C++.

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
ifeq ($(TARGET),MINGW32)
	# Nombre de la plataforma.
	PLATFORM=Win32/MinGW32
	# Sufijo del binario
	BINSUF = .exe
	OBJSUF = .o

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

	# manipulación de archivos.
	DELETE = rm -rf
	COPY   = cp
endif



# ------------------------------------
# -- No específico de la plataforma --
# ------------------------------------

OBJDIR = obj/compiler/
BINDIR = bin

FLAGS = -g -O- -pg -Mobjfpc
#FLAGS = -02 -Mobjfpc -Sh

# -- La lista de archivos fuente --
include makefile.list

# -- Las normas para construir el proyecto --
include makefile.all

