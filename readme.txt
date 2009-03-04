  Please, visit http://z80-pascal.sourceforge.net/ before to follow.

  After some testing I've decided to follow "Let's Build a Compiler!" articles
  by Jack W. Creenshaw <http://compilers.iecc.com/crenshaw/>.

  At the moment it doesn't do nothing. u_u"

HOW  TO COMPILE
===============

  Current version are for Free Pascal and Lazarus only.

  First run the "fix" script to define the platform objective, then run the
  makefile.

  For the IDE, open the "z80pide.lpr" file at z80-pascal/src/ide with Lazarus,
  check the project options and the compiler options to be sure the directory
  paths weren't twisted and compile.

HACKING
=======

  See the docs/internal directory for several documents about internal
  definitions, including a brief UML description of the objects (You'll need
  GNU's Dia to load this one).
