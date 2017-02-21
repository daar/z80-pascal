#!/bin/bash

./pasmo toggle.asm toggle.pasmo

rm z80asmlex.pas
plex z80asmlex.l

z80asm.pas
pyacc z80asm.y

fpc z80asm
./z80asm
