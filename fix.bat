@echo off

rem Sets makefile source code for the different platforms
rem Based on fix.bat of Allegro.
rem Modified By Kronoman - In loving memory of my father.

if [%1] == [linux]   goto linux
if [%1] == [djgpp]   goto djgpp
if [%1] == [mingw32] goto mingw32
goto help


:djgpp
echo Configuring for DOS/djgpp...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=DJGPP>> target.os
goto done


:mingw32
echo Configuring for Windows/Mingw32...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=MINGW32>> target.os
goto done


:linux
echo Configuring for GNU/Linux...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=LINUX>> target.os
goto done


:help
echo Usage: fix platform
echo.
echo Where platform is one of: djgpp, mingw32 or linux. 
echo.
goto end

:done
echo Done!

:end
