#!/bin/sh
#
# Sets makefile source code for the different platforms
# Based on fix.sh of Allegro.
# Modified By Kronoman - In loving memory of my father.


proc_help()
{
   echo "Usage: fix platform"
   echo
   echo "Where platform is one of: djgpp, mingw32 or linux. "
   echo
   echo
}

proc_fix()
{
   echo "Configuring for $1..."

   if [ "$2" != "none" ]; then
      echo "# Warning! This file will be overwritten by configuration routines!" > target.os
      echo "TARGET=$2" >> target.os
   fi
}


# prepare for the given platform.

case "$1" in
   "djgpp"   ) proc_fix "DOS"       "DJGPP";;
   "mingw32" ) proc_fix "Windows" "MINGW32";;
   "linux"   ) proc_fix "GNU/Linux"       "LINUX";;
   "help"    ) proc_help;;
   *         ) proc_help;;
esac

echo "Done!"
