#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/leandro/Projects/lazarus/editor/editor
OFS=$IFS
IFS="
"
/usr/bin/ld.bfd -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2     -L. -o /home/leandro/Projects/lazarus/editor/editor -T /home/leandro/Projects/lazarus/editor/link17413.res -e _start
if [ $? != 0 ]; then DoExitLink /home/leandro/Projects/lazarus/editor/editor; fi
IFS=$OFS
