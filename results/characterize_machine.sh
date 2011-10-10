#!/bin/bash

lstopo -v -s  > topology.txt

if [ ! -f "/proc/cpuinfo" ]; then exit 0; fi 

rm -rf system_details
mkdir  system_details
cd     system_details

cp -f /proc/cpuinfo cpuinfo.txt

SYS=name_and_version.txt
uname -a          > $SYS
cat /etc/issue   >> $SYS

lstopo -v     > topology.verbose.txt

lspci > lspci.txt
lsusb > lsusb.txt
lsmod > lsmod.txt

# Sudo produces much better output in this case:
LSHW=`which lshw`
if [ "$LSHW" != "" ]; then 
  lshw  > lshw.txt
  sudo lshw  > lshw_sudo.txt
  mv -f lshw_sudo.txt lshw.txt
fi

cd ..

# echo         >> $SYS; 
# echo "lspci" >> $SYS
# echo "================================================================================" >> $SYS
# lspci        >> $SYS

# I think the rest of these are unnecessary if we use lshw:
# lsusb, lsmod, lspci
# 
