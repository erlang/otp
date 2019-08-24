#!/bin/sh
#
# sunspeed
#
# Returns CPU speed in Mhz on sun/solaris 5.x & 6.x
#

echo `/usr/sbin/psrinfo -v | sed 's/.* \([0-9]*\)\ MHz.*/\1/;s/.*[^0-9].*//g'` | sed 's/ /+/g'

#