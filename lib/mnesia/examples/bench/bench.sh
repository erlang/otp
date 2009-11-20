#!/bin/sh
# Author  : Hakan Mattsson <hakan@cslab.ericsson.se>
# Purpose : Simplify benchmark execution
# Created : 21 Jun 2001 by Hakan Mattsson <hakan@cslab.ericsson.se>
######################################################################

args="-pa .. -boot start_sasl -sasl errlog_type error -sname bench"
set -x

if [ $# -eq 0 ] ; then
    
    erl $args

else

  while [ $# -gt 0 ]; do

    erl $args -s bench run $1 -s erlang halt
    shift
  done

fi

