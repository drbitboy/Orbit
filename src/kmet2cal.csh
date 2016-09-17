#!/bin/csh
unset echo

set one="$1"
if ( "$one" == "") set one="-"

set pidfil=kmet2cal.inp.$$

echo ' leading space for default .tls' >! $pidfil
echo ' leading space for default .tsc' >>! $pidfil
echo ' leading space to switch to kMET inputs' >>! $pidfil

cat $pidfil $one | kmet2cal | grep -v '^ *Enter'

\rm -f $pidfil
