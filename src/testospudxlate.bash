#!/bin/bash

spudxlate="`basename $0`"
spudxlate="${spudxlate#test}"
spudxlate="${spudxlate%.bash}"
spudxlate="${spudxlate%.sh}"
spudxlate="`dirname $0`/$spudxlate"

modone=temporary_file_testxlate1.shape
modtwo=temporary_file_testxlate2.shape
modtre=temporary_file_testxlate3.shape
modzero=temporary_file_testxlate0.shape

$spudxlate $1 $2 10.3 10.3  > $modone
$spudxlate $modone $2 85.3 170.3  > $modtwo
$spudxlate $modtwo $2 -10.3 190.3  > $modtre
$spudxlate $modtre $2 -85.3 350.3  > $modzero

diff $1 $modzero -y --supp -W 100 \
| awk '

BEGIN {
  sumerr=0.0+0.0
  sumfracerr=0.0+0.0
  maxerr=0.0+0.0
  maxfracerr=0.0+0.0
  nl=0
}

($3+0.0)==0.0 { next }

{
  ++nl
  err=((0.0 + $3) - $7)
  if ( err<0.0) err= 0.0-err

  sumerr += err

  if ( err > maxerr) maxerr=(err+0.0)

  fracerr = err / ($3 + 0.0)
  sumfracerr += fracerr
  if ( fracerr > maxfracerr) maxfracerr=fracerr
} 

END{
  print "abs. errors:  avg,max=", sumerr/(nl+0.000001) ",", maxerr "km"
  print "frac errors:  avg,max=", sumfracerr/(nl+0.000001) ",", (maxfracerr+0.0)*100.0 "%"
} 
'


\rm -f $modone $modezero
