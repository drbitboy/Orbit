#!/bin/csh

testorbdb -nxxx -t10
testorbdb -nyyy -t10 -o1000000
\rm -f zzz_??t.orbdb

set all3=( xxx yyy zzz ) 


orbdbMrg zzz xxx yyy

foreach cmd ( orbdbck orbdbDump )
  foreach i ( $all3 )
    echo "******* $cmd $i *******"
    $cmd $i
  end
end
