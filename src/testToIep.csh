#!/bin/csh

unset echo

set path=( "`dirname $0`" $path )

if ( "$1" == "" ) then
  echo Usage:  $0 '<framesFile>'
  exit
endif

if ( ! -f "$1" ) then
  echo Usage:  $0 '<framesFile>'
  exit
endif

if ( "`grep :alt.et $1 | wc -l | tr -d ' '`" == "0" ) then
  echo Usage:  $0 '<framesFile>'
  echo file $1 does not have any "':alt et'" lines in it
  exit
endif

set plates="`grep '^PLATES:' $1 | sed -e 's/^PLATES: *//' -e 's/ .*//'`"
if ( -f "$plates" ) then
  set plates=( -plates "$plates" )
else
  set plates=""
endif

set spud="`grep '^SPUD:' $1 | sed -e 's/^SPUD: *//' -e 's/ .*//'`"
if ( -f "$spud" ) then
  set spud=( -spud "$spud" )
else
  set spud=""
endif

cat "$1" | \
  frm2iep -rigor $spud $plates > "$1".iep.fromframes

grep :alt.et "$1" | \
  frm2iep -rigor -intype MET -timecol 2 -spicespec "$1" | \
  grep -v '^[A-Z][A-Z]*:' > "$1".iep.fromtimes


compareIep "$1".iep.fromframes "$1".iep.fromtimes | more


