#!/bin/csh

if ( "$1" == "" ) then
  check.csh xxx |grep -v '\.o:'|grep -v ' *0 *0 *0'
  exit
endif

foreach i ( * )
  echo -n  ${i}:"  "
  if ( -f ../src/$i ) then
    diff $i ../src/$i | wc
  else
    echo ../src/$i does not exist
  endif
end

foreach i ( ../src/* )
  echo -n  ${i}:"  "
  if ( -f $i:t ) then
    diff $i $i:t | wc
  else
    echo $i:t does not exist
  endif
end

