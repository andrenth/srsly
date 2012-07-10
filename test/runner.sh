#!/bin/sh

dir=`dirname $0`
for i in $dir/test-*.sh; do
  sh $i
  if [ $? = 0 ]; then
    echo "`basename $i`: OK"
  else
    echo "`basename $i`: FAIL"
  fi
done
