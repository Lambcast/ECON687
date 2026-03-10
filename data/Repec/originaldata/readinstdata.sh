#!/bin/bash
# NOTE : Quote it else use array to avoid problems #

allfiles="inst/*"
destfile="inst.csv"
iname=""

for f in $allfiles
do
  if grep -q 'Primary-Name-English' $f
  then
  	iname=`grep 'Primary-Name-English' $f | sed 's/^.*: //'`
  else 
  	if grep -q 'Primary-Name:' $f
  	then
  		iname=`grep 'Primary-Name:' $f | sed 's/^.*: //'`
  	else
  		iname="NA"
  	fi
  fi
  fn=${f##*/}
  echo "\"${fn%.*}\",\"${iname}\"" >> "$destfile"
done

