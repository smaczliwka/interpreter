#!/bin/bash
FILES="${2}/*.lat"
if [ $# -ne 2 ]; then
  echo "Invalid number of arguments"
else
  for f in ${FILES}
  do
    echo $f
     ./"${1}" <"${f}" > tmp.out 2>tmp.err
    diff -q tmp.out "${f::-4}".out
   diff -q tmp.err "${f::-4}".err
  done
  rm tmp.err
  rm tmp.out
fi