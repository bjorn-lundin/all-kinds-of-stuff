#!/bin/bash

DO_EXIT=0

function finish {
  DO_EXIT=1
  echo "\ncaught a signal"
}
trap finish SIGHUP SIGINT SIGTERM

# Recommend syntax for setting an infinite while loop

i=1
while :
do
  python3 horse_ai.py > h${i}.log 2>&1
  gzip h${i}.log
#  sleep 5
  i=$((i + 1)) 
  if [ $DO_EXIT -eq 1 ] ; then
    break
  fi

  if [ $i -gt 100 ] ; then
    break
  fi

done
