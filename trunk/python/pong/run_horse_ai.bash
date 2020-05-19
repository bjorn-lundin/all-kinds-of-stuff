#!/bin/bash

DO_EXIT=0
ONCE=0

function finish {
  DO_EXIT=1
  echo "caught a signal"
  kill $(cat pid_bash.dat)
}
trap finish SIGHUP SIGINT SIGTERM


echo $$ > pid_bash.dat

if [ "$1" == "once" ] ; then
  ONCE=1
  echo "running once only"
  sleep 4
fi

i=1

# Recommend syntax for setting an infinite while loop
while :
do
  python3 horse_ai.py > h${i}.log 2>&1
  [ -r h${i}.log.gz ] && rm -f h${i}.log.gz
  gzip h${i}.log
  i=$((i + 1))
  if [ $DO_EXIT -eq 1 ] ; then
    break
  fi

  if [ $i -gt 300 ] ; then
    break
  fi

  if [ $ONCE -eq 1 ] ; then
    break
  fi
done
