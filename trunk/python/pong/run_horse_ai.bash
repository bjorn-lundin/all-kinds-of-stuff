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

bettype=$2
#lr=$3
# Recommend syntax for setting an infinite while loop
while :
do
  logfile=h_one_bet_${bettype}_${lr}_${i}.log
  python3 horses_based_on_pong.py --bettype=${bettype} > log/${logfile} 2>&1
  [ -r log/${logfile}.gz ] && rm -f log/${logfile}.gz
  gzip log/${logfile}
  i=$((i + 1))
  if [ $DO_EXIT -eq 1 ] ; then
    break
  fi

  if [ $i -gt 10 ] ; then
    break
  fi

  if [ $ONCE -eq 1 ] ; then
    break
  fi
done

