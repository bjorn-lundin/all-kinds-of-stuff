#!/bin/bash


act_func_list="soft"
mtype_list="plc win"
learning_rate_list="0.005 0.01 0.015"
hidden_nodes_list="100 200 500 1000"
epoch_list="1 2 4 8 16"


for af in $act_func_list ; do
  for mt in $mtype_list ; do
    for lr in $learning_rate_list ; do
      for hn in $hidden_nodes_list ; do
        for ep in $epoch_list ; do
          python3 mnst_horses.py $mt $lr $hn $ep $af 2>&1 | grep -v "UserWarning"
        done
      done
    done
  done
done
