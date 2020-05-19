#!/bin/bash

learning_list="0.001"
hidden_nodes="200 800 1200 5000"
epoch_list="16 32 64"
for lr in $learning_list ; do
  for hn in $hidden_nodes ; do
    for ep in $epoch_list ; do
      python3 horseraces.py $lr $hn $ep
    done
  done
done
