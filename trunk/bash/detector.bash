#!/bin/bash

PIR_PIN=18
Take_Photo="1" 

while true ; do
    IS_SET=$(gpio -g read $PIR_PIN)

    if [ $IS_SET == "1" ] ; then
        echo "PIR ALARM!"
        if [ $Take_Photo == "1" ] ; then
            echo "will take photo now!"
            FILENAME=$(date +%Y_%m_%d_%H_%M_%S)
            sleep 2
            raspistill --nopreview -vf -hf -o /tmp/${FILENAME}.jpg
            sleep 3
            Take_Photo="0"
        fi
    else
        Take_Photo="1" 
        echo "nothing! $(date)"
    fi
    sleep 1
done

