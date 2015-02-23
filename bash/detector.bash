#!/bin/bash
PIR_PIN=18
Take_Photo="1" 
while true ; do
    IS_SET=$(gpio -g read $PIR_PIN)
    HOUR=$(date +%H)

    if [ $IS_SET == "1" ] ; then        
        TIME_OK=1
        case "$HOUR" in
          00) TIME_OK=0;;
          01) TIME_OK=0;;
          02) TIME_OK=0;;
          03) TIME_OK=0;;
          04) TIME_OK=0;;
          05) TIME_OK=0;;
          06) TIME_OK=0;;
          07) TIME_OK=0;;
          08) TIME_OK=0;;
        
          18) TIME_OK=0;;
          19) TIME_OK=0;;
          20) TIME_OK=0;;
          21) TIME_OK=0;;
          21) TIME_OK=0;;
          23) TIME_OK=0;;
        esac      
        if [ $TIME_OK == "1" ] ; then    
            echo "PIR ALARM!"
            if [ $Take_Photo == "1" ] ; then
                echo "will take photo now!"
                FILENAME=$(date +%Y_%m_%d_%H_%M_%S)
                sleep 1
                raspistill --nopreview -vf -hf -o /tmp/${FILENAME}.jpg
                sleep 3
                Take_Photo="0"
            fi
        else
          echo "Bad Time! $HOUR - only 09-17 is allowed"
        fi        
    else
        Take_Photo="1" 
        echo "nothing, pin not affected! $(date +%H:%M:%S)"
    fi
    sleep 1
done

