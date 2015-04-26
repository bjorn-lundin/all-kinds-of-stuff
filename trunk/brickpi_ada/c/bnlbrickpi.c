/*
*  Jaikrishna
*  t.s.jaikrishna<at>gmail.com
*  Initial date: June 20, 2013
*  Updated:  Feb 17, 2015 (John)
*  Based on Matthew Richardson's Example for testing BrickPi
*  You may use this code as you wish, provided you give credit where it's due.
*
*  This is a program for testing the RPi BrickPi driver with Lego Motor on Port1
*/

#include <stdio.h>
#include <math.h>
#include <time.h>

#include "tick.h"
#include "BrickPi.h"
 
// #include <linux/i2c-dev.h>  
// #include <fcntl.h>

// Compile Using:
//  gcc -c brickpi.c -lrt -lm
// Run the compiled program using:
// sudo ./program

int bnlmain() {
  return 0;
}

struct BrickPiStruct *BrickPiPointer = &BrickPi; 

struct BrickPiStruct * GetPointerToBrickPi(){
  return BrickPiPointer;
}
