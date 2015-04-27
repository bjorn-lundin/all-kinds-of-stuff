/*
*  This is a part of a thin Ada binding to the c-source from Dexter Industries
*  2015-04-27 Bj�rn Lundin
*
*
*  Compile Using:
*  gcc -c -Wall bnlbrickpi.c -o bnlbrickpi.o -L/usr/local/lib -lwiringPi -lm -lrt
*/

#include <stdio.h>
#include <math.h>
#include <time.h>

#include "tick.h"
#include "BrickPi.h"

// so we can use the BrickPi global variable from Ada.
struct BrickPiStruct *BrickPiPointer = &BrickPi;

struct BrickPiStruct * GetPointerToBrickPi(){
  return BrickPiPointer;
}

// use the values from C to makes a constant package
int print_constants() {
  printf("-- Autogenerated file. Do not Edit! Change in bnlbrickpi.c instead. \n\n");
  printf("with Interfaces.C;\n\n");
  printf("package Brickpi.Constants is\n");
  printf("  PORT_A                      : constant Interfaces.C.Int := %d; \n", PORT_A);
  printf("  PORT_B                      : constant Interfaces.C.Int := %d; \n", PORT_B);
  printf("  PORT_C                      : constant Interfaces.C.Int := %d; \n", PORT_C);
  printf("  PORT_D                      : constant Interfaces.C.Int := %d; \n", PORT_D);
  printf("  PORT_1                      : constant Interfaces.C.Int := %d; \n", PORT_1);
  printf("  PORT_2                      : constant Interfaces.C.Int := %d; \n", PORT_2);
  printf("  PORT_3                      : constant Interfaces.C.Int := %d; \n", PORT_3);
  printf("  PORT_4                      : constant Interfaces.C.Int := %d; \n", PORT_4);
  printf("  MASK_D0_M                   : constant Interfaces.C.Int := %d; \n", MASK_D0_M);
  printf("  MASK_D1_M                   : constant Interfaces.C.Int := %d; \n", MASK_D1_M);
  printf("  MASK_9V                     : constant Interfaces.C.Int := %d; \n", MASK_9V);
  printf("  MASK_D0_S                   : constant Interfaces.C.Int := %d; \n", MASK_D0_S);
  printf("  MASK_D1_S                   : constant Interfaces.C.Int := %d; \n", MASK_D1_S);
  printf("  BYTE_MSG_TYPE               : constant Interfaces.C.Int := %d; \n", BYTE_MSG_TYPE);
  printf("  MSG_TYPE_CHANGE_ADDR        : constant Interfaces.C.Int := %d; \n", MSG_TYPE_CHANGE_ADDR);
  printf("  MSG_TYPE_SENSOR_TYPE        : constant Interfaces.C.Int := %d; \n", MSG_TYPE_SENSOR_TYPE);
  printf("  MSG_TYPE_VALUES             : constant Interfaces.C.Int := %d; \n", MSG_TYPE_VALUES);
  printf("  MSG_TYPE_E_STOP             : constant Interfaces.C.Int := %d; \n", MSG_TYPE_E_STOP);
  printf("  MSG_TYPE_TIMEOUT_SETTINGS   : constant Interfaces.C.Int := %d; \n", MSG_TYPE_TIMEOUT_SETTINGS);
  printf("  BYTE_NEW_ADDRESS            : constant Interfaces.C.Int := %d; \n", BYTE_NEW_ADDRESS);
  printf("  BYTE_SENSOR_1_TYPE          : constant Interfaces.C.Int := %d; \n", BYTE_SENSOR_1_TYPE);
  printf("  BYTE_SENSOR_2_TYPE          : constant Interfaces.C.Int := %d; \n", BYTE_SENSOR_2_TYPE);
  printf("  BYTE_TIMEOUT                : constant Interfaces.C.Int := %d; \n", BYTE_TIMEOUT);
  printf("  TYPE_MOTOR_PWM              : constant Interfaces.C.Int := %d; \n", TYPE_MOTOR_PWM);
  printf("  TYPE_MOTOR_SPEED            : constant Interfaces.C.Int := %d; \n", TYPE_MOTOR_SPEED);
  printf("  TYPE_MOTOR_POSITION         : constant Interfaces.C.Int := %d; \n", TYPE_MOTOR_POSITION);
  printf("  TYPE_SENSOR_RAW             : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_RAW);
  printf("  TYPE_SENSOR_LIGHT_OFF       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_LIGHT_OFF);
  printf("  TYPE_SENSOR_LIGHT_ON        : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_LIGHT_ON);
  printf("  TYPE_SENSOR_TOUCH           : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_TOUCH);
  printf("  TYPE_SENSOR_ULTRASONIC_CONT : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_ULTRASONIC_CONT);
  printf("  TYPE_SENSOR_ULTRASONIC_SS   : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_ULTRASONIC_SS);
  printf("  TYPE_SENSOR_RCX_LIGHT       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_RCX_LIGHT);
  printf("  TYPE_SENSOR_COLOR_FULL      : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_COLOR_FULL);
  printf("  TYPE_SENSOR_COLOR_RED       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_COLOR_RED);
  printf("  TYPE_SENSOR_COLOR_GREEN     : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_COLOR_GREEN);
  printf("  TYPE_SENSOR_COLOR_BLUE      : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_COLOR_BLUE);
  printf("  TYPE_SENSOR_COLOR_NONE      : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_COLOR_NONE);
  printf("  TYPE_SENSOR_I2C             : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_I2C);
  printf("  TYPE_SENSOR_I2C_9V          : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_I2C_9V);
  printf("  TYPE_SENSOR_EV3_US_M0       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_US_M0);
  printf("  TYPE_SENSOR_EV3_US_M1       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_US_M1);
  printf("  TYPE_SENSOR_EV3_US_M2       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_US_M2);
  printf("  TYPE_SENSOR_EV3_US_M3       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_US_M3);
  printf("  TYPE_SENSOR_EV3_US_M4       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_US_M4);
  printf("  TYPE_SENSOR_EV3_US_M5       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_US_M5);
  printf("  TYPE_SENSOR_EV3_US_M6       : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_US_M6);
  printf("  TYPE_SENSOR_EV3_COLOR_M0    : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_COLOR_M0);
  printf("  TYPE_SENSOR_EV3_COLOR_M1    : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_COLOR_M1);
  printf("  TYPE_SENSOR_EV3_COLOR_M2    : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_COLOR_M2);
  printf("  TYPE_SENSOR_EV3_COLOR_M3    : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_COLOR_M3);
  printf("  TYPE_SENSOR_EV3_COLOR_M4    : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_COLOR_M4);
  printf("  TYPE_SENSOR_EV3_COLOR_M5    : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_COLOR_M5);
  printf("  TYPE_SENSOR_EV3_GYRO_M0     : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_GYRO_M0);
  printf("  TYPE_SENSOR_EV3_GYRO_M1     : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_GYRO_M1);
  printf("  TYPE_SENSOR_EV3_GYRO_M2     : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_GYRO_M2);
  printf("  TYPE_SENSOR_EV3_GYRO_M3     : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_GYRO_M3);
  printf("  TYPE_SENSOR_EV3_GYRO_M4     : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_GYRO_M4);
  printf("  TYPE_SENSOR_EV3_INFRARED_M0 : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_INFRARED_M0);
  printf("  TYPE_SENSOR_EV3_INFRARED_M1 : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_INFRARED_M1);
  printf("  TYPE_SENSOR_EV3_INFRARED_M2 : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_INFRARED_M2);
  printf("  TYPE_SENSOR_EV3_INFRARED_M3 : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_INFRARED_M3);
  printf("  TYPE_SENSOR_EV3_INFRARED_M4 : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_INFRARED_M4);
  printf("  TYPE_SENSOR_EV3_INFRARED_M5 : constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_INFRARED_M5);
  printf("  TYPE_SENSOR_EV3_TOUCH_0		: constant Interfaces.C.Int := %d; \n", TYPE_SENSOR_EV3_TOUCH_0);
  printf("  BIT_I2C_MID                 : constant Interfaces.C.Int := %d; \n", BIT_I2C_MID);
  printf("  BIT_I2C_SAME                : constant Interfaces.C.Int := %d; \n", BIT_I2C_SAME);
  printf("  INDEX_RED                   : constant Interfaces.C.Int := %d; \n", INDEX_RED);
  printf("  INDEX_GREEN                 : constant Interfaces.C.Int := %d; \n", INDEX_GREEN);
  printf("  INDEX_BLUE                  : constant Interfaces.C.Int := %d; \n", INDEX_BLUE);
  printf("  INDEX_BLANK                 : constant Interfaces.C.Int := %d; \n", INDEX_BLANK);
  printf("  US_I2C_SPEED                : constant Interfaces.C.Int := %d; \n", US_I2C_SPEED);
  printf("  US_I2C_IDX                  : constant Interfaces.C.Int := %d; \n", US_I2C_IDX);
  printf("  LEGO_US_I2C_ADDR            : constant Interfaces.C.Int := %d; \n", LEGO_US_I2C_ADDR);
  printf("  LEGO_US_I2C_DATA_REG        : constant Interfaces.C.Int := %d; \n", LEGO_US_I2C_DATA_REG);
  printf("end Brickpi.Constants;\n");
  return 0;

}


