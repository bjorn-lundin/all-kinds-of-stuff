/*
*  This is a part of a thin Ada binding to the c-source from Dexter Industries
*  2015-04-27 Björn Lundin
*
*
*  Compile Using:
*  gcc -c -Wall bnlbrickpi.c -o bnlbrickpi.o -L/usr/local/lib -lwiringPi -lm -lrt
*/

//bnlbrickpi.c
extern int print_constants() ;
extern int print_io_constants() ;

//BrickPi.h
extern int BrickPiSetup();
extern int BrickPiSetupSensors();
extern int BrickPiUpdateValues();

// tick.h
extern void ClearTick();
extern unsigned long CurrentTickMs();
extern unsigned long CurrentTickUs();
extern struct BrickPiStruct * GetPointerToBrickPi();




