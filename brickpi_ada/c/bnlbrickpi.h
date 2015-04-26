


#extern int ByteIOOpen (char *Device, void **Channel, int Shared);

extern int BrickPiSetup();
extern int BrickPiSetupSensors();
extern int BrickPiUpdateValues();

// tick.h
extern void ClearTick();
extern unsigned long CurrentTickMs();
extern unsigned long CurrentTickUs();
extern struct BrickPiStruct * GetPointerToBrickPi();




