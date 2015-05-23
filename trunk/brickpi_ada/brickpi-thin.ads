
with Unchecked_Conversion;
with Interfaces.C;

package Brickpi.Thin is
-- brickpi.h

--extern int BrickPiSetup()
  function Setup return Interfaces.C.Int ;
  pragma Import(C, Setup, "BrickPiSetup");


--extern int BrickPiSetupSensors();
  function Setup_Sensors return Interfaces.C.Int ;
  pragma Import(C, Setup_Sensors, "BrickPiSetupSensors");
  
  
--extern int BrickPiUpdateValues();
  function Update_Values return Interfaces.C.Int ;
  pragma Import(C, Update_Values, "BrickPiUpdateValues");

-- tick.h
--extern void ClearTick();
  procedure Clear_Tick ;
  pragma Import(C, Clear_Tick, "ClearTick");

--extern unsigned long CurrentTickMs();
  function Current_Tick_Ms return Interfaces.C.Unsigned_Long ;
  pragma Import(C, Current_Tick_Ms, "CurrentTickMs");

--extern unsigned long CurrentTickUs();
  function Current_Tick_Us return Interfaces.C.Unsigned_Long ;
  pragma Import(C, Current_Tick_Us, "CurrentTicKUs");
  

  type Sensor_Port_Type is (Port_1, Port_2, Port_3, Port_4);
  for Sensor_Port_Type use (Port_1 => 0, Port_2 => 1, Port_3 => 2, Port_4 => 3);
  for Sensor_Port_Type'size use Interfaces.C.Unsigned_Char'Size;
  function Sensor_Port is new Unchecked_Conversion(Sensor_Port_Type, Interfaces.C.Unsigned_Char);
  function Sensor_Port is new Unchecked_Conversion(Interfaces.C.Unsigned_Char, Sensor_Port_Type);
  
  
  type Address_Type            is array(1..2)                                of Interfaces.C.Unsigned_Char;
  type Motor_Speed_Type        is array(Motor_Port_Type'range)               of Interfaces.C.Int;
  type Motor_Enable_Type       is array(Motor_Port_Type'range)               of Interfaces.C.Unsigned_Char;
  type Encoder_Offset_Type     is array(Motor_Port_Type'range)               of Interfaces.C.Long;
  type Encoder_Type            is array(Motor_Port_Type'range)               of Interfaces.C.Long;
  type Sensor_Value_Type       is array(Sensor_Port_Type'range)              of Interfaces.C.Long;
  type Sensor_Array_Type       is array(Sensor_Port_Type'range, 1..4)        of Interfaces.C.Long;                               
  type Sensor_Type_Type        is array(Sensor_Port_Type'range)              of Interfaces.C.Unsigned_Char;
  
  -- mostly I2C stuff below  
  type Sensor_Settings_Type    is array(Sensor_Port_Type'range, 1..8)        of Interfaces.C.Unsigned_Char;
  type Sensor_I2C_Devices_Type is array(Sensor_Port_Type'range)              of Interfaces.C.Unsigned_Char;
  type Sensor_I2C_Speed_Type   is array(Sensor_Port_Type'range)              of Interfaces.C.Unsigned_Char;
  type Sensor_I2C_Addr_Type    is array(Sensor_Port_Type'range, 1..8)        of Interfaces.C.Unsigned_Char;
  type Sensor_I2C_Write_Type   is array(Sensor_Port_Type'range, 1..8)        of Interfaces.C.Unsigned_Char;
  type Sensor_I2C_Read_Type    is array(Sensor_Port_Type'range, 1..8)        of Interfaces.C.Unsigned_Char;
  type Sensor_I2C_Out_Type     is array(Sensor_Port_Type'range, 1..8, 1..16) of Interfaces.C.Unsigned_Char;
  type Sensor_I2C_In_Type      is array(Sensor_Port_Type'range, 1..8, 1..16) of Interfaces.C.Unsigned_Char;
    
  type Brick_Pi_Record is record
    Address           : Address_Type;               -- Communication addresses
    Timeout           : Interfaces.C.Unsigned_Long; -- Communication timeout (how long in ms since the last valid communication before floating the motors). 0 disables the timeout.  
    -- Motors      
    Motor_Speed       : Motor_Speed_Type;           -- Motor speeds, from -255 to 255
    Motor_Enable      : Motor_Enable_Type;          -- Motor enable/disable
    -- Encoders     
    Encoder_Offset     : Encoder_Offset_Type;        -- Encoder offsets (not yet implemented)
    Encoder            : Encoder_Type;               -- Encoder values
    -- Sensors
    Sensor_Value       : Sensor_Value_Type;          -- Primary sensor values
    Sensor_Array       : Sensor_Array_Type;          -- For more sensor values for the sensor (e.g. for color sensor FULL mode).
    Sensor_Type        : Sensor_Type_Type;           -- Sensor types
    -- I2C     
    Sensor_Settings    : Sensor_Settings_Type;       -- Sensor settings, used for specifying I2C settings.
    Sensor_I2C_Devices : Sensor_I2C_Devices_Type;    --How many I2C devices are on each bus (1 - 8).
    Sensor_I2C_Speed   : Sensor_I2C_Speed_Type;      --The I2C speed.
    Sensor_I2C_Addr    : Sensor_I2C_Addr_Type;       --The I2C address of each device on each bus.  
    Sensor_I2C_Write   : Sensor_I2C_Write_Type;      --How many bytes to write
    Sensor_I2C_Read    : Sensor_I2C_Read_Type;       --How many bytes to read
    Sensor_I2C_Out     : Sensor_I2C_Out_Type;        --The I2C bytes to write
    Sensor_I2C_In      : Sensor_I2C_In_Type;         --The I2C input buffers
  end record;
  pragma Convention(C,Brick_Pi_Record);
  type Brick_Pi_Record_Pointer is access all Brick_Pi_Record;

--extern struct BrickPiStruct * GetPointerToBrickPi();
  function Get_Pointer_To_Brick_Pi return Brick_Pi_Record_Pointer ;
  pragma Import(C, Get_Pointer_To_Brick_Pi, "GetPointerToBrickPi");

  
  
  procedure Print_Constants; -- discards result since it is always 0
  pragma Import(C, Print_Constants, "print_constants");
  
  
  
end Brickpi.Thin;
