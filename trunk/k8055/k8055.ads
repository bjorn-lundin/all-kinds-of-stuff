
with Interfaces.C;

package k8055 is

  type Longint                     is new Interfaces.C.Long;
  type Card_Address_Type           is new Longint range 0 .. 3;
  type Data_Type                   is new Longint range 0 .. 255;
  type Analog_Input_Channel_Type   is new Longint range 1 .. 2; 
  type Analog_Output_Channel_Type  is new Longint range 1 .. 2; 
  type Digital_Input_Channel_Type  is new Longint range 1 .. 5; 
  type Digital_Output_Channel_Type is new Longint range 1 .. 8; 
  type Counter_Number_Type         is new Longint range 1 .. 2; 



--Opens the communication link to the K8055 device
  function OpenDevice(CardAddress : Card_Address_Type) return Longint;
--              Parameter
--              CardAddress: Value between 0 and 3 which corresponds to the jumper (SK5, SK6) setting on the
--              K8055 board. See table 1.
--              Result
--              Longint: If succeeded the return value will be the card address read from the K8055 hardware.
--              Return value -1 indicates that K8055 card was not found.
--              Description
--              Opens the communication link to the K8055 card. Loads the drivers needed to communicate via the
--              USB port. This procedure must be performed before any attempts to communicate with the K8055
--              card.
--              This function can also be used to selects the active K8055 card to read and write the data. All the
--              communication routines after this function call are addressed to this card until the other card is
--              selected by this function call.


--Closes the link to the K8055 device
  procedure CloseDevice;
--              Description
--              Unloads the communication routines for K8055 card and unloads the driver needed to communicate
--              via the USB port. This is the last action of the application program before termination.


--  Analogue to Digital converter procedures
  --   Reads the status of one analogue input-channel
  function ReadAnalogChannel (Channel: Analog_Input_Channel_Type) return Data_Type;
--    Parameter
--    Channel: Value between 1 and 2 which corresponds to the AD channel whose status is to be read.
--    Result
--    Longint: The corresponding Analogue to Digital Converter data is read.
--    Description
--    The input voltage of the selected 8-bit Analogue to Digital converter channel is converted to a value
--    which lies between 0 and 255.

  procedure ReadAllAnalog( Data1, Data2 : out Data_Type);
--    Parameter
--    Data1, Data2: Pointers to the long integers where the data will be read.
--    Description
--  The status of both Analogue to Digital Converters are read to an array of long integers.


  procedure OutputAnalogChannel(Channel: Analog_Output_Channel_Type; Data: Data_Type);
--    Parameters
--    Channel: Value between 1 and 2 which corresponds to the 8-bit DA channel number whose data is
--    to be set.
--    Data: Value between 0 and 255 which is to be sent to the 8-bit Digital to Analogue Converter .
--    Description
--    The indicated 8-bit Digital to Analogue Converter channel is altered according to the new data. This
--    means that the data corresponds to a specific voltage. The value 0 corresponds to a minimum output
--    voltage (0 Volt) and the value 255 corresponds to a maximum output voltage (+5V). A value of 'Data'
--    lying in between these extremes can be translated by the following formula : Data / 255 x 5V.

  procedure  OutputAllAnalog(Data1: Data_Type; Data2: Data_Type);
--              Parameters
--              Data1, Data2: Value between 0 and 255 which is to be sent to the 8-bit Digital to Analogue
--              Converter.
--              Description
--              Both 8-bit Digital to Analogue Converter channels are altered according to the new data. This means
--              that the data corresponds to a specific voltage. The value 0 corresponds to a minimum output voltage
--              (0 Volt) and the value 255 corresponds to a maximum output voltage (+5V). A value of 'Data1' or
--              'Data2' lying in between these extremes can be translated by the following formula : Data / 255 x 5V.


  procedure ClearAnalogChannel(Channel: Analog_Output_Channel_Type);
--              Parameter
--              Channel: Value between 1 and 2 which corresponds to the 8-bit DA channel number in which the
--              data is to be erased.
--              Description
--              The selected DA-channel is set to minimum output voltage (0 Volt).

  procedure ClearAllAnalog;
--    Description
--    Both DA-channels are set to minimum output voltage (0 Volt) .


  procedure SetAnalogChannel(Channel: Analog_Output_Channel_Type);
--    Parameter
--    Channel: Value between 1 and 2 which corresponds to the 8-bit DA channel number in which the
--    data is to be set to maximum.
--    Description
--    The selected 8-bit Digital to Analogue Converter channel is set to maximum output voltage.

  procedure SetAllAnalog;
--    Description
--    All channels of the 8-bit Digital to Analogue Converters are set to maximum output voltage.


  procedure WriteAllDigital(Data: Data_Type);
--              Parameter
--              Data: Value between 0 and 255 that is sent to the output port (8 channels).
--              Description
--              The channels of the digital output port are updated 
--              with the status of the corresponding bits in the data
--              parameter. A high (1) level means that the microcontroller 
--              IC1 output is set, and a low (0) level means
--              that the output is cleared.




  procedure ClearDigitalChannel(Channel: Digital_Output_Channel_Type);
--              Parameter
--              Channel: Value between 1 and 8 which corresponds to the output channel that is to be cleared.
--              Description
--              The selected channel is cleared.



  procedure ClearAllDigital;
--              Result
--              All digital outputs are cleared.



  procedure SetDigitalChannel(Channel: Digital_Output_Channel_Type);
--    Parameter
--    Channel: Value between 1 and 8 which corresponds to the output channel that is to be set.
--    Description
--    The selected digital output channel is set.



  procedure SetAllDigital;
--    Description
--    All the digital output channels are set.



  function ReadDigitalChannel(Channel: Digital_Input_Channel_Type) return Boolean;
--    Parameter
--    Channel: Value between 1 and 5 which corresponds to the input channel whose status is to be read.
--    Result
--    Boolean: TRUE means that the channel has been set and FALSE means that it has been cleared.
--    Description
--  The status of the selected Input channel is read.


  function ReadAllDigital return Longint;
--              Result
--              Longint: The 5 LSB correspond to the status of the input channels. A high (1) means that the
--              channel is HIGH, a low (0) means that the channel is LOW.
--              Description
--              The function returns the status of the digital inputs.



  procedure ResetCounter(CounterNumber: Counter_Number_Type);
--              Parameter
--              CounterNumber: Value 1 or 2, which corresponds to the counter to be reset.
--              Description
--              The selected pulse counter is reset.


  function ReadCounter(CounterNumber: Counter_Number_Type) return Longint;
--              Parameter
--              CounterNumber: Value 1 or 2, which corresponds to the counter to be read.
--              Result
--              Longint: The content of the 16 bit pulse counter.
--              Description
--              The function returns the status of the selected 16 bit pulse counter.
--              The counter number 1 counts the pulses fed to the input I1 and the counter number 2 counts the
--              pulses fed to the input I2.




  procedure SetCounterDebounceTime(CounterNr : Counter_Number_Type; DebounceTime: Longint);
--     Parameter
--     CounterNumber: Value 1 or 2, which corresponds to the counter to be set.
--     DebounceTime: Debounce time for the pulse counter.
--     The DebounceTime value corresponds to the debounce time in milliseconds (ms) to be set for the
--     pulse counter. Debounce time value may vary between 0 and 5000.
--     Description
--     The counter inputs are debounced in the software to prevent false triggering when mechanical
--     switches or relay inputs are used. The debounce time is equal for both falling and rising edges. The
--     default debounce time is 2ms. This means the counter input must be stable for at least 2ms before it is
--     recognised, giving the maximum count rate of about 200 counts per second.
--     If the debounce time is set to 0, then the maximum counting rate is about 2000 counts per second.


private
  pragma Import(C, OpenDevice, "OpenDevice");
  pragma Import(C, CloseDevice, "CloseDevice");
  pragma Import(C, ReadAnalogChannel, "ReadAnalogChannel");
  pragma Import(C, ReadAllAnalog, "ReadAllAnalog");
  pragma Import(C, OutputAnalogChannel, "OutputAnalogChannel");
  pragma Import(C, OutputAllAnalog, "OutputAllAnalog");
  pragma Import(C, ClearAnalogChannel, "ClearAnalogChannel");
  pragma Import(C, ClearAllAnalog, "ClearAllAnalog");
  pragma Import(C, SetAnalogChannel, "SetAnalogChannel");
  pragma Import(C, SetAllAnalog, "SetAllAnalog");
  pragma Import(C, WriteAllDigital, "WriteAllDigital");
  pragma Import(C, ClearDigitalChannel, "ClearDigitalChannel");
  pragma Import(C, ClearAllDigital, "ClearAllDigital");
  pragma Import(C, ReadDigitalChannel, "ReadDigitalChannel");
  pragma Import(C, SetDigitalChannel, "SetDigitalChannel");
  pragma Import(C, SetAllDigital, "SetAllDigital");
  pragma Import(C, ReadAllDigital, "ReadAllDigital");
  pragma Import(C, ResetCounter, "ResetCounter");
  pragma Import(C, ReadCounter, "ReadCounter");
  pragma Import(C, SetCounterDebounceTime, "SetCounterDebounceTime");

end k8055;
