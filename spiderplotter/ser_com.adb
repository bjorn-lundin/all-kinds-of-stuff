------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2007-2013, AdaCore                      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the GNU/Linux implementation of this package
with Ada.Streams;                use Ada.Streams;
with Ada;                        use Ada;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;               use System;
pragma Warnings(Off);
with System.Communication; use System.Communication;
with System.CRTL;          use System.CRTL;
pragma Warnings(On);

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Ser_Com is

   use type Interfaces.C.unsigned;

   type Port_Data is new int;

   subtype unsigned is Interfaces.C.unsigned;
   subtype char is Interfaces.C.char;
   subtype unsigned_char is Interfaces.C.unsigned_char;

   function fcntl (fd : int; cmd : int; value : int) return int;
   pragma Import (C, fcntl, "fcntl");

   O_RDWR   : constant := 8#02#;
   O_NOCTTY : constant := 8#0400#;
   O_NDELAY : constant := 8#04000#;
   FNDELAY  : constant := O_NDELAY;
   F_SETFL  : constant := 4;
   TCSANOW  : constant := 0;
   TCIFLUSH : constant := 0;
   CLOCAL   : constant := 8#04000#;
   CREAD    : constant := 8#0200#;
   CSTOPB   : constant := 8#0100#;
   CRTSCTS  : constant := 8#020000000000#;
   PARENB   : constant := 8#00400#;
   PARODD   : constant := 8#01000#;

   --  c_cc indexes

   VTIME : constant := 5;
   VMIN  : constant := 6;

   C_Data_Rate : constant array (Data_Rate) of unsigned :=
                   (B1200   => 8#000011#,
                    B2400   => 8#000013#,
                    B4800   => 8#000014#,
                    B9600   => 8#000015#,
                    B19200  => 8#000016#,
                    B38400  => 8#000017#,
                    B57600  => 8#010001#,
                    B115200 => 8#010002#);

   C_Bits      : constant array (Data_Bits) of unsigned :=
                   (CS7 => 8#040#, CS8 => 8#060#);

   C_Stop_Bits : constant array (Stop_Bits_Number) of unsigned :=
                   (One => 0, Two => CSTOPB);

   C_Parity    : constant array (Parity_Check) of unsigned :=
                   (None => 0, Odd => PARENB or PARODD, Even => PARENB);

   procedure Raise_Error (Message : String; Error : Integer := Errno);
   pragma No_Return (Raise_Error);

   ----------
   -- Name --
   ----------

   function Name (Number : Positive) return Port_Name is
      N     : constant Natural := Number - 1;
      N_Img : constant String  := Natural'Image (N);
   begin
    --  return Port_Name ("/dev/ttyS" & N_Img (N_Img'First + 1 .. N_Img'Last));
      return Port_Name ("/dev/ttyAMA" & N_Img (N_Img'First + 1 .. N_Img'Last));
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (Port : out Serial_Port;
      Name : Port_Name)
   is
      C_Name : constant String := String (Name) & ASCII.NUL;
      Res    : int;

   begin
      if Port.H = null then
         Port.H := new Port_Data;
      end if;

      Port.H.all := Port_Data (open
         (C_Name (C_Name'First)'Address, int (O_RDWR + O_NOCTTY + O_NDELAY)));

      if Port.H.all = -1 then
         Raise_Error ("open: open failed");
      end if;

      --  By default we are in blocking mode

      Res := fcntl (int (Port.H.all), F_SETFL, 0);

      if Res = -1 then
         Raise_Error ("open: fcntl failed");
      end if;
   end Open;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Message : String; Error : Integer := Errno) is
   begin
      raise Serial_Error with Message & " (" & Integer'Image (Error) & ')';
   end Raise_Error;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Port   : in out Serial_Port;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Len : constant size_t := Buffer'Length;
      Res : ssize_t;

   begin
      if Port.H = null then
         Raise_Error ("read: port not opened", 0);
      end if;

      Res := read (Integer (Port.H.all), Buffer'Address, Len);

      if Res = -1 then
         Raise_Error ("read failed");
      end if;

      Last := Last_Index (Buffer'First, size_t (Res));
   end Read;

   ------------------------------------------------------

   procedure Read
     (Port   : in out Serial_Port;
      Buffer : out String;
      Last   : out Natural) is

    Local_Buffer : Stream_Element_Array(Stream_Element_Offset(Buffer'First) .. Stream_Element_Offset(Buffer'Last));
    Local_Last   : Stream_Element_Offset;
   begin
     Port.Read(Local_Buffer,Local_Last);
     Buffer := To_String(Local_Buffer);
     Last   := Natural(Local_Last);
   end Read;

   ------------------------------------------------------
   procedure Read
     (Port   : in out Serial_Port;
      Buffer : out Character;
      Last   : out Natural) is
    S_Buffer : String(1..1) := " ";
    Local_Buffer : Stream_Element_Array(1..1);
    Local_Last   : Stream_Element_Offset := 0;
   begin
     Port.Read(Local_Buffer,Local_Last);
     S_Buffer := To_String(Local_Buffer);
     Buffer := S_Buffer(1);
     Last   := Natural(Local_Last);
   end Read;

   ------------------------------------------------------
   
   procedure Write
     (Port   : in out Serial_Port;
      Buffer : String) is
   begin
     for i in Buffer'range loop
       Character'Write (Port'access, Buffer(i));
     end loop;  
   end Write;

   ------------------------------------------------------
   
   procedure Write
     (Port   : in out Serial_Port;
      Buffer : Character) is
   begin
      Character'Write (Port'access, Buffer);
   end Write;

   ---------
   -- Set --
   ---------
   C_Flow_Control : constant array (Flow_Control) of unsigned :=
                      (None => 0, Active => CRTSCTS);  --jsa


   procedure Set
     (Port      : Serial_Port;
      Rate      : Data_Rate        := B9600;
      Bits      : Data_Bits        := CS8;
      Stop_Bits : Stop_Bits_Number := One;
      Parity    : Parity_Check     := None;
      Block     : Boolean          := True;
      Timeout   : Duration         := 10.0;
      Flow      : Flow_Control     := Active) --jsa
   is
      type Termios is record
         c_iflag  : unsigned;
         c_oflag  : unsigned;
         c_cflag  : unsigned;
         c_lflag  : unsigned;
         c_line   : unsigned_char;
         c_cc     : Interfaces.C.char_array (0 .. 31);
         c_ispeed : unsigned;
         c_ospeed : unsigned;
      end record;
      pragma Convention (C, Termios);

      function tcgetattr (fd : int; termios_p : Address) return int;
      pragma Import (C, tcgetattr, "tcgetattr");

      function tcsetattr
        (fd : int; action : int; termios_p : Address) return int;
      pragma Import (C, tcsetattr, "tcsetattr");

      function tcflush (fd : int; queue_selector : int) return int;
      pragma Import (C, tcflush, "tcflush");

      Current : Termios;

      Res : int;
      pragma Warnings (Off, Res);
      --  Warnings off, since we don't always test the result

   begin
      if Port.H = null then
         Raise_Error ("set: port not opened", 0);
      end if;

      --  Get current port settings

      Res := tcgetattr (int (Port.H.all), Current'Address);

      --  Change settings now

      Current.c_cflag      := C_Data_Rate (Rate)
                                or C_Bits (Bits)
                                or C_Stop_Bits (Stop_Bits)
                                or C_Parity (Parity)
                                or CLOCAL
                                or CREAD
                                or C_Flow_Control (Flow); --jsa
--                                or CRTSCTS; --jsa
      Current.c_lflag      := 0;
      Current.c_iflag      := 0;
      Current.c_oflag      := 0;
      Current.c_ispeed     := Data_Rate_Value (Rate);
      Current.c_ospeed     := Data_Rate_Value (Rate);
      Current.c_cc (VMIN)  := char'Val (0);
      Current.c_cc (VTIME) := char'Val (Natural (Timeout * 10));

      --  Set port settings

      Res := tcflush (int (Port.H.all), TCIFLUSH);
      Res := tcsetattr (int (Port.H.all), TCSANOW, Current'Address);

      --  Block
      -- open set blocking mode
      if not Block then
        Res := fcntl (int (Port.H.all), F_SETFL, FNDELAY);
      end if;

      if Res = -1 then
         Raise_Error ("set: fcntl failed");
      end if;
   end Set;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Port   : in out Serial_Port;
      Buffer : Stream_Element_Array) is

      Len : constant size_t := Buffer'Length;
      Res : ssize_t;

   begin
      if Port.H = null then
         Raise_Error ("write: port not opened", 0);
      end if;

      Res := write (int (Port.H.all), Buffer'Address, Len);

      if Res = -1 then
         Raise_Error ("write failed");
      end if;

      pragma Assert (size_t (Res) = Len);
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (Port : in out Serial_Port) is
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Port_Data, Port_Data_Access);

      Res : int;
      pragma Unreferenced (Res);

   begin
      if Port.H /= null then
         Res := close (int (Port.H.all));
         Unchecked_Free (Port.H);
      end if;
   end Close;


   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------
   function To_Stream_Element_Array (Self : in String)
                                     return Ada.Streams.Stream_Element_Array is
      subtype StringX is String (1 .. Self'Length);
      subtype ArrayX  is Stream_Element_Array (1 .. Self'Length);
      function To_Array is new Ada.Unchecked_Conversion (StringX, ArrayX);
   begin
      return To_Array (Self);
   end To_Stream_Element_Array;
   pragma Unreferenced(To_Stream_Element_Array);
   ------------------------------------------------------------------------

   ---------------
   -- To_String --
   ---------------
   function To_String (Self : in Ada.Streams.Stream_Element_Array)
      return String
   is
      subtype StringX is String (1 .. Self'Length);
      subtype ArrayX  is Stream_Element_Array (1 .. Self'Length);
      function To_String is new Ada.Unchecked_Conversion (ArrayX, StringX);
   begin
      return To_String (Self);
   end To_String;
   ------------------------------------------------------------------------



end Ser_Com;
