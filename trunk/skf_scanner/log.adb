
with Ada.Directories;
with Ada.Calendar;
with Gnat.Calendar;

with Utils;
with Text_Io;
with Config;

package body Log is

  Lock               : Utils.Binary_Lock_Type;
  Log_Enabled        : Boolean := True;
  Hourly_Log_File    : Text_Io.File_Type;
  Current_Hour       : String(1..2) := "  ";

  -------------------------------------------------

  procedure Put2(What : String) is
    use Text_Io;
  begin 
    if Log_Enabled then
      Put_Line(Standard_Error, What);
    end if;
  end Put2;

  -------------------------------------------------

  procedure Put(What : String) is
    use Text_Io;
    use Ada.Directories;
    use Ada.Calendar;
    use Gnat.Calendar;
    Now : Time := Clock;
    Year : Year_Number;
    Month :Month_Number;
    Day : Day_Number;
    Hour : Hour_Number;
    Minute  : Minute_Number;
    Second  : Second_Number;
    Sub_Second : Second_Duration;
    sHour : String(1..2) := "00";
  begin
   if Log_Enabled then
      Split(Now, Year, Month, Day, Hour, Minute, Second, Sub_Second);

      case Hour is
        when 0  => sHour := "00";
        when 1  => sHour := "01";
        when 2  => sHour := "02";
        when 3  => sHour := "03";
        when 4  => sHour := "04";
        when 5  => sHour := "05";
        when 6  => sHour := "06";
        when 7  => sHour := "07";
        when 8  => sHour := "08";
        when 9  => sHour := "09";
        when 10 => sHour := "10";
        when 11 => sHour := "11";
        when 12 => sHour := "12";
        when 13 => sHour := "13";
        when 14 => sHour := "14";
        when 15 => sHour := "15";
        when 16 => sHour := "16";
        when 17 => sHour := "17";
        when 18 => sHour := "18";
        when 19 => sHour := "19";
        when 20 => sHour := "20";
        when 21 => sHour := "21";
        when 22 => sHour := "22";
        when 23 => sHour := "23";
      end case;

      Lock.Lock;
      if Current_Hour /= sHour then
        if Is_Open(Hourly_Log_File) then
          Close(Hourly_Log_File);
        end if;
      end if; 

      Current_Hour := sHour;

      if not Exists(Config.Hourly_Log_File_Directory & "/" & sHour & ".log") then
        Create(Hourly_Log_File, Out_File, Config.Hourly_Log_File_Directory & "/" & sHour & ".log");
      end if;

      if not Is_Open(Hourly_Log_File) then
        Open(Hourly_Log_File, Out_File, Config.Hourly_Log_File_Directory & "/" & sHour & ".log");
      end if;

      declare
        s : string :=           Year_Number'Image(Year) & Month_Number'Image(Month) & 
                                Day_Number'Image(Day)   & Hour_Number'Image(Hour)   & 
                                Minute_Number'Image(Minute) & Second_Number'Image(Second) & 
                                Second_Duration'Image(Sub_Second);
        j : integer := 0;
      begin 
        for i in s'range loop
          case s(i) is
            when ' ' =>
              case j is
                when 0      => null; -- we are at idx 1, ie before year
                when 1 .. 2 => s(i) := '-';
                when 3      => null;  -- between date and time
                when 4 .. 5 => s(i) := ':';
                when 6      => s(i) := '.'; -- subseconds
                when others => null;
              end case;
              j := j+1;
            when others => null;
          end case;
        end loop;
        Put_Line(Hourly_Log_File, S & " -> " & What);
      end;
      Lock.Release;
    end if;
  exception
    when E: others =>
      Put2(Utils.Tracebackinfo(E));
      if Lock.Is_Locked then
        Lock.Release;
      end if;
  end Put;

 
end Log;

