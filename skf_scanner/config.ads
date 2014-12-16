with Interfaces.C;

package Config is
  package C renames Interfaces.C;

--from route_conv.cgi
--set lockfile     [file join / var lock route_conveyor.lock]
--set datafile     [file join / usr local scanner_handler route_conveyor.dat]
--set hourlyLogDir [file join / usr local scanner_handler hourly_log]

  Base_Directory                : constant String   := "/usr/local/scanner_handler";  -- most things are based here
  Pid_File                      : constant String   := "/var/run/scanner_handler.pid"; -- used by startup script
  Lock_File                     : constant String   := "/var/lock/route_conveyor.lock"; -- to avoid simultanious access to the Datafile
  Data_File                     : constant String   := Base_Directory & "/route_conveyor.dat"; --shared file with cgi-app

  Incoming_FTP_Directory        : constant String   := Base_Directory & "/ftp";
  Work_Directory                : constant String   := Base_Directory & "/work";
  Log_Directory                 : constant String   := Base_Directory & "/log";
  Error_Directory               : constant String   := Base_Directory & "/err";
  Ftp_File_Extension            : constant String   := ".dat";
  Ftp_File_Pattern              : constant String   := "*" & Ftp_File_Extension;
  Single_Line_File_Extension    : constant String   := ".wrk";
  Single_Line_File_Pattern      : constant String   := "*" & Single_Line_File_Extension;
  Hourly_Log_File_Directory     : constant String   := Base_Directory & "/hourly_log";

  Baudrate                      : constant C.Int    := 9_600;
  File_Growth_Delay             : constant Duration := 2.0;
  Check_Files_Delay             : constant Duration := 60.0;
  Reset_PLC_Delay               : constant Duration := 1.0;
  Route_File_Check_Delay        : constant Duration := 5 * 60.0;

  Parallel_Port_Address         : constant C.Int    := 16#3bc#;
  -- 0x3bc for /dev/lp0, 0x378 for /dev/lp1, and 0x278 for /dev/lp2.
  -- 16#3bc#             16#378#                 16#278#
  subtype Serial_Port_Type is String(1..11);
  type Serial_Port_Pointer_Type is access all Serial_Port_Type;
  Serial_Port           : aliased Serial_Port_Type := "/dev/ttyS0" & Ascii.Nul;
  Serial_Port_Pointer   : Serial_Port_Pointer_Type := Serial_Port'Access;

end Config;
