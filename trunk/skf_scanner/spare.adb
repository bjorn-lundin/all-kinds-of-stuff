  procedure Treat_New_Files is
    use Ada.Directories;
    use Ada.Calendar;
    Cwd                  : String                   := Current_Directory;
    Search_Work_File     : Search_Type;
    Work_File_Entry      : Directory_Entry_Type;
    Error_In_File        : Boolean := False;
    Singel_Line_File,
    Data_File            : Text_Io.File_Type; 
    Buf                  : String_Buffer;
    Scan_Data            : Scanner_Data_Type;
    Len                  : Positive                 := 1;

  begin
    Set_Directory(Work_Directory);
    Start_Search(Search    => Search_Work_File,
                 Directory => Current_Directory,
                 Pattern   => Ftp_File_Pattern,
                 Filter    => Filter_Type'(Directory     => False,
                                           Ordinary_File => True,
                                           Special_File  => False));
    loop
      exit when not More_Entries(Search_Work_File);
      Get_Next_Entry(Search_Work_File, Work_File_Entry);
      begin
        Log("Open file: '" & Full_Name(Work_File_Entry) & "'");
        Text_Io.Open(File => Data_File,
                     Mode => Text_Io.In_File,
                     Name => Full_Name(Work_File_Entry));
        Read_File_Loop :
        loop
          Text_Io.Get_Line(Data_File,Buf,Len);
          Split(Buf(1..Len),Scan_Data);
          Scan_Handler.Put_Load_Route_Connection(Scan_Data);
          begin  -- create a file for each load. deleted when used in IO_Task
            Text_Io.Create(File => Singel_Line_File,
                           Name => Work_Directory & "/" & Scan_Data.Transaction_Identity & Single_Line_File_Extension);
            Text_Io.Put_Line(Singel_Line_File,Buf(1..Len));
          exception
            when others => null;
          end;
          Text_Io.Close(File => Singel_Line_File);
        end loop Read_File_Loop;
      exception
        when Text_Io.End_Error =>
          begin
            Text_Io.Close(File => Data_File);
          exception
            when others => null;
          end;
        when others  =>
          begin
            Error_In_File := True;
            Text_Io.Close(File => Data_File);
          exception
            when others => null;
          end;
      end;

      if Error_In_File then
        Log("Error in file: '" & Full_Name(Work_File_Entry) & "' now moved from work to error");
        Rename(Full_Name(Work_File_Entry), Error_Directory & "/" & Simple_Name(Work_File_Entry));
      else
        Log("File ok: '" & Full_Name(Work_File_Entry) & "' now moved from work to log");
        Rename(Full_Name(Work_File_Entry), Log_Directory & "/" & Simple_Name(Work_File_Entry));
      end if;
    end loop;

    End_Search(Search_Work_File);
    Set_Directory(Cwd);
  end Treat_New_Files;

 
 procedure Check_For_New_Files is
    use Ada.Directories;
    use Ada.Calendar;
    Cwd : String := Current_Directory;
    Search_Ftp_File : Search_Type;
    Ftp_File_Entry  : Directory_Entry_Type;
    Last_MF_Time    : Time;
  begin
    Set_Directory(Incoming_FTP_Directory);
    Start_Search(Search    => Search_Ftp_File,
                 Directory => Current_Directory,
                 Pattern   => Ftp_File_Pattern,
                 Filter    => Filter_Type'(Directory     => False,
                                           Ordinary_File => True,
                                           Special_File  => False));
    loop
      exit when not More_Entries(Search_Ftp_File);
      Get_Next_Entry(Search_Ftp_File, Ftp_File_Entry);
      Last_MF_Time := Modification_Time(Ftp_File_Entry);
      delay 5.0;
      if Modification_Time(Ftp_File_Entry) = Last_MF_Time then -- file has not grown in 5 secs -> ok
        Log("File found: '" & Full_Name(Ftp_File_Entry) & "' now moved from ftp to work");
        Rename(Full_Name(Ftp_File_Entry), Work_Directory & "/" & Simple_Name(Ftp_File_Entry));
      end if;
    end loop;

    End_Search(Search_Ftp_File);
    Set_Directory(Cwd);
  end Check_For_New_Files;


procedure Read_Files_From_Work is
--here, read files in 'work'
    use Ada.Directories;
    Cwd                 : String                    := Current_Directory;
    Search_Work_File    : Search_Type;
    Work_File_Entry     : Directory_Entry_Type;
    Work_File           : Text_Io.File_Type; 
    Buf                 : String_Buffer;
    Scan_Data           : Scanner_Data_Type;
    Len                 : Positive                  := 1;
  begin
    Set_Directory(Work_Directory);
    Start_Search(Search    => Search_Work_File,
                 Directory => Current_Directory,
                 Pattern   => Ftp_File_Pattern,
                 Filter    => Filter_Type'(Directory     => False,
                                           Ordinary_File => True,
                                           Special_File  => False));
    loop
      exit when not More_Entries(Search_Work_File);
      Get_Next_Entry(Search_Work_File, Work_File_Entry);
      begin
        Log("Open file: '" & Full_Name(Work_File_Entry) & "'");
        Text_Io.Open(File => Work_File,
                     Mode => Text_Io.In_File,
                     Name => Full_Name(Work_File_Entry));
        Read_File_Loop :
        loop
          Text_Io.Get_Line(Work_File,Buf,Len);
          Split(Buf(1..Len),Scan_Data);
          Scan_Handler.Put_Load_Route_Connection(Scan_Data);
        end loop Read_File_Loop;
      exception
        when others  =>
          begin
            Text_Io.Close(File => Work_File);
          exception
            when others => null;
          end;
      end;
    end loop;
    End_Search(Search_Work_File);
    Set_Directory(Cwd);
  end Read_Files_From_Work;
  --------------------------------------------------------------------------------


  procedure Cleanup(Target_Directory : in String;
                    Days             : in Integer) is
    use Ada.Directories;
    use Ada.Calendar;
    Cwd             : String := Current_Directory;
    Search_Ftp_File : Search_Type;
    Ftp_File_Entry  : Directory_Entry_Type;
    Now             : Time := Clock;
  begin
    Set_Directory(Target_Directory);
    Start_Search(Search    => Search_Ftp_File,
                 Directory => Current_Directory,
                 Pattern   => Ftp_File_Pattern,
                 Filter    => Filter_Type'(Directory     => False,
                                           Ordinary_File => True,
                                           Special_File  => False));
    loop
      exit when not More_Entries(Search_Ftp_File);
      Get_Next_Entry(Search_Ftp_File, Ftp_File_Entry);
      if (Now - Modification_Time(Ftp_File_Entry) > Days * 86_400.0) then
        Log("Delete: '" & Simple_Name(Ftp_File_Entry) & "'");
        Delete_File(Simple_Name(Ftp_File_Entry));
      end if;
    end loop;

    End_Search(Search_Ftp_File);
    Set_Directory(Cwd);
  end Cleanup;