-- $Id: create_pwdb_main.adb,v 1.2 2003/08/26 04:15:09 wwg Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with Ada.Text_IO;

with APQ.PostgreSQL.Client;
use APQ.PostgreSQL.Client;

with Load_Tables;
use Load_Tables;

procedure Create_PwDb_Main is
   C : Connection_Type;
begin

   Set_DB_Name(C,"apq_eg");
   Set_Notify_Proc(C,Standard_Error_Notify);
   Connect(C);

   Load_Passwd_Table(C);
   Load_Group_Table(C);

   Disconnect(C);

end Create_PwDb_Main;

-- End $Source: /home/cvs/ada/apq/eg2/create_pwdb_main.adb,v $
