-- $Id: create_mysql.adb,v 1.1 2003/08/28 03:20:03 wwg Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with Ada.Text_IO;

with APQ.MySQL.Client;
use APQ.MySQL.Client;

with Load_Tables;
use Load_Tables;

procedure Create_MySQL is
   C : Connection_Type;
begin

   Set_DB_Name(C,"apq_eg");
   Connect(C);

   Load_Passwd_Table(C);
   Load_Group_Table(C);

   Disconnect(C);

end Create_MySQL;

-- End $Source: /home/cvs/ada/apq/eg2/create_mysql.adb,v $
