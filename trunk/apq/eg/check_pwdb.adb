-- $Id: check_pwdb.adb,v 1.5 2003/08/21 04:28:18 wwg Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ.PostgreSQL, APQ.PostgreSQL.Client;
with Check_Tables;

procedure Check_PwDB is
   use APQ.PostgreSQL, APQ.PostgreSQL.Client, Check_Tables;

   C : Connection_Type;
begin

   Set_DB_Name(C,"apq_eg");
   Set_Notify_Proc(C,Standard_Error_Notify);
   Connect(C);

   Check_Userids(C);
   Check_Gid_Refs(C);

   Disconnect(C);

end Check_PwDB;

-- End $Source: /home/cvs/ada/apq/eg/check_pwdb.adb,v $
