-- $Id: check_tables.ads,v 1.1 2003/08/26 02:36:19 wwg Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ;
use APQ;

package Check_Tables is

   procedure Check_Userids(C : in out Root_Connection_Type'Class);
   procedure Check_Gid_Refs(C : in out Root_Connection_Type'Class);

end Check_Tables;

-- End $Source: /home/cvs/ada/apq/eg2/check_tables.ads,v $
