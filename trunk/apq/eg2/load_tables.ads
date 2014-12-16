-- $Id: load_tables.ads,v 1.1 2003/08/26 02:36:19 wwg Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ;
use APQ;

package Load_Tables is

   procedure Load_Passwd_Table(C : in out Root_Connection_Type'Class);
   procedure Load_Group_Table(C : in out Root_Connection_Type'Class);

end Load_Tables;

-- End $Source: /home/cvs/ada/apq/eg2/load_tables.ads,v $
