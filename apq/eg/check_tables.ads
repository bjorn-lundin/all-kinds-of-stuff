-- $Id: check_tables.ads,v 1.5 2003/08/21 04:28:19 wwg Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ.PostgreSQL, APQ.PostgreSQL.Client;
use APQ, APQ.PostgreSQL, APQ.PostgreSQL.Client;

package Check_Tables is

   procedure Check_Userids(C : in out Connection_Type);
   procedure Check_Gid_Refs(C : in out Connection_Type);

end Check_Tables;

-- End $Source: /home/cvs/ada/apq/eg/check_tables.ads,v $
