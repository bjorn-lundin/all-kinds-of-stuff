-- $Id: load_tables.ads,v 1.6 2003/08/21 02:29:17 wwg Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ.PostgreSQL, APQ.PostgreSQL.Client;
use APQ.PostgreSQL, APQ.PostgreSQL.Client;

package Load_Tables is

   procedure Load_Passwd_Table(C : in out Connection_Type);
   procedure Load_Group_Table(C : in out Connection_Type);

end Load_Tables;

-- End $Source: /home/cvs/ada/apq/eg/load_tables.ads,v $
