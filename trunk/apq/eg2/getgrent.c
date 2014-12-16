/* $Id: getgrent.c,v 1.1 2003/08/26 02:36:19 wwg Exp $
 * Warren W. Gay VE3WWG
 *
 * Licensed under the ACL (Ada Community License)
 */
#include <stdio.h>
#include <sys/types.h>
#include <grp.h>

int
main(int argc,char **argv) {
	struct group *gr;
	char **mp;

	while ( (gr = getgrent()) != NULL ) {
		printf("%s:%s:%d:",
			gr->gr_name,
			gr->gr_passwd,
			gr->gr_gid);
		for ( mp=gr->gr_mem; *mp != NULL; ++mp ) {
			if ( mp != gr->gr_mem )
				putchar(',');
			fputs(*mp,stdout);
		}
		putchar('\n');
	}

	return 0;
}

/* End $Source: /home/cvs/ada/apq/eg2/getgrent.c,v $ */
