/*
 * Author: Patrik Nyblom
 * Purpose: A port program to check the NFS cache size on VxWorks (returns 0
 * for other platforms).
 */

#ifdef VXWORKS
#include <vxWorks.h>
#include <taskVarLib.h>
#include <taskLib.h>
#include <sysLib.h>
#include <string.h>
#include <ioLib.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef VXWORKS
extern unsigned nfsCacheSize;
#define MAIN(argc, argv) nfs_check(argc, argv)
#else
#define MAIN(argc, argv) main(argc, argv)
#endif


MAIN(argc, argv)
int argc;
char *argv[];
{
#ifdef VXWORKS
    char str[100];
    sprintf(str,"%d\n", nfsCacheSize);
    write(1, str, strlen(str));
#else
    fprintf(stdout,"0");
    fflush(stdout);
#endif
    return 0;
}

