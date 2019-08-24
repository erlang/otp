/*
 * Author:  Bjorn Gustavsson
 * Purpose: Simple portable cat utility for the estone benchmark.
 *
 * Compiling instructions:
 *
 * Unix:    gcc -O2 -o estone_cat estone_cat.c
 * Windows: cl -Ox estone_cat.c
 */

#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>

main(argc, argv)
int argc;
char *argv[];
{
    char buf[16384];
    int n;

#ifdef _O_BINARY
    _setmode(0, _O_BINARY);
    _setmode(1, _O_BINARY);
#endif

    for (;;) {
	n = read(0, buf, sizeof(buf));
	if (n <= 0 && errno == EINTR)
	    continue;
	if (n <= 0)
	    break;
        write(1, buf, n);
    }
    return 0;
}
