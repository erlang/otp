#include <stdio.h>

int
main(int argc, char** argv)
{
    char* sep = "";

    /*
     * Echo all arguments separated with '::', so that we can check that
     * quotes are interpreted correctly.
     */

    while (argc-- > 1) {
	printf("%s%s", sep, argv++[1]);
	sep = "::";
    }
    putchar('\n');
    return 0;
}
