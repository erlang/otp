#ifdef __WIN32__
#include <windows.h>

int wmain(int argc, wchar_t **argv)
{
    char* sep = "";
    int len;

    /*
     * Echo all arguments separated with '::', so that we can check that
     * quotes are interpreted correctly.
     */

    while (argc-- > 1) {
	char *utf8;
	len = WideCharToMultiByte(CP_UTF8, 0, argv[1], -1, NULL, 0, NULL, NULL);
	utf8 = malloc(len*sizeof(char));
	WideCharToMultiByte(CP_UTF8, 0, argv++[1], -1, utf8, len, NULL, NULL);
	printf("%s%s", sep, utf8);
	free(utf8);
	sep = "::";
    }
    putchar('\n');
    return 0;
}
#else

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
#endif
