#include <stdio.h>

#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif

int
main(int argc, char** argv)
{
#if defined (__WIN32__)
    printf("Windows");
#else
    char buff[1];
    int res = read(STDIN_FILENO, buff, 1);
    printf("%d", res);
#endif
    return 0;
}
