#include <stdio.h>

#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif

int
main(int argc, char** argv)
{
    char buff[1];
    int res = read(stdin, buff, 1);
    printf("%d", res);
}
