#include <stdio.h>

int
main(int argc, char** argv)
{
    char buff[1];
    int res = read(stdin, buff, 1);
    printf("%d", res);
}
