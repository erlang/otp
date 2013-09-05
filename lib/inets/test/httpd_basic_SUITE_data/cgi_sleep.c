#include <stdlib.h>
#include <stdio.h>

#ifdef __WIN32__
#include <windows.h>
#include <fcntl.h>
#include <io.h>
#else
#include <unistd.h>
#endif

int main(void)
{
    unsigned int seconds = 10;

#ifdef __WIN32__
    Sleep(seconds * 1000);
    _setmode(_fileno(stdout), _O_BINARY);
#else
    sleep(seconds);
#endif

    printf("Content-type: text/plain\r\n\r\n");
    printf("Slept for %u seconds.\r\n", seconds);
    exit(EXIT_SUCCESS);
}
