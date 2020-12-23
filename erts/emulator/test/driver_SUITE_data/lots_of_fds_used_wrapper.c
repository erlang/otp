#if !defined(__WIN32__)
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#endif

int
main(int argc, char *argv[])
{
#if !defined(__WIN32__)

    char **exec_argv;
    int fds[12000];
    int max = sizeof(fds)/sizeof(fds[0]);
    int i;

    /* Open a bit more than 1024 file descriptors... */
    for (i = 0; i < max; i++) {
        fds[i] = open("/dev/null", 0, O_WRONLY);
        if (fds[i] < 0) {
            if (i < 1200)
                return 17; /* Not enough fds for the test... */
            max = i;
            break;
        }
    }

    /*
     * Close some of the latest fds to give room for
     * the emulators usage...
     */
    for (i = max-150; i < max; i++)
        close(fds[i]);

    if (argc < 2)
        return 1;

    /*
     * Ensure NULL pointer after last argument...
     */
    exec_argv = malloc(sizeof(char *)*argc);
    if (!exec_argv)
        return 2;

    for (i = 0; i < argc-1; i++) {
        /* printf("arg=%d: %s\n", i, argv[i+1]); */
        exec_argv[i] = argv[i+1];
    }
    exec_argv[i] = NULL;

    execvp(exec_argv[0], exec_argv);

    perror("Failed to exec");

#endif

    return 3;
}
