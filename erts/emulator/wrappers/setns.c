#include <dlfcn.h>
#include <stdlib.h>
#include <stdio.h>

typedef int (*funptr)(int, int);
static funptr realptr = NULL;

int is_ns_available(void)
{
    /* According to libc(7) libc.so.6 means glibc ver 2.XX
     * and this is not going to change in the near future.
     * So if this lib is missing it may be musl, uClibc(-ng)
     * or any other exotic library, which won't work anyway(?) */
    int *libhandle = dlopen("libc.so.6", RTLD_LAZY);
    if (NULL == libhandle)
    {
        return 0;
    }
    realptr = (funptr)dlsym(libhandle, "setns");
    return (int)(realptr != NULL);
}

int __wrap_setns(int fd, int nstype)
{
    if (NULL != realptr)
    {
        return realptr(fd, nstype);
    }
    else
        return 0;
}

