#include <stdio.h>
#include "erl_driver.h"

static ErlDrvPort erlang_port;
static ErlDrvData easy_start(ErlDrvPort, char*);
static void easy_stop(ErlDrvData), easy_read(ErlDrvData, char*, ErlDrvSizeT);

static ErlDrvEntry easy_driver_entry =
{
    NULL,
    easy_start,
    easy_stop,
    easy_read,
    NULL,
    NULL,
    "easy",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

#ifdef __WIN32__
/*
 * Define a correct driver_init here, or the module won't compile.
 * Note that it will not actually be used.
 */
DRIVER_INIT(noinit_drv)

#else
/*
 * Provoke an error when loading the module.
 */
ErlDrvEntry* no_driver_init(void *handle)
#endif
{
    erlang_port = (ErlDrvPort)-1;
    return &easy_driver_entry;
}

static ErlDrvData easy_start(ErlDrvPort port,char *buf)
{
    if (erlang_port != (ErlDrvPort)-1) {
	return ERL_DRV_ERROR_GENERAL;
    }
    
    fprintf(stderr, "Easy driver started with args %s\n", buf);
    erlang_port = port;
    return (ErlDrvData)port;
}

static void easy_read(ErlDrvData port, char *buf, ErlDrvSizeT count)
{
    driver_output(erlang_port, buf, count);
}

static void easy_stop(ErlDrvData port)
{
    erlang_port = (ErlDrvPort)-1;
}

