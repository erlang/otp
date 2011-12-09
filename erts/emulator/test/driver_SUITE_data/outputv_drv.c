#include <stdio.h>
#include "erl_driver.h"

static ErlDrvPort erlang_port;
static ErlDrvData outputv_start(ErlDrvPort, char*);
static void outputv_stop(ErlDrvData),
    outputv_read(ErlDrvData, char*, ErlDrvSizeT),
    outputv(ErlDrvData, ErlIOVec*);

static ErlDrvEntry outputv_driver_entry =
{
    NULL,
    outputv_start,
    outputv_stop,
    outputv_read,
    NULL,
    NULL,
    "outputv_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    outputv,
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

DRIVER_INIT(outputv_drv)
{
    erlang_port = (ErlDrvPort)-1;
    return &outputv_driver_entry;
}

static ErlDrvData outputv_start(ErlDrvPort port, char *buf)
{
    if (erlang_port != (ErlDrvPort)-1) {
	return ERL_DRV_ERROR_GENERAL;
    }
    
    erlang_port = port;
    return (ErlDrvData)port;
}

static void outputv_read(ErlDrvData port, char *buf, ErlDrvSizeT count)
{
    erlang_port = (ErlDrvPort)-1;
}

static void outputv_stop(ErlDrvData port)
{
    erlang_port = (ErlDrvPort)-1;
}

/* Erts outputv -> drv, echo it back */
static void outputv(ErlDrvData port, ErlIOVec* ev)
{
    driver_outputv(erlang_port, NULL, 0, ev, 0);
}








