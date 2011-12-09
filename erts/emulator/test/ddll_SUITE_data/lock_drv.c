#include <stdio.h>
#include "erl_driver.h"

static ErlDrvPort erlang_port;
static ErlDrvData echo_start(ErlDrvPort, char *);
static void from_erlang(ErlDrvData, char*, ErlDrvSizeT);
static ErlDrvSSizeT echo_call(ErlDrvData drv_data, unsigned int command,
			      char *buf, ErlDrvSizeT len,
			      char **rbuf, ErlDrvSizeT rlen, unsigned *ret_flags);
static ErlDrvEntry echo_driver_entry = { 
    NULL,			/* Init */
    echo_start,
    NULL,			/* Stop */
    from_erlang,
    NULL,			/* Ready input */
    NULL,			/* Ready output */
    "lock_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    echo_call,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(echo_drv)
{
    return &echo_driver_entry;
}

static ErlDrvData
echo_start(ErlDrvPort port, char *buf)
{
    driver_lock_driver(port);
    return (ErlDrvData) port;
}

static void
from_erlang(ErlDrvData data, char *buf, ErlDrvSizeT count)
{
    driver_output((ErlDrvPort) data, buf, count);
}

static ErlDrvSSizeT
echo_call(ErlDrvData drv_data, unsigned int command,
	  char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen,
	  unsigned *ret_flags)
{
    ErlDrvPort port = (ErlDrvPort) drv_data;
    driver_lock_driver(port);
    *rbuf = buf;
    *ret_flags |= DRIVER_CALL_KEEP_BUFFER;
    return len;
}

