#include <stdio.h>
#include "erl_driver.h"

static ErlDrvPort erlang_port;
static ErlDrvData echo_start(ErlDrvPort, char *);
static void from_erlang(ErlDrvData, char*, int);
static int echo_call(ErlDrvData drv_data, unsigned int command, char *buf, 
		     int len, char **rbuf, int rlen, unsigned *ret_flags);
static int echo_failing_init(void);

static ErlDrvEntry echo_driver_entry = { 
    echo_failing_init,
    echo_start,
    NULL,			/* Stop */
    from_erlang,
    NULL,			/* Ready input */
    NULL,			/* Ready output */
    "echo_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    echo_call
};

DRIVER_INIT(echo_drv)
{
    return &echo_driver_entry;
}

static int echo_failing_init(void)
{
    return -1;
}

static ErlDrvData
echo_start(ErlDrvPort port, char *buf)
{
    return (ErlDrvData) port;
}

static void
from_erlang(ErlDrvData data, char *buf, int count)
{
    driver_output((ErlDrvPort) data, buf, count);
}

static int 
echo_call(ErlDrvData drv_data, unsigned int command, char *buf, 
	  int len, char **rbuf, int rlen, unsigned *ret_flags) 
{
    *rbuf = buf;
    *ret_flags |= DRIVER_CALL_KEEP_BUFFER;
    return len;
}

