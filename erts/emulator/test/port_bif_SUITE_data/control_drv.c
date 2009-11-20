#include <stdio.h>
#include <stdlib.h>
#include "erl_driver.h"


static ErlDrvPort erlang_port;
static ErlDrvData control_start(ErlDrvPort, char*);
static void control_stop(ErlDrvData);
static void control_read(ErlDrvData, char*, int);
static int control_control(ErlDrvData, unsigned int, char*, int, char**, int);

static ErlDrvEntry control_driver_entry =
{
    NULL,
    control_start,
    control_stop,
    control_read,
    NULL,
    NULL,
    "control_drv",
    NULL,
    NULL,
    control_control,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(control_drv)
{
    erlang_port = (ErlDrvPort)-1;
    return &control_driver_entry;
}

static ErlDrvData control_start(ErlDrvPort port,char *buf)
{
    if (erlang_port != (ErlDrvPort)-1)
	return ERL_DRV_ERROR_GENERAL;
    
    erlang_port = port;
    return (ErlDrvData)port;
}

static void control_read(ErlDrvData port, char *buf, int count)
{
    driver_output(erlang_port, buf, count);
}

static void control_stop(ErlDrvData port)
{
    erlang_port = (ErlDrvPort)-1;
}

static int control_control(ErlDrvData port, unsigned command, char* buf, int count,
			   char** res, int res_size)
{
    switch (command) {
    case 'e':
	if (count > res_size) {
	    *res = (char *) driver_alloc(count);
	}
	memcpy(*res, buf, count);
	return count;
    case 'b':
	set_busy_port(erlang_port, buf[0]);
	return 0;
    case 'i':
	driver_output(erlang_port, buf, count);
	return 0;
    default:
	if (command < 256) {
	    return -1;
	} else {
	    char* p = *res;
	    int i;

	    for (i = 3; i >= 0; i--) {
		p[i] = command;
		command >>= 8;
	    }
	    return 4;
	}
    }
}
