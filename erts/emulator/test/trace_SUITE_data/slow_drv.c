#include <stdio.h>
#include "erl_driver.h"

typedef struct _erl_drv_data {
    ErlDrvPort   erlang_port;
} EchoDrvData;

static EchoDrvData slow_drv_data, *slow_drv_data_p;

static EchoDrvData *slow_drv_start(ErlDrvPort port, char *command);
static void         slow_drv_stop(EchoDrvData *data_p);
static void         slow_drv_output(ErlDrvData drv_data, char *buf,
				    ErlDrvSizeT len);
static ErlDrvSSizeT slow_drv_control(ErlDrvData drv_data, unsigned int command,
                                     char *buf, ErlDrvSizeT len,
                                     char **rbuf, ErlDrvSizeT rlen);
static void         slow_drv_timeout(ErlDrvData drv_data);
static void         slow_drv_finish(void);

static ErlDrvEntry slow_drv_entry = { 
    NULL, /* init */
    slow_drv_start,
    slow_drv_stop,
    slow_drv_output,
    NULL, /* ready_input */
    NULL, /* ready_output */
    "slow_drv",
    slow_drv_finish,
    NULL, /* handle */
    slow_drv_control, /* control */
    slow_drv_timeout, /* timeout */
    NULL, /* outputv */
    NULL,  /* ready_async */
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

DRIVER_INIT(slow_drv)
{
    slow_drv_data_p = NULL;
    return &slow_drv_entry;
}

static EchoDrvData *slow_drv_start(ErlDrvPort port, char *command)
{
    if (slow_drv_data_p != NULL) {
	return ERL_DRV_ERROR_GENERAL;
    }
    slow_drv_data_p = &slow_drv_data;
    slow_drv_data_p->erlang_port = port;
    return slow_drv_data_p;
}

static void slow_drv_stop(EchoDrvData *data_p) {
    slow_drv_data_p = NULL;
}

static void slow_drv_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {
    EchoDrvData* data_p = (EchoDrvData *) drv_data;
    driver_output(data_p->erlang_port, buf, len);
}

static ErlDrvSSizeT slow_drv_control(ErlDrvData drv_data, unsigned int command,
                                     char *buf, ErlDrvSizeT len,
                                     char **rbuf, ErlDrvSizeT rlen)
{
    EchoDrvData* data_p = (EchoDrvData *) drv_data;
    memcpy(*rbuf,"ok",2);
    if (command == 1) {
        driver_set_timer(data_p->erlang_port, 0);
    } else {
        slow_drv_timeout(drv_data);
    }
    return 2;
}

static void slow_drv_timeout(ErlDrvData drv_data)
{
    /* Sleep for 500 msec */
    usleep(150000);
}

static void slow_drv_finish() {
    slow_drv_data_p = NULL;
}
