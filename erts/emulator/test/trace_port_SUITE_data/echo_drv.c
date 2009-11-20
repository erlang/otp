#include <stdio.h>
#include "erl_driver.h"



/* -------------------------------------------------------------------------
** Data types
**/

enum e_heavy {
    heavy_off, heavy_set, heavy_reset
};

typedef struct _erl_drv_data {
    ErlDrvPort   erlang_port;
    enum e_heavy heavy;
} EchoDrvData;

static EchoDrvData echo_drv_data, *echo_drv_data_p;



/* -------------------------------------------------------------------------
** Entry struct
**/

static EchoDrvData *echo_drv_start(ErlDrvPort port, char *command);
static void         echo_drv_stop(EchoDrvData *data_p);
static void         echo_drv_output(EchoDrvData *data_p, char *buf, int len);
static void         echo_drv_finish(void);
static int          echo_drv_control(EchoDrvData *data_p, unsigned int command,
				     char *buf, int len,
				     char **rbuf, int rlen);

static ErlDrvEntry echo_drv_entry = { 
    NULL, /* init */
    echo_drv_start,
    echo_drv_stop,
    echo_drv_output,
    NULL, /* ready_input */
    NULL, /* ready_output */
    "echo_drv",
    echo_drv_finish,
    NULL, /* handle */
    echo_drv_control,
    NULL, /* timeout */
    NULL, /* outputv */
    NULL  /* ready_async */
};



/* -------------------------------------------------------------------------
** Entry functions
**/

DRIVER_INIT(echo_drv)
{
    echo_drv_data_p = NULL;
    return &echo_drv_entry;
}

static EchoDrvData *echo_drv_start(ErlDrvPort port, char *command)
{
    if (echo_drv_data_p != NULL) {
	return ERL_DRV_ERROR_GENERAL;
    }
    echo_drv_data_p = &echo_drv_data;
    echo_drv_data_p->erlang_port = port;
    echo_drv_data_p->heavy = heavy_off;
    return echo_drv_data_p;
}

static void echo_drv_stop(EchoDrvData *data_p) {
    echo_drv_data_p = NULL;
}

static void echo_drv_output(EchoDrvData *data_p, char *buf, int len) {
    driver_output(data_p->erlang_port, buf, len);
    switch (data_p->heavy) {
    case heavy_off:
	break;
    case heavy_set:
	set_port_control_flags(data_p->erlang_port, PORT_CONTROL_FLAG_HEAVY);
	data_p->heavy = heavy_reset;
	break;
    case heavy_reset:
	set_port_control_flags(data_p->erlang_port, 0);
	data_p->heavy = heavy_off;
	break;
    }
}

static void echo_drv_finish() {
    echo_drv_data_p = NULL;
}

static int echo_drv_control(EchoDrvData *data_p, unsigned int command,
			    char *buf, int len,
			    char **rbuf, int rlen) {
    switch (command) {
    case 'h':
	data_p->heavy = heavy_set;
	break;
    }
    return 0;
}
