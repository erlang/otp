#include <stdio.h>
#include <string.h>
#include "erl_driver.h"



/* -------------------------------------------------------------------------
** Data types
**/

typedef struct _erl_drv_data EchoDrvData;



/* -------------------------------------------------------------------------
** Entry struct
**/

static EchoDrvData *echo_drv_start(ErlDrvPort port, char *command);
static void         echo_drv_stop(EchoDrvData *data_p);
static void         echo_drv_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
static ErlDrvSSizeT echo_control(ErlDrvData drv_data,
                                 unsigned int command, char *buf,
                                 ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);
static void         echo_outputv(ErlDrvData drv_data, ErlIOVec *ev);
static void         echo_drv_finish(void);

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
    echo_control, /* control */
    NULL, /* timeout */
    echo_outputv, /* outputv */
    NULL, /* ready_async */
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,
};



/* -------------------------------------------------------------------------
** Entry functions
**/

DRIVER_INIT(echo_drv)
{
    char buf[10];
    size_t bufsz = sizeof(buf);
    char *use_outputv;
    use_outputv = (erl_drv_getenv("ECHO_DRV_USE_OUTPUTV", buf, &bufsz) == 0
		   ? buf
		   : "false");
    if (strcmp(use_outputv, "true") != 0)
	echo_drv_entry.outputv = NULL;
    return &echo_drv_entry;
}

static EchoDrvData *echo_drv_start(ErlDrvPort port, char *command) {
    void *void_ptr;
    int res = -4711;
    if (command) {
	while(*command != '\0' && *command != ' ')
	    ++command;
	while(*command != '\0' && *command == ' ')
	    ++command;
	if(*command == '-') {
	    res = driver_output(port, command+1, strlen(command) - 1);
	}
    }	
    return void_ptr = port;
}

static void echo_drv_stop(EchoDrvData *data_p) {
}

static void echo_drv_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {
    EchoDrvData *data_p = (EchoDrvData *) drv_data;
    void *void_ptr;
    ErlDrvPort port = void_ptr = data_p;
    
    driver_output(port, buf, len);
}

static void echo_drv_finish() {
}

static ErlDrvSSizeT echo_control(ErlDrvData drv_data,
                                 unsigned int command, char *buf,
                                 ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    return 0;
}

static void echo_outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
    return;
}
