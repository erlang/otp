#include <stdio.h>
#include "erl_driver.h"

typedef struct _erl_drv_data FailureDrvData;

static FailureDrvData *failure_drv_start(ErlDrvPort, char *);
static void failure_drv_stop(FailureDrvData *);
static void failure_drv_output(FailureDrvData *, char *, int);
static void failure_drv_finish(void);
static int failure_drv_control(FailureDrvData *, unsigned int,
			       char *, int, char **, int);

static ErlDrvEntry failure_drv_entry = { 
    NULL, /* init */
    failure_drv_start,
    failure_drv_stop,
    failure_drv_output,
    NULL, /* ready_input */
    NULL, /* ready_output */
    "failure_drv",
    failure_drv_finish,
    NULL, /* handle */
    failure_drv_control,
    NULL, /* timeout */
    NULL, /* outputv */
    NULL  /* ready_async */
};



/* -------------------------------------------------------------------------
** Entry functions
**/

DRIVER_INIT(failure_drv)
{
    return &failure_drv_entry;
}

static FailureDrvData *failure_drv_start(ErlDrvPort port, char *command) {
    void *void_ptr;
    
    return void_ptr = port;
}

static void failure_drv_stop(FailureDrvData *data_p) {
}

static void failure_drv_output(FailureDrvData *data_p, char *buf, int len) {
    void *void_ptr;
    ErlDrvPort port = void_ptr = data_p;
    
    driver_failure_atom(port, "driver_failed");
}

static void failure_drv_finish() {
}

static int failure_drv_control(FailureDrvData *data_p, unsigned int command,
			       char *buf, int len,
			       char **rbuf, int rlen) {
    return 0;
}
