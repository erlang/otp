#include <stdio.h>
#include "erl_driver.h"

typedef struct _erl_drv_data FailureDrvData;

static FailureDrvData *failure_drv_start(ErlDrvPort, char *);
static void failure_drv_stop(FailureDrvData *);
static void failure_drv_output(ErlDrvData, char *, ErlDrvSizeT);
static void failure_drv_finish(void);

static ErlDrvEntry failure_drv_entry = { 
    NULL, /* init */
    failure_drv_start,
    failure_drv_stop,
    failure_drv_output,
    NULL, /* ready_input */
    NULL, /* ready_output */
    "failure_drv",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
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

static void failure_drv_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {
    FailureDrvData *data_p = (FailureDrvData *) drv_data;
    void *void_ptr;
    ErlDrvPort port = void_ptr = data_p;
    
    driver_failure_atom(port, "driver_failed");
}

static void failure_drv_finish() {
}
