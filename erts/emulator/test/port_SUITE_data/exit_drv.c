#include <stdlib.h>
#include "erl_driver.h"

typedef struct _erl_drv_data ExitDrvData;

static ExitDrvData *exit_drv_start(ErlDrvPort port, char *command);
static void         exit_drv_stop(ExitDrvData *data_p);
static void         exit_drv_output(ExitDrvData *data_p, char *buf,
				    ErlDrvSizeT len);
static void         exit_drv_finish(void);

static ErlDrvEntry exit_drv_entry = { 
    NULL, /* init */
    exit_drv_start,
    exit_drv_stop,
    exit_drv_output,
    NULL, /* ready_input */
    NULL, /* ready_output */
    "exit_drv",
    exit_drv_finish,
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

DRIVER_INIT(exit_drv)
{
    return &exit_drv_entry;
}

static ExitDrvData *
exit_drv_start(ErlDrvPort port, char *command)
{
    return (ExitDrvData *) port;
}

static void 
exit_drv_stop(ExitDrvData *datap)
{

}

static void
exit_drv_output(ExitDrvData *datap, char *buf, ErlDrvSizeT len)
{
    driver_exit((ErlDrvPort) datap, 0);
}

static void
exit_drv_finish(void)
{

}
