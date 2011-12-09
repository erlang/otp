/*
 * Copied from driver_SUITE and modified...
 */

#include <stdio.h>
#include "erl_driver.h"

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define START_TIMER 0
#define CANCEL_TIMER 1
#define DELAY_START_TIMER 2
#define TIMER 3
#define CANCELLED 4

static ErlDrvData timer_start(ErlDrvPort, char*);
static void timer_stop(ErlDrvData),
    timer_read(ErlDrvData, char*, ErlDrvSizeT),
    timer(ErlDrvData);

static ErlDrvEntry timer_driver_entry =
{
    NULL,
    timer_start,
    timer_stop,
    timer_read,
    NULL,
    NULL,
    "timer_driver",
    NULL,
    NULL,
    NULL,
    timer,
    NULL,
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

DRIVER_INIT(timer_drv)
{
    return &timer_driver_entry;
}

static ErlDrvData timer_start(ErlDrvPort port, char *buf)
{
    return (ErlDrvData)port;
}

/* set the timer, this is monitored from erlang measuring the time */
static void timer_read(ErlDrvData p, char *buf, ErlDrvSizeT len)
{
    ErlDrvPort port = (ErlDrvPort) p;
    char reply[1];

    if (buf[0] == START_TIMER) { 
	/* fprintf(stderr, "[timer_drv] Setting timeout: %i\n", get_int32(buf + 1)); */
	driver_set_timer(port, get_int32(buf + 1));
    } else if (buf[0] == CANCEL_TIMER) {
	/*	fprintf(stderr, "[timer_drv] Timer cancelled\n"); */
	driver_cancel_timer(port);
	reply[0] = CANCELLED;
	driver_output(port, reply, 1);
    }
}

static void timer_stop(ErlDrvData p)
{
    ErlDrvPort port = (ErlDrvPort) p;
    driver_cancel_timer(port);
}

static void timer(ErlDrvData port)
{
    char reply[1];

    /*   fprintf(stderr, "[timer_drv] timer timed out\n"); */
    reply[0] = TIMER;
    driver_output((ErlDrvPort)port, reply, 1);
}
