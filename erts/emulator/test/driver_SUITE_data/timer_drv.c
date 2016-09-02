#include <stdio.h>
#include "erl_driver.h"
#ifdef __WIN32__
#  include <windows.h>
#else
#  include <sys/time.h>
#  include <sys/types.h>
#  include <sys/select.h>
#  include <unistd.h>
#endif

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define START_TIMER 0
#define CANCEL_TIMER 1
#define DELAY_START_TIMER 2
#define TIMER 3
#define CANCELLED 4

static ErlDrvPort erlang_port;
static ErlDrvData timer_start(ErlDrvPort, char*);
static void timer_stop(ErlDrvData);
static void timer_read(ErlDrvData, char*, ErlDrvSizeT);
static void timer(ErlDrvData);
static void ms_sleep(int ms);

static ErlDrvEntry timer_driver_entry =
{
    NULL,
    timer_start,
    timer_stop,
    timer_read,
    NULL,
    NULL,
    "timer_drv",
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
    erlang_port = (ErlDrvPort)-1;
    return &timer_driver_entry;
}

static ErlDrvData timer_start(ErlDrvPort port, char *buf)
{
    if (erlang_port != (ErlDrvPort)-1) {
	return ERL_DRV_ERROR_GENERAL;
    }
    erlang_port = port;
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
    } else if (buf[0] == DELAY_START_TIMER) {
	ms_sleep(1000);
	driver_set_timer(port, get_int32(buf + 1));
    }
}

static void timer_stop(ErlDrvData port)
{
    erlang_port = (ErlDrvPort)-1;
}

static void timer(ErlDrvData port)
{
    char reply[1];

    /*   fprintf(stderr, "[timer_drv] timer timed out\n"); */
    reply[0] = TIMER;
    driver_output((ErlDrvPort)port, reply, 1);
}

static void
ms_sleep(int ms)
{
    /* Important that we do not return too early... */
    ErlDrvTime time, timeout_time;

    time = erl_drv_monotonic_time(ERL_DRV_USEC);

    timeout_time = time + ((ErlDrvTime) ms)*1000;

    while (time < timeout_time) {
	ErlDrvTime timeout = timeout_time - time;

#ifdef __WIN32__
	Sleep((DWORD) (timeout / 1000));
#else
	{
	    struct timeval tv;

	    tv.tv_sec = (long) timeout / (1000*1000);
	    tv.tv_usec = (long) timeout % (1000*1000);

	    select(0, NULL, NULL, NULL, &tv);
	}
#endif

	time = erl_drv_monotonic_time(ERL_DRV_USEC);
    }

}
