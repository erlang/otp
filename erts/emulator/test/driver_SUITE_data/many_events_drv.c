#ifdef __WIN32__
#include <windows.h>
#endif

#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

static ErlDrvPort erlang_port;
static ErlDrvData many_events_start(ErlDrvPort, char *);
static void from_erlang(ErlDrvData, char*, ErlDrvSizeT);
static void from_port(ErlDrvData drv_data, ErlDrvEvent event);
static ErlDrvSSizeT many_events_call(ErlDrvData drv_data, unsigned int command,
				     char *buf, ErlDrvSizeT len,
				     char **rbuf, ErlDrvSizeT rlen,
				     unsigned *ret_flags);
static ErlDrvEntry many_events_driver_entry = { 
    NULL,			/* Init */
    many_events_start,
    NULL,			/* Stop */
    from_erlang,
    from_port,			/* Ready input */
    NULL,			/* Ready output */
    "many_events_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    many_events_call,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(many_events_drv)
{
    return &many_events_driver_entry;
}

static ErlDrvData
many_events_start(ErlDrvPort port, char *buf)
{
    return (ErlDrvData) port;
}

static void
from_erlang(ErlDrvData data, char *buf, ErlDrvSizeT count)
{
    int i;
    int num;
    char *b2 = driver_alloc(count + 1);
    char b3[1024];

    memcpy(b2,buf,count);
    b2[count] = '\0';

    num = atoi(b2);

    driver_free(b2);

    if(num < 0)
	num = 0;
#ifdef __WIN32__
    for (i = 0; i < num; ++i) {
	HANDLE ev = CreateEvent(NULL, TRUE, FALSE, NULL);
	
	if (ev == INVALID_HANDLE_VALUE || 
	    driver_select((ErlDrvPort) data, (ErlDrvEvent) ev, 
			  DO_READ, 1) != 0) {
	    break;
	}
	SetEvent(ev);
    }
#else
    i = num;
#endif
    sprintf(b3,"%d",i);
    driver_output((ErlDrvPort) data, b3, strlen(b3));
}

static void from_port(ErlDrvData data, ErlDrvEvent ev)
{
#ifdef __WIN32__
    /*static int counter = 0;*/
    driver_select((ErlDrvPort) data, (ErlDrvEvent) ev, 
		  DO_READ, 0);
    CloseHandle((HANDLE) ev);
    /*fprintf(stderr,"Close no %d\r\n",counter++);*/
#endif
    return;
}

static ErlDrvSSizeT
many_events_call(ErlDrvData drv_data, unsigned int command,
		 char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen,
		 unsigned *ret_flags)
{
    *rbuf = buf;
    *ret_flags |= DRIVER_CALL_KEEP_BUFFER;
    return len;
}

