/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd% 
 */

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#else
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>

#include "wxe_driver.h"

#define TEMP_BINARY_SIZE 512

static ErlDrvData wxe_driver_start(ErlDrvPort port, char *buff);
static int  wxe_driver_load(void);
static void wxe_driver_stop(ErlDrvData handle);
static void wxe_driver_unload(void);
static int  wxe_driver_control(ErlDrvData handle, unsigned int command,  
 			       char* buf, int count, char** res, int res_size); 
static int wxe_driver_call(ErlDrvData drv_data, unsigned int command, char *buf, int len, char **rbuf, int rlen, unsigned int *flags);

static void standard_outputv(ErlDrvData drv_data, ErlIOVec *ev);
static void wxe_process_died(ErlDrvData drv_data, ErlDrvMonitor *monitor);

int wxe_debug;

wxe_data * wxe_master;
char * erl_wx_privdir;

/*
** The driver struct
*/
static ErlDrvEntry wxe_driver_entry = {
   wxe_driver_load,	  /* F_PTR init,   called at loading */
   wxe_driver_start,      /* L_PTR start,  called when port is opened */
   wxe_driver_stop,       /* F_PTR stop,   called when port is closed  */
   NULL,	          /* F_PTR output, called when erlang has sent */
   NULL,                  /* F_PTR ready_input, called when input descriptor 
			     ready */
   NULL,                  /* F_PTR ready_output, called when output 
			     descriptor ready */
   "wxe_driver",          /* char *driver_name, the argument to open_port */
   wxe_driver_unload,     /* F_PTR finish, called when unloaded */
   NULL,                  /* void * that is not used (BC) */
   wxe_driver_control,     /* F_PTR control, port_control callback */
   NULL,                  /* F_PTR timeout, reserved */
   standard_outputv,	  /* F_PTR outputv, reserved */
   NULL,                  /* async */ 
   NULL,                  /* flush */
   wxe_driver_call,       /* call */
   NULL,                  /* Event */
   ERL_DRV_EXTENDED_MARKER,
   ERL_DRV_EXTENDED_MAJOR_VERSION,
   ERL_DRV_EXTENDED_MINOR_VERSION,
   ERL_DRV_FLAG_USE_PORT_LOCKING, /* Port lock */ 
   NULL,                  /* Reserved Handle */
   wxe_process_died,      /* Process Exited */
};

DRIVER_INIT(wxe_driver)
{
   return &wxe_driver_entry;
}

int wxe_driver_load() 
{
   if(load_native_gui())
      return 0;
   else 
      return -1;
}

ErlDrvPort WXE_DRV_PORT = 0;

static ErlDrvData 
wxe_driver_start(ErlDrvPort port, char *buff)
{      
   wxe_data *data;

   data = (wxe_data *) malloc(sizeof(wxe_data));
   wxe_debug = 0;
  
   if (data == NULL) {
      fprintf(stderr, " Couldn't alloc mem\r\n");
      return(ERL_DRV_ERROR_GENERAL);  /* ENOMEM */      
   } else {
      set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
      data->driver_data = NULL;
      data->bin = NULL; 
      data->port = port;
      data->pdl = driver_pdl_create(port);
      if(WXE_DRV_PORT == 0) {
	 for(; *buff != 32; buff++); 
	 buff++; 
	 erl_wx_privdir = strdup(buff);
	 
	 WXE_DRV_PORT = port;
	 wxe_master = data;
	 if(!(start_native_gui(data) == 1))
	    return(ERL_DRV_ERROR_GENERAL);  /* ENOMEM */
      } else {
	  meta_command(CREATE_PORT,data);
      }
      return (ErlDrvData) data;	 
   }
}

static void
wxe_driver_stop(ErlDrvData handle) 
{  
   wxe_data *sd = ((wxe_data *)handle);
   if(sd->port != WXE_DRV_PORT) {
      // fprintf(stderr, "%s:%d: STOP \r\n", __FILE__,__LINE__);
      meta_command(DELETE_PORT,sd);
      free(handle);
   }
}

static void
wxe_driver_unload(void) 
{
   // fprintf(stderr, "%s:%d: UNLOAD \r\n", __FILE__,__LINE__);
   stop_native_gui(wxe_master);
   unload_native_gui();
   free(wxe_master);
   wxe_master = NULL;
}

static int
wxe_driver_control(ErlDrvData handle, unsigned op,
		   char* buf, int count, char** res, int res_size)
{
   wxe_data *sd = ((wxe_data *)handle);
   push_command(op,buf,count,sd);
   return 0;
}

static int wxe_driver_call(ErlDrvData handle, unsigned int command, 
			   char *buf, int len, char **res, int rlen, unsigned int *flags)
{
   wxe_data *sd = ((wxe_data *)handle);
   if(command == WXE_DEBUG_DRIVER) {
      if(*buf) 
	 wxe_debug = 1;
      else
	 wxe_debug = 0;
   } else {
      meta_command(PING_PORT,sd);
   }
   if (len > rlen)
      *res = driver_alloc(len);
   memcpy((void *) *res, (void *) buf, len);
   return len;   
}


void wxe_process_died(ErlDrvData handle, ErlDrvMonitor *monitor)
{
   /* Callback is active for the dead process */
   wxe_data *sd = ((wxe_data *)handle);
   push_command(WXE_CB_RETURN,NULL,0,sd);

/*    ErlDrvTermData pid; */
/*    pid = driver_get_monitored_process(sd->port, monitor);    */
/*    fprintf(stderr, "Process died %d \r\n", (int) pid);  */
}


static void
standard_outputv(ErlDrvData drv_data, ErlIOVec* ev)
{
   wxe_data* sd = (wxe_data *) drv_data;
   WXEBinRef * binref;
   ErlDrvBinary* bin;
   
   if(ev->vsize == 2) {
      binref = driver_alloc(sizeof(WXEBinRef));
      binref->base = ev->iov[1].iov_base;
      binref->size = ev->iov[1].iov_len;
      binref->from = driver_caller(sd->port);
      bin = ev->binv[1];
      driver_binary_inc_refc(bin); /* Otherwise it could get deallocated */
      binref->bin = bin;
      binref->next = sd->bin;
      sd->bin = binref;      
   } else { /* Empty binary (becomes NULL) */ 
      binref = driver_alloc(sizeof(WXEBinRef));
      binref->base = NULL;
      binref->size = 0;
      binref->from = driver_caller(sd->port);
      binref->bin = NULL;
      binref->next = sd->bin;
      sd->bin = binref;
   }
}
