/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

#include <assert.h>
#include "wxe_driver.h"

#define TEMP_BINARY_SIZE 512

static ErlDrvData wxe_driver_start(ErlDrvPort port, char *buff);
static int  wxe_driver_load(void);
static void wxe_driver_stop(ErlDrvData handle);
static void wxe_driver_unload(void);
static ErlDrvSSizeT wxe_driver_control(ErlDrvData handle,
				       unsigned int command,  
				       char* buf, ErlDrvSizeT count,
				       char** res, ErlDrvSizeT res_size); 
static ErlDrvSSizeT wxe_driver_call(ErlDrvData drv_data, unsigned int command,
				    char *buf, ErlDrvSizeT len,
				    char **rbuf, ErlDrvSizeT rlen,
				    unsigned int *flags);

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

ErlDrvPort WXE_DRV_PORT_HANDLE = 0;
ErlDrvTermData WXE_DRV_PORT = 0;

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
      ErlDrvTermData term_port = driver_mk_port(port);
      set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
      data->driver_data = NULL;
      data->bin = (WXEBinRef*) driver_alloc(sizeof(WXEBinRef)*DEF_BINS);
      data->bin[0].from = 0;
      data->bin[1].from = 0;
      data->bin[2].from = 0;
      data->max_bins = DEF_BINS;
      data->port_handle = port;
      data->port = term_port;
      data->pdl = driver_pdl_create(port);
      if(WXE_DRV_PORT_HANDLE == 0) {
	 for(; *buff != 32; buff++); 
	 buff++; 
	 erl_wx_privdir = strdup(buff);
	 
	 WXE_DRV_PORT_HANDLE = port;
	 WXE_DRV_PORT = term_port;
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
   if(sd->port_handle != WXE_DRV_PORT_HANDLE) {
      // fprintf(stderr, "%s:%d: STOP \r\n", __FILE__,__LINE__);
      meta_command(DELETE_PORT,sd);
   } else {
       // fprintf(stderr, "%s:%d: STOP \r\n", __FILE__,__LINE__);
       stop_native_gui(wxe_master);
       unload_native_gui();
       free(wxe_master);
       wxe_master = NULL;
   }
}

static void
wxe_driver_unload(void) 
{
   // fprintf(stderr, "%s:%d: UNLOAD \r\n", __FILE__,__LINE__);
}

static ErlDrvSSizeT
wxe_driver_control(ErlDrvData handle, unsigned op,
		   char* buf, ErlDrvSizeT count,
		   char** res, ErlDrvSizeT res_size)
{
   wxe_data *sd = ((wxe_data *)handle);
   push_command(op,buf,count,sd);
   return 0;
}

static ErlDrvSSizeT
wxe_driver_call(ErlDrvData handle, unsigned int command, 
		char *buf, ErlDrvSizeT len,
		char **res, ErlDrvSizeT rlen, unsigned int *flags)
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
   push_command(WXE_CB_DIED,NULL,0,sd);

/*    ErlDrvTermData pid; */
/*    pid = driver_get_monitored_process(sd->port_handle, monitor);    */
/*    fprintf(stderr, "Process died %d \r\n", (int) pid);  */
}


static void
standard_outputv(ErlDrvData drv_data, ErlIOVec* ev)
{
   wxe_data* sd = (wxe_data *) drv_data;
   WXEBinRef * binref = NULL;
   ErlDrvBinary* bin;
   int i, max;

   for(i = 0; i < sd->max_bins; i++) {
       if(sd->bin[i].from == 0) {
	   binref = &sd->bin[i];
	   break;
       }
   }

   if(binref == NULL) { /* realloc */
       max = sd->max_bins + DEF_BINS;
       driver_realloc(sd->bin, sizeof(WXEBinRef)*max);
       for(i=sd->max_bins; i < max; i++) {
	   sd->bin[i].from = 0;
       }
       binref = &sd->bin[sd->max_bins];
       sd->max_bins = max;
   }

   if(ev->size > 0) {
       assert(ev->vsize == 2 && ev->iov[0].iov_len == 0
              && "erts changed how the ErlIOVec is structured for outputv");
       binref->from = driver_caller(sd->port_handle);
       binref->size = ev->iov[1].iov_len;
       if(ev->binv[1]) {
           binref->base = ev->iov[1].iov_base;
           bin = ev->binv[1];
           driver_binary_inc_refc(bin); /* Otherwise it could get deallocated */
       } else {
           bin = driver_alloc_binary(ev->iov[1].iov_len);
           memcpy(bin->orig_bytes, ev->iov[1].iov_base, ev->iov[1].iov_len);
           binref->base = bin->orig_bytes;
       }
       binref->bin = bin;
   } else { /* Empty binary (becomes NULL) */
      binref->base = NULL;
      binref->size = 0;
      binref->from = driver_caller(sd->port_handle);
      binref->bin = NULL;
   }
}
