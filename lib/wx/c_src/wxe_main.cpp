/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

#if defined(_WIN32)
#include <wx/msw/private.h> // for wxSetInstance
#endif

#include "wxe_impl.h"

// Until fixed in emulator
#ifndef _WIN32
extern "C" {
  extern void erts_thread_disable_fpe(void);
}
#endif

ErlDrvTid wxe_thread;

ErlDrvMutex *wxe_status_m;
ErlDrvCond  *wxe_status_c;

int wxe_status = WXE_NOT_INITIATED;

ErlDrvMutex * wxe_batch_locker_m;
ErlDrvCond  * wxe_batch_locker_c;
ErlDrvTermData  init_caller = 0;

#ifdef __DARWIN__
extern "C" {
  int erl_drv_stolen_main_thread_join(ErlDrvTid tid, void **respp);
  int erl_drv_steal_main_thread(char *name,
				ErlDrvTid *dtid,
				void* (*func)(void*),
				void* arg,
				ErlDrvThreadOpts *opts);
}
#endif

void *wxe_main_loop(void * );

/* ************************************************************
 *  START AND STOP of driver thread
 * ************************************************************/

int load_native_gui()
{
  return 1;
}

int start_native_gui(wxe_data *sd)
{
  int res;
  wxe_status_m = erl_drv_mutex_create((char *) "wxe_status_m");
  wxe_status_c = erl_drv_cond_create((char *)"wxe_status_c");

  wxe_batch_locker_m = erl_drv_mutex_create((char *)"wxe_batch_locker_m");
  wxe_batch_locker_c = erl_drv_cond_create((char *)"wxe_batch_locker_c");
  init_caller = driver_connected(sd->port_handle);

#ifdef __DARWIN__
  res = erl_drv_steal_main_thread((char *)"wxwidgets",
				  &wxe_thread,wxe_main_loop,(void *) sd->pdl,NULL);
#else
  res = erl_drv_thread_create((char *)"wxwidgets",
			      &wxe_thread,wxe_main_loop,(void *) sd->pdl,NULL);
#endif
  if(res == 0) {
    erl_drv_mutex_lock(wxe_status_m);
    for(;wxe_status == WXE_NOT_INITIATED;) {
      erl_drv_cond_wait(wxe_status_c, wxe_status_m);
    }
    erl_drv_mutex_unlock(wxe_status_m);
    return wxe_status;
  } else {
    wxString msg;
    msg.Printf(wxT("Erlang failed to create wxe-thread %d\r\n"), res);
    send_msg("error", &msg);
    return -1;
  }
}

void stop_native_gui(wxe_data *sd)
{
  if(wxe_status == WXE_INITIATED) {
    meta_command(WXE_SHUTDOWN, sd);
  }
#ifdef __DARWIN__
  erl_drv_stolen_main_thread_join(wxe_thread, NULL);
#else
  erl_drv_thread_join(wxe_thread, NULL);
#endif
  erl_drv_mutex_destroy(wxe_status_m);
  erl_drv_cond_destroy(wxe_status_c);
  erl_drv_mutex_destroy(wxe_batch_locker_m);
  erl_drv_cond_destroy(wxe_batch_locker_c);
}

void unload_native_gui()
{

}

/* ************************************************************
 *  wxWidgets Thread
 * ************************************************************/

void *wxe_main_loop(void *vpdl)
{
  int result;
  int  argc = 1;
  const wxChar temp[10] = L"Erlang";
  wxChar * argv[] = {(wxChar *)temp, NULL};
  ErlDrvPDL pdl = (ErlDrvPDL) vpdl;

  driver_pdl_inc_refc(pdl);

  // Disable floating point execption if they are on.
  // This should be done in emulator but it's not in yet.
#ifndef _WIN32
  erts_thread_disable_fpe();
#else
  // Setup that wxWidgets should look for cursors and icons in
  // this dll and not in werl.exe (which is the default)
  HMODULE WXEHandle = GetModuleHandle(_T("wxe_driver"));
  wxSetInstance((HINSTANCE) WXEHandle);
#endif

  wxe_ps_init();
  result = wxEntry(argc, argv);
  // fprintf(stderr, "WXWidgets quits main loop %d \r\n", result);
  if(result >= 0 && wxe_status == WXE_INITIATED) {
    /* We are done try to make a clean exit */
    wxe_status = WXE_EXITED;
    driver_pdl_dec_refc(pdl);
#ifndef __DARWIN__
    erl_drv_thread_exit(NULL);
#endif
    return NULL;
  } else {
    erl_drv_mutex_lock(wxe_status_m);
    wxe_status = WXE_ERROR;
    erl_drv_cond_signal(wxe_status_c);
    erl_drv_mutex_unlock(wxe_status_m);
    driver_pdl_dec_refc(pdl);
    return NULL;
  }
}
