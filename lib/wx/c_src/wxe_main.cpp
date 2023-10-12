/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2021. All Rights Reserved.
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

ErlNifTid wxe_thread;

ErlNifMutex *wxe_status_m;
ErlNifCond  *wxe_status_c;

int wxe_status = WXE_NOT_INITIATED;

ErlNifMutex * wxe_batch_locker_m;
ErlNifCond  * wxe_batch_locker_c;
ErlNifPid init_caller;

#ifdef __DARWIN__
extern "C" {
  int erl_drv_stolen_main_thread_join(ErlNifTid tid, void **respp);
  int erl_drv_steal_main_thread(char *name,
				ErlNifTid *dtid,
				void* (*func)(void*),
				void* arg,
				ErlNifThreadOpts *opts);
}
#endif

void *wxe_main_loop(void * );

/* ************************************************************
 *  START AND STOP of driver thread
 * ************************************************************/

int start_native_gui(ErlNifEnv *env)
{
  int res;
  wxe_status_m = enif_mutex_create((char *) "wxe_status_m");
  wxe_status_c = enif_cond_create((char *)"wxe_status_c");

  wxe_batch_locker_m = enif_mutex_create((char *)"wxe_batch_locker_m");
  wxe_batch_locker_c = enif_cond_create((char *)"wxe_batch_locker_c");
  enif_self(env, &init_caller);

#ifdef __DARWIN__
  res = erl_drv_steal_main_thread((char *)"wxwidgets",
				  &wxe_thread,wxe_main_loop,(void *) NULL,NULL);
#else
  ErlNifThreadOpts *opts = enif_thread_opts_create((char *)"wx thread");
  opts->suggested_stack_size = 8192;
  res = enif_thread_create((char *)"wxwidgets",
                           &wxe_thread,wxe_main_loop,(void *) NULL,opts);
  enif_thread_opts_destroy(opts);
#endif
  if(res == 0) {
    enif_mutex_lock(wxe_status_m);
    for(;wxe_status == WXE_NOT_INITIATED;) {
      enif_cond_wait(wxe_status_c, wxe_status_m);
    }
    enif_mutex_unlock(wxe_status_m);
    return wxe_status;
  } else {
    wxString msg;
    msg.Printf(wxT("Erlang failed to create wxe-thread %d\r\n"), res);
    send_msg("error", &msg);
    return -1;
  }
}

void stop_native_gui(ErlNifEnv* env)
{
  if(wxe_status == WXE_INITIATED) {
    meta_command(env, WXE_SHUTDOWN, NULL);
  }
#ifdef __DARWIN__
  erl_drv_stolen_main_thread_join(wxe_thread, NULL);
#else
  enif_thread_join(wxe_thread, NULL);
#endif
  enif_mutex_destroy(wxe_status_m);
  enif_cond_destroy(wxe_status_c);
  enif_mutex_destroy(wxe_batch_locker_m);
  enif_cond_destroy(wxe_batch_locker_c);
}

/* ************************************************************
 *  wxWidgets Thread
 * ************************************************************/

void *wxe_main_loop(void * _unused)
{
  int result;
  int  argc = 1;
  wxChar temp[128] = L"Erlang";

  size_t app_len = 127;
  char app_title_buf[128];
  int res = enif_getenv("WX_APP_TITLE", app_title_buf, &app_len);
  if (res == 0) {
    wxString title = wxString::FromUTF8(app_title_buf);
    int size = title.Length() < 127 ? title.Length() : 126;
    for(int i = 0; i < size; i++) {
      temp[i] = title[i];
    }
    temp[size] = 0;
  }

  wxChar * argv[] = {(wxChar *)temp, NULL};

#ifdef _WIN32
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
#ifndef __DARWIN__
    enif_thread_exit(NULL);
#endif
    return NULL;
  } else {
    enif_mutex_lock(wxe_status_m);
    wxe_status = WXE_ERROR;
    enif_cond_signal(wxe_status_c);
    enif_mutex_unlock(wxe_status_m);
    // driver_pdl_dec_refc(pdl);
    return NULL;
  }
}
