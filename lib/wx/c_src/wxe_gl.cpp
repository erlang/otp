/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

#include <stdio.h>
#include <string.h>
#ifndef _WIN32
#include <dlfcn.h>
#endif
#include "wxe_impl.h"
#include "wxe_return.h"
#include "wxe_gl.h"

#define GLE_LIB_START 5000

/* ****************************************************************************
 *
 * Opengl context management *
 * This is wx part of opengl context and dispatch handling
 * it is part of the wxe_driver dll
 *
 * ****************************************************************************/

ErlNifUInt64 gl_active_index = 0;
ErlNifPid gl_active_pid;

int egl_initiated = 0;

wxeGLC glc;
typedef void * (*WXE_GL_LOOKUP) (int);
void * wxe_not_loaded(int x);
WXE_GL_LOOKUP wxe_gl_lookup_func = (WXE_GL_LOOKUP) wxe_not_loaded;
typedef void * (*WXE_GL_FUNC) (ErlNifEnv*, ErlNifPid*, const ERL_NIF_TERM argv[]);

typedef const char * (*WXE_GL_FUNC_NAME) (int);
WXE_GL_FUNC_NAME wxe_gl_lookup_func_name;

extern "C" {
void wxe_initOpenGL(void * fptr, void *name_fptr) {
  wxe_gl_lookup_func = (WXE_GL_LOOKUP) fptr;
  wxe_gl_lookup_func_name = (WXE_GL_FUNC_NAME) name_fptr;
  enif_set_pid_undefined(&gl_active_pid);
}
}

void * wxe_not_loaded(int x) {
  return NULL;
}

ErlNifUInt64 wxe_make_hash(ErlNifEnv *env, ErlNifPid *pid)
{
  ERL_NIF_TERM term = enif_make_pid(env, pid);
  return enif_hash(ERL_NIF_INTERNAL_HASH, term, 786234121);
}

void setActiveGL(wxeMemEnv *memenv, ErlNifPid caller, wxGLCanvas *canvas, wxGLContext *context)
{
  ErlNifUInt64 callId = wxe_make_hash(memenv->tmp_env, &caller);
  wxe_glc * entry = glc[callId];
  gl_active_index = callId;
  gl_active_pid = caller;

  if(!entry) {
    entry = (wxe_glc *) malloc(sizeof(wxe_glc));
    entry->canvas = NULL;
    entry->context = NULL;
  }

  if(entry->canvas == canvas && entry->context == context)
    return;

  entry->canvas = canvas;
  entry->context = context;
  glc[gl_active_index] = entry;
  //fprintf(stderr, "set caller %p => %p\r\n", caller, canvas);
  if(!egl_initiated && wxe_gl_lookup_func) {
    WXE_GL_FUNC fptr;
    if((fptr = (WXE_GL_FUNC) wxe_gl_lookup_func(GLE_LIB_START))) {
      fptr(memenv->tmp_env, &caller, NULL);
      egl_initiated = 1;
    }
  }
}

void deleteActiveGL(wxGLCanvas *canvas)
{
  gl_active_index = 0;
  enif_set_pid_undefined(&gl_active_pid);

  wxeGLC::iterator it;
  for(it = glc.begin(); it != glc.end(); ++it) {
    wxe_glc * temp = it->second;
    if(temp && temp->canvas == canvas) {
      it->second = NULL;
      free(temp);
    }
  }
}

void no_context(wxeCommand *event) {
  enif_send(NULL, &event->caller, event->env,
            enif_make_tuple3(event->env,
                             enif_make_atom(event->env, "_egl_error_"),
                             enif_make_int(event->env, event->op),
                             enif_make_atom(event->env, "no_gl_context")));
  enif_clear_env(event->env);
}

void gl_print_cmd(wxeCommand *event)
{
  int i;
  const char *func = wxe_gl_lookup_func_name(event->op);
  enif_fprintf(stderr, "  %T %d %s(", event->caller, event->op, func);
  for(i=0; i < event->argc; i++) {
    wx_print_term(event->env, event->args[i]);
    if(i < event->argc - 1)
      enif_fprintf(stderr, ", ");
  }
  enif_fprintf(stderr, ")\r\n");
}

void gl_dispatch(wxeCommand *event)
{
  WXE_GL_FUNC fptr;
  if((fptr = (WXE_GL_FUNC) wxe_gl_lookup_func(event->op))) {
    if(enif_compare_pids(&(event->caller),&gl_active_pid) != 0) {
      ErlNifUInt64 caller_index = wxe_make_hash(event->env, &(event->caller));
      wxe_glc * current = glc[caller_index];
      if(current) {
        wxe_glc * active = gl_active_index ? glc[gl_active_index] : NULL;
        if(!active || current->canvas != active->canvas || current->context != active->context) {
          current->canvas->SetCurrent(*current->context);
        }
        gl_active_index = caller_index;
        gl_active_pid = event->caller;
      } else {
        no_context(event);
        return;
      }
    }
    // enif_fprintf(stderr, "GL: caller %T gl_active %T %d\r\n", event->caller, gl_active_pid, event->op);
    if(wxe_debug) {
      gl_print_cmd(event);
    }
    fptr(event->env, &event->caller, event->args);
  } else {
    enif_send(NULL, &event->caller, event->env,
              enif_make_tuple3(event->env,
                               enif_make_atom(event->env, "_egl_error_"),
                               enif_make_int(event->env, event->op),
                               enif_make_atom(event->env, "undef")));
  }
  enif_clear_env(event->env);
}

