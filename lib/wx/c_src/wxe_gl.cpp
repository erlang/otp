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

#include <stdio.h>
#include <string.h>
#ifndef _WIN32
#include <dlfcn.h>
#else
#include <windows.h>
#endif
#include "wxe_impl.h"
#include "wxe_return.h"
#include "wxe_gl.h"

/* ****************************************************************************
 * Opengl context management *
 * ****************************************************************************/

int erl_gl_initiated = FALSE;
ErlDrvTermData gl_active = 0;
wxeGLC glc;

typedef void (*WXE_GL_DISPATCH) (int, char *, ErlDrvPort, ErlDrvTermData, char **, int *);
WXE_GL_DISPATCH wxe_gl_dispatch;

#ifdef _WIN32
#define RTLD_LAZY 0
typedef HMODULE DL_LIB_P;
void * dlsym(HMODULE Lib, const char *func) {
  void * funcp;
  if((funcp = (void *) GetProcAddress(Lib, func)))
    return funcp;
  else
    return (void *) wglGetProcAddress(func);
}

HMODULE dlopen(const char *path, int unused) {
  WCHAR * DLL;
  int len = MultiByteToWideChar(CP_ACP, 0, path, -1, NULL, 0);
  DLL = (WCHAR *) malloc(len * sizeof(WCHAR));
  MultiByteToWideChar(CP_ACP, 0, path, -1, DLL, len);
  HMODULE lib = LoadLibrary(DLL);
  free(DLL);
  return lib;
}

void dlclose(HMODULE Lib) {
  FreeLibrary(Lib);
}
#else
typedef void * DL_LIB_P;
#endif

void wxe_initOpenGL(wxeReturn *rt, char *bp) {
  DL_LIB_P LIBhandle;
  int (*init_opengl)(void *);
#ifdef _WIN32
  void * erlCallbacks = &WinDynDriverCallbacks;
#else 
  void * erlCallbacks = NULL;
#endif
  
  if(erl_gl_initiated == FALSE) {
    if((LIBhandle = dlopen(bp, RTLD_LAZY))) {
      *(void **) (&init_opengl) = dlsym(LIBhandle, "egl_init_opengl");
      wxe_gl_dispatch = (WXE_GL_DISPATCH) dlsym(LIBhandle, "egl_dispatch");
      if(init_opengl && wxe_gl_dispatch) {
	(*init_opengl)(erlCallbacks);
	rt->addAtom((char *) "ok");
	rt->add(wxString::FromAscii("initiated"));
	rt->addTupleCount(2);
	erl_gl_initiated = TRUE;
      } else {
	wxString msg;
	msg.Printf(wxT("In library: "));
	msg += wxString::FromAscii(bp);
	msg += wxT(" functions: ");
	if(!init_opengl) 
	  msg += wxT("egl_init_opengl ");
	if(!wxe_gl_dispatch) 
	  msg += wxT("egl_dispatch ");
	rt->addAtom((char *) "error");
	rt->add(msg);
	rt->addTupleCount(2);
      }
    } else {
      wxString msg;
      msg.Printf(wxT("Could not load dll: "));
      msg += wxString::FromAscii(bp);
      rt->addAtom((char *) "error");
      rt->add(msg);
      rt->addTupleCount(2);
    }
  } else {
    rt->addAtom((char *) "ok");
    rt->add(wxString::FromAscii("already initilized"));
    rt->addTupleCount(2);
  }
  rt->send();
}

void setActiveGL(ErlDrvTermData caller, wxGLCanvas *canvas)
{
  gl_active = caller;
  glc[caller] = canvas;
}

void deleteActiveGL(wxGLCanvas *canvas)
{
  gl_active = 0;
  wxeGLC::iterator it;
  for(it = glc.begin(); it != glc.end(); ++it) {
    if(it->second == canvas) { 
      it->second = (wxGLCanvas *) 0;
    }
  }
}

void gl_dispatch(int op, char *bp,ErlDrvTermData caller,WXEBinRef *bins){
  if(caller != gl_active) {
    wxGLCanvas * current = glc[caller];
    if(current) {
      if(current != glc[gl_active]) {
	current->SetCurrent();
      }
      gl_active = caller;
    } else {
      ErlDrvTermData rt[] = // Error msg
	{ERL_DRV_ATOM, driver_mk_atom((char *) "_egl_error_"),
	 ERL_DRV_INT,  (ErlDrvTermData) op,
	 ERL_DRV_ATOM, driver_mk_atom((char *) "no_gl_context"),
	 ERL_DRV_TUPLE,3};
      erl_drv_send_term(WXE_DRV_PORT,caller,rt,8);
      return ;
    }
  };
  char * bs[3];
  int bs_sz[3];
  for(int i=0; i<3; i++) {
    if(bins[i].from) {
      bs[i] = bins[i].base;
      bs_sz[i] = bins[i].size;
    }
    else
      break;
  }
  wxe_gl_dispatch(op, bp, WXE_DRV_PORT_HANDLE, caller, bs, bs_sz);
}

