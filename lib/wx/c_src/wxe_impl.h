/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2014. All Rights Reserved.
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

#ifndef _WXE_IMPL_H
#define	_WXE_IMPL_H

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif


#include <wx/glcanvas.h>
#include <wx/treectrl.h>
#include <wx/print.h>
extern "C" {
#include "wxe_driver.h"
}

#include "wxe_helpers.h"
#include "wxe_callback_impl.h"
#include "wxe_memory.h"

#if !wxCHECK_VERSION(2,9,0)
#define wxeLocaleC wxChar *
#define wxeLocaleC2String(Str) wxString(Str)
#else
typedef wxString wxeLocaleC;
#define wxeLocaleC2String(Str) Str
#endif

#define WXE_NOT_INITIATED 0
#define WXE_INITIATED     1
#define WXE_EXITED        2
#define WXE_ERROR        -1

void send_msg(const char *, const wxString *);   // For debugging and error msgs

class WxeApp : public wxApp
{
public:
   virtual bool OnInit();
#ifdef  _MACOSX
  virtual void MacOpenFile(const wxString &filename);
#endif
  void shutdown(wxeMetaCommand& event);

  int dispatch(wxList *, int, int);
  void dispatch_cb(wxList * batch, wxList * temp, ErlDrvTermData process);

  void wxe_dispatch(wxeCommand& event);

  void idle(wxIdleEvent& event);
  void dispatch_cmds();

  void dummy_close(wxEvent& Ev);
  bool sendevent(wxEvent *event);

  // MemEnv handling
  void newMemEnv(wxeMetaCommand& event);
  void destroyMemEnv(wxeMetaCommand& event);
  wxeMemEnv * getMemEnv(ErlDrvTermData port);

  int  newPtr(void * ptr, int type, wxeMemEnv *memenv);
  int  getRef(void * ptr, wxeMemEnv *memenv);
  void * getPtr(char * bp, wxeMemEnv *memenv);
  void clearPtr(void *ptr);
  wxeRefData * getRefData(void *ptr);
  void registerPid(char *ptr, ErlDrvTermData pid, wxeMemEnv *memenv);
  void init_nonconsts(wxeMemEnv *memenv, ErlDrvTermData caller);

  // Code found in gen/wxe_derived_dest.h
  bool delete_object(void *ptr, wxeRefData *refd);

  wxeMemMap refmap;
  ptrMap   ptr2ref;
  wxeMemEnv * global_me;

  int recurse_level;
  wxList * delayed_cleanup;
  wxList * delayed_delete;
  // Temp container for callbacks
  char *cb_buff;
  int  cb_len;
};

#endif  //_WXE_IMPL_H
