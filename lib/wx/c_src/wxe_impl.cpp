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

#include <stdio.h>
#include <signal.h>


#include <wx/wx.h>

#if defined(_WIN32)
#include <wx/msw/private.h> // for wxSetInstance
#endif

// Avoid including these in dcbuffer below
#include "wx/dcmemory.h"
#include "wx/dcclient.h"
#include "wx/window.h"
// Ok ugly but needed for wxBufferedDC crash workaround
#define private public
#include <wx/dcbuffer.h>

#undef private

#include "wxe_impl.h"
#include "wxe_events.h"
#include "wxe_return.h"

IMPLEMENT_APP_NO_MAIN(WxeApp)

DECLARE_APP(WxeApp)

DEFINE_EVENT_TYPE(wxeEVT_META_COMMAND)

#define WXE_NOT_INITIATED 0
#define WXE_INITIATED     1
#define WXE_EXITED        2
#define WXE_ERROR        -1

#define WXE_NORMAL      0
#define WXE_CALLBACK    1
#define WXE_STORED      2

ErlDrvTid wxe_thread;

ErlDrvMutex *wxe_status_m;
ErlDrvCond  *wxe_status_c;

ErlDrvMutex * wxe_batch_locker_m;
ErlDrvCond  * wxe_batch_locker_c;

static int wxe_status = WXE_NOT_INITIATED;

wxList * wxe_batch = NULL;
wxList * wxe_batch_cb_saved = NULL;

ErlDrvTermData  wxe_batch_caller = 0;
ErlDrvTermData  init_caller = 0;

// extern opengl
void gl_dispatch(int op, char *bp, ErlDrvTermData caller, WXEBinRef *bins[]);


// Until fixed in emulator
#ifndef _WIN32
extern "C" {
extern void erts_thread_disable_fpe(void);
}
#endif

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

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
 *  Commands from erlang
 *    Called by emulator thread
 * ************************************************************/

void push_command(int op,char * buf,int len, wxe_data *sd)
{
  // fprintf(stderr, "Op %d %d\r\n", op, (int) driver_caller(sd->port_handle)),fflush(stderr);
  wxeCommand *Cmd = new wxeCommand(op, buf, len, sd);
  erl_drv_mutex_lock(wxe_batch_locker_m);
  wxe_batch->Append(Cmd);

  if(wxe_batch_caller > 0) {
    // wx-thread is waiting on batch end in cond_wait
    erl_drv_cond_signal(wxe_batch_locker_c);
  } else {
    // wx-thread is waiting gui-events
    if(op == WXE_BATCH_BEGIN) {
      wxe_batch_caller = 1;
    }
    erl_drv_cond_signal(wxe_batch_locker_c);
    wxWakeUpIdle();
  }
  erl_drv_mutex_unlock(wxe_batch_locker_m);
}

void meta_command(int what, wxe_data *sd) {
  if(what == PING_PORT) {
    erl_drv_mutex_lock(wxe_batch_locker_m);
    if(wxe_batch_caller > 0) {
      wxeCommand *Cmd = new wxeCommand(WXE_DEBUG_PING, NULL, 0, sd);
      wxe_batch->Append(Cmd);
      erl_drv_cond_signal(wxe_batch_locker_c);
    }
    wxWakeUpIdle();
    erl_drv_mutex_unlock(wxe_batch_locker_m);
  } else {
    if(sd) {
      wxeMetaCommand Cmd(sd, what);
      wxTheApp->AddPendingEvent(Cmd);
    }
  }
}

/* ************************************************************
 *  wxWidgets Thread
 * ************************************************************/

void *wxe_main_loop(void *vpdl)
{
  int result;
  int  argc = 1;
  char * temp = (char *) "Erlang";
  char * argv[] = {temp,NULL};
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

void WxeApp::dummy_close(wxEvent& Ev) {
  // fprintf(stderr, "Dummy Close invoked\r\n");
  // wxMac really wants a top level window which command-q quits if there are no
  // windows open, and this will kill the erlang, override default handling
}


// Init wx-widgets thread
bool WxeApp::OnInit()
{

  global_me = new wxeMemEnv();
  wxe_batch = new wxList;
  wxe_batch_cb_saved = new wxList;
  cb_buff = NULL;

  wxe_ps_init2();
  // wxIdleEvent::SetMode(wxIDLE_PROCESS_SPECIFIED); // Hmm printpreview doesn't work in 2.9 with this

  Connect(wxID_ANY, wxEVT_IDLE,	(wxObjectEventFunction) (wxEventFunction) &WxeApp::idle);
  Connect(CREATE_PORT, wxeEVT_META_COMMAND,(wxObjectEventFunction) (wxEventFunction) &WxeApp::newMemEnv);
  Connect(DELETE_PORT, wxeEVT_META_COMMAND,(wxObjectEventFunction) (wxEventFunction) &WxeApp::destroyMemEnv);
  Connect(WXE_SHUTDOWN, wxeEVT_META_COMMAND,(wxObjectEventFunction) (wxEventFunction) &WxeApp::shutdown);

//   fprintf(stderr, "Size void* %d: long %d long long %d int64 %d \r\n",
// 	  sizeof(void *), sizeof(long), sizeof(long long), sizeof(wxInt64));
  initEventTable();
  wxInitAllImageHandlers();

#ifdef  _MACOSX
  /* Create a default MenuBar so that we can intercept the quit command */
  wxMenuBar *macMB = new wxMenuBar;
  wxMenuBar::MacSetCommonMenuBar(macMB);
  macMB->MacInstallMenuBar();
  macMB->Connect(wxID_ANY, wxEVT_COMMAND_MENU_SELECTED,
		 (wxObjectEventFunction) (wxEventFunction) &WxeApp::dummy_close);
#endif

  SetExitOnFrameDelete(false);

  init_nonconsts(global_me, init_caller);
  erl_drv_mutex_lock(wxe_status_m);
  wxe_status = WXE_INITIATED;
  erl_drv_cond_signal(wxe_status_c);
  erl_drv_mutex_unlock(wxe_status_m);
  return TRUE;
}

void WxeApp::shutdown(wxeMetaCommand& Ecmd) {
  ExitMainLoop();
}

void send_msg(const char * type, wxString * msg) {
  wxeReturn rt = wxeReturn(WXE_DRV_PORT, init_caller);
  rt.addAtom((char *) "wxe_driver");
  rt.addAtom((char *) type);
  rt.add(msg);
  rt.addTupleCount(3);
  rt.send();
}

/* ************************************************************
 *  Erlang Command execution  *
 * ************************************************************/

/* Callback from printer and event callbacks */
void pre_callback()
{
  // no-op
}

void handle_event_callback(ErlDrvPort port, ErlDrvTermData process)
{
  WxeApp * app = (WxeApp *) wxTheApp;
  ErlDrvMonitor monitor;
  driver_monitor_process(port, process, &monitor);
  // Should we be able to handle commands when recursing? probably
  erl_drv_mutex_lock(wxe_batch_locker_m);
  //fprintf(stderr, "\r\nCB EV Start %lu \r\n", process);fflush(stderr);
  app->dispatch_cb(wxe_batch, wxe_batch_cb_saved, process);
  //fprintf(stderr, "CB EV done %lu \r\n", process);fflush(stderr);
  wxe_batch_caller = 0;
  erl_drv_mutex_unlock(wxe_batch_locker_m);
  driver_demonitor_process(port, &monitor);
}

// Called by wx thread
void WxeApp::idle(wxIdleEvent& event) {
  event.Skip(true);
  dispatch_cmds();
}

void WxeApp::dispatch_cmds() {
  erl_drv_mutex_lock(wxe_batch_locker_m);
  int level = dispatch(wxe_batch_cb_saved, 0, WXE_STORED);
  dispatch(wxe_batch, level, WXE_NORMAL);
  wxe_batch_caller = 0;
  erl_drv_mutex_unlock(wxe_batch_locker_m);
}

// Should have  erl_drv_mutex_lock(wxe_batch_locker_m);
// when entering this function and it should be released
// afterwards
int WxeApp::dispatch(wxList * batch, int blevel, int list_type)
{
  int ping = 0;
  // erl_drv_mutex_lock(wxe_batch_locker_m);  must be locked already
  while(true)
    {
      if (batch->size() > 0) {
	for( wxList::compatibility_iterator node = batch->GetFirst();
	     node;
	     node = batch->GetFirst())
	  {
	    wxeCommand *event = (wxeCommand *)node->GetData();
	    batch->Erase(node);
	    switch(event->op) {
	    case WXE_BATCH_END:
	      {--blevel; }
	      break;
	    case WXE_BATCH_BEGIN:
	      {blevel++; }
	      break;
	    case WXE_DEBUG_PING:
	      // When in debugger we don't want to hang waiting for a BATCH_END
	      // that never comes, because a breakpoint have hit.
	      ping++;
	      if(ping > 2)
		blevel = 0;
	      break;
	    case WXE_CB_RETURN:
	      // erl_drv_mutex_unlock(wxe_batch_locker_m); should be called after
	      // whatever cleaning is necessary
	      if(event->len > 0) {
		cb_buff = (char *) driver_alloc(event->len);
		memcpy(cb_buff, event->buffer, event->len);
	      }
	      return blevel;
	    default:
	      erl_drv_mutex_unlock(wxe_batch_locker_m);
	      if(event->op < OPENGL_START) {
		// fprintf(stderr, "  c %d (%d) \r\n", event->op, blevel);
		wxe_dispatch(*event);
	      } else {
		gl_dispatch(event->op,event->buffer,event->caller,event->bin);
	      }
	      erl_drv_mutex_lock(wxe_batch_locker_m);
	      break;
	    }
	    delete event;
	  }
      } else {
	if((list_type == WXE_STORED) || (blevel <= 0 && list_type == WXE_NORMAL)) {
	  // erl_drv_mutex_unlock(wxe_batch_locker_m); should be called after
	  // whatever cleaning is necessary
	  return blevel;
	}
	// sleep until something happens
	//fprintf(stderr, "%s:%d sleep %d %d %d %d \r\n", __FILE__, __LINE__, batch->size(), callback_returned, blevel, is_callback);fflush(stderr);
	wxe_batch_caller++;
	while(batch->size() == 0) {
	  erl_drv_cond_wait(wxe_batch_locker_c, wxe_batch_locker_m);
	}
      }
    }
}

void WxeApp::dispatch_cb(wxList * batch, wxList * temp, ErlDrvTermData process) {
  int callback_returned = 0;
  while(true) {
    if (batch->size() > 0) {
      for( wxList::compatibility_iterator node = batch->GetFirst();
	   node;
	   node = batch->GetFirst())
	{
	  wxeCommand *event = (wxeCommand *)node->GetData();
	  wxeMemEnv *memenv = getMemEnv(event->port);
	  batch->Erase(node);
	  // fprintf(stderr, "  Ev %d %lu\r\n", event->op, event->caller);
	  if(event->caller == process ||  // Callbacks from CB process only
	     event->op == WXE_CB_START || // Event callback start change process
	     // Allow connect_cb during CB i.e. msg from wxe_server.
	     (memenv && event->caller == memenv->owner))
	    {
	      switch(event->op) {
	      case WXE_BATCH_END:
	      case WXE_BATCH_BEGIN:
	      case WXE_DEBUG_PING:
		break;
	      case WXE_CB_RETURN:
		if(event->len > 0) {
		  cb_buff = (char *) driver_alloc(event->len);
		  memcpy(cb_buff, event->buffer, event->len);
		}
		callback_returned = 1;
		return;
	      case WXE_CB_START:
		// CB start from now accept message from CB process only
		process = event->caller;
		break;
	      default:
		erl_drv_mutex_unlock(wxe_batch_locker_m);
		size_t start=temp->GetCount();
		if(event->op < OPENGL_START) {
		  // fprintf(stderr, "  cb %d \r\n", event->op);
		  wxe_dispatch(*event);
		} else {
		  gl_dispatch(event->op,event->buffer,event->caller,event->bin);
		}
		erl_drv_mutex_lock(wxe_batch_locker_m);
		if(temp->GetCount() > start) {
		  // We have recursed dispatch_cb and messages for this
		  // callback may be saved on temp list move them
		  // to orig list
		  for(wxList::compatibility_iterator node = temp->Item(start);
		      node;
		      node = node->GetNext()) {
		    wxeCommand *ev = (wxeCommand *)node->GetData();
		    if(ev->caller == process) {
		      batch->Append(ev);
		      temp->Erase(node);
		    }
		  }
		}
		if(callback_returned)
		  return;
		break;
	      }
	      delete event;
	    } else {
	    // fprintf(stderr, "  save %d \r\n", event->op);
	    temp->Append(event);
	  }
	}
    } else {
      if(callback_returned) {
	return;
      }
      // sleep until something happens
      //fprintf(stderr, "%s:%d sleep %d %d %d %d \r\n", __FILE__, __LINE__, batch->size(), callback_returned, blevel, is_callback);fflush(stderr);
      while(batch->size() == 0) {
	erl_drv_cond_wait(wxe_batch_locker_c, wxe_batch_locker_m);
      }
    }
  }
}

/* Memory handling */

void WxeApp::newMemEnv(wxeMetaCommand& Ecmd) {
  wxeMemEnv * memenv = new wxeMemEnv();

  driver_pdl_inc_refc(Ecmd.pdl);

  for(int i = 0; i < global_me->next; i++) {
    memenv->ref2ptr[i] = global_me->ref2ptr[i];
  }
  memenv->next = global_me->next;
  refmap[Ecmd.port] = memenv;
  memenv->owner = Ecmd.caller;

  ErlDrvTermData rt[] = {ERL_DRV_ATOM, driver_mk_atom((char *)"wx_port_initiated")};
  erl_drv_send_term(WXE_DRV_PORT,Ecmd.caller,rt,2);
}

void WxeApp::destroyMemEnv(wxeMetaCommand& Ecmd) {
  // Clear incoming cmd queue first
  // dispatch_cmds();
  wxWindow *parent = NULL;
  wxeMemEnv * memenv = refmap[Ecmd.port];

  if(wxe_debug) {
    wxString msg;
    msg.Printf(wxT("Destroying all memory "));
    send_msg("debug", &msg);
  }

  // pre-pass delete all dialogs first since they might crash erlang otherwise
  for(int i=1; i < memenv->next; i++) {
    wxObject * ptr = (wxObject *) memenv->ref2ptr[i];
    if(ptr) {
      ptrMap::iterator it = ptr2ref.find(ptr);
      if(it != ptr2ref.end()) {
	wxeRefData *refd = it->second;
	if(refd->alloc_in_erl) {
	  if(refd->type == 2) {
	    wxDialog *win = (wxDialog *) ptr;
	    if(win->IsModal()) {
	      win->EndModal(-1);
	    }
	    parent = win->GetParent();
	    if(parent) {
	      ptrMap::iterator parentRef = ptr2ref.find(parent);
	      if(parentRef == ptr2ref.end()) {
		// The parent is already dead delete the parent ref
		win->SetParent(NULL);
	      }
	    }
	    delete win;
	  }
	}
      }
    }
  }
  // First pass, delete all top parents/windows of all linked objects
  //   fprintf(stderr, "close port %x\r\n", Ecmd.port);fflush(stderr);

  for(int i=1; i < memenv->next; i++) {
    void * ptr = memenv->ref2ptr[i];
    if(ptr) {
      ptrMap::iterator it = ptr2ref.find(ptr);
      if(it != ptr2ref.end()) {
	wxeRefData *refd = it->second;
	if(refd->alloc_in_erl && refd->type == 0) {
	  parent = (wxWindow *) ptr;
	  // fprintf(stderr, "window %x %d\r\n", (int) parent, refd->ref);
	  while(parent->GetParent()) {
	    parent = parent->GetParent();
	    // fprintf(stderr, "  parent %x \r\n", (int) parent);
	  }
	  ptrMap::iterator pdata = ptr2ref.find(parent);
	  if(pdata != ptr2ref.end()) {
	    delete parent;
	  } // else parent is already deleted
	}
      } else {
	// fprintf(stderr, "Error found no ref in %d => %x\r\n", i, ptr);
      }
    }
  }
  // Second pass delete everything else allocated
  // everything linked from windows should now be deleted
  for(int i=1; i < memenv->next; i++) {
    void * ptr = memenv->ref2ptr[i];
    if(ptr) {
      ptrMap::iterator it = ptr2ref.find(ptr);
      if(it != ptr2ref.end()) {
	wxeRefData *refd = it->second;
	if(refd->alloc_in_erl) {
	  int type = refd->type;
	  if((refd->type == 1) && ((wxObject *)ptr)->IsKindOf(CLASSINFO(wxBufferedDC))) {
	    ((wxBufferedDC *)ptr)->m_dc = NULL; // Workaround
	  }
	  wxString msg;
	  if((refd->type == 0)) { // Maybe also class 1
	    wxClassInfo *cinfo = ((wxObject *)ptr)->GetClassInfo();
	    msg.Printf(wxT("Memory leak: {wx_ref, %d, %s}"),
		       refd->ref, cinfo->GetClassName());
	    send_msg("error", &msg);
	  } else {
	    delete_object(ptr, refd);
	  }
	  if(type == 0 || type > 2) {
	    // Delete refs for leaks and non overridden allocs
	    delete refd;
	    ptr2ref.erase(it);
	  } // overridden allocs deletes meta-data in clearPtr
	} else { // Not alloced in erl just delete references
	  if(refd->ref >= global_me->next) { // if it is not part of global ptrs
	    delete refd;
	    ptr2ref.erase(it);
	  }
	}
      }
    }
  }
// // Assert ?
// for(ptrMap::iterator it = ptr2ref.begin(); it != ptr2ref.end(); it++) {
//   wxeRefData *refd = it->second;
//   if(refd->ref >= global_me->next)
//     fprintf(stderr, "L %d %d %d\r\n", refd->ref, refd->type, refd->alloc_in_erl);
// }
//  fflush(stderr);
  delete memenv;
  driver_pdl_dec_refc(Ecmd.pdl);
  refmap.erase((ErlDrvTermData) Ecmd.port);
}

wxeMemEnv * WxeApp::getMemEnv(ErlDrvTermData port) {
  return refmap[port];
}

int WxeApp::newPtr(void * ptr, int type, wxeMemEnv *memenv) {
  int ref;
  intList free = memenv->free;

  if(free.IsEmpty()) {
    ref = memenv->next++;
  } else {
    ref = free.Pop();
  };
  if(ref >= memenv->max) {
    memenv->max *= 2;
    memenv->ref2ptr =
      (void **) driver_realloc(memenv->ref2ptr,memenv->max * sizeof(void*));
  }
  memenv->ref2ptr[ref] = ptr;

  if(wxe_debug) {
    wxString msg;
    msg.Printf(wxT("Creating {wx_ref, %d, unknown} at %p "), ref, ptr);
    send_msg("debug", &msg);
  }

  ptr2ref[ptr] = new wxeRefData(ref, type, true, memenv);
  // fprintf(stderr, "ptr %x id %d\r\n", (int) ptr,ref);
  return ref;
}

int WxeApp::getRef(void * ptr, wxeMemEnv *memenv) {
  if(!ptr) return 0;  // NULL and zero is the same
  ptrMap::iterator it = ptr2ref.find(ptr);
  if(it != ptr2ref.end()) {
    wxeRefData *refd = it->second;
    if(refd->memenv == memenv || refd->memenv == global_me) {
      // Found it return
      return refd->ref;
    } // else
    // Old reference to deleted object, release old and recreate in current memenv.
    ptr2ref.erase(it);
  }
  int ref;
  intList free = memenv->free;

  if(free.IsEmpty()) {
    ref = memenv->next++;
  } else {
    ref = free.Pop();
  };
  if(ref >= memenv->max) {
    memenv->max *= 2;
    memenv->ref2ptr =
      (void **) driver_realloc(memenv->ref2ptr,memenv->max * sizeof(void*));
  }

  memenv->ref2ptr[ref] = ptr;
  ptr2ref[ptr] = new wxeRefData(ref, 0, false, memenv);
  return ref;
}


void WxeApp::clearPtr(void * ptr) {
  ptrMap::iterator it;
  it = ptr2ref.find(ptr);

  if(it != ptr2ref.end()) {
    wxeRefData *refd = it->second;
    intList free = refd->memenv->free;
    int ref = refd->ref;
    refd->memenv->ref2ptr[ref] = NULL;
    free.Append(ref);

    if(wxe_debug) {
      wxString msg;
      msg.Printf(wxT("Deleting {wx_ref, %d, unknown} at %p "), ref, ptr);
      send_msg("debug", &msg);
    }

    if(((int) refd->pid) != -1) {
      // Send terminate pid to owner
      wxeReturn rt = wxeReturn(WXE_DRV_PORT,refd->memenv->owner, false);
      rt.addAtom("_wxe_destroy_");
      rt.add(ERL_DRV_PID, refd->pid);
      rt.addTupleCount(2);
      rt.send();
      refd->pid = -1;
    };
    if(refd->type == 1 && ((wxObject*)ptr)->IsKindOf(CLASSINFO(wxSizer))) {
      wxSizerItemList list = ((wxSizer*)ptr)->GetChildren();
      for(wxSizerItemList::compatibility_iterator node = list.GetFirst();
	  node; node = node->GetNext()) {
	wxSizerItem *item = node->GetData();
	wxObject *content=NULL;
	if((content = item->GetWindow()))
	  if(ptr2ref.end() == ptr2ref.find(content)) {
	    wxString msg;
	    wxClassInfo *cinfo = ((wxObject *)ptr)->GetClassInfo();
	    msg.Printf(wxT("Double usage detected of window at %p in sizer {wx_ref, %d, %s}"),
		       content, ref, cinfo->GetClassName());
	    send_msg("error", &msg);
	    ((wxSizer*)ptr)->Detach((wxWindow*)content);
	  }
	if((content = item->GetSizer()))
	  if(ptr2ref.end() == ptr2ref.find(content)) {
	    wxString msg;
	    wxClassInfo *cinfo = ((wxObject *)ptr)->GetClassInfo();
	    msg.Printf(wxT("Double usage detected of sizer at %p in sizer {wx_ref, %d, %s}"),
		       content, ref, cinfo->GetClassName());
	    send_msg("error", &msg);
	    ((wxSizer*)ptr)->Detach((wxSizer*)content);
	  }
      }
    }

    delete refd;
    ptr2ref.erase(it);
  }
}

void * WxeApp::getPtr(char * bp, wxeMemEnv *memenv) {
  int index = *(int *) bp;
  if(!memenv) {
    throw wxe_badarg(index);
  }
  void * temp = memenv->ref2ptr[index];
  if((index < memenv->next) && ((index == 0) || (temp > NULL)))
    return temp;
  else {
    throw wxe_badarg(index);
  }
}

void WxeApp::registerPid(char * bp, ErlDrvTermData pid, wxeMemEnv * memenv) {
  int index = *(int *) bp;
  if(!memenv)
    throw wxe_badarg(index);
  void * temp = memenv->ref2ptr[index];
  if((index < memenv->next) && ((index == 0) || (temp > NULL))) {
    ptrMap::iterator it;
    it = ptr2ref.find(temp);
    if(it != ptr2ref.end()) {
      wxeRefData *refd = it->second;
      refd->pid = pid;
      return ;
    }
  };
  throw wxe_badarg(index);
}


/* ************************************************************
 *  Misc utility classes
 * ************************************************************/

/* ****************************************************************************
 * Memory handling
 * ****************************************************************************/

wxeMemEnv::wxeMemEnv() {
  ref2ptr = (void **) driver_alloc(128*sizeof(void *));
  ref2ptr[0] = NULL;
  next = 1;
  max = 128;
}

wxeMemEnv::~wxeMemEnv() {
  driver_free(ref2ptr);
}

/* ****************************************************************************
 * Erlang Commands (don't need to be derived of wxEvent anymore should
 * be re-written to own class struct)
 * ****************************************************************************/

wxeCommand::wxeCommand(int fc,char * cbuf,int buflen, wxe_data *sd)
  : wxObject()
{
  WXEBinRef *temp, *start, *prev;
  int n = 0;
  caller = driver_caller(sd->port_handle);
  port   = sd->port;
  op = fc;
  len = buflen;
  bin[0] = NULL;
  bin[1] = NULL;
  bin[2] = NULL;

  if(cbuf) {
    buffer = (char *) driver_alloc(len);
    memcpy((void *) buffer, (void *) cbuf, len);;

    temp = sd->bin;

    prev  = NULL;
    start = temp;

    while(temp) {
      if(caller == temp->from) {
	bin[n++] = temp;
	if(prev) {
	  prev->next = temp->next;
	} else {
	  start = temp->next;
	}
	temp = temp->next;
      } else {
	prev = temp;
	temp = temp->next;
      }
    }
    sd->bin = start;
  } else {   // No-op only PING currently
    buffer = NULL;
  }
}

wxeCommand::~wxeCommand() {
  int n = 0;
  if(buffer) {
    while(bin[n]) {
      if(bin[n]->bin)
	driver_free_binary(bin[n]->bin);
      driver_free(bin[n++]);
    }
    driver_free(buffer);
  }
}

/* ****************************************************************************
 * TreeItemData
 * ****************************************************************************/

wxETreeItemData::wxETreeItemData(int sz, char * data) {
  size = sz;
  bin = (char *) driver_alloc(sz);
  memcpy(bin, data, sz);
}

wxETreeItemData::~wxETreeItemData()
{
  driver_free(bin);
}

/* ****************************************************************************
 * CallbackData *
 * ****************************************************************************/

wxeCallbackData::wxeCallbackData(ErlDrvTermData caller, int req, char *req_type,
				 int funcb, int skip_ev, wxeErlTerm * userData,
				 wxeEvtListener *handler_cb)
  : wxObject()
{
  listener = caller;
  obj = req;
  fun_id = funcb;
  strcpy(class_name, req_type);
  skip = skip_ev;
  user_data = userData;
  handler = handler_cb;
}

wxeCallbackData::~wxeCallbackData() {
  // fprintf(stderr, "CBD Deleteing %p %s\r\n", this, class_name); fflush(stderr);
  if(user_data) {
    delete user_data;
  }
  ptrMap::iterator it;
  it = ((WxeApp *)wxTheApp)->ptr2ref.find(handler);
  if(it != ((WxeApp *)wxTheApp)->ptr2ref.end()) {
    wxeRefData *refd = it->second;
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, refd->memenv->owner, false);
    rt.addAtom("wx_delete_cb");
    rt.addInt(fun_id);
    rt.addRef(refd->ref, "wxeEvtListener");
    rt.addRef(obj, class_name);
    rt.addTupleCount(4);
    rt.send();
  }
}

/* ****************************************************************************
 * wxListCtrlCompare wrapper
 * ****************************************************************************/

int wxCALLBACK wxEListCtrlCompare(long item1, long item2, long callbackInfoPtr)
{
  callbackInfo * cb = (callbackInfo *)callbackInfoPtr;
  wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(cb->port);
  wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);
  rt.addInt(cb->callbackID);
  rt.addInt(item1);
  rt.addInt(item2);
  rt.endList(2);
  rt.addAtom("_wx_invoke_cb_");
  rt.addTupleCount(3);
  rt.send();
  handle_event_callback(WXE_DRV_PORT_HANDLE, memenv->owner);

  if(((WxeApp *) wxTheApp)->cb_buff) {
    int res = * (int*) ((WxeApp *) wxTheApp)->cb_buff;
    driver_free(((WxeApp *) wxTheApp)->cb_buff);
    ((WxeApp *) wxTheApp)->cb_buff = NULL;
    return res;
  }
  return 0;
}
