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
#include <signal.h>


#include <wx/wx.h>

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
#include "wxe_gl.h"

IMPLEMENT_APP_NO_MAIN(WxeApp)

DECLARE_APP(WxeApp)

DEFINE_EVENT_TYPE(wxeEVT_META_COMMAND)

#define WXE_NORMAL      0
#define WXE_CALLBACK    1
#define WXE_STORED      2

// Globals initiated in wxe_init.cpp
extern ErlDrvMutex *wxe_status_m;
extern ErlDrvCond  *wxe_status_c;
extern ErlDrvMutex * wxe_batch_locker_m;
extern ErlDrvCond  * wxe_batch_locker_c;
extern ErlDrvTermData  init_caller;
extern int wxe_status;

wxeFifo * wxe_queue = NULL;

unsigned int wxe_needs_signal = 0;  // inside batch if larger than 0

/* ************************************************************
 *  Commands from erlang
 *    Called by emulator thread
 * ************************************************************/

void push_command(int op,char * buf,int len, wxe_data *sd)
{
  /* fprintf(stderr, "Op %d %d [%ld] %d\r\n", op, (int) driver_caller(sd->port_handle),
     wxe_batch->size(), wxe_batch_caller),fflush(stderr); */
  erl_drv_mutex_lock(wxe_batch_locker_m);
  wxe_queue->Add(op, buf, len, sd);

  if(wxe_needs_signal) {
    // wx-thread is waiting on batch end in cond_wait
    erl_drv_cond_signal(wxe_batch_locker_c);
    erl_drv_mutex_unlock(wxe_batch_locker_m);
  } else {
    // wx-thread is waiting gui-events
    erl_drv_mutex_unlock(wxe_batch_locker_m);
    wxWakeUpIdle();
  }
}

void meta_command(int what, wxe_data *sd) {
  if(what == PING_PORT && wxe_status == WXE_INITIATED) {
    erl_drv_mutex_lock(wxe_batch_locker_m);
    if(wxe_needs_signal) {
      wxe_queue->Add(WXE_DEBUG_PING, NULL, 0, sd);
      erl_drv_cond_signal(wxe_batch_locker_c);
    }
    wxWakeUpIdle();
    erl_drv_mutex_unlock(wxe_batch_locker_m);
  } else {
    if(sd && wxe_status == WXE_INITIATED) {
      wxeMetaCommand Cmd(sd, what);
      wxTheApp->AddPendingEvent(Cmd);
      if(what == DELETE_PORT) {
	driver_free(sd->bin);
	free(sd);
      }
    }
  }
}

void send_msg(const char * type, const wxString * msg) {
  wxeReturn rt = wxeReturn(WXE_DRV_PORT, init_caller);
  rt.addAtom((char *) "wxe_driver");
  rt.addAtom((char *) type);
  rt.add(msg);
  rt.addTupleCount(3);
  rt.send();
}

/* ************************************************************
 *  Init WxeApp the application emulator
 * ************************************************************/

bool WxeApp::OnInit()
{

  global_me = new wxeMemEnv();
  wxe_queue = new wxeFifo(2000);
  cb_buff = NULL;
  recurse_level = 0;
  delayed_delete = new wxeFifo(100);
  delayed_cleanup  = new wxList;

  wxe_ps_init2();
  wxIdleEvent::SetMode(wxIDLE_PROCESS_SPECIFIED);

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


#ifdef  _MACOSX
void WxeApp::MacOpenFile(const wxString &filename) {
  send_msg("open_file", &filename);
}
#endif

void WxeApp::shutdown(wxeMetaCommand& Ecmd) {
  wxe_status = WXE_EXITING;
  ExitMainLoop();
  delete wxe_queue;
}

void WxeApp::dummy_close(wxEvent& Ev) {
  // fprintf(stderr, "Dummy Close invoked\r\n");
  // wxMac really wants a top level window which command-q quits if there are no
  // windows open, and this will kill the erlang, override default handling
}

void WxeApp::OnAssertFailure(const wxChar *file, int line, const wxChar *cfunc,
			    const wxChar *cond, const wxChar *cmsgUser) {
  wxString msg;
  wxString func(cfunc);
  wxString msgUser(cmsgUser);

  msg.Printf(wxT("wxWidgets Assert failure: %s(%d): \"%s\""),
	     file, line, cond);
  if ( !func.empty() ) {
    msg << wxT(" in ") << func << wxT("()");
  }
  // and the message itself
  if ( !msgUser.empty() ) {
    msg << wxT(" : ") << msgUser;
  }

  send_msg("error", &msg);
}

// Called by wx thread
void WxeApp::idle(wxIdleEvent& event) {
  event.Skip(true);
  if(dispatch_cmds())
    event.RequestMore();
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

  if(wxe_status != WXE_INITIATED)
    return;

  // Is thread safe if pdl have been incremented
  if(driver_monitor_process(port, process, &monitor) == 0) {
    // Should we be able to handle commands when recursing? probably
    // fprintf(stderr, "\r\nCB EV Start %lu \r\n", process);fflush(stderr);
    app->recurse_level++;
    app->dispatch_cb(wxe_queue, process);
    app->recurse_level--;
    // fprintf(stderr, "CB EV done %lu \r\n", process);fflush(stderr);
    driver_demonitor_process(port, &monitor);
  }
}

int WxeApp::dispatch_cmds()
{
  int more = 0;
  if(wxe_status != WXE_INITIATED)
    return more;
  recurse_level++;
  // fprintf(stderr, "\r\ndispatch_normal %d\r\n", recurse_level);fflush(stderr);
  wxe_queue->cb_start = 0;
  more = dispatch(wxe_queue);
  // fprintf(stderr, "\r\ndispatch_done %d\r\n", recurse_level);fflush(stderr);
  recurse_level--;

  // Cleanup old memenv's and deleted objects
  if(recurse_level == 0) {
    wxeCommand *curr;
    while((curr = delayed_delete->Get()) != NULL) {
      wxe_dispatch(*curr);
      curr->Delete();
    }
    delayed_delete->Cleanup();
    if(delayed_cleanup->size() > 0)
      for( wxList::compatibility_iterator node = delayed_cleanup->GetFirst();
	   node;
	   node = delayed_cleanup->GetFirst()) {
	wxeMetaCommand *event = (wxeMetaCommand *)node->GetData();
	delayed_cleanup->Erase(node);
	destroyMemEnv(*event);
	delete event;
      }
  }
  return more;
}

#define BREAK_BATCH 200

int WxeApp::dispatch(wxeFifo * batch)
{
  int ping = 0;
  int blevel = 0;
  int wait = 0; // Let event handling generate events sometime
  wxeCommand *event;
  erl_drv_mutex_lock(wxe_batch_locker_m);
  while(true) {
    while((event = batch->Get()) != NULL) {
      erl_drv_mutex_unlock(wxe_batch_locker_m);
      switch(event->op) {
      case WXE_BATCH_END:
	if(blevel>0) {
          blevel--;
          if(blevel==0)
            wait += BREAK_BATCH*100;
        }
	break;
      case WXE_BATCH_BEGIN:
	blevel++;
	break;
      case WXE_DEBUG_PING:
	// When in debugger we don't want to hang waiting for a BATCH_END
	// that never comes, because a breakpoint have hit.
	ping++;
	if(ping > 2)
	  blevel = 0;
	break;
      case WXE_CB_RETURN:
	if(event->len > 0) {
	  cb_buff = (char *) driver_alloc(event->len);
	  memcpy(cb_buff, event->buffer, event->len);
	}
	event->Delete();
	return 1;
      default:
	if(event->op < OPENGL_START) {
	  // fprintf(stderr, "  c %d (%d) \r\n", event->op, blevel);
	  wxe_dispatch(*event);
	} else {
	  gl_dispatch(event->op,event->buffer,event->caller,event->bin);
	}
	break;
      }
      event->Delete();
      erl_drv_mutex_lock(wxe_batch_locker_m);
      batch->Cleanup();
    }
    if(blevel <= 0 || wait > BREAK_BATCH) {
      erl_drv_mutex_unlock(wxe_batch_locker_m);
      if(blevel > 0) {
        return 1; // We are still in a batch but we can let wx check for events
      } else {
        return 0;
      }
    }
    // sleep until something happens
    // fprintf(stderr, "%s:%d sleep %d %d %d\r\n", __FILE__, __LINE__, batch->m_n, blevel, wait);fflush(stderr);
    wxe_needs_signal = 1;
    wait += 1;
    while(batch->m_n == 0) {
      erl_drv_cond_wait(wxe_batch_locker_c, wxe_batch_locker_m);
    }
    wxe_needs_signal = 0;
  }
}

void WxeApp::dispatch_cb(wxeFifo * batch, ErlDrvTermData process) {
  wxeCommand *event;
  unsigned int peek;
  erl_drv_mutex_lock(wxe_batch_locker_m);
  peek = batch->Cleanup(batch->cb_start);
  while(true) {
    while((event = batch->Peek(&peek)) != NULL) {
      wxeMemEnv *memenv = getMemEnv(event->port);
      // fprintf(stderr, "  Ev %d %lu\r\n", event->op, event->caller);
      if(event->caller == process ||  // Callbacks from CB process only
	 event->op == WXE_CB_START || // Event callback start change process
	 event->op == WXE_CB_DIED ||  // Event callback process died
	 // Allow connect_cb during CB i.e. msg from wxe_server.
	 (memenv && event->caller == memenv->owner)) {
	erl_drv_mutex_unlock(wxe_batch_locker_m);
	switch(event->op) {
	case WXE_BATCH_END:
	case WXE_BATCH_BEGIN:
	case WXE_DEBUG_PING:
	  break;
	case WXE_CB_RETURN:
	  if(event->len > 0) {
	    cb_buff = (char *) driver_alloc(event->len);
	    memcpy(cb_buff, event->buffer, event->len);
	  }  // continue
	case WXE_CB_DIED:
	  batch->cb_start = 0;
	  event->Delete();
	  erl_drv_mutex_lock(wxe_batch_locker_m);
	  batch->Strip();
	  erl_drv_mutex_unlock(wxe_batch_locker_m);
	  return;
	case WXE_CB_START:
	  // CB start from now accept message from CB process only
	  process = event->caller;
	  break;
	default:
	  batch->cb_start = peek; // In case of recursive callbacks
	  if(event->op < OPENGL_START) {
	    wxe_dispatch(*event);
	  } else {
	    gl_dispatch(event->op,event->buffer,event->caller,event->bin);
	  }
	  break;
	}
	event->Delete();
	erl_drv_mutex_lock(wxe_batch_locker_m);
	peek = batch->Cleanup(peek);
      }
    }
    // sleep until something happens
    // fprintf(stderr, "%s:%d sleep %d %d\r\n", __FILE__, __LINE__,
    // 	    peek, batch->m_n);fflush(stderr);
    wxe_needs_signal = 1;
    while(peek >= batch->m_n) {
      erl_drv_cond_wait(wxe_batch_locker_c, wxe_batch_locker_m);
      peek = batch->Cleanup(peek);
    }
    wxe_needs_signal = 0;
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

void WxeApp::destroyMemEnv(wxeMetaCommand& Ecmd)
{
  // Clear incoming cmd queue first
  // dispatch_cmds();
  wxWindow *parent = NULL;
  wxeMemEnv * memenv = refmap[Ecmd.port];

  if(!memenv) {
    wxString msg;
    msg.Printf(wxT("MemEnv already deleted"));
    send_msg("debug", &msg);
    return;
  }

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
	if(refd->alloc_in_erl && refd->type == 2) {
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
	  if(recurse_level > 0) {
	    // Delay delete until we are out of dispatch*
	  } else {
	    delete win;
	  }
	}
      }
    }
  }

  if(recurse_level > 0) {
    delayed_cleanup->Append(Ecmd.Clone());
    return;
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
	  if((refd->type == 8) && ((wxObject *)ptr)->IsKindOf(CLASSINFO(wxBufferedDC))) {
	    ((wxBufferedDC *)ptr)->m_dc = NULL; // Workaround
	  }
	  wxString msg;
	  bool cleanup_ref=true;
	  if(refd->type == 0) { // Maybe also class 1
	    wxClassInfo *cinfo = ((wxObject *)ptr)->GetClassInfo();
	    msg.Printf(wxT("Memory leak: {wx_ref, %d, %s}"),
		       refd->ref, cinfo->GetClassName());
	    send_msg("error", &msg);
	  } else if(refd->type != 4) {
	    cleanup_ref = delete_object(ptr, refd);
	  }
	  if(cleanup_ref) {
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


wxeRefData * WxeApp::getRefData(void *ptr) {
  ptrMap::iterator it = ptr2ref.find(ptr);
  if(it != ptr2ref.end()) {
    wxeRefData *refd = it->second;
    return refd;
  }
  return NULL;
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
    const wxChar *class_info = wxT("unknown");
    if(type < 10) {
      wxClassInfo *cinfo = ((wxObject *)ptr)->GetClassInfo();
      class_info = cinfo->GetClassName();
    }
    msg.Printf(wxT("Creating {wx_ref, %d, %s} at %p "), ref, class_info, ptr);
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

    if(((int) refd->pid) != -1) {
      // Send terminate pid to owner
      wxeReturn rt = wxeReturn(WXE_DRV_PORT,refd->pid, false);
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
  if((index < memenv->next) && ((index == 0) || (temp != (void *)NULL)))
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
  if((index < memenv->next) && ((index == 0) || (temp != (void *) NULL))) {
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
