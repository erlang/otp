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

#ifdef HAVE_GLIB
 #include <glib.h>
#endif

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
extern ErlNifMutex *wxe_status_m;
extern ErlNifCond  *wxe_status_c;
extern ErlNifMutex * wxe_batch_locker_m;
extern ErlNifCond  * wxe_batch_locker_c;
extern ErlNifPid  init_caller;
extern int wxe_status;

wxeFifo * wxe_queue = NULL;

unsigned int wxe_idle_processed = 0;
unsigned int wxe_needs_signal = 0;  // inside batch if larger than 0
unsigned int wxe_needs_wakeup = 0;  // inside batch if larger than 0

/* ************************************************************
 *  Commands from erlang
 *    Called by emulator thread
 * ************************************************************/
void push_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int op, wxe_me_ref *mp)
{
  ErlNifPid caller;

  if(!enif_self(env, &caller)) {
    caller = ((wxeMemEnv *) mp->memenv)->owner;
  }

  enif_mutex_lock(wxe_batch_locker_m);
  int n = wxe_queue->Add(argc, argv, op, mp, caller);

  if(wxe_needs_signal) {
    enif_cond_signal(wxe_batch_locker_c);
    enif_mutex_unlock(wxe_batch_locker_m);
  } else {
    // wx-thread is waiting gui-events
    int wakeup = wxe_needs_wakeup;
    wxe_needs_wakeup = 0;
    enif_mutex_unlock(wxe_batch_locker_m);
    if(n < 2 || wakeup || WXE_DEBUG_PING) {
      wxWakeUpIdle();
    }
  }
}

void meta_command(ErlNifEnv *env, int what, wxe_me_ref *mp) {
  int status;
  enif_mutex_lock(wxe_status_m);
  status = wxe_status;
  enif_cond_signal(wxe_status_c);
  enif_mutex_unlock(wxe_status_m);

  if(status == WXE_INITIATED) {
    ErlNifPid self;
    enif_self(env, &self);
    wxeMetaCommand Cmd(self, what, mp);
    wxTheApp->AddPendingEvent(Cmd);
  }
}

void send_msg(const char * type, const wxString * msg) {
  WxeApp * app = (WxeApp *) wxTheApp;
  wxeReturn rt = wxeReturn(app->global_me, init_caller);
  ErlNifEnv *env = enif_alloc_env();
  rt.env = env;
  ERL_NIF_TERM emsg = enif_make_tuple3(rt.env,
                                       rt.make_atom((char *) "wxe_driver"),
                                       rt.make_atom((char *) type),
                                       rt.make(msg));
  rt.send(emsg);
  enif_free_env(env);
}

void wx_print_term(ErlNifEnv * env, ERL_NIF_TERM t)
{
  if(enif_is_binary(env, t)) {
    ErlNifBinary bin;
    enif_inspect_binary(env, t, &bin);
    if(bin.size > 128) {
      enif_fprintf(stderr, "<<...LARGE BIN>");
    } else {
      enif_fprintf(stderr, "%T", t);
    }
  } else {
    enif_fprintf(stderr, "%T", t);
  }
}



void print_cmd(wxeCommand& event)
{
  int i;
  wxe_fns_t *func = &wxe_fns[event.op];
  enif_fprintf(stderr, "  %T %d %s::%s(", event.caller, event.op, func->cname, func->fname);
  for(i=0; i < event.argc; i++) {
    wx_print_term(event.env, event.args[i]);
    if(i < event.argc - 1)
      enif_fprintf(stderr, ", ");
  }
  enif_fprintf(stderr, ")\r\n");
}
 

/* ************************************************************
 *  Init WxeApp the application emulator
 * ************************************************************/

#ifdef HAVE_GLIB
static GLogWriterOutput wxe_log_glib(GLogLevelFlags log_level,
                                     const GLogField *fields,
                                     gsize n_fields,
                                     gpointer user_data)
{
  for (gsize i = 0; i < n_fields; i++) {
    if(strcmp(fields[i].key, "MESSAGE") == 0) {
      wxString msg;
      msg.Printf(wxT("GTK: %s"), (char *) fields[i].value);
      send_msg("debug", &msg);
    }
  }
  return G_LOG_WRITER_HANDLED;
}
#endif

bool WxeApp::OnInit()
{

  global_me = new wxeMemEnv();
  wxe_queue = new wxeFifo(2000);
  cb_return = NULL;
  recurse_level = 0;
  delayed_delete = new wxeFifo(100);
  delayed_cleanup  = new wxList;

  wxe_ps_init2();
  wxIdleEvent::SetMode(wxIDLE_PROCESS_SPECIFIED);

  Connect(wxID_ANY, wxEVT_IDLE,	(wxObjectEventFunction) (wxEventFunction) &WxeApp::idle);
  Connect(WXE_GET_CONSTS, wxeEVT_META_COMMAND,(wxObjectEventFunction) (wxEventFunction) &WxeApp::init_consts);
  Connect(WXE_DELETE_ENV, wxeEVT_META_COMMAND,(wxObjectEventFunction) (wxEventFunction) &WxeApp::destroyMemEnv);
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

#ifdef HAVE_GLIB
  g_log_set_writer_func(wxe_log_glib, NULL, NULL);
#endif

  SetExitOnFrameDelete(false);

  enif_mutex_lock(wxe_status_m);
  wxe_status = WXE_INITIATED;
  enif_cond_signal(wxe_status_c);
  enif_mutex_unlock(wxe_status_m);
  return TRUE;
}


#ifdef  _MACOSX
void WxeApp::MacPrintFile(const wxString &filename) {
  send_msg("print_file", &filename);
}

void WxeApp::MacOpenFile(const wxString &filename) {
  send_msg("open_file", &filename);
}

void WxeApp::MacOpenURL(const wxString &url) {
  send_msg("open_url", &url);
}

void WxeApp::MacNewFile() {
  wxString empty;
  send_msg("new_file", &empty);
}

void WxeApp::MacReopenApp() {
  wxString empty;
  send_msg("reopen_app", &empty);
}

// See: https://github.com/wxWidgets/wxWidgets/blob/v3.1.5/src/osx/cocoa/utils.mm#L76:L93
bool WxeApp::OSXIsGUIApplication() {
   char val_buf[8];
   size_t val_len = 7;
   int res = enif_getenv("WX_MACOS_NON_GUI_APP", val_buf, &val_len);
   if (res == 0) {
     return FALSE;
   } else {
     return TRUE;
   }
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

void handle_event_callback(wxe_me_ref *mr, ErlNifPid process)
{
  WxeApp * app = (WxeApp *) wxTheApp;
  ErlNifMonitor monitor;

  if(wxe_status != WXE_INITIATED)
    return;
  // enif_fprintf(stderr, "CB EV start %T \r\n", process);
  // Is thread safe if pdl have been incremented
  if(mr->memenv && enif_monitor_process(NULL, mr, &process, &monitor) == 0) {
    // Should we be able to handle commands when recursing? probably
    app->cb_return = NULL;
    app->recurse_level++;
    app->dispatch_cb(wxe_queue, (wxeMemEnv *) mr->memenv, process);
    app->recurse_level--;
    enif_demonitor_process(NULL, mr, &monitor);
  } else {
    // enif_fprintf(stderr, "CB %T is not alive ignoring\r\n", process);
    app->cb_return = NULL;
  }
  // enif_fprintf(stderr, "CB EV done %T \r\n", process);
}

int WxeApp::dispatch_cmds()
{
  int more = 0;
  if(wxe_status != WXE_INITIATED)
    return more;
  recurse_level++;
  // fprintf(stderr, "\r\ndispatch_normal %d\r\n", recurse_level);fflush(stderr);
  more = dispatch(wxe_queue);
  // fprintf(stderr, "\r\ndispatch_done %d\r\n", recurse_level);fflush(stderr);
  recurse_level--;

  // Cleanup old memenv's and deleted objects
  if(recurse_level == 0) {
    wxeCommand *curr;
    while((curr = delayed_delete->Get()) != NULL) {
      wxe_dispatch(*curr);
      delayed_delete->DeleteCmd(curr);
    }
    // delayed_delete->Cleanup();
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

#define CHECK_EVENTS 10000

int WxeApp::dispatch(wxeFifo * batch)
{
  int ping = 0;
  int blevel = 0;
  int wait = 0; // Let event handling generate events sometime
  wxeCommand *event;
  enif_mutex_lock(wxe_batch_locker_m);
  wxe_idle_processed = 1;
  while(true) {
    while((event = batch->Get()) != NULL) {
      wait += 1;
      switch(event->op) {
      case WXE_BATCH_END:
	if(blevel>0) {
          blevel--;
          if(blevel==0)
            wait += CHECK_EVENTS/4;
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
      case WXE_CB_START:
        // CB process died just ignore this
        break;
      case WXE_CB_RETURN:
        if(enif_is_identical(event->args[0], WXE_ATOM_ok)) {
          batch->DeleteCmd(event);
        } else {
          cb_return = event;   // must be deleted after taken care of
        }
        enif_mutex_unlock(wxe_batch_locker_m);
	return 1;
      default:
        enif_mutex_unlock(wxe_batch_locker_m);
	if(event->op < OPENGL_START) {
	  // fprintf(stderr, "  c %d (%d) \r\n", event->op, blevel);
	  wxe_dispatch(*event);
	} else {
	  gl_dispatch(event);
	}
        enif_mutex_lock(wxe_batch_locker_m);
	break;
      }
      if(wait > CHECK_EVENTS) {
        enif_mutex_unlock(wxe_batch_locker_m);
        return 1; // Let wx check for events
      }
      batch->DeleteCmd(event);
    }
    if(blevel <= 0) {
      enif_mutex_unlock(wxe_batch_locker_m);
      return 0;
    }
    // sleep until something happens
    // fprintf(stderr, "%s:%d sleep %d %d %d\r\n", __FILE__, __LINE__, batch->m_n, blevel, wait);fflush(stderr);
    wxe_needs_signal = 1;
    while(batch->m_q.empty()) {
      enif_cond_wait(wxe_batch_locker_c, wxe_batch_locker_m);
    }
    wxe_needs_signal = 0;
  }
}

void WxeApp::dispatch_cb(wxeFifo * batch, wxeMemEnv * memenv, ErlNifPid process) {
  wxeCommand *event;
  unsigned int peek = 0;
  enif_mutex_lock(wxe_batch_locker_m);
  unsigned int i = 0;
  unsigned int last = batch->m_q.size();
  wxe_idle_processed = 0;
  while(true) {

    while (i < last ) {
      event = batch->m_q[i];
      // enif_fprintf(stderr, "%d: CB %T owner %T  it %d %d (%d) \r\n",
      //              recurse_level, event ? event->caller : process, process,
      //              i, batch->Size(), batch->m_q.size());
      if(event &&
         (event->op == WXE_CB_START || // Event callback start change process
          event->op == WXE_CB_DIED ||  // Event callback process died
          event->op == WXE_DEBUG_PING ||
          enif_compare_pids(&event->caller, &process) == 0 ||  // Callbacks from CB process only
          // Allow connect_cb during CB i.e. msg from wxe_server.
          (memenv && enif_compare_pids(&event->caller,&memenv->owner) == 0)
          ))
        {
          // enif_fprintf(stderr, "Exec:"); print_cmd(*event);
          batch->DelQueue(i);
          switch(event->op) {
          case WXE_BATCH_END:
          case WXE_BATCH_BEGIN:
          case WXE_DEBUG_PING:
            break;
          case WXE_CB_RETURN:
            if(enif_is_identical(event->args[0], WXE_ATOM_ok)) {
              batch->DeleteCmd(event);
            } else {
              cb_return = event;   // must be deleted after taken care of
            }
            wxe_needs_wakeup = 1;
            enif_mutex_unlock(wxe_batch_locker_m);
            return;
          case WXE_CB_DIED:
            cb_return = NULL;
            batch->DeleteCmd(event);
            wxe_needs_wakeup = 1;
            enif_mutex_unlock(wxe_batch_locker_m);
            return;
          case WXE_CB_START:
            // CB start from now accept message from CB process only
            process = event->caller;
            break;
          default:
            enif_mutex_unlock(wxe_batch_locker_m);
            if(event->op < OPENGL_START) {
              wxe_dispatch(*event);
            } else {
              gl_dispatch(event);
            }
            enif_mutex_lock(wxe_batch_locker_m);
            last = batch->m_q.size();
            break;
          }
          batch->DeleteCmd(event);
        } else {
        // enif_fprintf(stderr, "Ignore:"); event ? print_cmd(*event) : fprintf(stderr, "NULL\r\n");
      }
      if(wxe_idle_processed) {
        // We have processed cmds inside dispatch()
        // so the iterator may be wrong, restart from
        // beginning of the queue
        i = 0;
        wxe_idle_processed = 0;
      } else {
        i++;
      }
    }
    // sleep until something happens
    // enif_fprintf(stderr, "\r\n%s:%d: %d: sleep sz %d (%d) it pos: %d\r\n", __FILE__, __LINE__, recurse_level,
    //              batch->Size(), batch->m_q.size(), i); fflush(stderr);
    wxe_needs_signal = 1;
    peek = batch->Size();
    while(peek >= batch->Size()) {
      enif_cond_wait(wxe_batch_locker_c, wxe_batch_locker_m);
    }
    wxe_needs_signal = 0;
    last = batch->m_q.size();
  }
}


void WxeApp::wxe_dispatch(wxeCommand& event)
{
  int op = event.op;
  wxe_fns_t *func = &wxe_fns[op];
  void (*nif_cb) (WxeApp *, wxeMemEnv *, wxeCommand& ) = func->nif_cb;
  wxeMemEnv * memenv = (wxeMemEnv *) event.me_ref->memenv;
  if(wxe_debug) {
    print_cmd(event);
  }
  if (event.me_ref->memenv) {
    if(nif_cb) {
      try { nif_cb(this, memenv, event); }
      catch (wxe_badarg badarg) {
        wxeReturn rt = wxeReturn(memenv, event.caller, false);
        ERL_NIF_TERM ba =
          enif_make_tuple2(rt.env,
                           WXE_ATOM_badarg,
                           enif_make_string(rt.env, badarg.var, ERL_NIF_LATIN1));
        ERL_NIF_TERM mfa = enif_make_tuple3(rt.env, enif_make_atom(rt.env, func->cname),
                                            enif_make_atom(rt.env, func->fname),
                                            rt.make_int(func->n));
        rt.send(enif_make_tuple4(rt.env, WXE_ATOM_error, rt.make_int(op), mfa, ba));
      }
    } else {
      wxeReturn rt = wxeReturn(memenv, event.caller, false);
      ERL_NIF_TERM undef = enif_make_atom(rt.env, "undefined_function");
      ERL_NIF_TERM mfa = enif_make_tuple3(rt.env, enif_make_atom(rt.env, func->cname),
                                          enif_make_atom(rt.env, func->fname),
                                          rt.make_int(func->n));
      rt.send(enif_make_tuple4(rt.env, WXE_ATOM_error, rt.make_int(op), mfa, undef));
    }
  } else {
    wxeReturn rt = wxeReturn(global_me, event.caller);
    ERL_NIF_TERM unknown_env = enif_make_atom(rt.env, "unknown_env");
    ERL_NIF_TERM mfa = enif_make_tuple3(rt.env, enif_make_atom(rt.env, func->cname),
                                        enif_make_atom(rt.env, func->fname),
                                        rt.make_int(func->n));
    rt.send(enif_make_tuple4(rt.env, WXE_ATOM_error, rt.make_int(op), mfa, unknown_env));
  }
}

/* Memory handling */

void * newMemEnv(ErlNifEnv* env, wxe_me_ref *mr)
{
  WxeApp * app = (WxeApp *) wxTheApp;
  wxeMemEnv* global_me = app->global_me;
  wxeMemEnv* memenv = new wxeMemEnv();
  memenv->create();

  for(int i = 0; i < global_me->next; i++) {
    memenv->ref2ptr[i] = global_me->ref2ptr[i];
  }
  memenv->next = global_me->next;
  enif_self(env, &memenv->owner);
  memenv->me_ref = mr;
  return memenv;
}

void WxeApp::destroyMemEnv(wxeMetaCommand &Ecmd)
{
  // Clear incoming cmd queue first
  dispatch_cmds();
  enif_mutex_lock(wxe_batch_locker_m);
  wxe_needs_wakeup = 1;
  enif_mutex_unlock(wxe_batch_locker_m);

  wxWindow *parent = NULL;

  if(!Ecmd.me_ref || !Ecmd.me_ref->memenv) {
    wxString msg;
    msg.Printf(wxT("MemEnv already deleted"));
    send_msg("debug", &msg);
    return;
  }
  wxeMemEnv *memenv = (wxeMemEnv *) Ecmd.me_ref->memenv;

  if(wxe_debug) {
    wxString msg;
    msg.Printf(wxT("Destroying all memory "));
    send_msg("debug", &msg);
  }

  // pre-pass delete all dialogs and DC's first since they might crash erlang otherwise
  for(int i=memenv->next-1; i > 0; i--) {
    wxObject * ptr = (wxObject *) memenv->ref2ptr[i];
    if(ptr) {
      ptrMap::iterator it = ptr2ref.find(ptr);
      if(it != ptr2ref.end()) {
	wxeRefData *refd = it->second;
	if(refd->alloc_in_erl && refd->type == 2) {
	  wxDialog *win = (wxDialog *) ptr;
	  if(win->IsModal()) { win->EndModal(-1); }
	  parent = win->GetParent();
	  if(parent) {
	    ptrMap::iterator parentRef = ptr2ref.find(parent);
            // if the parent is already dead delete the parent ref
	    if(parentRef == ptr2ref.end()) { win->SetParent(NULL); }
	  }
          // Delay delete until we are out of dispatch*
	  if(recurse_level == 0) { delete win; }
	} else if(refd->alloc_in_erl && refd->type == 8) {
	  if(delete_object(ptr, refd)) {
	    // Delete refs for leaks and non overridden allocs
	    delete refd;
	    ptr2ref.erase(it);
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
	} else { // Not allocated in erl just delete references
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
  enif_free(memenv->ref2ptr);
  enif_free_env(memenv->tmp_env);
  if(wxe_debug) enif_fprintf(stderr, "Deleting memenv %d\r\n", memenv);
  Ecmd.me_ref->memenv = NULL;
  enif_release_resource(Ecmd.me_ref);
}


wxeRefData * WxeApp::getRefData(void *ptr) {
  ptrMap::iterator it = ptr2ref.find(ptr);
  if(it != ptr2ref.end()) {
    wxeRefData *refd = it->second;
    return refd;
  }
  return NULL;
}


// wxeMemEnv * WxeApp::getMemEnv(ErlDrvTermData port) {
//   return refmap[port];
// }

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
    memenv->ref2ptr = (void **) enif_realloc(memenv->ref2ptr,memenv->max * sizeof(void*));
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

int WxeApp::getRef(void * ptr, wxeMemEnv *memenv, int type) {
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
    memenv->ref2ptr = (void **) enif_realloc(memenv->ref2ptr,memenv->max * sizeof(void*));
  }

  memenv->ref2ptr[ref] = ptr;
  ptr2ref[ptr] = new wxeRefData(ref, type, false, memenv);
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

    if(!enif_is_pid_undefined(&(refd->pid))) {
      // Send terminate pid to owner
      wxeReturn rt = wxeReturn(refd->memenv,refd->pid, false);
      rt.send(enif_make_tuple2(rt.env,
                               rt.make_atom("_wxe_destroy_"),
                               enif_make_pid(rt.env, &refd->pid)));
      enif_set_pid_undefined(&(refd->pid));
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


int WxeApp::registerPid(int index, ErlNifPid pid, wxeMemEnv * memenv) {
  void * temp = memenv->ref2ptr[index];
  if((index < memenv->next) && ((index == 0) || (temp != (void *) NULL))) {
    ptrMap::iterator it;
    it = ptr2ref.find(temp);
    if(it != ptr2ref.end()) {
      wxeRefData *refd = it->second;
      refd->pid = pid;
      return 1;
    }
  };
  return 0;
}
