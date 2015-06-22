/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013-2013. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "errno.h"
#include "stdio.h"
#include "string.h"
#include "stddef.h"

#include "sys.h"
#include "erl_driver.h"
#include "ose.h"


#ifdef HAVE_OSE_SPI_H
#include "ose_spi/ose_spi.h"
#endif

#define DEBUG_ATTACH   0
#define DEBUG_HUNT     0
#define DEBUG_SEND     0
#define DEBUG_LISTEN   0

#if 0
#define DEBUGP(FMT,...) printf(FMT, __VA_ARGS__)
#else
#define DEBUGP(FMT,...)
#endif

#if DEBUG_ATTACH
#define DEBUGP_ATTACH(...) DEBUGP( __VA_ARGS__)
#else
#define DEBUGP_ATTACH(...)
#endif

#if DEBUG_HUNT
#define DEBUGP_HUNT(...) DEBUGP( __VA_ARGS__)
#else
#define DEBUGP_HUNT(...)
#endif

#if DEBUG_LISTEN
#define DEBUGP_LISTEN(...) DEBUGP( __VA_ARGS__)
#else
#define DEBUGP_LISTEN(...)
#endif

#if DEBUG_SEND
#define DEBUGP_SEND(...) DEBUGP( __VA_ARGS__)
#else
#define DEBUGP_SEND(...)
#endif


#define DRIVER_NAME "ose_signal_drv"
#define GET_SPID        1
#define GET_NAME        2
#define HUNT          100
#define DEHUNT        101
#define ATTACH        102
#define DETACH        103
#define SEND          104
#define SEND_W_S      105
#define LISTEN        106
#define OPEN          200

#define REF_SEGMENT_SIZE 8

struct async {
  SIGSELECT signo;
  ErlDrvTermData port;
  ErlDrvTermData proc;
  PROCESS spid;
  PROCESS target;
  Uint32 ref;
};

/**
 * OSE signals
 **/
union SIGNAL {
  SIGSELECT signo;
  struct async async;
};

/**
 * The driver's context
 **/
typedef struct _driver_context {
  ErlDrvPort port;
  PROCESS spid;
  ErlDrvEvent perm_events[2];
  ErlDrvEvent *events;
  Uint32 event_cnt;
  Uint32 ref;
  Uint32 *outstanding_refs;
  Uint32 outstanding_refs_max;
  Uint32 outstanding_refs_cnt;
} driver_context_t;

/**
 * Global variables
 **/
static ErlDrvTermData a_ok;
static ErlDrvTermData a_error;
static ErlDrvTermData a_enomem;
static ErlDrvTermData a_enoent;
static ErlDrvTermData a_badarg;
static ErlDrvTermData a_mailbox_up;
static ErlDrvTermData a_mailbox_down;
static ErlDrvTermData a_ose_drv_reply;
static ErlDrvTermData a_message;
static PROCESS proxy_proc;


/**
 * Serialize/unserialize unsigned 32-bit values
 **/
static char *put_u32(unsigned int value, char *ptr) {
  *ptr++ = (value & 0xff000000) >> 24;
  *ptr++ = (value & 0x00ff0000) >> 16;
  *ptr++ = (value & 0x0000ff00) >> 8;
  *ptr++ = (value & 0xff);

  return ptr;
}

static unsigned int get_u32(char *ptr) {
  unsigned int result = 0;
  result += (ptr[0] & 0xff) << 24;
  result += (ptr[1] & 0xff) << 16;
  result += (ptr[2] & 0xff) << 8;
  result += (ptr[3] & 0xff);

  return result;
}


/* Stolen from efile_drv.c */

/* char EV_CHAR_P(ErlIOVec *ev, int p, int q) */
#define EV_CHAR_P(ev, p, q)                   \
    (((char *)(ev)->iov[(q)].iov_base) + (p))

/* int EV_GET_CHAR(ErlIOVec *ev, char *p, int *pp, int *qp) */
#define EV_GET_CHAR(ev, p, pp, qp) ev_get_char(ev, p ,pp, qp)
static int
ev_get_char(ErlIOVec *ev, char *p, int *pp, int *qp) {
  if (*(pp)+1 <= (ev)->iov[*(qp)].iov_len) {
    *(p) = *EV_CHAR_P(ev, *(pp), *(qp));
    if (*(pp)+1 < (ev)->iov[*(qp)].iov_len)
      *(pp) = *(pp)+1;
    else {
      (*(qp))++;
      *pp = 0;
    }
    return !0;
  }
  return 0;
}

/* Uint32 EV_UINT32(ErlIOVec *ev, int p, int q)*/
#define EV_UINT32(ev, p, q) \
    ((Uint32) *(((unsigned char *)(ev)->iov[(q)].iov_base) + (p)))

/* int EV_GET_UINT32(ErlIOVec *ev, Uint32 *p, int *pp, int *qp) */
#define EV_GET_UINT32(ev, p, pp, qp) ev_get_uint32(ev,(Uint32*)(p),pp,qp)
static int
ev_get_uint32(ErlIOVec *ev, Uint32 *p, int *pp, int *qp) {
  if (*(pp)+4 <= (ev)->iov[*(qp)].iov_len) {
    *(p) = (EV_UINT32(ev, *(pp),   *(qp)) << 24)
      | (EV_UINT32(ev, *(pp)+1, *(qp)) << 16)
      | (EV_UINT32(ev, *(pp)+2, *(qp)) << 8)
      | (EV_UINT32(ev, *(pp)+3, *(qp)));
    if (*(pp)+4 < (ev)->iov[*(qp)].iov_len)
      *(pp) = *(pp)+4;
    else {
      (*(qp))++;
      *pp = 0;
    }
    return !0;
  }
  return 0;
}

/**
 * Convinience macros
 **/
#define send_response(port,output) erl_drv_send_term(driver_mk_port(port),\
    driver_caller(port), output, sizeof(output) / sizeof(output[0]));

void iov_memcpy(void *dest,ErlIOVec *ev,int ind,int off);
void iov_memcpy(void *dest,ErlIOVec *ev,int ind,int off) {
  int i;
  memcpy(dest,ev->iov[ind].iov_base+off,ev->iov[ind].iov_len-off);
  for (i = ind+1; i < ev->vsize; i++)
    memcpy(dest,ev->iov[i].iov_base,ev->iov[i].iov_len);
}

/**
 * Reference handling
 **/

static int add_reference(driver_context_t *ctxt, Uint32 ref) {

  /*
   * Premature optimizations may be evil, but they sure are fun.
   */

  if (ctxt->outstanding_refs == NULL) {
    /* First ref to be ignored */
    ctxt->outstanding_refs = driver_alloc(REF_SEGMENT_SIZE*sizeof(Uint32));
    if (!ctxt->outstanding_refs)
      return 1;

    memset(ctxt->outstanding_refs,0,REF_SEGMENT_SIZE*sizeof(Uint32));
    ctxt->outstanding_refs_max += REF_SEGMENT_SIZE;
    ctxt->outstanding_refs[ctxt->outstanding_refs_cnt++] = ref;
  } else if (ctxt->outstanding_refs_cnt == ctxt->outstanding_refs_max) {
    /* Expand ref array */
    Uint32 *new_array;
    ctxt->outstanding_refs_max += REF_SEGMENT_SIZE;
    new_array = driver_realloc(ctxt->outstanding_refs,
			       ctxt->outstanding_refs_max*sizeof(Uint32));

    if (!new_array) {
      ctxt->outstanding_refs_max -= REF_SEGMENT_SIZE;
      return 1;
    }

    ctxt->outstanding_refs = new_array;

    memset(ctxt->outstanding_refs+ctxt->outstanding_refs_cnt,0,
	   REF_SEGMENT_SIZE*sizeof(Uint32));
    ctxt->outstanding_refs[ctxt->outstanding_refs_cnt++] = ref;

  } else {
    /* Find an empty slot:
     *   First we try current index,
     *   then we scan for a slot.
     */
    if (!ctxt->outstanding_refs[ctxt->outstanding_refs_cnt]) {
      ctxt->outstanding_refs[ctxt->outstanding_refs_cnt++] = ref;
    } else {
      int i;
      ASSERT(ctxt->outstanding_refs_cnt < ctxt->outstanding_refs_max);
      for (i = 0; i < ctxt->outstanding_refs_max; i++)
	if (!ctxt->outstanding_refs[i])
	  break;
      ASSERT(ctxt->outstanding_refs[i] == 0);
      ctxt->outstanding_refs[i] = ref;
      ctxt->outstanding_refs_cnt++;
    }
  }
  return 0;
}

/* Return 0 if removed, 1 if does not exist, */
static int remove_reference(driver_context_t *ctxt, Uint32 ref) {
  int i,j;

  if (ctxt->outstanding_refs_max == 0 && ctxt->outstanding_refs_cnt == 0) {
    ASSERT(ctxt->outstanding_refs == NULL);
    return 1;
  }

  for (i = 0; i < ctxt->outstanding_refs_max; i++) {
    if (ctxt->outstanding_refs[i] == ref) {
      ctxt->outstanding_refs[i] = 0;
      ctxt->outstanding_refs_cnt--;
      i = -1;
      break;
    }
  }

  if (i != -1)
    return 1;

  if (ctxt->outstanding_refs_cnt == 0) {
    driver_free(ctxt->outstanding_refs);
    ctxt->outstanding_refs = NULL;
    ctxt->outstanding_refs_max = 0;
  } else if (ctxt->outstanding_refs_cnt == (ctxt->outstanding_refs_max - REF_SEGMENT_SIZE)) {
    Uint32 *new_array;
    for (i = 0, j = 0; i < ctxt->outstanding_refs_cnt; i++) {
      if (ctxt->outstanding_refs[i] == 0) {
	for (j = i+1; j < ctxt->outstanding_refs_max; j++)
	  if (ctxt->outstanding_refs[j]) {
	    ctxt->outstanding_refs[i] = ctxt->outstanding_refs[j];
	    ctxt->outstanding_refs[j] = 0;
	    break;
	  }
      }
    }
    ctxt->outstanding_refs_max -= REF_SEGMENT_SIZE;
    new_array = driver_realloc(ctxt->outstanding_refs,
			       ctxt->outstanding_refs_max*sizeof(Uint32));
    if (!new_array) {
      ctxt->outstanding_refs_max += REF_SEGMENT_SIZE;
      return 2;
    }

    ctxt->outstanding_refs = new_array;

  }

  return 0;
}

/**
 * The OSE proxy process. This only handles ERTS_SIGNAL_OSE_DRV_ATTACH.
 * The process is needed because signals triggered by attach ignore
 * redir tables.
 *
 * We have one global proxy process to save memory. An attempt to make each
 * port phantom into a proxy was made, but that used way to much memory.
 */
static OS_PROCESS(driver_proxy_process) {
  SIGSELECT sigs[] = {1,ERTS_SIGNAL_OSE_DRV_ATTACH};
  PROCESS master = 0;

  while (1) {
    union SIGNAL *sig = receive(sigs);

    if (sig->signo == ERTS_SIGNAL_OSE_DRV_ATTACH) {

      /* The first message is used to determine who to send messages to. */
      if (master == 0)
	master = sender(&sig);

      if (sig->async.target == 0) {
	PROCESS from = sender(&sig);
	restore(sig);
	DEBUGP_ATTACH("0x%x: got attach 0x%x, sending to 0x%x\n",
		      current_process(),from,master);
	sig->async.target = from;
	send(&sig,master);
      } else {
	PROCESS target = sig->async.target;
	restore(sig);
	sig->async.target = 0;
	DEBUGP_ATTACH("0x%x: doing attach on 0x%x\n",current_process(),target);
	attach(&sig,target);
      }
    }
  }
}


/**
 * Init routine for the driver
 **/
static int drv_init(void) {

  a_ok = driver_mk_atom("ok");
  a_error = driver_mk_atom("error");
  a_enomem = driver_mk_atom("enomem");
  a_enoent = driver_mk_atom("enoent");
  a_badarg = driver_mk_atom("badarg");
  a_mailbox_up = driver_mk_atom("mailbox_up");
  a_mailbox_down = driver_mk_atom("mailbox_down");
  a_ose_drv_reply = driver_mk_atom("ose_drv_reply");
  a_message = driver_mk_atom("message");

  proxy_proc = create_process(get_ptype(current_process()),
			      "ose_signal_driver_proxy",
			      driver_proxy_process, 10000,
			      get_pri(current_process()),
			      0, 0, NULL, 0, 0);

#ifdef DEBUG
  efs_clone(proxy_proc);
#endif
  start(proxy_proc);

  return 0;
}

/* Signal resolution callback */
static ErlDrvOseEventId resolve_signal(union SIGNAL* osig) {
  union SIGNAL *sig = osig;
  if (sig->signo == ERTS_SIGNAL_OSE_DRV_HUNT ||
      sig->signo == ERTS_SIGNAL_OSE_DRV_ATTACH) {
    return sig->async.spid;
  }
  DEBUGP("%p: Got signal %d sent to %p from 0x%p\n",
	 current_process(),sig->signo,addressee(&sig),sender(&sig));
  return addressee(&sig);
}


/**
 * Start routine for the driver
 **/
static ErlDrvData drv_start(ErlDrvPort port, char *command)
{
  driver_context_t *ctxt = driver_alloc(sizeof(driver_context_t));

  ctxt->perm_events[0] = NULL;
  ctxt->perm_events[1] = NULL;

  ctxt->spid = 0;
  ctxt->port = port;
  ctxt->event_cnt = 0;
  ctxt->events = NULL;
  ctxt->ref = 0;
  ctxt->outstanding_refs = NULL;
  ctxt->outstanding_refs_max = 0;
  ctxt->outstanding_refs_cnt = 0;


  /* Set the communication protocol to Erlang to be binary */
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

  /* Everything ok */
  return (ErlDrvData)ctxt;
}

/**
 * Stop routine for the driver
 **/
static void drv_stop(ErlDrvData driver_data)
{
  driver_context_t *ctxt = (driver_context_t *)driver_data;
  int i;

  /* HUNT + ATTACH */
  if (ctxt->perm_events[0])
    driver_select(ctxt->port, ctxt->perm_events[0],
		  ERL_DRV_USE|ERL_DRV_READ, 0);
  if (ctxt->perm_events[1])
    driver_select(ctxt->port, ctxt->perm_events[1],
		  ERL_DRV_USE|ERL_DRV_READ, 0);

  for (i = 0; i < ctxt->event_cnt; i++) {
    driver_select(ctxt->port, ctxt->events[i], ERL_DRV_USE|ERL_DRV_READ, 0);
  }

  if (ctxt->spid != 0)
    kill_proc(ctxt->spid);
  DEBUGP("0x%x: stopped\n",ctxt->spid);
  if (ctxt->events)
    driver_free(ctxt->events);
  if (ctxt->outstanding_refs)
    driver_free(ctxt->outstanding_refs);

  driver_free(ctxt);
}

/**
 * Output from Erlang
 **/
static void outputv(ErlDrvData driver_data, ErlIOVec *ev)
{
  driver_context_t *ctxt = (driver_context_t *)driver_data;
  int p = 0, q = 1;
  char cmd;

  if (! EV_GET_CHAR(ev,&cmd,&p,&q)) {
    ErlDrvTermData output[] = {
      ERL_DRV_ATOM, a_ose_drv_reply,
      ERL_DRV_PORT, driver_mk_port(ctxt->port),
      ERL_DRV_ATOM, a_badarg,
      ERL_DRV_TUPLE, 3};
    send_response(ctxt->port, output);
    return;
  }

  /* Command is in the buffer's first byte */
  switch(cmd) {

  case OPEN: {
    char *name = driver_alloc(ev->size - 1+1);
    struct OS_redir_entry redir[2];

    redir[0].sig = 1;
    redir[0].pid = current_process();

    iov_memcpy(name,ev,q,p);
    name[ev->size-1] = '\0';

    ctxt->spid = create_process(OS_PHANTOM, name, NULL, 0,
				0, 0, 0, redir, 0, 0);

    DEBUGP("0x%x: open\n",ctxt->spid);

    ctxt->perm_events[1] =
      erl_drv_ose_event_alloc(ERTS_SIGNAL_OSE_DRV_ATTACH,(int)ctxt->spid,
			      resolve_signal, NULL);
    driver_select(ctxt->port,ctxt->perm_events[1],ERL_DRV_READ|ERL_DRV_USE,1);

    ctxt->perm_events[0] =
      erl_drv_ose_event_alloc(ERTS_SIGNAL_OSE_DRV_HUNT,(int)ctxt->spid,
			      resolve_signal, NULL);
    driver_select(ctxt->port,ctxt->perm_events[0],ERL_DRV_READ|ERL_DRV_USE,1);

    start(ctxt->spid);

    {
      ErlDrvTermData output[] = {
	ERL_DRV_ATOM, a_ose_drv_reply,
	ERL_DRV_PORT, driver_mk_port(ctxt->port),
	ERL_DRV_ATOM, a_ok,
	ERL_DRV_TUPLE, 3};

      send_response(ctxt->port, output);
    }

    break;

  }

  case ATTACH:
  case HUNT:
    {
      union SIGNAL *sig = alloc(sizeof(union SIGNAL),
				cmd == HUNT ? ERTS_SIGNAL_OSE_DRV_HUNT:ERTS_SIGNAL_OSE_DRV_ATTACH);

      sig->async.port = driver_mk_port(ctxt->port);
      sig->async.proc = driver_caller(ctxt->port);
      sig->async.spid = ctxt->spid;
      sig->async.ref = ++ctxt->ref;

      if (add_reference(ctxt,ctxt->ref)) {
	ErlDrvTermData output[] = {
	  ERL_DRV_ATOM, a_ose_drv_reply,
	  ERL_DRV_PORT, driver_mk_port(ctxt->port),
	  ERL_DRV_ATOM, a_enomem,
	  ERL_DRV_TUPLE, 3};
	send_response(ctxt->port, output);
	free_buf(&sig);
      } else {
	ErlDrvTermData output[] = {
	  ERL_DRV_ATOM, a_ose_drv_reply,
	  ERL_DRV_PORT, driver_mk_port(ctxt->port),
	  ERL_DRV_PORT, driver_mk_port(ctxt->port),
	  ERL_DRV_INT, (ErlDrvUInt)ctxt->ref,
	  ERL_DRV_TUPLE, 2,
	  ERL_DRV_TUPLE, 3};
	send_response(ctxt->port, output);

	if (cmd == HUNT) {
	  char *huntname = driver_alloc(sizeof(char)*((ev->size-1)+1));

	  iov_memcpy(huntname,ev,q,p);
	  huntname[ev->size-1] = '\0';

	  DEBUGP_HUNT("0x%x: hunt %s -> %u (%u,%u)\n",
		      ctxt->spid,huntname,ctxt->ref,
		      ctxt->outstanding_refs_cnt,
		      ctxt->outstanding_refs_max);

	  hunt(huntname, 0, NULL, &sig);

	  driver_free(huntname);
	} else {
	  EV_GET_UINT32(ev,&sig->async.target,&p,&q);
	  DEBUGP_ATTACH("0x%x: attach %u -> %u (%u,%u)\n",
			ctxt->spid,sig->async.target,
			ctxt->ref,
			ctxt->outstanding_refs_cnt,
			ctxt->outstanding_refs_max);

	  send(&sig,proxy_proc);
	}

      }

      break;
    }

  case DETACH:
  case DEHUNT:
    {

        Uint32 ref;

	EV_GET_UINT32(ev,&ref,&p,&q);
	if (cmd == DETACH) {
	  DEBUGP_ATTACH("0x%x: detach %u (%u,%u)\n",ctxt->spid,ref,
			ctxt->outstanding_refs_cnt,
			ctxt->outstanding_refs_max);
	} else {
	  DEBUGP_HUNT("0x%x: dehunt %u (%u,%u)\n",ctxt->spid,ref,
		      ctxt->outstanding_refs_cnt,
		      ctxt->outstanding_refs_max);
	}

	if (remove_reference(ctxt,ref)) {
	  ErlDrvTermData output[] = {
	    ERL_DRV_ATOM, a_ose_drv_reply,
	    ERL_DRV_PORT, driver_mk_port(ctxt->port),
	    ERL_DRV_ATOM, a_error,
	    ERL_DRV_ATOM, a_enoent,
	    ERL_DRV_TUPLE, 2,
	    ERL_DRV_TUPLE, 3};

	  send_response(ctxt->port, output);
	} else {
	  ErlDrvTermData output[] = {
	    ERL_DRV_ATOM, a_ose_drv_reply,
	    ERL_DRV_PORT, driver_mk_port(ctxt->port),
	    ERL_DRV_ATOM, a_ok,
	    ERL_DRV_TUPLE, 3};

	  send_response(ctxt->port, output);
	}

      break;
    }

  case SEND:
  case SEND_W_S:
    {
      PROCESS spid;
      PROCESS sender;
      SIGSELECT signo;
      OSBUFSIZE size = ev->size-9;
      union SIGNAL *sig;

      EV_GET_UINT32(ev,&spid,&p,&q);

      if (cmd == SEND_W_S) {
	EV_GET_UINT32(ev,&sender,&p,&q);
	size -= 4;
      } else {
	sender = ctxt->spid;
      }

      EV_GET_UINT32(ev,&signo,&p,&q);

      sig = alloc(size + sizeof(SIGSELECT),signo);

      if (cmd == SEND_W_S) {
	DEBUGP_SEND("0x%x: send_w_s(%u,%u,%u)\n",ctxt->spid,spid,signo,sender);
      } else {
	DEBUGP_SEND("0x%x: send(%u,%u)\n",ctxt->spid,spid,signo);
      }

      iov_memcpy(((char *)&sig->signo) + sizeof(SIGSELECT),ev,q,p);

      send_w_s(&sig, sender, spid);

      break;
    }

    case LISTEN:
    {
      int i,j,event_cnt = (ev->size - 1)/4;
      ErlDrvEvent *events = NULL;
      SIGSELECT signo,tmp_signo;

      if (event_cnt == 0) {
	for (i = 0; i < ctxt->event_cnt; i++)
	  driver_select(ctxt->port,ctxt->events[i],ERL_DRV_READ|ERL_DRV_USE,0);
	if (ctxt->events)
	  driver_free(ctxt->events);
      } else {
	events = driver_alloc(sizeof(ErlDrvEvent)*event_cnt);
	EV_GET_UINT32(ev,&signo,&p,&q);
	for (i = 0, j = 0; i < event_cnt || j < ctxt->event_cnt; ) {

	  if (ctxt->events)
	    erl_drv_ose_event_fetch(ctxt->events[j],&tmp_signo,NULL,NULL);

	  if (signo == tmp_signo) {
	    events[i++] = ctxt->events[j++];
	    EV_GET_UINT32(ev,&signo,&p,&q);
	  } else if (signo < tmp_signo || !ctxt->events) {
	    /* New signal to select on */
	    events[i] = erl_drv_ose_event_alloc(signo,(int)ctxt->spid,
						resolve_signal, NULL);
	    driver_select(ctxt->port,events[i++],ERL_DRV_READ|ERL_DRV_USE,1);
	    EV_GET_UINT32(ev,&signo,&p,&q);
	  } else {
	    /* Remove old signal to select on */
	    driver_select(ctxt->port,ctxt->events[j++],ERL_DRV_READ|ERL_DRV_USE,0);
	  }
	}
	if (ctxt->events)
	  driver_free(ctxt->events);
      }
      ctxt->events = events;
      ctxt->event_cnt = event_cnt;

      {
	ErlDrvTermData output[] = {
	  ERL_DRV_ATOM, a_ose_drv_reply,
	  ERL_DRV_PORT, driver_mk_port(ctxt->port),
	  ERL_DRV_ATOM, a_ok,
	  ERL_DRV_TUPLE, 3};
	send_response(ctxt->port, output);
      }
      break;
    }

    default:
    {
      DEBUGP("Warning: 'ose_signal_drv' unknown command '%d'\n", cmd);
      break;
    }
  }
}

/**
 * Handler for when OSE signal arrives
 **/
static void ready_input(ErlDrvData driver_data, ErlDrvEvent event)
{
  driver_context_t *ctxt = (driver_context_t *)driver_data;
  union SIGNAL *sig = erl_drv_ose_get_signal(event);

  while (sig != NULL) {

    switch(sig->signo)
      {
	/* Remote process is available */
      case ERTS_SIGNAL_OSE_DRV_HUNT:
	{
	  const PROCESS spid = sender(&sig);

	  if (remove_reference(ctxt,sig->async.ref)) {
	    DEBUGP_HUNT("0x%x: Got hunt from 0x%x -> %u (CANCELLED) (%u,%u)\n",
			ctxt->spid,spid,sig->async.ref,
			ctxt->outstanding_refs_cnt,
			ctxt->outstanding_refs_max);
	    /* Already removed by dehunt */
	  } else {
	    ErlDrvTermData reply[] = {
	      ERL_DRV_ATOM, a_mailbox_up,
	      ERL_DRV_PORT, sig->async.port,
	      ERL_DRV_PORT, sig->async.port,
	      ERL_DRV_UINT, (ErlDrvUInt)sig->async.ref,
	      ERL_DRV_TUPLE, 2,
	      ERL_DRV_UINT, (ErlDrvUInt)spid,
	      ERL_DRV_TUPLE, 4};
	    DEBUGP_HUNT("0x%x: Got hunt from 0x%x -> %u (%u,%u)\n",
			ctxt->spid,spid,sig->async.ref,
			ctxt->outstanding_refs_cnt,
			ctxt->outstanding_refs_max);
	    erl_drv_send_term(sig->async.port, sig->async.proc, reply,
			      sizeof(reply) / sizeof(reply[0]));
	  }
	  break;
	}

	/* Remote process is down */
      case ERTS_SIGNAL_OSE_DRV_ATTACH:
	{
	  PROCESS spid = sig->async.target;

	  if (remove_reference(ctxt,sig->async.ref)) {
	    DEBUGP_ATTACH("0x%x: Got attach from 0x%x -> %u (CANCELLED) (%u,%u)\n",
			  ctxt->spid,spid,sig->async.ref,
			  ctxt->outstanding_refs_cnt,
			  ctxt->outstanding_refs_max);
	    /* Already removed by detach */
	  } else {
	    ErlDrvTermData reply[] = {
	      ERL_DRV_ATOM, a_mailbox_down,
	      ERL_DRV_PORT, sig->async.port,
	      ERL_DRV_PORT, sig->async.port,
	      ERL_DRV_UINT, sig->async.ref,
	      ERL_DRV_TUPLE, 2,
	      ERL_DRV_UINT, (ErlDrvUInt)spid,
	      ERL_DRV_TUPLE, 4};
	     DEBUGP_ATTACH("0x%x: Got attach from 0x%x -> %u (%u,%u)\n",
			   ctxt->spid,spid,sig->async.ref,
			   ctxt->outstanding_refs_cnt,
			   ctxt->outstanding_refs_max);
	    erl_drv_send_term(sig->async.port, sig->async.proc, reply,
			      sizeof(reply) / sizeof(reply[0]));
	  }
	  break;
	}

	/* Received user defined signal */
      default:
	{
	  const PROCESS spid = sender(&sig);
	  const OSBUFSIZE size = sigsize(&sig) - sizeof(SIGSELECT);
	  const char *sig_data = ((char *)&sig->signo) + sizeof(SIGSELECT);

	  ErlDrvTermData reply[] = {
	    ERL_DRV_ATOM, a_message,
	    ERL_DRV_PORT, driver_mk_port(ctxt->port),
	    ERL_DRV_UINT, (ErlDrvUInt)spid,
	    ERL_DRV_UINT, (ErlDrvUInt)ctxt->spid,
	    ERL_DRV_UINT, (ErlDrvUInt)sig->signo,
	    ERL_DRV_BUF2BINARY, (ErlDrvTermData)sig_data, (ErlDrvUInt)size,
	    ERL_DRV_TUPLE, 4,
	    ERL_DRV_TUPLE, 3};

	  DEBUGP_SEND("0x%x: Got 0x%u\r\n", spid, sig->signo);

	  erl_drv_output_term(driver_mk_port(ctxt->port), reply,
			      sizeof(reply) / sizeof(reply[0]));
	  break;
	}
      }

    free_buf(&sig);
    sig = erl_drv_ose_get_signal(event);
  }
}

/**
 * Handler for 'port_control'
 **/
static ErlDrvSSizeT control(ErlDrvData driver_data, unsigned int cmd,
                            char *buf, ErlDrvSizeT len,
                            char **rbuf, ErlDrvSizeT rlen)
{
  driver_context_t *ctxt = (driver_context_t *)driver_data;

  switch(cmd)
  {
    case GET_SPID:
    {
      const PROCESS spid = ctxt->spid;
      put_u32(spid, *rbuf);
      return sizeof(PROCESS);
    }

#ifdef HAVE_OSE_SPI_H
    case GET_NAME:
    {
      const PROCESS spid = get_u32(buf);
      char *name = (char*)get_pid_info(spid,OSE_PI_NAME);
      int n;
      if (!name) {
	*rbuf = NULL;
	return 0;
      }

      if (rlen < (n = strlen(name))) {
	ErlDrvBinary *bin = driver_alloc_binary(n);
	strncpy(bin->orig_bytes,name,n);
	*rbuf = (char*)bin;
      } else
	strncpy(*rbuf,name,n);
      free_buf((union SIGNAL**)&name);

      return n;
    }
#endif
    default:
    {
      /* Unknown command */
      return (ErlDrvSSizeT)ERL_DRV_ERROR_GENERAL;
      break;
    }
  }
}

static void stop_select(ErlDrvEvent event, void *reserved)
{
  erl_drv_ose_event_free(event);
}

/**
 * Setup the driver entry for the Erlang runtime
 **/
ErlDrvEntry ose_signal_driver_entry = {
  .init                         = drv_init,
  .start                        = drv_start,
  .stop                         = drv_stop,
  .outputv                      = outputv,
  .ready_input                  = ready_input,
  .driver_name                  = DRIVER_NAME,
  .control                      = control,
  .extended_marker              = ERL_DRV_EXTENDED_MARKER,
  .major_version                = ERL_DRV_EXTENDED_MAJOR_VERSION,
  .minor_version                = ERL_DRV_EXTENDED_MINOR_VERSION,
  .driver_flags                 = ERL_DRV_FLAG_USE_PORT_LOCKING,
  .stop_select                  = stop_select
};

