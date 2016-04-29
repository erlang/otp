/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

/**********************************************************************
 * Header for monitors and links data structures.
 * Monitors are kept in an AVL tree and the data structures for
 * the four different types of monitors are like this:
 **********************************************************************
 * Local monitor by pid/port: 
 * (Ref is always same in all involved data structures)
 **********************************************************************
 *        Process/Port X     Process Y
 *       +-------------+    +-------------+
 * Type: | MON_ORIGIN  |    | MON_TARGET  |
 *       +-------------+    +-------------+
 * Pid:  | Pid(Y)      |    | Pid/Port(X) |
 *       +-------------+    +-------------+
 * Name: | []          |    | []          |
 *       +-------------+    +-------------+
 **********************************************************************
 * Local monitor by name: (Ref is always same in all involved data structures)
 **********************************************************************
 *        Process X          Process Y (name foo)
 *       +-------------+    +-------------+
 * Type: | MON_ORIGIN  |    | MON_TARGET  |
 *       +-------------+    +-------------+
 * Pid:  | Pid(Y)      |    | Pid(X)      |
 *       +-------------+    +-------------+
 * Name: | Atom(foo)   |    | Atom(foo)   |
 *       +-------------+    +-------------+
 **********************************************************************
 * Remote monitor by pid: (Ref is always same in all involved data structures)
 **********************************************************************
 *                    Node A              |           Node B
 *       ---------------------------------+----------------------------------
 *        Process X (@A)   Distentry @A       Distentry @B     Process Y (@B)
 *                         for node B         for node A 
 *       +-------------+  +-------------+    +-------------+  +-------------+
 * Type: | MON_ORIGIN  |  | MON_TARGET  |    | MON_ORIGIN  |  | MON_TARGET  |
 *       +-------------+  +-------------+    +-------------+  +-------------+
 * Pid:  | Pid(Y)      |  | Pid(X)      |    | Pid(Y)      |  | Pid(X)      |
 *       +-------------+  +-------------+    +-------------+  +-------------+
 * Name: | []          |  | []          |    | []          |  | []          |
 *       +-------------+  +-------------+    +-------------+  +-------------+
 **********************************************************************
 * Remote monitor by name: (Ref is always same in all involved data structures)
 **********************************************************************
 *                    Node A              |           Node B
 *       ---------------------------------+----------------------------------
 *        Process X (@A)   Distentry @A       Distentry @B     Process Y (@B)
 *                         for node B         for node A       (name foo)
 *       +-------------+  +-------------+    +-------------+  +-------------+
 * Type: | MON_ORIGIN  |  | MON_TARGET  |    | MON_ORIGIN  |  | MON_TARGET  |
 *       +-------------+  +-------------+    +-------------+  +-------------+
 * Pid:  | Atom(node B)|  | Pid(X)      |    | Pid(Y)      |  | Pid(X)      |
 *       +-------------+  +-------------+    +-------------+  +-------------+
 * Name: | Atom(foo)   |  | Atom(foo)   |    | Atom(foo)   |  | Atom(foo)   |
 *       +-------------+  +-------------+    +-------------+  +-------------+
 * The reason for the node atom in X->pid is that we don't know the actual
 * pid of the monitored process on the other node when setting the monitor
 * (which is done asyncronously).
 **********************************************************************/
#ifndef _ERL_MONITORS_H
#define _ERL_MONITORS_H

/* Type tags for monitors */
#define MON_ORIGIN 1
#define MON_TARGET 3
#define MON_TIME_OFFSET 7

/* Type tags for links */
#define LINK_PID 1   /* ...Or port */
#define LINK_NODE 3  /* "Node monitor" */  

/* Size of a monitor without heap, in words (fixalloc) */
#define ERTS_MONITOR_SIZE ((sizeof(ErtsMonitor) - sizeof(Uint))/sizeof(Uint))
#define ERTS_MONITOR_SH_SIZE (ERTS_MONITOR_SIZE + REF_THING_SIZE)
#define ERTS_LINK_SIZE ((sizeof(ErtsLink) - sizeof(Uint))/sizeof(Uint))
#define ERTS_LINK_SH_SIZE ERTS_LINK_SIZE /* Size of fix-alloced links */

/* ErtsMonitor and ErtsLink *need* to begin in a similar way as 
   ErtsMonitorOrLink */
typedef struct erts_monitor_or_link {
    struct erts_monitor_or_link *left, *right;
    Sint16 balance;    
} ErtsMonitorOrLink;

typedef struct erts_monitor {
    struct erts_monitor *left, *right; 
    Sint16 balance;
    Uint16 type;  /* MON_ORIGIN | MON_TARGET | MON_TIME_OFFSET */
    Eterm ref;
    Eterm pid;    /* In case of distributed named monitor, this is the
		     nodename atom in MON_ORIGIN process, otherwise a pid or
		     , in case of a MON_TARGET, a port */
    Eterm name;   /* When monitoring a named process: atom() else [] */
    Uint heap[1]; /* Larger in reality */
} ErtsMonitor;

typedef struct erts_link {
    struct erts_link *left, *right;
    Sint16 balance;
    Uint16 type;             /* LINK_PID | LINK_NODE */
    Eterm pid;               /* When node monitor, 
				the node atom is here instead */
    union {
	struct erts_link *root;  /* Used only in dist entries */
	Uint refc;
    } shared;
    Uint heap[1];            /* Larger in reality */
} ErtsLink;   

typedef struct erts_suspend_monitor {
    struct erts_suspend_monitor *left, *right;
    Sint16 balance;

    int pending;
    int active;
    Eterm pid;
} ErtsSuspendMonitor;

#define ERTS_LINK_ROOT(Linkp) ((Linkp)->shared.root)
#define ERTS_LINK_REFC(Linkp) ((Linkp)->shared.refc) 

Uint erts_tot_link_lh_size(void);


/* Prototypes */
void erts_destroy_monitor(ErtsMonitor *mon);
void erts_add_monitor(ErtsMonitor **root, Uint type, Eterm ref, Eterm pid,
		      Eterm name);
ErtsMonitor *erts_remove_monitor(ErtsMonitor **root, Eterm ref);
ErtsMonitor *erts_lookup_monitor(ErtsMonitor *root, Eterm ref);
void erts_sweep_monitors(ErtsMonitor *root, 
			 void (*doit)(ErtsMonitor *, void *),
			 void *context);

void erts_destroy_link(ErtsLink *lnk);
/* Returns 0 if OK, < 0 if already present */
int erts_add_link(ErtsLink **root, Uint type, Eterm pid);
ErtsLink *erts_add_or_lookup_link(ErtsLink **root, Uint type, Eterm pid);
ErtsLink *erts_remove_link(ErtsLink **root, Eterm pid);
ErtsLink *erts_lookup_link(ErtsLink *root, Eterm pid);
void erts_sweep_links(ErtsLink *root, 
		      void (*doit)(ErtsLink *, void *),
		      void *context);

void erts_destroy_suspend_monitor(ErtsSuspendMonitor *sproc);
void erts_sweep_suspend_monitors(ErtsSuspendMonitor *root,
				 void (*doit)(ErtsSuspendMonitor *, void *),
				 void *context);
ErtsSuspendMonitor *erts_add_or_lookup_suspend_monitor(ErtsSuspendMonitor **root,
						       Eterm pid);
ErtsSuspendMonitor *erts_lookup_suspend_monitor(ErtsSuspendMonitor *root,
						Eterm pid);
void erts_delete_suspend_monitor(ErtsSuspendMonitor **root, Eterm pid);
void erts_init_monitors(void);
void erts_one_link_size(ErtsLink *lnk, void *vpu);
void erts_one_mon_size(ErtsMonitor *mon, void *vpu);

#define erts_doforall_monitors erts_sweep_monitors
#define erts_doforall_links erts_sweep_links
#define erts_doforall_suspend_monitors erts_sweep_suspend_monitors

#endif /* _ERL_MONITORS_H */
