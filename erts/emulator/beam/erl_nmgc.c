/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "global.h"
#include "erl_gc.h"
#include "erl_binary.h"
#include "erl_nmgc.h"
#include "erl_debug.h"
#if HIPE
#include "hipe_bif0.h" /* for hipe_constants_{start,next} */
#include "hipe_stack.h"
#endif


#ifdef INCREMENTAL
/***************************************************************************
 *                                                                         *
 *         Incremental Garbage Collector for the Message Area              *
 *                                                                         *
 ***************************************************************************/

/*
 * The heap pointers are declared in erl_init.c
 * global_heap is the nursery
 * global_old_heap is the old generation
 */
unsigned char *blackmap = NULL;
INC_Page *inc_used_mem = NULL;
INC_MemBlock *inc_free_list = NULL;
Eterm *inc_fromspc;
Eterm *inc_fromend;
Eterm *inc_nursery_scn_ptr;
Eterm **fwdptrs;
Eterm *inc_alloc_limit;
Process *inc_active_proc;
Process *inc_active_last;
int   inc_words_to_go;

static Eterm       *inc_last_nursery;
static int          inc_pages = INC_NoPAGES;
static INC_Page    *inc_bibop = NULL;
static int          inc_used_pages;

/* Used when growing the old generation */
/*
#define INC_ROOTSAVE 16384
static Eterm       *root_save[INC_ROOTSAVE];
static int          roots_saved = 0;
*/

INC_STORAGE_DECLARATION(,gray);

static void inc_minor_gc(Process *p, int need, Eterm* objv, int nobj);
static void inc_major_gc(Process *p, int need, Eterm* objv, int nobj);

#ifdef INC_TIME_BASED
#if USE_PERFCTR

/*
 * This uses the Linux perfctr extension to virtualise the
 * time-stamp counter.
 */
#include "libperfctr.h"
static struct vperfctr *vperfctr;
static double cpu_khz;
static double tsc_to_cpu_mult;

static void inc_start_hrvtime(void)
{
    struct perfctr_info info;
    struct vperfctr_control control;

    if( vperfctr != NULL )
        return;
    vperfctr = vperfctr_open();
    if( vperfctr == NULL )
        return;
    if( vperfctr_info(vperfctr, &info) >= 0 ) {
        cpu_khz = (double)info.cpu_khz;
        tsc_to_cpu_mult = (double)(info.tsc_to_cpu_mult ? : 1);
        if( info.cpu_features & PERFCTR_FEATURE_RDTSC ) {
            memset(&control, 0, sizeof control);
            control.cpu_control.tsc_on = 1;
            if( vperfctr_control(vperfctr, &control) >= 0 )
                return;
        }
    }
    vperfctr_close(vperfctr);
    vperfctr = NULL;
}

#define inc_get_hrvtime() (((double)vperfctr_read_tsc(vperfctr) * tsc_to_cpu_mult) / cpu_khz)

#endif /* USE_PERFCTR */
#endif /* INC_TIME_BASED */

#ifdef INC_TIME_BASED
#  define timeslice 1 /* milli seconds */
#  define WORK_MORE (inc_get_hrvtime() < start_time + timeslice)
#else
//#  define inc_min_work 100 /* words */
#  define inc_min_work global_heap_sz + inc_pages * INC_FULLPAGE /* words */
#  define WORK_MORE (inc_words_to_go > 0)
#endif

void erts_init_incgc(void)
{
    int i;
    int size = inc_pages * INC_FULLPAGE;

    /* Young generation */
    global_heap = (Eterm *)erts_alloc(ERTS_ALC_T_MESSAGE_AREA,
                                      sizeof(Eterm) * global_heap_sz);
    global_hend = global_heap + global_heap_sz;
    global_htop = global_heap;
    inc_alloc_limit = global_hend;

    /* Fromspace */
    inc_last_nursery = (Eterm *) erts_alloc(ERTS_ALC_T_MESSAGE_AREA,
                                            global_heap_sz * sizeof(Eterm));
    inc_fromspc = inc_fromend = NULL;

    /* Forward-pointers */
    fwdptrs = erts_alloc(ERTS_ALC_T_MESSAGE_AREA,
                         global_heap_sz * sizeof(Eterm*));
    /* Old generation */
    global_old_heap = (Eterm *)erts_alloc(ERTS_ALC_T_MESSAGE_AREA,
                                          size * sizeof(Eterm));
    global_old_hend = global_old_heap + size;

    /* Pages i BiBOP */
    for (i = 0; i < inc_pages; i++)
    {
        INC_Page *this = (INC_Page*)(global_old_heap + i * INC_FULLPAGE);
        this->next = (INC_Page*)((Eterm*)this + INC_FULLPAGE);
    }

    inc_bibop = (INC_Page*)global_old_heap;
    ((INC_Page*)(global_old_heap + (inc_pages - 1) * INC_FULLPAGE))->next =
        NULL;

    inc_used_mem = inc_bibop;
    inc_bibop = inc_bibop->next;
    inc_used_mem->next = NULL;
    inc_used_pages = 1;

    /* Free-list */
    inc_free_list = (INC_MemBlock*)inc_used_mem->start;
    inc_free_list->size = INC_PAGESIZE;
    inc_free_list->prev = NULL;
    inc_free_list->next = NULL;

    /* Blackmap */
    blackmap = (unsigned char*)erts_alloc(ERTS_ALC_T_MESSAGE_AREA,
                                          INC_FULLPAGE * inc_pages);
    /* Gray stack */
    INC_STORAGE_INIT(gray);

    inc_active_proc = NULL;
    inc_active_last = NULL;

#ifdef INC_TIME_BASED
    inc_start_hrvtime();
#endif
}

void erts_cleanup_incgc(void)
{
    INC_STORAGE_ERASE(gray);

    if (inc_fromspc)
        inc_last_nursery = inc_fromspc;

    erts_free(ERTS_ALC_T_MESSAGE_AREA,(void*)global_heap);
    erts_free(ERTS_ALC_T_MESSAGE_AREA,(void*)inc_last_nursery);
    erts_free(ERTS_ALC_T_MESSAGE_AREA,(void*)global_old_heap);
    erts_free(ERTS_ALC_T_MESSAGE_AREA,(void*)blackmap);
    erts_free(ERTS_ALC_T_MESSAGE_AREA,(void*)fwdptrs);
}

void erts_incremental_gc(Process* p, int need, Eterm* objv, int nobj)
{
    int repeat_minor;
#ifdef INC_TIME_BASED
    double start_time = inc_get_hrvtime();
    int work_left_before = inc_words_to_go;
#endif
    /* Used when growing the fromspace */
    static char inc_growing_nurs = 0;

    BM_STOP_TIMER(system);
    //BM_MMU_READ();
    BM_RESET_TIMER(gc);
    BM_START_TIMER(gc);

    VERBOSE(DEBUG_HYBRID_GC,
            ("INCGC: Incremental GC START  Caused by: %T  Need: %d\n",
             p->id,need));

    ma_gc_flags |= GC_GLOBAL;
    ma_gc_flags &= ~GC_CYCLE_START;

#ifndef INC_TIME_BASED
    /* Decide how much work to do this GC stage. The work is meassured
     * in number of words copied from the young generation to the old
     * plus number of work marked in the old generation.
     */
    if (ma_gc_flags & GC_MAJOR) {
        int wm = (need > inc_min_work) ? need : inc_min_work;
        inc_words_to_go = (int)((wm * (((inc_used_pages * INC_PAGESIZE) /
                                        (double)global_heap_sz) + 1)) + 0.5);
    }
    else
        inc_words_to_go = (need > inc_min_work) ? need : inc_min_work;
#endif

    do {
        if (ma_gc_flags & GC_MAJOR) {
            /* This is a major collection cycle. */
            inc_major_gc(p,need,objv,nobj);
        } else if (ma_gc_flags & GC_CYCLE) {
            /* This is a minor collection cycle. */
            inc_minor_gc(p,need,objv,nobj);
        } else {
            VERBOSE(DEBUG_HYBRID_GC,("INCGC: Collection cycle START\n"));
            ma_gc_flags |= (GC_CYCLE | GC_CYCLE_START);
            inc_fromspc = global_heap;
            inc_fromend = global_htop;
            global_heap = global_htop = inc_last_nursery;
            global_hend = global_heap + global_heap_sz;
            inc_nursery_scn_ptr = global_heap;
#ifdef INC_TIME_BASED
            work_left_before = inc_words_to_go = global_heap_sz;
#endif
#ifdef DEBUG
            inc_last_nursery = NULL;
#endif
            memset(fwdptrs,0,global_heap_sz * sizeof(Eterm));

            {
                /* TODO: Alla processer ska väl egentligen inte aktiveras här... */
                int i;
                for (i = 0; i < erts_num_active_procs; i++) {
                    Process *cp = erts_active_procs[i];
                    INC_ACTIVATE(cp);
                    cp->scan_top = cp->high_water;
                }
            }

            if (ma_gc_flags & GC_NEED_MAJOR) {
                /* The previous collection cycle caused the old generation to
                 * overflow.  This collection cycle will therefore be a major
                 * one.
                 */
                BM_COUNT(major_gc_cycles);
                VERBOSE(DEBUG_HYBRID_GC,("INCGC: MAJOR cycle\n"));
                inc_major_gc(p,need,objv,nobj);
            } else {
                BM_COUNT(minor_gc_cycles);
                VERBOSE(DEBUG_HYBRID_GC,("INCGC: MINOR cycle\n"));
                inc_minor_gc(p,need,objv,nobj);
            }
        }

        repeat_minor = 0;
        if (!(ma_gc_flags & GC_CYCLE)) {
            inc_alloc_limit = global_hend;
            inc_last_nursery = inc_fromspc;
            inc_fromspc = inc_fromend = NULL;
            ASSERT(INC_STORAGE_EMPTY(gray));

            if (inc_growing_nurs) {
                /*
                 * The previous collection cycle caused the nursery to
                 * grow, now we have to grow the from-space as well.
                 */
                inc_last_nursery =
                    (Eterm*) erts_realloc(ERTS_ALC_T_MESSAGE_AREA,
                                          (void*)inc_last_nursery,
                                          sizeof(Eterm) * global_heap_sz);
                inc_growing_nurs = 0;
            }

            if (global_hend - global_htop <= need) {
                /*
                 * Initiate a new GC cycle immediately and, if necessary,
                 * enlarge the nursery.
                 */
                if (global_heap_sz <= need) {
                    VERBOSE(DEBUG_HYBRID_GC,
                            ("INCGC: Allocating a larger nursery\n"));
                    global_heap_sz = erts_next_heap_size(need * 1.5,0);
                    inc_last_nursery =
                        (Eterm*) erts_realloc(ERTS_ALC_T_MESSAGE_AREA,
                                              (void*)inc_last_nursery,
                                              sizeof(Eterm) * global_heap_sz);
                    fwdptrs = erts_realloc(ERTS_ALC_T_MESSAGE_AREA,fwdptrs,
                                           global_heap_sz * sizeof(Eterm*));
                    inc_growing_nurs = 1;
                }
                repeat_minor = 1;
            }

#ifdef DEBUG
            /* Fill the from-space with bad things */
            memset(inc_last_nursery,DEBUG_BAD_BYTE,
                   global_heap_sz * sizeof(Eterm));
#endif
        }
    } while (repeat_minor);


    /* Clean up after garbage collection ********************************/

    if (inc_alloc_limit != global_hend) {

#ifdef INC_TIME_BASED
        if ((work_left_before - inc_words_to_go) == 0) {
            inc_alloc_limit = global_htop + need;
        } else {
            inc_alloc_limit = (global_hend - global_htop) /
                (inc_words_to_go / (work_left_before - inc_words_to_go)) +
                global_htop;
            if (inc_alloc_limit > global_hend)
                inc_alloc_limit = global_hend;
        }
#else
        inc_alloc_limit = (Eterm*)(global_htop + (need > inc_min_work) ?
                                   need : inc_min_work);
        if (inc_alloc_limit > global_hend)
            inc_alloc_limit = global_hend;
#endif
    }

    ma_gc_flags &= ~GC_GLOBAL;

    /* INC_TIME_BASED: If this fails we have to increase the timeslice! */
    ASSERT(inc_alloc_limit - global_htop > need);

    BM_STOP_TIMER(gc);
#ifdef BM_TIMERS
    minor_global_gc_time += gc_time;
    if (gc_time > max_global_minor_time)
        max_global_minor_time = gc_time;

    pause_times[(((gc_time * 1000) < MAX_PAUSE_TIME) ?
                 (int)(gc_time * 1000) :
                 MAX_PAUSE_TIME - 1)]++;
#endif
    //BM_MMU_INIT();
    { static long long verif = 0;
        //erts_printf("innan verify: %d\n",++verif);
        if (verif==168) print_memory(NULL);
        verify_everything();
        //erts_printf("efter verify: %d\n",verif);
    }
    BM_START_TIMER(system);
    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Incremental GC END\n"));
}


/***************************************************************************
 *                                                                         *
 *     Minor collection  - Copy live data from young generation to old     *
 *                                                                         *
 ***************************************************************************/

#define MINOR_SCAN(PTR,END) do {                                        \
    ASSERT(PTR <= END);                                                 \
    while (WORK_MORE && PTR < END) {                                    \
        Eterm val = *PTR;                                               \
        Eterm *obj_ptr = ptr_val(val);                                  \
        switch (primary_tag(val)) {                                     \
        case TAG_PRIMARY_LIST:                                          \
            if (ptr_within(obj_ptr,inc_fromspc,inc_fromend)) {          \
                if (INC_IS_FORWARDED(obj_ptr)) {                        \
                    *PTR = make_list(INC_FORWARD_VALUE(obj_ptr));       \
                }                                                       \
                else {                                                  \
                    Eterm *hp = erts_inc_alloc(2);                      \
                    INC_STORE(gray,hp,2);                               \
                    INC_COPY_CONS(obj_ptr,hp,PTR);                      \
                }                                                       \
            }                                                           \
            break;                                                      \
        case TAG_PRIMARY_BOXED:                                         \
            if (ptr_within(obj_ptr,inc_fromspc,inc_fromend)) {          \
                if (INC_IS_FORWARDED(obj_ptr)) {                        \
                    *PTR = make_boxed(INC_FORWARD_VALUE(obj_ptr));      \
                }                                                       \
                else {                                                  \
                    Eterm *hp = erts_inc_alloc(BOXED_NEED(obj_ptr,*obj_ptr)); \
                    INC_STORE(gray,hp,BOXED_NEED(obj_ptr,*obj_ptr));    \
                    INC_COPY_BOXED(obj_ptr,hp,PTR);                     \
                }                                                       \
            }                                                           \
            break;                                                      \
        case TAG_PRIMARY_HEADER:                                        \
            switch (val & _TAG_HEADER_MASK) {                           \
            case ARITYVAL_SUBTAG: break;                                \
            default: PTR += thing_arityval(val); break;                 \
            }                                                           \
            break;                                                      \
        }                                                               \
        PTR++;                                                          \
    }                                                                   \
} while(0)


/* Returns: TRUE (1) if the need is greater than the available space
 * and the garbage collector needs to be restarted immediately. FALSE
 * (0) otherwise.
 */
static void inc_minor_gc(Process* p, int need, Eterm* objv, int nobj)
{
    BM_COUNT(minor_gc_stages);

    /* Start with looking at gray objects found in earlier collection
     * stages.
     */
    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Rescue gray found from nursery\n"));
    {
        INC_Object *obj = NULL;
        Eterm *ptr;

        while (WORK_MORE && !INC_STORAGE_EMPTY(gray)) {
            obj = INC_STORAGE_GET(gray);
            if ((*obj->this & _TAG_HEADER_MASK) == FUN_SUBTAG) {
                ptr = obj->this + thing_arityval(*obj->this) + 1;
            } else {
                ptr = obj->this;
            }
            MINOR_SCAN(ptr,obj->this + obj->size);
        }
        /* TODO: Se föregående uppdatering av grå objekt */
        if (!WORK_MORE && obj != NULL)
            INC_STORE(gray,obj->this,obj->size);
    }

    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Scan root-set\n"));
    while (WORK_MORE && inc_active_proc) {
        Rootset rootset;
        Process *cp = inc_active_proc;

        ASSERT(INC_IS_ACTIVE(cp));

        /* TODO: Hur dyrt är det att bygga nytt rootset varje gång? */

        /* TODO: Fundera på ordningen! Rootset, Heap, Old heap... */

        /* TODO: Scanna stacken från p->send till p->stop! [Brooks84] */
        /* Notera: Vi GC:ar inte de yngsta objekten - de som allokeras
           under GC-cykeln. Detta ger ynglingarna en chans att dö innan
           GC:n börjar kopiera dem. [StefanovicMcKinleyMoss@OOPSLA99] */

        /* TODO: När rootset är scannat borde processen inte vara
           aktiv mer. Den bör aktiveras i schedule, endast om en
           process har kört behöver vi scanna rootset igen. */

        /* MT: In a multithreaded system the process cp needs to be
         * locked here.
         */

        if (cp == p)
            rootset.n = setup_rootset(cp, objv, nobj, &rootset);
        else
            rootset.n = setup_rootset(cp, cp->arg_reg, cp->arity, &rootset);

        //MA_GENSWEEP_NSTACK(cp, old_htop, n_htop, objv, nobj);

        while (WORK_MORE && rootset.n--) {
            Eterm *g_ptr = rootset.v[rootset.n];
            Uint g_sz = rootset.sz[rootset.n];

            while (WORK_MORE && g_sz--) {
                Eterm gval = *g_ptr;
                switch (primary_tag(gval)) {
                case TAG_PRIMARY_LIST: {
                    Eterm *ptr = list_val(gval);
                    if (ptr_within(ptr,inc_fromspc,inc_fromend)) {
                        if (INC_IS_FORWARDED(ptr)) {
                            *g_ptr++ = make_list(INC_FORWARD_VALUE(ptr));
                        }
                        else {
                            Eterm *hp = erts_inc_alloc(2);
                            INC_STORE(gray,hp,2);
                            INC_COPY_CONS(ptr,hp,g_ptr++);
                        }
                    }
                    else
                        ++g_ptr;
                    continue;
                }

                case TAG_PRIMARY_BOXED: {
                    Eterm *ptr = boxed_val(gval);
                    if (ptr_within(ptr,inc_fromspc,inc_fromend)) {
                        if (INC_IS_FORWARDED(ptr)) {
                            *g_ptr++ = make_boxed(INC_FORWARD_VALUE(ptr));
                        }
                        else {
                            Eterm *hp = erts_inc_alloc(BOXED_NEED(ptr,*ptr));
                            INC_STORE(gray,hp,BOXED_NEED(ptr,*ptr));
                            INC_COPY_BOXED(ptr,hp,g_ptr++);
                        }
                    }
                    else
                        ++g_ptr;
                    continue;
                }

                default:
                    g_ptr++;
                    continue;
                }
            }
        }

        restore_one_rootset(cp, &rootset);

        /* MT: cp can be unlocked now. */

        /* VERBOSE(DEBUG_HYBRID_GC,("INCGC: Scan private nursery\n")); */
        if (cp->scan_top != HEAP_TOP(cp)) {
            Eterm *ptr = cp->scan_top;
            MINOR_SCAN(ptr,HEAP_TOP(cp));
            /* TODO: För att spara scan_top här måste alla ma-pekare
             * som hittas läggas till i cp->rrma.
             */
            //cp->scan_top = ptr;
        }

        /* VERBOSE(DEBUG_HYBRID_GC,("INCGC: Scan heap fragments\n")); */
        {
            ErlHeapFragment* bp = MBUF(cp);

            while (WORK_MORE && bp) {
                Eterm *ptr = bp->mem;
                if ((ARITH_HEAP(cp) >= bp->mem) &&
                    (ARITH_HEAP(cp) < bp->mem + bp->size)) {
                    MINOR_SCAN(ptr,ARITH_HEAP(cp));
                } else {
                    MINOR_SCAN(ptr,bp->mem + bp->size);
                }
                bp = bp->next;
            }
        }

        /* VERBOSE(DEBUG_HYBRID_GC,("INCGC: Scan gray\n")); */
        {
            INC_Object *obj = NULL;
            Eterm *ptr;
            while (WORK_MORE && !INC_STORAGE_EMPTY(gray)) {
                obj = INC_STORAGE_GET(gray);
                if ((*obj->this & _TAG_HEADER_MASK) == FUN_SUBTAG) {
                    ptr = obj->this + thing_arityval(*obj->this) + 1;
                } else {
                    ptr = obj->this;
                }
                MINOR_SCAN(ptr,obj->this + obj->size);
            }
            /* TODO: INC_STORE(gray,ptr,obj->size-(ptr-obj->this)); Typ.. */
            if (!WORK_MORE && obj != NULL)
                INC_STORE(gray,obj->this,obj->size);
        }

        if (WORK_MORE) {
            //printf("Rootset after:\r\n");
            //print_one_rootset(&rootset);
            INC_DEACTIVATE(cp);
        }
    }

    /* Update new pointers in the nursery to new copies in old generation. */
    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Update nursery\n"));
    {
        Eterm *ptr = inc_nursery_scn_ptr;
        MINOR_SCAN(ptr,global_htop);
        inc_nursery_scn_ptr = ptr;
    }

    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Rescue gray found from nursery\n"));
    {
        INC_Object *obj = NULL;
        Eterm *ptr;

        while (WORK_MORE && !INC_STORAGE_EMPTY(gray)) {
            obj = INC_STORAGE_GET(gray);
            if ((*obj->this & _TAG_HEADER_MASK) == FUN_SUBTAG) {
                ptr = obj->this + thing_arityval(*obj->this) + 1;
            } else {
                ptr = obj->this;
            }
            MINOR_SCAN(ptr,obj->this + obj->size);
        }
        /* TODO: Se föregående uppdatering av grå objekt */
        if (!WORK_MORE && obj != NULL)
            INC_STORE(gray,obj->this,obj->size);
    }

    /* Atomic phase */
    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Update copy stack\n"));
    {
        Uint i;
        for (i = 0; i < ma_dst_top; i++) {
            if (ptr_within(ma_dst_stack[i],inc_fromspc,inc_fromend)) {
                if (INC_IS_FORWARDED(ma_dst_stack[i]))
                    ma_dst_stack[i] = INC_FORWARD_VALUE(ma_dst_stack[i]);
            }
        }
    }

    if (WORK_MORE) {
        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Update offheap-lists\n"));
        {
            ExternalThing **prev = &erts_global_offheap.externals;
            ExternalThing *ptr   = erts_global_offheap.externals;

            /* Atomic phase */
            VERBOSE(DEBUG_HYBRID_GC,("INCGC: Sweep proc externals\n"));
            while (ptr) {
                Eterm *ppt = (Eterm*) ptr;

                if (ptr_within(ppt,global_old_heap,global_old_hend)) {
                    prev = &ptr->next;
                    ptr = ptr->next;
                } else if (ptr_within(ppt, inc_fromspc, inc_fromend) &&
                           INC_IS_FORWARDED(ppt)) {
                    ExternalThing *ro = (ExternalThing*)INC_FORWARD_VALUE(ppt);
                    *prev = ro;         /* Patch to moved pos */
                    prev = &ro->next;
                    ptr = ro->next;
                } else {
                    erts_deref_node_entry(ptr->node);
                    *prev = ptr = ptr->next;
                }
            }
            ASSERT(*prev == NULL);
        }

        {
            ProcBin **prev = &erts_global_offheap.mso;
            ProcBin *ptr   = erts_global_offheap.mso;

            /* Atomic phase */
            VERBOSE(DEBUG_HYBRID_GC,("INCGC: Sweep proc bins\n"));
            while (ptr) {
                Eterm *ppt = (Eterm*)ptr;

                if (ptr_within(ppt,global_old_heap,global_old_hend)) {
                    prev = &ptr->next;
                    ptr = ptr->next;
                } else if (ptr_within(ppt, inc_fromspc, inc_fromend) &&
                           INC_IS_FORWARDED(ppt)) {
                    ProcBin *ro = (ProcBin*)INC_FORWARD_VALUE(ppt);
                    *prev = ro;         /* Patch to moved pos */
                    prev = &ro->next;
                    ptr = ro->next;
                } else {
                    Binary *bptr;
                    *prev = ptr->next;
                    bptr = ptr->val;
                    if (erts_refc_dectest(&bptr->refc, 0) == 0)
			erts_bin_free(bptr);
                    ptr = *prev;
                }
            }
            ASSERT(*prev == NULL);
        }

        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Minor collection cycle END\n"));
        ma_gc_flags &= ~GC_CYCLE;
    }
}




/***************************************************************************
 *                                                                         *
 *   Major collection - CopyMark - Copy young to old, Mark-Sweep old       *
 *                                                                         *
 ***************************************************************************/

#define COPYMARK(PTR,END) do {                                          \
    ASSERT(PTR <= END);                                                 \
    while (WORK_MORE && PTR < END) {                                    \
        Eterm val = *PTR;                                               \
        Eterm *obj_ptr = ptr_val(val);                                  \
        switch (primary_tag(val)) {                                     \
            case TAG_PRIMARY_LIST:                                      \
                COPYMARK_CONS(obj_ptr,aging_htop,PTR,aging_end); break; \
            case TAG_PRIMARY_BOXED:                                     \
                COPYMARK_BOXED(obj_ptr,aging_htop,PTR,aging_end); break; \
            case TAG_PRIMARY_HEADER:                                    \
                switch (val & _TAG_HEADER_MASK) {                       \
                    case ARITYVAL_SUBTAG: break;                        \
                    default:                                            \
                        PTR += thing_arityval(val);                     \
                        break;                                          \
                }                                                       \
                break;                                                  \
            default: break;                                             \
        }                                                               \
        PTR++;                                                          \
    }                                                                   \
} while(0);
/* TODO:
    if (aging_htop + 10 > aging + INC_FULLPAGE) {
        aging->next = inc_used_mem;
        inc_used_mem = aging;
    }
*/

static void inc_major_gc(Process *p, int need, Eterm* objv, int nobj)
{
    Eterm *free_start = NULL;
    Uint live = 0;
    Uint old_gen_sz = 0;
    static INC_Page *aging;
    static Eterm *aging_htop;
    static Eterm *aging_end;
    BM_NEW_TIMER(old_gc);

    BM_SWAP_TIMER(gc,old_gc);
    BM_COUNT(major_gc_stages);

    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Major collection START\n"));

    ma_gc_flags |= GC_INCLUDE_ALL;

    if (ma_gc_flags & GC_NEED_MAJOR)
    {
        INC_Page *page = inc_used_mem;

        ma_gc_flags |= GC_MAJOR;
        ma_gc_flags &= ~GC_NEED_MAJOR;

        while (page)
        {
            memset(blackmap +
                   ((void*)page - (void*)global_old_heap) / sizeof(void*),
                   0, INC_FULLPAGE);
            page = page->next;
        }

        if (inc_bibop) {
            aging = inc_bibop;
            inc_bibop = inc_bibop->next;
            aging->next = NULL;
            memset(blackmap +
                   ((void*)aging - (void*)global_old_heap) / sizeof(void*),
                   1, INC_FULLPAGE);
            aging_htop = aging->start;
            aging_end = aging->start + INC_PAGESIZE;
        }
        else {
            /* There are no free pages.. Either fragmentation is a
             * problem or we are simply out of memory. Allocation in
             * the old generation will be done through the free-list
             * this GC cycle.
             */
            aging = NULL;
            aging_htop = aging_end = NULL;
        }
    }

    /* Start with looking at gray objects found in earlier collection
     * stages.
     */
    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Copy-Mark gray\n"));
    {
        INC_Object *obj = NULL;

        while (WORK_MORE && !INC_STORAGE_EMPTY(gray)) {
            Eterm *ptr;

            obj = INC_STORAGE_GET(gray);
            if ((*obj->this & _TAG_HEADER_MASK) == FUN_SUBTAG) {
                ptr = obj->this + thing_arityval(*obj->this) + 1;
            } else {
                ptr = obj->this;
            }
            COPYMARK(ptr,obj->this + obj->size);
        }
        /* TODO: Titta på motsvarande i minor. */
        if (!WORK_MORE && obj != NULL)
            INC_STORE(gray,obj->this,obj->size);
    }

    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Copy-Mark roots\n"));
    while (WORK_MORE && inc_active_proc)
    {
        /* For each process: Scan all areas containing pointers to the
         * message area. When a process is done here, all it's
         * message-pointers should be to the old generation.
         */
        Rootset rootset;
        Process *cp = inc_active_proc;

        ASSERT(INC_IS_ACTIVE(cp));

        /* MT: In a multithreaded system the process cp needs to be
         * locked here.
         */
        if (cp == p)
            rootset.n = setup_rootset(cp, objv, nobj, &rootset);
        else
            rootset.n = setup_rootset(cp, cp->arg_reg, cp->arity, &rootset);

        while (WORK_MORE && rootset.n--)
        {
            Eterm *ptr = rootset.v[rootset.n];
            Eterm *end = ptr + rootset.sz[rootset.n];

            while (WORK_MORE && ptr < end) {
                Eterm val = *ptr;
                Eterm *obj_ptr = ptr_val(val);

                switch (primary_tag(val)) {
                    case TAG_PRIMARY_LIST:
                    {
                        COPYMARK_CONS(obj_ptr,aging_htop,ptr,aging_end);
                        break;
                    }

                    case TAG_PRIMARY_BOXED:
                    {
                        COPYMARK_BOXED(obj_ptr,aging_htop,ptr,aging_end);
                        break;
                    }
                }
                ptr++;
            }
        }

#ifdef HIPE
        /* Atomic phase */
        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Native stack scan: %T\n",cp->id));
        aging_htop = ma_fullsweep_nstack(cp,aging_htop,aging_end);
#endif
        restore_one_rootset(cp, &rootset);

        /* MT: cp can be unlocked now. But beware!! The message queue
         * might be updated with new pointers to the fromspace while
         * we work below. The send operation can not assume that all
         * active processes will look through their message queue
         * before deactivating as is the case in non-MT incremental
         * collection.
         */

        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Copy-Mark process heap\n"));
        {
            Eterm *ptr = cp->scan_top;
            COPYMARK(ptr,cp->htop);
            //cp->scan_top = ptr;
        }

        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Copy-Mark heap fragments\n"));
        {
            ErlHeapFragment* bp = MBUF(cp);

            while (WORK_MORE && bp) {
                Eterm *ptr = bp->mem;
                Eterm *end;

                if ((ARITH_HEAP(cp) >= bp->mem) &&
                    (ARITH_HEAP(cp) < bp->mem + bp->size)) {
                    end = ARITH_HEAP(cp);
                } else {
                    end = bp->mem + bp->size;
                }

                COPYMARK(ptr,end);
                bp = bp->next;
            }
        }

        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Copy-Mark gray stack\n"));
        {
            INC_Object *obj = NULL;

            while (WORK_MORE && !INC_STORAGE_EMPTY(gray)) {
                Eterm *ptr;

                obj = INC_STORAGE_GET(gray);
                if ((*obj->this & _TAG_HEADER_MASK) == FUN_SUBTAG) {
                    ptr = obj->this + thing_arityval(*obj->this) + 1;
                } else {
                    ptr = obj->this;
                }
                COPYMARK(ptr,obj->this + obj->size);
            }
            /* TODO: Titta på motsvarande i minor. */
            if (!WORK_MORE && obj != NULL)
                INC_STORE(gray,obj->this,obj->size);
        }

        if (WORK_MORE) {
            INC_DEACTIVATE(cp);
        }
    }

    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Copy-Mark nursery\n"));
    {
        Eterm *ptr = inc_nursery_scn_ptr;
        COPYMARK(ptr,global_htop);
        inc_nursery_scn_ptr = ptr;
    }

    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Copy-Mark gray found in nursery\n"));
    {
        INC_Object *obj = NULL;

        while (WORK_MORE && !INC_STORAGE_EMPTY(gray)) {
            Eterm *ptr;

            obj = INC_STORAGE_GET(gray);
            if ((*obj->this & _TAG_HEADER_MASK) == FUN_SUBTAG) {
                ptr = obj->this + thing_arityval(*obj->this) + 1;
            } else {
                ptr = obj->this;
            }
            COPYMARK(ptr,obj->this + obj->size);
        }
        /* TODO: Titta på motsvarande i minor. */
        if (!WORK_MORE && obj != NULL)
            INC_STORE(gray,obj->this,obj->size);
    }


    /**********************************************************************/
    if (WORK_MORE) {
        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Sweep phase\n"));

        /* Atomic phase */
        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Sweep externals in old generation\n"));
        {
            ExternalThing** prev = &erts_global_offheap.externals;
            ExternalThing* ptr = erts_global_offheap.externals;

            while (ptr) {
                Eterm* ppt = (Eterm *) ptr;

                if ((ptr_within(ppt, global_old_heap, global_old_hend) &&
                     blackmap[ppt - global_old_heap] == 0) ||
                    (ptr_within(ppt, inc_fromspc, inc_fromend) &&
                     !INC_IS_FORWARDED(ppt)))
                {
                    erts_deref_node_entry(ptr->node);
                    *prev = ptr = ptr->next;
                } else if (ptr_within(ppt, inc_fromspc, inc_fromend)) {
                    ExternalThing* ro = (ExternalThing*)INC_FORWARD_VALUE(ppt);
                    *prev = ro;         /* Patch to moved pos */
                    prev = &ro->next;
                    ptr = ro->next;
                } else {
                    prev = &ptr->next;
                    ptr = ptr->next;
                }
            }
            ASSERT(*prev == NULL);
        }

        /* Atomic phase */
        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Sweep refc bins in old generation\n"));
        {
            ProcBin** prev = &erts_global_offheap.mso;
            ProcBin*  ptr  = erts_global_offheap.mso;

            while (ptr) {
                Eterm *ppt = (Eterm*)ptr;

                if ((ptr_within(ppt, global_old_heap, global_old_hend) &&
                     blackmap[ppt - global_old_heap] == 0) ||
                    (ptr_within(ppt, inc_fromspc, inc_fromend) &&
                     !INC_IS_FORWARDED(ppt)))
                {
                    Binary* bptr;
                    *prev = ptr->next;
                    bptr = ptr->val;
                    if (erts_refc_dectest(&bptr->refc, 0) == 0)
			erts_bin_free(bptr);
                    ptr = *prev;
                } else if (ptr_within(ppt, inc_fromspc, inc_fromend)) {
                    ProcBin* ro = (ProcBin*)INC_FORWARD_VALUE(ppt);
                    *prev = ro;         /* Patch to moved pos */
                    prev = &ro->next;
                    ptr = ro->next;
                } else {
                    prev = &ptr->next;
                    ptr = ptr->next;
                }
            }
            ASSERT(*prev == NULL);
        }

        /* TODO: Currently atomic phase - Can not be later of course. */
        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Sweep old generation\n"));
        {
            INC_Page *page = inc_used_mem;
            INC_Page *prev = NULL;
            inc_free_list = NULL;

            while (page) {
                int scavenging = 0;
                int n = page->start - global_old_heap;
                int stop = n + INC_PAGESIZE;

                old_gen_sz += INC_PAGESIZE;
                while (n < stop) {
                    if (blackmap[n] != 0) {
                        if (scavenging) {
                            Eterm *ptr = global_old_heap + n;
                            scavenging = 0;
                            if ((ptr - free_start) * sizeof(Eterm) >=
                                sizeof(INC_MemBlock))
                            {
                                INC_MemBlock *new = (INC_MemBlock*)free_start;
                                new->size = ptr - free_start;
                                new->prev = NULL;
                                new->next = inc_free_list;
                                if (inc_free_list)
                                    inc_free_list->prev = new;
                                inc_free_list = new;
                            }
                        }
                        if (blackmap[n] == 255) {
                            unsigned int size =
                                *(unsigned int*)(((long)&blackmap[n]+4) & ~3);
                            live += size;
                            n += size;
                        }
                        else {
                            live += blackmap[n];
                            n += blackmap[n];
                        }
                    }
                    else if (!scavenging) {
                        free_start = global_old_heap + n;
                        scavenging = 1;
                        n++;
                    }
                    else {
                        n++;
                    }
                }

                if (scavenging) {
                    if ((global_old_heap + n - free_start) * sizeof(Eterm) >
                        sizeof(INC_MemBlock))
                    {
                        INC_MemBlock *new = (INC_MemBlock*)free_start;
                        new->size = global_old_heap + n - free_start;
                        new->prev = NULL;
                        new->next = inc_free_list;
                        if (inc_free_list)
                            inc_free_list->prev = new;
                        inc_free_list = new;
                    }
                    else if (free_start == page->start) {
                        INC_Page *next = page->next;

                        if (prev)
                            prev->next = page->next;
                        else
                            inc_used_mem = page->next;

                        page->next = inc_bibop;
                        inc_bibop = page;
                        inc_used_pages--;
                        page = next;
                        continue;
                    }
                }
                prev = page;
                page = page->next;
            }
        }
    }

    ASSERT(inc_bibop);
    /*
      This code is not expected to work right now.
    if (!inc_bibop) {
        int i;
        int new_pages = inc_pages * 2;
        int size = sizeof(Eterm) * new_pages * INC_FULLPAGE;
        Eterm *new_heap = erts_alloc(ERTS_ALC_T_MESSAGE_AREA,size);
        Eterm *new_hend = new_heap + size;
        Eterm *new_htop;
        Eterm *last_page_end;
        INC_Page *new_used_mem;
        INC_Page *page;

        erts_printf("The last page has been allocated..\n");
        erts_printf("We need to copy things!\n");

        / * Create new, bigger bag of pages * /
        for (i = 0; i < new_pages; i++)
        {
            INC_Page *this =
              (INC_Page*)(new_heap + i * INC_FULLPAGE);
            this->next = (INC_Page*)((Eterm*)this + INC_FULLPAGE);
        }
        inc_bibop = (INC_Page*)new_heap;
        ((INC_Page*)(new_heap + (new_pages - 1) *
                           INC_FULLPAGE))->next = NULL;

        new_used_mem = inc_bibop;
        inc_bibop = inc_bibop->next;
        new_used_mem->next = NULL;

        / * Move stuff from old bag to new * /
        inc_free_list = NULL;
        new_htop = new_used_mem->start;
        last_page_end = new_htop + INC_PAGESIZE;
        page = inc_used_mem;
        while (page)
        {
            Eterm *ptr = page->start;
            Eterm *page_end = ptr + INC_PAGESIZE;
            int n = offsetof(INC_Page,start) / sizeof(void*) +
              ((Eterm*)page - global_old_heap);
            while (ptr < page_end)
            {
                if (blackmap[n] > 0)
                {
                    if (last_page_end - new_htop < blackmap[n])
                    {
                        INC_Page *new_page = inc_bibop;
                        inc_bibop = inc_bibop->next;
                        new_page->next = new_used_mem;
                        new_used_mem = new_page;
                        new_htop = new_page->start;
                        last_page_end = new_htop + INC_PAGESIZE;
                    }

                    memcpy(new_htop,ptr,blackmap[n] * sizeof(Eterm));
                    for (i = 0; i < blackmap[n]; i++)
                    {
                        *ptr++ = (Eterm)new_htop++;
                    }
                  //new_htop += blackmap[n];
                  //ptr += blackmap[n];
                    / *
                    if (blackmap[n] == 255) Do the right thing...
                    * /
                    n += blackmap[n];
                }
                else
                {
                    n++; ptr++;
                }
            }
            page = page->next;
        }

        page = inc_used_mem;
        while (page)
        {
            Eterm *ptr = page->start;
            Eterm *page_end = ptr + INC_PAGESIZE;

            / * TODO: If inc_used_mem is sorted in address order, this
             * pass can be done at the same time as copying. * /
            while (ptr < page_end)
            {
                if (ptr_within(ptr_val(*ptr),global_old_heap,global_old_hend))
                {
                    *ptr = *((Eterm*)ptr_val(*ptr));
                }
                ptr++;
            }
            page = page->next;
        }

        printf("Restore rootset after heap move. Roots: %d\r\n",roots_saved);
        while (roots_saved--)
        {
            Eterm *ptr = root_save[roots_saved];
            *ptr = *((Eterm*)ptr_val(*ptr));
        }

        erts_free(ERTS_ALC_T_MESSAGE_AREA,(void*)global_old_heap);

        global_old_heap = new_heap;
        global_old_hend = new_hend;
        inc_used_mem = new_used_mem;
        inc_pages = new_pages;

        if ((last_page_end - new_htop) * sizeof(Eterm) >=
            sizeof(INC_MemBlock))
        {
            inc_free_list = (INC_MemBlock*)(new_htop);
            inc_free_list->size = last_page_end - new_htop;
            inc_free_list->prev = NULL;
            inc_free_list->next = NULL;
        }
    }
    */

    /* I vilka lägen kan vi vilja slänga på en extra sida.. ( < 25% kvar?)
    if ()
    {
        INC_Page *new_page = inc_bibop;
        INC_MemBlock *new_free =
          (INC_MemBlock*)new_page->start;

        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Fetching new page\n"));
        inc_bibop = inc_bibop->next;

        new_page->next = inc_used_mem;
        if (inc_used_mem)
            inc_used_mem->prev = new_page;
        inc_used_mem = new_page;

        // kolla detta med normal sidstorlek! old_gen_sz += INC_PAGESIZE;
        //BM_SWAP_TIMER(gc,misc1);
        memset(blackmap +
               ((void*)new_page - (void*)global_old_heap) / sizeof(void*),
               0, INC_FULLPAGE);
        //BM_SWAP_TIMER(misc1,gc);

        new_free->prev = NULL;
        new_free->next = inc_free_list;
        new_free->size = INC_PAGESIZE;
        if (inc_free_list)
            inc_free_list->prev = new_free;
        inc_free_list = new_free;
        //printf("Snatched a new page @ 0x%08x\r\n",(int)new_page);
        //print_free_list();
        found = new_free;
    }
    */

    VERBOSE(DEBUG_HYBRID_GC,("INCGC: Update copy stack\n"));
    {
        Uint i;
        for (i = 0; i < ma_dst_top; i++) {
            if (ptr_within(ma_dst_stack[i],inc_fromspc,inc_fromend)) {
                if (INC_IS_FORWARDED(ma_dst_stack[i]))
                    ma_dst_stack[i] = INC_FORWARD_VALUE(ma_dst_stack[i]);
            }
        }
    }

    if (WORK_MORE)
    {
        int size_left = INC_PAGESIZE - (aging_htop - aging->start);

        if (size_left > sizeof(INC_MemBlock))
        {
            ((INC_MemBlock*)aging_htop)->size = size_left;
            ((INC_MemBlock*)aging_htop)->prev = NULL;
            ((INC_MemBlock*)aging_htop)->next = inc_free_list;
            if (inc_free_list)
                inc_free_list->prev = (INC_MemBlock*)aging_htop;
            inc_free_list = (INC_MemBlock*)aging_htop;
        }
        aging->next = inc_used_mem;
        inc_used_mem = aging;
        inc_used_pages++;

        ma_gc_flags &= ~GC_MAJOR;
        ma_gc_flags &= ~GC_CYCLE;

        VERBOSE(DEBUG_HYBRID_GC,("INCGC: Major collection cycle END\n"));
    }

    ma_gc_flags &= ~GC_INCLUDE_ALL;

    BM_STOP_TIMER(old_gc);
#ifdef BM_TIMER
    major_global_gc_time += old_gc_time;
    if (old_gc_time > max_global_major_time)
      max_global_major_time = old_gc_time;

    if ((old_gc_time * 1000) < MAX_PAUSE_TIME)
        pause_times_old[(int)(old_gc_time * 1000)]++;
    else
        pause_times_old[MAX_PAUSE_TIME - 1]++;
#endif
    BM_START_TIMER(gc);
}



/***************************************************************************
 *                                                                         *
 * Allocation in the old generation. Used in minor colection and when      *
 * copying the rest of a message after a GC.                               *
 *                                                                         *
 ***************************************************************************/


Eterm *erts_inc_alloc(int need)
{
    INC_MemBlock *this = inc_free_list;

    ASSERT(need < INC_PAGESIZE);
    while (this && (this->size) < need)
    {
        this = this->next;
    }

    if (!this)
    {
        /* If a free block large enough is not found, a new page is
         * allocated. GC_NEED_MAJOR is set so that the next garbage
         * collection cycle will be a major one, that is, both
         * generations will be garbage collected.
         */
        INC_Page *new_page = inc_bibop;
        INC_MemBlock *new_free = (INC_MemBlock*)new_page->start;

        if (new_page)
        {
            VERBOSE(DEBUG_HYBRID_GC,
                    ("INCGC: Allocation grabs a new page\n"));
            inc_bibop = inc_bibop->next;
            new_page->next = inc_used_mem;
            inc_used_mem = new_page;
            inc_used_pages++;

            new_free->prev = NULL;
            new_free->next = inc_free_list;
                new_free->size = INC_PAGESIZE;
            if (inc_free_list)
                inc_free_list->prev = new_free;
            inc_free_list = new_free;

            this = new_free;
            if (!(ma_gc_flags & GC_MAJOR))
                ma_gc_flags |= GC_NEED_MAJOR;
        }
        else
        {
            erl_exit(-1, "inc_alloc ran out of pages!\n");
        }
    }

    if (((this->size) - need) * sizeof(Eterm) >= sizeof(INC_MemBlock))
    {
        INC_MemBlock *rest = (INC_MemBlock*)((Eterm*)this + need);

        /* The order here IS important! */
        rest->next = this->next;

        if (rest->next)
            rest->next->prev = rest;

        rest->prev = this->prev;

        if (rest->prev)
            rest->prev->next = rest;
        else
            inc_free_list = rest;

        rest->size = this->size - need;
    }
    else
    {
        if (this->prev)
            this->prev->next = this->next;
        else
            inc_free_list = this->next;

        if (this->next)
          this->next->prev = this->prev;
    }

    if (ma_gc_flags & GC_MAJOR) {
        if (need > 254) {
            blackmap[(Eterm*)this - global_old_heap] = 255;
            *(int*)((long)(&blackmap[(Eterm*)this - global_old_heap]+4) & ~3) =
                need;
        } else
            blackmap[(Eterm*)this - global_old_heap] = need;
    }
    return (Eterm*)this;
}
#endif /* INCREMENTAL */
