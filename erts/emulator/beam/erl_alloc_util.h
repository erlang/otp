/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2010. All Rights Reserved.
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

#ifndef ERL_ALLOC_UTIL__
#define ERL_ALLOC_UTIL__

#define ERTS_ALCU_VSN_STR "2.2"

#include "erl_alloc_types.h"

typedef struct Allctr_t_ Allctr_t;

typedef struct {
    UWord ycs;
    UWord mmc;
} AlcUInit_t;

typedef struct {
    char *name_prefix;
    ErtsAlcType_t alloc_no;
    int ts;
    int tspec;
    int tpref;
    int ramv;
    UWord sbct;
    UWord asbcst;
    UWord rsbcst;
    UWord rsbcmt;
    UWord rmbcmt;
    UWord mmbcs;
    UWord mmsbc;
    UWord mmmbc;
    UWord lmbcs;
    UWord smbcs;
    UWord mbcgs;
} AllctrInit_t;

typedef struct {
    UWord blocks;
    UWord carriers;
} AllctrSize_t;

#ifndef SMALL_MEMORY

#define ERTS_DEFAULT_ALCU_INIT {                                           \
    1024*1024,		/* (bytes)  ycs:    sys_alloc carrier size       */\
    1024      		/* (amount) mmc:    max mseg carriers            */\
}

#define ERTS_DEFAULT_ALLCTR_INIT {                                         \
    NULL,                                                                  \
    ERTS_ALC_A_INVALID,	/* (number) alloc_no: allocator number           */\
    1,			/* (bool)   ts:     thread safe                  */\
    0,			/* (bool)   tspec:  thread specific              */\
    0,			/* (bool)   tpref:  thread preferred             */\
    0,			/* (bool)   ramv:   realloc always moves         */\
    512*1024,		/* (bytes)  sbct:   sbc threshold                */\
    2*1024*2024,	/* (amount) asbcst: abs sbc shrink threshold     */\
    20,			/* (%)      rsbcst: rel sbc shrink threshold     */\
    80,			/* (%)      rsbcmt: rel sbc move threshold       */\
    50,			/* (%)      rmbcmt: rel mbc move threshold       */\
    1024*1024,		/* (bytes)  mmbcs:  main multiblock carrier size */\
    256,		/* (amount) mmsbc:  max mseg sbcs                */\
    10,			/* (amount) mmmbc:  max mseg mbcs                */\
    10*1024*1024,	/* (bytes)  lmbcs:  largest mbc size             */\
    1024*1024,		/* (bytes)  smbcs:  smallest mbc size            */\
    10			/* (amount) mbcgs:  mbc growth stages            */\
}

#else /* if SMALL_MEMORY */

#define ERTS_DEFAULT_ALCU_INIT {                                           \
    128*1024,		/* (bytes)  ycs:    sys_alloc carrier size       */\
    1024      		/* (amount) mmc:    max mseg carriers            */\
}

#define ERTS_DEFAULT_ALLCTR_INIT {                                         \
    NULL,                                                                  \
    ERTS_ALC_A_INVALID,	/* (number) alloc_no: allocator number           */\
    1,			/* (bool)   ts:     thread safe                  */\
    0,			/* (bool)   tspec:  thread specific              */\
    0,			/* (bool)   tpref:  thread preferred             */\
    0,			/* (bool)   ramv:   realloc always moves         */\
    64*1024,		/* (bytes)  sbct:   sbc threshold                */\
    2*1024*2024,	/* (amount) asbcst: abs sbc shrink threshold     */\
    20,			/* (%)      rsbcst: rel sbc shrink threshold     */\
    80,			/* (%)      rsbcmt: rel sbc move threshold       */\
    128*1024,		/* (bytes)  mmbcs:  main multiblock carrier size */\
    256,		/* (amount) mmsbc:  max mseg sbcs                */\
    10,			/* (amount) mmmbc:  max mseg mbcs                */\
    1024*1024,		/* (bytes)  lmbcs:  largest mbc size             */\
    128*1024,		/* (bytes)  smbcs:  smallest mbc size            */\
    10			/* (amount) mbcgs:  mbc growth stages            */\
}

#endif

void *	erts_alcu_alloc(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc(ErtsAlcType_t, void *, void *, Uint);
void *	erts_alcu_realloc_mv(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free(ErtsAlcType_t, void *, void *);
#ifdef USE_THREADS
void *	erts_alcu_alloc_ts(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc_ts(ErtsAlcType_t, void *, void *, Uint);
void *	erts_alcu_realloc_mv_ts(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free_ts(ErtsAlcType_t, void *, void *);
void *	erts_alcu_alloc_thr_spec(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc_thr_spec(ErtsAlcType_t, void *, void *, Uint);
void *	erts_alcu_realloc_mv_thr_spec(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free_thr_spec(ErtsAlcType_t, void *, void *);
void *	erts_alcu_alloc_thr_pref(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc_thr_pref(ErtsAlcType_t, void *, void *, Uint);
void *	erts_alcu_realloc_mv_thr_pref(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free_thr_pref(ErtsAlcType_t, void *, void *);
#endif
Eterm	erts_alcu_au_info_options(int *, void *, Uint **, Uint *);
Eterm	erts_alcu_info_options(Allctr_t *, int *, void *, Uint **, Uint *);
Eterm	erts_alcu_sz_info(Allctr_t *, int, int *, void *, Uint **, Uint *);
Eterm	erts_alcu_info(Allctr_t *, int, int *, void *, Uint **, Uint *);
void	erts_alcu_init(AlcUInit_t *);
void    erts_alcu_current_size(Allctr_t *, AllctrSize_t *);

#endif

#if defined(GET_ERL_ALLOC_UTIL_IMPL) && !defined(ERL_ALLOC_UTIL_IMPL__)
#define ERL_ALLOC_UTIL_IMPL__

#ifdef USE_THREADS
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#endif

#include "erl_mseg.h"

#undef ERTS_ALLOC_UTIL_HARD_DEBUG
#ifdef DEBUG
#  if 0
#    define ERTS_ALLOC_UTIL_HARD_DEBUG
#  endif
#endif

#undef MIN
#undef MAX
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define FLOOR(X, I) (((X)/(I))*(I))
#define CEILING(X, I)  ((((X) - 1)/(I) + 1)*(I))

#undef  WORD_MASK
#define INV_WORD_MASK	((UWord) (sizeof(UWord) - 1))
#define WORD_MASK	(~INV_WORD_MASK)
#define WORD_FLOOR(X)	((X) & WORD_MASK)
#define WORD_CEILING(X)	WORD_FLOOR((X) + INV_WORD_MASK)

#undef  UNIT_MASK
#define INV_UNIT_MASK	((UWord) (sizeof(Unit_t) - 1))
#define UNIT_MASK	(~INV_UNIT_MASK)
#define UNIT_FLOOR(X)	((X) & UNIT_MASK)
#define UNIT_CEILING(X)	UNIT_FLOOR((X) + INV_UNIT_MASK)


#define SZ_MASK			(~((UWord) 0) << 3)
#define FLG_MASK		(~(SZ_MASK))


#define BLK_SZ(B) \
  (*((Block_t *) (B)) & SZ_MASK)

#define CARRIER_SZ(C) \
  ((C)->chdr & SZ_MASK)

typedef union {char c[8]; long l; double d;} Unit_t;

typedef struct Carrier_t_ Carrier_t;
struct Carrier_t_ {
    UWord chdr;
    Carrier_t *next;
    Carrier_t *prev;
};

typedef struct {
    Carrier_t *first;
    Carrier_t *last;
} CarrierList_t;

typedef UWord Block_t;
typedef UWord FreeBlkFtr_t;

typedef struct {
    UWord giga_no;
    UWord no;
} CallCounter_t;

typedef struct {
    UWord		no;
    UWord		size;
} StatValues_t;

typedef struct {
    StatValues_t	curr_mseg;
    StatValues_t	curr_sys_alloc;
    StatValues_t	max;
    StatValues_t	max_ever;
    struct {
	StatValues_t	curr;
	StatValues_t	max;
	StatValues_t	max_ever;
    } blocks;
} CarriersStats_t;

struct Allctr_t_ {

    /* Allocator name prefix */
    char *		name_prefix;

    /* Allocator number */
    ErtsAlcType_t	alloc_no;

    /* Alloc, realloc and free names as atoms */
    struct {
	Eterm		alloc;
	Eterm		realloc;
	Eterm		free;
    } name;

    /* Version string */
    char *		vsn_str;

    /* Options */
    int			t;
    int			ramv;
    Uint		sbc_threshold;
    Uint		sbc_move_threshold;
    Uint		mbc_move_threshold;
    Uint		main_carrier_size;
    Uint		max_mseg_sbcs;
    Uint		max_mseg_mbcs;
    Uint		largest_mbc_size;
    Uint		smallest_mbc_size;
    Uint		mbc_growth_stages;
#if HAVE_ERTS_MSEG
    ErtsMsegOpt_t	mseg_opt;
#endif

    /* */
    Uint		mbc_header_size;
    Uint		sbc_header_size;
    Uint		min_mbc_size;
    Uint		min_mbc_first_free_size;
    Uint		min_block_size;

    /* Carriers */
    CarrierList_t	mbc_list;
    CarrierList_t	sbc_list;

    /* Main carrier (if there is one) */
    Carrier_t *		main_carrier;

    /* Callback functions (first 4 are mandatory) */
    Block_t *		(*get_free_block)	(Allctr_t *, Uint,
						 Block_t *, Uint);
    void		(*link_free_block)	(Allctr_t *, Block_t *);
    void		(*unlink_free_block)	(Allctr_t *, Block_t *);
    Eterm		(*info_options)		(Allctr_t *, char *, int *,
						 void *, Uint **, Uint *);

    Uint		(*get_next_mbc_size)	(Allctr_t *);
    void		(*creating_mbc)		(Allctr_t *, Carrier_t *);
    void		(*destroying_mbc)	(Allctr_t *, Carrier_t *);
    void		(*init_atoms)		(void);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    void		(*check_block)		(Allctr_t *, Block_t *,  int);
    void		(*check_mbc)		(Allctr_t *, Carrier_t *);
#endif

#ifdef USE_THREADS
    /* Mutex for this allocator */
    erts_mtx_t		mutex;
    int			thread_safe;
    struct {
	Allctr_t	*prev;
	Allctr_t	*next;
    } ts_list;
#endif

    int			atoms_initialized;

    int			stopped;

    /* Some statistics ... */
    struct {
	CallCounter_t	this_alloc;
	CallCounter_t	this_free;
	CallCounter_t	this_realloc;
	CallCounter_t	mseg_alloc;
	CallCounter_t	mseg_dealloc;
	CallCounter_t	mseg_realloc;
	CallCounter_t	sys_alloc;
	CallCounter_t	sys_free;
	CallCounter_t	sys_realloc;
    } calls;

    CarriersStats_t	sbcs;
    CarriersStats_t	mbcs;

#ifdef DEBUG
#ifdef USE_THREADS
    struct {
	int saved_tid;
	erts_tid_t tid;
    } debug;
#endif
#endif
};

int	erts_alcu_start(Allctr_t *, AllctrInit_t *);
void	erts_alcu_stop(Allctr_t *);

unsigned long	erts_alcu_test(unsigned long, unsigned long, unsigned long);



#endif /* #if defined(GET_ERL_ALLOC_UTIL_IMPL)
	      && !defined(ERL_ALLOC_UTIL_IMPL__) */

