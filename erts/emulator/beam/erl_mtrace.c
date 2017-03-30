/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

/*
 * Description:	Memory allocation trace. The trace is sent over a
 *              tcp/ip connection.
 *
 *              The trace format is not intended to be documented.
 *              Instead a library for parsing the trace will be
 *              distributed. This in order to more easily be able
 *              to make changes in the trace format. The library
 *              for parsing the trace is currently not included in
 *              the OTP distribution, but will be in the future.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_sock.h"
#include "erl_threads.h"
#include "erl_memory_trace_protocol.h"
#include "erl_mtrace.h"

#if defined(MAXHOSTNAMELEN) && MAXHOSTNAMELEN > 255
#  undef MAXHOSTNAMELEN
#endif

#ifndef MAXHOSTNAMELEN
#  define MAXHOSTNAMELEN 255
#endif

#define TRACE_PRINTOUTS 0
#ifdef TRACE_PRINTOUTS
#define MSB2BITS(X) ((((unsigned)(X))+1)*8)
#endif

static erts_mtx_t mtrace_op_mutex;
static erts_mtx_t mtrace_buf_mutex;

#define TRACE_BUF_SZ 				(16*1024)


#define UI8_MSB_EHF_SZ				ERTS_MT_UI8_MSB_EHDR_FLD_SZ
#define UI16_MSB_EHF_SZ				ERTS_MT_UI16_MSB_EHDR_FLD_SZ
#define UI32_MSB_EHF_SZ				ERTS_MT_UI32_MSB_EHDR_FLD_SZ
#define UI64_MSB_EHF_SZ				ERTS_MT_UI64_MSB_EHDR_FLD_SZ
#define UI_MSB_EHF_SZ				ERTS_MT_UI64_MSB_EHDR_FLD_SZ
#define TAG_EHF_SZ				ERTS_MT_TAG_EHDR_FLD_SZ

#define UI8_MSB_EHF_MSK				ERTS_MT_UI8_MSB_EHDR_FLD_MSK
#define UI16_MSB_EHF_MSK			ERTS_MT_UI16_MSB_EHDR_FLD_MSK
#define UI32_MSB_EHF_MSK			ERTS_MT_UI32_MSB_EHDR_FLD_MSK
#define UI_MSB_EHF_MSK				ERTS_MT_UI64_MSB_EHDR_FLD_MSK
#define UI64_MSB_EHF_MSK			ERTS_MT_UI64_MSB_EHDR_FLD_MSK
#define TAG_EHF_MSK				ERTS_MT_TAG_EHDR_FLD_MSK

#define UI8_SZ					(1)
#define UI16_SZ					(2)
#define UI32_SZ					(4)
#define UI64_SZ					(8)
#ifdef ARCH_64 /* XXX:PaN Halfword? (whole file...) */
#  define UI_SZ					UI64_SZ
#else
#  define UI_SZ					UI32_SZ
#endif

#define WRITE_UI8(P, V) (*(P) = (byte) ((V) & 0xff))

#define WRITE_UI16(P, V)						\
  ((P)[0] = (byte) (((V) >>  8) & 0xff),				\
   (P)[1] = (byte) ( (V)        & 0xff))

#define WRITE_UI32(P, V)						\
  ((P)[0] = (byte) (((V) >> 24) & 0xff),				\
   (P)[1] = (byte) (((V) >> 16) & 0xff),				\
   (P)[2] = (byte) (((V) >>  8) & 0xff),				\
   (P)[3] = (byte) ( (V)        & 0xff))

#define WRITE_UI64(P, V)						\
  ((P)[0] = (byte) (((V) >> 56) & 0xff),				\
   (P)[1] = (byte) (((V) >> 48) & 0xff),				\
   (P)[2] = (byte) (((V) >> 40) & 0xff),				\
   (P)[3] = (byte) (((V) >> 32) & 0xff),				\
   (P)[4] = (byte) (((V) >> 24) & 0xff),				\
   (P)[5] = (byte) (((V) >> 16) & 0xff),				\
   (P)[6] = (byte) (((V) >>  8) & 0xff),				\
   (P)[7] = (byte) ( (V)        & 0xff))

#define PUT_UI8(P, V)  (WRITE_UI8((P),  (V)), (P) += UI8_SZ)
#define PUT_UI16(P, V) (WRITE_UI16((P), (V)), (P) += UI16_SZ)
#define PUT_UI32(P, V) (WRITE_UI32((P), (V)), (P) += UI32_SZ)
#define PUT_UI64(P, V) (WRITE_UI64((P), (V)), (P) += UI64_SZ)

#define PUT_VSZ_UI16(P, M, V)						\
do {									\
    Uint16 v__ = (Uint16) (V);						\
    if (v__ >= (((Uint16) 1) << 8)) (M) = 1; else (M) = 0;		\
    switch ((M)) {							\
    case 1: *((P)++) = (byte) ((v__ >>  8) & 0xff);			\
    case 0: *((P)++) = (byte) ( v__        & 0xff);			\
    }									\
} while (0)

#define PUT_VSZ_UI32(P, M, V)						\
do {									\
    Uint32 v__ = (Uint32) (V);						\
    if (v__ >= (((Uint32) 1) << 16)) {					\
	if (v__ >= (((Uint32) 1) << 24)) (M) = 3; else (M) = 2;		\
    } else {								\
	if (v__ >= (((Uint32) 1) << 8)) (M) = 1; else (M) = 0;		\
    }									\
    switch ((M)) {							\
    case 3: *((P)++) = (byte) ((v__ >> 24) & 0xff);			\
    case 2: *((P)++) = (byte) ((v__ >> 16) & 0xff);			\
    case 1: *((P)++) = (byte) ((v__ >>  8) & 0xff);			\
    case 0: *((P)++) = (byte) ( v__        & 0xff);			\
    }									\
} while (0)

#ifdef ARCH_64

#define PUT_VSZ_UI64(P, M, V)						\
do {									\
    Uint64 v__ = (Uint64) (V);						\
    if (v__ >= (((Uint64) 1) << 32)) {					\
	if (v__ >= (((Uint64) 1) << 48)) {				\
	    if (v__ >= (((Uint64) 1) << 56)) (M) = 7; else (M) = 6;	\
	} else {							\
	    if (v__ >= (((Uint64) 1) << 40)) (M) = 5; else (M) = 4;	\
	}								\
    } else {								\
	if (v__ >= (((Uint64) 1) << 16)) {				\
	    if (v__ >= (((Uint64) 1) << 24)) (M) = 3; else (M) = 2;	\
	} else {							\
	    if (v__ >= (((Uint64) 1) << 8)) (M) = 1; else (M) = 0;	\
	}								\
    }	    								\
    switch ((M)) {							\
    case 7: *((P)++) = (byte) ((v__ >> 56) & 0xff);			\
    case 6: *((P)++) = (byte) ((v__ >> 48) & 0xff);			\
    case 5: *((P)++) = (byte) ((v__ >> 40) & 0xff);			\
    case 4: *((P)++) = (byte) ((v__ >> 32) & 0xff);			\
    case 3: *((P)++) = (byte) ((v__ >> 24) & 0xff);			\
    case 2: *((P)++) = (byte) ((v__ >> 16) & 0xff);			\
    case 1: *((P)++) = (byte) ((v__ >>  8) & 0xff);			\
    case 0: *((P)++) = (byte) ( v__        & 0xff);			\
    }									\
} while (0)

#define PUT_VSZ_UI	PUT_VSZ_UI64
#else /* #ifdef ARCH_64 */
#define PUT_VSZ_UI	PUT_VSZ_UI32
#endif /* #ifdef ARCH_64 */

#define MAKE_TBUF_SZ(SZ)						\
  (TRACE_BUF_SZ < (SZ)							\
   ? (disable_trace(1, "Internal buffer overflow", 0), 0)		\
   : (endp - tracep < (SZ) ? send_trace_buffer() : 1))


static void disable_trace(int error, char *reason, int eno);
static int send_trace_buffer(void);

#ifdef DEBUG
void
check_alloc_entry(byte *sp, byte *ep,
		  byte tag,
		  Uint16 ct_no, int ct_no_n,
		  Uint16 type, int type_n,
		  UWord res, int res_n,
		  Uint size, int size_n,
		  Uint32 ti,int ti_n);
void
check_realloc_entry(byte *sp, byte *ep,
		    byte tag,
		    Uint16 ct_no, int ct_no_n,
		    Uint16 type, int type_n,
		    UWord res, int res_n,
		    UWord ptr, int ptr_n,
		    Uint size, int size_n,
		    Uint32 ti,int ti_n);
void
check_free_entry(byte *sp, byte *ep,
		 byte tag,
		 Uint16 ct_no, int ct_no_n,
		 Uint16 t_no, int t_no_n,
		 UWord ptr, int ptr_n,
		 Uint32 ti,int ti_n);
void
check_time_inc_entry(byte *sp, byte *ep,
		     Uint32 secs, int secs_n,
		     Uint32 usecs, int usecs_n);
#endif



int erts_mtrace_enabled;
static erts_sock_t socket_desc;
static byte trace_buffer[TRACE_BUF_SZ];
static byte *tracep;
static byte *endp;
static SysTimeval last_tv;

static ErtsAllocatorWrapper_t mtrace_wrapper;

#if ERTS_MTRACE_SEGMENT_ID >= ERTS_ALC_A_MIN || ERTS_MTRACE_SEGMENT_ID < 0
#error ERTS_MTRACE_SEGMENT_ID >= ERTS_ALC_A_MIN || ERTS_MTRACE_SEGMENT_ID < 0
#endif

char* erl_errno_id(int error);

#define INVALID_TIME_INC (0xffffffff)

static ERTS_INLINE Uint32
get_time_inc(void)
{
    Sint32 secs;
    Sint32 usecs;
    Uint32 res;
    SysTimeval tv;
    sys_gettimeofday(&tv);

    secs = tv.tv_sec - last_tv.tv_sec;
    if (tv.tv_usec >= last_tv.tv_usec)
	usecs = tv.tv_usec - last_tv.tv_usec;
    else {
	secs--;
	usecs = 1000000 + tv.tv_usec - last_tv.tv_usec;
    }

    ASSERT(0 <= usecs);
    ASSERT(usecs < 1000000);

    if (secs < 0) {
	/* Clock stepped backwards; we pretend that no time has past. */
	res = 0;
    }
    else if (secs < ERTS_MT_TIME_INC_SECS_MASK) {
	res = ((((Uint32) secs) << ERTS_MT_TIME_INC_SECS_SHIFT)
	       | (((Uint32) usecs) << ERTS_MT_TIME_INC_USECS_SHIFT));
    }
    else {
	/* Increment too large to fit in a 32-bit integer;
	   put a time inc entry in trace ... */
	if (MAKE_TBUF_SZ(UI8_SZ + UI16_SZ + 2*UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int secs_n, usecs_n;

	    *(tracep++) = ERTS_MT_TIME_INC_BDY_TAG;

	    hdrp = tracep;
	    tracep += 2;

	    PUT_VSZ_UI32(tracep, secs_n,  secs);
	    PUT_VSZ_UI32(tracep, usecs_n, usecs);

	    hdr = usecs_n;

	    hdr <<= UI32_MSB_EHF_SZ;
	    hdr |= secs_n;

	    WRITE_UI16(hdrp, hdr);
#ifdef DEBUG
	    check_time_inc_entry(hdrp-1, tracep,
				 (Uint32) secs, secs_n,
				 (Uint32) usecs, usecs_n);
#endif
	    res = 0;
	}
	else {
	    res = INVALID_TIME_INC;
	}
    }

    last_tv = tv;
    return res;
}


static void
disable_trace(int error, char *reason, int eno)
{
    char *mt_dis = "Memory trace disabled";
    char *eno_str;

    erts_mtrace_enabled = 0;
    erts_sock_close(socket_desc);
    socket_desc = ERTS_SOCK_INVALID_SOCKET;

    if (eno == 0)
	erts_fprintf(stderr, "%s: %s\n", mt_dis, reason);
    else {
	eno_str = erl_errno_id(eno);
	if (strcmp(eno_str, "unknown") == 0)
	    erts_fprintf(stderr, "%s: %s: %d\n", mt_dis, reason, eno);
	else
	    erts_fprintf(stderr, "%s: %s: %s\n", mt_dis, reason, eno_str);
    }
}

static int
send_trace_buffer(void)
{
    ssize_t ssz;
    size_t sz;

    sz = tracep - trace_buffer;
    tracep = trace_buffer;

    do {
	ssz = erts_sock_send(socket_desc, (void  *) tracep, sz);
	if (ssz < 0) {
	    int socket_errno = erts_sock_errno();

#ifdef EINTR
	    if (socket_errno == EINTR)
		continue;
#endif
	    disable_trace(0, "Connection lost", socket_errno);
	    return 0;
	}
	if (ssz > sz) {
	    disable_trace(1, "Unexpected error", 0);
	    return 0;
	}
	tracep += ssz;
	sz -= ssz;
    } while (sz);

    tracep = trace_buffer;
    return 1;
}

#if ERTS_ALC_N_MAX >= (1 << 16)
#error "Excessively large type numbers"
#endif


static int
write_trace_header(char *nodename, char *pid, char *hostname)
{
#ifdef DEBUG
    byte *startp;
#endif
    Uint16 entry_sz;
    Uint32 flags, n_len, h_len, p_len, hdr_prolog_len;
    int i, no, str_len;
    const char *str;
    struct {
	Uint32 gsec;
	Uint32 sec;
	Uint32 usec;
    } start_time;

    sys_gettimeofday(&last_tv);

    start_time.gsec = (Uint32) (last_tv.tv_sec / 1000000000);
    start_time.sec  = (Uint32) (last_tv.tv_sec % 1000000000);
    start_time.usec = (Uint32) last_tv.tv_usec;

    if (!MAKE_TBUF_SZ(3*UI32_SZ))
	return 0;

    flags = 0;
#ifdef ARCH_64
    flags |= ERTS_MT_64_BIT_FLAG;
#endif
    flags |= ERTS_MT_CRR_INFO;
#ifdef ERTS_CAN_TRACK_MALLOC
    flags |= ERTS_MT_SEG_CRR_INFO;
#endif

    /*
     * The following 3 ui32 words *always* have to come
     * first in the trace.
     */
    PUT_UI32(tracep, ERTS_MT_START_WORD);
    PUT_UI32(tracep, ERTS_MT_MAJOR_VSN);
    PUT_UI32(tracep, ERTS_MT_MINOR_VSN);

    n_len = strlen(nodename);
    h_len = strlen(hostname);
    p_len = strlen(pid);
    hdr_prolog_len = (2*UI32_SZ
		      + 3*UI16_SZ
		      + 3*UI32_SZ
		      + 3*UI8_SZ
		      + n_len
		      + h_len
		      + p_len);

    if (!MAKE_TBUF_SZ(hdr_prolog_len))
	return 0;

    /*
     * New stuff can be added at the end the of header prolog
     * (EOHP). The reader should skip stuff at the end, that it
     * doesn't understand.
     */

#ifdef DEBUG
    startp = tracep;
#endif

    PUT_UI32(tracep, hdr_prolog_len);
    PUT_UI32(tracep, flags);
    PUT_UI16(tracep, ERTS_MTRACE_SEGMENT_ID);
    PUT_UI16(tracep, ERTS_ALC_A_MAX);
    PUT_UI16(tracep, ERTS_ALC_N_MAX);

    PUT_UI32(tracep, start_time.gsec);
    PUT_UI32(tracep, start_time.sec);
    PUT_UI32(tracep, start_time.usec);

    PUT_UI8(tracep, (byte) n_len);
    memcpy((void *) tracep, (void *) nodename, n_len);
    tracep += n_len;

    PUT_UI8(tracep, (byte) h_len);
    memcpy((void *) tracep, (void *) hostname, h_len);
    tracep += h_len;

    PUT_UI8(tracep, (byte) p_len);
    memcpy((void *) tracep, (void *) pid, p_len);
    tracep += p_len;

    ASSERT(startp + hdr_prolog_len == tracep);

    /*
     * EOHP
     */

    /*
     * All tags from here on should be followed by an Uint16 size
     * field containing the total size of the entry.
     *
     * New stuff can eigther be added at the end of an entry, or
     * as a new tagged entry. The reader should skip stuff at the
     * end, that it doesn't understand.
     */

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	Uint16 aflags = 0;

#ifndef ERTS_CAN_TRACK_MALLOC
	if (i != ERTS_ALC_A_SYSTEM)
#endif
	    aflags |= ERTS_MT_ALLCTR_USD_CRR_INFO;

	str = ERTS_ALC_A2AD(i);
	ASSERT(str);
	str_len = strlen(str);
	if (str_len >= (1 << 8)) {
	    disable_trace(1, "Excessively large allocator string", 0);
	    return 0;
	}

	entry_sz = UI8_SZ + 3*UI16_SZ + UI8_SZ;
	entry_sz += (erts_allctrs_info[i].alloc_util ? 2 : 1)*UI16_SZ;
	entry_sz += UI8_SZ + str_len;

	if (!MAKE_TBUF_SZ(entry_sz))
	    return 0;

#ifdef DEBUG
	startp = tracep;
#endif
	PUT_UI8(tracep, ERTS_MT_ALLOCATOR_HDR_TAG);
	PUT_UI16(tracep, entry_sz);
	PUT_UI16(tracep, aflags);
	PUT_UI16(tracep, (Uint16) i);
	PUT_UI8( tracep, (byte) str_len);
	memcpy((void *) tracep, (void *) str, str_len);
	tracep += str_len;
	if (erts_allctrs_info[i].alloc_util) {
	    PUT_UI8(tracep, 2);
	    PUT_UI16(tracep, ERTS_MTRACE_SEGMENT_ID);
	    PUT_UI16(tracep, ERTS_ALC_A_SYSTEM);
	}
	else {
	    PUT_UI8(tracep, 1);
	    switch (i) {
	    case ERTS_ALC_A_SYSTEM:
		PUT_UI16(tracep, ERTS_MTRACE_SEGMENT_ID);
		break;
	    default:
		PUT_UI16(tracep, ERTS_MTRACE_SEGMENT_ID);
		break;
	    }
	}
	ASSERT(startp + entry_sz == tracep);
    }

    for (i = ERTS_ALC_N_MIN; i <= ERTS_ALC_N_MAX; i++) {
	Uint16 nflags = 0;
	str = ERTS_ALC_N2TD(i);
	ASSERT(str);

	str_len = strlen(str);
	if (str_len >= (1 << 8)) {
	    disable_trace(1, "Excessively large type string", 0);
	    return 0;
	}

	no = ERTS_ALC_T2A(ERTS_ALC_N2T(i));
	if (!erts_allctrs_info[no].enabled)
	    no = ERTS_ALC_A_SYSTEM;
	ASSERT(ERTS_ALC_A_MIN <= no && no <= ERTS_ALC_A_MAX);

	entry_sz = UI8_SZ + 3*UI16_SZ + UI8_SZ + str_len + UI16_SZ;

	if (!MAKE_TBUF_SZ(entry_sz))
	    return 0;

#ifdef DEBUG
	startp = tracep;
#endif
	PUT_UI8(tracep, ERTS_MT_BLOCK_TYPE_HDR_TAG);
	PUT_UI16(tracep, entry_sz);
	PUT_UI16(tracep, nflags);
	PUT_UI16(tracep, (Uint16) i);
	PUT_UI8(tracep, (byte) str_len);
	memcpy((void *) tracep, (void *) str, str_len);
	tracep += str_len;
	PUT_UI16(tracep, no);
	ASSERT(startp + entry_sz == tracep);
    }

    entry_sz = UI8_SZ + UI16_SZ;
    if (!MAKE_TBUF_SZ(entry_sz))
	return 0;
    PUT_UI8(tracep, ERTS_MT_END_OF_HDR_TAG);
    PUT_UI16(tracep, entry_sz);

    return 1;
}

static void mtrace_pre_lock(void);
static void mtrace_pre_unlock(void);
static void *mtrace_alloc(ErtsAlcType_t, void *, Uint);
static void *mtrace_realloc(ErtsAlcType_t, void *, void *, Uint);
static void mtrace_free(ErtsAlcType_t, void *, void *);

static ErtsAllocatorFunctions_t real_allctrs[ERTS_ALC_A_MAX+1];

void erts_mtrace_pre_init(void)
{
}

void erts_mtrace_init(char *receiver, char *nodename)
{
    char hostname[MAXHOSTNAMELEN + 1];
    char pid[21]; /* enough for a 64 bit number */

    socket_desc = ERTS_SOCK_INVALID_SOCKET;
    erts_mtrace_enabled = receiver != NULL;

    if (erts_mtrace_enabled) {
	unsigned a, b, c, d, p;
	byte ip_addr[4];
	Uint16 port;

	erts_mtx_init(&mtrace_buf_mutex, "mtrace_buf");
	erts_mtx_init(&mtrace_op_mutex, "mtrace_op");

	socket_desc = erts_sock_open();
	if (socket_desc == ERTS_SOCK_INVALID_SOCKET) {
	    disable_trace(1, "Failed to open socket", erts_sock_errno());
	    return;
	}

	if (5 != sscanf(receiver, "%u.%u.%u.%u:%u", &a, &b, &c, &d, &p)
	    || a >= (1 << 8) || b >= (1 << 8)|| c >= (1 << 8) || d >= (1 << 8)
	    || p >= (1 << 16)) {
	    disable_trace(1, "Invalid receiver address", 0);
	    return;
	}

	ip_addr[0] = (byte) a;
	ip_addr[1] = (byte) b;
	ip_addr[2] = (byte) c;
	ip_addr[3] = (byte) d; 

	port = (Uint16) p;

	if (!erts_sock_connect(socket_desc, ip_addr, 4, port)) {
	    disable_trace(1, "Failed to connect to receiver",
			  erts_sock_errno());
	    return;
	}
	tracep = trace_buffer;
	endp = trace_buffer + TRACE_BUF_SZ;
        /* gethostname requires that the len is max(hostname) + 1 */
	if (erts_sock_gethostname(hostname, MAXHOSTNAMELEN + 1) != 0)
	    hostname[0] = '\0';
	hostname[MAXHOSTNAMELEN] = '\0';
	sys_get_pid(pid, sizeof(pid));
	write_trace_header(nodename ? nodename : "", pid, hostname);
	erts_mtrace_update_heap_size();
    }
}

void
erts_mtrace_install_wrapper_functions(void)
{
    if (erts_mtrace_enabled) {
	int i;
	/* Install trace functions */
	ERTS_CT_ASSERT(sizeof(erts_allctrs) == sizeof(real_allctrs));

	sys_memcpy((void *) real_allctrs,
		   (void *) erts_allctrs,
		   sizeof(erts_allctrs));

	for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	    erts_allctrs[i].alloc	= mtrace_alloc;
	    erts_allctrs[i].realloc	= mtrace_realloc;
	    erts_allctrs[i].free	= mtrace_free;
	    erts_allctrs[i].extra	= (void *) &real_allctrs[i];
	}
	mtrace_wrapper.lock = mtrace_pre_lock;
	mtrace_wrapper.unlock = mtrace_pre_unlock;
	erts_allctr_wrapper_prelock_init(&mtrace_wrapper);
    }
}

void
erts_mtrace_stop(void)
{
    ASSERT(!erts_is_allctr_wrapper_prelocked());
    erts_mtx_lock(&mtrace_op_mutex);
    erts_mtx_lock(&mtrace_buf_mutex);
    if (erts_mtrace_enabled) {
	Uint32 ti = get_time_inc();
    
	if (ti != INVALID_TIME_INC
	    && MAKE_TBUF_SZ(UI8_SZ + UI16_SZ + UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int ti_n;

	    *(tracep++) = ERTS_MT_STOP_BDY_TAG;

	    hdrp = tracep;
	    tracep += 2;

	    PUT_VSZ_UI32(tracep, ti_n,  ti);

	    hdr = ti_n;

	    WRITE_UI16(hdrp, hdr);

	    if(send_trace_buffer()) {
		erts_mtrace_enabled = 0;
		erts_sock_close(socket_desc);
		socket_desc = ERTS_SOCK_INVALID_SOCKET;
	    }
	}
    }
    erts_mtx_unlock(&mtrace_buf_mutex);
    erts_mtx_unlock(&mtrace_op_mutex);
}

void
erts_mtrace_exit(Uint32 exit_value)
{
    ASSERT(!erts_is_allctr_wrapper_prelocked());
    erts_mtx_lock(&mtrace_op_mutex);
    erts_mtx_lock(&mtrace_buf_mutex);
    if (erts_mtrace_enabled) {
	Uint32 ti = get_time_inc();
    
	if (ti != INVALID_TIME_INC
	    && MAKE_TBUF_SZ(UI8_SZ + UI16_SZ + 2*UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int ti_n, exit_value_n;

	    *(tracep++) = ERTS_MT_EXIT_BDY_TAG;

	    hdrp = tracep;
	    tracep += 2;

	    PUT_VSZ_UI32(tracep, exit_value_n,  exit_value);
	    PUT_VSZ_UI32(tracep, ti_n,  ti);

	    hdr = ti_n;

	    hdr <<= UI32_MSB_EHF_SZ;
	    hdr |= exit_value_n;

	    WRITE_UI16(hdrp, hdr);

	    if(send_trace_buffer()) {
		erts_mtrace_enabled = 0;
		erts_sock_close(socket_desc);
		socket_desc = ERTS_SOCK_INVALID_SOCKET;
	    }
	}
    }
    erts_mtx_unlock(&mtrace_buf_mutex);
    erts_mtx_unlock(&mtrace_op_mutex);
}

static ERTS_INLINE void
write_alloc_entry(byte tag,
		  void *res,
		  ErtsAlcType_t x,
		  ErtsAlcType_t y,
		  Uint size)
{
    erts_mtx_lock(&mtrace_buf_mutex);
    if (erts_mtrace_enabled) {
	Uint32 ti = get_time_inc();

	if (ti != INVALID_TIME_INC
	    && MAKE_TBUF_SZ(UI8_SZ + 2*UI16_SZ + 2*UI_SZ + UI32_SZ)) {
	    Uint16 hdr, t_no = (Uint16) x, ct_no = (Uint16) y;
	    byte *hdrp;
	    int t_no_n, ct_no_n = 0, res_n, size_n, ti_n;

	    *(tracep++) = tag;

	    hdrp = tracep;
	    tracep += 2;

	    if (tag == ERTS_MT_CRR_ALLOC_BDY_TAG) {
		PUT_VSZ_UI16(tracep, ct_no_n, ct_no);
	    }
	    PUT_VSZ_UI16(tracep, t_no_n, t_no);
	    PUT_VSZ_UI(  tracep, res_n, res);
	    PUT_VSZ_UI(  tracep, size_n, size);
	    PUT_VSZ_UI32(tracep, ti_n, ti);

	    hdr = ti_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= size_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= res_n;

	    hdr <<= UI16_MSB_EHF_SZ;
	    hdr |= t_no_n;

	    if (tag == ERTS_MT_CRR_ALLOC_BDY_TAG) {
		hdr <<= UI16_MSB_EHF_SZ;
		hdr |= ct_no_n;
	    }

	    WRITE_UI16(hdrp, hdr);

#if TRACE_PRINTOUTS
	    print_trace_entry(tag,
			      ct_no, ct_no_n,
			      t_no, t_no_n,
			      (Uint) res, res_n,
			      0, 0,
			      size, size_n,
			      ti, ti_n);
#endif

#ifdef DEBUG
	    check_alloc_entry(hdrp-1, tracep,
			      tag,
			      ct_no, ct_no_n,
			      t_no, t_no_n,
			      (UWord) res, res_n,
			      size, size_n,
			      ti, ti_n);
#endif

	}

    }
    erts_mtx_unlock(&mtrace_buf_mutex);

}

static ERTS_INLINE void
write_realloc_entry(byte tag,
		    void *res,
		    ErtsAlcType_t x,
		    ErtsAlcType_t y,
		    void *ptr,
		    Uint size)
{
    erts_mtx_lock(&mtrace_buf_mutex);
    if (erts_mtrace_enabled) {
	Uint32 ti = get_time_inc();

	if (ti != INVALID_TIME_INC
	    && MAKE_TBUF_SZ(UI8_SZ + 2*UI16_SZ + 3*UI_SZ + UI32_SZ)) {
	    Uint16 hdr, t_no = (Uint16) x, ct_no = (Uint16) y;
	    byte *hdrp;
	    int t_no_n, ct_no_n = 0, res_n, ptr_n, size_n, ti_n;

	    *(tracep++) = tag;

	    hdrp = tracep;
	    tracep += 2;

	    if (tag == ERTS_MT_CRR_REALLOC_BDY_TAG) {
		PUT_VSZ_UI16(tracep, ct_no_n, ct_no);
	    }
	    PUT_VSZ_UI16(tracep, t_no_n, t_no);
	    PUT_VSZ_UI(  tracep, res_n, res);
	    PUT_VSZ_UI(  tracep, ptr_n, ptr);
	    PUT_VSZ_UI(  tracep, size_n, size);
	    PUT_VSZ_UI32(tracep, ti_n, ti);

	    hdr = ti_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= size_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= ptr_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= res_n;

	    hdr <<= UI16_MSB_EHF_SZ;
	    hdr |= t_no_n;

	    if (tag == ERTS_MT_CRR_REALLOC_BDY_TAG) {
		hdr <<= UI16_MSB_EHF_SZ;
		hdr |= ct_no_n;
	    }

	    WRITE_UI16(hdrp, hdr);

#if TRACE_PRINTOUTS
	    print_trace_entry(tag,
			      ct_no, ct_no_n,
			      t_no, t_no_n,
			      (Uint) res, res_n,
			      (Uint) ptr, ptr_n,
			      size, size_n,
			      ti, ti_n);
#endif

#ifdef DEBUG
	    check_realloc_entry(hdrp-1, tracep,
				tag,
				ct_no, ct_no_n,
				t_no, t_no_n,
				(UWord) res, res_n,
				(UWord) ptr, ptr_n,
				size, size_n,
				ti, ti_n);
#endif

	}
    }
    erts_mtx_unlock(&mtrace_buf_mutex);
}

static ERTS_INLINE void
write_free_entry(byte tag,
		 ErtsAlcType_t x,
		 ErtsAlcType_t y,
		 void *ptr)
{
    erts_mtx_lock(&mtrace_buf_mutex);
    if (erts_mtrace_enabled) {
	Uint32 ti = get_time_inc();

	if (ti != INVALID_TIME_INC
	    && MAKE_TBUF_SZ(UI8_SZ + 2*UI16_SZ + UI_SZ + UI32_SZ)) {
	    Uint16 hdr, t_no = (Uint16) x, ct_no = (Uint16) y;
	    byte *hdrp;
	    int t_no_n, ct_no_n = 0, ptr_n, ti_n;

	    *(tracep++) = tag;

	    hdrp = tracep;
	    tracep += 2;

	    if (tag == ERTS_MT_CRR_FREE_BDY_TAG) {
		PUT_VSZ_UI16(tracep, ct_no_n, ct_no);
	    }
	    PUT_VSZ_UI16(tracep, t_no_n, t_no);
	    PUT_VSZ_UI(  tracep, ptr_n,  ptr);
	    PUT_VSZ_UI32(tracep, ti_n,   ti);

	    hdr = ti_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= ptr_n;

	    hdr <<= UI16_MSB_EHF_SZ;
	    hdr |= t_no_n;

	    if (tag == ERTS_MT_CRR_FREE_BDY_TAG) {
		hdr <<= UI16_MSB_EHF_SZ;
		hdr |= ct_no_n;
	    }

	    WRITE_UI16(hdrp, hdr);

#if TRACE_PRINTOUTS
	    print_trace_entry(tag,
			      ct_no, ct_no_n,
			      t_no, t_no_n,
			      (Uint) 0, 0,
			      (Uint) ptr, ptr_n,
			      0, 0,
			      ti, ti_n);
#endif

#ifdef DEBUG
	    check_free_entry(hdrp-1, tracep,
			     tag,
			     ct_no, ct_no_n,
			     t_no, t_no_n,
			     (UWord) ptr, ptr_n,
			     ti, ti_n);
#endif
	}

    }
    erts_mtx_unlock(&mtrace_buf_mutex);
}

static void mtrace_pre_lock(void)
{
    erts_mtx_lock(&mtrace_op_mutex);
}

static void mtrace_pre_unlock(void)
{
    erts_mtx_unlock(&mtrace_op_mutex);
}


static void *
mtrace_alloc(ErtsAlcType_t n, void *extra, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    void *res;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&mtrace_op_mutex);
    }

    res = (*real_af->alloc)(n, real_af->extra, size);
    write_alloc_entry(ERTS_MT_ALLOC_BDY_TAG, res, n, 0, size);

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_unlock(&mtrace_op_mutex);
    }

    return res;
}

static void *
mtrace_realloc(ErtsAlcType_t n, void *extra, void *ptr, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    void *res;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&mtrace_op_mutex);
    }

    res = (*real_af->realloc)(n, real_af->extra, ptr, size);
    write_realloc_entry(ERTS_MT_REALLOC_BDY_TAG, res, n, 0, ptr, size);

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_unlock(&mtrace_op_mutex);
    }

    return res;

}

static void
mtrace_free(ErtsAlcType_t n, void *extra, void *ptr)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&mtrace_op_mutex);
    }

    (*real_af->free)(n, real_af->extra, ptr);
    if (!erts_is_allctr_wrapper_prelocked()) {
	write_free_entry(ERTS_MT_FREE_BDY_TAG, n, 0, ptr);
    }

    erts_mtx_unlock(&mtrace_op_mutex);
}


void
erts_mtrace_crr_alloc(void *res, ErtsAlcType_t n, ErtsAlcType_t m, Uint size)
{
    write_alloc_entry(ERTS_MT_CRR_ALLOC_BDY_TAG, res, n, m, size);
}

void
erts_mtrace_crr_realloc(void *res, ErtsAlcType_t n, ErtsAlcType_t m, void *ptr,
			Uint size)
{
    write_realloc_entry(ERTS_MT_CRR_REALLOC_BDY_TAG, res, n, m, ptr, size);
}

void
erts_mtrace_crr_free(ErtsAlcType_t n, ErtsAlcType_t m, void *ptr)
{
    write_free_entry(ERTS_MT_CRR_FREE_BDY_TAG, n, m, ptr);
}


#if TRACE_PRINTOUTS
static void
print_trace_entry(byte tag,
		  Uint16 t_no, int t_no_n,
		  Uint16 ct_no, int ct_no_n,
		  Uint res, int res_n,
		  Uint ptr, int ptr_n,
		  Uint size, int size_n,
		  Uint32 ti,int ti_n)
{
    switch (tag) {
    case ERTS_MT_ALLOC_BDY_TAG:
	fprintf(stderr,
		"{alloc, {%lu, %lu, %lu}, {%u, %u, %u, %u}}\n\r",

		(unsigned long) t_no, (unsigned long) res,
		(unsigned long) size,
	    
		MSB2BITS(t_no_n), MSB2BITS(res_n),
		MSB2BITS(size_n), MSB2BITS(ti_n));
	break;
    case ERTS_MT_REALLOC_BDY_TAG:
	fprintf(stderr,
		"{realloc, {%lu, %lu, %lu, %lu}, {%u, %u, %u, %u, %u}}\n\r",

		(unsigned long) t_no, (unsigned long) res,
		(unsigned long) ptr, (unsigned long) size,
	    
		MSB2BITS(t_no_n), MSB2BITS(res_n),
		MSB2BITS(ptr_n), MSB2BITS(size_n), MSB2BITS(ti_n));
	break;
    case ERTS_MT_FREE_BDY_TAG:
	fprintf(stderr,
		"{free, {%lu, %lu}, {%u, %u, %u, %u, %u}}\n\r",

		(unsigned long) t_no, (unsigned long) ptr,
	    
		MSB2BITS(t_no_n), MSB2BITS(ptr_n), MSB2BITS(ti_n));
	break;
    case ERTS_MT_CRR_ALLOC_BDY_TAG:
	fprintf(stderr,
		"{crr_alloc, {%lu, %lu, %lu, %lu}, {%u, %u, %u, %u, %u}}\n\r",

		(unsigned long) ct_no, (unsigned long) t_no,
		(unsigned long) res, (unsigned long) size,
	    
		MSB2BITS(ct_no_n), MSB2BITS(t_no_n),
		MSB2BITS(res_n), MSB2BITS(size_n),
		MSB2BITS(ti_n));
	break;
    case ERTS_MT_CRR_REALLOC_BDY_TAG:
	fprintf(stderr,
		"{crr_realloc, {%lu, %lu, %lu, %lu, %lu}, "
		"{%u, %u, %u, %u, %u, %u}}\n\r",

		(unsigned long) ct_no, (unsigned long) t_no,
		(unsigned long) res, (unsigned long) ptr,
		(unsigned long) size,
	    
		MSB2BITS(ct_no_n), MSB2BITS(t_no_n),
		MSB2BITS(res_n), MSB2BITS(ptr_n),
		MSB2BITS(size_n), MSB2BITS(ti_n));
	break;
    case ERTS_MT_CRR_FREE_BDY_TAG:
	fprintf(stderr,
		"{crr_free, {%lu, %lu, %lu}, {%u, %u, %u, %u}}\n\r",

		(unsigned long) ct_no, (unsigned long) t_no,
		(unsigned long) ptr,
	    
		MSB2BITS(ct_no_n), MSB2BITS(t_no_n),
		MSB2BITS(ptr_n), MSB2BITS(ti_n));
	break;
    default:
	fprintf(stderr, "{'\?\?\?'}\n\r");
	break;
    }
}

#endif /* #if TRACE_PRINTOUTS */

#ifdef DEBUG

#define GET_UI16(P) ((P) += UI16_SZ, \
		     (((Uint16) (*((P) - 2) << 8)) | ((Uint16) (*((P) - 1)))))

static void
check_ui(Uint16 *hdrp, byte **pp, Uint ui, int msb,
	 Uint16 f_mask, Uint16 f_size)
{
    Uint x;
    int n;

    ASSERT((msb & ~f_mask) == 0);

    n = (int) (*hdrp & f_mask);

    ASSERT(n == msb);

    *hdrp >>= f_size;

    x = 0;
    switch (n) {
#ifdef ARCH_64
    case 7: x |= *((*pp)++); x <<= 8;
    case 6: x |= *((*pp)++); x <<= 8;
    case 5: x |= *((*pp)++); x <<= 8;
    case 4: x |= *((*pp)++); x <<= 8;
#endif
    case 3: x |= *((*pp)++); x <<= 8;
    case 2: x |= *((*pp)++); x <<= 8;
    case 1: x |= *((*pp)++); x <<= 8;
    case 0: x |= *((*pp)++); break;
    default: ASSERT(0);
    }

    ASSERT(x == ui);
}


void
check_alloc_entry(byte *sp, byte *ep,
		  byte tag,
		  Uint16 ct_no, int ct_no_n,
		  Uint16 t_no, int t_no_n,
		  UWord res, int res_n,
		  Uint size, int size_n,
		  Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT(*p == tag);
    p++;

    hdr = GET_UI16(p);

    if (tag == ERTS_MT_CRR_ALLOC_BDY_TAG)
	check_ui(&hdr, &p, ct_no, ct_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, t_no, t_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, res,  res_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, size, size_n, UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);
}

void
check_realloc_entry(byte *sp, byte *ep,
		    byte tag,
		    Uint16 ct_no, int ct_no_n,
		    Uint16 t_no, int t_no_n,
		    UWord res, int res_n,
		    UWord ptr, int ptr_n,
		    Uint size, int size_n,
		    Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT(*p == tag);
    p++;

    hdr = GET_UI16(p);

    if (tag == ERTS_MT_CRR_REALLOC_BDY_TAG)
	check_ui(&hdr, &p, ct_no, ct_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, t_no, t_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, res,  res_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ptr,  ptr_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, size, size_n, UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);
}

void
check_free_entry(byte *sp, byte *ep,
		 byte tag,
		 Uint16 ct_no, int ct_no_n,
		 Uint16 t_no, int t_no_n,
		 UWord ptr, int ptr_n,
		 Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT(*p == tag);
    p++;

    hdr = GET_UI16(p);

    if (tag == ERTS_MT_CRR_FREE_BDY_TAG)
	check_ui(&hdr, &p, ct_no, ct_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, t_no, t_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, ptr,  ptr_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);

}

void
check_time_inc_entry(byte *sp, byte *ep,
		     Uint32 secs, int secs_n,
		     Uint32 usecs, int usecs_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT(*p == ERTS_MT_TIME_INC_BDY_TAG);
    p++;

    hdr = GET_UI16(p);

    check_ui(&hdr, &p, secs,  secs_n,  UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);
    check_ui(&hdr, &p, usecs, usecs_n, UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);

}

#endif /* #ifdef DEBUG */

