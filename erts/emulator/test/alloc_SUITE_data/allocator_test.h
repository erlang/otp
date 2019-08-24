/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#ifndef ALLOCATOR_TEST_H__
#define ALLOCATOR_TEST_H__

#if SIZEOF_VOID_P == SIZEOF_INT
typedef unsigned int Ulong;
#elif SIZEOF_VOID_P == SIZEOF_LONG
typedef unsigned long Ulong;
#elif SIZEOF_VOID_P == SIZEOF_LONG_LONG
typedef unsigned long long Ulong;
#else
# error No pointer sized integer type found ???
#endif

#ifdef __WIN32__
typedef Ulong erts_alc_test_Fn(Ulong, Ulong, Ulong, Ulong);
#  define erts_alc_test ((erts_alc_test_Fn*)WinDynNifCallbacks.erts_alc_test)
#else
Ulong erts_alc_test(Ulong, Ulong, Ulong, Ulong);
#endif

#define UNDEF__ ~((Ulong) 0)

#define ALC_TEST0(OP) \
  erts_alc_test((Ulong) (OP), UNDEF__, UNDEF__, UNDEF__)
#define ALC_TEST1(OP, A1) \
  erts_alc_test((Ulong) (OP), (Ulong) (A1), UNDEF__, UNDEF__)
#define ALC_TEST2(OP, A1, A2) \
  erts_alc_test((Ulong) (OP), (Ulong) (A1), (Ulong) (A2), UNDEF__)
#define ALC_TEST3(OP, A1, A2, A3) \
  erts_alc_test((Ulong) (OP), (Ulong) (A1), (Ulong) (A2), (Ulong) (A3))

typedef Ulong Block_t;
typedef Ulong Carrier_t;
typedef Ulong Allctr_t;
typedef Ulong RBT_t;
typedef Ulong RBTL_t;
typedef void* erts_thread;
typedef void* erts_mutex;
typedef void* erts_cond;

/* From erl_alloc_util.c */

#define BLK_SZ(B)		((Ulong)	ALC_TEST1(0x000, (B)))
#define UMEM_SZ(B)		((Ulong)	ALC_TEST1(0x001, (B)))
#define IS_PREV_FREE_BLK(B)	((Ulong)	ALC_TEST1(0x002, (B)))
#define IS_FREE_BLK(B)		((Ulong)	ALC_TEST1(0x003, (B)))
#define IS_LAST_BLK(B)		((Ulong)	ALC_TEST1(0x004, (B)))
#define UMEM2BLK(U)		((Block_t *)	ALC_TEST1(0x005, (U)))
#define BLK2UMEM(B)		((void *)	ALC_TEST1(0x006, (B)))
#define IS_SBC(C)		((Ulong)	ALC_TEST1(0x007, (C)))
#define IS_SBC_BLK(B)		((Ulong)	ALC_TEST1(0x008, (B)))
#define IS_MBC(C)		((Ulong)	ALC_TEST1(0x009, (C)))
#define IS_MMAP_C(C)		((Ulong)	ALC_TEST1(0x00a, (C)))
#define C_SZ(C)			((Ulong)	ALC_TEST1(0x00b, (C)))
#define SBC2BLK(A, C)		((Block_t *)	ALC_TEST2(0x00c, (A), (C)))
#define BLK_TO_SBC(A, B)	((Carrier_t *)	ALC_TEST2(0x00d, (A), (B)))
#define MBC_TO_FIRST_BLK(A, C)	((Block_t *)	ALC_TEST2(0x00e, (A), (C)))
#define FIRST_BLK_TO_MBC(A, B)	((Carrier_t *)	ALC_TEST2(0x00f, (A), (B)))
#define FIRST_MBC(A)		((Carrier_t *)	ALC_TEST1(0x010, (A)))
#define LAST_MBC(A)		((Carrier_t *)	ALC_TEST1(0x011, (A)))
#define FIRST_SBC(A)		((Carrier_t *)	ALC_TEST1(0x012, (A)))
#define LAST_SBC(A)		((Carrier_t *)	ALC_TEST1(0x013, (A)))
#define NEXT_C(C)		((Carrier_t *)	ALC_TEST1(0x014, (C)))
#define PREV_C(C)		((Carrier_t *)	ALC_TEST1(0x015, (C)))
#define ABLK_HDR_SZ		((Ulong)	ALC_TEST0(0x016))
#define MIN_BLK_SZ(A)		((Ulong)	ALC_TEST1(0x017, (A)))
#define NXT_BLK(B)		((Block_t *)	ALC_TEST1(0x018, (B)))
#define PREV_BLK(B)		((Block_t *)	ALC_TEST1(0x019, (B)))
#define IS_MBC_FIRST_BLK(A,B)	((Ulong)	ALC_TEST2(0x01a, (A), (B)))
#define UNIT_SZ			((Ulong)	ALC_TEST0(0x01b))
#define BLK_TO_MBC(B)		((Carrier_t *)	ALC_TEST1(0x01c, (B)))
#define ADD_MBC(A, C)        	((void)	ALC_TEST2(0x01d, (A), (C)))
#define REMOVE_MBC(A, C)       	((void)	ALC_TEST2(0x01e, (A), (C)))
#define ZERO_CRR_SIZE		((Ulong)	ALC_TEST0(0x01f))
#define ZERO_CRR_INIT(A,B)	((Carrier_t *)	ALC_TEST2(0x020, (A), (B)))
#define CPOOL_INSERT(A,B)	((Carrier_t *)	ALC_TEST2(0x021, (A), (B)))
#define CPOOL_DELETE(A,B)	((Carrier_t *)	ALC_TEST2(0x022, (A), (B)))
#define CPOOL_IS_EMPTY(A)	((int)		ALC_TEST1(0x023, (A)))
#define CPOOL_IS_IN_POOL(A,B)	((int)		ALC_TEST2(0x024, (A), (B)))
#define UMEM2BLK_TEST(P)	((Block_t*)	ALC_TEST1(0x025, (P)))

/* From erl_goodfit_alloc.c */
#define BKT_IX(A, S)		((Ulong)	ALC_TEST2(0x100, (A), (S)))
#define BKT_MIN_SZ(A, I)	((Ulong)	ALC_TEST2(0x101, (A), (I)))
#define NO_OF_BKTS		((Ulong)	ALC_TEST0(0x102))
#define FIND_BKT(A, I)		((int)		ALC_TEST2(0x103, (A), (I)))

/* From erl_bestfit_alloc.c and erl_ao_firstfit_alloc.c */
#define IS_AOBF(A)		((Ulong)	ALC_TEST1(RBT_OP(0), (A)))
#define RBT_ROOT(A,SZ)		((RBT_t *)	ALC_TEST2(RBT_OP(1), (A), (SZ)))
#define RBT_PARENT(T)		((RBT_t *)	ALC_TEST1(RBT_OP(2), (T)))
#define RBT_LEFT(T)		((RBT_t *)	ALC_TEST1(RBT_OP(3), (T)))
#define RBT_RIGHT(T)		((RBT_t *)	ALC_TEST1(RBT_OP(4), (T)))
#define RBT_NEXT(T)		((RBTL_t *)	ALC_TEST1(RBT_OP(5), (T)))
#define RBT_IS_BLACK(T)		((Ulong)	ALC_TEST1(RBT_OP(6), (T)))
#define RBT_IS_TREE(T)		((Ulong)	ALC_TEST1(RBT_OP(7), (T)))
#define IS_BF_ALGO(A)		((Ulong)	ALC_TEST1(RBT_OP(8), (A)))
#define RBT_MAX_SZ(T)		((Ulong)	ALC_TEST1(RBT_OP(9), (T)))
#define IS_BF(A)		((Ulong)	ALC_TEST1(RBT_OP(0xa), (A)))
#define RBT_PREV(T)		((RBTL_t *)	ALC_TEST1(RBT_OP(0xb), (T)))

/* From erl_mseg.c */
#define HAVE_MSEG()		((int)		ALC_TEST0(0x400))
#define MSEG_ALLOC(SP)		((void *)	ALC_TEST1(0x401, (SP)))
#define MSEG_DEALLOC(P, S)	((void)		ALC_TEST2(0x402, (P), (S)))
#define MSEG_REALLOC(P, OS, SP)	((void *)	ALC_TEST3(0x403, (P), (OS), \
							  (SP)))
#define MSEG_CLEAR_CACHE()	((void)		ALC_TEST0(0x404))
#define MSEG_NO()		((Ulong)	ALC_TEST0(0x405))
#define MSEG_CACHE_SIZE()	((Ulong)	ALC_TEST0(0x406))

/* From erl_alloc.c */

#undef  ALLOC
#undef  REALLOC
#undef  FREE

#define ALLOC(A, S)		((void *)	ALC_TEST2(0xf00, (A), (S)))
#define REALLOC(A, P, S)	((void *)	ALC_TEST3(0xf01, (A), (P), (S)))
#define FREE(A, P)		((void)		ALC_TEST2(0xf02, (A), (P)))
#define START_ALC(N, T, A) 	((Allctr_t *)	ALC_TEST3(0xf03, (N), (T), (A)))
#define STOP_ALC(A)		((void)		ALC_TEST1(0xf04, (A)))
#define IS_THREADS_ENABLED	((int)		ALC_TEST0(0xf05))
#define IS_ALLOC_THREAD_SAFE(A)	((int)		ALC_TEST1(0xf06, (A)))
#define IS_ALLOC_FORK_SAFE(A)	((int)		ALC_TEST1(0xf07, (A)))
#define THR_MTX_CREATE()	((erts_mutex)	ALC_TEST0(0xf08))
#define THR_MTX_DESTROY(M)	((void)		ALC_TEST1(0xf09, (M)))
#define THR_MTX_LOCK(M)		((void)		ALC_TEST1(0xf0a, (M)))
#define THR_MTX_UNLOCK(M)	((void)		ALC_TEST1(0xf0b, (M)))
#define THR_COND_CREATE()	((erts_cond)	ALC_TEST0(0xf0c))
#define THR_COND_DESTROY(C)	((void)		ALC_TEST1(0xf0d, (C)))
#define THR_COND_BCAST(C)	((void)		ALC_TEST1(0xf0e, (C)))
#define THR_COND_WAIT(C, M)	((void)		ALC_TEST2(0xf0f, (C), (M)))
#define THR_CREATE(F, A)	((erts_thread)	ALC_TEST2(0xf10, (F), (A)))
#define THR_JOIN(T)		((void)		ALC_TEST1(0xf11, (T)))
#define THR_EXIT(R)		((void)		ALC_TEST1(0xf12, (R)))
#define IS_SMP_ENABLED		((int)		ALC_TEST0(0xf13))
#define ALLOC_TEST(S)		((void*)	ALC_TEST1(0xf14, (S)))
#define FREE_TEST(P)		((void)		ALC_TEST1(0xf15, (P)))
#define REALLOC_TEST(P,S)	((void*)	ALC_TEST2(0xf16, (P), (S)))
#define SET_TEST_MBC_USER_HEADER(SZ,CMBC,DMBC) ((int)ALC_TEST3(0xf17, (SZ), (CMBC), (DMBC)))
#define GET_TEST_MBC_SIZE()     ((int)          ALC_TEST0(0xf18))

#endif
