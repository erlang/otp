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

#include "testcase_driver.h"
#include "allocator_test.h"
#include <stdio.h>
#include <string.h>

#if 1
#define PRINT_ALLOC_OPS
#endif

#define SBC_THRESHOLD 8192
#define NO_OF_BLOCKS   7
#define NO_OF_ALLOC_OPS_PER_BLOCK 700

typedef struct {
    unsigned char *p;
    Ulong  s;
    int   i;
    Ulong *as;
} block;

Ulong alloc_seq_1[] = {
  SBC_THRESHOLD,     /* mmap */
  SBC_THRESHOLD*4,   /* mmap to new mmap */
  SBC_THRESHOLD/50,  /* mmap to malloc */
  SBC_THRESHOLD,     /* malloc to mmap */
  0
};


Ulong alloc_seq_2[] = {
  1,
  SBC_THRESHOLD/10,
  SBC_THRESHOLD/9,
  SBC_THRESHOLD/8,
  SBC_THRESHOLD/7,
  SBC_THRESHOLD/6,
  SBC_THRESHOLD/5,
  SBC_THRESHOLD/4,
  SBC_THRESHOLD/3,
  SBC_THRESHOLD/2,
  SBC_THRESHOLD*1,
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*3,
  SBC_THRESHOLD*4,
  SBC_THRESHOLD*5,
  SBC_THRESHOLD*6,
  SBC_THRESHOLD*7,
  SBC_THRESHOLD*8,
  SBC_THRESHOLD*9,
  SBC_THRESHOLD*10,
  SBC_THRESHOLD*9,
  SBC_THRESHOLD*8,
  SBC_THRESHOLD*7,
  SBC_THRESHOLD*6,
  SBC_THRESHOLD*5,
  SBC_THRESHOLD*4,
  SBC_THRESHOLD*3,
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*1,
  SBC_THRESHOLD/2,
  SBC_THRESHOLD/3,
  SBC_THRESHOLD/4,
  SBC_THRESHOLD/5,
  SBC_THRESHOLD/6,
  SBC_THRESHOLD/7,
  SBC_THRESHOLD/8,
  SBC_THRESHOLD/9,
  SBC_THRESHOLD/10,
  1,
  0
};

Ulong alloc_seq_3[] = {
  SBC_THRESHOLD*11,
  SBC_THRESHOLD*10,
  SBC_THRESHOLD*9,
  SBC_THRESHOLD*8,
  SBC_THRESHOLD*7,
  SBC_THRESHOLD*6,
  SBC_THRESHOLD*5,
  SBC_THRESHOLD*4,
  SBC_THRESHOLD*3,
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*1,
  SBC_THRESHOLD/2,
  SBC_THRESHOLD/3,
  SBC_THRESHOLD/4,
  SBC_THRESHOLD/5,
  SBC_THRESHOLD/6,
  SBC_THRESHOLD/7,
  SBC_THRESHOLD/8,
  SBC_THRESHOLD/9,
  SBC_THRESHOLD/10,
  1,
  SBC_THRESHOLD/10,
  SBC_THRESHOLD/9,
  SBC_THRESHOLD/8,
  SBC_THRESHOLD/7,
  SBC_THRESHOLD/6,
  SBC_THRESHOLD/5,
  SBC_THRESHOLD/4,
  SBC_THRESHOLD/3,
  SBC_THRESHOLD/2,
  SBC_THRESHOLD*1,
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*3,
  SBC_THRESHOLD*4,
  SBC_THRESHOLD*5,
  SBC_THRESHOLD*6,
  SBC_THRESHOLD*7,
  SBC_THRESHOLD*8,
  SBC_THRESHOLD*9,
  SBC_THRESHOLD*10,
  0
};

Ulong alloc_seq_4[] = {
  SBC_THRESHOLD*1,
  SBC_THRESHOLD*10,
  SBC_THRESHOLD*1,
  0
};

Ulong alloc_seq_5[] = {
  SBC_THRESHOLD/50,
  SBC_THRESHOLD*10,
  SBC_THRESHOLD/50,
  0
};

Ulong alloc_seq_6[] = {
  SBC_THRESHOLD/50,
  SBC_THRESHOLD*10,
  SBC_THRESHOLD/50,
  SBC_THRESHOLD*10,
  0
};

Ulong alloc_seq_7[] = {
  1,
  SBC_THRESHOLD/50,
  SBC_THRESHOLD*10,
  SBC_THRESHOLD/50,
  0
};


block blocks[NO_OF_BLOCKS] = {{NULL, 0, 0, alloc_seq_1},
			      {NULL, 0, 0, alloc_seq_2},
			      {NULL, 0, 0, alloc_seq_3},
			      {NULL, 0, 0, alloc_seq_4},
			      {NULL, 0, 0, alloc_seq_5},
			      {NULL, 0, 0, alloc_seq_6},
			      {NULL, 0, 0, alloc_seq_7}};

#define CHECK_BLOCK_DATA(T, P, S, D) \
  check_block_data(__FILE__, __LINE__, (T), (P), (S), (D))

static void
check_block_data(char *file, int line,
		 TestCaseState_t *tcs, unsigned char *p, Ulong sz, int d)
{
    Ulong i;
    for (i = 0; i < sz; i++)
	if (p[i] != (unsigned char) d)
	    testcase_failed(tcs, "%s:%d: Data clobbered! found id=%d; "
			    "expected id=%d\n", file, line, (int) p[i], d);
}


static void
alloc_op(TestCaseState_t *tcs, Allctr_t *a, block *bp, int id, int clean_up)
{
    if(bp->p)
	CHECK_BLOCK_DATA(tcs, bp->p, bp->s, id);

    if(bp->as[bp->i] == 0 || clean_up) {
	FREE(a, bp->p);
#ifdef PRINT_ALLOC_OPS
	testcase_printf(tcs, "FREE(0x%lx) [id=%d]\n", (Ulong) bp->p, id);
#endif
	bp->p = NULL;
	bp->s = 0;
	bp->i = 0; /* start from the beginning again */
	return;
    }

    if(!bp->p) {
	bp->s = bp->as[bp->i];
	bp->p = (unsigned char *) ALLOC(a, bp->s);
#ifdef PRINT_ALLOC_OPS
	testcase_printf(tcs, "0x%lx = ALLOC(%lu) [id=%d]\n",
			(Ulong) bp->p, bp->s, id);
#endif
	if(!bp->p)
	    testcase_failed(tcs, "ALLOC(%lu) failed [id=%d])\n", bp->s, id);
	memset((void *) bp->p, id, (size_t) bp->s);
    }
    else {
	unsigned char *p = (unsigned char *) REALLOC(a, bp->p, bp->as[bp->i]);
#ifdef PRINT_ALLOC_OPS
	testcase_printf(tcs, "0x%lx = REALLOC(0x%lx, %lu) [id=%d]\n",
			(Ulong) p, (Ulong) bp->p, bp->as[bp->i], id);
#endif
	if(!p) {
	    testcase_failed(tcs, "REALLOC(0x%lx, %lu) failed [id=%d]\n",
			    (Ulong) bp->p, bp->as[bp->i], id);
	}

	if(bp->s < bp->as[bp->i]) {
	    CHECK_BLOCK_DATA(tcs, p, bp->s, id);
	    memset((void *) p, id, (size_t) bp->as[bp->i]);
	}
	else
	    CHECK_BLOCK_DATA(tcs, p, bp->as[bp->i], id);

	bp->s = bp->as[bp->i];
	bp->p = p;
    }

    bp->i++;
}

char *
testcase_name(void)
{
    return "realloc_copy";
}

void
testcase_run(TestCaseState_t *tcs)
{
    int i, j;
    char sbct_buf[20];
    char *argv[] = {"-tmmsbc", "5000", "-tsbct", &sbct_buf[0], NULL};
    Allctr_t *a;

    sprintf(sbct_buf, "%d", SBC_THRESHOLD/1024);

    a = START_ALC("realloc_copy_", 0, argv);
    ASSERT(tcs, a);
    tcs->extra = (void *) a;

    for(i = 0; i < NO_OF_ALLOC_OPS_PER_BLOCK; i++)
	for(j = 0; j < NO_OF_BLOCKS; j++)	
	    alloc_op(tcs, a, &blocks[j], j + 1, 0);

    for(j = 0; j < NO_OF_BLOCKS; j++)
	alloc_op(tcs, a, &blocks[j], j + 1, 1);
    
    STOP_ALC((Allctr_t *) tcs->extra);
    tcs->extra = NULL;
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    if (tcs->extra)
	STOP_ALC((Allctr_t *) tcs->extra);
}

ERL_NIF_INIT(realloc_copy, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
