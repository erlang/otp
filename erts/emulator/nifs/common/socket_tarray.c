/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2019. All Rights Reserved.
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
 *
 * ----------------------------------------------------------------------
 *  Purpose : Build and "maintain" a (erlang) term array of 
 *            variable length.
 * ----------------------------------------------------------------------
 *
 */

/* #ifdef HAVE_CONFIG_H */
/* #include "config.h" */
/* #endif */

#include <stdio.h>

#include <erl_nif.h>

#include "socket_int.h"
#include <sys.h>
#include "socket_util.h"
#include "socket_tarray.h"



/* ----------------------------------------------------------------------
 * Types 
 */

typedef struct {
  Uint32        sz;
  Uint32        idx;
  ERL_NIF_TERM* array;
} SocketTArrayInt;


/* ----------------------------------------------------------------------
 * Forward for internal functions
 */

static void esock_tarray_add1(SocketTArrayInt* taP, ERL_NIF_TERM t);
static void esock_tarray_ensure_fits(SocketTArrayInt* taP, Uint32 needs);


/* ----------------------------------------------------------------------
 * API
 */

extern
void* esock_tarray_create(Uint32 sz)
{
    SocketTArrayInt* tarrayP;

    ESOCK_ASSERT( (sz > 0) );

    tarrayP = MALLOC(sizeof(SocketTArrayInt));
    ESOCK_ASSERT( (tarrayP != NULL) );
  
    tarrayP->array = MALLOC(sz * sizeof(ERL_NIF_TERM));
    ESOCK_ASSERT( (tarrayP->array != NULL) );
    tarrayP->sz   = sz;
    tarrayP->idx  = 0;
    
    return ((SocketTArray) tarrayP);
}

extern
void esock_tarray_delete(SocketTArray ta)
{
    SocketTArrayInt* taP = (SocketTArrayInt*) ta;

    FREE(taP->array);
    FREE(taP);
}


extern
Uint32 esock_tarray_sz(SocketTArray a)
{
  return ( ((SocketTArrayInt*) a)->idx );
}

extern
void esock_tarray_add(SocketTArray ta, ERL_NIF_TERM t)
{
    esock_tarray_add1((SocketTArrayInt*) ta, t);
}

extern
void esock_tarray_tolist(SocketTArray  ta,
                         ErlNifEnv*    env,
                         ERL_NIF_TERM* list)
{
    SocketTArrayInt* taP = (SocketTArrayInt*) ta;

    *list = MKLA(env, taP->array, taP->idx);

    esock_tarray_delete(taP);
}



/* ----------------------------------------------------------------------
 * "Internal" functions
 */

static
void esock_tarray_add1(SocketTArrayInt* taP, ERL_NIF_TERM t)
{
  esock_tarray_ensure_fits(taP, 1);

  taP->array[taP->idx++] = t;
}

static
void esock_tarray_ensure_fits(SocketTArrayInt* taP, Uint32 needs)
{ 
  if (taP->sz < (taP->idx + needs)) {
    Uint32 newSz = (needs < taP->sz) ? 2*taP->sz : 2*needs;
    void*  mem   = REALLOC(taP->array, newSz * sizeof(ERL_NIF_TERM));
    
    ESOCK_ASSERT( (mem != NULL) );
    
    taP->sz    = newSz;
    taP->array = (ERL_NIF_TERM*) mem;
  }
}
