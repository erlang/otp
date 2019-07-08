/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2018. All Rights Reserved.
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
 * Purpose : Defines and macros for the debug util of the socket and 
 * net NIF(s).
 * ----------------------------------------------------------------------
 *
 */

#ifndef SOCKET_DBG_H__
#define SOCKET_DBG_H__

/* Used when calling the init function */
#define ESOCK_DBGOUT_DEFAULT "stdout"
#define ESOCK_DBGOUT_UNIQUE  "unique"


/* Used in debug printouts */
#ifdef __WIN32__
#define LLU "%I64u"
#else
#define LLU "%llu"
#endif
typedef unsigned long long llu_t;

extern FILE* esock_dbgout; // Initiated by the 'init' function

#define ESOCK_DBG_PRINTF( ___COND___ , proto ) \
    if ( ___COND___ ) {                        \
        esock_dbg_printf proto;                \
        fflush(esock_dbgout);                  \
    }


extern void esock_dbg_init(char* filename);
extern void esock_dbg_printf( const char* prefix, const char* format, ... );

#endif // SOCKET_DBG_H__
