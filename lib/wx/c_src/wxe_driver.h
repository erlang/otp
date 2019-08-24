/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

#ifndef _WXE_DRIVER_H
#define _WXE_DRIVER_H
#include "erl_driver.h"

typedef unsigned char Uint8;
typedef unsigned int Uint32;

#define MAX_FUNCTIONS_H 2000

#define error() {fprintf(stderr, "Error in %s:%d \n\r", \
                                 __FILE__, __LINE__); \
                return;}

typedef struct wxe_bin_ref * WXEBinRefptr;

typedef struct wxe_bin_ref {
   char*  base;
   size_t size;
   ErlDrvBinary* bin;
   ErlDrvTermData from;
} WXEBinRef;

typedef struct wxe_data_def {
   void * driver_data;
   WXEBinRef * bin;		/* Argument binaries */
   Uint32 max_bins;
   ErlDrvPort port_handle;
   ErlDrvTermData port;
   int is_cbport;
   ErlDrvPDL pdl;
} wxe_data;


/* Number of bins per port should be small */
#define DEF_BINS 3

void init_glexts(wxe_data*);

int  start_native_gui(wxe_data *sd);
void stop_native_gui(wxe_data *sd);

#define FUNC_CALL    13
#define CREATE_PORT  14
#define DELETE_PORT  15
#define PING_PORT    16


void push_command(int op,char * buf,int len, wxe_data *);
void meta_command(int what, wxe_data *sd);

void * wxe_ps_init();
void * wxe_ps_init2();

extern ErlDrvPort WXE_DRV_PORT_HANDLE;
extern ErlDrvTermData WXE_DRV_PORT;
extern int wxe_debug;

extern char * erl_wx_privdir;

#define WXE_BATCH_BEGIN     0
#define WXE_BATCH_END       1
#define WXE_CREATE_PORT     2
#define WXE_REMOVE_PORT     3
#define DESTROY_OBJECT      4
#define WXE_CB_RETURN       5
#define WXE_SHUTDOWN        6
#define WXE_REGISTER_OBJECT 7
#define WXE_CB_START        8
#define WXE_DEBUG_DRIVER    9
#define WXE_DEBUG_PING      10
#define WXE_BIN_INCR        11
#define WXE_BIN_DECR        12
#define WXE_INIT_OPENGL     13
#define WXE_CB_DIED         14

#define OPENGL_START        5000

#endif

