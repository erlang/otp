/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2019-2022. All Rights Reserved.
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

#include <erl_nif.h>

extern int wxe_debug;
extern ErlNifResourceType* wxeMemEnvRt;

extern ERL_NIF_TERM WXE_ATOM_ok;
extern ERL_NIF_TERM WXE_ATOM_badarg;
extern ERL_NIF_TERM WXE_ATOM_undefined;
extern ERL_NIF_TERM WXE_ATOM_true;
extern ERL_NIF_TERM WXE_ATOM_false;

extern ERL_NIF_TERM WXE_ATOM_wx;
extern ERL_NIF_TERM WXE_ATOM_wx_ref;
extern ERL_NIF_TERM WXE_ATOM_reply;
extern ERL_NIF_TERM WXE_ATOM_error;
extern ERL_NIF_TERM WXE_ATOM__wx_invoke_cb_;

extern ERL_NIF_TERM WXE_ATOM_define;
extern ERL_NIF_TERM WXE_ATOM_global;

/* Used for comparisons */
extern ERL_NIF_TERM WXE_ATOM_wxWindow;
extern ERL_NIF_TERM WXE_ATOM_wxSizer;

extern ERL_NIF_TERM WXE_ATOM_wxDC;
extern ERL_NIF_TERM WXE_ATOM_wxWindowDC;
extern ERL_NIF_TERM WXE_ATOM_wxMemoryDC;
extern ERL_NIF_TERM WXE_ATOM_wxRegion;

extern ERL_NIF_TERM WXE_ATOM_wxGraphicsContext;
extern ERL_NIF_TERM WXE_ATOM_wxGraphicsBitmap;
extern ERL_NIF_TERM WXE_ATOM_wxGraphicsPath;
extern ERL_NIF_TERM WXE_ATOM_wxBitmap;
extern ERL_NIF_TERM WXE_ATOM_wxGraphicsBrush;
extern ERL_NIF_TERM WXE_ATOM_wxBrush;
extern ERL_NIF_TERM WXE_ATOM_wxGraphicsPen;
extern ERL_NIF_TERM WXE_ATOM_wxPen;
extern ERL_NIF_TERM WXE_ATOM_wxCursor;
extern ERL_NIF_TERM WXE_ATOM_wxImage;
extern ERL_NIF_TERM WXE_ATOM_wxIcon;
extern ERL_NIF_TERM WXE_ATOM_wxIconBundle;

extern ERL_NIF_TERM WXE_ATOM_wxPrintData;
extern ERL_NIF_TERM WXE_ATOM_wxPageSetupDialogData;
extern ERL_NIF_TERM WXE_ATOM_wxPrintDialogData;
/* extern ERL_NIF_TERM WXE_ATOM_wx */

#define WXE_NOT_INITIATED 0
#define WXE_INITIATED     1
#define WXE_EXITING       2
#define WXE_EXITED        3
#define WXE_ERROR        -1

#define OPENGL_START 5000

// META COMMANDS

#define WXE_BATCH_BEGIN      5
#define WXE_BATCH_END        6

#define WXE_CB_START         9
#define WXE_CB_DIED         10
#define WXE_CB_RETURN       11

#define WXE_SHUTDOWN        13
#define WXE_DEBUG_PING      14
#define WXE_DELETE_ENV      15
#define WXE_GET_CONSTS      16

// Taylormade functions

#define WXE_DESTROY_OBJECT  50
#define WXE_REGISTER_OBJECT 51 // Used for object monitoring
// #define WXE_INIT_OPENGL 52


typedef struct {
    void *memenv;
} wxe_me_ref;

int wxe_get_size_t(ErlNifEnv* env, ERL_NIF_TERM term, size_t* dp);
int wxe_get_float(ErlNifEnv* env, ERL_NIF_TERM term, float* dp);
int wxe_get_double(ErlNifEnv* env, ERL_NIF_TERM term, double* dp);
/* wxe_impl.cpp functions */

void push_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int op, wxe_me_ref * mp);
void meta_command(ErlNifEnv* env, int op, wxe_me_ref * mr);
void * newMemEnv(ErlNifEnv* env, wxe_me_ref *);
ERL_NIF_TERM impl_get_consts(ErlNifEnv* env);

/* wxe_main functions */

int  start_native_gui(ErlNifEnv *);
void stop_native_gui(ErlNifEnv *);

/* wxe_ps_init */
void * wxe_ps_init();
void * wxe_ps_init2();

#ifdef  _MACOSX
int is_packaged_app();
#endif
