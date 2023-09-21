/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017-2023. All Rights Reserved.
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
#include <stdio.h>
#include <stdlib.h>
#include "wxe_nif.h"

ERL_NIF_TERM WXE_ATOM_ok;
ERL_NIF_TERM WXE_ATOM_undefined;
ERL_NIF_TERM WXE_ATOM_badarg;
ERL_NIF_TERM WXE_ATOM_true;
ERL_NIF_TERM WXE_ATOM_false;

ERL_NIF_TERM WXE_ATOM_wx;
ERL_NIF_TERM WXE_ATOM_reply;
ERL_NIF_TERM WXE_ATOM_error;
ERL_NIF_TERM WXE_ATOM_wx_ref;
ERL_NIF_TERM WXE_ATOM__wx_invoke_cb_;

ERL_NIF_TERM WXE_ATOM_define;
ERL_NIF_TERM WXE_ATOM_global;

ERL_NIF_TERM WXE_ATOM_wxWindow;
ERL_NIF_TERM WXE_ATOM_wxSizer;

ERL_NIF_TERM WXE_ATOM_wxDC;
ERL_NIF_TERM WXE_ATOM_wxWindowDC;
ERL_NIF_TERM WXE_ATOM_wxMemoryDC;
ERL_NIF_TERM WXE_ATOM_wxRegion;

ERL_NIF_TERM WXE_ATOM_wxGraphicsContext;
ERL_NIF_TERM WXE_ATOM_wxGraphicsBitmap;
ERL_NIF_TERM WXE_ATOM_wxGraphicsPath;
ERL_NIF_TERM WXE_ATOM_wxBitmap;
ERL_NIF_TERM WXE_ATOM_wxGraphicsBrush;
ERL_NIF_TERM WXE_ATOM_wxBrush;
ERL_NIF_TERM WXE_ATOM_wxGraphicsPen;
ERL_NIF_TERM WXE_ATOM_wxPen;
ERL_NIF_TERM WXE_ATOM_wxCursor;
ERL_NIF_TERM WXE_ATOM_wxImage;
ERL_NIF_TERM WXE_ATOM_wxIcon;
ERL_NIF_TERM WXE_ATOM_wxIconBundle;

ERL_NIF_TERM WXE_ATOM_wxPrintData;
ERL_NIF_TERM WXE_ATOM_wxPageSetupDialogData;
ERL_NIF_TERM WXE_ATOM_wxPrintDialogData;


ErlNifResourceType* wxeMemEnvRt = NULL;
int wxe_debug = 0;

extern void wxe_initOpenGL(void * fptr, void *debug);

// void destroyMemEnv(wxeMemEnv *memenv);

int get_ptr(ErlNifEnv* env, ERL_NIF_TERM term, void** dp)
{
    if(sizeof(void *) == sizeof(int))
        return enif_get_uint(env, term, (unsigned int *) dp);
    else
        return enif_get_uint64(env, term, (ErlNifUInt64 *) dp);
}

int wxe_get_size_t(ErlNifEnv* env, ERL_NIF_TERM term, size_t* dp)
{
    if(sizeof(int) == sizeof(size_t))
        return enif_get_uint(env, term, (unsigned int *) dp);
    else
        return enif_get_uint64(env, term, (ErlNifUInt64 *) dp);
}

int wxe_get_float(ErlNifEnv* env, ERL_NIF_TERM term, float* dp)
{
    double dtemp;
    int itemp;
    if(enif_get_double(env, term, &dtemp)) {
        *dp = (float) dtemp;
        return 1;
    } else if(enif_get_int(env, term, &itemp)) {
        *dp = (float) itemp;
        return 1;
    } else return 0;
}

int wxe_get_double(ErlNifEnv* env, ERL_NIF_TERM term, double* dp)
{
    ErlNifSInt64 itemp;
    if(enif_get_double(env, term, dp)) {
        return 1;
    } else if(enif_get_int64(env, term, &itemp)) {
        *dp = (double) itemp;
        return 1;
    } else return 0;
}


static ERL_NIF_TERM wx_setup_cmd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int op;
    wxe_me_ref *ptr;
    if(!enif_get_int(env, argv[argc-1], &op))
        return enif_make_badarg(env);
    if(op >= OPENGL_START || op < 50) {
        push_nif(env, argc-1, argv, op, NULL);
    } else if(enif_get_resource(env, argv[argc-2], wxeMemEnvRt, (void **) &ptr)) {
        push_nif(env, argc-2, argv, op, ptr);
    } else return enif_make_badarg(env);
    return WXE_ATOM_ok;
}

static ERL_NIF_TERM wx_init_opengl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void * fptr;
    void * debug;
    if(!get_ptr(env, argv[0], &fptr))
        return enif_make_badarg(env);
    if(!get_ptr(env, argv[1], &debug))
        return enif_make_badarg(env);

    wxe_initOpenGL(fptr, debug);
    return WXE_ATOM_ok;
}

static ERL_NIF_TERM wxe_make_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* The returned memenv must only be manipulated by the wx_thread */
    wxe_me_ref * ptr = enif_alloc_resource(wxeMemEnvRt, sizeof(wxe_me_ref));
    ptr->memenv = newMemEnv(env, ptr);

    return enif_make_resource(env, ptr);
}

static ERL_NIF_TERM wxe_delete_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    wxe_me_ref * obj;
    if(!enif_get_resource(env, argv[argc-1], wxeMemEnvRt, (void **) &obj))
        obj = NULL;
    meta_command(env, WXE_DELETE_ENV, obj);
    return WXE_ATOM_ok;
}

static ERL_NIF_TERM wxe_debug_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int debug;
    if(enif_get_int(env, argv[0], &debug)) {
        if(debug) wxe_debug = 1;
        else wxe_debug = 0;
    }
    return enif_make_int(env, wxe_debug);
}


static ERL_NIF_TERM wxe_get_consts(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    meta_command(env, WXE_GET_CONSTS, NULL);
    return WXE_ATOM_ok;
}


// Callback
static void wxe_destroy_env(ErlNifEnv* env, void *obj)
{
    if(wxe_debug)
        enif_fprintf(stderr, "Deleting enif_env\r\n");
}

static void wxe_process_down(ErlNifEnv* env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    push_nif(env, 0, NULL, WXE_CB_DIED, obj);
}

static ErlNifFunc nif_funcs[] =
{
    {"queue_cmd", 1, wx_setup_cmd},
    {"queue_cmd", 2, wx_setup_cmd},
    {"queue_cmd", 3, wx_setup_cmd},
    {"queue_cmd", 4, wx_setup_cmd},
    {"queue_cmd", 5, wx_setup_cmd},
    {"queue_cmd", 6, wx_setup_cmd},
    {"queue_cmd", 7, wx_setup_cmd},
    {"queue_cmd", 8, wx_setup_cmd},
    {"queue_cmd", 9, wx_setup_cmd},
    {"queue_cmd",10, wx_setup_cmd},
    {"queue_cmd",11, wx_setup_cmd},
    {"queue_cmd",12, wx_setup_cmd},
    {"queue_cmd",13, wx_setup_cmd},
    {"queue_cmd",14, wx_setup_cmd},
    {"queue_cmd",15, wx_setup_cmd},
    {"init_opengl", 2, wx_init_opengl},
    {"make_env", 0, wxe_make_env},
    {"delete_env", 1, wxe_delete_env},
    {"debug_driver", 1, wxe_debug_driver},
    {"get_consts_impl", 0, wxe_get_consts}
};

void wxe_init_atoms(ErlNifEnv *env) {
    WXE_ATOM_ok = enif_make_atom(env, "ok");
    WXE_ATOM_badarg = enif_make_atom(env, "badarg");
    WXE_ATOM_undefined = enif_make_atom(env, "undefined");
    WXE_ATOM_true = enif_make_atom(env, "true");
    WXE_ATOM_false = enif_make_atom(env, "false");

    WXE_ATOM_wx = enif_make_atom(env, "wx");
    WXE_ATOM_wx_ref = enif_make_atom(env, "wx_ref");
    WXE_ATOM_reply = enif_make_atom(env, "_wxe_result_");
    WXE_ATOM_error = enif_make_atom(env, "_wxe_error_");
    WXE_ATOM__wx_invoke_cb_ = enif_make_atom(env, "_wx_invoke_cb_");

    WXE_ATOM_define = enif_make_atom(env, "define");
    WXE_ATOM_global = enif_make_atom(env, "global");

    WXE_ATOM_wxWindow = enif_make_atom(env, "wxWindow");
    WXE_ATOM_wxSizer = enif_make_atom(env, "wxSizer");
    WXE_ATOM_wxDC = enif_make_atom(env, "wxDC");
    WXE_ATOM_wxWindowDC = enif_make_atom(env, "wxWindowDC");
    WXE_ATOM_wxMemoryDC = enif_make_atom(env, "wxMemoryDC");
    WXE_ATOM_wxRegion = enif_make_atom(env, "wxRegion");

    WXE_ATOM_wxGraphicsContext = enif_make_atom(env, "wxGraphicsContext");
    WXE_ATOM_wxGraphicsBitmap = enif_make_atom(env, "wxGraphicsBitmap");
    WXE_ATOM_wxGraphicsPath = enif_make_atom(env, "wxGraphicsPath");
    WXE_ATOM_wxBitmap = enif_make_atom(env, "wxBitmap");
    WXE_ATOM_wxGraphicsBrush = enif_make_atom(env, "wxGraphicsBrush");
    WXE_ATOM_wxBrush = enif_make_atom(env, "wxBrush");
    WXE_ATOM_wxGraphicsPen = enif_make_atom(env, "wxGraphicsPen");
    WXE_ATOM_wxPen = enif_make_atom(env, "wxPen");
    WXE_ATOM_wxCursor = enif_make_atom(env, "wxCursor");
    WXE_ATOM_wxImage = enif_make_atom(env, "wxImage");
    WXE_ATOM_wxIcon = enif_make_atom(env, "wxIcon");
    WXE_ATOM_wxIconBundle = enif_make_atom(env, "wxIconBundle");

    WXE_ATOM_wxPrintData = enif_make_atom(env, "wxPrintData");
    WXE_ATOM_wxPageSetupDialogData = enif_make_atom(env, "wxPageSetupDialogData");
    WXE_ATOM_wxPrintDialogData = enif_make_atom(env, "wxPrintDialogData");

}

static int wxe_init(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    ErlNifResourceTypeInit init = {wxe_destroy_env, NULL, wxe_process_down};

    wxe_init_atoms(env);

    wxeMemEnvRt = enif_open_resource_type_x(env, "wxMemEnv", &init, ERL_NIF_RT_CREATE, NULL);

    if(start_native_gui(env) == WXE_INITIATED)
        return 0;
    else
        return 1;
}

static void wxe_unload(ErlNifEnv *env, void *priv_data)
{
    stop_native_gui(env);
}

ERL_NIF_INIT(wxe_util,nif_funcs,wxe_init,NULL,NULL,wxe_unload)
