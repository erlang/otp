/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2023. All Rights Reserved.
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
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#endif

#include "egl_impl.h"

#define WX_DEF_EXTS
#include "gen/gl_fdefs.h"

void init_tess();
void exit_tess();
int load_gl_functions();

/* ****************************************************************************
 * OPENGL INITIALIZATION
 *
 * Function initializer and loader, part of erl_gl dynamic library.
 * Also supports gl_nif files
 *
 * NOTE: Must function loading must be done after the context is set and in
 *       the gui thread
 *****************************************************************************/

ERL_NIF_TERM EGL_ATOM_OK;
ERL_NIF_TERM EGL_ATOM_REPLY;
ERL_NIF_TERM EGL_ATOM_ERROR;
ERL_NIF_TERM EGL_ATOM_BADARG;

static ErlNifFunc egl_funcs[] =
{
    {"lookup_func_nif", 1, egl_lookup_func_func}
};
static int egl_init(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    EGL_ATOM_OK = enif_make_atom(env, "ok");
    EGL_ATOM_BADARG = enif_make_atom(env, "badarg");
    EGL_ATOM_REPLY = enif_make_atom(env, "_egl_result_");
    EGL_ATOM_ERROR = enif_make_atom(env, "_egl_error_");

    return 0;
}

ERL_NIF_INIT(gl, egl_funcs, egl_init, NULL, NULL, NULL)

int egl_get_float(ErlNifEnv* env, ERL_NIF_TERM term, GLfloat* dp)
{
    double temp;
    if(enif_get_double(env, term, &temp)) {
        *dp = (GLfloat) temp;
        return 1;
    } else return 0;
}

int egl_get_short(ErlNifEnv* env, ERL_NIF_TERM term, GLshort* dp)
{
    int temp;
    if(enif_get_int(env, term, &temp)) {
        *dp = (GLshort) temp;
        return 1;
    } else return 0;
}

int egl_get_ushort(ErlNifEnv* env, ERL_NIF_TERM term, GLushort* dp)
{
    unsigned int temp;
    if(enif_get_uint(env, term, &temp)) {
        *dp = (GLushort) temp;
        return 1;
    } else return 0;
}

int egl_get_byte(ErlNifEnv* env, ERL_NIF_TERM term, GLbyte* dp)
{
    int temp;
    if(enif_get_int(env, term, &temp)) {
        *dp = (GLbyte) temp;
        return 1;
    } else return 0;
}

int egl_get_ubyte(ErlNifEnv* env, ERL_NIF_TERM term, GLubyte* dp)
{
    unsigned int temp;
    if(enif_get_uint(env, term, &temp)) {
        *dp = (GLubyte) temp;
        return 1;
    } else return 0;
}

int egl_get_word(ErlNifEnv* env, ERL_NIF_TERM term, egl_word* dp)
{
    if(sizeof(egl_word) == sizeof(int))
        return enif_get_int(env, term, (signed int *) dp);
    else
        return enif_get_int64(env, term, (ErlNifSInt64 *) dp);
}

int egl_get_ptr(ErlNifEnv* env, ERL_NIF_TERM term, void** dp)
{
    if(sizeof(void *) == sizeof(int))
        return enif_get_uint(env, term, (unsigned int *) dp);
    else
        return enif_get_uint64(env, term, (ErlNifUInt64 *) dp);
}

void egl_badarg(ErlNifEnv* env, ErlNifPid *self, int op, const char * argc) {
    const char * func;
    func = gl_fns[op-GLE_LIB_START].name;
    enif_send(NULL, self, env,
              enif_make_tuple3(env, EGL_ATOM_ERROR,
                               enif_make_tuple2(env, enif_make_int(env, op),
                                                enif_make_string(env, func, ERL_NIF_LATIN1)),
                               enif_make_tuple2(env, EGL_ATOM_BADARG,
                                                enif_make_string(env, argc, ERL_NIF_LATIN1))));
}

void * egl_lookup_func(int op)
{
    return gl_fns[op-GLE_LIB_START].nif_cb;
}

const char * egl_lookup_func_desc(int op)
{
    return gl_fns[op-GLE_LIB_START].name;
}


ERL_NIF_TERM egl_lookup_func_func(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    egl_uword func = 0;
    unsigned int which;
    if(!(enif_get_uint(env, argv[0], &which)) && !(which == 1 || which == 2))
        return enif_make_badarg(env);
    if(which == 1)
        func = (egl_uword) egl_lookup_func;
    if(which == 2)
        func = (egl_uword) egl_lookup_func_desc;
    if(sizeof(void *) == sizeof(unsigned int))
        return enif_make_uint(env, (unsigned int) func);
    else
        return enif_make_uint64(env, (ErlNifUInt64) func);
}

#ifdef _WIN32
#define RTLD_LAZY 0
#define OPENGL_LIB L"opengl32.dll"
#define OPENGLU_LIB L"glu32.dll"
typedef HMODULE DL_LIB_P;
typedef WCHAR DL_CHAR;
void * dlsym(HMODULE Lib, const char *func) {
  void * p;
  p = (void *) wglGetProcAddress(func);
  if(p == 0 || (p == (void *) 0x1) || (p == (void *) 0x2)
     || (p == (void *) 0x3) || (p == (void *) -1) ) {
      p = (void *) GetProcAddress(Lib, func);
  }
  return p;
}

HMODULE dlopen(const WCHAR *DLL, int unused) {
  return LoadLibrary(DLL);
}

void dlclose(HMODULE Lib) {
  FreeLibrary(Lib);
}

#else
typedef void * DL_LIB_P;
typedef char DL_CHAR;
# ifdef _MACOSX
#  define OPENGL_LIB "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib"
#  define OPENGLU_LIB "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGLU.dylib"
# else
#  define OPENGL_LIB "libGL.so.1"
#  define OPENGLU_LIB "libGLU.so.1"
# endif
#endif

/* NOTE: Must be done after the context is set and in the gui thread */
int egl_load_functions() {
    DL_CHAR * DLName;
    DL_LIB_P LIBhandle;
    void * func = NULL;
    int i;

#ifdef _WIN32
    if(!wglGetCurrentContext())
        enif_fprintf(stderr, "wglGetCurrentContext is not set this will not work\r\n");
#endif

    /* Load GLU functions */
    DLName = (DL_CHAR *) OPENGLU_LIB;
    LIBhandle = dlopen(DLName, RTLD_LAZY);
    // fprintf(stderr, "Loading GLU: %s\r\n", (const char*)DLName);
    func = NULL;

    if(LIBhandle) {
        for(i=0; i < (GLE_GL_FUNC_START-GLE_LIB_START); i++) {
            if(gl_fns[i].func) {
                if((func = dlsym(LIBhandle, gl_fns[i].name))) {
                    * (void **) (gl_fns[i].func) = func;
                } else {
                    if(gl_fns[i].alt != NULL) {
                        if((func = dlsym(LIBhandle, gl_fns[i].alt))) {
                            * (void **) (gl_fns[i].func) = func;
                        } else {
                            * (void **) (gl_fns[i].func) = NULL;
                            gl_fns[i].nif_cb = NULL;
                            // fprintf(stderr, "GLU Skipped %s\r\n", glu_fns[i].alt);
                        };
                    } else {
                        * (void **) (gl_fns[i].func) = NULL;
                        gl_fns[i].nif_cb = NULL;
                        // fprintf(stderr, "GLU Skipped %s\r\n", glu_fns[i].name);
                    }
                }
            }
        }
        // dlclose(LIBhandle);
        // fprintf(stderr, "GLU library is loaded\r\n");
    } else {
        for(i=0; i < GLE_GL_FUNC_START; i++) {
            gl_fns[i].nif_cb = NULL;
        }
        fprintf(stderr, "Could NOT load OpenGL GLU library: %s\r\n", (char *) DLName);
    };

    /* Load GL functions */

    DLName = (DL_CHAR *) OPENGL_LIB;
    LIBhandle = dlopen(DLName, RTLD_LAZY);
    func = NULL;

    if(LIBhandle) {
        for(; i <= (GLE_GL_FUNC_LAST-GLE_LIB_START); i++) {
            if(gl_fns[i].func) {
                if((func = dlsym(LIBhandle, gl_fns[i].name))) {
                    * (void **) (gl_fns[i].func) = func;
                    // fprintf(stderr, "GL LOADED %s \r\n", gl_fns[i].name);
                } else {
                    if(gl_fns[i].alt != NULL) {
                        if((func = dlsym(LIBhandle, gl_fns[i].alt))) {
                            * (void **) (gl_fns[i].func) = func;
                            // fprintf(stderr, "GL LOADED %s \r\n", gl_fns[i].alt);
                        } else {
                            * (void **) (gl_fns[i].func) = NULL;
                            gl_fns[i].nif_cb = NULL;
                            // fprintf(stderr, "GL Skipped %s and %s \r\n", gl_fns[i].name, gl_fns[i].alt);
                        };
                    } else {
                        * (void **) (gl_fns[i].func) = NULL;
                        gl_fns[i].nif_cb = NULL;
                        // fprintf(stderr, "GL Skipped %s \r\n", gl_fns[i].name);
                    }
                }
            }
        }
        // dlclose(LIBhandle);
        // fprintf(stderr, "OPENGL library is loaded\r\n");
    } else {
        for(i=0; i <= (GLE_GL_FUNC_LAST-GLE_LIB_START); i++) {
            gl_fns[i].nif_cb = NULL;
        }
        fprintf(stderr, "Could NOT load OpenGL library: %s\r\n", (char *) DLName);
    };

    return 0;
}

/* *******************************************************************************
 * GLU Tessellation special
 * ******************************************************************************/

static GLUtesselator* tess;

typedef struct {
    GLdouble * tess_coords;
    int alloc_n;
    int alloc_max;

    int * tess_index_list;
    int index_n;
    int index_max;

    int error;
} egl_tess_data;

#define NEED_MORE_ALLOC 1
#define NEED_MORE_INDEX 2

static egl_tess_data egl_tess;

void CALLBACK
egl_ogla_vertex(GLdouble* coords)
{
    /* fprintf(stderr, "%d\r\n", (int) (coords - tess_coords) / 3); */
    if(egl_tess.index_n < egl_tess.index_max) {
        egl_tess.tess_index_list[egl_tess.index_n] = (int) (coords - egl_tess.tess_coords) / 3;
        egl_tess.index_n++;
    }
    else
        egl_tess.error = NEED_MORE_INDEX;
}

void CALLBACK
egl_ogla_combine(GLdouble coords[3],
                 void* vertex_data[4],
                 GLfloat w[4],
                 void **dataOut)
{
    GLdouble* vertex = &egl_tess.tess_coords[egl_tess.alloc_n];
    if(egl_tess.alloc_n < egl_tess.alloc_max) {
        egl_tess.alloc_n += 3;
        vertex[0] = coords[0];
        vertex[1] = coords[1];
        vertex[2] = coords[2];
        *dataOut = vertex;

#if 0
        fprintf(stderr, "combine: ");
        int i;
        for (i = 0; i < 4; i++) {
            if (w[i] > 0.0) {
                fprintf(stderr, "%d(%g) ", (int) vertex_data[i], w[i]);
            }
        }
        fprintf(stderr, "\r\n");
        fprintf(stderr, "%g %g %g\r\n", vertex[0], vertex[1], vertex[2]);
#endif

    } else {
        egl_tess.error = NEED_MORE_ALLOC;
        *dataOut = NULL;
    }
}

void CALLBACK
egl_ogla_edge_flag(GLboolean flag)
{
}

void CALLBACK
egl_ogla_error(GLenum errorCode)
{
    // const GLubyte *err;
    // err = gluErrorString(errorCode);
    // fprintf(stderr, "Tessellation error: %d: %s\r\n", (int) errorCode, err);
}

void init_tess()
{
    tess = gluNewTess();

    gluTessCallback(tess, GLU_TESS_VERTEX,     (GLUfuncptr) egl_ogla_vertex);
    gluTessCallback(tess, GLU_TESS_EDGE_FLAG,  (GLUfuncptr) egl_ogla_edge_flag);
    gluTessCallback(tess, GLU_TESS_COMBINE,    (GLUfuncptr) egl_ogla_combine);
    gluTessCallback(tess, GLU_TESS_ERROR,      (GLUfuncptr) egl_ogla_error);

}

void exit_tess()
{
    gluDeleteTess(tess);
}

void erl_tess_impl(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
    int i, a;
    unsigned int num_vertices;
    GLdouble n[3], *vs;
    ErlNifBinary bin;
    const ERL_NIF_TERM *tuple;
    ERL_NIF_TERM vs_l, vs_h, vs_t, reply;

    int a_max = 2;
    int i_max = 6;

    if(!enif_get_tuple(env, argv[0], &a, &tuple) && a != 3) Badarg(5009, "Normal");
    if(!enif_get_double(env, tuple[0], &n[0])) Badarg(5009,"Normal");
    if(!enif_get_double(env, tuple[1], &n[1])) Badarg(5009,"Normal");
    if(!enif_get_double(env, tuple[2], &n[2])) Badarg(5009,"Normal");

    if(!enif_get_list_length(env, argv[1], &num_vertices)) Badarg(5009,"Vs");
    egl_tess.alloc_max = a_max*num_vertices*3;
    egl_tess.error = 0;
    enif_alloc_binary(egl_tess.alloc_max*sizeof(GLdouble), &bin);
    vs = (GLdouble *) bin.data;
    egl_tess.tess_coords = vs;

    vs_l = argv[1];
    while(enif_get_list_cell(env,  vs_l, &vs_h, &vs_t)) {
        if(!enif_get_tuple(env, vs_h, &a, &tuple) && a != 3) Badarg(5009,"Vs");
        if(!enif_get_double(env, tuple[0], vs++)) Badarg(5009,"Normal");
        if(!enif_get_double(env, tuple[1], vs++)) Badarg(5009,"Normal");
        if(!enif_get_double(env, tuple[2], vs++)) Badarg(5009,"Normal");
        vs_l = vs_t;
    }
    egl_tess.index_max = i_max*3*num_vertices;
    egl_tess.tess_index_list = (int *) enif_alloc(sizeof(int) * egl_tess.index_max);

    egl_tess.index_n = 0;
    egl_tess.alloc_n = num_vertices*3;

    gluTessNormal(tess, n[0], n[1], n[2]);
    gluTessBeginPolygon(tess, 0);
    gluTessBeginContour(tess);
    for (i = 0; i < num_vertices; i++) {
        gluTessVertex(tess, egl_tess.tess_coords+3*i, egl_tess.tess_coords+3*i);
    }
    gluTessEndContour(tess);
    gluTessEndPolygon(tess);

    vs_t = enif_make_list(env, 0);
    i=egl_tess.index_n;
    while(i > 0) {
        i--;
        vs_t = enif_make_list_cell(env, enif_make_int(env, egl_tess.tess_index_list[i]), vs_t);
    };

    enif_realloc_binary(&bin, egl_tess.alloc_n*sizeof(GLdouble));
    reply = enif_make_tuple2(env, vs_t, enif_make_binary(env, &bin));
    enif_send(NULL, self, env, enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
    /* fprintf(stderr, "List %d: %d %d %d \r\n",  */
    /* 	  res, */
    /* 	  n_pos,  */
    /* 	  (tess_alloc_vertex-new_vertices)*sizeof(GLdouble),  */
    /* 	  num_vertices*6*sizeof(GLdouble)); */
    enif_free(egl_tess.tess_index_list);
}
