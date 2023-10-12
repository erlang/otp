/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2010-2022. All Rights Reserved.
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
/* Wrap everything from glext.h so we are not dependent on the user version of it */

#ifndef _WIN32
# include <dlfcn.h>
#endif

#ifdef _WIN32
#include <windows.h>  
#include <gl/gl.h>
#include <gl/glu.h>
#elif defined(HAVE_GL_GL_H) && defined(HAVE_GL_GLU_H)
#include <GL/gl.h>
# include <GL/glu.h>	
#elif defined(HAVE_OPENGL_GL_H) && defined(HAVE_OPENGL_GLU_H)
#include <OpenGL/gl.h> 
#include <OpenGL/glu.h> 
#endif

#ifndef APIENTRY
#define APIENTRY 
#endif

#ifndef CALLBACK
# define CALLBACK
#endif

#ifdef _WIN32
# ifndef _GLUfuncptr
//  Visual studio CPP ++ compiler
#  define _GLUfuncptr void (_stdcall *)() 
# endif
#endif

#ifdef _GLUfuncptr
# define GLUfuncptr _GLUfuncptr
#elif defined(TESS_CB_TIGER_STYLE)
# define GLUfuncptr GLvoid (*)(...)
#else 
# define GLUfuncptr GLvoid (*)()
#endif

#ifdef _WIN64
typedef unsigned long long int egl_uword;
typedef signed   long long int egl_word;
#else
typedef unsigned long  int     egl_uword;
typedef signed   long  int     egl_word;
#endif

typedef ErlNifSInt64 egl_int64_t;
typedef ErlNifUInt64 egl_uint64_t;

/* Some new GL types (eliminates the need for glext.h) */

#ifndef HAVE_GLINTPTR
#ifndef HAVE_GLINTPTRARB
# include <stddef.h>
/* GL types for handling large vertex buffer objects */
typedef ptrdiff_t GLintptrARB;
typedef ptrdiff_t GLsizeiptrARB;
#endif  /* HAVE_GLINTPTRARB */
typedef GLintptrARB   GLintptr;
typedef GLsizeiptrARB GLsizeiptr;
#endif  /* HAVE_GLINTPTR */

#ifndef HAVE_GLCHAR
# ifndef HAVE_GLCHARARB
/* GL types for handling shader object handles and characters */
typedef char GLcharARB;		     /* native character */
typedef unsigned int GLhandleARB;    /* shader object handle */
#endif  /* HAVE_GLCHARARB */
typedef GLcharARB GLchar;
#endif

#ifndef HAVE_GLHALFARB 
/* GL types for "half" precision (s10e5) float data in host memory */
typedef unsigned short GLhalfARB;
#endif

#ifndef HAVE_GLINT64EXT
typedef egl_int64_t GLint64EXT;
typedef egl_uint64_t GLuint64EXT;
#endif

#ifndef GL_ARB_sync
typedef egl_int64_t GLint64;
typedef egl_uint64_t GLuint64;
typedef struct __GLsync *GLsync;
#endif

#ifndef DEF_EGL_CMD
typedef struct egl_cmd_t {
    ErlNifEnv *env;
    ERL_NIF_TERM args[16];
    void (*fptr) (ErlNifEnv *, ErlNifPid *, ERL_NIF_TERM *);
    ErlNifPid pid;
} egl_cmd;
#endif

typedef struct {
    int op;
    const char * name;
    const char * alt;
    void * func;
    void (*nif_cb) (ErlNifEnv *, ErlNifPid *, ERL_NIF_TERM *);
} gl_fns_t;

extern gl_fns_t gl_fns[];
extern int egl_load_functions();

/* internal */
void init_tess();
void erl_tess_impl(ErlNifEnv *, ErlNifPid *, ERL_NIF_TERM *);
extern int gl_error_op;

extern ERL_NIF_TERM EGL_ATOM_OK;
extern ERL_NIF_TERM EGL_ATOM_REPLY;
extern ERL_NIF_TERM EGL_ATOM_ERROR;
extern ERL_NIF_TERM EGL_ATOM_BADARG;

#define Badarg(Op, Argc) {egl_badarg(env,self,Op,Argc); return;}

int egl_get_float(ErlNifEnv* env, ERL_NIF_TERM term, GLfloat* dp);
int egl_get_short(ErlNifEnv* env, ERL_NIF_TERM term, GLshort* dp);
int egl_get_ushort(ErlNifEnv* env, ERL_NIF_TERM term, GLushort* dp);
int egl_get_byte(ErlNifEnv* env, ERL_NIF_TERM term, GLbyte* dp);
int egl_get_ubyte(ErlNifEnv* env, ERL_NIF_TERM term, GLubyte* dp);
int egl_get_word(ErlNifEnv* env, ERL_NIF_TERM term, egl_word * dp);
int egl_get_ptr(ErlNifEnv* env, ERL_NIF_TERM term, void** dp);
ERL_NIF_TERM egl_lookup_func_func(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
void egl_badarg(ErlNifEnv* env, ErlNifPid *self, int op, const char * argc);
