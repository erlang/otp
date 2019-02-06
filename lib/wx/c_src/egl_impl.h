/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

#include "erl_driver.h"

/* Wrap everything from glext.h so we are not dependent on the user version of it */

#ifndef _WIN32
# include <dlfcn.h>
#endif

#ifdef _WIN32
#include <windows.h>  
#include <gl/gl.h>
#include <gl/glu.h>
#elif defined(HAVE_GL_GL_H)
#include <GL/gl.h>
# include <GL/glu.h>	
#elif defined(HAVE_OPENGL_GL_H)
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

/* Define int32_t, int64_t, and uint64_t types for UST/MSC */
/* (as used in the GLX_OML_sync_control extension). */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#include <inttypes.h>
#elif defined(__sun__)
#include <inttypes.h>
#if defined(__STDC__)
#if defined(__arch64__)
typedef long int int64_t;
typedef unsigned long int uint64_t;
#else
typedef long long int int64_t;
typedef unsigned long long int uint64_t;
#endif /* __arch64__ */
#endif /* __STDC__ */
#elif defined( __VMS )
#include <inttypes.h>
#elif defined(__SCO__) || defined(__USLC__)
#include <stdint.h>
#elif defined(__UNIXOS2__) || defined(__SOL64__)
typedef long int int32_t;
typedef long long int int64_t;
typedef unsigned long long int uint64_t;
#elif defined(WIN32) && defined(_MSC_VER)
typedef __int32 int32_t;
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
#elif defined(WIN32) && defined(__GNUC__)
#include <stdint.h>
#else
#include <inttypes.h>     /* Fallback option */
#endif

#ifndef HAVE_GLINT64EXT
typedef int64_t GLint64EXT;
typedef uint64_t GLuint64EXT;
#endif

#ifndef GL_ARB_sync
typedef int64_t GLint64;
typedef uint64_t GLuint64;
typedef struct __GLsync *GLsync;
#endif

/* External Api */

#ifdef _WIN32
extern "C" __declspec(dllexport) int  egl_init_opengl(void *);
extern "C" __declspec(dllexport) void egl_dispatch(int, char *, ErlDrvPort, ErlDrvTermData, char **, int *);
#else 
extern "C" int egl_init_opengl(void *);
extern "C" void egl_dispatch(int, char *, ErlDrvPort, ErlDrvTermData, char **, int *);
#endif

/* internal */
int erl_tess_impl(char* buff, ErlDrvPort port, ErlDrvTermData caller);
void gl_error();
extern int gl_error_op;
extern ErlDrvTermData gl_active;

