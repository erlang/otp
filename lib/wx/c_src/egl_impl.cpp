/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
#include "gen/gl_finit.h"
#include "gen/glu_finit.h"

void init_tess();
void exit_tess();
int load_gl_functions();

/* ****************************************************************************
 * OPENGL INITIALIZATION
 *****************************************************************************/

int egl_initiated = 0;

#ifdef _WIN32
#define RTLD_LAZY 0
#define OPENGL_LIB L"opengl32.dll"
#define OPENGLU_LIB L"glu32.dll"
typedef HMODULE DL_LIB_P;
typedef WCHAR DL_CHAR;
void * dlsym(HMODULE Lib, const char *func) {
  void * funcp;
  if((funcp = (void *) GetProcAddress(Lib, func)))
    return funcp;
  else
    return (void *) wglGetProcAddress(func);
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
extern "C" {
DRIVER_INIT(EGL_DRIVER) {
  return NULL;
}
}

int egl_init_opengl(void *erlCallbacks)
{
#ifdef _WIN32
  driver_init((TWinDynDriverCallbacks *) erlCallbacks);
#endif
  if(egl_initiated == 0) {
    if(load_gl_functions()) {
      init_tess();
      egl_initiated = 1;
    }
  }
  return 1;
}

int load_gl_functions() {
  DL_CHAR * DLName = (DL_CHAR *) OPENGL_LIB;
  DL_LIB_P LIBhandle = dlopen(DLName, RTLD_LAZY);
  //fprintf(stderr, "Loading GL: %s\r\n", (const char*)DLName);
  void * func = NULL;
  int i;

  if(LIBhandle) {
    for(i=0; gl_fns[i].name != NULL; i++) {
      if((func = dlsym(LIBhandle, gl_fns[i].name))) {
	* (void **) (gl_fns[i].func) = func;
	// fprintf(stderr, "GL LOADED %s \r\n", gl_fns[i].name);
      } else {
	if(gl_fns[i].alt != NULL) {
	  if((func = dlsym(LIBhandle, gl_fns[i].alt))) {
	    * (void **) (gl_fns[i].func) = func;
	    // fprintf(stderr, "GL LOADED %s \r\n", gl_fns[i].alt);
	  } else {
	    * (void **) (gl_fns[i].func) = (void *) &gl_error;
	    // fprintf(stderr, "GL Skipped %s and %s \r\n", gl_fns[i].name, gl_fns[i].alt);
	  };
	} else {
	  * (void **) (gl_fns[i].func) = (void *) &gl_error;
	  // fprintf(stderr, "GL Skipped %s \r\n", gl_fns[i].name);
	}
      }
    }
    // dlclose(LIBhandle);
    // fprintf(stderr, "OPENGL library is loaded\r\n");
  } else {
    fprintf(stderr, "Could NOT load OpenGL library: %s\r\n", DLName);
  };

  DLName = (DL_CHAR *) OPENGLU_LIB;
  LIBhandle = dlopen(DLName, RTLD_LAZY);
  // fprintf(stderr, "Loading GLU: %s\r\n", (const char*)DLName);
  func = NULL;

  if(LIBhandle) {
    for(i=0; glu_fns[i].name != NULL; i++) {
      if((func = dlsym(LIBhandle, glu_fns[i].name))) {
	* (void **) (glu_fns[i].func) = func;
      } else {
	if(glu_fns[i].alt != NULL) {
	  if((func = dlsym(LIBhandle, glu_fns[i].alt))) {
	    * (void **) (glu_fns[i].func) = func;
	  } else {
	    * (void **) (glu_fns[i].func) = (void *) &gl_error;
	    // fprintf(stderr, "GLU Skipped %s\r\n", glu_fns[i].alt);
	  };
	} else {
	  * (void **) (glu_fns[i].func) = (void *) &gl_error;
	  // fprintf(stderr, "GLU Skipped %s\r\n", glu_fns[i].name);
	}
      }
    }
    // dlclose(LIBhandle);
    // fprintf(stderr, "GLU library is loaded\r\n");
  } else {
    fprintf(stderr, "Could NOT load OpenGL GLU library: %s\r\n", DLName);
  };

  return 1;
}

void gl_error() {
  // fprintf(stderr, "OpenGL Extension not available \r\n");
  throw "undef_extension";
}

/* *******************************************************************************
 * GLU Tesselation special
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
  // fprintf(stderr, "Tesselation error: %d: %s\r\n", (int) errorCode, err);
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

int erl_tess_impl(char* buff, ErlDrvPort port, ErlDrvTermData caller)
{
  ErlDrvBinary* bin;
  int i;
  int num_vertices;
  GLdouble *n;
  int AP;
  int a_max = 2;
  int i_max = 6;
  num_vertices = * (int *) buff; buff += 8; /* Align */
  n = (double *) buff; buff += 8*3;

  egl_tess.alloc_max = a_max*num_vertices*3;
  bin = driver_alloc_binary(egl_tess.alloc_max*sizeof(GLdouble));
  egl_tess.error = 0;
  egl_tess.tess_coords = (double *) bin->orig_bytes;
  memcpy(egl_tess.tess_coords,buff,num_vertices*3*sizeof(GLdouble));
  egl_tess.index_max = i_max*3*num_vertices;
  egl_tess.tess_index_list = (int *) driver_alloc(sizeof(int) * egl_tess.index_max);

  egl_tess.tess_coords = (double *) bin->orig_bytes;
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

  AP = 0; ErlDrvTermData *rt;
  rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData) * (13+egl_tess.index_n*2));
  rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_egl_result_");

  for(i=0; i < egl_tess.index_n; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (int) egl_tess.tess_index_list[i];
  };
  rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = egl_tess.index_n+1;

  rt[AP++] = ERL_DRV_BINARY; rt[AP++] = (ErlDrvTermData) bin;
  rt[AP++] = egl_tess.alloc_n*sizeof(GLdouble); rt[AP++] = 0;

  rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2; // Return tuple {list, Bin}
  rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2; // Result tuple

  driver_send_term(port,caller,rt,AP);
  /* fprintf(stderr, "List %d: %d %d %d \r\n",  */
  /* 	  res, */
  /* 	  n_pos,  */
  /* 	  (tess_alloc_vertex-new_vertices)*sizeof(GLdouble),  */
  /* 	  num_vertices*6*sizeof(GLdouble)); */
  driver_free_binary(bin);
  driver_free(egl_tess.tess_index_list);
  driver_free(rt);
  return 0;
}
