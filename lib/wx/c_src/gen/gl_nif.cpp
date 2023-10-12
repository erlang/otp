/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2023. All Rights Reserved.
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
/***** This file is generated do not edit ****/

#include <stdio.h>
#include <string.h>
#include <vector>
extern "C" {
 #include "../egl_impl.h"
 #include "gl_fdefs.h"
}

void ecb_init_opengl(ErlNifEnv *env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
    egl_load_functions();
    init_tess();
}

void ecb_glClearIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat c;
  if(!egl_get_float(env, argv[0],  &c)) Badarg(5037,"c");
  weglClearIndex(c);
}

void ecb_glClearColor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf red;
  GLclampf green;
  GLclampf blue;
  GLclampf alpha;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5038,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5038,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5038,"blue");
  if(!egl_get_float(env, argv[3],  &alpha)) Badarg(5038,"alpha");
  weglClearColor(red,green,blue,alpha);
}

void ecb_glClear(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5039,"mask");
  weglClear(mask);
}

void ecb_glIndexMask(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5040,"mask");
  weglIndexMask(mask);
}

void ecb_glColorMask(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean red;
  GLboolean green;
  GLboolean blue;
  GLboolean alpha;
  if(!egl_get_ubyte(env, argv[0],  &red)) Badarg(5041,"red");
  if(!egl_get_ubyte(env, argv[1],  &green)) Badarg(5041,"green");
  if(!egl_get_ubyte(env, argv[2],  &blue)) Badarg(5041,"blue");
  if(!egl_get_ubyte(env, argv[3],  &alpha)) Badarg(5041,"alpha");
  weglColorMask(red,green,blue,alpha);
}

void ecb_glAlphaFunc(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum func;
  GLclampf ref;
  if(!enif_get_uint(env, argv[0],  &func)) Badarg(5042,"func");
  if(!egl_get_float(env, argv[1],  &ref)) Badarg(5042,"ref");
  weglAlphaFunc(func,ref);
}

void ecb_glBlendFunc(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum sfactor;
  GLenum dfactor;
  if(!enif_get_uint(env, argv[0],  &sfactor)) Badarg(5043,"sfactor");
  if(!enif_get_uint(env, argv[1],  &dfactor)) Badarg(5043,"dfactor");
  weglBlendFunc(sfactor,dfactor);
}

void ecb_glLogicOp(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum opcode;
  if(!enif_get_uint(env, argv[0],  &opcode)) Badarg(5044,"opcode");
  weglLogicOp(opcode);
}

void ecb_glCullFace(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5045,"mode");
  weglCullFace(mode);
}

void ecb_glFrontFace(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5046,"mode");
  weglFrontFace(mode);
}

void ecb_glPointSize(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat size;
  if(!egl_get_float(env, argv[0],  &size)) Badarg(5047,"size");
  weglPointSize(size);
}

void ecb_glLineWidth(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat width;
  if(!egl_get_float(env, argv[0],  &width)) Badarg(5048,"width");
  weglLineWidth(width);
}

void ecb_glLineStipple(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint factor;
  GLushort pattern;
  if(!enif_get_int(env, argv[0],  &factor)) Badarg(5049,"factor");
  if(!egl_get_ushort(env, argv[1],  &pattern)) Badarg(5049,"pattern");
  weglLineStipple(factor,pattern);
}

void ecb_glPolygonMode(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5050,"face");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5050,"mode");
  weglPolygonMode(face,mode);
}

void ecb_glPolygonOffset(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat factor;
  GLfloat units;
  if(!egl_get_float(env, argv[0],  &factor)) Badarg(5051,"factor");
  if(!egl_get_float(env, argv[1],  &units)) Badarg(5051,"units");
  weglPolygonOffset(factor,units);
}

void ecb_glPolygonStipple(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ErlNifBinary mask;
  if(!enif_inspect_binary(env, argv[0], &mask)) Badarg(5052,"mask");
  weglPolygonStipple((GLubyte *) mask.data);
}

void ecb_glGetPolygonStipple(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  ErlNifBinary mask;
  enif_alloc_binary((int) 128*sizeof(GLubyte), &mask);
  weglGetPolygonStipple((GLubyte *) mask.data);
  reply =      enif_make_binary(env, &mask);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glEdgeFlag(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean flag;
  if(!egl_get_ubyte(env, argv[0],  &flag)) Badarg(5054,"flag");
  weglEdgeFlag(flag);
}

void ecb_glScissor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5055,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5055,"y");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5055,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5055,"height");
  weglScissor(x,y,width,height);
}

void ecb_glClipPlane(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum plane;
  GLdouble equation[4];
  if(!enif_get_uint(env, argv[0],  &plane)) Badarg(5056,"plane");
  {
   int equation_a;
   const ERL_NIF_TERM *equation_t;
   if(!enif_get_tuple(env, argv[1], &equation_a, &equation_t) || equation_a != 4) {
     Badarg(5056,"equation");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, equation_t[i1++], &equation[0])) Badarg(5056,"equation");
     if(!enif_get_double(env, equation_t[i1++], &equation[1])) Badarg(5056,"equation");
     if(!enif_get_double(env, equation_t[i1++], &equation[2])) Badarg(5056,"equation");
     if(!enif_get_double(env, equation_t[i1++], &equation[3])) Badarg(5056,"equation");
   }};
  weglClipPlane(plane,equation);
}

void ecb_glGetClipPlane(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum plane;
  GLdouble equation[4];
  if(!enif_get_uint(env, argv[0],  &plane)) Badarg(5057,"plane");
  weglGetClipPlane(plane,equation);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, equation[0]),
            enif_make_double(env, equation[1]),
            enif_make_double(env, equation[2]),
            enif_make_double(env, equation[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDrawBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5058,"mode");
  weglDrawBuffer(mode);
}

void ecb_glReadBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5059,"mode");
  weglReadBuffer(mode);
}

void ecb_glEnable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5060,"cap");
  weglEnable(cap);
}

void ecb_glDisable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5061,"cap");
  weglDisable(cap);
}

void ecb_glIsEnabled(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5062,"cap");
  result = weglIsEnabled(cap);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glEnableClientState(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5063,"cap");
  weglEnableClientState(cap);
}

void ecb_glDisableClientState(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5064,"cap");
  weglDisableClientState(cap);
}

void ecb_glGetBooleanv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLboolean params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5065,"pname");
  weglGetBooleanv(pname,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetDoublev(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLdouble params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5066,"pname");
  weglGetDoublev(pname,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_double(env, params[ri]);
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetFloatv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLfloat params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5067,"pname");
  weglGetFloatv(pname,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_double(env, (double) params[ri]);
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetIntegerv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5068,"pname");
  weglGetIntegerv(pname,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glPushAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5069,"mask");
  weglPushAttrib(mask);
}

void ecb_glPopAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopAttrib();
}

void ecb_glPushClientAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5071,"mask");
  weglPushClientAttrib(mask);
}

void ecb_glPopClientAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopClientAttrib();
}

void ecb_glRenderMode(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5073,"mode");
  result = weglRenderMode(mode);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetError(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum result;
  ERL_NIF_TERM reply;
  result = weglGetError();
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetString(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  const GLubyte *  result;
  ERL_NIF_TERM reply;
  GLenum name;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5075,"name");
  result = weglGetString(name);
  reply =      enif_make_string(env, (const char *) result, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glFinish(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglFinish();
}

void ecb_glFlush(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglFlush();
}

void ecb_glHint(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5078,"target");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5078,"mode");
  weglHint(target,mode);
}

void ecb_glClearDepth(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampd depth;
  if(!enif_get_double(env, argv[0],  &depth)) Badarg(5079,"depth");
  weglClearDepth(depth);
}

void ecb_glDepthFunc(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum func;
  if(!enif_get_uint(env, argv[0],  &func)) Badarg(5080,"func");
  weglDepthFunc(func);
}

void ecb_glDepthMask(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean flag;
  if(!egl_get_ubyte(env, argv[0],  &flag)) Badarg(5081,"flag");
  weglDepthMask(flag);
}

void ecb_glDepthRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampd near_val;
  GLclampd far_val;
  if(!enif_get_double(env, argv[0],  &near_val)) Badarg(5082,"near_val");
  if(!enif_get_double(env, argv[1],  &far_val)) Badarg(5082,"far_val");
  weglDepthRange(near_val,far_val);
}

void ecb_glClearAccum(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  GLfloat alpha;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5083,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5083,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5083,"blue");
  if(!egl_get_float(env, argv[3],  &alpha)) Badarg(5083,"alpha");
  weglClearAccum(red,green,blue,alpha);
}

void ecb_glAccum(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum op;
  GLfloat value;
  if(!enif_get_uint(env, argv[0],  &op)) Badarg(5084,"op");
  if(!egl_get_float(env, argv[1],  &value)) Badarg(5084,"value");
  weglAccum(op,value);
}

void ecb_glMatrixMode(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5085,"mode");
  weglMatrixMode(mode);
}

void ecb_glOrtho(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble left;
  GLdouble right;
  GLdouble bottom;
  GLdouble top;
  GLdouble near_val;
  GLdouble far_val;
  if(!enif_get_double(env, argv[0],  &left)) Badarg(5086,"left");
  if(!enif_get_double(env, argv[1],  &right)) Badarg(5086,"right");
  if(!enif_get_double(env, argv[2],  &bottom)) Badarg(5086,"bottom");
  if(!enif_get_double(env, argv[3],  &top)) Badarg(5086,"top");
  if(!enif_get_double(env, argv[4],  &near_val)) Badarg(5086,"near_val");
  if(!enif_get_double(env, argv[5],  &far_val)) Badarg(5086,"far_val");
  weglOrtho(left,right,bottom,top,near_val,far_val);
}

void ecb_glFrustum(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble left;
  GLdouble right;
  GLdouble bottom;
  GLdouble top;
  GLdouble near_val;
  GLdouble far_val;
  if(!enif_get_double(env, argv[0],  &left)) Badarg(5087,"left");
  if(!enif_get_double(env, argv[1],  &right)) Badarg(5087,"right");
  if(!enif_get_double(env, argv[2],  &bottom)) Badarg(5087,"bottom");
  if(!enif_get_double(env, argv[3],  &top)) Badarg(5087,"top");
  if(!enif_get_double(env, argv[4],  &near_val)) Badarg(5087,"near_val");
  if(!enif_get_double(env, argv[5],  &far_val)) Badarg(5087,"far_val");
  weglFrustum(left,right,bottom,top,near_val,far_val);
}

void ecb_glViewport(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5088,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5088,"y");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5088,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5088,"height");
  weglViewport(x,y,width,height);
}

void ecb_glPushMatrix(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPushMatrix();
}

void ecb_glPopMatrix(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopMatrix();
}

void ecb_glLoadIdentity(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglLoadIdentity();
}

void ecb_glLoadMatrixd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5092,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5092,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5092,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5092,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5092,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5092,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadMatrixd(m);
}

void ecb_glLoadMatrixf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5093,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5093,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5093,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5093,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5093,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5093,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadMatrixf(m);
}

void ecb_glMultMatrixd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5094,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5094,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5094,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5094,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5094,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5094,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultMatrixd(m);
}

void ecb_glMultMatrixf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5095,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5095,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5095,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5095,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5095,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5095,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultMatrixf(m);
}

void ecb_glRotated(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble angle;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &angle)) Badarg(5096,"angle");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5096,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5096,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5096,"z");
  weglRotated(angle,x,y,z);
}

void ecb_glRotatef(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat angle;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &angle)) Badarg(5097,"angle");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5097,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5097,"y");
  if(!egl_get_float(env, argv[3],  &z)) Badarg(5097,"z");
  weglRotatef(angle,x,y,z);
}

void ecb_glScaled(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5098,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5098,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5098,"z");
  weglScaled(x,y,z);
}

void ecb_glScalef(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5099,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5099,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5099,"z");
  weglScalef(x,y,z);
}

void ecb_glTranslated(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5100,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5100,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5100,"z");
  weglTranslated(x,y,z);
}

void ecb_glTranslatef(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5101,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5101,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5101,"z");
  weglTranslatef(x,y,z);
}

void ecb_glIsList(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint list;
  if(!enif_get_uint(env, argv[0],  &list)) Badarg(5102,"list");
  result = weglIsList(list);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDeleteLists(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint list;
  GLsizei range;
  if(!enif_get_uint(env, argv[0],  &list)) Badarg(5103,"list");
  if(!enif_get_int(env, argv[1],  &range)) Badarg(5103,"range");
  weglDeleteLists(list,range);
}

void ecb_glGenLists(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLsizei range;
  if(!enif_get_int(env, argv[0],  &range)) Badarg(5104,"range");
  result = weglGenLists(range);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glNewList(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint list;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &list)) Badarg(5105,"list");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5105,"mode");
  weglNewList(list,mode);
}

void ecb_glEndList(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEndList();
}

void ecb_glCallList(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint list;
  if(!enif_get_uint(env, argv[0],  &list)) Badarg(5107,"list");
  weglCallList(list);
}

void ecb_glCallLists(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *lists;
  std::vector <GLuint> lists_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5108,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5108, "lists")
  else {
    ERL_NIF_TERM lists_l, lists_h, lists_t;
    GLuint lists_tmp;
    lists_l = argv[1];
    while(enif_get_list_cell(env, lists_l, &lists_h, &lists_t)) {
        if(!enif_get_uint(env, lists_h, &lists_tmp)) Badarg(5108,"lists");
        lists_vec.push_back(lists_tmp);
        lists_l = lists_t;
    };
    lists = lists_vec.data();
  }
  weglCallLists(n,GL_UNSIGNED_INT,lists);
}

void ecb_glListBase(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint base;
  if(!enif_get_uint(env, argv[0],  &base)) Badarg(5109,"base");
  weglListBase(base);
}

void ecb_glBegin(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5110,"mode");
  weglBegin(mode);
}

void ecb_glEnd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEnd();
}

void ecb_glVertex2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5112,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5112,"y");
  weglVertex2d(x,y);
}

void ecb_glVertex2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5113,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5113,"y");
  weglVertex2f(x,y);
}

void ecb_glVertex2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5114,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5114,"y");
  weglVertex2i(x,y);
}

void ecb_glVertex2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5115,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5115,"y");
  weglVertex2s(x,y);
}

void ecb_glVertex3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5116,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5116,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5116,"z");
  weglVertex3d(x,y,z);
}

void ecb_glVertex3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5117,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5117,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5117,"z");
  weglVertex3f(x,y,z);
}

void ecb_glVertex3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5118,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5118,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5118,"z");
  weglVertex3i(x,y,z);
}

void ecb_glVertex3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5119,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5119,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5119,"z");
  weglVertex3s(x,y,z);
}

void ecb_glVertex4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5120,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5120,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5120,"z");
  if(!enif_get_double(env, argv[3],  &w)) Badarg(5120,"w");
  weglVertex4d(x,y,z,w);
}

void ecb_glVertex4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5121,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5121,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5121,"z");
  if(!egl_get_float(env, argv[3],  &w)) Badarg(5121,"w");
  weglVertex4f(x,y,z,w);
}

void ecb_glVertex4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  GLint w;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5122,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5122,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5122,"z");
  if(!enif_get_int(env, argv[3],  &w)) Badarg(5122,"w");
  weglVertex4i(x,y,z,w);
}

void ecb_glVertex4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  GLshort w;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5123,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5123,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5123,"z");
  if(!egl_get_short(env, argv[3],  &w)) Badarg(5123,"w");
  weglVertex4s(x,y,z,w);
}

void ecb_glNormal3b(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbyte nx;
  GLbyte ny;
  GLbyte nz;
  if(!egl_get_byte(env, argv[0],  &nx)) Badarg(5124,"nx");
  if(!egl_get_byte(env, argv[1],  &ny)) Badarg(5124,"ny");
  if(!egl_get_byte(env, argv[2],  &nz)) Badarg(5124,"nz");
  weglNormal3b(nx,ny,nz);
}

void ecb_glNormal3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble nx;
  GLdouble ny;
  GLdouble nz;
  if(!enif_get_double(env, argv[0],  &nx)) Badarg(5125,"nx");
  if(!enif_get_double(env, argv[1],  &ny)) Badarg(5125,"ny");
  if(!enif_get_double(env, argv[2],  &nz)) Badarg(5125,"nz");
  weglNormal3d(nx,ny,nz);
}

void ecb_glNormal3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat nx;
  GLfloat ny;
  GLfloat nz;
  if(!egl_get_float(env, argv[0],  &nx)) Badarg(5126,"nx");
  if(!egl_get_float(env, argv[1],  &ny)) Badarg(5126,"ny");
  if(!egl_get_float(env, argv[2],  &nz)) Badarg(5126,"nz");
  weglNormal3f(nx,ny,nz);
}

void ecb_glNormal3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint nx;
  GLint ny;
  GLint nz;
  if(!enif_get_int(env, argv[0],  &nx)) Badarg(5127,"nx");
  if(!enif_get_int(env, argv[1],  &ny)) Badarg(5127,"ny");
  if(!enif_get_int(env, argv[2],  &nz)) Badarg(5127,"nz");
  weglNormal3i(nx,ny,nz);
}

void ecb_glNormal3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort nx;
  GLshort ny;
  GLshort nz;
  if(!egl_get_short(env, argv[0],  &nx)) Badarg(5128,"nx");
  if(!egl_get_short(env, argv[1],  &ny)) Badarg(5128,"ny");
  if(!egl_get_short(env, argv[2],  &nz)) Badarg(5128,"nz");
  weglNormal3s(nx,ny,nz);
}

void ecb_glIndexd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble c;
  if(!enif_get_double(env, argv[0],  &c)) Badarg(5129,"c");
  weglIndexd(c);
}

void ecb_glIndexf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat c;
  if(!egl_get_float(env, argv[0],  &c)) Badarg(5130,"c");
  weglIndexf(c);
}

void ecb_glIndexi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint c;
  if(!enif_get_int(env, argv[0],  &c)) Badarg(5131,"c");
  weglIndexi(c);
}

void ecb_glIndexs(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort c;
  if(!egl_get_short(env, argv[0],  &c)) Badarg(5132,"c");
  weglIndexs(c);
}

void ecb_glIndexub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLubyte c;
  if(!egl_get_ubyte(env, argv[0],  &c)) Badarg(5133,"c");
  weglIndexub(c);
}

void ecb_glColor3b(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbyte red;
  GLbyte green;
  GLbyte blue;
  if(!egl_get_byte(env, argv[0],  &red)) Badarg(5134,"red");
  if(!egl_get_byte(env, argv[1],  &green)) Badarg(5134,"green");
  if(!egl_get_byte(env, argv[2],  &blue)) Badarg(5134,"blue");
  weglColor3b(red,green,blue);
}

void ecb_glColor3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble red;
  GLdouble green;
  GLdouble blue;
  if(!enif_get_double(env, argv[0],  &red)) Badarg(5135,"red");
  if(!enif_get_double(env, argv[1],  &green)) Badarg(5135,"green");
  if(!enif_get_double(env, argv[2],  &blue)) Badarg(5135,"blue");
  weglColor3d(red,green,blue);
}

void ecb_glColor3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5136,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5136,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5136,"blue");
  weglColor3f(red,green,blue);
}

void ecb_glColor3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint red;
  GLint green;
  GLint blue;
  if(!enif_get_int(env, argv[0],  &red)) Badarg(5137,"red");
  if(!enif_get_int(env, argv[1],  &green)) Badarg(5137,"green");
  if(!enif_get_int(env, argv[2],  &blue)) Badarg(5137,"blue");
  weglColor3i(red,green,blue);
}

void ecb_glColor3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort red;
  GLshort green;
  GLshort blue;
  if(!egl_get_short(env, argv[0],  &red)) Badarg(5138,"red");
  if(!egl_get_short(env, argv[1],  &green)) Badarg(5138,"green");
  if(!egl_get_short(env, argv[2],  &blue)) Badarg(5138,"blue");
  weglColor3s(red,green,blue);
}

void ecb_glColor3ub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLubyte red;
  GLubyte green;
  GLubyte blue;
  if(!egl_get_ubyte(env, argv[0],  &red)) Badarg(5139,"red");
  if(!egl_get_ubyte(env, argv[1],  &green)) Badarg(5139,"green");
  if(!egl_get_ubyte(env, argv[2],  &blue)) Badarg(5139,"blue");
  weglColor3ub(red,green,blue);
}

void ecb_glColor3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint red;
  GLuint green;
  GLuint blue;
  if(!enif_get_uint(env, argv[0],  &red)) Badarg(5140,"red");
  if(!enif_get_uint(env, argv[1],  &green)) Badarg(5140,"green");
  if(!enif_get_uint(env, argv[2],  &blue)) Badarg(5140,"blue");
  weglColor3ui(red,green,blue);
}

void ecb_glColor3us(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLushort red;
  GLushort green;
  GLushort blue;
  if(!egl_get_ushort(env, argv[0],  &red)) Badarg(5141,"red");
  if(!egl_get_ushort(env, argv[1],  &green)) Badarg(5141,"green");
  if(!egl_get_ushort(env, argv[2],  &blue)) Badarg(5141,"blue");
  weglColor3us(red,green,blue);
}

void ecb_glColor4b(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbyte red;
  GLbyte green;
  GLbyte blue;
  GLbyte alpha;
  if(!egl_get_byte(env, argv[0],  &red)) Badarg(5142,"red");
  if(!egl_get_byte(env, argv[1],  &green)) Badarg(5142,"green");
  if(!egl_get_byte(env, argv[2],  &blue)) Badarg(5142,"blue");
  if(!egl_get_byte(env, argv[3],  &alpha)) Badarg(5142,"alpha");
  weglColor4b(red,green,blue,alpha);
}

void ecb_glColor4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble red;
  GLdouble green;
  GLdouble blue;
  GLdouble alpha;
  if(!enif_get_double(env, argv[0],  &red)) Badarg(5143,"red");
  if(!enif_get_double(env, argv[1],  &green)) Badarg(5143,"green");
  if(!enif_get_double(env, argv[2],  &blue)) Badarg(5143,"blue");
  if(!enif_get_double(env, argv[3],  &alpha)) Badarg(5143,"alpha");
  weglColor4d(red,green,blue,alpha);
}

void ecb_glColor4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  GLfloat alpha;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5144,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5144,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5144,"blue");
  if(!egl_get_float(env, argv[3],  &alpha)) Badarg(5144,"alpha");
  weglColor4f(red,green,blue,alpha);
}

void ecb_glColor4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint red;
  GLint green;
  GLint blue;
  GLint alpha;
  if(!enif_get_int(env, argv[0],  &red)) Badarg(5145,"red");
  if(!enif_get_int(env, argv[1],  &green)) Badarg(5145,"green");
  if(!enif_get_int(env, argv[2],  &blue)) Badarg(5145,"blue");
  if(!enif_get_int(env, argv[3],  &alpha)) Badarg(5145,"alpha");
  weglColor4i(red,green,blue,alpha);
}

void ecb_glColor4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort red;
  GLshort green;
  GLshort blue;
  GLshort alpha;
  if(!egl_get_short(env, argv[0],  &red)) Badarg(5146,"red");
  if(!egl_get_short(env, argv[1],  &green)) Badarg(5146,"green");
  if(!egl_get_short(env, argv[2],  &blue)) Badarg(5146,"blue");
  if(!egl_get_short(env, argv[3],  &alpha)) Badarg(5146,"alpha");
  weglColor4s(red,green,blue,alpha);
}

void ecb_glColor4ub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLubyte red;
  GLubyte green;
  GLubyte blue;
  GLubyte alpha;
  if(!egl_get_ubyte(env, argv[0],  &red)) Badarg(5147,"red");
  if(!egl_get_ubyte(env, argv[1],  &green)) Badarg(5147,"green");
  if(!egl_get_ubyte(env, argv[2],  &blue)) Badarg(5147,"blue");
  if(!egl_get_ubyte(env, argv[3],  &alpha)) Badarg(5147,"alpha");
  weglColor4ub(red,green,blue,alpha);
}

void ecb_glColor4ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint red;
  GLuint green;
  GLuint blue;
  GLuint alpha;
  if(!enif_get_uint(env, argv[0],  &red)) Badarg(5148,"red");
  if(!enif_get_uint(env, argv[1],  &green)) Badarg(5148,"green");
  if(!enif_get_uint(env, argv[2],  &blue)) Badarg(5148,"blue");
  if(!enif_get_uint(env, argv[3],  &alpha)) Badarg(5148,"alpha");
  weglColor4ui(red,green,blue,alpha);
}

void ecb_glColor4us(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLushort red;
  GLushort green;
  GLushort blue;
  GLushort alpha;
  if(!egl_get_ushort(env, argv[0],  &red)) Badarg(5149,"red");
  if(!egl_get_ushort(env, argv[1],  &green)) Badarg(5149,"green");
  if(!egl_get_ushort(env, argv[2],  &blue)) Badarg(5149,"blue");
  if(!egl_get_ushort(env, argv[3],  &alpha)) Badarg(5149,"alpha");
  weglColor4us(red,green,blue,alpha);
}

void ecb_glTexCoord1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble s;
  if(!enif_get_double(env, argv[0],  &s)) Badarg(5150,"s");
  weglTexCoord1d(s);
}

void ecb_glTexCoord1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat s;
  if(!egl_get_float(env, argv[0],  &s)) Badarg(5151,"s");
  weglTexCoord1f(s);
}

void ecb_glTexCoord1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5152,"s");
  weglTexCoord1i(s);
}

void ecb_glTexCoord1s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort s;
  if(!egl_get_short(env, argv[0],  &s)) Badarg(5153,"s");
  weglTexCoord1s(s);
}

void ecb_glTexCoord2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble s;
  GLdouble t;
  if(!enif_get_double(env, argv[0],  &s)) Badarg(5154,"s");
  if(!enif_get_double(env, argv[1],  &t)) Badarg(5154,"t");
  weglTexCoord2d(s,t);
}

void ecb_glTexCoord2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat s;
  GLfloat t;
  if(!egl_get_float(env, argv[0],  &s)) Badarg(5155,"s");
  if(!egl_get_float(env, argv[1],  &t)) Badarg(5155,"t");
  weglTexCoord2f(s,t);
}

void ecb_glTexCoord2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  GLint t;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5156,"s");
  if(!enif_get_int(env, argv[1],  &t)) Badarg(5156,"t");
  weglTexCoord2i(s,t);
}

void ecb_glTexCoord2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort s;
  GLshort t;
  if(!egl_get_short(env, argv[0],  &s)) Badarg(5157,"s");
  if(!egl_get_short(env, argv[1],  &t)) Badarg(5157,"t");
  weglTexCoord2s(s,t);
}

void ecb_glTexCoord3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble s;
  GLdouble t;
  GLdouble r;
  if(!enif_get_double(env, argv[0],  &s)) Badarg(5158,"s");
  if(!enif_get_double(env, argv[1],  &t)) Badarg(5158,"t");
  if(!enif_get_double(env, argv[2],  &r)) Badarg(5158,"r");
  weglTexCoord3d(s,t,r);
}

void ecb_glTexCoord3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat s;
  GLfloat t;
  GLfloat r;
  if(!egl_get_float(env, argv[0],  &s)) Badarg(5159,"s");
  if(!egl_get_float(env, argv[1],  &t)) Badarg(5159,"t");
  if(!egl_get_float(env, argv[2],  &r)) Badarg(5159,"r");
  weglTexCoord3f(s,t,r);
}

void ecb_glTexCoord3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  GLint t;
  GLint r;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5160,"s");
  if(!enif_get_int(env, argv[1],  &t)) Badarg(5160,"t");
  if(!enif_get_int(env, argv[2],  &r)) Badarg(5160,"r");
  weglTexCoord3i(s,t,r);
}

void ecb_glTexCoord3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort s;
  GLshort t;
  GLshort r;
  if(!egl_get_short(env, argv[0],  &s)) Badarg(5161,"s");
  if(!egl_get_short(env, argv[1],  &t)) Badarg(5161,"t");
  if(!egl_get_short(env, argv[2],  &r)) Badarg(5161,"r");
  weglTexCoord3s(s,t,r);
}

void ecb_glTexCoord4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble s;
  GLdouble t;
  GLdouble r;
  GLdouble q;
  if(!enif_get_double(env, argv[0],  &s)) Badarg(5162,"s");
  if(!enif_get_double(env, argv[1],  &t)) Badarg(5162,"t");
  if(!enif_get_double(env, argv[2],  &r)) Badarg(5162,"r");
  if(!enif_get_double(env, argv[3],  &q)) Badarg(5162,"q");
  weglTexCoord4d(s,t,r,q);
}

void ecb_glTexCoord4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat s;
  GLfloat t;
  GLfloat r;
  GLfloat q;
  if(!egl_get_float(env, argv[0],  &s)) Badarg(5163,"s");
  if(!egl_get_float(env, argv[1],  &t)) Badarg(5163,"t");
  if(!egl_get_float(env, argv[2],  &r)) Badarg(5163,"r");
  if(!egl_get_float(env, argv[3],  &q)) Badarg(5163,"q");
  weglTexCoord4f(s,t,r,q);
}

void ecb_glTexCoord4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  GLint t;
  GLint r;
  GLint q;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5164,"s");
  if(!enif_get_int(env, argv[1],  &t)) Badarg(5164,"t");
  if(!enif_get_int(env, argv[2],  &r)) Badarg(5164,"r");
  if(!enif_get_int(env, argv[3],  &q)) Badarg(5164,"q");
  weglTexCoord4i(s,t,r,q);
}

void ecb_glTexCoord4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort s;
  GLshort t;
  GLshort r;
  GLshort q;
  if(!egl_get_short(env, argv[0],  &s)) Badarg(5165,"s");
  if(!egl_get_short(env, argv[1],  &t)) Badarg(5165,"t");
  if(!egl_get_short(env, argv[2],  &r)) Badarg(5165,"r");
  if(!egl_get_short(env, argv[3],  &q)) Badarg(5165,"q");
  weglTexCoord4s(s,t,r,q);
}

void ecb_glRasterPos2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5166,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5166,"y");
  weglRasterPos2d(x,y);
}

void ecb_glRasterPos2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5167,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5167,"y");
  weglRasterPos2f(x,y);
}

void ecb_glRasterPos2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5168,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5168,"y");
  weglRasterPos2i(x,y);
}

void ecb_glRasterPos2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5169,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5169,"y");
  weglRasterPos2s(x,y);
}

void ecb_glRasterPos3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5170,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5170,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5170,"z");
  weglRasterPos3d(x,y,z);
}

void ecb_glRasterPos3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5171,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5171,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5171,"z");
  weglRasterPos3f(x,y,z);
}

void ecb_glRasterPos3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5172,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5172,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5172,"z");
  weglRasterPos3i(x,y,z);
}

void ecb_glRasterPos3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5173,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5173,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5173,"z");
  weglRasterPos3s(x,y,z);
}

void ecb_glRasterPos4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5174,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5174,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5174,"z");
  if(!enif_get_double(env, argv[3],  &w)) Badarg(5174,"w");
  weglRasterPos4d(x,y,z,w);
}

void ecb_glRasterPos4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5175,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5175,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5175,"z");
  if(!egl_get_float(env, argv[3],  &w)) Badarg(5175,"w");
  weglRasterPos4f(x,y,z,w);
}

void ecb_glRasterPos4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  GLint w;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5176,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5176,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5176,"z");
  if(!enif_get_int(env, argv[3],  &w)) Badarg(5176,"w");
  weglRasterPos4i(x,y,z,w);
}

void ecb_glRasterPos4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  GLshort w;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5177,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5177,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5177,"z");
  if(!egl_get_short(env, argv[3],  &w)) Badarg(5177,"w");
  weglRasterPos4s(x,y,z,w);
}

void ecb_glRectd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x1;
  GLdouble y1;
  GLdouble x2;
  GLdouble y2;
  if(!enif_get_double(env, argv[0],  &x1)) Badarg(5178,"x1");
  if(!enif_get_double(env, argv[1],  &y1)) Badarg(5178,"y1");
  if(!enif_get_double(env, argv[2],  &x2)) Badarg(5178,"x2");
  if(!enif_get_double(env, argv[3],  &y2)) Badarg(5178,"y2");
  weglRectd(x1,y1,x2,y2);
}

void ecb_glRectf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x1;
  GLfloat y1;
  GLfloat x2;
  GLfloat y2;
  if(!egl_get_float(env, argv[0],  &x1)) Badarg(5179,"x1");
  if(!egl_get_float(env, argv[1],  &y1)) Badarg(5179,"y1");
  if(!egl_get_float(env, argv[2],  &x2)) Badarg(5179,"x2");
  if(!egl_get_float(env, argv[3],  &y2)) Badarg(5179,"y2");
  weglRectf(x1,y1,x2,y2);
}

void ecb_glRecti(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x1;
  GLint y1;
  GLint x2;
  GLint y2;
  if(!enif_get_int(env, argv[0],  &x1)) Badarg(5180,"x1");
  if(!enif_get_int(env, argv[1],  &y1)) Badarg(5180,"y1");
  if(!enif_get_int(env, argv[2],  &x2)) Badarg(5180,"x2");
  if(!enif_get_int(env, argv[3],  &y2)) Badarg(5180,"y2");
  weglRecti(x1,y1,x2,y2);
}

void ecb_glRects(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x1;
  GLshort y1;
  GLshort x2;
  GLshort y2;
  if(!egl_get_short(env, argv[0],  &x1)) Badarg(5181,"x1");
  if(!egl_get_short(env, argv[1],  &y1)) Badarg(5181,"y1");
  if(!egl_get_short(env, argv[2],  &x2)) Badarg(5181,"x2");
  if(!egl_get_short(env, argv[3],  &y2)) Badarg(5181,"y2");
  weglRects(x1,y1,x2,y2);
}

void ecb_glRectdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble v1[2];
  GLdouble v2[2];
  {
   int v1_a;
   const ERL_NIF_TERM *v1_t;
   if(!enif_get_tuple(env, argv[0], &v1_a, &v1_t) || v1_a != 2) {
     Badarg(5182,"v1");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, v1_t[i1++], &v1[0])) Badarg(5182,"v1");
     if(!enif_get_double(env, v1_t[i1++], &v1[1])) Badarg(5182,"v1");
   }};
  {
   int v2_a;
   const ERL_NIF_TERM *v2_t;
   if(!enif_get_tuple(env, argv[1], &v2_a, &v2_t) || v2_a != 2) {
     Badarg(5182,"v2");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, v2_t[i1++], &v2[0])) Badarg(5182,"v2");
     if(!enif_get_double(env, v2_t[i1++], &v2[1])) Badarg(5182,"v2");
   }};
  weglRectdv(v1,v2);
}

void ecb_glRectfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat v1[2];
  GLfloat v2[2];
  {
   int v1_a;
   const ERL_NIF_TERM *v1_t;
   if(!enif_get_tuple(env, argv[0], &v1_a, &v1_t) || v1_a != 2) {
     Badarg(5183,"v1");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, v1_t[i1++], &v1[0])) Badarg(5183,"v1");
     if(!egl_get_float(env, v1_t[i1++], &v1[1])) Badarg(5183,"v1");
   }};
  {
   int v2_a;
   const ERL_NIF_TERM *v2_t;
   if(!enif_get_tuple(env, argv[1], &v2_a, &v2_t) || v2_a != 2) {
     Badarg(5183,"v2");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, v2_t[i1++], &v2[0])) Badarg(5183,"v2");
     if(!egl_get_float(env, v2_t[i1++], &v2[1])) Badarg(5183,"v2");
   }};
  weglRectfv(v1,v2);
}

void ecb_glRectiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint v1[2];
  GLint v2[2];
  {
   int v1_a;
   const ERL_NIF_TERM *v1_t;
   if(!enif_get_tuple(env, argv[0], &v1_a, &v1_t) || v1_a != 2) {
     Badarg(5184,"v1");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v1_t[i1++], &v1[0])) Badarg(5184,"v1");
     if(!enif_get_int(env, v1_t[i1++], &v1[1])) Badarg(5184,"v1");
   }};
  {
   int v2_a;
   const ERL_NIF_TERM *v2_t;
   if(!enif_get_tuple(env, argv[1], &v2_a, &v2_t) || v2_a != 2) {
     Badarg(5184,"v2");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v2_t[i1++], &v2[0])) Badarg(5184,"v2");
     if(!enif_get_int(env, v2_t[i1++], &v2[1])) Badarg(5184,"v2");
   }};
  weglRectiv(v1,v2);
}

void ecb_glRectsv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort v1[2];
  GLshort v2[2];
  {
   int v1_a;
   const ERL_NIF_TERM *v1_t;
   if(!enif_get_tuple(env, argv[0], &v1_a, &v1_t) || v1_a != 2) {
     Badarg(5185,"v1");
   } else {
    int i1 = 0;
     if(!egl_get_short(env, v1_t[i1++], &v1[0])) Badarg(5185,"v1");
     if(!egl_get_short(env, v1_t[i1++], &v1[1])) Badarg(5185,"v1");
   }};
  {
   int v2_a;
   const ERL_NIF_TERM *v2_t;
   if(!enif_get_tuple(env, argv[1], &v2_a, &v2_t) || v2_a != 2) {
     Badarg(5185,"v2");
   } else {
    int i1 = 0;
     if(!egl_get_short(env, v2_t[i1++], &v2[0])) Badarg(5185,"v2");
     if(!egl_get_short(env, v2_t[i1++], &v2[1])) Badarg(5185,"v2");
   }};
  weglRectsv(v1,v2);
}

void ecb_glVertexPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5186,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5186,"type");
  if(!enif_get_int(env, argv[2],  &stride)) Badarg(5186,"stride");
  if(!egl_get_ptr(env, argv[3], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5186,"ptr");
  }
  weglVertexPointer(size,type,stride,ptr_idx);
}

void ecb_glNormalPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5188,"type");
  if(!enif_get_int(env, argv[1],  &stride)) Badarg(5188,"stride");
  if(!egl_get_ptr(env, argv[2], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5188,"ptr");
  }
  weglNormalPointer(type,stride,ptr_idx);
}

void ecb_glColorPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5190,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5190,"type");
  if(!enif_get_int(env, argv[2],  &stride)) Badarg(5190,"stride");
  if(!egl_get_ptr(env, argv[3], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5190,"ptr");
  }
  weglColorPointer(size,type,stride,ptr_idx);
}

void ecb_glIndexPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5192,"type");
  if(!enif_get_int(env, argv[1],  &stride)) Badarg(5192,"stride");
  if(!egl_get_ptr(env, argv[2], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5192,"ptr");
  }
  weglIndexPointer(type,stride,ptr_idx);
}

void ecb_glTexCoordPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5194,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5194,"type");
  if(!enif_get_int(env, argv[2],  &stride)) Badarg(5194,"stride");
  if(!egl_get_ptr(env, argv[3], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5194,"ptr");
  }
  weglTexCoordPointer(size,type,stride,ptr_idx);
}

void ecb_glEdgeFlagPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_int(env, argv[0],  &stride)) Badarg(5196,"stride");
  if(!egl_get_ptr(env, argv[1], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[1], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5196,"ptr");
  }
  weglEdgeFlagPointer(stride,ptr_idx);
}

void ecb_glArrayElement(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint i;
  if(!enif_get_int(env, argv[0],  &i)) Badarg(5198,"i");
  weglArrayElement(i);
}

void ecb_glDrawArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint first;
  GLsizei count;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5199,"mode");
  if(!enif_get_int(env, argv[1],  &first)) Badarg(5199,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5199,"count");
  weglDrawArrays(mode,first,count);
}

void ecb_glDrawElements(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  GLvoid *indices_idx;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5200,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5200,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5200,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (GLvoid *) indices.data;
    else Badarg(5200,"indices");
  }
  weglDrawElements(mode,count,type,indices_idx);
}

void ecb_glInterleavedArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum format;
  GLsizei stride;
  ErlNifBinary pointer;
  GLvoid *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &format)) Badarg(5202,"format");
  if(!enif_get_int(env, argv[1],  &stride)) Badarg(5202,"stride");
  if(!egl_get_ptr(env, argv[2], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &pointer))
        pointer_idx = (GLvoid *) pointer.data;
    else Badarg(5202,"pointer");
  }
  weglInterleavedArrays(format,stride,pointer_idx);
}

void ecb_glShadeModel(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5204,"mode");
  weglShadeModel(mode);
}

void ecb_glLightf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum light;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5205,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5205,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5205,"param");
  weglLightf(light,pname,param);
}

void ecb_glLighti(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum light;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5206,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5206,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5206,"param");
  weglLighti(light,pname,param);
}

void ecb_glLightfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum light;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5207,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5207,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5207,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5207,"params");
   }};
  weglLightfv(light,pname,params);
}

void ecb_glLightiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum light;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5208,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5208,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5208,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5208,"params");
   }};
  weglLightiv(light,pname,params);
}

void ecb_glGetLightfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum light;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5209,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5209,"pname");
  weglGetLightfv(light,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetLightiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum light;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5210,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5210,"pname");
  weglGetLightiv(light,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glLightModelf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5211,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5211,"param");
  weglLightModelf(pname,param);
}

void ecb_glLightModeli(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5212,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5212,"param");
  weglLightModeli(pname,param);
}

void ecb_glLightModelfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5213,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5213,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5213,"params");
   }};
  weglLightModelfv(pname,params);
}

void ecb_glLightModeliv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5214,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5214,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5214,"params");
   }};
  weglLightModeliv(pname,params);
}

void ecb_glMaterialf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5215,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5215,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5215,"param");
  weglMaterialf(face,pname,param);
}

void ecb_glMateriali(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5216,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5216,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5216,"param");
  weglMateriali(face,pname,param);
}

void ecb_glMaterialfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5217,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5217,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5217,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5217,"params");
   }};
  weglMaterialfv(face,pname,params);
}

void ecb_glMaterialiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5218,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5218,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5218,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5218,"params");
   }};
  weglMaterialiv(face,pname,params);
}

void ecb_glGetMaterialfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum face;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5219,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5219,"pname");
  weglGetMaterialfv(face,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetMaterialiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum face;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5220,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5220,"pname");
  weglGetMaterialiv(face,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glColorMaterial(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5221,"face");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5221,"mode");
  weglColorMaterial(face,mode);
}

void ecb_glPixelZoom(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat xfactor;
  GLfloat yfactor;
  if(!egl_get_float(env, argv[0],  &xfactor)) Badarg(5222,"xfactor");
  if(!egl_get_float(env, argv[1],  &yfactor)) Badarg(5222,"yfactor");
  weglPixelZoom(xfactor,yfactor);
}

void ecb_glPixelStoref(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5223,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5223,"param");
  weglPixelStoref(pname,param);
}

void ecb_glPixelStorei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5224,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5224,"param");
  weglPixelStorei(pname,param);
}

void ecb_glPixelTransferf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5225,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5225,"param");
  weglPixelTransferf(pname,param);
}

void ecb_glPixelTransferi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5226,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5226,"param");
  weglPixelTransferi(pname,param);
}

void ecb_glPixelMapfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  GLsizei mapsize;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5227,"map");
  if(!enif_get_int(env, argv[1],  &mapsize)) Badarg(5227,"mapsize");
  if(!enif_inspect_binary(env, argv[2], &values)) Badarg(5227,"values");
  weglPixelMapfv(map,mapsize,(GLfloat *) values.data);
}

void ecb_glPixelMapuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  GLsizei mapsize;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5228,"map");
  if(!enif_get_int(env, argv[1],  &mapsize)) Badarg(5228,"mapsize");
  if(!enif_inspect_binary(env, argv[2], &values)) Badarg(5228,"values");
  weglPixelMapuiv(map,mapsize,(GLuint *) values.data);
}

void ecb_glPixelMapusv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  GLsizei mapsize;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5229,"map");
  if(!enif_get_int(env, argv[1],  &mapsize)) Badarg(5229,"mapsize");
  if(!enif_inspect_binary(env, argv[2], &values)) Badarg(5229,"values");
  weglPixelMapusv(map,mapsize,(GLushort *) values.data);
}

void ecb_glGetPixelMapfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5230,"map");
  if(enif_is_binary(env, argv[1]))
    enif_inspect_binary(env, argv[1], &values);
  else if(enif_is_tuple(env, argv[1])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[1], &values_a, &values_t) &&
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5230, "values");
  } else Badarg(5230, "values");
  weglGetPixelMapfv(map,(GLfloat *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetPixelMapuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5231,"map");
  if(enif_is_binary(env, argv[1]))
    enif_inspect_binary(env, argv[1], &values);
  else if(enif_is_tuple(env, argv[1])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[1], &values_a, &values_t) &&
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5231, "values");
  } else Badarg(5231, "values");
  weglGetPixelMapuiv(map,(GLuint *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetPixelMapusv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5232,"map");
  if(enif_is_binary(env, argv[1]))
    enif_inspect_binary(env, argv[1], &values);
  else if(enif_is_tuple(env, argv[1])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[1], &values_a, &values_t) &&
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5232, "values");
  } else Badarg(5232, "values");
  weglGetPixelMapusv(map,(GLushort *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glBitmap(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei width;
  GLsizei height;
  GLfloat xorig;
  GLfloat yorig;
  GLfloat xmove;
  GLfloat ymove;
  ErlNifBinary bitmap;
  GLubyte *bitmap_idx;
  if(!enif_get_int(env, argv[0],  &width)) Badarg(5233,"width");
  if(!enif_get_int(env, argv[1],  &height)) Badarg(5233,"height");
  if(!egl_get_float(env, argv[2],  &xorig)) Badarg(5233,"xorig");
  if(!egl_get_float(env, argv[3],  &yorig)) Badarg(5233,"yorig");
  if(!egl_get_float(env, argv[4],  &xmove)) Badarg(5233,"xmove");
  if(!egl_get_float(env, argv[5],  &ymove)) Badarg(5233,"ymove");
  if(!egl_get_ptr(env, argv[6], (void **) &bitmap_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &bitmap))
        bitmap_idx = (GLubyte *) bitmap.data;
    else Badarg(5233,"bitmap");
  }
  weglBitmap(width,height,xorig,yorig,xmove,ymove,bitmap_idx);
}

void ecb_glReadPixels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5235,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5235,"y");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5235,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5235,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5235,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5235,"type");
  if(enif_is_binary(env, argv[6]))
    enif_inspect_binary(env, argv[6], &pixels);
  else if(enif_is_tuple(env, argv[6])) {
    int pixels_a;
    const ERL_NIF_TERM *pixels_t;
    if(enif_get_tuple(env, argv[6], &pixels_a, &pixels_t) &&
         enif_is_binary(env, pixels_t[1]))
       enif_inspect_binary(env, pixels_t[1], &pixels);
    else Badarg(5235, "pixels");
  } else Badarg(5235, "pixels");
  weglReadPixels(x,y,width,height,format,type,(GLvoid *) pixels.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glDrawPixels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_int(env, argv[0],  &width)) Badarg(5236,"width");
  if(!enif_get_int(env, argv[1],  &height)) Badarg(5236,"height");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5236,"format");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5236,"type");
  if(!egl_get_ptr(env, argv[4], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[4], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5236,"pixels");
  }
  weglDrawPixels(width,height,format,type,pixels_idx);
}

void ecb_glCopyPixels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  GLenum type;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5238,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5238,"y");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5238,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5238,"height");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5238,"type");
  weglCopyPixels(x,y,width,height,type);
}

void ecb_glStencilFunc(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum func;
  GLint ref;
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &func)) Badarg(5239,"func");
  if(!enif_get_int(env, argv[1],  &ref)) Badarg(5239,"ref");
  if(!enif_get_uint(env, argv[2],  &mask)) Badarg(5239,"mask");
  weglStencilFunc(func,ref,mask);
}

void ecb_glStencilMask(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5240,"mask");
  weglStencilMask(mask);
}

void ecb_glStencilOp(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum fail;
  GLenum zfail;
  GLenum zpass;
  if(!enif_get_uint(env, argv[0],  &fail)) Badarg(5241,"fail");
  if(!enif_get_uint(env, argv[1],  &zfail)) Badarg(5241,"zfail");
  if(!enif_get_uint(env, argv[2],  &zpass)) Badarg(5241,"zpass");
  weglStencilOp(fail,zfail,zpass);
}

void ecb_glClearStencil(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5242,"s");
  weglClearStencil(s);
}

void ecb_glTexGend(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLdouble param;
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5243,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5243,"pname");
  if(!enif_get_double(env, argv[2],  &param)) Badarg(5243,"param");
  weglTexGend(coord,pname,param);
}

void ecb_glTexGenf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5244,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5244,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5244,"param");
  weglTexGenf(coord,pname,param);
}

void ecb_glTexGeni(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5245,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5245,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5245,"param");
  weglTexGeni(coord,pname,param);
}

void ecb_glTexGendv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5246,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5246,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5246,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_double(env, params_t[i], &params[i])) Badarg(5246,"params");
   }};
  weglTexGendv(coord,pname,params);
}

void ecb_glTexGenfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5247,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5247,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5247,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5247,"params");
   }};
  weglTexGenfv(coord,pname,params);
}

void ecb_glTexGeniv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5248,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5248,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5248,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5248,"params");
   }};
  weglTexGeniv(coord,pname,params);
}

void ecb_glGetTexGendv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum coord;
  GLenum pname;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5249,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5249,"pname");
  weglGetTexGendv(coord,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetTexGenfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum coord;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5250,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5250,"pname");
  weglGetTexGenfv(coord,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetTexGeniv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum coord;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5251,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5251,"pname");
  weglGetTexGeniv(coord,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glTexEnvf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5252,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5252,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5252,"param");
  weglTexEnvf(target,pname,param);
}

void ecb_glTexEnvi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5253,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5253,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5253,"param");
  weglTexEnvi(target,pname,param);
}

void ecb_glTexEnvfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5254,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5254,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5254,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5254,"params");
   }};
  weglTexEnvfv(target,pname,params);
}

void ecb_glTexEnviv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5255,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5255,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5255,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5255,"params");
   }};
  weglTexEnviv(target,pname,params);
}

void ecb_glGetTexEnvfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5256,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5256,"pname");
  weglGetTexEnvfv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetTexEnviv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5257,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5257,"pname");
  weglGetTexEnviv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glTexParameterf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5258,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5258,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5258,"param");
  weglTexParameterf(target,pname,param);
}

void ecb_glTexParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5259,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5259,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5259,"param");
  weglTexParameteri(target,pname,param);
}

void ecb_glTexParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5260,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5260,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5260,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5260,"params");
   }};
  weglTexParameterfv(target,pname,params);
}

void ecb_glTexParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5261,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5261,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5261,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5261,"params");
   }};
  weglTexParameteriv(target,pname,params);
}

void ecb_glGetTexParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5262,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5262,"pname");
  weglGetTexParameterfv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetTexParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5263,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5263,"pname");
  weglGetTexParameteriv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetTexLevelParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLint level;
  GLenum pname;
  GLfloat params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5264,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5264,"level");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5264,"pname");
  weglGetTexLevelParameterfv(target,level,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_double(env, (double) params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetTexLevelParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLint level;
  GLenum pname;
  GLint params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5265,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5265,"level");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5265,"pname");
  weglGetTexLevelParameteriv(target,level,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_int(env, params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glTexImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint internalFormat;
  GLsizei width;
  GLint border;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5266,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5266,"level");
  if(!enif_get_int(env, argv[2],  &internalFormat)) Badarg(5266,"internalFormat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5266,"width");
  if(!enif_get_int(env, argv[4],  &border)) Badarg(5266,"border");
  if(!enif_get_uint(env, argv[5],  &format)) Badarg(5266,"format");
  if(!enif_get_uint(env, argv[6],  &type)) Badarg(5266,"type");
  if(!egl_get_ptr(env, argv[7], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[7], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5266,"pixels");
  }
  weglTexImage1D(target,level,internalFormat,width,border,format,type,pixels_idx);
}

void ecb_glTexImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLint border;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5268,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5268,"level");
  if(!enif_get_int(env, argv[2],  &internalFormat)) Badarg(5268,"internalFormat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5268,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5268,"height");
  if(!enif_get_int(env, argv[5],  &border)) Badarg(5268,"border");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5268,"format");
  if(!enif_get_uint(env, argv[7],  &type)) Badarg(5268,"type");
  if(!egl_get_ptr(env, argv[8], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5268,"pixels");
  }
  weglTexImage2D(target,level,internalFormat,width,height,border,format,type,pixels_idx);
}

void ecb_glGetTexImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5270,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5270,"level");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5270,"format");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5270,"type");
  if(enif_is_binary(env, argv[4]))
    enif_inspect_binary(env, argv[4], &pixels);
  else if(enif_is_tuple(env, argv[4])) {
    int pixels_a;
    const ERL_NIF_TERM *pixels_t;
    if(enif_get_tuple(env, argv[4], &pixels_a, &pixels_t) &&
         enif_is_binary(env, pixels_t[1]))
       enif_inspect_binary(env, pixels_t[1], &pixels);
    else Badarg(5270, "pixels");
  } else Badarg(5270, "pixels");
  weglGetTexImage(target,level,format,type,(GLvoid *) pixels.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGenTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5271,"n");
  std::vector <GLuint> textures (n);
  std::vector <ERL_NIF_TERM> textures_ts (n);
  weglGenTextures(n,textures.data());
  for(int ri=0; ri < (int) n; ri++)
    textures_ts[ri] =      enif_make_int(env, textures[ri]);
  reply =      enif_make_list_from_array(env, textures_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDeleteTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *textures;
  std::vector <GLuint> textures_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5272,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5272, "textures")
  else {
    ERL_NIF_TERM textures_l, textures_h, textures_t;
    GLuint textures_tmp;
    textures_l = argv[1];
    while(enif_get_list_cell(env, textures_l, &textures_h, &textures_t)) {
        if(!enif_get_uint(env, textures_h, &textures_tmp)) Badarg(5272,"textures");
        textures_vec.push_back(textures_tmp);
        textures_l = textures_t;
    };
    textures = textures_vec.data();
  }
  weglDeleteTextures(n,textures);
}

void ecb_glBindTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint texture;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5273,"target");
  if(!enif_get_uint(env, argv[1],  &texture)) Badarg(5273,"texture");
  weglBindTexture(target,texture);
}

void ecb_glPrioritizeTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *textures;
  std::vector <GLuint> textures_vec;
  GLclampf *priorities;
  std::vector <GLclampf> priorities_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5274,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5274, "textures")
  else {
    ERL_NIF_TERM textures_l, textures_h, textures_t;
    GLuint textures_tmp;
    textures_l = argv[1];
    while(enif_get_list_cell(env, textures_l, &textures_h, &textures_t)) {
        if(!enif_get_uint(env, textures_h, &textures_tmp)) Badarg(5274,"textures");
        textures_vec.push_back(textures_tmp);
        textures_l = textures_t;
    };
    textures = textures_vec.data();
  }
  if(!enif_is_list(env, argv[2])) Badarg(5274, "priorities")
  else {
    ERL_NIF_TERM priorities_l, priorities_h, priorities_t;
    GLclampf priorities_tmp;
    priorities_l = argv[2];
    while(enif_get_list_cell(env, priorities_l, &priorities_h, &priorities_t)) {
        if(!egl_get_float(env, priorities_h, &priorities_tmp)) Badarg(5274,"priorities");
        priorities_vec.push_back(priorities_tmp);
        priorities_l = priorities_t;
    };
    priorities = priorities_vec.data();
  }
  weglPrioritizeTextures(n,textures,priorities);
}

void ecb_glAreTexturesResident(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint *textures;
  std::vector <GLuint> textures_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5275,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5275, "textures")
  else {
    ERL_NIF_TERM textures_l, textures_h, textures_t;
    GLuint textures_tmp;
    textures_l = argv[1];
    while(enif_get_list_cell(env, textures_l, &textures_h, &textures_t)) {
        if(!enif_get_uint(env, textures_h, &textures_tmp)) Badarg(5275,"textures");
        textures_vec.push_back(textures_tmp);
        textures_l = textures_t;
    };
    textures = textures_vec.data();
  }
  std::vector <GLboolean> residences (n);
  std::vector <ERL_NIF_TERM> residences_ts (n);
  result = weglAreTexturesResident(n,textures,residences.data());
  for(int ri=0; ri < (int) n; ri++)
    residences_ts[ri] =      enif_make_int(env, residences[ri]);
  reply = enif_make_tuple2(env,
          enif_make_int(env, result),
     enif_make_list_from_array(env, residences_ts.data(), n) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint texture;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5276,"texture");
  result = weglIsTexture(texture);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glTexSubImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLsizei width;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5277,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5277,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5277,"xoffset");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5277,"width");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5277,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5277,"type");
  if(!egl_get_ptr(env, argv[6], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5277,"pixels");
  }
  weglTexSubImage1D(target,level,xoffset,width,format,type,pixels_idx);
}

void ecb_glTexSubImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5279,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5279,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5279,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5279,"yoffset");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5279,"width");
  if(!enif_get_int(env, argv[5],  &height)) Badarg(5279,"height");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5279,"format");
  if(!enif_get_uint(env, argv[7],  &type)) Badarg(5279,"type");
  if(!egl_get_ptr(env, argv[8], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5279,"pixels");
  }
  weglTexSubImage2D(target,level,xoffset,yoffset,width,height,format,type,pixels_idx);
}

void ecb_glCopyTexImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  GLint border;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5281,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5281,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5281,"internalformat");
  if(!enif_get_int(env, argv[3],  &x)) Badarg(5281,"x");
  if(!enif_get_int(env, argv[4],  &y)) Badarg(5281,"y");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5281,"width");
  if(!enif_get_int(env, argv[6],  &border)) Badarg(5281,"border");
  weglCopyTexImage1D(target,level,internalformat,x,y,width,border);
}

void ecb_glCopyTexImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  GLint border;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5282,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5282,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5282,"internalformat");
  if(!enif_get_int(env, argv[3],  &x)) Badarg(5282,"x");
  if(!enif_get_int(env, argv[4],  &y)) Badarg(5282,"y");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5282,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5282,"height");
  if(!enif_get_int(env, argv[7],  &border)) Badarg(5282,"border");
  weglCopyTexImage2D(target,level,internalformat,x,y,width,height,border);
}

void ecb_glCopyTexSubImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint x;
  GLint y;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5283,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5283,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5283,"xoffset");
  if(!enif_get_int(env, argv[3],  &x)) Badarg(5283,"x");
  if(!enif_get_int(env, argv[4],  &y)) Badarg(5283,"y");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5283,"width");
  weglCopyTexSubImage1D(target,level,xoffset,x,y,width);
}

void ecb_glCopyTexSubImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5284,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5284,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5284,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5284,"yoffset");
  if(!enif_get_int(env, argv[4],  &x)) Badarg(5284,"x");
  if(!enif_get_int(env, argv[5],  &y)) Badarg(5284,"y");
  if(!enif_get_int(env, argv[6],  &width)) Badarg(5284,"width");
  if(!enif_get_int(env, argv[7],  &height)) Badarg(5284,"height");
  weglCopyTexSubImage2D(target,level,xoffset,yoffset,x,y,width,height);
}

void ecb_glMap1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble u1;
  GLdouble u2;
  GLint stride;
  GLint order;
  ErlNifBinary points;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5285,"target");
  if(!enif_get_double(env, argv[1],  &u1)) Badarg(5285,"u1");
  if(!enif_get_double(env, argv[2],  &u2)) Badarg(5285,"u2");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5285,"stride");
  if(!enif_get_int(env, argv[4],  &order)) Badarg(5285,"order");
  if(!enif_inspect_binary(env, argv[5], &points)) Badarg(5285,"points");
  weglMap1d(target,u1,u2,stride,order,(GLdouble *) points.data);
}

void ecb_glMap1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat u1;
  GLfloat u2;
  GLint stride;
  GLint order;
  ErlNifBinary points;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5286,"target");
  if(!egl_get_float(env, argv[1],  &u1)) Badarg(5286,"u1");
  if(!egl_get_float(env, argv[2],  &u2)) Badarg(5286,"u2");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5286,"stride");
  if(!enif_get_int(env, argv[4],  &order)) Badarg(5286,"order");
  if(!enif_inspect_binary(env, argv[5], &points)) Badarg(5286,"points");
  weglMap1f(target,u1,u2,stride,order,(GLfloat *) points.data);
}

void ecb_glMap2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble u1;
  GLdouble u2;
  GLint ustride;
  GLint uorder;
  GLdouble v1;
  GLdouble v2;
  GLint vstride;
  GLint vorder;
  ErlNifBinary points;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5287,"target");
  if(!enif_get_double(env, argv[1],  &u1)) Badarg(5287,"u1");
  if(!enif_get_double(env, argv[2],  &u2)) Badarg(5287,"u2");
  if(!enif_get_int(env, argv[3],  &ustride)) Badarg(5287,"ustride");
  if(!enif_get_int(env, argv[4],  &uorder)) Badarg(5287,"uorder");
  if(!enif_get_double(env, argv[5],  &v1)) Badarg(5287,"v1");
  if(!enif_get_double(env, argv[6],  &v2)) Badarg(5287,"v2");
  if(!enif_get_int(env, argv[7],  &vstride)) Badarg(5287,"vstride");
  if(!enif_get_int(env, argv[8],  &vorder)) Badarg(5287,"vorder");
  if(!enif_inspect_binary(env, argv[9], &points)) Badarg(5287,"points");
  weglMap2d(target,u1,u2,ustride,uorder,v1,v2,vstride,vorder,(GLdouble *) points.data);
}

void ecb_glMap2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat u1;
  GLfloat u2;
  GLint ustride;
  GLint uorder;
  GLfloat v1;
  GLfloat v2;
  GLint vstride;
  GLint vorder;
  ErlNifBinary points;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5288,"target");
  if(!egl_get_float(env, argv[1],  &u1)) Badarg(5288,"u1");
  if(!egl_get_float(env, argv[2],  &u2)) Badarg(5288,"u2");
  if(!enif_get_int(env, argv[3],  &ustride)) Badarg(5288,"ustride");
  if(!enif_get_int(env, argv[4],  &uorder)) Badarg(5288,"uorder");
  if(!egl_get_float(env, argv[5],  &v1)) Badarg(5288,"v1");
  if(!egl_get_float(env, argv[6],  &v2)) Badarg(5288,"v2");
  if(!enif_get_int(env, argv[7],  &vstride)) Badarg(5288,"vstride");
  if(!enif_get_int(env, argv[8],  &vorder)) Badarg(5288,"vorder");
  if(!enif_inspect_binary(env, argv[9], &points)) Badarg(5288,"points");
  weglMap2f(target,u1,u2,ustride,uorder,v1,v2,vstride,vorder,(GLfloat *) points.data);
}

void ecb_glGetMapdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum query;
  ErlNifBinary v;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5289,"target");
  if(!enif_get_uint(env, argv[1],  &query)) Badarg(5289,"query");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &v);
  else if(enif_is_tuple(env, argv[2])) {
    int v_a;
    const ERL_NIF_TERM *v_t;
    if(enif_get_tuple(env, argv[2], &v_a, &v_t) &&
         enif_is_binary(env, v_t[1]))
       enif_inspect_binary(env, v_t[1], &v);
    else Badarg(5289, "v");
  } else Badarg(5289, "v");
  weglGetMapdv(target,query,(GLdouble *) v.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetMapfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum query;
  ErlNifBinary v;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5290,"target");
  if(!enif_get_uint(env, argv[1],  &query)) Badarg(5290,"query");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &v);
  else if(enif_is_tuple(env, argv[2])) {
    int v_a;
    const ERL_NIF_TERM *v_t;
    if(enif_get_tuple(env, argv[2], &v_a, &v_t) &&
         enif_is_binary(env, v_t[1]))
       enif_inspect_binary(env, v_t[1], &v);
    else Badarg(5290, "v");
  } else Badarg(5290, "v");
  weglGetMapfv(target,query,(GLfloat *) v.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetMapiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum query;
  ErlNifBinary v;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5291,"target");
  if(!enif_get_uint(env, argv[1],  &query)) Badarg(5291,"query");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &v);
  else if(enif_is_tuple(env, argv[2])) {
    int v_a;
    const ERL_NIF_TERM *v_t;
    if(enif_get_tuple(env, argv[2], &v_a, &v_t) &&
         enif_is_binary(env, v_t[1]))
       enif_inspect_binary(env, v_t[1], &v);
    else Badarg(5291, "v");
  } else Badarg(5291, "v");
  weglGetMapiv(target,query,(GLint *) v.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glEvalCoord1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble u;
  if(!enif_get_double(env, argv[0],  &u)) Badarg(5292,"u");
  weglEvalCoord1d(u);
}

void ecb_glEvalCoord1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat u;
  if(!egl_get_float(env, argv[0],  &u)) Badarg(5293,"u");
  weglEvalCoord1f(u);
}

void ecb_glEvalCoord2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble u;
  GLdouble v;
  if(!enif_get_double(env, argv[0],  &u)) Badarg(5294,"u");
  if(!enif_get_double(env, argv[1],  &v)) Badarg(5294,"v");
  weglEvalCoord2d(u,v);
}

void ecb_glEvalCoord2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat u;
  GLfloat v;
  if(!egl_get_float(env, argv[0],  &u)) Badarg(5295,"u");
  if(!egl_get_float(env, argv[1],  &v)) Badarg(5295,"v");
  weglEvalCoord2f(u,v);
}

void ecb_glMapGrid1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint un;
  GLdouble u1;
  GLdouble u2;
  if(!enif_get_int(env, argv[0],  &un)) Badarg(5296,"un");
  if(!enif_get_double(env, argv[1],  &u1)) Badarg(5296,"u1");
  if(!enif_get_double(env, argv[2],  &u2)) Badarg(5296,"u2");
  weglMapGrid1d(un,u1,u2);
}

void ecb_glMapGrid1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint un;
  GLfloat u1;
  GLfloat u2;
  if(!enif_get_int(env, argv[0],  &un)) Badarg(5297,"un");
  if(!egl_get_float(env, argv[1],  &u1)) Badarg(5297,"u1");
  if(!egl_get_float(env, argv[2],  &u2)) Badarg(5297,"u2");
  weglMapGrid1f(un,u1,u2);
}

void ecb_glMapGrid2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint un;
  GLdouble u1;
  GLdouble u2;
  GLint vn;
  GLdouble v1;
  GLdouble v2;
  if(!enif_get_int(env, argv[0],  &un)) Badarg(5298,"un");
  if(!enif_get_double(env, argv[1],  &u1)) Badarg(5298,"u1");
  if(!enif_get_double(env, argv[2],  &u2)) Badarg(5298,"u2");
  if(!enif_get_int(env, argv[3],  &vn)) Badarg(5298,"vn");
  if(!enif_get_double(env, argv[4],  &v1)) Badarg(5298,"v1");
  if(!enif_get_double(env, argv[5],  &v2)) Badarg(5298,"v2");
  weglMapGrid2d(un,u1,u2,vn,v1,v2);
}

void ecb_glMapGrid2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint un;
  GLfloat u1;
  GLfloat u2;
  GLint vn;
  GLfloat v1;
  GLfloat v2;
  if(!enif_get_int(env, argv[0],  &un)) Badarg(5299,"un");
  if(!egl_get_float(env, argv[1],  &u1)) Badarg(5299,"u1");
  if(!egl_get_float(env, argv[2],  &u2)) Badarg(5299,"u2");
  if(!enif_get_int(env, argv[3],  &vn)) Badarg(5299,"vn");
  if(!egl_get_float(env, argv[4],  &v1)) Badarg(5299,"v1");
  if(!egl_get_float(env, argv[5],  &v2)) Badarg(5299,"v2");
  weglMapGrid2f(un,u1,u2,vn,v1,v2);
}

void ecb_glEvalPoint1(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint i;
  if(!enif_get_int(env, argv[0],  &i)) Badarg(5300,"i");
  weglEvalPoint1(i);
}

void ecb_glEvalPoint2(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint i;
  GLint j;
  if(!enif_get_int(env, argv[0],  &i)) Badarg(5301,"i");
  if(!enif_get_int(env, argv[1],  &j)) Badarg(5301,"j");
  weglEvalPoint2(i,j);
}

void ecb_glEvalMesh1(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint i1;
  GLint i2;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5302,"mode");
  if(!enif_get_int(env, argv[1],  &i1)) Badarg(5302,"i1");
  if(!enif_get_int(env, argv[2],  &i2)) Badarg(5302,"i2");
  weglEvalMesh1(mode,i1,i2);
}

void ecb_glEvalMesh2(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint i1;
  GLint i2;
  GLint j1;
  GLint j2;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5303,"mode");
  if(!enif_get_int(env, argv[1],  &i1)) Badarg(5303,"i1");
  if(!enif_get_int(env, argv[2],  &i2)) Badarg(5303,"i2");
  if(!enif_get_int(env, argv[3],  &j1)) Badarg(5303,"j1");
  if(!enif_get_int(env, argv[4],  &j2)) Badarg(5303,"j2");
  weglEvalMesh2(mode,i1,i2,j1,j2);
}

void ecb_glFogf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5304,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5304,"param");
  weglFogf(pname,param);
}

void ecb_glFogi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5305,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5305,"param");
  weglFogi(pname,param);
}

void ecb_glFogfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5306,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5306,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5306,"params");
   }};
  weglFogfv(pname,params);
}

void ecb_glFogiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5307,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5307,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5307,"params");
   }};
  weglFogiv(pname,params);
}

void ecb_glFeedbackBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei size;
  GLenum type;
  ErlNifBinary buffer;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5308,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5308,"type");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &buffer);
  else if(enif_is_tuple(env, argv[2])) {
    int buffer_a;
    const ERL_NIF_TERM *buffer_t;
    if(enif_get_tuple(env, argv[2], &buffer_a, &buffer_t) &&
         enif_is_binary(env, buffer_t[1]))
       enif_inspect_binary(env, buffer_t[1], &buffer);
    else Badarg(5308, "buffer");
  } else Badarg(5308, "buffer");
  weglFeedbackBuffer(size,type,(GLfloat *) buffer.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glPassThrough(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat token;
  if(!egl_get_float(env, argv[0],  &token)) Badarg(5309,"token");
  weglPassThrough(token);
}

void ecb_glSelectBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei size;
  ErlNifBinary buffer;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5310,"size");
  if(enif_is_binary(env, argv[1]))
    enif_inspect_binary(env, argv[1], &buffer);
  else if(enif_is_tuple(env, argv[1])) {
    int buffer_a;
    const ERL_NIF_TERM *buffer_t;
    if(enif_get_tuple(env, argv[1], &buffer_a, &buffer_t) &&
         enif_is_binary(env, buffer_t[1]))
       enif_inspect_binary(env, buffer_t[1], &buffer);
    else Badarg(5310, "buffer");
  } else Badarg(5310, "buffer");
  weglSelectBuffer(size,(GLuint *) buffer.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glInitNames(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglInitNames();
}

void ecb_glLoadName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint name;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5312,"name");
  weglLoadName(name);
}

void ecb_glPushName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint name;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5313,"name");
  weglPushName(name);
}

void ecb_glPopName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopName();
}

void ecb_glDrawRangeElements(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint start;
  GLuint end;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  GLvoid *indices_idx;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5315,"mode");
  if(!enif_get_uint(env, argv[1],  &start)) Badarg(5315,"start");
  if(!enif_get_uint(env, argv[2],  &end)) Badarg(5315,"end");
  if(!enif_get_int(env, argv[3],  &count)) Badarg(5315,"count");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5315,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &indices))
        indices_idx = (GLvoid *) indices.data;
    else Badarg(5315,"indices");
  }
  weglDrawRangeElements(mode,start,end,count,type,indices_idx);
}

void ecb_glTexImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLint border;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5317,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5317,"level");
  if(!enif_get_int(env, argv[2],  &internalFormat)) Badarg(5317,"internalFormat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5317,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5317,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5317,"depth");
  if(!enif_get_int(env, argv[6],  &border)) Badarg(5317,"border");
  if(!enif_get_uint(env, argv[7],  &format)) Badarg(5317,"format");
  if(!enif_get_uint(env, argv[8],  &type)) Badarg(5317,"type");
  if(!egl_get_ptr(env, argv[9], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[9], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5317,"pixels");
  }
  weglTexImage3D(target,level,internalFormat,width,height,depth,border,format,type,pixels_idx);
}

void ecb_glTexSubImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5319,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5319,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5319,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5319,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5319,"zoffset");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5319,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5319,"height");
  if(!enif_get_int(env, argv[7],  &depth)) Badarg(5319,"depth");
  if(!enif_get_uint(env, argv[8],  &format)) Badarg(5319,"format");
  if(!enif_get_uint(env, argv[9],  &type)) Badarg(5319,"type");
  if(!egl_get_ptr(env, argv[10], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[10], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5319,"pixels");
  }
  weglTexSubImage3D(target,level,xoffset,yoffset,zoffset,width,height,depth,format,type,pixels_idx);
}

void ecb_glCopyTexSubImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5321,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5321,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5321,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5321,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5321,"zoffset");
  if(!enif_get_int(env, argv[5],  &x)) Badarg(5321,"x");
  if(!enif_get_int(env, argv[6],  &y)) Badarg(5321,"y");
  if(!enif_get_int(env, argv[7],  &width)) Badarg(5321,"width");
  if(!enif_get_int(env, argv[8],  &height)) Badarg(5321,"height");
  weglCopyTexSubImage3D(target,level,xoffset,yoffset,zoffset,x,y,width,height);
}

void ecb_glActiveTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum texture;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5322,"texture");
  weglActiveTexture(texture);
}

void ecb_glSampleCoverage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf value;
  GLboolean invert;
  if(!egl_get_float(env, argv[0],  &value)) Badarg(5323,"value");
  if(!egl_get_ubyte(env, argv[1],  &invert)) Badarg(5323,"invert");
  weglSampleCoverage(value,invert);
}

void ecb_glCompressedTexImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLint border;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5324,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5324,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5324,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5324,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5324,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5324,"depth");
  if(!enif_get_int(env, argv[6],  &border)) Badarg(5324,"border");
  if(!enif_get_int(env, argv[7],  &imageSize)) Badarg(5324,"imageSize");
  if(!egl_get_ptr(env, argv[8], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5324,"data");
  }
  weglCompressedTexImage3D(target,level,internalformat,width,height,depth,border,imageSize,data_idx);
}

void ecb_glCompressedTexImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLint border;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5326,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5326,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5326,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5326,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5326,"height");
  if(!enif_get_int(env, argv[5],  &border)) Badarg(5326,"border");
  if(!enif_get_int(env, argv[6],  &imageSize)) Badarg(5326,"imageSize");
  if(!egl_get_ptr(env, argv[7], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[7], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5326,"data");
  }
  weglCompressedTexImage2D(target,level,internalformat,width,height,border,imageSize,data_idx);
}

void ecb_glCompressedTexImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLsizei width;
  GLint border;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5328,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5328,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5328,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5328,"width");
  if(!enif_get_int(env, argv[4],  &border)) Badarg(5328,"border");
  if(!enif_get_int(env, argv[5],  &imageSize)) Badarg(5328,"imageSize");
  if(!egl_get_ptr(env, argv[6], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5328,"data");
  }
  weglCompressedTexImage1D(target,level,internalformat,width,border,imageSize,data_idx);
}

void ecb_glCompressedTexSubImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5330,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5330,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5330,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5330,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5330,"zoffset");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5330,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5330,"height");
  if(!enif_get_int(env, argv[7],  &depth)) Badarg(5330,"depth");
  if(!enif_get_uint(env, argv[8],  &format)) Badarg(5330,"format");
  if(!enif_get_int(env, argv[9],  &imageSize)) Badarg(5330,"imageSize");
  if(!egl_get_ptr(env, argv[10], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[10], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5330,"data");
  }
  weglCompressedTexSubImage3D(target,level,xoffset,yoffset,zoffset,width,height,depth,format,imageSize,data_idx);
}

void ecb_glCompressedTexSubImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5332,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5332,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5332,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5332,"yoffset");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5332,"width");
  if(!enif_get_int(env, argv[5],  &height)) Badarg(5332,"height");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5332,"format");
  if(!enif_get_int(env, argv[7],  &imageSize)) Badarg(5332,"imageSize");
  if(!egl_get_ptr(env, argv[8], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5332,"data");
  }
  weglCompressedTexSubImage2D(target,level,xoffset,yoffset,width,height,format,imageSize,data_idx);
}

void ecb_glCompressedTexSubImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLsizei width;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5334,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5334,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5334,"xoffset");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5334,"width");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5334,"format");
  if(!enif_get_int(env, argv[5],  &imageSize)) Badarg(5334,"imageSize");
  if(!egl_get_ptr(env, argv[6], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5334,"data");
  }
  weglCompressedTexSubImage1D(target,level,xoffset,width,format,imageSize,data_idx);
}

void ecb_glGetCompressedTexImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint lod;
  ErlNifBinary img;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5336,"target");
  if(!enif_get_int(env, argv[1],  &lod)) Badarg(5336,"lod");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &img);
  else if(enif_is_tuple(env, argv[2])) {
    int img_a;
    const ERL_NIF_TERM *img_t;
    if(enif_get_tuple(env, argv[2], &img_a, &img_t) &&
         enif_is_binary(env, img_t[1]))
       enif_inspect_binary(env, img_t[1], &img);
    else Badarg(5336, "img");
  } else Badarg(5336, "img");
  weglGetCompressedTexImage(target,lod,(GLvoid *) img.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glClientActiveTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum texture;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5337,"texture");
  weglClientActiveTexture(texture);
}

void ecb_glMultiTexCoord1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble s;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5338,"target");
  if(!enif_get_double(env, argv[1],  &s)) Badarg(5338,"s");
  weglMultiTexCoord1d(target,s);
}

void ecb_glMultiTexCoord1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat s;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5339,"target");
  if(!egl_get_float(env, argv[1],  &s)) Badarg(5339,"s");
  weglMultiTexCoord1f(target,s);
}

void ecb_glMultiTexCoord1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint s;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5340,"target");
  if(!enif_get_int(env, argv[1],  &s)) Badarg(5340,"s");
  weglMultiTexCoord1i(target,s);
}

void ecb_glMultiTexCoord1s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLshort s;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5341,"target");
  if(!egl_get_short(env, argv[1],  &s)) Badarg(5341,"s");
  weglMultiTexCoord1s(target,s);
}

void ecb_glMultiTexCoord2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble s;
  GLdouble t;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5342,"target");
  if(!enif_get_double(env, argv[1],  &s)) Badarg(5342,"s");
  if(!enif_get_double(env, argv[2],  &t)) Badarg(5342,"t");
  weglMultiTexCoord2d(target,s,t);
}

void ecb_glMultiTexCoord2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat s;
  GLfloat t;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5343,"target");
  if(!egl_get_float(env, argv[1],  &s)) Badarg(5343,"s");
  if(!egl_get_float(env, argv[2],  &t)) Badarg(5343,"t");
  weglMultiTexCoord2f(target,s,t);
}

void ecb_glMultiTexCoord2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint s;
  GLint t;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5344,"target");
  if(!enif_get_int(env, argv[1],  &s)) Badarg(5344,"s");
  if(!enif_get_int(env, argv[2],  &t)) Badarg(5344,"t");
  weglMultiTexCoord2i(target,s,t);
}

void ecb_glMultiTexCoord2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLshort s;
  GLshort t;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5345,"target");
  if(!egl_get_short(env, argv[1],  &s)) Badarg(5345,"s");
  if(!egl_get_short(env, argv[2],  &t)) Badarg(5345,"t");
  weglMultiTexCoord2s(target,s,t);
}

void ecb_glMultiTexCoord3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble s;
  GLdouble t;
  GLdouble r;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5346,"target");
  if(!enif_get_double(env, argv[1],  &s)) Badarg(5346,"s");
  if(!enif_get_double(env, argv[2],  &t)) Badarg(5346,"t");
  if(!enif_get_double(env, argv[3],  &r)) Badarg(5346,"r");
  weglMultiTexCoord3d(target,s,t,r);
}

void ecb_glMultiTexCoord3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat s;
  GLfloat t;
  GLfloat r;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5347,"target");
  if(!egl_get_float(env, argv[1],  &s)) Badarg(5347,"s");
  if(!egl_get_float(env, argv[2],  &t)) Badarg(5347,"t");
  if(!egl_get_float(env, argv[3],  &r)) Badarg(5347,"r");
  weglMultiTexCoord3f(target,s,t,r);
}

void ecb_glMultiTexCoord3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint s;
  GLint t;
  GLint r;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5348,"target");
  if(!enif_get_int(env, argv[1],  &s)) Badarg(5348,"s");
  if(!enif_get_int(env, argv[2],  &t)) Badarg(5348,"t");
  if(!enif_get_int(env, argv[3],  &r)) Badarg(5348,"r");
  weglMultiTexCoord3i(target,s,t,r);
}

void ecb_glMultiTexCoord3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLshort s;
  GLshort t;
  GLshort r;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5349,"target");
  if(!egl_get_short(env, argv[1],  &s)) Badarg(5349,"s");
  if(!egl_get_short(env, argv[2],  &t)) Badarg(5349,"t");
  if(!egl_get_short(env, argv[3],  &r)) Badarg(5349,"r");
  weglMultiTexCoord3s(target,s,t,r);
}

void ecb_glMultiTexCoord4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble s;
  GLdouble t;
  GLdouble r;
  GLdouble q;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5350,"target");
  if(!enif_get_double(env, argv[1],  &s)) Badarg(5350,"s");
  if(!enif_get_double(env, argv[2],  &t)) Badarg(5350,"t");
  if(!enif_get_double(env, argv[3],  &r)) Badarg(5350,"r");
  if(!enif_get_double(env, argv[4],  &q)) Badarg(5350,"q");
  weglMultiTexCoord4d(target,s,t,r,q);
}

void ecb_glMultiTexCoord4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat s;
  GLfloat t;
  GLfloat r;
  GLfloat q;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5351,"target");
  if(!egl_get_float(env, argv[1],  &s)) Badarg(5351,"s");
  if(!egl_get_float(env, argv[2],  &t)) Badarg(5351,"t");
  if(!egl_get_float(env, argv[3],  &r)) Badarg(5351,"r");
  if(!egl_get_float(env, argv[4],  &q)) Badarg(5351,"q");
  weglMultiTexCoord4f(target,s,t,r,q);
}

void ecb_glMultiTexCoord4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint s;
  GLint t;
  GLint r;
  GLint q;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5352,"target");
  if(!enif_get_int(env, argv[1],  &s)) Badarg(5352,"s");
  if(!enif_get_int(env, argv[2],  &t)) Badarg(5352,"t");
  if(!enif_get_int(env, argv[3],  &r)) Badarg(5352,"r");
  if(!enif_get_int(env, argv[4],  &q)) Badarg(5352,"q");
  weglMultiTexCoord4i(target,s,t,r,q);
}

void ecb_glMultiTexCoord4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLshort s;
  GLshort t;
  GLshort r;
  GLshort q;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5353,"target");
  if(!egl_get_short(env, argv[1],  &s)) Badarg(5353,"s");
  if(!egl_get_short(env, argv[2],  &t)) Badarg(5353,"t");
  if(!egl_get_short(env, argv[3],  &r)) Badarg(5353,"r");
  if(!egl_get_short(env, argv[4],  &q)) Badarg(5353,"q");
  weglMultiTexCoord4s(target,s,t,r,q);
}

void ecb_glLoadTransposeMatrixf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5354,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5354,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5354,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5354,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5354,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5354,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5354,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadTransposeMatrixf(m);
}

void ecb_glLoadTransposeMatrixd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5355,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5355,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5355,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5355,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5355,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5355,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5355,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadTransposeMatrixd(m);
}

void ecb_glMultTransposeMatrixf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5356,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5356,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5356,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5356,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5356,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5356,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5356,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultTransposeMatrixf(m);
}

void ecb_glMultTransposeMatrixd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5357,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5357,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5357,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5357,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5357,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5357,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5357,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultTransposeMatrixd(m);
}

void ecb_glBlendFuncSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum sfactorRGB;
  GLenum dfactorRGB;
  GLenum sfactorAlpha;
  GLenum dfactorAlpha;
  if(!enif_get_uint(env, argv[0],  &sfactorRGB)) Badarg(5358,"sfactorRGB");
  if(!enif_get_uint(env, argv[1],  &dfactorRGB)) Badarg(5358,"dfactorRGB");
  if(!enif_get_uint(env, argv[2],  &sfactorAlpha)) Badarg(5358,"sfactorAlpha");
  if(!enif_get_uint(env, argv[3],  &dfactorAlpha)) Badarg(5358,"dfactorAlpha");
  weglBlendFuncSeparate(sfactorRGB,dfactorRGB,sfactorAlpha,dfactorAlpha);
}

void ecb_glMultiDrawArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  ErlNifBinary first_bin;
  unsigned int first_len;
  std::vector <GLint> first_vec;
  GLint *first;
  ErlNifBinary count_bin;
  unsigned int count_len;
  std::vector <GLsizei> count_vec;
  GLsizei *count;
  GLsizei drawcount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5359,"mode");
  if(!enif_is_list(env, argv[1])) {
    if(enif_is_binary(env, argv[1])) {
       enif_inspect_binary(env, argv[1], &first_bin);
    } else if(enif_is_tuple(env, argv[1])) {
       int first_a;
       const ERL_NIF_TERM *first_t;
       if(enif_get_tuple(env, argv[1], &first_a, &first_t) &&
              enif_is_binary(env, first_t[1]))
          enif_inspect_binary(env, first_t[1], &first_bin);
       else Badarg(5359, "first");
    } else Badarg(5359, "first");
    first = (GLint *) first_bin.data;
    first_len = first_bin.size / sizeof(GLint);
 } else {
    ERL_NIF_TERM first_l, first_h, first_t;
    GLint first_tmp;
    first_l = argv[1];
    while(enif_get_list_cell(env, first_l, &first_h, &first_t)) {
        if(!enif_get_int(env, first_h, &first_tmp)) Badarg(5359,"first");
        first_vec.push_back(first_tmp);
        first_l = first_t;
    };
    first = first_vec.data();
    first_len = first_vec.size();
  }
  if(!enif_is_list(env, argv[2])) {
    if(enif_is_binary(env, argv[2])) {
       enif_inspect_binary(env, argv[2], &count_bin);
    } else if(enif_is_tuple(env, argv[2])) {
       int count_a;
       const ERL_NIF_TERM *count_t;
       if(enif_get_tuple(env, argv[2], &count_a, &count_t) &&
              enif_is_binary(env, count_t[1]))
          enif_inspect_binary(env, count_t[1], &count_bin);
       else Badarg(5359, "count");
    } else Badarg(5359, "count");
    count = (GLsizei *) count_bin.data;
    count_len = count_bin.size / sizeof(GLsizei);
 } else {
    ERL_NIF_TERM count_l, count_h, count_t;
    GLsizei count_tmp;
    count_l = argv[2];
    while(enif_get_list_cell(env, count_l, &count_h, &count_t)) {
        if(!enif_get_int(env, count_h, &count_tmp)) Badarg(5359,"count");
        count_vec.push_back(count_tmp);
        count_l = count_t;
    };
    count = count_vec.data();
    count_len = count_vec.size();
  }
 if (count_len != first_len)  Badarg(5359, "first");
 drawcount = (GLsizei) count_len;
  weglMultiDrawArrays(mode,first,count,drawcount);
}

void ecb_glPointParameterf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5361,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5361,"param");
  weglPointParameterf(pname,param);
}

void ecb_glPointParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5362,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5362,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5362,"params");
   }};
  weglPointParameterfv(pname,params);
}

void ecb_glPointParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5363,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5363,"param");
  weglPointParameteri(pname,param);
}

void ecb_glPointParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5364,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5364,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5364,"params");
   }};
  weglPointParameteriv(pname,params);
}

void ecb_glFogCoordf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat coord;
  if(!egl_get_float(env, argv[0],  &coord)) Badarg(5365,"coord");
  weglFogCoordf(coord);
}

void ecb_glFogCoordd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble coord;
  if(!enif_get_double(env, argv[0],  &coord)) Badarg(5366,"coord");
  weglFogCoordd(coord);
}

void ecb_glFogCoordPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum type;
  GLsizei stride;
  ErlNifBinary pointer;
  void *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5367,"type");
  if(!enif_get_int(env, argv[1],  &stride)) Badarg(5367,"stride");
  if(!egl_get_ptr(env, argv[2], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &pointer))
        pointer_idx = (void *) pointer.data;
    else Badarg(5367,"pointer");
  }
  weglFogCoordPointer(type,stride,pointer_idx);
}

void ecb_glSecondaryColor3b(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbyte red;
  GLbyte green;
  GLbyte blue;
  if(!egl_get_byte(env, argv[0],  &red)) Badarg(5369,"red");
  if(!egl_get_byte(env, argv[1],  &green)) Badarg(5369,"green");
  if(!egl_get_byte(env, argv[2],  &blue)) Badarg(5369,"blue");
  weglSecondaryColor3b(red,green,blue);
}

void ecb_glSecondaryColor3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble red;
  GLdouble green;
  GLdouble blue;
  if(!enif_get_double(env, argv[0],  &red)) Badarg(5370,"red");
  if(!enif_get_double(env, argv[1],  &green)) Badarg(5370,"green");
  if(!enif_get_double(env, argv[2],  &blue)) Badarg(5370,"blue");
  weglSecondaryColor3d(red,green,blue);
}

void ecb_glSecondaryColor3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5371,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5371,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5371,"blue");
  weglSecondaryColor3f(red,green,blue);
}

void ecb_glSecondaryColor3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint red;
  GLint green;
  GLint blue;
  if(!enif_get_int(env, argv[0],  &red)) Badarg(5372,"red");
  if(!enif_get_int(env, argv[1],  &green)) Badarg(5372,"green");
  if(!enif_get_int(env, argv[2],  &blue)) Badarg(5372,"blue");
  weglSecondaryColor3i(red,green,blue);
}

void ecb_glSecondaryColor3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort red;
  GLshort green;
  GLshort blue;
  if(!egl_get_short(env, argv[0],  &red)) Badarg(5373,"red");
  if(!egl_get_short(env, argv[1],  &green)) Badarg(5373,"green");
  if(!egl_get_short(env, argv[2],  &blue)) Badarg(5373,"blue");
  weglSecondaryColor3s(red,green,blue);
}

void ecb_glSecondaryColor3ub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLubyte red;
  GLubyte green;
  GLubyte blue;
  if(!egl_get_ubyte(env, argv[0],  &red)) Badarg(5374,"red");
  if(!egl_get_ubyte(env, argv[1],  &green)) Badarg(5374,"green");
  if(!egl_get_ubyte(env, argv[2],  &blue)) Badarg(5374,"blue");
  weglSecondaryColor3ub(red,green,blue);
}

void ecb_glSecondaryColor3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint red;
  GLuint green;
  GLuint blue;
  if(!enif_get_uint(env, argv[0],  &red)) Badarg(5375,"red");
  if(!enif_get_uint(env, argv[1],  &green)) Badarg(5375,"green");
  if(!enif_get_uint(env, argv[2],  &blue)) Badarg(5375,"blue");
  weglSecondaryColor3ui(red,green,blue);
}

void ecb_glSecondaryColor3us(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLushort red;
  GLushort green;
  GLushort blue;
  if(!egl_get_ushort(env, argv[0],  &red)) Badarg(5376,"red");
  if(!egl_get_ushort(env, argv[1],  &green)) Badarg(5376,"green");
  if(!egl_get_ushort(env, argv[2],  &blue)) Badarg(5376,"blue");
  weglSecondaryColor3us(red,green,blue);
}

void ecb_glSecondaryColorPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary pointer;
  void *pointer_idx;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5377,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5377,"type");
  if(!enif_get_int(env, argv[2],  &stride)) Badarg(5377,"stride");
  if(!egl_get_ptr(env, argv[3], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &pointer))
        pointer_idx = (void *) pointer.data;
    else Badarg(5377,"pointer");
  }
  weglSecondaryColorPointer(size,type,stride,pointer_idx);
}

void ecb_glWindowPos2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5379,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5379,"y");
  weglWindowPos2d(x,y);
}

void ecb_glWindowPos2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5380,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5380,"y");
  weglWindowPos2f(x,y);
}

void ecb_glWindowPos2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5381,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5381,"y");
  weglWindowPos2i(x,y);
}

void ecb_glWindowPos2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5382,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5382,"y");
  weglWindowPos2s(x,y);
}

void ecb_glWindowPos3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5383,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5383,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5383,"z");
  weglWindowPos3d(x,y,z);
}

void ecb_glWindowPos3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5384,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5384,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5384,"z");
  weglWindowPos3f(x,y,z);
}

void ecb_glWindowPos3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5385,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5385,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5385,"z");
  weglWindowPos3i(x,y,z);
}

void ecb_glWindowPos3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5386,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5386,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5386,"z");
  weglWindowPos3s(x,y,z);
}

void ecb_glBlendColor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf red;
  GLclampf green;
  GLclampf blue;
  GLclampf alpha;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5387,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5387,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5387,"blue");
  if(!egl_get_float(env, argv[3],  &alpha)) Badarg(5387,"alpha");
  weglBlendColor(red,green,blue,alpha);
}

void ecb_glBlendEquation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5388,"mode");
  weglBlendEquation(mode);
}

void ecb_glGenQueries(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5389,"n");
  std::vector <GLuint> ids (n);
  std::vector <ERL_NIF_TERM> ids_ts (n);
  weglGenQueries(n,ids.data());
  for(int ri=0; ri < (int) n; ri++)
    ids_ts[ri] =      enif_make_int(env, ids[ri]);
  reply =      enif_make_list_from_array(env, ids_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDeleteQueries(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *ids;
  std::vector <GLuint> ids_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5390,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5390, "ids")
  else {
    ERL_NIF_TERM ids_l, ids_h, ids_t;
    GLuint ids_tmp;
    ids_l = argv[1];
    while(enif_get_list_cell(env, ids_l, &ids_h, &ids_t)) {
        if(!enif_get_uint(env, ids_h, &ids_tmp)) Badarg(5390,"ids");
        ids_vec.push_back(ids_tmp);
        ids_l = ids_t;
    };
    ids = ids_vec.data();
  }
  weglDeleteQueries(n,ids);
}

void ecb_glIsQuery(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5391,"id");
  result = weglIsQuery(id);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBeginQuery(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5392,"target");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5392,"id");
  weglBeginQuery(target,id);
}

void ecb_glEndQuery(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5393,"target");
  weglEndQuery(target);
}

void ecb_glGetQueryiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5394,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5394,"pname");
  weglGetQueryiv(target,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetQueryObjectiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint id;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5395,"id");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5395,"pname");
  weglGetQueryObjectiv(id,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetQueryObjectuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint id;
  GLenum pname;
  GLuint params;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5396,"id");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5396,"pname");
  weglGetQueryObjectuiv(id,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBindBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5397,"target");
  if(!enif_get_uint(env, argv[1],  &buffer)) Badarg(5397,"buffer");
  weglBindBuffer(target,buffer);
}

void ecb_glDeleteBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *buffers;
  std::vector <GLuint> buffers_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5398,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5398, "buffers")
  else {
    ERL_NIF_TERM buffers_l, buffers_h, buffers_t;
    GLuint buffers_tmp;
    buffers_l = argv[1];
    while(enif_get_list_cell(env, buffers_l, &buffers_h, &buffers_t)) {
        if(!enif_get_uint(env, buffers_h, &buffers_tmp)) Badarg(5398,"buffers");
        buffers_vec.push_back(buffers_tmp);
        buffers_l = buffers_t;
    };
    buffers = buffers_vec.data();
  }
  weglDeleteBuffers(n,buffers);
}

void ecb_glGenBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5399,"n");
  std::vector <GLuint> buffers (n);
  std::vector <ERL_NIF_TERM> buffers_ts (n);
  weglGenBuffers(n,buffers.data());
  for(int ri=0; ri < (int) n; ri++)
    buffers_ts[ri] =      enif_make_int(env, buffers[ri]);
  reply =      enif_make_list_from_array(env, buffers_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5400,"buffer");
  result = weglIsBuffer(buffer);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBufferData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizeiptr size;
  ErlNifBinary data;
  void *data_idx;
  GLenum usage;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5401,"target");
  if(!egl_get_word(env, argv[1], (egl_word *) &size)) Badarg(5401,"size");
  if(!egl_get_ptr(env, argv[2], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &data))
        data_idx = (void *) data.data;
    else Badarg(5401,"data");
  }
  if(!enif_get_uint(env, argv[3],  &usage)) Badarg(5401,"usage");
  weglBufferData(target,size,data_idx,usage);
}

void ecb_glBufferSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLintptr offset;
  GLsizeiptr size;
  ErlNifBinary data;
  void *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5403,"target");
  if(!egl_get_word(env, argv[1], (egl_word *) &offset)) Badarg(5403,"offset");
  if(!egl_get_word(env, argv[2], (egl_word *) &size)) Badarg(5403,"size");
  if(!egl_get_ptr(env, argv[3], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &data))
        data_idx = (void *) data.data;
    else Badarg(5403,"data");
  }
  weglBufferSubData(target,offset,size,data_idx);
}

void ecb_glGetBufferSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLintptr offset;
  GLsizeiptr size;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5405,"target");
  if(!egl_get_word(env, argv[1], (egl_word *) &offset)) Badarg(5405,"offset");
  if(!egl_get_word(env, argv[2], (egl_word *) &size)) Badarg(5405,"size");
  if(enif_is_binary(env, argv[3]))
    enif_inspect_binary(env, argv[3], &data);
  else if(enif_is_tuple(env, argv[3])) {
    int data_a;
    const ERL_NIF_TERM *data_t;
    if(enif_get_tuple(env, argv[3], &data_a, &data_t) &&
         enif_is_binary(env, data_t[1]))
       enif_inspect_binary(env, data_t[1], &data);
    else Badarg(5405, "data");
  } else Badarg(5405, "data");
  weglGetBufferSubData(target,offset,size,(void *) data.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetBufferParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5406,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5406,"pname");
  weglGetBufferParameteriv(target,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBlendEquationSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum modeRGB;
  GLenum modeAlpha;
  if(!enif_get_uint(env, argv[0],  &modeRGB)) Badarg(5407,"modeRGB");
  if(!enif_get_uint(env, argv[1],  &modeAlpha)) Badarg(5407,"modeAlpha");
  weglBlendEquationSeparate(modeRGB,modeAlpha);
}

void ecb_glDrawBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLenum *bufs;
  std::vector <GLenum> bufs_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5408,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5408, "bufs")
  else {
    ERL_NIF_TERM bufs_l, bufs_h, bufs_t;
    GLenum bufs_tmp;
    bufs_l = argv[1];
    while(enif_get_list_cell(env, bufs_l, &bufs_h, &bufs_t)) {
        if(!enif_get_uint(env, bufs_h, &bufs_tmp)) Badarg(5408,"bufs");
        bufs_vec.push_back(bufs_tmp);
        bufs_l = bufs_t;
    };
    bufs = bufs_vec.data();
  }
  weglDrawBuffers(n,bufs);
}

void ecb_glStencilOpSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum sfail;
  GLenum dpfail;
  GLenum dppass;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5409,"face");
  if(!enif_get_uint(env, argv[1],  &sfail)) Badarg(5409,"sfail");
  if(!enif_get_uint(env, argv[2],  &dpfail)) Badarg(5409,"dpfail");
  if(!enif_get_uint(env, argv[3],  &dppass)) Badarg(5409,"dppass");
  weglStencilOpSeparate(face,sfail,dpfail,dppass);
}

void ecb_glStencilFuncSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum func;
  GLint ref;
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5410,"face");
  if(!enif_get_uint(env, argv[1],  &func)) Badarg(5410,"func");
  if(!enif_get_int(env, argv[2],  &ref)) Badarg(5410,"ref");
  if(!enif_get_uint(env, argv[3],  &mask)) Badarg(5410,"mask");
  weglStencilFuncSeparate(face,func,ref,mask);
}

void ecb_glStencilMaskSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5411,"face");
  if(!enif_get_uint(env, argv[1],  &mask)) Badarg(5411,"mask");
  weglStencilMaskSeparate(face,mask);
}

void ecb_glAttachShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5412,"program");
  if(!enif_get_uint(env, argv[1],  &shader)) Badarg(5412,"shader");
  weglAttachShader(program,shader);
}

void ecb_glBindAttribLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint index;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5413,"program");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5413,"index");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5413,"name");
  weglBindAttribLocation(program,index,(GLchar *) name.data);
}

void ecb_glCompileShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5414,"shader");
  weglCompileShader(shader);
}

void ecb_glCreateProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  result = weglCreateProgram();
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glCreateShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLenum type;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5416,"type");
  result = weglCreateShader(type);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDeleteProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5417,"program");
  weglDeleteProgram(program);
}

void ecb_glDeleteShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5418,"shader");
  weglDeleteShader(shader);
}

void ecb_glDetachShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5419,"program");
  if(!enif_get_uint(env, argv[1],  &shader)) Badarg(5419,"shader");
  weglDetachShader(program,shader);
}

void ecb_glDisableVertexAttribArray(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5420,"index");
  weglDisableVertexAttribArray(index);
}

void ecb_glEnableVertexAttribArray(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5421,"index");
  weglEnableVertexAttribArray(index);
}

void ecb_glGetActiveAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint index;
  GLsizei bufSize;
  GLsizei length;
  GLint size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5422,"program");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5422,"index");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5422,"bufSize");
  name = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetActiveAttrib(program,index,bufSize,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

void ecb_glGetActiveUniform(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint index;
  GLsizei bufSize;
  GLsizei length;
  GLint size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5423,"program");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5423,"index");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5423,"bufSize");
  name = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetActiveUniform(program,index,bufSize,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

void ecb_glGetAttachedShaders(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei maxCount;
  GLsizei count;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5424,"program");
  if(!enif_get_int(env, argv[1],  &maxCount)) Badarg(5424,"maxCount");
  std::vector <GLuint> shaders (maxCount);
  std::vector <ERL_NIF_TERM> shaders_ts (maxCount);
  weglGetAttachedShaders(program,maxCount,&count,shaders.data());
  for(int ri=0; ri < (int) count; ri++)
    shaders_ts[ri] =      enif_make_int(env, shaders[ri]);
  reply =      enif_make_list_from_array(env, shaders_ts.data(), count);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetAttribLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5425,"program");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5425,"name");
  result = weglGetAttribLocation(program,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5426,"program");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5426,"pname");
  weglGetProgramiv(program,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramInfoLog(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *infoLog;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5427,"program");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5427,"bufSize");
  infoLog = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetProgramInfoLog(program,bufSize,&length,(GLchar *) infoLog);
  reply =      enif_make_string(env, (const char *) infoLog, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(infoLog);
}

void ecb_glGetShaderiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint shader;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5428,"shader");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5428,"pname");
  weglGetShaderiv(shader,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetShaderInfoLog(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint shader;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *infoLog;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5429,"shader");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5429,"bufSize");
  infoLog = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetShaderInfoLog(shader,bufSize,&length,(GLchar *) infoLog);
  reply =      enif_make_string(env, (const char *) infoLog, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(infoLog);
}

void ecb_glGetShaderSource(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint shader;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *source;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5430,"shader");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5430,"bufSize");
  source = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetShaderSource(shader,bufSize,&length,(GLchar *) source);
  reply =      enif_make_string(env, (const char *) source, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(source);
}

void ecb_glGetUniformLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5431,"program");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5431,"name");
  result = weglGetUniformLocation(program,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetUniformfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLfloat params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5432,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5432,"location");
  weglGetUniformfv(program,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_double(env, (double) params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetUniformiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5433,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5433,"location");
  weglGetUniformiv(program,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetVertexAttribdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5434,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5434,"pname");
  weglGetVertexAttribdv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetVertexAttribfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5435,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5435,"pname");
  weglGetVertexAttribfv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetVertexAttribiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5436,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5436,"pname");
  weglGetVertexAttribiv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5437,"program");
  result = weglIsProgram(program);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5438,"shader");
  result = weglIsShader(shader);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glLinkProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5439,"program");
  weglLinkProgram(program);
}

void ecb_glShaderSource(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint shader;
  GLsizei count;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5440,"shader");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5440,"count");
  ERL_NIF_TERM string_l, string_h, string_t;
  ErlNifBinary string_tmp;
  std::vector <GLchar *> string;
  string_l = argv[2];
  while(enif_get_list_cell(env, string_l, &string_h, &string_t)) {
    if(!enif_inspect_binary(env, string_h, &string_tmp)) Badarg(5440,"string");
    string.push_back((GLchar *) string_tmp.data);
    string_l = string_t;
  }
  weglShaderSource(shader,count,(const GLchar **) string.data(),NULL);
}

void ecb_glUseProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5441,"program");
  weglUseProgram(program);
}

void ecb_glUniform1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLfloat v0;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5442,"location");
  if(!egl_get_float(env, argv[1],  &v0)) Badarg(5442,"v0");
  weglUniform1f(location,v0);
}

void ecb_glUniform2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLfloat v0;
  GLfloat v1;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5443,"location");
  if(!egl_get_float(env, argv[1],  &v0)) Badarg(5443,"v0");
  if(!egl_get_float(env, argv[2],  &v1)) Badarg(5443,"v1");
  weglUniform2f(location,v0,v1);
}

void ecb_glUniform3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLfloat v0;
  GLfloat v1;
  GLfloat v2;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5444,"location");
  if(!egl_get_float(env, argv[1],  &v0)) Badarg(5444,"v0");
  if(!egl_get_float(env, argv[2],  &v1)) Badarg(5444,"v1");
  if(!egl_get_float(env, argv[3],  &v2)) Badarg(5444,"v2");
  weglUniform3f(location,v0,v1,v2);
}

void ecb_glUniform4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLfloat v0;
  GLfloat v1;
  GLfloat v2;
  GLfloat v3;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5445,"location");
  if(!egl_get_float(env, argv[1],  &v0)) Badarg(5445,"v0");
  if(!egl_get_float(env, argv[2],  &v1)) Badarg(5445,"v1");
  if(!egl_get_float(env, argv[3],  &v2)) Badarg(5445,"v2");
  if(!egl_get_float(env, argv[4],  &v3)) Badarg(5445,"v3");
  weglUniform4f(location,v0,v1,v2,v3);
}

void ecb_glUniform1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint v0;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5446,"location");
  if(!enif_get_int(env, argv[1],  &v0)) Badarg(5446,"v0");
  weglUniform1i(location,v0);
}

void ecb_glUniform2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint v0;
  GLint v1;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5447,"location");
  if(!enif_get_int(env, argv[1],  &v0)) Badarg(5447,"v0");
  if(!enif_get_int(env, argv[2],  &v1)) Badarg(5447,"v1");
  weglUniform2i(location,v0,v1);
}

void ecb_glUniform3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint v0;
  GLint v1;
  GLint v2;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5448,"location");
  if(!enif_get_int(env, argv[1],  &v0)) Badarg(5448,"v0");
  if(!enif_get_int(env, argv[2],  &v1)) Badarg(5448,"v1");
  if(!enif_get_int(env, argv[3],  &v2)) Badarg(5448,"v2");
  weglUniform3i(location,v0,v1,v2);
}

void ecb_glUniform4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint v0;
  GLint v1;
  GLint v2;
  GLint v3;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5449,"location");
  if(!enif_get_int(env, argv[1],  &v0)) Badarg(5449,"v0");
  if(!enif_get_int(env, argv[2],  &v1)) Badarg(5449,"v1");
  if(!enif_get_int(env, argv[3],  &v2)) Badarg(5449,"v2");
  if(!enif_get_int(env, argv[4],  &v3)) Badarg(5449,"v3");
  weglUniform4i(location,v0,v1,v2,v3);
}

void ecb_glUniform1fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLfloat *value;
  std::vector <GLfloat> value_vec;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5450,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5450,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5450, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLfloat value_tmp;
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!egl_get_float(env, value_h, &value_tmp)) Badarg(5450,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglUniform1fv(location,count,value);
}

void ecb_glUniform2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5451,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5451,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5451,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (2*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5451,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5451,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5451,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform2fv(location,count,value);
}

void ecb_glUniform3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5452,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5452,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5452,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (3*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5452,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5452,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5452,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5452,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform3fv(location,count,value);
}

void ecb_glUniform4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5453,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5453,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5453,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (4*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5453,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5453,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5453,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5453,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5453,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform4fv(location,count,value);
}

void ecb_glUniform1iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint *value;
  std::vector <GLint> value_vec;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5454,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5454,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5454, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLint value_tmp;
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_int(env, value_h, &value_tmp)) Badarg(5454,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglUniform1iv(location,count,value);
}

void ecb_glUniform2iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5455,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5455,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5455,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint> value_vec (2*count);
  GLint *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5455,"value");
      if(!enif_get_int(env, value_tpl[0], value_ptr++)) Badarg(5455,"value");
      if(!enif_get_int(env, value_tpl[1], value_ptr++)) Badarg(5455,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform2iv(location,count,value);
}

void ecb_glUniform3iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5456,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5456,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5456,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint> value_vec (3*count);
  GLint *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5456,"value");
      if(!enif_get_int(env, value_tpl[0], value_ptr++)) Badarg(5456,"value");
      if(!enif_get_int(env, value_tpl[1], value_ptr++)) Badarg(5456,"value");
      if(!enif_get_int(env, value_tpl[2], value_ptr++)) Badarg(5456,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform3iv(location,count,value);
}

void ecb_glUniform4iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5457,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5457,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5457,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint> value_vec (4*count);
  GLint *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5457,"value");
      if(!enif_get_int(env, value_tpl[0], value_ptr++)) Badarg(5457,"value");
      if(!enif_get_int(env, value_tpl[1], value_ptr++)) Badarg(5457,"value");
      if(!enif_get_int(env, value_tpl[2], value_ptr++)) Badarg(5457,"value");
      if(!enif_get_int(env, value_tpl[3], value_ptr++)) Badarg(5457,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform4iv(location,count,value);
}

void ecb_glUniformMatrix2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5458,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5458,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5458,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5458,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (4*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5458,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5458,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5458,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5458,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5458,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix2fv(location,count,transpose,value);
}

void ecb_glUniformMatrix3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5459,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5459,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5459,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5459,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (9*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 9) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5459,"value");
      if(!egl_get_float(env, value_tpl[8], value_ptr++)) Badarg(5459,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix3fv(location,count,transpose,value);
}

void ecb_glUniformMatrix4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5460,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5460,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5460,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5460,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (16*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 16) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[8], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[9], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[10], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[11], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[12], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[13], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[14], value_ptr++)) Badarg(5460,"value");
      if(!egl_get_float(env, value_tpl[15], value_ptr++)) Badarg(5460,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix4fv(location,count,transpose,value);
}

void ecb_glValidateProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5461,"program");
  weglValidateProgram(program);
}

void ecb_glVertexAttrib1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5462,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5462,"x");
  weglVertexAttrib1d(index,x);
}

void ecb_glVertexAttrib1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5463,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5463,"x");
  weglVertexAttrib1f(index,x);
}

void ecb_glVertexAttrib1s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5464,"index");
  if(!egl_get_short(env, argv[1],  &x)) Badarg(5464,"x");
  weglVertexAttrib1s(index,x);
}

void ecb_glVertexAttrib2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5465,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5465,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5465,"y");
  weglVertexAttrib2d(index,x,y);
}

void ecb_glVertexAttrib2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  GLfloat y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5466,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5466,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5466,"y");
  weglVertexAttrib2f(index,x,y);
}

void ecb_glVertexAttrib2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort x;
  GLshort y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5467,"index");
  if(!egl_get_short(env, argv[1],  &x)) Badarg(5467,"x");
  if(!egl_get_short(env, argv[2],  &y)) Badarg(5467,"y");
  weglVertexAttrib2s(index,x,y);
}

void ecb_glVertexAttrib3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5468,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5468,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5468,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5468,"z");
  weglVertexAttrib3d(index,x,y,z);
}

void ecb_glVertexAttrib3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5469,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5469,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5469,"y");
  if(!egl_get_float(env, argv[3],  &z)) Badarg(5469,"z");
  weglVertexAttrib3f(index,x,y,z);
}

void ecb_glVertexAttrib3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort x;
  GLshort y;
  GLshort z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5470,"index");
  if(!egl_get_short(env, argv[1],  &x)) Badarg(5470,"x");
  if(!egl_get_short(env, argv[2],  &y)) Badarg(5470,"y");
  if(!egl_get_short(env, argv[3],  &z)) Badarg(5470,"z");
  weglVertexAttrib3s(index,x,y,z);
}

void ecb_glVertexAttrib4Nbv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLbyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5471,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5471,"v");
   } else {
    int i1 = 0;
     if(!egl_get_byte(env, v_t[i1++], &v[0])) Badarg(5471,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[1])) Badarg(5471,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[2])) Badarg(5471,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[3])) Badarg(5471,"v");
   }};
  weglVertexAttrib4Nbv(index,v);
}

void ecb_glVertexAttrib4Niv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5472,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5472,"v");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v_t[i1++], &v[0])) Badarg(5472,"v");
     if(!enif_get_int(env, v_t[i1++], &v[1])) Badarg(5472,"v");
     if(!enif_get_int(env, v_t[i1++], &v[2])) Badarg(5472,"v");
     if(!enif_get_int(env, v_t[i1++], &v[3])) Badarg(5472,"v");
   }};
  weglVertexAttrib4Niv(index,v);
}

void ecb_glVertexAttrib4Nsv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5473,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5473,"v");
   } else {
    int i1 = 0;
     if(!egl_get_short(env, v_t[i1++], &v[0])) Badarg(5473,"v");
     if(!egl_get_short(env, v_t[i1++], &v[1])) Badarg(5473,"v");
     if(!egl_get_short(env, v_t[i1++], &v[2])) Badarg(5473,"v");
     if(!egl_get_short(env, v_t[i1++], &v[3])) Badarg(5473,"v");
   }};
  weglVertexAttrib4Nsv(index,v);
}

void ecb_glVertexAttrib4Nub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLubyte x;
  GLubyte y;
  GLubyte z;
  GLubyte w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5474,"index");
  if(!egl_get_ubyte(env, argv[1],  &x)) Badarg(5474,"x");
  if(!egl_get_ubyte(env, argv[2],  &y)) Badarg(5474,"y");
  if(!egl_get_ubyte(env, argv[3],  &z)) Badarg(5474,"z");
  if(!egl_get_ubyte(env, argv[4],  &w)) Badarg(5474,"w");
  weglVertexAttrib4Nub(index,x,y,z,w);
}

void ecb_glVertexAttrib4Nuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5475,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5475,"v");
   } else {
    int i1 = 0;
     if(!enif_get_uint(env, v_t[i1++], &v[0])) Badarg(5475,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[1])) Badarg(5475,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[2])) Badarg(5475,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[3])) Badarg(5475,"v");
   }};
  weglVertexAttrib4Nuiv(index,v);
}

void ecb_glVertexAttrib4Nusv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLushort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5476,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5476,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ushort(env, v_t[i1++], &v[0])) Badarg(5476,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[1])) Badarg(5476,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[2])) Badarg(5476,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[3])) Badarg(5476,"v");
   }};
  weglVertexAttrib4Nusv(index,v);
}

void ecb_glVertexAttrib4bv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLbyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5477,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5477,"v");
   } else {
    int i1 = 0;
     if(!egl_get_byte(env, v_t[i1++], &v[0])) Badarg(5477,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[1])) Badarg(5477,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[2])) Badarg(5477,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[3])) Badarg(5477,"v");
   }};
  weglVertexAttrib4bv(index,v);
}

void ecb_glVertexAttrib4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5478,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5478,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5478,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5478,"z");
  if(!enif_get_double(env, argv[4],  &w)) Badarg(5478,"w");
  weglVertexAttrib4d(index,x,y,z,w);
}

void ecb_glVertexAttrib4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5479,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5479,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5479,"y");
  if(!egl_get_float(env, argv[3],  &z)) Badarg(5479,"z");
  if(!egl_get_float(env, argv[4],  &w)) Badarg(5479,"w");
  weglVertexAttrib4f(index,x,y,z,w);
}

void ecb_glVertexAttrib4iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5480,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5480,"v");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v_t[i1++], &v[0])) Badarg(5480,"v");
     if(!enif_get_int(env, v_t[i1++], &v[1])) Badarg(5480,"v");
     if(!enif_get_int(env, v_t[i1++], &v[2])) Badarg(5480,"v");
     if(!enif_get_int(env, v_t[i1++], &v[3])) Badarg(5480,"v");
   }};
  weglVertexAttrib4iv(index,v);
}

void ecb_glVertexAttrib4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort x;
  GLshort y;
  GLshort z;
  GLshort w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5481,"index");
  if(!egl_get_short(env, argv[1],  &x)) Badarg(5481,"x");
  if(!egl_get_short(env, argv[2],  &y)) Badarg(5481,"y");
  if(!egl_get_short(env, argv[3],  &z)) Badarg(5481,"z");
  if(!egl_get_short(env, argv[4],  &w)) Badarg(5481,"w");
  weglVertexAttrib4s(index,x,y,z,w);
}

void ecb_glVertexAttrib4ubv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLubyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5482,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5482,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ubyte(env, v_t[i1++], &v[0])) Badarg(5482,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[1])) Badarg(5482,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[2])) Badarg(5482,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[3])) Badarg(5482,"v");
   }};
  weglVertexAttrib4ubv(index,v);
}

void ecb_glVertexAttrib4uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5483,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5483,"v");
   } else {
    int i1 = 0;
     if(!enif_get_uint(env, v_t[i1++], &v[0])) Badarg(5483,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[1])) Badarg(5483,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[2])) Badarg(5483,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[3])) Badarg(5483,"v");
   }};
  weglVertexAttrib4uiv(index,v);
}

void ecb_glVertexAttrib4usv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLushort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5484,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5484,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ushort(env, v_t[i1++], &v[0])) Badarg(5484,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[1])) Badarg(5484,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[2])) Badarg(5484,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[3])) Badarg(5484,"v");
   }};
  weglVertexAttrib4usv(index,v);
}

void ecb_glVertexAttribPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint size;
  GLenum type;
  GLboolean normalized;
  GLsizei stride;
  ErlNifBinary pointer;
  void *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5485,"index");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5485,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5485,"type");
  if(!egl_get_ubyte(env, argv[3],  &normalized)) Badarg(5485,"normalized");
  if(!enif_get_int(env, argv[4],  &stride)) Badarg(5485,"stride");
  if(!egl_get_ptr(env, argv[5], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &pointer))
        pointer_idx = (void *) pointer.data;
    else Badarg(5485,"pointer");
  }
  weglVertexAttribPointer(index,size,type,normalized,stride,pointer_idx);
}

void ecb_glUniformMatrix2x3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5487,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5487,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5487,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5487,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (6*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) Badarg(5487,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5487,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5487,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5487,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5487,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5487,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5487,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix2x3fv(location,count,transpose,value);
}

void ecb_glUniformMatrix3x2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5488,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5488,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5488,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5488,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (6*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) Badarg(5488,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5488,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5488,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5488,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5488,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5488,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5488,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix3x2fv(location,count,transpose,value);
}

void ecb_glUniformMatrix2x4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5489,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5489,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5489,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5489,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (8*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) Badarg(5489,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5489,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5489,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5489,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5489,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5489,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5489,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5489,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5489,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix2x4fv(location,count,transpose,value);
}

void ecb_glUniformMatrix4x2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5490,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5490,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5490,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5490,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (8*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) Badarg(5490,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5490,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5490,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5490,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5490,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5490,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5490,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5490,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5490,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix4x2fv(location,count,transpose,value);
}

void ecb_glUniformMatrix3x4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5491,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5491,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5491,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5491,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (12*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[8], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[9], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[10], value_ptr++)) Badarg(5491,"value");
      if(!egl_get_float(env, value_tpl[11], value_ptr++)) Badarg(5491,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix3x4fv(location,count,transpose,value);
}

void ecb_glUniformMatrix4x3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5492,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5492,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5492,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5492,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (12*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[8], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[9], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[10], value_ptr++)) Badarg(5492,"value");
      if(!egl_get_float(env, value_tpl[11], value_ptr++)) Badarg(5492,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix4x3fv(location,count,transpose,value);
}

void ecb_glColorMaski(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLboolean r;
  GLboolean g;
  GLboolean b;
  GLboolean a;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5493,"index");
  if(!egl_get_ubyte(env, argv[1],  &r)) Badarg(5493,"r");
  if(!egl_get_ubyte(env, argv[2],  &g)) Badarg(5493,"g");
  if(!egl_get_ubyte(env, argv[3],  &b)) Badarg(5493,"b");
  if(!egl_get_ubyte(env, argv[4],  &a)) Badarg(5493,"a");
  weglColorMaski(index,r,g,b,a);
}

void ecb_glGetBooleani_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLboolean data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5494,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5494,"index");
  weglGetBooleani_v(target,index,data);
  for(int ri=0; ri < (int) 16; ri++)
     data_ts[ri] =      enif_make_int(env, data[ri]);
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetIntegeri_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLint data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5495,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5495,"index");
  weglGetIntegeri_v(target,index,data);
  for(int ri=0; ri < (int) 16; ri++)
     data_ts[ri] =      enif_make_int(env, data[ri]);
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glEnablei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5496,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5496,"index");
  weglEnablei(target,index);
}

void ecb_glDisablei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5497,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5497,"index");
  weglDisablei(target,index);
}

void ecb_glIsEnabledi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5498,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5498,"index");
  result = weglIsEnabledi(target,index);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBeginTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum primitiveMode;
  if(!enif_get_uint(env, argv[0],  &primitiveMode)) Badarg(5499,"primitiveMode");
  weglBeginTransformFeedback(primitiveMode);
}

void ecb_glEndTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEndTransformFeedback();
}

void ecb_glBindBufferRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLuint buffer;
  GLintptr offset;
  GLsizeiptr size;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5501,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5501,"index");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5501,"buffer");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5501,"offset");
  if(!egl_get_word(env, argv[4], (egl_word *) &size)) Badarg(5501,"size");
  weglBindBufferRange(target,index,buffer,offset,size);
}

void ecb_glBindBufferBase(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5502,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5502,"index");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5502,"buffer");
  weglBindBufferBase(target,index,buffer);
}

void ecb_glTransformFeedbackVaryings(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLsizei count;
  GLenum bufferMode;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5503,"program");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5503,"count");
  ERL_NIF_TERM varyings_l, varyings_h, varyings_t;
  ErlNifBinary varyings_tmp;
  std::vector <GLchar *> varyings;
  varyings_l = argv[2];
  while(enif_get_list_cell(env, varyings_l, &varyings_h, &varyings_t)) {
    if(!enif_inspect_binary(env, varyings_h, &varyings_tmp)) Badarg(5503,"varyings");
    varyings.push_back((GLchar *) varyings_tmp.data);
    varyings_l = varyings_t;
  }
  if(!enif_get_uint(env, argv[3],  &bufferMode)) Badarg(5503,"bufferMode");
  weglTransformFeedbackVaryings(program,count,(const GLchar **) varyings.data(),bufferMode);
}

void ecb_glGetTransformFeedbackVarying(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint index;
  GLsizei bufSize;
  GLsizei length;
  GLsizei size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5504,"program");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5504,"index");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5504,"bufSize");
  name = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetTransformFeedbackVarying(program,index,bufSize,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

void ecb_glClampColor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum clamp;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5505,"target");
  if(!enif_get_uint(env, argv[1],  &clamp)) Badarg(5505,"clamp");
  weglClampColor(target,clamp);
}

void ecb_glBeginConditionalRender(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint id;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5506,"id");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5506,"mode");
  weglBeginConditionalRender(id,mode);
}

void ecb_glEndConditionalRender(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEndConditionalRender();
}

void ecb_glVertexAttribIPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary pointer;
  void *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5508,"index");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5508,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5508,"type");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5508,"stride");
  if(!egl_get_ptr(env, argv[4], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[4], &pointer))
        pointer_idx = (void *) pointer.data;
    else Badarg(5508,"pointer");
  }
  weglVertexAttribIPointer(index,size,type,stride,pointer_idx);
}

void ecb_glGetVertexAttribIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5510,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5510,"pname");
  weglGetVertexAttribIiv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetVertexAttribIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLuint params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5511,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5511,"pname");
  weglGetVertexAttribIuiv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glVertexAttribI1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5512,"index");
  if(!enif_get_int(env, argv[1],  &x)) Badarg(5512,"x");
  weglVertexAttribI1i(index,x);
}

void ecb_glVertexAttribI2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint x;
  GLint y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5513,"index");
  if(!enif_get_int(env, argv[1],  &x)) Badarg(5513,"x");
  if(!enif_get_int(env, argv[2],  &y)) Badarg(5513,"y");
  weglVertexAttribI2i(index,x,y);
}

void ecb_glVertexAttribI3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint x;
  GLint y;
  GLint z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5514,"index");
  if(!enif_get_int(env, argv[1],  &x)) Badarg(5514,"x");
  if(!enif_get_int(env, argv[2],  &y)) Badarg(5514,"y");
  if(!enif_get_int(env, argv[3],  &z)) Badarg(5514,"z");
  weglVertexAttribI3i(index,x,y,z);
}

void ecb_glVertexAttribI4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint x;
  GLint y;
  GLint z;
  GLint w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5515,"index");
  if(!enif_get_int(env, argv[1],  &x)) Badarg(5515,"x");
  if(!enif_get_int(env, argv[2],  &y)) Badarg(5515,"y");
  if(!enif_get_int(env, argv[3],  &z)) Badarg(5515,"z");
  if(!enif_get_int(env, argv[4],  &w)) Badarg(5515,"w");
  weglVertexAttribI4i(index,x,y,z,w);
}

void ecb_glVertexAttribI1ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5516,"index");
  if(!enif_get_uint(env, argv[1],  &x)) Badarg(5516,"x");
  weglVertexAttribI1ui(index,x);
}

void ecb_glVertexAttribI2ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint x;
  GLuint y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5517,"index");
  if(!enif_get_uint(env, argv[1],  &x)) Badarg(5517,"x");
  if(!enif_get_uint(env, argv[2],  &y)) Badarg(5517,"y");
  weglVertexAttribI2ui(index,x,y);
}

void ecb_glVertexAttribI3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint x;
  GLuint y;
  GLuint z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5518,"index");
  if(!enif_get_uint(env, argv[1],  &x)) Badarg(5518,"x");
  if(!enif_get_uint(env, argv[2],  &y)) Badarg(5518,"y");
  if(!enif_get_uint(env, argv[3],  &z)) Badarg(5518,"z");
  weglVertexAttribI3ui(index,x,y,z);
}

void ecb_glVertexAttribI4ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint x;
  GLuint y;
  GLuint z;
  GLuint w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5519,"index");
  if(!enif_get_uint(env, argv[1],  &x)) Badarg(5519,"x");
  if(!enif_get_uint(env, argv[2],  &y)) Badarg(5519,"y");
  if(!enif_get_uint(env, argv[3],  &z)) Badarg(5519,"z");
  if(!enif_get_uint(env, argv[4],  &w)) Badarg(5519,"w");
  weglVertexAttribI4ui(index,x,y,z,w);
}

void ecb_glVertexAttribI4bv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLbyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5520,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5520,"v");
   } else {
    int i1 = 0;
     if(!egl_get_byte(env, v_t[i1++], &v[0])) Badarg(5520,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[1])) Badarg(5520,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[2])) Badarg(5520,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[3])) Badarg(5520,"v");
   }};
  weglVertexAttribI4bv(index,v);
}

void ecb_glVertexAttribI4sv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5521,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5521,"v");
   } else {
    int i1 = 0;
     if(!egl_get_short(env, v_t[i1++], &v[0])) Badarg(5521,"v");
     if(!egl_get_short(env, v_t[i1++], &v[1])) Badarg(5521,"v");
     if(!egl_get_short(env, v_t[i1++], &v[2])) Badarg(5521,"v");
     if(!egl_get_short(env, v_t[i1++], &v[3])) Badarg(5521,"v");
   }};
  weglVertexAttribI4sv(index,v);
}

void ecb_glVertexAttribI4ubv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLubyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5522,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5522,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ubyte(env, v_t[i1++], &v[0])) Badarg(5522,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[1])) Badarg(5522,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[2])) Badarg(5522,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[3])) Badarg(5522,"v");
   }};
  weglVertexAttribI4ubv(index,v);
}

void ecb_glVertexAttribI4usv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLushort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5523,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5523,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ushort(env, v_t[i1++], &v[0])) Badarg(5523,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[1])) Badarg(5523,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[2])) Badarg(5523,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[3])) Badarg(5523,"v");
   }};
  weglVertexAttribI4usv(index,v);
}

void ecb_glGetUniformuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLuint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5524,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5524,"location");
  weglGetUniformuiv(program,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBindFragDataLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint color;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5525,"program");
  if(!enif_get_uint(env, argv[1],  &color)) Badarg(5525,"color");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5525,"name");
  weglBindFragDataLocation(program,color,(GLchar *) name.data);
}

void ecb_glGetFragDataLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5526,"program");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5526,"name");
  result = weglGetFragDataLocation(program,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glUniform1ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint v0;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5527,"location");
  if(!enif_get_uint(env, argv[1],  &v0)) Badarg(5527,"v0");
  weglUniform1ui(location,v0);
}

void ecb_glUniform2ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint v0;
  GLuint v1;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5528,"location");
  if(!enif_get_uint(env, argv[1],  &v0)) Badarg(5528,"v0");
  if(!enif_get_uint(env, argv[2],  &v1)) Badarg(5528,"v1");
  weglUniform2ui(location,v0,v1);
}

void ecb_glUniform3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint v0;
  GLuint v1;
  GLuint v2;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5529,"location");
  if(!enif_get_uint(env, argv[1],  &v0)) Badarg(5529,"v0");
  if(!enif_get_uint(env, argv[2],  &v1)) Badarg(5529,"v1");
  if(!enif_get_uint(env, argv[3],  &v2)) Badarg(5529,"v2");
  weglUniform3ui(location,v0,v1,v2);
}

void ecb_glUniform4ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint v0;
  GLuint v1;
  GLuint v2;
  GLuint v3;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5530,"location");
  if(!enif_get_uint(env, argv[1],  &v0)) Badarg(5530,"v0");
  if(!enif_get_uint(env, argv[2],  &v1)) Badarg(5530,"v1");
  if(!enif_get_uint(env, argv[3],  &v2)) Badarg(5530,"v2");
  if(!enif_get_uint(env, argv[4],  &v3)) Badarg(5530,"v3");
  weglUniform4ui(location,v0,v1,v2,v3);
}

void ecb_glUniform1uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint *value;
  std::vector <GLuint> value_vec;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5531,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5531,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5531, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLuint value_tmp;
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_uint(env, value_h, &value_tmp)) Badarg(5531,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglUniform1uiv(location,count,value);
}

void ecb_glUniform2uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5532,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5532,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5532,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint> value_vec (2*count);
  GLuint *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5532,"value");
      if(!enif_get_uint(env, value_tpl[0], value_ptr++)) Badarg(5532,"value");
      if(!enif_get_uint(env, value_tpl[1], value_ptr++)) Badarg(5532,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform2uiv(location,count,value);
}

void ecb_glUniform3uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5533,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5533,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5533,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint> value_vec (3*count);
  GLuint *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5533,"value");
      if(!enif_get_uint(env, value_tpl[0], value_ptr++)) Badarg(5533,"value");
      if(!enif_get_uint(env, value_tpl[1], value_ptr++)) Badarg(5533,"value");
      if(!enif_get_uint(env, value_tpl[2], value_ptr++)) Badarg(5533,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform3uiv(location,count,value);
}

void ecb_glUniform4uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5534,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5534,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5534,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint> value_vec (4*count);
  GLuint *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5534,"value");
      if(!enif_get_uint(env, value_tpl[0], value_ptr++)) Badarg(5534,"value");
      if(!enif_get_uint(env, value_tpl[1], value_ptr++)) Badarg(5534,"value");
      if(!enif_get_uint(env, value_tpl[2], value_ptr++)) Badarg(5534,"value");
      if(!enif_get_uint(env, value_tpl[3], value_ptr++)) Badarg(5534,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform4uiv(location,count,value);
}

void ecb_glTexParameterIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5535,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5535,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5535,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5535,"params");
   }};
  weglTexParameterIiv(target,pname,params);
}

void ecb_glTexParameterIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLuint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5536,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5536,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5536,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_uint(env, params_t[i], &params[i])) Badarg(5536,"params");
   }};
  weglTexParameterIuiv(target,pname,params);
}

void ecb_glGetTexParameterIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5537,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5537,"pname");
  weglGetTexParameterIiv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetTexParameterIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLuint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5538,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5538,"pname");
  weglGetTexParameterIuiv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glClearBufferiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum buffer;
  GLint drawbuffer;
  GLint value[4];
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5539,"buffer");
  if(!enif_get_int(env, argv[1],  &drawbuffer)) Badarg(5539,"drawbuffer");
  {
   int value_a;
   const ERL_NIF_TERM *value_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &value_a, &value_t)) {
     Badarg(5539,"value");
   } else {
     for(i = 0; i < value_a; i++)
       if(!enif_get_int(env, value_t[i], &value[i])) Badarg(5539,"value");
   }};
  weglClearBufferiv(buffer,drawbuffer,value);
}

void ecb_glClearBufferuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum buffer;
  GLint drawbuffer;
  GLuint value[4];
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5540,"buffer");
  if(!enif_get_int(env, argv[1],  &drawbuffer)) Badarg(5540,"drawbuffer");
  {
   int value_a;
   const ERL_NIF_TERM *value_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &value_a, &value_t)) {
     Badarg(5540,"value");
   } else {
     for(i = 0; i < value_a; i++)
       if(!enif_get_uint(env, value_t[i], &value[i])) Badarg(5540,"value");
   }};
  weglClearBufferuiv(buffer,drawbuffer,value);
}

void ecb_glClearBufferfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum buffer;
  GLint drawbuffer;
  GLfloat value[4];
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5541,"buffer");
  if(!enif_get_int(env, argv[1],  &drawbuffer)) Badarg(5541,"drawbuffer");
  {
   int value_a;
   const ERL_NIF_TERM *value_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &value_a, &value_t)) {
     Badarg(5541,"value");
   } else {
     for(i = 0; i < value_a; i++)
       if(!egl_get_float(env, value_t[i], &value[i])) Badarg(5541,"value");
   }};
  weglClearBufferfv(buffer,drawbuffer,value);
}

void ecb_glClearBufferfi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum buffer;
  GLint drawbuffer;
  GLfloat depth;
  GLint stencil;
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5542,"buffer");
  if(!enif_get_int(env, argv[1],  &drawbuffer)) Badarg(5542,"drawbuffer");
  if(!egl_get_float(env, argv[2],  &depth)) Badarg(5542,"depth");
  if(!enif_get_int(env, argv[3],  &stencil)) Badarg(5542,"stencil");
  weglClearBufferfi(buffer,drawbuffer,depth,stencil);
}

void ecb_glGetStringi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  const GLubyte *  result;
  ERL_NIF_TERM reply;
  GLenum name;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5543,"name");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5543,"index");
  result = weglGetStringi(name,index);
  reply =      enif_make_string(env, (const char *) result, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsRenderbuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint renderbuffer;
  if(!enif_get_uint(env, argv[0],  &renderbuffer)) Badarg(5544,"renderbuffer");
  result = weglIsRenderbuffer(renderbuffer);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBindRenderbuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint renderbuffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5545,"target");
  if(!enif_get_uint(env, argv[1],  &renderbuffer)) Badarg(5545,"renderbuffer");
  weglBindRenderbuffer(target,renderbuffer);
}

void ecb_glDeleteRenderbuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *renderbuffers;
  std::vector <GLuint> renderbuffers_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5546,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5546, "renderbuffers")
  else {
    ERL_NIF_TERM renderbuffers_l, renderbuffers_h, renderbuffers_t;
    GLuint renderbuffers_tmp;
    renderbuffers_l = argv[1];
    while(enif_get_list_cell(env, renderbuffers_l, &renderbuffers_h, &renderbuffers_t)) {
        if(!enif_get_uint(env, renderbuffers_h, &renderbuffers_tmp)) Badarg(5546,"renderbuffers");
        renderbuffers_vec.push_back(renderbuffers_tmp);
        renderbuffers_l = renderbuffers_t;
    };
    renderbuffers = renderbuffers_vec.data();
  }
  weglDeleteRenderbuffers(n,renderbuffers);
}

void ecb_glGenRenderbuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5547,"n");
  std::vector <GLuint> renderbuffers (n);
  std::vector <ERL_NIF_TERM> renderbuffers_ts (n);
  weglGenRenderbuffers(n,renderbuffers.data());
  for(int ri=0; ri < (int) n; ri++)
    renderbuffers_ts[ri] =      enif_make_int(env, renderbuffers[ri]);
  reply =      enif_make_list_from_array(env, renderbuffers_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glRenderbufferStorage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5548,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5548,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5548,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5548,"height");
  weglRenderbufferStorage(target,internalformat,width,height);
}

void ecb_glGetRenderbufferParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5549,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5549,"pname");
  weglGetRenderbufferParameteriv(target,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsFramebuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint framebuffer;
  if(!enif_get_uint(env, argv[0],  &framebuffer)) Badarg(5550,"framebuffer");
  result = weglIsFramebuffer(framebuffer);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBindFramebuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint framebuffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5551,"target");
  if(!enif_get_uint(env, argv[1],  &framebuffer)) Badarg(5551,"framebuffer");
  weglBindFramebuffer(target,framebuffer);
}

void ecb_glDeleteFramebuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *framebuffers;
  std::vector <GLuint> framebuffers_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5552,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5552, "framebuffers")
  else {
    ERL_NIF_TERM framebuffers_l, framebuffers_h, framebuffers_t;
    GLuint framebuffers_tmp;
    framebuffers_l = argv[1];
    while(enif_get_list_cell(env, framebuffers_l, &framebuffers_h, &framebuffers_t)) {
        if(!enif_get_uint(env, framebuffers_h, &framebuffers_tmp)) Badarg(5552,"framebuffers");
        framebuffers_vec.push_back(framebuffers_tmp);
        framebuffers_l = framebuffers_t;
    };
    framebuffers = framebuffers_vec.data();
  }
  weglDeleteFramebuffers(n,framebuffers);
}

void ecb_glGenFramebuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5553,"n");
  std::vector <GLuint> framebuffers (n);
  std::vector <ERL_NIF_TERM> framebuffers_ts (n);
  weglGenFramebuffers(n,framebuffers.data());
  for(int ri=0; ri < (int) n; ri++)
    framebuffers_ts[ri] =      enif_make_int(env, framebuffers[ri]);
  reply =      enif_make_list_from_array(env, framebuffers_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glCheckFramebufferStatus(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum result;
  ERL_NIF_TERM reply;
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5554,"target");
  result = weglCheckFramebufferStatus(target);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glFramebufferTexture1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLenum textarget;
  GLuint texture;
  GLint level;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5555,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5555,"attachment");
  if(!enif_get_uint(env, argv[2],  &textarget)) Badarg(5555,"textarget");
  if(!enif_get_uint(env, argv[3],  &texture)) Badarg(5555,"texture");
  if(!enif_get_int(env, argv[4],  &level)) Badarg(5555,"level");
  weglFramebufferTexture1D(target,attachment,textarget,texture,level);
}

void ecb_glFramebufferTexture2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLenum textarget;
  GLuint texture;
  GLint level;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5556,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5556,"attachment");
  if(!enif_get_uint(env, argv[2],  &textarget)) Badarg(5556,"textarget");
  if(!enif_get_uint(env, argv[3],  &texture)) Badarg(5556,"texture");
  if(!enif_get_int(env, argv[4],  &level)) Badarg(5556,"level");
  weglFramebufferTexture2D(target,attachment,textarget,texture,level);
}

void ecb_glFramebufferTexture3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLenum textarget;
  GLuint texture;
  GLint level;
  GLint zoffset;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5557,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5557,"attachment");
  if(!enif_get_uint(env, argv[2],  &textarget)) Badarg(5557,"textarget");
  if(!enif_get_uint(env, argv[3],  &texture)) Badarg(5557,"texture");
  if(!enif_get_int(env, argv[4],  &level)) Badarg(5557,"level");
  if(!enif_get_int(env, argv[5],  &zoffset)) Badarg(5557,"zoffset");
  weglFramebufferTexture3D(target,attachment,textarget,texture,level,zoffset);
}

void ecb_glFramebufferRenderbuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLenum renderbuffertarget;
  GLuint renderbuffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5558,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5558,"attachment");
  if(!enif_get_uint(env, argv[2],  &renderbuffertarget)) Badarg(5558,"renderbuffertarget");
  if(!enif_get_uint(env, argv[3],  &renderbuffer)) Badarg(5558,"renderbuffer");
  weglFramebufferRenderbuffer(target,attachment,renderbuffertarget,renderbuffer);
}

void ecb_glGetFramebufferAttachmentParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum attachment;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5559,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5559,"attachment");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5559,"pname");
  weglGetFramebufferAttachmentParameteriv(target,attachment,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGenerateMipmap(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5560,"target");
  weglGenerateMipmap(target);
}

void ecb_glBlitFramebuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint srcX0;
  GLint srcY0;
  GLint srcX1;
  GLint srcY1;
  GLint dstX0;
  GLint dstY0;
  GLint dstX1;
  GLint dstY1;
  GLbitfield mask;
  GLenum filter;
  if(!enif_get_int(env, argv[0],  &srcX0)) Badarg(5561,"srcX0");
  if(!enif_get_int(env, argv[1],  &srcY0)) Badarg(5561,"srcY0");
  if(!enif_get_int(env, argv[2],  &srcX1)) Badarg(5561,"srcX1");
  if(!enif_get_int(env, argv[3],  &srcY1)) Badarg(5561,"srcY1");
  if(!enif_get_int(env, argv[4],  &dstX0)) Badarg(5561,"dstX0");
  if(!enif_get_int(env, argv[5],  &dstY0)) Badarg(5561,"dstY0");
  if(!enif_get_int(env, argv[6],  &dstX1)) Badarg(5561,"dstX1");
  if(!enif_get_int(env, argv[7],  &dstY1)) Badarg(5561,"dstY1");
  if(!enif_get_uint(env, argv[8],  &mask)) Badarg(5561,"mask");
  if(!enif_get_uint(env, argv[9],  &filter)) Badarg(5561,"filter");
  weglBlitFramebuffer(srcX0,srcY0,srcX1,srcY1,dstX0,dstY0,dstX1,dstY1,mask,filter);
}

void ecb_glRenderbufferStorageMultisample(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei samples;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5562,"target");
  if(!enif_get_int(env, argv[1],  &samples)) Badarg(5562,"samples");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5562,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5562,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5562,"height");
  weglRenderbufferStorageMultisample(target,samples,internalformat,width,height);
}

void ecb_glFramebufferTextureLayer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLuint texture;
  GLint level;
  GLint layer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5563,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5563,"attachment");
  if(!enif_get_uint(env, argv[2],  &texture)) Badarg(5563,"texture");
  if(!enif_get_int(env, argv[3],  &level)) Badarg(5563,"level");
  if(!enif_get_int(env, argv[4],  &layer)) Badarg(5563,"layer");
  weglFramebufferTextureLayer(target,attachment,texture,level,layer);
}

void ecb_glFlushMappedBufferRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLintptr offset;
  GLsizeiptr length;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5564,"target");
  if(!egl_get_word(env, argv[1], (egl_word *) &offset)) Badarg(5564,"offset");
  if(!egl_get_word(env, argv[2], (egl_word *) &length)) Badarg(5564,"length");
  weglFlushMappedBufferRange(target,offset,length);
}

void ecb_glBindVertexArray(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint array;
  if(!enif_get_uint(env, argv[0],  &array)) Badarg(5565,"array");
  weglBindVertexArray(array);
}

void ecb_glDeleteVertexArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *arrays;
  std::vector <GLuint> arrays_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5566,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5566, "arrays")
  else {
    ERL_NIF_TERM arrays_l, arrays_h, arrays_t;
    GLuint arrays_tmp;
    arrays_l = argv[1];
    while(enif_get_list_cell(env, arrays_l, &arrays_h, &arrays_t)) {
        if(!enif_get_uint(env, arrays_h, &arrays_tmp)) Badarg(5566,"arrays");
        arrays_vec.push_back(arrays_tmp);
        arrays_l = arrays_t;
    };
    arrays = arrays_vec.data();
  }
  weglDeleteVertexArrays(n,arrays);
}

void ecb_glGenVertexArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5567,"n");
  std::vector <GLuint> arrays (n);
  std::vector <ERL_NIF_TERM> arrays_ts (n);
  weglGenVertexArrays(n,arrays.data());
  for(int ri=0; ri < (int) n; ri++)
    arrays_ts[ri] =      enif_make_int(env, arrays[ri]);
  reply =      enif_make_list_from_array(env, arrays_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsVertexArray(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint array;
  if(!enif_get_uint(env, argv[0],  &array)) Badarg(5568,"array");
  result = weglIsVertexArray(array);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDrawArraysInstanced(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint first;
  GLsizei count;
  GLsizei instancecount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5569,"mode");
  if(!enif_get_int(env, argv[1],  &first)) Badarg(5569,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5569,"count");
  if(!enif_get_int(env, argv[3],  &instancecount)) Badarg(5569,"instancecount");
  weglDrawArraysInstanced(mode,first,count,instancecount);
}

void ecb_glDrawElementsInstanced(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  void *indices_idx;
  GLsizei instancecount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5570,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5570,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5570,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (void *) indices.data;
    else Badarg(5570,"indices");
  }
  if(!enif_get_int(env, argv[4],  &instancecount)) Badarg(5570,"instancecount");
  weglDrawElementsInstanced(mode,count,type,indices_idx,instancecount);
}

void ecb_glTexBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5572,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5572,"internalformat");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5572,"buffer");
  weglTexBuffer(target,internalformat,buffer);
}

void ecb_glPrimitiveRestartIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5573,"index");
  weglPrimitiveRestartIndex(index);
}

void ecb_glCopyBufferSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum readTarget;
  GLenum writeTarget;
  GLintptr readOffset;
  GLintptr writeOffset;
  GLsizeiptr size;
  if(!enif_get_uint(env, argv[0],  &readTarget)) Badarg(5574,"readTarget");
  if(!enif_get_uint(env, argv[1],  &writeTarget)) Badarg(5574,"writeTarget");
  if(!egl_get_word(env, argv[2], (egl_word *) &readOffset)) Badarg(5574,"readOffset");
  if(!egl_get_word(env, argv[3], (egl_word *) &writeOffset)) Badarg(5574,"writeOffset");
  if(!egl_get_word(env, argv[4], (egl_word *) &size)) Badarg(5574,"size");
  weglCopyBufferSubData(readTarget,writeTarget,readOffset,writeOffset,size);
}

void ecb_glGetUniformIndices(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei uniformCount;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5575,"program");
  if(!enif_get_int(env, argv[1],  &uniformCount)) Badarg(5575,"uniformCount");
  ERL_NIF_TERM uniformNames_l, uniformNames_h, uniformNames_t;
  ErlNifBinary uniformNames_tmp;
  std::vector <GLchar *> uniformNames;
  uniformNames_l = argv[2];
  while(enif_get_list_cell(env, uniformNames_l, &uniformNames_h, &uniformNames_t)) {
    if(!enif_inspect_binary(env, uniformNames_h, &uniformNames_tmp)) Badarg(5575,"uniformNames");
    uniformNames.push_back((GLchar *) uniformNames_tmp.data);
    uniformNames_l = uniformNames_t;
  }
  std::vector <GLuint> uniformIndices (uniformCount);
  std::vector <ERL_NIF_TERM> uniformIndices_ts (uniformCount);
  weglGetUniformIndices(program,uniformCount,(const GLchar **) uniformNames.data(),uniformIndices.data());
  for(int ri=0; ri < (int) uniformCount; ri++)
    uniformIndices_ts[ri] =      enif_make_int(env, uniformIndices[ri]);
  reply =      enif_make_list_from_array(env, uniformIndices_ts.data(), uniformCount);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetActiveUniformsiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei uniformCount;
  GLuint *uniformIndices;
  std::vector <GLuint> uniformIndices_vec;
  GLenum pname;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5576,"program");
  if(!enif_get_int(env, argv[1],  &uniformCount)) Badarg(5576,"uniformCount");
  if(!enif_is_list(env, argv[2])) Badarg(5576, "uniformIndices")
  else {
    ERL_NIF_TERM uniformIndices_l, uniformIndices_h, uniformIndices_t;
    GLuint uniformIndices_tmp;
    uniformIndices_l = argv[2];
    while(enif_get_list_cell(env, uniformIndices_l, &uniformIndices_h, &uniformIndices_t)) {
        if(!enif_get_uint(env, uniformIndices_h, &uniformIndices_tmp)) Badarg(5576,"uniformIndices");
        uniformIndices_vec.push_back(uniformIndices_tmp);
        uniformIndices_l = uniformIndices_t;
    };
    uniformIndices = uniformIndices_vec.data();
  }
  if(!enif_get_uint(env, argv[3],  &pname)) Badarg(5576,"pname");
  std::vector <GLint> params (uniformCount);
  std::vector <ERL_NIF_TERM> params_ts (uniformCount);
  weglGetActiveUniformsiv(program,uniformCount,uniformIndices,pname,params.data());
  for(int ri=0; ri < (int) uniformCount; ri++)
    params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_list_from_array(env, params_ts.data(), uniformCount);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetActiveUniformName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint uniformIndex;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *uniformName;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5577,"program");
  if(!enif_get_uint(env, argv[1],  &uniformIndex)) Badarg(5577,"uniformIndex");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5577,"bufSize");
  uniformName = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetActiveUniformName(program,uniformIndex,bufSize,&length,(GLchar *) uniformName);
  reply =      enif_make_string(env, (const char *) uniformName, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(uniformName);
}

void ecb_glGetUniformBlockIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary uniformBlockName;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5578,"program");
  if(!enif_inspect_binary(env, argv[1], &uniformBlockName)) Badarg(5578,"uniformBlockName");
  result = weglGetUniformBlockIndex(program,(GLchar *) uniformBlockName.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetActiveUniformBlockiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint uniformBlockIndex;
  GLenum pname;
  ErlNifBinary params;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5579,"program");
  if(!enif_get_uint(env, argv[1],  &uniformBlockIndex)) Badarg(5579,"uniformBlockIndex");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5579,"pname");
  if(enif_is_binary(env, argv[3]))
    enif_inspect_binary(env, argv[3], &params);
  else if(enif_is_tuple(env, argv[3])) {
    int params_a;
    const ERL_NIF_TERM *params_t;
    if(enif_get_tuple(env, argv[3], &params_a, &params_t) &&
         enif_is_binary(env, params_t[1]))
       enif_inspect_binary(env, params_t[1], &params);
    else Badarg(5579, "params");
  } else Badarg(5579, "params");
  weglGetActiveUniformBlockiv(program,uniformBlockIndex,pname,(GLint *) params.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetActiveUniformBlockName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint uniformBlockIndex;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *uniformBlockName;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5580,"program");
  if(!enif_get_uint(env, argv[1],  &uniformBlockIndex)) Badarg(5580,"uniformBlockIndex");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5580,"bufSize");
  uniformBlockName = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetActiveUniformBlockName(program,uniformBlockIndex,bufSize,&length,(GLchar *) uniformBlockName);
  reply =      enif_make_string(env, (const char *) uniformBlockName, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(uniformBlockName);
}

void ecb_glUniformBlockBinding(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint uniformBlockIndex;
  GLuint uniformBlockBinding;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5581,"program");
  if(!enif_get_uint(env, argv[1],  &uniformBlockIndex)) Badarg(5581,"uniformBlockIndex");
  if(!enif_get_uint(env, argv[2],  &uniformBlockBinding)) Badarg(5581,"uniformBlockBinding");
  weglUniformBlockBinding(program,uniformBlockIndex,uniformBlockBinding);
}

void ecb_glDrawElementsBaseVertex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  void *indices_idx;
  GLint basevertex;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5582,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5582,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5582,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (void *) indices.data;
    else Badarg(5582,"indices");
  }
  if(!enif_get_int(env, argv[4],  &basevertex)) Badarg(5582,"basevertex");
  weglDrawElementsBaseVertex(mode,count,type,indices_idx,basevertex);
}

void ecb_glDrawRangeElementsBaseVertex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint start;
  GLuint end;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  void *indices_idx;
  GLint basevertex;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5584,"mode");
  if(!enif_get_uint(env, argv[1],  &start)) Badarg(5584,"start");
  if(!enif_get_uint(env, argv[2],  &end)) Badarg(5584,"end");
  if(!enif_get_int(env, argv[3],  &count)) Badarg(5584,"count");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5584,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &indices))
        indices_idx = (void *) indices.data;
    else Badarg(5584,"indices");
  }
  if(!enif_get_int(env, argv[6],  &basevertex)) Badarg(5584,"basevertex");
  weglDrawRangeElementsBaseVertex(mode,start,end,count,type,indices_idx,basevertex);
}

void ecb_glDrawElementsInstancedBaseVertex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  void *indices_idx;
  GLsizei instancecount;
  GLint basevertex;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5586,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5586,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5586,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (void *) indices.data;
    else Badarg(5586,"indices");
  }
  if(!enif_get_int(env, argv[4],  &instancecount)) Badarg(5586,"instancecount");
  if(!enif_get_int(env, argv[5],  &basevertex)) Badarg(5586,"basevertex");
  weglDrawElementsInstancedBaseVertex(mode,count,type,indices_idx,instancecount,basevertex);
}

void ecb_glProvokingVertex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5588,"mode");
  weglProvokingVertex(mode);
}

void ecb_glFenceSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsync result;
  ERL_NIF_TERM reply;
  GLenum condition;
  GLbitfield flags;
  if(!enif_get_uint(env, argv[0],  &condition)) Badarg(5589,"condition");
  if(!enif_get_uint(env, argv[1],  &flags)) Badarg(5589,"flags");
  result = weglFenceSync(condition,flags);
  reply =      enif_make_uint64(env, (egl_uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  egl_uint64_t sync;
  if(!egl_get_ptr(env, argv[0], (void **) &sync)) Badarg(5590,"sync");
  result = weglIsSync((GLsync) sync);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDeleteSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t sync;
  if(!egl_get_ptr(env, argv[0], (void **) &sync)) Badarg(5591,"sync");
  weglDeleteSync((GLsync) sync);
}

void ecb_glClientWaitSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum result;
  ERL_NIF_TERM reply;
  egl_uint64_t sync;
  GLbitfield flags;
  GLuint64 timeout;
  if(!egl_get_ptr(env, argv[0], (void **) &sync)) Badarg(5592,"sync");
  if(!enif_get_uint(env, argv[1],  &flags)) Badarg(5592,"flags");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &timeout)) Badarg(5592,"timeout");
  result = weglClientWaitSync((GLsync) sync,flags,timeout);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glWaitSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t sync;
  GLbitfield flags;
  GLuint64 timeout;
  if(!egl_get_ptr(env, argv[0], (void **) &sync)) Badarg(5593,"sync");
  if(!enif_get_uint(env, argv[1],  &flags)) Badarg(5593,"flags");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &timeout)) Badarg(5593,"timeout");
  weglWaitSync((GLsync) sync,flags,timeout);
}

void ecb_glGetInteger64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLint64 data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5594,"pname");
  weglGetInteger64v(pname,data);
  for(int ri=0; ri < (int) 16; ri++)
     data_ts[ri] =      enif_make_int64(env, (egl_int64_t) data[ri]);
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetSynciv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t sync;
  GLenum pname;
  GLsizei bufSize;
  GLsizei length;
  if(!egl_get_ptr(env, argv[0], (void **) &sync)) Badarg(5595,"sync");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5595,"pname");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5595,"bufSize");
  std::vector <GLint> values (bufSize);
  std::vector <ERL_NIF_TERM> values_ts (bufSize);
  weglGetSynciv((GLsync) sync,pname,bufSize,&length,values.data());
  for(int ri=0; ri < (int) length; ri++)
    values_ts[ri] =      enif_make_int(env, values[ri]);
  reply =      enif_make_list_from_array(env, values_ts.data(), length);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetInteger64i_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLint64 data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5596,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5596,"index");
  weglGetInteger64i_v(target,index,data);
  for(int ri=0; ri < (int) 16; ri++)
     data_ts[ri] =      enif_make_int64(env, (egl_int64_t) data[ri]);
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetBufferParameteri64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint64 params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5597,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5597,"pname");
  weglGetBufferParameteri64v(target,pname,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int64(env, (egl_int64_t) params[ri]);
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glFramebufferTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLuint texture;
  GLint level;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5598,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5598,"attachment");
  if(!enif_get_uint(env, argv[2],  &texture)) Badarg(5598,"texture");
  if(!enif_get_int(env, argv[3],  &level)) Badarg(5598,"level");
  weglFramebufferTexture(target,attachment,texture,level);
}

void ecb_glTexImage2DMultisample(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei samples;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLboolean fixedsamplelocations;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5599,"target");
  if(!enif_get_int(env, argv[1],  &samples)) Badarg(5599,"samples");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5599,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5599,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5599,"height");
  if(!egl_get_ubyte(env, argv[5],  &fixedsamplelocations)) Badarg(5599,"fixedsamplelocations");
  weglTexImage2DMultisample(target,samples,internalformat,width,height,fixedsamplelocations);
}

void ecb_glTexImage3DMultisample(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei samples;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLboolean fixedsamplelocations;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5600,"target");
  if(!enif_get_int(env, argv[1],  &samples)) Badarg(5600,"samples");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5600,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5600,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5600,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5600,"depth");
  if(!egl_get_ubyte(env, argv[6],  &fixedsamplelocations)) Badarg(5600,"fixedsamplelocations");
  weglTexImage3DMultisample(target,samples,internalformat,width,height,depth,fixedsamplelocations);
}

void ecb_glGetMultisamplefv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLuint index;
  GLfloat val[2];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5601,"pname");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5601,"index");
  weglGetMultisamplefv(pname,index,val);
  reply =      enif_make_tuple2(env,
     enif_make_double(env, (double) val[0]),
            enif_make_double(env, (double) val[1]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glSampleMaski(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint maskNumber;
  GLbitfield mask;
  if(!enif_get_uint(env, argv[0],  &maskNumber)) Badarg(5602,"maskNumber");
  if(!enif_get_uint(env, argv[1],  &mask)) Badarg(5602,"mask");
  weglSampleMaski(maskNumber,mask);
}

void ecb_glBindFragDataLocationIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint colorNumber;
  GLuint index;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5603,"program");
  if(!enif_get_uint(env, argv[1],  &colorNumber)) Badarg(5603,"colorNumber");
  if(!enif_get_uint(env, argv[2],  &index)) Badarg(5603,"index");
  if(!enif_inspect_binary(env, argv[3], &name)) Badarg(5603,"name");
  weglBindFragDataLocationIndexed(program,colorNumber,index,(GLchar *) name.data);
}

void ecb_glGetFragDataIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5604,"program");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5604,"name");
  result = weglGetFragDataIndex(program,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGenSamplers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei count;
  if(!enif_get_int(env, argv[0],  &count)) Badarg(5605,"count");
  std::vector <GLuint> samplers (count);
  std::vector <ERL_NIF_TERM> samplers_ts (count);
  weglGenSamplers(count,samplers.data());
  for(int ri=0; ri < (int) count; ri++)
    samplers_ts[ri] =      enif_make_int(env, samplers[ri]);
  reply =      enif_make_list_from_array(env, samplers_ts.data(), count);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDeleteSamplers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei count;
  GLuint *samplers;
  std::vector <GLuint> samplers_vec;
  if(!enif_get_int(env, argv[0],  &count)) Badarg(5606,"count");
  if(!enif_is_list(env, argv[1])) Badarg(5606, "samplers")
  else {
    ERL_NIF_TERM samplers_l, samplers_h, samplers_t;
    GLuint samplers_tmp;
    samplers_l = argv[1];
    while(enif_get_list_cell(env, samplers_l, &samplers_h, &samplers_t)) {
        if(!enif_get_uint(env, samplers_h, &samplers_tmp)) Badarg(5606,"samplers");
        samplers_vec.push_back(samplers_tmp);
        samplers_l = samplers_t;
    };
    samplers = samplers_vec.data();
  }
  weglDeleteSamplers(count,samplers);
}

void ecb_glIsSampler(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint sampler;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5607,"sampler");
  result = weglIsSampler(sampler);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBindSampler(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint unit;
  GLuint sampler;
  if(!enif_get_uint(env, argv[0],  &unit)) Badarg(5608,"unit");
  if(!enif_get_uint(env, argv[1],  &sampler)) Badarg(5608,"sampler");
  weglBindSampler(unit,sampler);
}

void ecb_glSamplerParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5609,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5609,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5609,"param");
  weglSamplerParameteri(sampler,pname,param);
}

void ecb_glSamplerParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  GLint *param;
  std::vector <GLint> param_vec;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5610,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5610,"pname");
  if(!enif_is_list(env, argv[2])) Badarg(5610, "param")
  else {
    ERL_NIF_TERM param_l, param_h, param_t;
    GLint param_tmp;
    param_l = argv[2];
    while(enif_get_list_cell(env, param_l, &param_h, &param_t)) {
        if(!enif_get_int(env, param_h, &param_tmp)) Badarg(5610,"param");
        param_vec.push_back(param_tmp);
        param_l = param_t;
    };
    param = param_vec.data();
  }
  weglSamplerParameteriv(sampler,pname,param);
}

void ecb_glSamplerParameterf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5611,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5611,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5611,"param");
  weglSamplerParameterf(sampler,pname,param);
}

void ecb_glSamplerParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  GLfloat *param;
  std::vector <GLfloat> param_vec;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5612,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5612,"pname");
  if(!enif_is_list(env, argv[2])) Badarg(5612, "param")
  else {
    ERL_NIF_TERM param_l, param_h, param_t;
    GLfloat param_tmp;
    param_l = argv[2];
    while(enif_get_list_cell(env, param_l, &param_h, &param_t)) {
        if(!egl_get_float(env, param_h, &param_tmp)) Badarg(5612,"param");
        param_vec.push_back(param_tmp);
        param_l = param_t;
    };
    param = param_vec.data();
  }
  weglSamplerParameterfv(sampler,pname,param);
}

void ecb_glSamplerParameterIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  GLint *param;
  std::vector <GLint> param_vec;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5613,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5613,"pname");
  if(!enif_is_list(env, argv[2])) Badarg(5613, "param")
  else {
    ERL_NIF_TERM param_l, param_h, param_t;
    GLint param_tmp;
    param_l = argv[2];
    while(enif_get_list_cell(env, param_l, &param_h, &param_t)) {
        if(!enif_get_int(env, param_h, &param_tmp)) Badarg(5613,"param");
        param_vec.push_back(param_tmp);
        param_l = param_t;
    };
    param = param_vec.data();
  }
  weglSamplerParameterIiv(sampler,pname,param);
}

void ecb_glSamplerParameterIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  GLuint *param;
  std::vector <GLuint> param_vec;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5614,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5614,"pname");
  if(!enif_is_list(env, argv[2])) Badarg(5614, "param")
  else {
    ERL_NIF_TERM param_l, param_h, param_t;
    GLuint param_tmp;
    param_l = argv[2];
    while(enif_get_list_cell(env, param_l, &param_h, &param_t)) {
        if(!enif_get_uint(env, param_h, &param_tmp)) Badarg(5614,"param");
        param_vec.push_back(param_tmp);
        param_l = param_t;
    };
    param = param_vec.data();
  }
  weglSamplerParameterIuiv(sampler,pname,param);
}

void ecb_glGetSamplerParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint sampler;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5615,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5615,"pname");
  weglGetSamplerParameteriv(sampler,pname,params);
  reply =      enif_make_list4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetSamplerParameterIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint sampler;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5616,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5616,"pname");
  weglGetSamplerParameterIiv(sampler,pname,params);
  reply =      enif_make_list4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetSamplerParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint sampler;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5617,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5617,"pname");
  weglGetSamplerParameterfv(sampler,pname,params);
  reply =      enif_make_list4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetSamplerParameterIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint sampler;
  GLenum pname;
  GLuint params[4];
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5618,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5618,"pname");
  weglGetSamplerParameterIuiv(sampler,pname,params);
  reply =      enif_make_list4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glQueryCounter(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint id;
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5619,"id");
  if(!enif_get_uint(env, argv[1],  &target)) Badarg(5619,"target");
  weglQueryCounter(id,target);
}

void ecb_glGetQueryObjecti64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint id;
  GLenum pname;
  GLint64 params;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5620,"id");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5620,"pname");
  weglGetQueryObjecti64v(id,pname,&params);
  reply =      enif_make_int64(env, (egl_int64_t) params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetQueryObjectui64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint id;
  GLenum pname;
  GLuint64 params;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5621,"id");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5621,"pname");
  weglGetQueryObjectui64v(id,pname,&params);
  reply =      enif_make_int64(env, (egl_int64_t) params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glVertexAttribDivisor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint divisor;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5622,"index");
  if(!enif_get_uint(env, argv[1],  &divisor)) Badarg(5622,"divisor");
  weglVertexAttribDivisor(index,divisor);
}

void ecb_glMinSampleShading(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat value;
  if(!egl_get_float(env, argv[0],  &value)) Badarg(5623,"value");
  weglMinSampleShading(value);
}

void ecb_glBlendEquationi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buf;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &buf)) Badarg(5624,"buf");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5624,"mode");
  weglBlendEquationi(buf,mode);
}

void ecb_glBlendEquationSeparatei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buf;
  GLenum modeRGB;
  GLenum modeAlpha;
  if(!enif_get_uint(env, argv[0],  &buf)) Badarg(5625,"buf");
  if(!enif_get_uint(env, argv[1],  &modeRGB)) Badarg(5625,"modeRGB");
  if(!enif_get_uint(env, argv[2],  &modeAlpha)) Badarg(5625,"modeAlpha");
  weglBlendEquationSeparatei(buf,modeRGB,modeAlpha);
}

void ecb_glBlendFunci(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buf;
  GLenum src;
  GLenum dst;
  if(!enif_get_uint(env, argv[0],  &buf)) Badarg(5626,"buf");
  if(!enif_get_uint(env, argv[1],  &src)) Badarg(5626,"src");
  if(!enif_get_uint(env, argv[2],  &dst)) Badarg(5626,"dst");
  weglBlendFunci(buf,src,dst);
}

void ecb_glBlendFuncSeparatei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buf;
  GLenum srcRGB;
  GLenum dstRGB;
  GLenum srcAlpha;
  GLenum dstAlpha;
  if(!enif_get_uint(env, argv[0],  &buf)) Badarg(5627,"buf");
  if(!enif_get_uint(env, argv[1],  &srcRGB)) Badarg(5627,"srcRGB");
  if(!enif_get_uint(env, argv[2],  &dstRGB)) Badarg(5627,"dstRGB");
  if(!enif_get_uint(env, argv[3],  &srcAlpha)) Badarg(5627,"srcAlpha");
  if(!enif_get_uint(env, argv[4],  &dstAlpha)) Badarg(5627,"dstAlpha");
  weglBlendFuncSeparatei(buf,srcRGB,dstRGB,srcAlpha,dstAlpha);
}

void ecb_glDrawArraysIndirect(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  ErlNifBinary indirect;
  void *indirect_idx;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5628,"mode");
  if(!egl_get_ptr(env, argv[1], (void **) &indirect_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[1], &indirect))
        indirect_idx = (void *) indirect.data;
    else Badarg(5628,"indirect");
  }
  weglDrawArraysIndirect(mode,indirect_idx);
}

void ecb_glDrawElementsIndirect(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLenum type;
  ErlNifBinary indirect;
  void *indirect_idx;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5630,"mode");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5630,"type");
  if(!egl_get_ptr(env, argv[2], (void **) &indirect_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &indirect))
        indirect_idx = (void *) indirect.data;
    else Badarg(5630,"indirect");
  }
  weglDrawElementsIndirect(mode,type,indirect_idx);
}

void ecb_glUniform1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLdouble x;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5632,"location");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5632,"x");
  weglUniform1d(location,x);
}

void ecb_glUniform2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLdouble x;
  GLdouble y;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5633,"location");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5633,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5633,"y");
  weglUniform2d(location,x,y);
}

void ecb_glUniform3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5634,"location");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5634,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5634,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5634,"z");
  weglUniform3d(location,x,y,z);
}

void ecb_glUniform4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5635,"location");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5635,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5635,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5635,"z");
  if(!enif_get_double(env, argv[4],  &w)) Badarg(5635,"w");
  weglUniform4d(location,x,y,z,w);
}

void ecb_glUniform1dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLdouble *value;
  std::vector <GLdouble> value_vec;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5636,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5636,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5636, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLdouble value_tmp;
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_double(env, value_h, &value_tmp)) Badarg(5636,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglUniform1dv(location,count,value);
}

void ecb_glUniform2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5637,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5637,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5637,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (2*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5637,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5637,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5637,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform2dv(location,count,value);
}

void ecb_glUniform3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5638,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5638,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5638,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (3*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5638,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5638,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5638,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5638,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform3dv(location,count,value);
}

void ecb_glUniform4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5639,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5639,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5639,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (4*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5639,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5639,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5639,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5639,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5639,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform4dv(location,count,value);
}

void ecb_glUniformMatrix2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5640,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5640,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5640,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5640,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (4*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5640,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5640,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5640,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5640,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5640,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix2dv(location,count,transpose,value);
}

void ecb_glUniformMatrix3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5641,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5641,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5641,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5641,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (9*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 9) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5641,"value");
      if(!enif_get_double(env, value_tpl[8], value_ptr++)) Badarg(5641,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix3dv(location,count,transpose,value);
}

void ecb_glUniformMatrix4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5642,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5642,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5642,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5642,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (16*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 16) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[8], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[9], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[10], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[11], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[12], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[13], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[14], value_ptr++)) Badarg(5642,"value");
      if(!enif_get_double(env, value_tpl[15], value_ptr++)) Badarg(5642,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix4dv(location,count,transpose,value);
}

void ecb_glUniformMatrix2x3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5643,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5643,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5643,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5643,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (6*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) Badarg(5643,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5643,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5643,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5643,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5643,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5643,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5643,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix2x3dv(location,count,transpose,value);
}

void ecb_glUniformMatrix2x4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5644,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5644,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5644,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5644,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (8*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) Badarg(5644,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5644,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5644,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5644,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5644,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5644,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5644,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5644,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5644,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix2x4dv(location,count,transpose,value);
}

void ecb_glUniformMatrix3x2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5645,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5645,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5645,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5645,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (6*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) Badarg(5645,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5645,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5645,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5645,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5645,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5645,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5645,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix3x2dv(location,count,transpose,value);
}

void ecb_glUniformMatrix3x4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5646,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5646,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5646,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5646,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (12*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[8], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[9], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[10], value_ptr++)) Badarg(5646,"value");
      if(!enif_get_double(env, value_tpl[11], value_ptr++)) Badarg(5646,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix3x4dv(location,count,transpose,value);
}

void ecb_glUniformMatrix4x2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5647,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5647,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5647,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5647,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (8*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) Badarg(5647,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5647,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5647,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5647,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5647,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5647,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5647,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5647,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5647,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix4x2dv(location,count,transpose,value);
}

void ecb_glUniformMatrix4x3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5648,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5648,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5648,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5648,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (12*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[8], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[9], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[10], value_ptr++)) Badarg(5648,"value");
      if(!enif_get_double(env, value_tpl[11], value_ptr++)) Badarg(5648,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniformMatrix4x3dv(location,count,transpose,value);
}

void ecb_glGetUniformdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLdouble params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5649,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5649,"location");
  weglGetUniformdv(program,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_double(env, params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetSubroutineUniformLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5650,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5650,"shadertype");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5650,"name");
  result = weglGetSubroutineUniformLocation(program,shadertype,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetSubroutineIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5651,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5651,"shadertype");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5651,"name");
  result = weglGetSubroutineIndex(program,shadertype,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetActiveSubroutineUniformName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  GLuint index;
  GLsizei bufsize;
  GLsizei length;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5652,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5652,"shadertype");
  if(!enif_get_uint(env, argv[2],  &index)) Badarg(5652,"index");
  if(!enif_get_int(env, argv[3],  &bufsize)) Badarg(5652,"bufsize");
  name = (unsigned char *) enif_alloc((int) bufsize*sizeof(GLchar));
  weglGetActiveSubroutineUniformName(program,shadertype,index,bufsize,&length,(GLchar *) name);
  reply =      enif_make_string(env, (const char *) name, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

void ecb_glGetActiveSubroutineName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  GLuint index;
  GLsizei bufsize;
  GLsizei length;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5653,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5653,"shadertype");
  if(!enif_get_uint(env, argv[2],  &index)) Badarg(5653,"index");
  if(!enif_get_int(env, argv[3],  &bufsize)) Badarg(5653,"bufsize");
  name = (unsigned char *) enif_alloc((int) bufsize*sizeof(GLchar));
  weglGetActiveSubroutineName(program,shadertype,index,bufsize,&length,(GLchar *) name);
  reply =      enif_make_string(env, (const char *) name, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

void ecb_glUniformSubroutinesuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum shadertype;
  GLsizei count;
  GLuint *indices;
  std::vector <GLuint> indices_vec;
  if(!enif_get_uint(env, argv[0],  &shadertype)) Badarg(5654,"shadertype");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5654,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5654, "indices")
  else {
    ERL_NIF_TERM indices_l, indices_h, indices_t;
    GLuint indices_tmp;
    indices_l = argv[2];
    while(enif_get_list_cell(env, indices_l, &indices_h, &indices_t)) {
        if(!enif_get_uint(env, indices_h, &indices_tmp)) Badarg(5654,"indices");
        indices_vec.push_back(indices_tmp);
        indices_l = indices_t;
    };
    indices = indices_vec.data();
  }
  weglUniformSubroutinesuiv(shadertype,count,indices);
}

void ecb_glGetUniformSubroutineuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum shadertype;
  GLint location;
  GLuint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &shadertype)) Badarg(5655,"shadertype");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5655,"location");
  weglGetUniformSubroutineuiv(shadertype,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramStageiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  GLenum pname;
  GLint values;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5656,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5656,"shadertype");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5656,"pname");
  weglGetProgramStageiv(program,shadertype,pname,&values);
  reply =      enif_make_int(env, values);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glPatchParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint value;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5657,"pname");
  if(!enif_get_int(env, argv[1],  &value)) Badarg(5657,"value");
  weglPatchParameteri(pname,value);
}

void ecb_glPatchParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat *values;
  std::vector <GLfloat> values_vec;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5658,"pname");
  if(!enif_is_list(env, argv[1])) Badarg(5658, "values")
  else {
    ERL_NIF_TERM values_l, values_h, values_t;
    GLfloat values_tmp;
    values_l = argv[1];
    while(enif_get_list_cell(env, values_l, &values_h, &values_t)) {
        if(!egl_get_float(env, values_h, &values_tmp)) Badarg(5658,"values");
        values_vec.push_back(values_tmp);
        values_l = values_t;
    };
    values = values_vec.data();
  }
  weglPatchParameterfv(pname,values);
}

void ecb_glBindTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5659,"target");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5659,"id");
  weglBindTransformFeedback(target,id);
}

void ecb_glDeleteTransformFeedbacks(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *ids;
  std::vector <GLuint> ids_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5660,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5660, "ids")
  else {
    ERL_NIF_TERM ids_l, ids_h, ids_t;
    GLuint ids_tmp;
    ids_l = argv[1];
    while(enif_get_list_cell(env, ids_l, &ids_h, &ids_t)) {
        if(!enif_get_uint(env, ids_h, &ids_tmp)) Badarg(5660,"ids");
        ids_vec.push_back(ids_tmp);
        ids_l = ids_t;
    };
    ids = ids_vec.data();
  }
  weglDeleteTransformFeedbacks(n,ids);
}

void ecb_glGenTransformFeedbacks(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5661,"n");
  std::vector <GLuint> ids (n);
  std::vector <ERL_NIF_TERM> ids_ts (n);
  weglGenTransformFeedbacks(n,ids.data());
  for(int ri=0; ri < (int) n; ri++)
    ids_ts[ri] =      enif_make_int(env, ids[ri]);
  reply =      enif_make_list_from_array(env, ids_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5662,"id");
  result = weglIsTransformFeedback(id);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glPauseTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPauseTransformFeedback();
}

void ecb_glResumeTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglResumeTransformFeedback();
}

void ecb_glDrawTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5665,"mode");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5665,"id");
  weglDrawTransformFeedback(mode,id);
}

void ecb_glDrawTransformFeedbackStream(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint id;
  GLuint stream;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5666,"mode");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5666,"id");
  if(!enif_get_uint(env, argv[2],  &stream)) Badarg(5666,"stream");
  weglDrawTransformFeedbackStream(mode,id,stream);
}

void ecb_glBeginQueryIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5667,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5667,"index");
  if(!enif_get_uint(env, argv[2],  &id)) Badarg(5667,"id");
  weglBeginQueryIndexed(target,index,id);
}

void ecb_glEndQueryIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5668,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5668,"index");
  weglEndQueryIndexed(target,index);
}

void ecb_glGetQueryIndexediv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5669,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5669,"index");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5669,"pname");
  weglGetQueryIndexediv(target,index,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glReleaseShaderCompiler(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglReleaseShaderCompiler();
}

void ecb_glShaderBinary(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei count;
  GLuint *shaders;
  std::vector <GLuint> shaders_vec;
  GLenum binaryformat;
  ErlNifBinary binary;
  if(!enif_get_int(env, argv[0],  &count)) Badarg(5671,"count");
  if(!enif_is_list(env, argv[1])) Badarg(5671, "shaders")
  else {
    ERL_NIF_TERM shaders_l, shaders_h, shaders_t;
    GLuint shaders_tmp;
    shaders_l = argv[1];
    while(enif_get_list_cell(env, shaders_l, &shaders_h, &shaders_t)) {
        if(!enif_get_uint(env, shaders_h, &shaders_tmp)) Badarg(5671,"shaders");
        shaders_vec.push_back(shaders_tmp);
        shaders_l = shaders_t;
    };
    shaders = shaders_vec.data();
  }
  if(!enif_get_uint(env, argv[2],  &binaryformat)) Badarg(5671,"binaryformat");
  if(!enif_inspect_binary(env, argv[3], &binary)) Badarg(5671,"binary");
  weglShaderBinary(count,shaders,binaryformat,(void *) binary.data,(GLsizei) binary.size);
}

void ecb_glGetShaderPrecisionFormat(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum shadertype;
  GLenum precisiontype;
  GLint range[2];
  GLint precision;
  if(!enif_get_uint(env, argv[0],  &shadertype)) Badarg(5672,"shadertype");
  if(!enif_get_uint(env, argv[1],  &precisiontype)) Badarg(5672,"precisiontype");
  weglGetShaderPrecisionFormat(shadertype,precisiontype,range,&precision);
  reply = enif_make_tuple2(env,
          enif_make_tuple2(env,
     enif_make_int(env, range[0]),
            enif_make_int(env, range[1])),
     enif_make_int(env, precision) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDepthRangef(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat n;
  GLfloat f;
  if(!egl_get_float(env, argv[0],  &n)) Badarg(5673,"n");
  if(!egl_get_float(env, argv[1],  &f)) Badarg(5673,"f");
  weglDepthRangef(n,f);
}

void ecb_glClearDepthf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat d;
  if(!egl_get_float(env, argv[0],  &d)) Badarg(5674,"d");
  weglClearDepthf(d);
}

void ecb_glGetProgramBinary(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei bufSize;
  GLsizei length;
  GLenum binaryFormat;
  ErlNifBinary binary;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5675,"program");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5675,"bufSize");
  enif_alloc_binary((int) bufSize*sizeof(char), &binary);
  weglGetProgramBinary(program,bufSize,&length,&binaryFormat,(void *) binary.data);
  reply = enif_make_tuple2(env,
          enif_make_int(env, binaryFormat),
     enif_make_binary(env, &binary) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glProgramBinary(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLenum binaryFormat;
  ErlNifBinary binary;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5676,"program");
  if(!enif_get_uint(env, argv[1],  &binaryFormat)) Badarg(5676,"binaryFormat");
  if(!enif_inspect_binary(env, argv[2], &binary)) Badarg(5676,"binary");
  weglProgramBinary(program,binaryFormat,(void *) binary.data,(GLsizei) binary.size);
}

void ecb_glProgramParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLenum pname;
  GLint value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5677,"program");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5677,"pname");
  if(!enif_get_int(env, argv[2],  &value)) Badarg(5677,"value");
  weglProgramParameteri(program,pname,value);
}

void ecb_glUseProgramStages(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint pipeline;
  GLbitfield stages;
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5678,"pipeline");
  if(!enif_get_uint(env, argv[1],  &stages)) Badarg(5678,"stages");
  if(!enif_get_uint(env, argv[2],  &program)) Badarg(5678,"program");
  weglUseProgramStages(pipeline,stages,program);
}

void ecb_glActiveShaderProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint pipeline;
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5679,"pipeline");
  if(!enif_get_uint(env, argv[1],  &program)) Badarg(5679,"program");
  weglActiveShaderProgram(pipeline,program);
}

void ecb_glCreateShaderProgramv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLenum type;
  GLsizei count;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5680,"type");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5680,"count");
  ERL_NIF_TERM strings_l, strings_h, strings_t;
  ErlNifBinary strings_tmp;
  std::vector <GLchar *> strings;
  strings_l = argv[2];
  while(enif_get_list_cell(env, strings_l, &strings_h, &strings_t)) {
    if(!enif_inspect_binary(env, strings_h, &strings_tmp)) Badarg(5680,"strings");
    strings.push_back((GLchar *) strings_tmp.data);
    strings_l = strings_t;
  }
  result = weglCreateShaderProgramv(type,count,(const GLchar **) strings.data());
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBindProgramPipeline(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint pipeline;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5681,"pipeline");
  weglBindProgramPipeline(pipeline);
}

void ecb_glDeleteProgramPipelines(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *pipelines;
  std::vector <GLuint> pipelines_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5682,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5682, "pipelines")
  else {
    ERL_NIF_TERM pipelines_l, pipelines_h, pipelines_t;
    GLuint pipelines_tmp;
    pipelines_l = argv[1];
    while(enif_get_list_cell(env, pipelines_l, &pipelines_h, &pipelines_t)) {
        if(!enif_get_uint(env, pipelines_h, &pipelines_tmp)) Badarg(5682,"pipelines");
        pipelines_vec.push_back(pipelines_tmp);
        pipelines_l = pipelines_t;
    };
    pipelines = pipelines_vec.data();
  }
  weglDeleteProgramPipelines(n,pipelines);
}

void ecb_glGenProgramPipelines(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5683,"n");
  std::vector <GLuint> pipelines (n);
  std::vector <ERL_NIF_TERM> pipelines_ts (n);
  weglGenProgramPipelines(n,pipelines.data());
  for(int ri=0; ri < (int) n; ri++)
    pipelines_ts[ri] =      enif_make_int(env, pipelines[ri]);
  reply =      enif_make_list_from_array(env, pipelines_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsProgramPipeline(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint pipeline;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5684,"pipeline");
  result = weglIsProgramPipeline(pipeline);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramPipelineiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint pipeline;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5685,"pipeline");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5685,"pname");
  weglGetProgramPipelineiv(pipeline,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glProgramUniform1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint v0;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5686,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5686,"location");
  if(!enif_get_int(env, argv[2],  &v0)) Badarg(5686,"v0");
  weglProgramUniform1i(program,location,v0);
}

void ecb_glProgramUniform1iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint *value;
  std::vector <GLint> value_vec;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5687,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5687,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5687,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5687, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLint value_tmp;
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_int(env, value_h, &value_tmp)) Badarg(5687,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglProgramUniform1iv(program,location,count,value);
}

void ecb_glProgramUniform1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLfloat v0;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5688,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5688,"location");
  if(!egl_get_float(env, argv[2],  &v0)) Badarg(5688,"v0");
  weglProgramUniform1f(program,location,v0);
}

void ecb_glProgramUniform1fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLfloat *value;
  std::vector <GLfloat> value_vec;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5689,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5689,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5689,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5689, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLfloat value_tmp;
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!egl_get_float(env, value_h, &value_tmp)) Badarg(5689,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglProgramUniform1fv(program,location,count,value);
}

void ecb_glProgramUniform1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLdouble v0;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5690,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5690,"location");
  if(!enif_get_double(env, argv[2],  &v0)) Badarg(5690,"v0");
  weglProgramUniform1d(program,location,v0);
}

void ecb_glProgramUniform1dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLdouble *value;
  std::vector <GLdouble> value_vec;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5691,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5691,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5691,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5691, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLdouble value_tmp;
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_double(env, value_h, &value_tmp)) Badarg(5691,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglProgramUniform1dv(program,location,count,value);
}

void ecb_glProgramUniform1ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint v0;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5692,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5692,"location");
  if(!enif_get_uint(env, argv[2],  &v0)) Badarg(5692,"v0");
  weglProgramUniform1ui(program,location,v0);
}

void ecb_glProgramUniform1uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint *value;
  std::vector <GLuint> value_vec;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5693,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5693,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5693,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5693, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLuint value_tmp;
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_uint(env, value_h, &value_tmp)) Badarg(5693,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglProgramUniform1uiv(program,location,count,value);
}

void ecb_glProgramUniform2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint v0;
  GLint v1;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5694,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5694,"location");
  if(!enif_get_int(env, argv[2],  &v0)) Badarg(5694,"v0");
  if(!enif_get_int(env, argv[3],  &v1)) Badarg(5694,"v1");
  weglProgramUniform2i(program,location,v0,v1);
}

void ecb_glProgramUniform2iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5695,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5695,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5695,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5695,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint> value_vec (2*count);
  GLint *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5695,"value");
      if(!enif_get_int(env, value_tpl[0], value_ptr++)) Badarg(5695,"value");
      if(!enif_get_int(env, value_tpl[1], value_ptr++)) Badarg(5695,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform2iv(program,location,count,value);
}

void ecb_glProgramUniform2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLfloat v0;
  GLfloat v1;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5696,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5696,"location");
  if(!egl_get_float(env, argv[2],  &v0)) Badarg(5696,"v0");
  if(!egl_get_float(env, argv[3],  &v1)) Badarg(5696,"v1");
  weglProgramUniform2f(program,location,v0,v1);
}

void ecb_glProgramUniform2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5697,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5697,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5697,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5697,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (2*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5697,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5697,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5697,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform2fv(program,location,count,value);
}

void ecb_glProgramUniform2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLdouble v0;
  GLdouble v1;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5698,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5698,"location");
  if(!enif_get_double(env, argv[2],  &v0)) Badarg(5698,"v0");
  if(!enif_get_double(env, argv[3],  &v1)) Badarg(5698,"v1");
  weglProgramUniform2d(program,location,v0,v1);
}

void ecb_glProgramUniform2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5699,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5699,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5699,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5699,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (2*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5699,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5699,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5699,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform2dv(program,location,count,value);
}

void ecb_glProgramUniform2ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint v0;
  GLuint v1;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5700,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5700,"location");
  if(!enif_get_uint(env, argv[2],  &v0)) Badarg(5700,"v0");
  if(!enif_get_uint(env, argv[3],  &v1)) Badarg(5700,"v1");
  weglProgramUniform2ui(program,location,v0,v1);
}

void ecb_glProgramUniform2uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5701,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5701,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5701,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5701,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint> value_vec (2*count);
  GLuint *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5701,"value");
      if(!enif_get_uint(env, value_tpl[0], value_ptr++)) Badarg(5701,"value");
      if(!enif_get_uint(env, value_tpl[1], value_ptr++)) Badarg(5701,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform2uiv(program,location,count,value);
}

void ecb_glProgramUniform3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint v0;
  GLint v1;
  GLint v2;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5702,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5702,"location");
  if(!enif_get_int(env, argv[2],  &v0)) Badarg(5702,"v0");
  if(!enif_get_int(env, argv[3],  &v1)) Badarg(5702,"v1");
  if(!enif_get_int(env, argv[4],  &v2)) Badarg(5702,"v2");
  weglProgramUniform3i(program,location,v0,v1,v2);
}

void ecb_glProgramUniform3iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5703,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5703,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5703,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5703,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint> value_vec (3*count);
  GLint *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5703,"value");
      if(!enif_get_int(env, value_tpl[0], value_ptr++)) Badarg(5703,"value");
      if(!enif_get_int(env, value_tpl[1], value_ptr++)) Badarg(5703,"value");
      if(!enif_get_int(env, value_tpl[2], value_ptr++)) Badarg(5703,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform3iv(program,location,count,value);
}

void ecb_glProgramUniform3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLfloat v0;
  GLfloat v1;
  GLfloat v2;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5704,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5704,"location");
  if(!egl_get_float(env, argv[2],  &v0)) Badarg(5704,"v0");
  if(!egl_get_float(env, argv[3],  &v1)) Badarg(5704,"v1");
  if(!egl_get_float(env, argv[4],  &v2)) Badarg(5704,"v2");
  weglProgramUniform3f(program,location,v0,v1,v2);
}

void ecb_glProgramUniform3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5705,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5705,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5705,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5705,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (3*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5705,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5705,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5705,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5705,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform3fv(program,location,count,value);
}

void ecb_glProgramUniform3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLdouble v0;
  GLdouble v1;
  GLdouble v2;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5706,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5706,"location");
  if(!enif_get_double(env, argv[2],  &v0)) Badarg(5706,"v0");
  if(!enif_get_double(env, argv[3],  &v1)) Badarg(5706,"v1");
  if(!enif_get_double(env, argv[4],  &v2)) Badarg(5706,"v2");
  weglProgramUniform3d(program,location,v0,v1,v2);
}

void ecb_glProgramUniform3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5707,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5707,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5707,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5707,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (3*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5707,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5707,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5707,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5707,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform3dv(program,location,count,value);
}

void ecb_glProgramUniform3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint v0;
  GLuint v1;
  GLuint v2;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5708,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5708,"location");
  if(!enif_get_uint(env, argv[2],  &v0)) Badarg(5708,"v0");
  if(!enif_get_uint(env, argv[3],  &v1)) Badarg(5708,"v1");
  if(!enif_get_uint(env, argv[4],  &v2)) Badarg(5708,"v2");
  weglProgramUniform3ui(program,location,v0,v1,v2);
}

void ecb_glProgramUniform3uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5709,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5709,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5709,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5709,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint> value_vec (3*count);
  GLuint *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5709,"value");
      if(!enif_get_uint(env, value_tpl[0], value_ptr++)) Badarg(5709,"value");
      if(!enif_get_uint(env, value_tpl[1], value_ptr++)) Badarg(5709,"value");
      if(!enif_get_uint(env, value_tpl[2], value_ptr++)) Badarg(5709,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform3uiv(program,location,count,value);
}

void ecb_glProgramUniform4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint v0;
  GLint v1;
  GLint v2;
  GLint v3;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5710,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5710,"location");
  if(!enif_get_int(env, argv[2],  &v0)) Badarg(5710,"v0");
  if(!enif_get_int(env, argv[3],  &v1)) Badarg(5710,"v1");
  if(!enif_get_int(env, argv[4],  &v2)) Badarg(5710,"v2");
  if(!enif_get_int(env, argv[5],  &v3)) Badarg(5710,"v3");
  weglProgramUniform4i(program,location,v0,v1,v2,v3);
}

void ecb_glProgramUniform4iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5711,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5711,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5711,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5711,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint> value_vec (4*count);
  GLint *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5711,"value");
      if(!enif_get_int(env, value_tpl[0], value_ptr++)) Badarg(5711,"value");
      if(!enif_get_int(env, value_tpl[1], value_ptr++)) Badarg(5711,"value");
      if(!enif_get_int(env, value_tpl[2], value_ptr++)) Badarg(5711,"value");
      if(!enif_get_int(env, value_tpl[3], value_ptr++)) Badarg(5711,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform4iv(program,location,count,value);
}

void ecb_glProgramUniform4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLfloat v0;
  GLfloat v1;
  GLfloat v2;
  GLfloat v3;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5712,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5712,"location");
  if(!egl_get_float(env, argv[2],  &v0)) Badarg(5712,"v0");
  if(!egl_get_float(env, argv[3],  &v1)) Badarg(5712,"v1");
  if(!egl_get_float(env, argv[4],  &v2)) Badarg(5712,"v2");
  if(!egl_get_float(env, argv[5],  &v3)) Badarg(5712,"v3");
  weglProgramUniform4f(program,location,v0,v1,v2,v3);
}

void ecb_glProgramUniform4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5713,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5713,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5713,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5713,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (4*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5713,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5713,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5713,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5713,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5713,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform4fv(program,location,count,value);
}

void ecb_glProgramUniform4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLdouble v0;
  GLdouble v1;
  GLdouble v2;
  GLdouble v3;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5714,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5714,"location");
  if(!enif_get_double(env, argv[2],  &v0)) Badarg(5714,"v0");
  if(!enif_get_double(env, argv[3],  &v1)) Badarg(5714,"v1");
  if(!enif_get_double(env, argv[4],  &v2)) Badarg(5714,"v2");
  if(!enif_get_double(env, argv[5],  &v3)) Badarg(5714,"v3");
  weglProgramUniform4d(program,location,v0,v1,v2,v3);
}

void ecb_glProgramUniform4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5715,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5715,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5715,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5715,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (4*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5715,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5715,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5715,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5715,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5715,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform4dv(program,location,count,value);
}

void ecb_glProgramUniform4ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint v0;
  GLuint v1;
  GLuint v2;
  GLuint v3;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5716,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5716,"location");
  if(!enif_get_uint(env, argv[2],  &v0)) Badarg(5716,"v0");
  if(!enif_get_uint(env, argv[3],  &v1)) Badarg(5716,"v1");
  if(!enif_get_uint(env, argv[4],  &v2)) Badarg(5716,"v2");
  if(!enif_get_uint(env, argv[5],  &v3)) Badarg(5716,"v3");
  weglProgramUniform4ui(program,location,v0,v1,v2,v3);
}

void ecb_glProgramUniform4uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5717,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5717,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5717,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5717,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint> value_vec (4*count);
  GLuint *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5717,"value");
      if(!enif_get_uint(env, value_tpl[0], value_ptr++)) Badarg(5717,"value");
      if(!enif_get_uint(env, value_tpl[1], value_ptr++)) Badarg(5717,"value");
      if(!enif_get_uint(env, value_tpl[2], value_ptr++)) Badarg(5717,"value");
      if(!enif_get_uint(env, value_tpl[3], value_ptr++)) Badarg(5717,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform4uiv(program,location,count,value);
}

void ecb_glProgramUniformMatrix2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5718,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5718,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5718,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5718,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5718,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (4*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5718,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5718,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5718,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5718,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5718,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix2fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5719,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5719,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5719,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5719,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5719,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (9*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 9) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5719,"value");
      if(!egl_get_float(env, value_tpl[8], value_ptr++)) Badarg(5719,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix3fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5720,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5720,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5720,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5720,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5720,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (16*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 16) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[8], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[9], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[10], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[11], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[12], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[13], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[14], value_ptr++)) Badarg(5720,"value");
      if(!egl_get_float(env, value_tpl[15], value_ptr++)) Badarg(5720,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix4fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5721,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5721,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5721,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5721,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5721,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (4*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5721,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5721,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5721,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5721,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5721,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix2dv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5722,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5722,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5722,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5722,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5722,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (9*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 9) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5722,"value");
      if(!enif_get_double(env, value_tpl[8], value_ptr++)) Badarg(5722,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix3dv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5723,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5723,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5723,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5723,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5723,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (16*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 16) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[8], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[9], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[10], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[11], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[12], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[13], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[14], value_ptr++)) Badarg(5723,"value");
      if(!enif_get_double(env, value_tpl[15], value_ptr++)) Badarg(5723,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix4dv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix2x3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5724,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5724,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5724,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5724,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5724,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (6*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) Badarg(5724,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5724,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5724,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5724,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5724,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5724,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5724,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix2x3fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix3x2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5725,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5725,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5725,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5725,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5725,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (6*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) Badarg(5725,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5725,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5725,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5725,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5725,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5725,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5725,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix3x2fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix2x4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5726,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5726,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5726,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5726,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5726,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (8*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) Badarg(5726,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5726,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5726,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5726,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5726,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5726,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5726,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5726,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5726,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix2x4fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix4x2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5727,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5727,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5727,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5727,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5727,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (8*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) Badarg(5727,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5727,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5727,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5727,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5727,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5727,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5727,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5727,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5727,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix4x2fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix3x4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5728,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5728,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5728,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5728,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5728,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (12*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[8], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[9], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[10], value_ptr++)) Badarg(5728,"value");
      if(!egl_get_float(env, value_tpl[11], value_ptr++)) Badarg(5728,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix3x4fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix4x3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5729,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5729,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5729,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5729,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5729,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLfloat> value_vec (12*count);
  GLfloat *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[0], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[1], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[2], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[3], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[4], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[5], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[6], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[7], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[8], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[9], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[10], value_ptr++)) Badarg(5729,"value");
      if(!egl_get_float(env, value_tpl[11], value_ptr++)) Badarg(5729,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix4x3fv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix2x3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5730,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5730,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5730,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5730,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5730,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (6*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) Badarg(5730,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5730,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5730,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5730,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5730,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5730,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5730,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix2x3dv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix3x2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5731,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5731,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5731,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5731,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5731,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (6*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) Badarg(5731,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5731,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5731,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5731,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5731,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5731,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5731,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix3x2dv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix2x4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5732,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5732,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5732,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5732,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5732,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (8*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) Badarg(5732,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5732,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5732,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5732,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5732,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5732,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5732,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5732,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5732,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix2x4dv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix4x2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5733,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5733,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5733,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5733,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5733,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (8*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) Badarg(5733,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5733,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5733,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5733,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5733,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5733,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5733,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5733,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5733,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix4x2dv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix3x4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5734,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5734,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5734,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5734,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5734,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (12*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[8], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[9], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[10], value_ptr++)) Badarg(5734,"value");
      if(!enif_get_double(env, value_tpl[11], value_ptr++)) Badarg(5734,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix3x4dv(program,location,count,transpose,value);
}

void ecb_glProgramUniformMatrix4x3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5735,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5735,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5735,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5735,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5735,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLdouble> value_vec (12*count);
  GLdouble *value_ptr = value_vec.data();
  value_l = argv[4];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[0], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[1], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[2], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[3], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[4], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[5], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[6], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[7], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[8], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[9], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[10], value_ptr++)) Badarg(5735,"value");
      if(!enif_get_double(env, value_tpl[11], value_ptr++)) Badarg(5735,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniformMatrix4x3dv(program,location,count,transpose,value);
}

void ecb_glValidateProgramPipeline(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint pipeline;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5736,"pipeline");
  weglValidateProgramPipeline(pipeline);
}

void ecb_glGetProgramPipelineInfoLog(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint pipeline;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *infoLog;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5737,"pipeline");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5737,"bufSize");
  infoLog = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetProgramPipelineInfoLog(pipeline,bufSize,&length,(GLchar *) infoLog);
  reply =      enif_make_string(env, (const char *) infoLog, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(infoLog);
}

void ecb_glVertexAttribL1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5738,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5738,"x");
  weglVertexAttribL1d(index,x);
}

void ecb_glVertexAttribL2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5739,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5739,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5739,"y");
  weglVertexAttribL2d(index,x,y);
}

void ecb_glVertexAttribL3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5740,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5740,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5740,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5740,"z");
  weglVertexAttribL3d(index,x,y,z);
}

void ecb_glVertexAttribL4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5741,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5741,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5741,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5741,"z");
  if(!enif_get_double(env, argv[4],  &w)) Badarg(5741,"w");
  weglVertexAttribL4d(index,x,y,z,w);
}

void ecb_glVertexAttribLPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary pointer;
  void *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5742,"index");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5742,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5742,"type");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5742,"stride");
  if(!egl_get_ptr(env, argv[4], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[4], &pointer))
        pointer_idx = (void *) pointer.data;
    else Badarg(5742,"pointer");
  }
  weglVertexAttribLPointer(index,size,type,stride,pointer_idx);
}

void ecb_glGetVertexAttribLdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5744,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5744,"pname");
  weglGetVertexAttribLdv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glViewportArrayv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLfloat *v;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5745,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5745,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5745,"v")}
  int v_a;
  const ERL_NIF_TERM *v_tpl;
  ERL_NIF_TERM v_l, v_h, v_t;
  std::vector <GLfloat> v_vec (4*count);
  GLfloat *v_ptr = v_vec.data();
  v_l = argv[2];
  while(enif_get_list_cell(env, v_l, &v_h, &v_t)) {
      if(!enif_get_tuple(env, v_h, &v_a, &v_tpl) || v_a != 4) Badarg(5745,"v");
      if(!egl_get_float(env, v_tpl[0], v_ptr++)) Badarg(5745,"v");
      if(!egl_get_float(env, v_tpl[1], v_ptr++)) Badarg(5745,"v");
      if(!egl_get_float(env, v_tpl[2], v_ptr++)) Badarg(5745,"v");
      if(!egl_get_float(env, v_tpl[3], v_ptr++)) Badarg(5745,"v");
      v_l = v_t;
    };
  v = v_vec.data();
  weglViewportArrayv(first,count,v);
}

void ecb_glViewportIndexedf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat w;
  GLfloat h;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5746,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5746,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5746,"y");
  if(!egl_get_float(env, argv[3],  &w)) Badarg(5746,"w");
  if(!egl_get_float(env, argv[4],  &h)) Badarg(5746,"h");
  weglViewportIndexedf(index,x,y,w,h);
}

void ecb_glViewportIndexedfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5747,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5747,"v");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, v_t[i1++], &v[0])) Badarg(5747,"v");
     if(!egl_get_float(env, v_t[i1++], &v[1])) Badarg(5747,"v");
     if(!egl_get_float(env, v_t[i1++], &v[2])) Badarg(5747,"v");
     if(!egl_get_float(env, v_t[i1++], &v[3])) Badarg(5747,"v");
   }};
  weglViewportIndexedfv(index,v);
}

void ecb_glScissorArrayv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLint *v;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5748,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5748,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5748,"v")}
  int v_a;
  const ERL_NIF_TERM *v_tpl;
  ERL_NIF_TERM v_l, v_h, v_t;
  std::vector <GLint> v_vec (4*count);
  GLint *v_ptr = v_vec.data();
  v_l = argv[2];
  while(enif_get_list_cell(env, v_l, &v_h, &v_t)) {
      if(!enif_get_tuple(env, v_h, &v_a, &v_tpl) || v_a != 4) Badarg(5748,"v");
      if(!enif_get_int(env, v_tpl[0], v_ptr++)) Badarg(5748,"v");
      if(!enif_get_int(env, v_tpl[1], v_ptr++)) Badarg(5748,"v");
      if(!enif_get_int(env, v_tpl[2], v_ptr++)) Badarg(5748,"v");
      if(!enif_get_int(env, v_tpl[3], v_ptr++)) Badarg(5748,"v");
      v_l = v_t;
    };
  v = v_vec.data();
  weglScissorArrayv(first,count,v);
}

void ecb_glScissorIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint left;
  GLint bottom;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5749,"index");
  if(!enif_get_int(env, argv[1],  &left)) Badarg(5749,"left");
  if(!enif_get_int(env, argv[2],  &bottom)) Badarg(5749,"bottom");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5749,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5749,"height");
  weglScissorIndexed(index,left,bottom,width,height);
}

void ecb_glScissorIndexedv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5750,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5750,"v");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v_t[i1++], &v[0])) Badarg(5750,"v");
     if(!enif_get_int(env, v_t[i1++], &v[1])) Badarg(5750,"v");
     if(!enif_get_int(env, v_t[i1++], &v[2])) Badarg(5750,"v");
     if(!enif_get_int(env, v_t[i1++], &v[3])) Badarg(5750,"v");
   }};
  weglScissorIndexedv(index,v);
}

void ecb_glDepthRangeArrayv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLdouble *v;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5751,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5751,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5751,"v")}
  int v_a;
  const ERL_NIF_TERM *v_tpl;
  ERL_NIF_TERM v_l, v_h, v_t;
  std::vector <GLdouble> v_vec (2*count);
  GLdouble *v_ptr = v_vec.data();
  v_l = argv[2];
  while(enif_get_list_cell(env, v_l, &v_h, &v_t)) {
      if(!enif_get_tuple(env, v_h, &v_a, &v_tpl) || v_a != 2) Badarg(5751,"v");
      if(!enif_get_double(env, v_tpl[0], v_ptr++)) Badarg(5751,"v");
      if(!enif_get_double(env, v_tpl[1], v_ptr++)) Badarg(5751,"v");
      v_l = v_t;
    };
  v = v_vec.data();
  weglDepthRangeArrayv(first,count,v);
}

void ecb_glDepthRangeIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble n;
  GLdouble f;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5752,"index");
  if(!enif_get_double(env, argv[1],  &n)) Badarg(5752,"n");
  if(!enif_get_double(env, argv[2],  &f)) Badarg(5752,"f");
  weglDepthRangeIndexed(index,n,f);
}

void ecb_glGetFloati_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLfloat data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5753,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5753,"index");
  weglGetFloati_v(target,index,data);
  for(int ri=0; ri < (int) 16; ri++)
     data_ts[ri] =      enif_make_double(env, (double) data[ri]);
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetDoublei_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLdouble data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5754,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5754,"index");
  weglGetDoublei_v(target,index,data);
  for(int ri=0; ri < (int) 16; ri++)
     data_ts[ri] =      enif_make_double(env, data[ri]);
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDrawArraysInstancedBaseInstance(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint first;
  GLsizei count;
  GLsizei instancecount;
  GLuint baseinstance;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5755,"mode");
  if(!enif_get_int(env, argv[1],  &first)) Badarg(5755,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5755,"count");
  if(!enif_get_int(env, argv[3],  &instancecount)) Badarg(5755,"instancecount");
  if(!enif_get_uint(env, argv[4],  &baseinstance)) Badarg(5755,"baseinstance");
  weglDrawArraysInstancedBaseInstance(mode,first,count,instancecount,baseinstance);
}

void ecb_glDrawElementsInstancedBaseInstance(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  void *indices_idx;
  GLsizei instancecount;
  GLuint baseinstance;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5756,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5756,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5756,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (void *) indices.data;
    else Badarg(5756,"indices");
  }
  if(!enif_get_int(env, argv[4],  &instancecount)) Badarg(5756,"instancecount");
  if(!enif_get_uint(env, argv[5],  &baseinstance)) Badarg(5756,"baseinstance");
  weglDrawElementsInstancedBaseInstance(mode,count,type,indices_idx,instancecount,baseinstance);
}

void ecb_glDrawElementsInstancedBaseVertexBaseInstance(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  void *indices_idx;
  GLsizei instancecount;
  GLint basevertex;
  GLuint baseinstance;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5758,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5758,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5758,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (void *) indices.data;
    else Badarg(5758,"indices");
  }
  if(!enif_get_int(env, argv[4],  &instancecount)) Badarg(5758,"instancecount");
  if(!enif_get_int(env, argv[5],  &basevertex)) Badarg(5758,"basevertex");
  if(!enif_get_uint(env, argv[6],  &baseinstance)) Badarg(5758,"baseinstance");
  weglDrawElementsInstancedBaseVertexBaseInstance(mode,count,type,indices_idx,instancecount,basevertex,baseinstance);
}

void ecb_glGetInternalformativ(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum internalformat;
  GLenum pname;
  GLsizei bufSize;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5760,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5760,"internalformat");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5760,"pname");
  if(!enif_get_int(env, argv[3],  &bufSize)) Badarg(5760,"bufSize");
  std::vector <GLint> params (bufSize);
  std::vector <ERL_NIF_TERM> params_ts (bufSize);
  weglGetInternalformativ(target,internalformat,pname,bufSize,params.data());
  for(int ri=0; ri < (int) bufSize; ri++)
    params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_list_from_array(env, params_ts.data(), bufSize);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBindImageTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint unit;
  GLuint texture;
  GLint level;
  GLboolean layered;
  GLint layer;
  GLenum access;
  GLenum format;
  if(!enif_get_uint(env, argv[0],  &unit)) Badarg(5761,"unit");
  if(!enif_get_uint(env, argv[1],  &texture)) Badarg(5761,"texture");
  if(!enif_get_int(env, argv[2],  &level)) Badarg(5761,"level");
  if(!egl_get_ubyte(env, argv[3],  &layered)) Badarg(5761,"layered");
  if(!enif_get_int(env, argv[4],  &layer)) Badarg(5761,"layer");
  if(!enif_get_uint(env, argv[5],  &access)) Badarg(5761,"access");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5761,"format");
  weglBindImageTexture(unit,texture,level,layered,layer,access,format);
}

void ecb_glMemoryBarrier(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield barriers;
  if(!enif_get_uint(env, argv[0],  &barriers)) Badarg(5762,"barriers");
  weglMemoryBarrier(barriers);
}

void ecb_glTexStorage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei levels;
  GLenum internalformat;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5763,"target");
  if(!enif_get_int(env, argv[1],  &levels)) Badarg(5763,"levels");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5763,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5763,"width");
  weglTexStorage1D(target,levels,internalformat,width);
}

void ecb_glTexStorage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei levels;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5764,"target");
  if(!enif_get_int(env, argv[1],  &levels)) Badarg(5764,"levels");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5764,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5764,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5764,"height");
  weglTexStorage2D(target,levels,internalformat,width,height);
}

void ecb_glTexStorage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei levels;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5765,"target");
  if(!enif_get_int(env, argv[1],  &levels)) Badarg(5765,"levels");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5765,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5765,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5765,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5765,"depth");
  weglTexStorage3D(target,levels,internalformat,width,height,depth);
}

void ecb_glDrawTransformFeedbackInstanced(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint id;
  GLsizei instancecount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5766,"mode");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5766,"id");
  if(!enif_get_int(env, argv[2],  &instancecount)) Badarg(5766,"instancecount");
  weglDrawTransformFeedbackInstanced(mode,id,instancecount);
}

void ecb_glDrawTransformFeedbackStreamInstanced(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint id;
  GLuint stream;
  GLsizei instancecount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5767,"mode");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5767,"id");
  if(!enif_get_uint(env, argv[2],  &stream)) Badarg(5767,"stream");
  if(!enif_get_int(env, argv[3],  &instancecount)) Badarg(5767,"instancecount");
  weglDrawTransformFeedbackStreamInstanced(mode,id,stream,instancecount);
}

void ecb_glClearBufferData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  void *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5768,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5768,"internalformat");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5768,"format");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5768,"type");
  if(!egl_get_ptr(env, argv[4], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[4], &data))
        data_idx = (void *) data.data;
    else Badarg(5768,"data");
  }
  weglClearBufferData(target,internalformat,format,type,data_idx);
}

void ecb_glClearBufferSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLintptr offset;
  GLsizeiptr size;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  void *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5770,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5770,"internalformat");
  if(!egl_get_word(env, argv[2], (egl_word *) &offset)) Badarg(5770,"offset");
  if(!egl_get_word(env, argv[3], (egl_word *) &size)) Badarg(5770,"size");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5770,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5770,"type");
  if(!egl_get_ptr(env, argv[6], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &data))
        data_idx = (void *) data.data;
    else Badarg(5770,"data");
  }
  weglClearBufferSubData(target,internalformat,offset,size,format,type,data_idx);
}

void ecb_glDispatchCompute(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint num_groups_x;
  GLuint num_groups_y;
  GLuint num_groups_z;
  if(!enif_get_uint(env, argv[0],  &num_groups_x)) Badarg(5772,"num_groups_x");
  if(!enif_get_uint(env, argv[1],  &num_groups_y)) Badarg(5772,"num_groups_y");
  if(!enif_get_uint(env, argv[2],  &num_groups_z)) Badarg(5772,"num_groups_z");
  weglDispatchCompute(num_groups_x,num_groups_y,num_groups_z);
}

void ecb_glDispatchComputeIndirect(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLintptr indirect;
  if(!egl_get_word(env, argv[0], (egl_word *) &indirect)) Badarg(5773,"indirect");
  weglDispatchComputeIndirect(indirect);
}

void ecb_glCopyImageSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint srcName;
  GLenum srcTarget;
  GLint srcLevel;
  GLint srcX;
  GLint srcY;
  GLint srcZ;
  GLuint dstName;
  GLenum dstTarget;
  GLint dstLevel;
  GLint dstX;
  GLint dstY;
  GLint dstZ;
  GLsizei srcWidth;
  GLsizei srcHeight;
  GLsizei srcDepth;
  if(!enif_get_uint(env, argv[0],  &srcName)) Badarg(5774,"srcName");
  if(!enif_get_uint(env, argv[1],  &srcTarget)) Badarg(5774,"srcTarget");
  if(!enif_get_int(env, argv[2],  &srcLevel)) Badarg(5774,"srcLevel");
  if(!enif_get_int(env, argv[3],  &srcX)) Badarg(5774,"srcX");
  if(!enif_get_int(env, argv[4],  &srcY)) Badarg(5774,"srcY");
  if(!enif_get_int(env, argv[5],  &srcZ)) Badarg(5774,"srcZ");
  if(!enif_get_uint(env, argv[6],  &dstName)) Badarg(5774,"dstName");
  if(!enif_get_uint(env, argv[7],  &dstTarget)) Badarg(5774,"dstTarget");
  if(!enif_get_int(env, argv[8],  &dstLevel)) Badarg(5774,"dstLevel");
  if(!enif_get_int(env, argv[9],  &dstX)) Badarg(5774,"dstX");
  if(!enif_get_int(env, argv[10],  &dstY)) Badarg(5774,"dstY");
  if(!enif_get_int(env, argv[11],  &dstZ)) Badarg(5774,"dstZ");
  if(!enif_get_int(env, argv[12],  &srcWidth)) Badarg(5774,"srcWidth");
  if(!enif_get_int(env, argv[13],  &srcHeight)) Badarg(5774,"srcHeight");
  if(!enif_get_int(env, argv[14],  &srcDepth)) Badarg(5774,"srcDepth");
  weglCopyImageSubData(srcName,srcTarget,srcLevel,srcX,srcY,srcZ,dstName,dstTarget,dstLevel,dstX,dstY,dstZ,srcWidth,srcHeight,srcDepth);
}

void ecb_glFramebufferParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5775,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5775,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5775,"param");
  weglFramebufferParameteri(target,pname,param);
}

void ecb_glGetFramebufferParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5776,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5776,"pname");
  weglGetFramebufferParameteriv(target,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetInternalformati64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum internalformat;
  GLenum pname;
  GLsizei bufSize;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5777,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5777,"internalformat");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5777,"pname");
  if(!enif_get_int(env, argv[3],  &bufSize)) Badarg(5777,"bufSize");
  std::vector <GLint64> params (bufSize);
  std::vector <ERL_NIF_TERM> params_ts (bufSize);
  weglGetInternalformati64v(target,internalformat,pname,bufSize,params.data());
  for(int ri=0; ri < (int) bufSize; ri++)
    params_ts[ri] =      enif_make_int64(env, (egl_int64_t) params[ri]);
  reply =      enif_make_list_from_array(env, params_ts.data(), bufSize);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glInvalidateTexSubImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5778,"texture");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5778,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5778,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5778,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5778,"zoffset");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5778,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5778,"height");
  if(!enif_get_int(env, argv[7],  &depth)) Badarg(5778,"depth");
  weglInvalidateTexSubImage(texture,level,xoffset,yoffset,zoffset,width,height,depth);
}

void ecb_glInvalidateTexImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLint level;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5779,"texture");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5779,"level");
  weglInvalidateTexImage(texture,level);
}

void ecb_glInvalidateBufferSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buffer;
  GLintptr offset;
  GLsizeiptr length;
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5780,"buffer");
  if(!egl_get_word(env, argv[1], (egl_word *) &offset)) Badarg(5780,"offset");
  if(!egl_get_word(env, argv[2], (egl_word *) &length)) Badarg(5780,"length");
  weglInvalidateBufferSubData(buffer,offset,length);
}

void ecb_glInvalidateBufferData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5781,"buffer");
  weglInvalidateBufferData(buffer);
}

void ecb_glInvalidateFramebuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei numAttachments;
  GLenum *attachments;
  std::vector <GLenum> attachments_vec;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5782,"target");
  if(!enif_get_int(env, argv[1],  &numAttachments)) Badarg(5782,"numAttachments");
  if(!enif_is_list(env, argv[2])) Badarg(5782, "attachments")
  else {
    ERL_NIF_TERM attachments_l, attachments_h, attachments_t;
    GLenum attachments_tmp;
    attachments_l = argv[2];
    while(enif_get_list_cell(env, attachments_l, &attachments_h, &attachments_t)) {
        if(!enif_get_uint(env, attachments_h, &attachments_tmp)) Badarg(5782,"attachments");
        attachments_vec.push_back(attachments_tmp);
        attachments_l = attachments_t;
    };
    attachments = attachments_vec.data();
  }
  weglInvalidateFramebuffer(target,numAttachments,attachments);
}

void ecb_glInvalidateSubFramebuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei numAttachments;
  GLenum *attachments;
  std::vector <GLenum> attachments_vec;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5783,"target");
  if(!enif_get_int(env, argv[1],  &numAttachments)) Badarg(5783,"numAttachments");
  if(!enif_is_list(env, argv[2])) Badarg(5783, "attachments")
  else {
    ERL_NIF_TERM attachments_l, attachments_h, attachments_t;
    GLenum attachments_tmp;
    attachments_l = argv[2];
    while(enif_get_list_cell(env, attachments_l, &attachments_h, &attachments_t)) {
        if(!enif_get_uint(env, attachments_h, &attachments_tmp)) Badarg(5783,"attachments");
        attachments_vec.push_back(attachments_tmp);
        attachments_l = attachments_t;
    };
    attachments = attachments_vec.data();
  }
  if(!enif_get_int(env, argv[3],  &x)) Badarg(5783,"x");
  if(!enif_get_int(env, argv[4],  &y)) Badarg(5783,"y");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5783,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5783,"height");
  weglInvalidateSubFramebuffer(target,numAttachments,attachments,x,y,width,height);
}

void ecb_glMultiDrawArraysIndirect(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  ErlNifBinary indirect;
  void *indirect_idx;
  GLsizei drawcount;
  GLsizei stride;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5784,"mode");
  if(!egl_get_ptr(env, argv[1], (void **) &indirect_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[1], &indirect))
        indirect_idx = (void *) indirect.data;
    else Badarg(5784,"indirect");
  }
  if(!enif_get_int(env, argv[2],  &drawcount)) Badarg(5784,"drawcount");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5784,"stride");
  weglMultiDrawArraysIndirect(mode,indirect_idx,drawcount,stride);
}

void ecb_glGetProgramInterfaceiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum programInterface;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5786,"program");
  if(!enif_get_uint(env, argv[1],  &programInterface)) Badarg(5786,"programInterface");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5786,"pname");
  weglGetProgramInterfaceiv(program,programInterface,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramResourceIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum programInterface;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5787,"program");
  if(!enif_get_uint(env, argv[1],  &programInterface)) Badarg(5787,"programInterface");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5787,"name");
  result = weglGetProgramResourceIndex(program,programInterface,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramResourceName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum programInterface;
  GLuint index;
  GLsizei bufSize;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5788,"program");
  if(!enif_get_uint(env, argv[1],  &programInterface)) Badarg(5788,"programInterface");
  if(!enif_get_uint(env, argv[2],  &index)) Badarg(5788,"index");
  if(!enif_get_int(env, argv[3],  &bufSize)) Badarg(5788,"bufSize");
  name = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetProgramResourceName(program,programInterface,index,bufSize,NULL,(GLchar *) name);
  reply =      enif_make_string(env, (const char *) name, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

void ecb_glGetProgramResourceLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum programInterface;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5789,"program");
  if(!enif_get_uint(env, argv[1],  &programInterface)) Badarg(5789,"programInterface");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5789,"name");
  result = weglGetProgramResourceLocation(program,programInterface,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramResourceLocationIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum programInterface;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5790,"program");
  if(!enif_get_uint(env, argv[1],  &programInterface)) Badarg(5790,"programInterface");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5790,"name");
  result = weglGetProgramResourceLocationIndex(program,programInterface,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glShaderStorageBlockBinding(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint storageBlockIndex;
  GLuint storageBlockBinding;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5791,"program");
  if(!enif_get_uint(env, argv[1],  &storageBlockIndex)) Badarg(5791,"storageBlockIndex");
  if(!enif_get_uint(env, argv[2],  &storageBlockBinding)) Badarg(5791,"storageBlockBinding");
  weglShaderStorageBlockBinding(program,storageBlockIndex,storageBlockBinding);
}

void ecb_glTexBufferRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLuint buffer;
  GLintptr offset;
  GLsizeiptr size;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5792,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5792,"internalformat");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5792,"buffer");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5792,"offset");
  if(!egl_get_word(env, argv[4], (egl_word *) &size)) Badarg(5792,"size");
  weglTexBufferRange(target,internalformat,buffer,offset,size);
}

void ecb_glTexStorage2DMultisample(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei samples;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLboolean fixedsamplelocations;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5793,"target");
  if(!enif_get_int(env, argv[1],  &samples)) Badarg(5793,"samples");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5793,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5793,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5793,"height");
  if(!egl_get_ubyte(env, argv[5],  &fixedsamplelocations)) Badarg(5793,"fixedsamplelocations");
  weglTexStorage2DMultisample(target,samples,internalformat,width,height,fixedsamplelocations);
}

void ecb_glTexStorage3DMultisample(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei samples;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLboolean fixedsamplelocations;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5794,"target");
  if(!enif_get_int(env, argv[1],  &samples)) Badarg(5794,"samples");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5794,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5794,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5794,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5794,"depth");
  if(!egl_get_ubyte(env, argv[6],  &fixedsamplelocations)) Badarg(5794,"fixedsamplelocations");
  weglTexStorage3DMultisample(target,samples,internalformat,width,height,depth,fixedsamplelocations);
}

void ecb_glTextureView(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLenum target;
  GLuint origtexture;
  GLenum internalformat;
  GLuint minlevel;
  GLuint numlevels;
  GLuint minlayer;
  GLuint numlayers;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5795,"texture");
  if(!enif_get_uint(env, argv[1],  &target)) Badarg(5795,"target");
  if(!enif_get_uint(env, argv[2],  &origtexture)) Badarg(5795,"origtexture");
  if(!enif_get_uint(env, argv[3],  &internalformat)) Badarg(5795,"internalformat");
  if(!enif_get_uint(env, argv[4],  &minlevel)) Badarg(5795,"minlevel");
  if(!enif_get_uint(env, argv[5],  &numlevels)) Badarg(5795,"numlevels");
  if(!enif_get_uint(env, argv[6],  &minlayer)) Badarg(5795,"minlayer");
  if(!enif_get_uint(env, argv[7],  &numlayers)) Badarg(5795,"numlayers");
  weglTextureView(texture,target,origtexture,internalformat,minlevel,numlevels,minlayer,numlayers);
}

void ecb_glBindVertexBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint bindingindex;
  GLuint buffer;
  GLintptr offset;
  GLsizei stride;
  if(!enif_get_uint(env, argv[0],  &bindingindex)) Badarg(5796,"bindingindex");
  if(!enif_get_uint(env, argv[1],  &buffer)) Badarg(5796,"buffer");
  if(!egl_get_word(env, argv[2], (egl_word *) &offset)) Badarg(5796,"offset");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5796,"stride");
  weglBindVertexBuffer(bindingindex,buffer,offset,stride);
}

void ecb_glVertexAttribFormat(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint attribindex;
  GLint size;
  GLenum type;
  GLboolean normalized;
  GLuint relativeoffset;
  if(!enif_get_uint(env, argv[0],  &attribindex)) Badarg(5797,"attribindex");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5797,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5797,"type");
  if(!egl_get_ubyte(env, argv[3],  &normalized)) Badarg(5797,"normalized");
  if(!enif_get_uint(env, argv[4],  &relativeoffset)) Badarg(5797,"relativeoffset");
  weglVertexAttribFormat(attribindex,size,type,normalized,relativeoffset);
}

void ecb_glVertexAttribIFormat(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint attribindex;
  GLint size;
  GLenum type;
  GLuint relativeoffset;
  if(!enif_get_uint(env, argv[0],  &attribindex)) Badarg(5798,"attribindex");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5798,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5798,"type");
  if(!enif_get_uint(env, argv[3],  &relativeoffset)) Badarg(5798,"relativeoffset");
  weglVertexAttribIFormat(attribindex,size,type,relativeoffset);
}

void ecb_glVertexAttribLFormat(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint attribindex;
  GLint size;
  GLenum type;
  GLuint relativeoffset;
  if(!enif_get_uint(env, argv[0],  &attribindex)) Badarg(5799,"attribindex");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5799,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5799,"type");
  if(!enif_get_uint(env, argv[3],  &relativeoffset)) Badarg(5799,"relativeoffset");
  weglVertexAttribLFormat(attribindex,size,type,relativeoffset);
}

void ecb_glVertexAttribBinding(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint attribindex;
  GLuint bindingindex;
  if(!enif_get_uint(env, argv[0],  &attribindex)) Badarg(5800,"attribindex");
  if(!enif_get_uint(env, argv[1],  &bindingindex)) Badarg(5800,"bindingindex");
  weglVertexAttribBinding(attribindex,bindingindex);
}

void ecb_glVertexBindingDivisor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint bindingindex;
  GLuint divisor;
  if(!enif_get_uint(env, argv[0],  &bindingindex)) Badarg(5801,"bindingindex");
  if(!enif_get_uint(env, argv[1],  &divisor)) Badarg(5801,"divisor");
  weglVertexBindingDivisor(bindingindex,divisor);
}

void ecb_glDebugMessageControl(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum source;
  GLenum type;
  GLenum severity;
  GLsizei count;
  GLuint *ids;
  std::vector <GLuint> ids_vec;
  GLboolean enabled;
  if(!enif_get_uint(env, argv[0],  &source)) Badarg(5802,"source");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5802,"type");
  if(!enif_get_uint(env, argv[2],  &severity)) Badarg(5802,"severity");
  if(!enif_get_int(env, argv[3],  &count)) Badarg(5802,"count");
  if(!enif_is_list(env, argv[4])) Badarg(5802, "ids")
  else {
    ERL_NIF_TERM ids_l, ids_h, ids_t;
    GLuint ids_tmp;
    ids_l = argv[4];
    while(enif_get_list_cell(env, ids_l, &ids_h, &ids_t)) {
        if(!enif_get_uint(env, ids_h, &ids_tmp)) Badarg(5802,"ids");
        ids_vec.push_back(ids_tmp);
        ids_l = ids_t;
    };
    ids = ids_vec.data();
  }
  if(!egl_get_ubyte(env, argv[5],  &enabled)) Badarg(5802,"enabled");
  weglDebugMessageControl(source,type,severity,count,ids,enabled);
}

void ecb_glDebugMessageInsert(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum source;
  GLenum type;
  GLuint id;
  GLenum severity;
  ErlNifBinary buf;
  if(!enif_get_uint(env, argv[0],  &source)) Badarg(5803,"source");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5803,"type");
  if(!enif_get_uint(env, argv[2],  &id)) Badarg(5803,"id");
  if(!enif_get_uint(env, argv[3],  &severity)) Badarg(5803,"severity");
  if(!enif_inspect_binary(env, argv[4], &buf)) Badarg(5803,"buf");
  weglDebugMessageInsert(source,type,id,severity,(GLsizei) buf.size,(GLchar *) buf.data);
}

void ecb_glGetDebugMessageLog(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLuint count;
  GLsizei bufSize;
  unsigned char *messageLog;
  if(!enif_get_uint(env, argv[0],  &count)) Badarg(5804,"count");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5804,"bufSize");
  std::vector <GLenum> sources (count);
  std::vector <ERL_NIF_TERM> sources_ts (count);
  std::vector <GLenum> types (count);
  std::vector <ERL_NIF_TERM> types_ts (count);
  std::vector <GLuint> ids (count);
  std::vector <ERL_NIF_TERM> ids_ts (count);
  std::vector <GLenum> severities (count);
  std::vector <ERL_NIF_TERM> severities_ts (count);
  std::vector <GLsizei> lengths (count);
  std::vector <ERL_NIF_TERM> lengths_ts (count);
  messageLog = (unsigned char *) enif_alloc((int) bufSize*sizeof(GLchar));
  unsigned char *messageLog_ptr = messageLog;
  std::vector <ERL_NIF_TERM> messageLog_ts (count);
  result = weglGetDebugMessageLog(count,bufSize,sources.data(),types.data(),ids.data(),severities.data(),lengths.data(),(GLchar *) messageLog);
  for(int ri=0; ri < (int) result; ri++)
    sources_ts[ri] =      enif_make_int(env, sources[ri]);
  for(int ri=0; ri < (int) result; ri++)
    types_ts[ri] =      enif_make_int(env, types[ri]);
  for(int ri=0; ri < (int) result; ri++)
    ids_ts[ri] =      enif_make_int(env, ids[ri]);
  for(int ri=0; ri < (int) result; ri++)
    severities_ts[ri] =      enif_make_int(env, severities[ri]);
  for(int ri=0; ri < (int) result; ri++) {
    messageLog_ts[ri] =      enif_make_string(env, (const char *) messageLog, ERL_NIF_LATIN1);
    messageLog += lengths[ri];
   }
  reply = enif_make_tuple6(env,
          enif_make_int(env, result),
     enif_make_list_from_array(env, sources_ts.data(), result),
     enif_make_list_from_array(env, types_ts.data(), result),
     enif_make_list_from_array(env, ids_ts.data(), result),
     enif_make_list_from_array(env, severities_ts.data(), result),
     enif_make_list_from_array(env, messageLog_ts.data(), result) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(messageLog_ptr);
}

void ecb_glPushDebugGroup(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum source;
  GLuint id;
  GLsizei length;
  ErlNifBinary message;
  if(!enif_get_uint(env, argv[0],  &source)) Badarg(5805,"source");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5805,"id");
  if(!enif_get_int(env, argv[2],  &length)) Badarg(5805,"length");
  if(!enif_inspect_binary(env, argv[3], &message)) Badarg(5805,"message");
  weglPushDebugGroup(source,id,length,(GLchar *) message.data);
}

void ecb_glPopDebugGroup(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopDebugGroup();
}

void ecb_glObjectPtrLabel(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ErlNifBinary ptr;
  void *ptr_idx;
  GLsizei length;
  ErlNifBinary label;
  if(!egl_get_ptr(env, argv[0], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[0], &ptr))
        ptr_idx = (void *) ptr.data;
    else Badarg(5807,"ptr");
  }
  if(!enif_get_int(env, argv[1],  &length)) Badarg(5807,"length");
  if(!enif_inspect_binary(env, argv[2], &label)) Badarg(5807,"label");
  weglObjectPtrLabel(ptr_idx,length,(GLchar *) label.data);
}

void ecb_glBufferStorage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizeiptr size;
  ErlNifBinary data;
  void *data_idx;
  GLbitfield flags;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5809,"target");
  if(!egl_get_word(env, argv[1], (egl_word *) &size)) Badarg(5809,"size");
  if(!egl_get_ptr(env, argv[2], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &data))
        data_idx = (void *) data.data;
    else Badarg(5809,"data");
  }
  if(!enif_get_uint(env, argv[3],  &flags)) Badarg(5809,"flags");
  weglBufferStorage(target,size,data_idx,flags);
}

void ecb_glClearTexImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLint level;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  void *data_idx;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5811,"texture");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5811,"level");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5811,"format");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5811,"type");
  if(!egl_get_ptr(env, argv[4], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[4], &data))
        data_idx = (void *) data.data;
    else Badarg(5811,"data");
  }
  weglClearTexImage(texture,level,format,type,data_idx);
}

void ecb_glClearTexSubImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  void *data_idx;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5813,"texture");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5813,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5813,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5813,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5813,"zoffset");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5813,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5813,"height");
  if(!enif_get_int(env, argv[7],  &depth)) Badarg(5813,"depth");
  if(!enif_get_uint(env, argv[8],  &format)) Badarg(5813,"format");
  if(!enif_get_uint(env, argv[9],  &type)) Badarg(5813,"type");
  if(!egl_get_ptr(env, argv[10], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[10], &data))
        data_idx = (void *) data.data;
    else Badarg(5813,"data");
  }
  weglClearTexSubImage(texture,level,xoffset,yoffset,zoffset,width,height,depth,format,type,data_idx);
}

void ecb_glBindBuffersBase(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint first;
  GLsizei count;
  GLuint *buffers;
  std::vector <GLuint> buffers_vec;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5815,"target");
  if(!enif_get_uint(env, argv[1],  &first)) Badarg(5815,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5815,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5815, "buffers")
  else {
    ERL_NIF_TERM buffers_l, buffers_h, buffers_t;
    GLuint buffers_tmp;
    buffers_l = argv[3];
    while(enif_get_list_cell(env, buffers_l, &buffers_h, &buffers_t)) {
        if(!enif_get_uint(env, buffers_h, &buffers_tmp)) Badarg(5815,"buffers");
        buffers_vec.push_back(buffers_tmp);
        buffers_l = buffers_t;
    };
    buffers = buffers_vec.data();
  }
  weglBindBuffersBase(target,first,count,buffers);
}

void ecb_glBindBuffersRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint first;
  GLsizei count;
  GLuint *buffers;
  std::vector <GLuint> buffers_vec;
  GLintptr *offsets;
  std::vector <GLintptr> offsets_vec;
  GLsizeiptr *sizes;
  std::vector <GLsizeiptr> sizes_vec;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5816,"target");
  if(!enif_get_uint(env, argv[1],  &first)) Badarg(5816,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5816,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5816, "buffers")
  else {
    ERL_NIF_TERM buffers_l, buffers_h, buffers_t;
    GLuint buffers_tmp;
    buffers_l = argv[3];
    while(enif_get_list_cell(env, buffers_l, &buffers_h, &buffers_t)) {
        if(!enif_get_uint(env, buffers_h, &buffers_tmp)) Badarg(5816,"buffers");
        buffers_vec.push_back(buffers_tmp);
        buffers_l = buffers_t;
    };
    buffers = buffers_vec.data();
  }
  if(!enif_is_list(env, argv[4])) Badarg(5816, "offsets")
  else {
    ERL_NIF_TERM offsets_l, offsets_h, offsets_t;
    GLintptr offsets_tmp;
    offsets_l = argv[4];
    while(enif_get_list_cell(env, offsets_l, &offsets_h, &offsets_t)) {
        if(!egl_get_word(env, offsets_h,(egl_word *) &offsets_tmp)) Badarg(5816,"offsets");
        offsets_vec.push_back(offsets_tmp);
        offsets_l = offsets_t;
    };
    offsets = offsets_vec.data();
  }
  if(!enif_is_list(env, argv[5])) Badarg(5816, "sizes")
  else {
    ERL_NIF_TERM sizes_l, sizes_h, sizes_t;
    GLsizeiptr sizes_tmp;
    sizes_l = argv[5];
    while(enif_get_list_cell(env, sizes_l, &sizes_h, &sizes_t)) {
        if(!egl_get_word(env, sizes_h,(egl_word *) &sizes_tmp)) Badarg(5816,"sizes");
        sizes_vec.push_back(sizes_tmp);
        sizes_l = sizes_t;
    };
    sizes = sizes_vec.data();
  }
  weglBindBuffersRange(target,first,count,buffers,offsets,sizes);
}

void ecb_glBindTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLuint *textures;
  std::vector <GLuint> textures_vec;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5817,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5817,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5817, "textures")
  else {
    ERL_NIF_TERM textures_l, textures_h, textures_t;
    GLuint textures_tmp;
    textures_l = argv[2];
    while(enif_get_list_cell(env, textures_l, &textures_h, &textures_t)) {
        if(!enif_get_uint(env, textures_h, &textures_tmp)) Badarg(5817,"textures");
        textures_vec.push_back(textures_tmp);
        textures_l = textures_t;
    };
    textures = textures_vec.data();
  }
  weglBindTextures(first,count,textures);
}

void ecb_glBindSamplers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLuint *samplers;
  std::vector <GLuint> samplers_vec;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5818,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5818,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5818, "samplers")
  else {
    ERL_NIF_TERM samplers_l, samplers_h, samplers_t;
    GLuint samplers_tmp;
    samplers_l = argv[2];
    while(enif_get_list_cell(env, samplers_l, &samplers_h, &samplers_t)) {
        if(!enif_get_uint(env, samplers_h, &samplers_tmp)) Badarg(5818,"samplers");
        samplers_vec.push_back(samplers_tmp);
        samplers_l = samplers_t;
    };
    samplers = samplers_vec.data();
  }
  weglBindSamplers(first,count,samplers);
}

void ecb_glBindImageTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLuint *textures;
  std::vector <GLuint> textures_vec;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5819,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5819,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5819, "textures")
  else {
    ERL_NIF_TERM textures_l, textures_h, textures_t;
    GLuint textures_tmp;
    textures_l = argv[2];
    while(enif_get_list_cell(env, textures_l, &textures_h, &textures_t)) {
        if(!enif_get_uint(env, textures_h, &textures_tmp)) Badarg(5819,"textures");
        textures_vec.push_back(textures_tmp);
        textures_l = textures_t;
    };
    textures = textures_vec.data();
  }
  weglBindImageTextures(first,count,textures);
}

void ecb_glBindVertexBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLuint *buffers;
  std::vector <GLuint> buffers_vec;
  GLintptr *offsets;
  std::vector <GLintptr> offsets_vec;
  GLsizei *strides;
  std::vector <GLsizei> strides_vec;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5820,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5820,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5820, "buffers")
  else {
    ERL_NIF_TERM buffers_l, buffers_h, buffers_t;
    GLuint buffers_tmp;
    buffers_l = argv[2];
    while(enif_get_list_cell(env, buffers_l, &buffers_h, &buffers_t)) {
        if(!enif_get_uint(env, buffers_h, &buffers_tmp)) Badarg(5820,"buffers");
        buffers_vec.push_back(buffers_tmp);
        buffers_l = buffers_t;
    };
    buffers = buffers_vec.data();
  }
  if(!enif_is_list(env, argv[3])) Badarg(5820, "offsets")
  else {
    ERL_NIF_TERM offsets_l, offsets_h, offsets_t;
    GLintptr offsets_tmp;
    offsets_l = argv[3];
    while(enif_get_list_cell(env, offsets_l, &offsets_h, &offsets_t)) {
        if(!egl_get_word(env, offsets_h,(egl_word *) &offsets_tmp)) Badarg(5820,"offsets");
        offsets_vec.push_back(offsets_tmp);
        offsets_l = offsets_t;
    };
    offsets = offsets_vec.data();
  }
  if(!enif_is_list(env, argv[4])) Badarg(5820, "strides")
  else {
    ERL_NIF_TERM strides_l, strides_h, strides_t;
    GLsizei strides_tmp;
    strides_l = argv[4];
    while(enif_get_list_cell(env, strides_l, &strides_h, &strides_t)) {
        if(!enif_get_int(env, strides_h, &strides_tmp)) Badarg(5820,"strides");
        strides_vec.push_back(strides_tmp);
        strides_l = strides_t;
    };
    strides = strides_vec.data();
  }
  weglBindVertexBuffers(first,count,buffers,offsets,strides);
}

void ecb_glClipControl(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum origin;
  GLenum depth;
  if(!enif_get_uint(env, argv[0],  &origin)) Badarg(5821,"origin");
  if(!enif_get_uint(env, argv[1],  &depth)) Badarg(5821,"depth");
  weglClipControl(origin,depth);
}

void ecb_glCreateTransformFeedbacks(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5822,"n");
  std::vector <GLuint> ids (n);
  std::vector <ERL_NIF_TERM> ids_ts (n);
  weglCreateTransformFeedbacks(n,ids.data());
  for(int ri=0; ri < (int) n; ri++)
    ids_ts[ri] =      enif_make_int(env, ids[ri]);
  reply =      enif_make_list_from_array(env, ids_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glTransformFeedbackBufferBase(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint xfb;
  GLuint index;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &xfb)) Badarg(5823,"xfb");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5823,"index");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5823,"buffer");
  weglTransformFeedbackBufferBase(xfb,index,buffer);
}

void ecb_glTransformFeedbackBufferRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint xfb;
  GLuint index;
  GLuint buffer;
  GLintptr offset;
  GLsizeiptr size;
  if(!enif_get_uint(env, argv[0],  &xfb)) Badarg(5824,"xfb");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5824,"index");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5824,"buffer");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5824,"offset");
  if(!egl_get_word(env, argv[4], (egl_word *) &size)) Badarg(5824,"size");
  weglTransformFeedbackBufferRange(xfb,index,buffer,offset,size);
}

void ecb_glCreateBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5825,"n");
  std::vector <GLuint> buffers (n);
  std::vector <ERL_NIF_TERM> buffers_ts (n);
  weglCreateBuffers(n,buffers.data());
  for(int ri=0; ri < (int) n; ri++)
    buffers_ts[ri] =      enif_make_int(env, buffers[ri]);
  reply =      enif_make_list_from_array(env, buffers_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glFlushMappedNamedBufferRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buffer;
  GLintptr offset;
  GLsizeiptr length;
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5826,"buffer");
  if(!egl_get_word(env, argv[1], (egl_word *) &offset)) Badarg(5826,"offset");
  if(!egl_get_word(env, argv[2], (egl_word *) &length)) Badarg(5826,"length");
  weglFlushMappedNamedBufferRange(buffer,offset,length);
}

void ecb_glCreateFramebuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5827,"n");
  std::vector <GLuint> framebuffers (n);
  std::vector <ERL_NIF_TERM> framebuffers_ts (n);
  weglCreateFramebuffers(n,framebuffers.data());
  for(int ri=0; ri < (int) n; ri++)
    framebuffers_ts[ri] =      enif_make_int(env, framebuffers[ri]);
  reply =      enif_make_list_from_array(env, framebuffers_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glCreateRenderbuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5828,"n");
  std::vector <GLuint> renderbuffers (n);
  std::vector <ERL_NIF_TERM> renderbuffers_ts (n);
  weglCreateRenderbuffers(n,renderbuffers.data());
  for(int ri=0; ri < (int) n; ri++)
    renderbuffers_ts[ri] =      enif_make_int(env, renderbuffers[ri]);
  reply =      enif_make_list_from_array(env, renderbuffers_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glCreateTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLsizei n;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5829,"target");
  if(!enif_get_int(env, argv[1],  &n)) Badarg(5829,"n");
  std::vector <GLuint> textures (n);
  std::vector <ERL_NIF_TERM> textures_ts (n);
  weglCreateTextures(target,n,textures.data());
  for(int ri=0; ri < (int) n; ri++)
    textures_ts[ri] =      enif_make_int(env, textures[ri]);
  reply =      enif_make_list_from_array(env, textures_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glTextureBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLenum internalformat;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5830,"texture");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5830,"internalformat");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5830,"buffer");
  weglTextureBuffer(texture,internalformat,buffer);
}

void ecb_glTextureBufferRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLenum internalformat;
  GLuint buffer;
  GLintptr offset;
  GLsizeiptr size;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5831,"texture");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5831,"internalformat");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5831,"buffer");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5831,"offset");
  if(!egl_get_word(env, argv[4], (egl_word *) &size)) Badarg(5831,"size");
  weglTextureBufferRange(texture,internalformat,buffer,offset,size);
}

void ecb_glCompressedTextureSubImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLint level;
  GLint xoffset;
  GLsizei width;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  void *data_idx;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5832,"texture");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5832,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5832,"xoffset");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5832,"width");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5832,"format");
  if(!enif_get_int(env, argv[5],  &imageSize)) Badarg(5832,"imageSize");
  if(!egl_get_ptr(env, argv[6], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &data))
        data_idx = (void *) data.data;
    else Badarg(5832,"data");
  }
  weglCompressedTextureSubImage1D(texture,level,xoffset,width,format,imageSize,data_idx);
}

void ecb_glCompressedTextureSubImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  void *data_idx;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5834,"texture");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5834,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5834,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5834,"yoffset");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5834,"width");
  if(!enif_get_int(env, argv[5],  &height)) Badarg(5834,"height");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5834,"format");
  if(!enif_get_int(env, argv[7],  &imageSize)) Badarg(5834,"imageSize");
  if(!egl_get_ptr(env, argv[8], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &data))
        data_idx = (void *) data.data;
    else Badarg(5834,"data");
  }
  weglCompressedTextureSubImage2D(texture,level,xoffset,yoffset,width,height,format,imageSize,data_idx);
}

void ecb_glCompressedTextureSubImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  void *data_idx;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5836,"texture");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5836,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5836,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5836,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5836,"zoffset");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5836,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5836,"height");
  if(!enif_get_int(env, argv[7],  &depth)) Badarg(5836,"depth");
  if(!enif_get_uint(env, argv[8],  &format)) Badarg(5836,"format");
  if(!enif_get_int(env, argv[9],  &imageSize)) Badarg(5836,"imageSize");
  if(!egl_get_ptr(env, argv[10], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[10], &data))
        data_idx = (void *) data.data;
    else Badarg(5836,"data");
  }
  weglCompressedTextureSubImage3D(texture,level,xoffset,yoffset,zoffset,width,height,depth,format,imageSize,data_idx);
}

void ecb_glGenerateTextureMipmap(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint texture;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5838,"texture");
  weglGenerateTextureMipmap(texture);
}

void ecb_glBindTextureUnit(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint unit;
  GLuint texture;
  if(!enif_get_uint(env, argv[0],  &unit)) Badarg(5839,"unit");
  if(!enif_get_uint(env, argv[1],  &texture)) Badarg(5839,"texture");
  weglBindTextureUnit(unit,texture);
}

void ecb_glCreateVertexArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5840,"n");
  std::vector <GLuint> arrays (n);
  std::vector <ERL_NIF_TERM> arrays_ts (n);
  weglCreateVertexArrays(n,arrays.data());
  for(int ri=0; ri < (int) n; ri++)
    arrays_ts[ri] =      enif_make_int(env, arrays[ri]);
  reply =      enif_make_list_from_array(env, arrays_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDisableVertexArrayAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5841,"vaobj");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5841,"index");
  weglDisableVertexArrayAttrib(vaobj,index);
}

void ecb_glEnableVertexArrayAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5842,"vaobj");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5842,"index");
  weglEnableVertexArrayAttrib(vaobj,index);
}

void ecb_glVertexArrayElementBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5843,"vaobj");
  if(!enif_get_uint(env, argv[1],  &buffer)) Badarg(5843,"buffer");
  weglVertexArrayElementBuffer(vaobj,buffer);
}

void ecb_glVertexArrayVertexBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint bindingindex;
  GLuint buffer;
  GLintptr offset;
  GLsizei stride;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5844,"vaobj");
  if(!enif_get_uint(env, argv[1],  &bindingindex)) Badarg(5844,"bindingindex");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5844,"buffer");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5844,"offset");
  if(!enif_get_int(env, argv[4],  &stride)) Badarg(5844,"stride");
  weglVertexArrayVertexBuffer(vaobj,bindingindex,buffer,offset,stride);
}

void ecb_glVertexArrayVertexBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint first;
  GLsizei count;
  GLuint *buffers;
  std::vector <GLuint> buffers_vec;
  GLintptr *offsets;
  std::vector <GLintptr> offsets_vec;
  GLsizei *strides;
  std::vector <GLsizei> strides_vec;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5845,"vaobj");
  if(!enif_get_uint(env, argv[1],  &first)) Badarg(5845,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5845,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5845, "buffers")
  else {
    ERL_NIF_TERM buffers_l, buffers_h, buffers_t;
    GLuint buffers_tmp;
    buffers_l = argv[3];
    while(enif_get_list_cell(env, buffers_l, &buffers_h, &buffers_t)) {
        if(!enif_get_uint(env, buffers_h, &buffers_tmp)) Badarg(5845,"buffers");
        buffers_vec.push_back(buffers_tmp);
        buffers_l = buffers_t;
    };
    buffers = buffers_vec.data();
  }
  if(!enif_is_list(env, argv[4])) Badarg(5845, "offsets")
  else {
    ERL_NIF_TERM offsets_l, offsets_h, offsets_t;
    GLintptr offsets_tmp;
    offsets_l = argv[4];
    while(enif_get_list_cell(env, offsets_l, &offsets_h, &offsets_t)) {
        if(!egl_get_word(env, offsets_h,(egl_word *) &offsets_tmp)) Badarg(5845,"offsets");
        offsets_vec.push_back(offsets_tmp);
        offsets_l = offsets_t;
    };
    offsets = offsets_vec.data();
  }
  if(!enif_is_list(env, argv[5])) Badarg(5845, "strides")
  else {
    ERL_NIF_TERM strides_l, strides_h, strides_t;
    GLsizei strides_tmp;
    strides_l = argv[5];
    while(enif_get_list_cell(env, strides_l, &strides_h, &strides_t)) {
        if(!enif_get_int(env, strides_h, &strides_tmp)) Badarg(5845,"strides");
        strides_vec.push_back(strides_tmp);
        strides_l = strides_t;
    };
    strides = strides_vec.data();
  }
  weglVertexArrayVertexBuffers(vaobj,first,count,buffers,offsets,strides);
}

void ecb_glVertexArrayAttribBinding(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint attribindex;
  GLuint bindingindex;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5846,"vaobj");
  if(!enif_get_uint(env, argv[1],  &attribindex)) Badarg(5846,"attribindex");
  if(!enif_get_uint(env, argv[2],  &bindingindex)) Badarg(5846,"bindingindex");
  weglVertexArrayAttribBinding(vaobj,attribindex,bindingindex);
}

void ecb_glVertexArrayAttribFormat(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint attribindex;
  GLint size;
  GLenum type;
  GLboolean normalized;
  GLuint relativeoffset;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5847,"vaobj");
  if(!enif_get_uint(env, argv[1],  &attribindex)) Badarg(5847,"attribindex");
  if(!enif_get_int(env, argv[2],  &size)) Badarg(5847,"size");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5847,"type");
  if(!egl_get_ubyte(env, argv[4],  &normalized)) Badarg(5847,"normalized");
  if(!enif_get_uint(env, argv[5],  &relativeoffset)) Badarg(5847,"relativeoffset");
  weglVertexArrayAttribFormat(vaobj,attribindex,size,type,normalized,relativeoffset);
}

void ecb_glVertexArrayAttribIFormat(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint attribindex;
  GLint size;
  GLenum type;
  GLuint relativeoffset;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5848,"vaobj");
  if(!enif_get_uint(env, argv[1],  &attribindex)) Badarg(5848,"attribindex");
  if(!enif_get_int(env, argv[2],  &size)) Badarg(5848,"size");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5848,"type");
  if(!enif_get_uint(env, argv[4],  &relativeoffset)) Badarg(5848,"relativeoffset");
  weglVertexArrayAttribIFormat(vaobj,attribindex,size,type,relativeoffset);
}

void ecb_glVertexArrayAttribLFormat(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint attribindex;
  GLint size;
  GLenum type;
  GLuint relativeoffset;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5849,"vaobj");
  if(!enif_get_uint(env, argv[1],  &attribindex)) Badarg(5849,"attribindex");
  if(!enif_get_int(env, argv[2],  &size)) Badarg(5849,"size");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5849,"type");
  if(!enif_get_uint(env, argv[4],  &relativeoffset)) Badarg(5849,"relativeoffset");
  weglVertexArrayAttribLFormat(vaobj,attribindex,size,type,relativeoffset);
}

void ecb_glVertexArrayBindingDivisor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint vaobj;
  GLuint bindingindex;
  GLuint divisor;
  if(!enif_get_uint(env, argv[0],  &vaobj)) Badarg(5850,"vaobj");
  if(!enif_get_uint(env, argv[1],  &bindingindex)) Badarg(5850,"bindingindex");
  if(!enif_get_uint(env, argv[2],  &divisor)) Badarg(5850,"divisor");
  weglVertexArrayBindingDivisor(vaobj,bindingindex,divisor);
}

void ecb_glCreateSamplers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5851,"n");
  std::vector <GLuint> samplers (n);
  std::vector <ERL_NIF_TERM> samplers_ts (n);
  weglCreateSamplers(n,samplers.data());
  for(int ri=0; ri < (int) n; ri++)
    samplers_ts[ri] =      enif_make_int(env, samplers[ri]);
  reply =      enif_make_list_from_array(env, samplers_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glCreateProgramPipelines(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5852,"n");
  std::vector <GLuint> pipelines (n);
  std::vector <ERL_NIF_TERM> pipelines_ts (n);
  weglCreateProgramPipelines(n,pipelines.data());
  for(int ri=0; ri < (int) n; ri++)
    pipelines_ts[ri] =      enif_make_int(env, pipelines[ri]);
  reply =      enif_make_list_from_array(env, pipelines_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glCreateQueries(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLsizei n;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5853,"target");
  if(!enif_get_int(env, argv[1],  &n)) Badarg(5853,"n");
  std::vector <GLuint> ids (n);
  std::vector <ERL_NIF_TERM> ids_ts (n);
  weglCreateQueries(target,n,ids.data());
  for(int ri=0; ri < (int) n; ri++)
    ids_ts[ri] =      enif_make_int(env, ids[ri]);
  reply =      enif_make_list_from_array(env, ids_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetQueryBufferObjecti64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint id;
  GLuint buffer;
  GLenum pname;
  GLintptr offset;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5854,"id");
  if(!enif_get_uint(env, argv[1],  &buffer)) Badarg(5854,"buffer");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5854,"pname");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5854,"offset");
  weglGetQueryBufferObjecti64v(id,buffer,pname,offset);
}

void ecb_glGetQueryBufferObjectiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint id;
  GLuint buffer;
  GLenum pname;
  GLintptr offset;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5855,"id");
  if(!enif_get_uint(env, argv[1],  &buffer)) Badarg(5855,"buffer");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5855,"pname");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5855,"offset");
  weglGetQueryBufferObjectiv(id,buffer,pname,offset);
}

void ecb_glGetQueryBufferObjectui64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint id;
  GLuint buffer;
  GLenum pname;
  GLintptr offset;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5856,"id");
  if(!enif_get_uint(env, argv[1],  &buffer)) Badarg(5856,"buffer");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5856,"pname");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5856,"offset");
  weglGetQueryBufferObjectui64v(id,buffer,pname,offset);
}

void ecb_glGetQueryBufferObjectuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint id;
  GLuint buffer;
  GLenum pname;
  GLintptr offset;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5857,"id");
  if(!enif_get_uint(env, argv[1],  &buffer)) Badarg(5857,"buffer");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5857,"pname");
  if(!egl_get_word(env, argv[3], (egl_word *) &offset)) Badarg(5857,"offset");
  weglGetQueryBufferObjectuiv(id,buffer,pname,offset);
}

void ecb_glMemoryBarrierByRegion(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield barriers;
  if(!enif_get_uint(env, argv[0],  &barriers)) Badarg(5858,"barriers");
  weglMemoryBarrierByRegion(barriers);
}

void ecb_glGetGraphicsResetStatus(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum result;
  ERL_NIF_TERM reply;
  result = weglGetGraphicsResetStatus();
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glTextureBarrier(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglTextureBarrier();
}

void ecb_glMultiDrawArraysIndirectCount(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  ErlNifBinary indirect;
  void *indirect_idx;
  GLintptr drawcount;
  GLsizei maxdrawcount;
  GLsizei stride;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5861,"mode");
  if(!egl_get_ptr(env, argv[1], (void **) &indirect_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[1], &indirect))
        indirect_idx = (void *) indirect.data;
    else Badarg(5861,"indirect");
  }
  if(!egl_get_word(env, argv[2], (egl_word *) &drawcount)) Badarg(5861,"drawcount");
  if(!enif_get_int(env, argv[3],  &maxdrawcount)) Badarg(5861,"maxdrawcount");
  if(!enif_get_int(env, argv[4],  &stride)) Badarg(5861,"stride");
  weglMultiDrawArraysIndirectCount(mode,indirect_idx,drawcount,maxdrawcount,stride);
}

void ecb_glPolygonOffsetClamp(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat factor;
  GLfloat units;
  GLfloat clamp;
  if(!egl_get_float(env, argv[0],  &factor)) Badarg(5863,"factor");
  if(!egl_get_float(env, argv[1],  &units)) Badarg(5863,"units");
  if(!egl_get_float(env, argv[2],  &clamp)) Badarg(5863,"clamp");
  weglPolygonOffsetClamp(factor,units,clamp);
}

void ecb_glPrimitiveBoundingBoxARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat minX;
  GLfloat minY;
  GLfloat minZ;
  GLfloat minW;
  GLfloat maxX;
  GLfloat maxY;
  GLfloat maxZ;
  GLfloat maxW;
  if(!egl_get_float(env, argv[0],  &minX)) Badarg(5864,"minX");
  if(!egl_get_float(env, argv[1],  &minY)) Badarg(5864,"minY");
  if(!egl_get_float(env, argv[2],  &minZ)) Badarg(5864,"minZ");
  if(!egl_get_float(env, argv[3],  &minW)) Badarg(5864,"minW");
  if(!egl_get_float(env, argv[4],  &maxX)) Badarg(5864,"maxX");
  if(!egl_get_float(env, argv[5],  &maxY)) Badarg(5864,"maxY");
  if(!egl_get_float(env, argv[6],  &maxZ)) Badarg(5864,"maxZ");
  if(!egl_get_float(env, argv[7],  &maxW)) Badarg(5864,"maxW");
  weglPrimitiveBoundingBoxARB(minX,minY,minZ,minW,maxX,maxY,maxZ,maxW);
}

void ecb_glMakeTextureHandleResidentARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint64 handle;
  if(!enif_get_uint64(env, argv[0], (egl_uint64_t *) &handle)) Badarg(5865,"handle");
  weglMakeTextureHandleResidentARB(handle);
}

void ecb_glMakeTextureHandleNonResidentARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint64 handle;
  if(!enif_get_uint64(env, argv[0], (egl_uint64_t *) &handle)) Badarg(5866,"handle");
  weglMakeTextureHandleNonResidentARB(handle);
}

void ecb_glGetImageHandleARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint64 result;
  ERL_NIF_TERM reply;
  GLuint texture;
  GLint level;
  GLboolean layered;
  GLint layer;
  GLenum format;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5867,"texture");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5867,"level");
  if(!egl_get_ubyte(env, argv[2],  &layered)) Badarg(5867,"layered");
  if(!enif_get_int(env, argv[3],  &layer)) Badarg(5867,"layer");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5867,"format");
  result = weglGetImageHandleARB(texture,level,layered,layer,format);
  reply =      enif_make_int64(env, (egl_int64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glMakeImageHandleResidentARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint64 handle;
  GLenum access;
  if(!enif_get_uint64(env, argv[0], (egl_uint64_t *) &handle)) Badarg(5868,"handle");
  if(!enif_get_uint(env, argv[1],  &access)) Badarg(5868,"access");
  weglMakeImageHandleResidentARB(handle,access);
}

void ecb_glMakeImageHandleNonResidentARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint64 handle;
  if(!enif_get_uint64(env, argv[0], (egl_uint64_t *) &handle)) Badarg(5869,"handle");
  weglMakeImageHandleNonResidentARB(handle);
}

void ecb_glUniformHandleui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint64 value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5870,"location");
  if(!enif_get_uint64(env, argv[1], (egl_uint64_t *) &value)) Badarg(5870,"value");
  weglUniformHandleui64ARB(location,value);
}

void ecb_glProgramUniformHandleui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint64 value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5871,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5871,"location");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &value)) Badarg(5871,"value");
  weglProgramUniformHandleui64ARB(program,location,value);
}

void ecb_glIsTextureHandleResidentARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint64 handle;
  if(!enif_get_uint64(env, argv[0], (egl_uint64_t *) &handle)) Badarg(5872,"handle");
  result = weglIsTextureHandleResidentARB(handle);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glIsImageHandleResidentARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint64 handle;
  if(!enif_get_uint64(env, argv[0], (egl_uint64_t *) &handle)) Badarg(5873,"handle");
  result = weglIsImageHandleResidentARB(handle);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDispatchComputeGroupSizeARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint num_groups_x;
  GLuint num_groups_y;
  GLuint num_groups_z;
  GLuint group_size_x;
  GLuint group_size_y;
  GLuint group_size_z;
  if(!enif_get_uint(env, argv[0],  &num_groups_x)) Badarg(5874,"num_groups_x");
  if(!enif_get_uint(env, argv[1],  &num_groups_y)) Badarg(5874,"num_groups_y");
  if(!enif_get_uint(env, argv[2],  &num_groups_z)) Badarg(5874,"num_groups_z");
  if(!enif_get_uint(env, argv[3],  &group_size_x)) Badarg(5874,"group_size_x");
  if(!enif_get_uint(env, argv[4],  &group_size_y)) Badarg(5874,"group_size_y");
  if(!enif_get_uint(env, argv[5],  &group_size_z)) Badarg(5874,"group_size_z");
  weglDispatchComputeGroupSizeARB(num_groups_x,num_groups_y,num_groups_z,group_size_x,group_size_y,group_size_z);
}

void ecb_glProgramStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum format;
  ErlNifBinary string;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5875,"target");
  if(!enif_get_uint(env, argv[1],  &format)) Badarg(5875,"format");
  if(!enif_inspect_binary(env, argv[2], &string)) Badarg(5875,"string");
  weglProgramStringARB(target,format,(GLsizei) string.size,(void *) string.data);
}

void ecb_glBindProgramARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5876,"target");
  if(!enif_get_uint(env, argv[1],  &program)) Badarg(5876,"program");
  weglBindProgramARB(target,program);
}

void ecb_glDeleteProgramsARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  GLuint *programs;
  std::vector <GLuint> programs_vec;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5877,"n");
  if(!enif_is_list(env, argv[1])) Badarg(5877, "programs")
  else {
    ERL_NIF_TERM programs_l, programs_h, programs_t;
    GLuint programs_tmp;
    programs_l = argv[1];
    while(enif_get_list_cell(env, programs_l, &programs_h, &programs_t)) {
        if(!enif_get_uint(env, programs_h, &programs_tmp)) Badarg(5877,"programs");
        programs_vec.push_back(programs_tmp);
        programs_l = programs_t;
    };
    programs = programs_vec.data();
  }
  weglDeleteProgramsARB(n,programs);
}

void ecb_glGenProgramsARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5878,"n");
  std::vector <GLuint> programs (n);
  std::vector <ERL_NIF_TERM> programs_ts (n);
  weglGenProgramsARB(n,programs.data());
  for(int ri=0; ri < (int) n; ri++)
    programs_ts[ri] =      enif_make_int(env, programs[ri]);
  reply =      enif_make_list_from_array(env, programs_ts.data(), n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glProgramEnvParameter4dARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5879,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5879,"index");
  if(!enif_get_double(env, argv[2],  &x)) Badarg(5879,"x");
  if(!enif_get_double(env, argv[3],  &y)) Badarg(5879,"y");
  if(!enif_get_double(env, argv[4],  &z)) Badarg(5879,"z");
  if(!enif_get_double(env, argv[5],  &w)) Badarg(5879,"w");
  weglProgramEnvParameter4dARB(target,index,x,y,z,w);
}

void ecb_glProgramEnvParameter4dvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5880,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5880,"index");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5880,"params");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, params_t[i1++], &params[0])) Badarg(5880,"params");
     if(!enif_get_double(env, params_t[i1++], &params[1])) Badarg(5880,"params");
     if(!enif_get_double(env, params_t[i1++], &params[2])) Badarg(5880,"params");
     if(!enif_get_double(env, params_t[i1++], &params[3])) Badarg(5880,"params");
   }};
  weglProgramEnvParameter4dvARB(target,index,params);
}

void ecb_glProgramEnvParameter4fARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5881,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5881,"index");
  if(!egl_get_float(env, argv[2],  &x)) Badarg(5881,"x");
  if(!egl_get_float(env, argv[3],  &y)) Badarg(5881,"y");
  if(!egl_get_float(env, argv[4],  &z)) Badarg(5881,"z");
  if(!egl_get_float(env, argv[5],  &w)) Badarg(5881,"w");
  weglProgramEnvParameter4fARB(target,index,x,y,z,w);
}

void ecb_glProgramEnvParameter4fvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5882,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5882,"index");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5882,"params");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, params_t[i1++], &params[0])) Badarg(5882,"params");
     if(!egl_get_float(env, params_t[i1++], &params[1])) Badarg(5882,"params");
     if(!egl_get_float(env, params_t[i1++], &params[2])) Badarg(5882,"params");
     if(!egl_get_float(env, params_t[i1++], &params[3])) Badarg(5882,"params");
   }};
  weglProgramEnvParameter4fvARB(target,index,params);
}

void ecb_glProgramLocalParameter4dARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5883,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5883,"index");
  if(!enif_get_double(env, argv[2],  &x)) Badarg(5883,"x");
  if(!enif_get_double(env, argv[3],  &y)) Badarg(5883,"y");
  if(!enif_get_double(env, argv[4],  &z)) Badarg(5883,"z");
  if(!enif_get_double(env, argv[5],  &w)) Badarg(5883,"w");
  weglProgramLocalParameter4dARB(target,index,x,y,z,w);
}

void ecb_glProgramLocalParameter4dvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5884,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5884,"index");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5884,"params");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, params_t[i1++], &params[0])) Badarg(5884,"params");
     if(!enif_get_double(env, params_t[i1++], &params[1])) Badarg(5884,"params");
     if(!enif_get_double(env, params_t[i1++], &params[2])) Badarg(5884,"params");
     if(!enif_get_double(env, params_t[i1++], &params[3])) Badarg(5884,"params");
   }};
  weglProgramLocalParameter4dvARB(target,index,params);
}

void ecb_glProgramLocalParameter4fARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5885,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5885,"index");
  if(!egl_get_float(env, argv[2],  &x)) Badarg(5885,"x");
  if(!egl_get_float(env, argv[3],  &y)) Badarg(5885,"y");
  if(!egl_get_float(env, argv[4],  &z)) Badarg(5885,"z");
  if(!egl_get_float(env, argv[5],  &w)) Badarg(5885,"w");
  weglProgramLocalParameter4fARB(target,index,x,y,z,w);
}

void ecb_glProgramLocalParameter4fvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5886,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5886,"index");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5886,"params");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, params_t[i1++], &params[0])) Badarg(5886,"params");
     if(!egl_get_float(env, params_t[i1++], &params[1])) Badarg(5886,"params");
     if(!egl_get_float(env, params_t[i1++], &params[2])) Badarg(5886,"params");
     if(!egl_get_float(env, params_t[i1++], &params[3])) Badarg(5886,"params");
   }};
  weglProgramLocalParameter4fvARB(target,index,params);
}

void ecb_glGetProgramEnvParameterdvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5887,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5887,"index");
  weglGetProgramEnvParameterdvARB(target,index,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramEnvParameterfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5888,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5888,"index");
  weglGetProgramEnvParameterfvARB(target,index,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramLocalParameterdvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5889,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5889,"index");
  weglGetProgramLocalParameterdvARB(target,index,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramLocalParameterfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5890,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5890,"index");
  weglGetProgramLocalParameterfvARB(target,index,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetProgramStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  ErlNifBinary string;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5891,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5891,"pname");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &string);
  else if(enif_is_tuple(env, argv[2])) {
    int string_a;
    const ERL_NIF_TERM *string_t;
    if(enif_get_tuple(env, argv[2], &string_a, &string_t) &&
         enif_is_binary(env, string_t[1]))
       enif_inspect_binary(env, string_t[1], &string);
    else Badarg(5891, "string");
  } else Badarg(5891, "string");
  weglGetProgramStringARB(target,pname,(void *) string.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glFramebufferTextureFaceARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLuint texture;
  GLint level;
  GLenum face;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5892,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5892,"attachment");
  if(!enif_get_uint(env, argv[2],  &texture)) Badarg(5892,"texture");
  if(!enif_get_int(env, argv[3],  &level)) Badarg(5892,"level");
  if(!enif_get_uint(env, argv[4],  &face)) Badarg(5892,"face");
  weglFramebufferTextureFaceARB(target,attachment,texture,level,face);
}

void ecb_glUniform1i64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint64 x;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5893,"location");
  if(!enif_get_int64(env, argv[1], (egl_int64_t *) &x)) Badarg(5893,"x");
  weglUniform1i64ARB(location,x);
}

void ecb_glUniform2i64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint64 x;
  GLint64 y;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5894,"location");
  if(!enif_get_int64(env, argv[1], (egl_int64_t *) &x)) Badarg(5894,"x");
  if(!enif_get_int64(env, argv[2], (egl_int64_t *) &y)) Badarg(5894,"y");
  weglUniform2i64ARB(location,x,y);
}

void ecb_glUniform3i64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint64 x;
  GLint64 y;
  GLint64 z;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5895,"location");
  if(!enif_get_int64(env, argv[1], (egl_int64_t *) &x)) Badarg(5895,"x");
  if(!enif_get_int64(env, argv[2], (egl_int64_t *) &y)) Badarg(5895,"y");
  if(!enif_get_int64(env, argv[3], (egl_int64_t *) &z)) Badarg(5895,"z");
  weglUniform3i64ARB(location,x,y,z);
}

void ecb_glUniform4i64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint64 x;
  GLint64 y;
  GLint64 z;
  GLint64 w;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5896,"location");
  if(!enif_get_int64(env, argv[1], (egl_int64_t *) &x)) Badarg(5896,"x");
  if(!enif_get_int64(env, argv[2], (egl_int64_t *) &y)) Badarg(5896,"y");
  if(!enif_get_int64(env, argv[3], (egl_int64_t *) &z)) Badarg(5896,"z");
  if(!enif_get_int64(env, argv[4], (egl_int64_t *) &w)) Badarg(5896,"w");
  weglUniform4i64ARB(location,x,y,z,w);
}

void ecb_glUniform1i64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint64 *value;
  std::vector <GLint64> value_vec;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5897,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5897,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5897, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLint64 value_tmp;
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_int64(env, value_h,(egl_int64_t *) &value_tmp)) Badarg(5897,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglUniform1i64vARB(location,count,value);
}

void ecb_glUniform2i64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint64 *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5898,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5898,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5898,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint64> value_vec (2*count);
  GLint64 *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5898,"value");
      if(!enif_get_int64(env, value_tpl[0],(egl_int64_t *) value_ptr++)) Badarg(5898,"value");
      if(!enif_get_int64(env, value_tpl[1],(egl_int64_t *) value_ptr++)) Badarg(5898,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform2i64vARB(location,count,value);
}

void ecb_glUniform3i64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint64 *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5899,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5899,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5899,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint64> value_vec (3*count);
  GLint64 *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5899,"value");
      if(!enif_get_int64(env, value_tpl[0],(egl_int64_t *) value_ptr++)) Badarg(5899,"value");
      if(!enif_get_int64(env, value_tpl[1],(egl_int64_t *) value_ptr++)) Badarg(5899,"value");
      if(!enif_get_int64(env, value_tpl[2],(egl_int64_t *) value_ptr++)) Badarg(5899,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform3i64vARB(location,count,value);
}

void ecb_glUniform4i64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint64 *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5900,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5900,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5900,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint64> value_vec (4*count);
  GLint64 *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5900,"value");
      if(!enif_get_int64(env, value_tpl[0],(egl_int64_t *) value_ptr++)) Badarg(5900,"value");
      if(!enif_get_int64(env, value_tpl[1],(egl_int64_t *) value_ptr++)) Badarg(5900,"value");
      if(!enif_get_int64(env, value_tpl[2],(egl_int64_t *) value_ptr++)) Badarg(5900,"value");
      if(!enif_get_int64(env, value_tpl[3],(egl_int64_t *) value_ptr++)) Badarg(5900,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform4i64vARB(location,count,value);
}

void ecb_glUniform1ui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint64 x;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5901,"location");
  if(!enif_get_uint64(env, argv[1], (egl_uint64_t *) &x)) Badarg(5901,"x");
  weglUniform1ui64ARB(location,x);
}

void ecb_glUniform2ui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint64 x;
  GLuint64 y;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5902,"location");
  if(!enif_get_uint64(env, argv[1], (egl_uint64_t *) &x)) Badarg(5902,"x");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &y)) Badarg(5902,"y");
  weglUniform2ui64ARB(location,x,y);
}

void ecb_glUniform3ui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint64 x;
  GLuint64 y;
  GLuint64 z;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5903,"location");
  if(!enif_get_uint64(env, argv[1], (egl_uint64_t *) &x)) Badarg(5903,"x");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &y)) Badarg(5903,"y");
  if(!enif_get_uint64(env, argv[3], (egl_uint64_t *) &z)) Badarg(5903,"z");
  weglUniform3ui64ARB(location,x,y,z);
}

void ecb_glUniform4ui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint64 x;
  GLuint64 y;
  GLuint64 z;
  GLuint64 w;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5904,"location");
  if(!enif_get_uint64(env, argv[1], (egl_uint64_t *) &x)) Badarg(5904,"x");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &y)) Badarg(5904,"y");
  if(!enif_get_uint64(env, argv[3], (egl_uint64_t *) &z)) Badarg(5904,"z");
  if(!enif_get_uint64(env, argv[4], (egl_uint64_t *) &w)) Badarg(5904,"w");
  weglUniform4ui64ARB(location,x,y,z,w);
}

void ecb_glUniform1ui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint64 *value;
  std::vector <GLuint64> value_vec;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5905,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5905,"count");
  if(!enif_is_list(env, argv[2])) Badarg(5905, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLuint64 value_tmp;
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_uint64(env, value_h,(egl_uint64_t *) &value_tmp)) Badarg(5905,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglUniform1ui64vARB(location,count,value);
}

void ecb_glUniform2ui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint64 *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5906,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5906,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5906,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint64> value_vec (2*count);
  GLuint64 *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5906,"value");
      if(!enif_get_uint64(env, value_tpl[0],(egl_uint64_t *) value_ptr++)) Badarg(5906,"value");
      if(!enif_get_uint64(env, value_tpl[1],(egl_uint64_t *) value_ptr++)) Badarg(5906,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform2ui64vARB(location,count,value);
}

void ecb_glUniform3ui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint64 *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5907,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5907,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5907,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint64> value_vec (3*count);
  GLuint64 *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5907,"value");
      if(!enif_get_uint64(env, value_tpl[0],(egl_uint64_t *) value_ptr++)) Badarg(5907,"value");
      if(!enif_get_uint64(env, value_tpl[1],(egl_uint64_t *) value_ptr++)) Badarg(5907,"value");
      if(!enif_get_uint64(env, value_tpl[2],(egl_uint64_t *) value_ptr++)) Badarg(5907,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform3ui64vARB(location,count,value);
}

void ecb_glUniform4ui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint64 *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5908,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5908,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5908,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint64> value_vec (4*count);
  GLuint64 *value_ptr = value_vec.data();
  value_l = argv[2];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5908,"value");
      if(!enif_get_uint64(env, value_tpl[0],(egl_uint64_t *) value_ptr++)) Badarg(5908,"value");
      if(!enif_get_uint64(env, value_tpl[1],(egl_uint64_t *) value_ptr++)) Badarg(5908,"value");
      if(!enif_get_uint64(env, value_tpl[2],(egl_uint64_t *) value_ptr++)) Badarg(5908,"value");
      if(!enif_get_uint64(env, value_tpl[3],(egl_uint64_t *) value_ptr++)) Badarg(5908,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglUniform4ui64vARB(location,count,value);
}

void ecb_glGetUniformi64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLint64 params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5909,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5909,"location");
  weglGetUniformi64vARB(program,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int64(env, (egl_int64_t) params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetUniformui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLuint64 params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5910,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5910,"location");
  weglGetUniformui64vARB(program,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int64(env, (egl_int64_t) params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glProgramUniform1i64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint64 x;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5911,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5911,"location");
  if(!enif_get_int64(env, argv[2], (egl_int64_t *) &x)) Badarg(5911,"x");
  weglProgramUniform1i64ARB(program,location,x);
}

void ecb_glProgramUniform2i64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint64 x;
  GLint64 y;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5912,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5912,"location");
  if(!enif_get_int64(env, argv[2], (egl_int64_t *) &x)) Badarg(5912,"x");
  if(!enif_get_int64(env, argv[3], (egl_int64_t *) &y)) Badarg(5912,"y");
  weglProgramUniform2i64ARB(program,location,x,y);
}

void ecb_glProgramUniform3i64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint64 x;
  GLint64 y;
  GLint64 z;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5913,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5913,"location");
  if(!enif_get_int64(env, argv[2], (egl_int64_t *) &x)) Badarg(5913,"x");
  if(!enif_get_int64(env, argv[3], (egl_int64_t *) &y)) Badarg(5913,"y");
  if(!enif_get_int64(env, argv[4], (egl_int64_t *) &z)) Badarg(5913,"z");
  weglProgramUniform3i64ARB(program,location,x,y,z);
}

void ecb_glProgramUniform4i64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint64 x;
  GLint64 y;
  GLint64 z;
  GLint64 w;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5914,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5914,"location");
  if(!enif_get_int64(env, argv[2], (egl_int64_t *) &x)) Badarg(5914,"x");
  if(!enif_get_int64(env, argv[3], (egl_int64_t *) &y)) Badarg(5914,"y");
  if(!enif_get_int64(env, argv[4], (egl_int64_t *) &z)) Badarg(5914,"z");
  if(!enif_get_int64(env, argv[5], (egl_int64_t *) &w)) Badarg(5914,"w");
  weglProgramUniform4i64ARB(program,location,x,y,z,w);
}

void ecb_glProgramUniform1i64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint64 *value;
  std::vector <GLint64> value_vec;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5915,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5915,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5915,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5915, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLint64 value_tmp;
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_int64(env, value_h,(egl_int64_t *) &value_tmp)) Badarg(5915,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglProgramUniform1i64vARB(program,location,count,value);
}

void ecb_glProgramUniform2i64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint64 *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5916,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5916,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5916,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5916,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint64> value_vec (2*count);
  GLint64 *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5916,"value");
      if(!enif_get_int64(env, value_tpl[0],(egl_int64_t *) value_ptr++)) Badarg(5916,"value");
      if(!enif_get_int64(env, value_tpl[1],(egl_int64_t *) value_ptr++)) Badarg(5916,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform2i64vARB(program,location,count,value);
}

void ecb_glProgramUniform3i64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint64 *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5917,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5917,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5917,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5917,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint64> value_vec (3*count);
  GLint64 *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5917,"value");
      if(!enif_get_int64(env, value_tpl[0],(egl_int64_t *) value_ptr++)) Badarg(5917,"value");
      if(!enif_get_int64(env, value_tpl[1],(egl_int64_t *) value_ptr++)) Badarg(5917,"value");
      if(!enif_get_int64(env, value_tpl[2],(egl_int64_t *) value_ptr++)) Badarg(5917,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform3i64vARB(program,location,count,value);
}

void ecb_glProgramUniform4i64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint64 *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5918,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5918,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5918,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5918,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLint64> value_vec (4*count);
  GLint64 *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5918,"value");
      if(!enif_get_int64(env, value_tpl[0],(egl_int64_t *) value_ptr++)) Badarg(5918,"value");
      if(!enif_get_int64(env, value_tpl[1],(egl_int64_t *) value_ptr++)) Badarg(5918,"value");
      if(!enif_get_int64(env, value_tpl[2],(egl_int64_t *) value_ptr++)) Badarg(5918,"value");
      if(!enif_get_int64(env, value_tpl[3],(egl_int64_t *) value_ptr++)) Badarg(5918,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform4i64vARB(program,location,count,value);
}

void ecb_glProgramUniform1ui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint64 x;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5919,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5919,"location");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &x)) Badarg(5919,"x");
  weglProgramUniform1ui64ARB(program,location,x);
}

void ecb_glProgramUniform2ui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint64 x;
  GLuint64 y;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5920,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5920,"location");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &x)) Badarg(5920,"x");
  if(!enif_get_uint64(env, argv[3], (egl_uint64_t *) &y)) Badarg(5920,"y");
  weglProgramUniform2ui64ARB(program,location,x,y);
}

void ecb_glProgramUniform3ui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint64 x;
  GLuint64 y;
  GLuint64 z;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5921,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5921,"location");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &x)) Badarg(5921,"x");
  if(!enif_get_uint64(env, argv[3], (egl_uint64_t *) &y)) Badarg(5921,"y");
  if(!enif_get_uint64(env, argv[4], (egl_uint64_t *) &z)) Badarg(5921,"z");
  weglProgramUniform3ui64ARB(program,location,x,y,z);
}

void ecb_glProgramUniform4ui64ARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint64 x;
  GLuint64 y;
  GLuint64 z;
  GLuint64 w;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5922,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5922,"location");
  if(!enif_get_uint64(env, argv[2], (egl_uint64_t *) &x)) Badarg(5922,"x");
  if(!enif_get_uint64(env, argv[3], (egl_uint64_t *) &y)) Badarg(5922,"y");
  if(!enif_get_uint64(env, argv[4], (egl_uint64_t *) &z)) Badarg(5922,"z");
  if(!enif_get_uint64(env, argv[5], (egl_uint64_t *) &w)) Badarg(5922,"w");
  weglProgramUniform4ui64ARB(program,location,x,y,z,w);
}

void ecb_glProgramUniform1ui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint64 *value;
  std::vector <GLuint64> value_vec;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5923,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5923,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5923,"count");
  if(!enif_is_list(env, argv[3])) Badarg(5923, "value")
  else {
    ERL_NIF_TERM value_l, value_h, value_t;
    GLuint64 value_tmp;
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_uint64(env, value_h,(egl_uint64_t *) &value_tmp)) Badarg(5923,"value");
        value_vec.push_back(value_tmp);
        value_l = value_t;
    };
    value = value_vec.data();
  }
  weglProgramUniform1ui64vARB(program,location,count,value);
}

void ecb_glProgramUniform2ui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint64 *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5924,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5924,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5924,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5924,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint64> value_vec (2*count);
  GLuint64 *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) Badarg(5924,"value");
      if(!enif_get_uint64(env, value_tpl[0],(egl_uint64_t *) value_ptr++)) Badarg(5924,"value");
      if(!enif_get_uint64(env, value_tpl[1],(egl_uint64_t *) value_ptr++)) Badarg(5924,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform2ui64vARB(program,location,count,value);
}

void ecb_glProgramUniform3ui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint64 *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5925,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5925,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5925,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5925,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint64> value_vec (3*count);
  GLuint64 *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) Badarg(5925,"value");
      if(!enif_get_uint64(env, value_tpl[0],(egl_uint64_t *) value_ptr++)) Badarg(5925,"value");
      if(!enif_get_uint64(env, value_tpl[1],(egl_uint64_t *) value_ptr++)) Badarg(5925,"value");
      if(!enif_get_uint64(env, value_tpl[2],(egl_uint64_t *) value_ptr++)) Badarg(5925,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform3ui64vARB(program,location,count,value);
}

void ecb_glProgramUniform4ui64vARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint64 *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5926,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5926,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5926,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5926,"value")}
  int value_a;
  const ERL_NIF_TERM *value_tpl;
  ERL_NIF_TERM value_l, value_h, value_t;
  std::vector <GLuint64> value_vec (4*count);
  GLuint64 *value_ptr = value_vec.data();
  value_l = argv[3];
  while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
      if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) Badarg(5926,"value");
      if(!enif_get_uint64(env, value_tpl[0],(egl_uint64_t *) value_ptr++)) Badarg(5926,"value");
      if(!enif_get_uint64(env, value_tpl[1],(egl_uint64_t *) value_ptr++)) Badarg(5926,"value");
      if(!enif_get_uint64(env, value_tpl[2],(egl_uint64_t *) value_ptr++)) Badarg(5926,"value");
      if(!enif_get_uint64(env, value_tpl[3],(egl_uint64_t *) value_ptr++)) Badarg(5926,"value");
      value_l = value_t;
    };
  value = value_vec.data();
  weglProgramUniform4ui64vARB(program,location,count,value);
}

void ecb_glColorTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLenum format;
  GLenum type;
  ErlNifBinary table;
  GLvoid *table_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5927,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5927,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5927,"width");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5927,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5927,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &table_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &table))
        table_idx = (GLvoid *) table.data;
    else Badarg(5927,"table");
  }
  weglColorTable(target,internalformat,width,format,type,table_idx);
}

void ecb_glColorTableParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5929,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5929,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5929,"params");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, params_t[i1++], &params[0])) Badarg(5929,"params");
     if(!egl_get_float(env, params_t[i1++], &params[1])) Badarg(5929,"params");
     if(!egl_get_float(env, params_t[i1++], &params[2])) Badarg(5929,"params");
     if(!egl_get_float(env, params_t[i1++], &params[3])) Badarg(5929,"params");
   }};
  weglColorTableParameterfv(target,pname,params);
}

void ecb_glColorTableParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5930,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5930,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5930,"params");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, params_t[i1++], &params[0])) Badarg(5930,"params");
     if(!enif_get_int(env, params_t[i1++], &params[1])) Badarg(5930,"params");
     if(!enif_get_int(env, params_t[i1++], &params[2])) Badarg(5930,"params");
     if(!enif_get_int(env, params_t[i1++], &params[3])) Badarg(5930,"params");
   }};
  weglColorTableParameteriv(target,pname,params);
}

void ecb_glCopyColorTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5931,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5931,"internalformat");
  if(!enif_get_int(env, argv[2],  &x)) Badarg(5931,"x");
  if(!enif_get_int(env, argv[3],  &y)) Badarg(5931,"y");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5931,"width");
  weglCopyColorTable(target,internalformat,x,y,width);
}

void ecb_glGetColorTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum format;
  GLenum type;
  ErlNifBinary table;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5932,"target");
  if(!enif_get_uint(env, argv[1],  &format)) Badarg(5932,"format");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5932,"type");
  if(enif_is_binary(env, argv[3]))
    enif_inspect_binary(env, argv[3], &table);
  else if(enif_is_tuple(env, argv[3])) {
    int table_a;
    const ERL_NIF_TERM *table_t;
    if(enif_get_tuple(env, argv[3], &table_a, &table_t) &&
         enif_is_binary(env, table_t[1]))
       enif_inspect_binary(env, table_t[1], &table);
    else Badarg(5932, "table");
  } else Badarg(5932, "table");
  weglGetColorTable(target,format,type,(GLvoid *) table.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetColorTableParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5933,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5933,"pname");
  weglGetColorTableParameterfv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetColorTableParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5934,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5934,"pname");
  weglGetColorTableParameteriv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glColorSubTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei start;
  GLsizei count;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5935,"target");
  if(!enif_get_int(env, argv[1],  &start)) Badarg(5935,"start");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5935,"count");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5935,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5935,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5935,"data");
  }
  weglColorSubTable(target,start,count,format,type,data_idx);
}

void ecb_glCopyColorSubTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei start;
  GLint x;
  GLint y;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5937,"target");
  if(!enif_get_int(env, argv[1],  &start)) Badarg(5937,"start");
  if(!enif_get_int(env, argv[2],  &x)) Badarg(5937,"x");
  if(!enif_get_int(env, argv[3],  &y)) Badarg(5937,"y");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5937,"width");
  weglCopyColorSubTable(target,start,x,y,width);
}

void ecb_glConvolutionFilter1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLenum format;
  GLenum type;
  ErlNifBinary image;
  GLvoid *image_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5938,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5938,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5938,"width");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5938,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5938,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &image_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &image))
        image_idx = (GLvoid *) image.data;
    else Badarg(5938,"image");
  }
  weglConvolutionFilter1D(target,internalformat,width,format,type,image_idx);
}

void ecb_glConvolutionFilter2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary image;
  GLvoid *image_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5940,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5940,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5940,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5940,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5940,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5940,"type");
  if(!egl_get_ptr(env, argv[6], (void **) &image_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &image))
        image_idx = (GLvoid *) image.data;
    else Badarg(5940,"image");
  }
  weglConvolutionFilter2D(target,internalformat,width,height,format,type,image_idx);
}

void ecb_glConvolutionParameterf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5942,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5942,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5942,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5942,"params");
   }};
  weglConvolutionParameterf(target,pname,params);
}

void ecb_glConvolutionParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5943,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5943,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5943,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5943,"params");
   }};
  weglConvolutionParameterfv(target,pname,params);
}

void ecb_glConvolutionParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5944,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5944,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5944,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5944,"params");
   }};
  weglConvolutionParameteri(target,pname,params);
}

void ecb_glConvolutionParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5945,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5945,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5945,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5945,"params");
   }};
  weglConvolutionParameteriv(target,pname,params);
}

void ecb_glCopyConvolutionFilter1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5946,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5946,"internalformat");
  if(!enif_get_int(env, argv[2],  &x)) Badarg(5946,"x");
  if(!enif_get_int(env, argv[3],  &y)) Badarg(5946,"y");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5946,"width");
  weglCopyConvolutionFilter1D(target,internalformat,x,y,width);
}

void ecb_glCopyConvolutionFilter2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5947,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5947,"internalformat");
  if(!enif_get_int(env, argv[2],  &x)) Badarg(5947,"x");
  if(!enif_get_int(env, argv[3],  &y)) Badarg(5947,"y");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5947,"width");
  if(!enif_get_int(env, argv[5],  &height)) Badarg(5947,"height");
  weglCopyConvolutionFilter2D(target,internalformat,x,y,width,height);
}

void ecb_glGetConvolutionFilter(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum format;
  GLenum type;
  ErlNifBinary image;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5948,"target");
  if(!enif_get_uint(env, argv[1],  &format)) Badarg(5948,"format");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5948,"type");
  if(enif_is_binary(env, argv[3]))
    enif_inspect_binary(env, argv[3], &image);
  else if(enif_is_tuple(env, argv[3])) {
    int image_a;
    const ERL_NIF_TERM *image_t;
    if(enif_get_tuple(env, argv[3], &image_a, &image_t) &&
         enif_is_binary(env, image_t[1]))
       enif_inspect_binary(env, image_t[1], &image);
    else Badarg(5948, "image");
  } else Badarg(5948, "image");
  weglGetConvolutionFilter(target,format,type,(GLvoid *) image.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetConvolutionParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5949,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5949,"pname");
  weglGetConvolutionParameterfv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetConvolutionParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5950,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5950,"pname");
  weglGetConvolutionParameteriv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glSeparableFilter2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary row;
  GLvoid *row_idx;
  ErlNifBinary column;
  GLvoid *column_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5951,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5951,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5951,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5951,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5951,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5951,"type");
  if(!egl_get_ptr(env, argv[6], (void **) &row_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &row))
        row_idx = (GLvoid *) row.data;
    else Badarg(5951,"row");
  }
  if(!egl_get_ptr(env, argv[7], (void **) &column_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[7], &column))
        column_idx = (GLvoid *) column.data;
    else Badarg(5951,"column");
  }
  weglSeparableFilter2D(target,internalformat,width,height,format,type,row_idx,column_idx);
}

void ecb_glGetHistogram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLboolean reset;
  GLenum format;
  GLenum type;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5953,"target");
  if(!egl_get_ubyte(env, argv[1],  &reset)) Badarg(5953,"reset");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5953,"format");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5953,"type");
  if(enif_is_binary(env, argv[4]))
    enif_inspect_binary(env, argv[4], &values);
  else if(enif_is_tuple(env, argv[4])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[4], &values_a, &values_t) &&
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5953, "values");
  } else Badarg(5953, "values");
  weglGetHistogram(target,reset,format,type,(GLvoid *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetHistogramParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5954,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5954,"pname");
  weglGetHistogramParameterfv(target,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_double(env, (double) params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetHistogramParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5955,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5955,"pname");
  weglGetHistogramParameteriv(target,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_int(env, params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetMinmax(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLboolean reset;
  GLenum format;
  GLenum types;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5956,"target");
  if(!egl_get_ubyte(env, argv[1],  &reset)) Badarg(5956,"reset");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5956,"format");
  if(!enif_get_uint(env, argv[3],  &types)) Badarg(5956,"types");
  if(enif_is_binary(env, argv[4]))
    enif_inspect_binary(env, argv[4], &values);
  else if(enif_is_tuple(env, argv[4])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[4], &values_a, &values_t) &&
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5956, "values");
  } else Badarg(5956, "values");
  weglGetMinmax(target,reset,format,types,(GLvoid *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glGetMinmaxParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5957,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5957,"pname");
  weglGetMinmaxParameterfv(target,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_double(env, (double) params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetMinmaxParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5958,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5958,"pname");
  weglGetMinmaxParameteriv(target,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_int(env, params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glHistogram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei width;
  GLenum internalformat;
  GLboolean sink;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5959,"target");
  if(!enif_get_int(env, argv[1],  &width)) Badarg(5959,"width");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5959,"internalformat");
  if(!egl_get_ubyte(env, argv[3],  &sink)) Badarg(5959,"sink");
  weglHistogram(target,width,internalformat,sink);
}

void ecb_glMinmax(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLboolean sink;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5960,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5960,"internalformat");
  if(!egl_get_ubyte(env, argv[2],  &sink)) Badarg(5960,"sink");
  weglMinmax(target,internalformat,sink);
}

void ecb_glResetHistogram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5961,"target");
  weglResetHistogram(target);
}

void ecb_glResetMinmax(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5962,"target");
  weglResetMinmax(target);
}

void ecb_glCurrentPaletteMatrixARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint index;
  if(!enif_get_int(env, argv[0],  &index)) Badarg(5963,"index");
  weglCurrentPaletteMatrixARB(index);
}

void ecb_glMatrixIndexubvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLubyte *indices;
  std::vector <GLubyte> indices_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5964,"size");
  if(!enif_is_list(env, argv[1])) Badarg(5964, "indices")
  else {
    ERL_NIF_TERM indices_l, indices_h, indices_t;
    GLubyte indices_tmp;
    indices_l = argv[1];
    while(enif_get_list_cell(env, indices_l, &indices_h, &indices_t)) {
        if(!egl_get_ubyte(env, indices_h, &indices_tmp)) Badarg(5964,"indices");
        indices_vec.push_back(indices_tmp);
        indices_l = indices_t;
    };
    indices = indices_vec.data();
  }
  weglMatrixIndexubvARB(size,indices);
}

void ecb_glMatrixIndexusvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLushort *indices;
  std::vector <GLushort> indices_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5965,"size");
  if(!enif_is_list(env, argv[1])) Badarg(5965, "indices")
  else {
    ERL_NIF_TERM indices_l, indices_h, indices_t;
    GLushort indices_tmp;
    indices_l = argv[1];
    while(enif_get_list_cell(env, indices_l, &indices_h, &indices_t)) {
        if(!egl_get_ushort(env, indices_h, &indices_tmp)) Badarg(5965,"indices");
        indices_vec.push_back(indices_tmp);
        indices_l = indices_t;
    };
    indices = indices_vec.data();
  }
  weglMatrixIndexusvARB(size,indices);
}

void ecb_glMatrixIndexuivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLuint *indices;
  std::vector <GLuint> indices_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5966,"size");
  if(!enif_is_list(env, argv[1])) Badarg(5966, "indices")
  else {
    ERL_NIF_TERM indices_l, indices_h, indices_t;
    GLuint indices_tmp;
    indices_l = argv[1];
    while(enif_get_list_cell(env, indices_l, &indices_h, &indices_t)) {
        if(!enif_get_uint(env, indices_h, &indices_tmp)) Badarg(5966,"indices");
        indices_vec.push_back(indices_tmp);
        indices_l = indices_t;
    };
    indices = indices_vec.data();
  }
  weglMatrixIndexuivARB(size,indices);
}

void ecb_glSampleCoverageARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat value;
  GLboolean invert;
  if(!egl_get_float(env, argv[0],  &value)) Badarg(5967,"value");
  if(!egl_get_ubyte(env, argv[1],  &invert)) Badarg(5967,"invert");
  weglSampleCoverageARB(value,invert);
}

void ecb_glMaxShaderCompilerThreadsARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint count;
  if(!enif_get_uint(env, argv[0],  &count)) Badarg(5968,"count");
  weglMaxShaderCompilerThreadsARB(count);
}

void ecb_glEvaluateDepthValuesARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEvaluateDepthValuesARB();
}

void ecb_glDeleteObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t obj;
  if(!enif_get_uint64(env, argv[0],  &obj)) Badarg(5970,"obj");
  weglDeleteObjectARB((GLhandleARB) obj);
}

void ecb_glGetHandleARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLhandleARB result;
  ERL_NIF_TERM reply;
  GLenum pname;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5971,"pname");
  result = weglGetHandleARB(pname);
  reply =      enif_make_uint64(env, (egl_uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glDetachObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t containerObj;
  egl_uint64_t attachedObj;
  if(!enif_get_uint64(env, argv[0],  &containerObj)) Badarg(5972,"containerObj");
  if(!enif_get_uint64(env, argv[1],  &attachedObj)) Badarg(5972,"attachedObj");
  weglDetachObjectARB((GLhandleARB) containerObj,(GLhandleARB) attachedObj);
}

void ecb_glCreateShaderObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLhandleARB result;
  ERL_NIF_TERM reply;
  GLenum shaderType;
  if(!enif_get_uint(env, argv[0],  &shaderType)) Badarg(5973,"shaderType");
  result = weglCreateShaderObjectARB(shaderType);
  reply =      enif_make_uint64(env, (egl_uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glShaderSourceARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t shaderObj;
  GLsizei count;
  if(!enif_get_uint64(env, argv[0],  &shaderObj)) Badarg(5974,"shaderObj");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5974,"count");
  ERL_NIF_TERM string_l, string_h, string_t;
  ErlNifBinary string_tmp;
  std::vector <GLchar *> string;
  string_l = argv[2];
  while(enif_get_list_cell(env, string_l, &string_h, &string_t)) {
    if(!enif_inspect_binary(env, string_h, &string_tmp)) Badarg(5974,"string");
    string.push_back((GLchar *) string_tmp.data);
    string_l = string_t;
  }
  weglShaderSourceARB((GLhandleARB) shaderObj,count,(const GLchar **) string.data(),NULL);
}

void ecb_glCompileShaderARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t shaderObj;
  if(!enif_get_uint64(env, argv[0],  &shaderObj)) Badarg(5975,"shaderObj");
  weglCompileShaderARB((GLhandleARB) shaderObj);
}

void ecb_glCreateProgramObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLhandleARB result;
  ERL_NIF_TERM reply;
  result = weglCreateProgramObjectARB();
  reply =      enif_make_uint64(env, (egl_uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glAttachObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t containerObj;
  egl_uint64_t obj;
  if(!enif_get_uint64(env, argv[0],  &containerObj)) Badarg(5977,"containerObj");
  if(!enif_get_uint64(env, argv[1],  &obj)) Badarg(5977,"obj");
  weglAttachObjectARB((GLhandleARB) containerObj,(GLhandleARB) obj);
}

void ecb_glLinkProgramARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t programObj;
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(5978,"programObj");
  weglLinkProgramARB((GLhandleARB) programObj);
}

void ecb_glUseProgramObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t programObj;
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(5979,"programObj");
  weglUseProgramObjectARB((GLhandleARB) programObj);
}

void ecb_glValidateProgramARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t programObj;
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(5980,"programObj");
  weglValidateProgramARB((GLhandleARB) programObj);
}

void ecb_glGetObjectParameterfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t obj;
  GLenum pname;
  GLfloat params;
  if(!enif_get_uint64(env, argv[0],  &obj)) Badarg(5981,"obj");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5981,"pname");
  weglGetObjectParameterfvARB((GLhandleARB) obj,pname,&params);
  reply =      enif_make_double(env, (double) params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetObjectParameterivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t obj;
  GLenum pname;
  GLint params;
  if(!enif_get_uint64(env, argv[0],  &obj)) Badarg(5982,"obj");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5982,"pname");
  weglGetObjectParameterivARB((GLhandleARB) obj,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetInfoLogARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t obj;
  GLsizei maxLength;
  GLsizei length;
  unsigned char *infoLog;
  if(!enif_get_uint64(env, argv[0],  &obj)) Badarg(5983,"obj");
  if(!enif_get_int(env, argv[1],  &maxLength)) Badarg(5983,"maxLength");
  infoLog = (unsigned char *) enif_alloc((int) maxLength*sizeof(GLchar));
  weglGetInfoLogARB((GLhandleARB) obj,maxLength,&length,(GLchar *) infoLog);
  reply =      enif_make_string(env, (const char *) infoLog, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(infoLog);
}

void ecb_glGetAttachedObjectsARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t containerObj;
  GLsizei maxCount;
  GLsizei count;
  if(!enif_get_uint64(env, argv[0],  &containerObj)) Badarg(5984,"containerObj");
  if(!enif_get_int(env, argv[1],  &maxCount)) Badarg(5984,"maxCount");
  std::vector <GLhandleARB> obj (maxCount);
  std::vector <ERL_NIF_TERM> obj_ts (maxCount);
  weglGetAttachedObjectsARB((GLhandleARB) containerObj,maxCount,&count,obj.data());
  for(int ri=0; ri < (int) count; ri++)
    obj_ts[ri] =      enif_make_uint64(env, (egl_uint64_t) obj[ri]);
  reply =      enif_make_list_from_array(env, obj_ts.data(), count);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetUniformLocationARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  egl_uint64_t programObj;
  ErlNifBinary name;
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(5985,"programObj");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5985,"name");
  result = weglGetUniformLocationARB((GLhandleARB) programObj,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetActiveUniformARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t programObj;
  GLuint index;
  GLsizei maxLength;
  GLsizei length;
  GLint size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(5986,"programObj");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5986,"index");
  if(!enif_get_int(env, argv[2],  &maxLength)) Badarg(5986,"maxLength");
  name = (unsigned char *) enif_alloc((int) maxLength*sizeof(GLchar));
  weglGetActiveUniformARB((GLhandleARB) programObj,index,maxLength,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

void ecb_glGetUniformfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t programObj;
  GLint location;
  GLfloat params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(5987,"programObj");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5987,"location");
  weglGetUniformfvARB((GLhandleARB) programObj,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_double(env, (double) params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetUniformivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t programObj;
  GLint location;
  GLint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(5988,"programObj");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5988,"location");
  weglGetUniformivARB((GLhandleARB) programObj,location,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glGetShaderSourceARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t obj;
  GLsizei maxLength;
  GLsizei length;
  unsigned char *source;
  if(!enif_get_uint64(env, argv[0],  &obj)) Badarg(5989,"obj");
  if(!enif_get_int(env, argv[1],  &maxLength)) Badarg(5989,"maxLength");
  source = (unsigned char *) enif_alloc((int) maxLength*sizeof(GLchar));
  weglGetShaderSourceARB((GLhandleARB) obj,maxLength,&length,(GLchar *) source);
  reply =      enif_make_string(env, (const char *) source, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(source);
}

void ecb_glDeleteNamedStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ErlNifBinary name;
  if(!enif_inspect_binary(env, argv[0], &name)) Badarg(5990,"name");
  weglDeleteNamedStringARB((GLint) name.size,(GLchar *) name.data);
}

void ecb_glCompileShaderIncludeARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint shader;
  GLsizei count;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5991,"shader");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5991,"count");
  ERL_NIF_TERM path_l, path_h, path_t;
  ErlNifBinary path_tmp;
  std::vector <GLchar *> path;
  path_l = argv[2];
  while(enif_get_list_cell(env, path_l, &path_h, &path_t)) {
    if(!enif_inspect_binary(env, path_h, &path_tmp)) Badarg(5991,"path");
    path.push_back((GLchar *) path_tmp.data);
    path_l = path_t;
  }
  weglCompileShaderIncludeARB(shader,count,(const GLchar **) path.data(),NULL);
}

void ecb_glIsNamedStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  ErlNifBinary name;
  if(!enif_inspect_binary(env, argv[0], &name)) Badarg(5992,"name");
  result = weglIsNamedStringARB((GLint) name.size,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBufferPageCommitmentARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLintptr offset;
  GLsizeiptr size;
  GLboolean commit;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5993,"target");
  if(!egl_get_word(env, argv[1], (egl_word *) &offset)) Badarg(5993,"offset");
  if(!egl_get_word(env, argv[2], (egl_word *) &size)) Badarg(5993,"size");
  if(!egl_get_ubyte(env, argv[3],  &commit)) Badarg(5993,"commit");
  weglBufferPageCommitmentARB(target,offset,size,commit);
}

void ecb_glTexPageCommitmentARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLboolean commit;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5994,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5994,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5994,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5994,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5994,"zoffset");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5994,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5994,"height");
  if(!enif_get_int(env, argv[7],  &depth)) Badarg(5994,"depth");
  if(!egl_get_ubyte(env, argv[8],  &commit)) Badarg(5994,"commit");
  weglTexPageCommitmentARB(target,level,xoffset,yoffset,zoffset,width,height,depth,commit);
}

void ecb_glGetCompressedTexImageARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  ErlNifBinary img;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5995,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5995,"level");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &img);
  else if(enif_is_tuple(env, argv[2])) {
    int img_a;
    const ERL_NIF_TERM *img_t;
    if(enif_get_tuple(env, argv[2], &img_a, &img_t) &&
         enif_is_binary(env, img_t[1]))
       enif_inspect_binary(env, img_t[1], &img);
    else Badarg(5995, "img");
  } else Badarg(5995, "img");
  weglGetCompressedTexImageARB(target,level,(void *) img.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

void ecb_glLoadTransposeMatrixfARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5996,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5996,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5996,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5996,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5996,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5996,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5996,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadTransposeMatrixfARB(m);
}

void ecb_glLoadTransposeMatrixdARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5997,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5997,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5997,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5997,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5997,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5997,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5997,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadTransposeMatrixdARB(m);
}

void ecb_glMultTransposeMatrixfARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5998,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5998,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5998,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5998,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5998,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5998,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5998,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultTransposeMatrixfARB(m);
}

void ecb_glMultTransposeMatrixdARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5999,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5999,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5999,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5999,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5999,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5999,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5999,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultTransposeMatrixdARB(m);
}

void ecb_glWeightbvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLbyte *weights;
  std::vector <GLbyte> weights_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(6000,"size");
  if(!enif_is_list(env, argv[1])) Badarg(6000, "weights")
  else {
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    GLbyte weights_tmp;
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_byte(env, weights_h, &weights_tmp)) Badarg(6000,"weights");
        weights_vec.push_back(weights_tmp);
        weights_l = weights_t;
    };
    weights = weights_vec.data();
  }
  weglWeightbvARB(size,weights);
}

void ecb_glWeightsvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLshort *weights;
  std::vector <GLshort> weights_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(6001,"size");
  if(!enif_is_list(env, argv[1])) Badarg(6001, "weights")
  else {
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    GLshort weights_tmp;
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_short(env, weights_h, &weights_tmp)) Badarg(6001,"weights");
        weights_vec.push_back(weights_tmp);
        weights_l = weights_t;
    };
    weights = weights_vec.data();
  }
  weglWeightsvARB(size,weights);
}

void ecb_glWeightivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLint *weights;
  std::vector <GLint> weights_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(6002,"size");
  if(!enif_is_list(env, argv[1])) Badarg(6002, "weights")
  else {
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    GLint weights_tmp;
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!enif_get_int(env, weights_h, &weights_tmp)) Badarg(6002,"weights");
        weights_vec.push_back(weights_tmp);
        weights_l = weights_t;
    };
    weights = weights_vec.data();
  }
  weglWeightivARB(size,weights);
}

void ecb_glWeightfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLfloat *weights;
  std::vector <GLfloat> weights_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(6003,"size");
  if(!enif_is_list(env, argv[1])) Badarg(6003, "weights")
  else {
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    GLfloat weights_tmp;
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_float(env, weights_h, &weights_tmp)) Badarg(6003,"weights");
        weights_vec.push_back(weights_tmp);
        weights_l = weights_t;
    };
    weights = weights_vec.data();
  }
  weglWeightfvARB(size,weights);
}

void ecb_glWeightdvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLdouble *weights;
  std::vector <GLdouble> weights_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(6004,"size");
  if(!enif_is_list(env, argv[1])) Badarg(6004, "weights")
  else {
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    GLdouble weights_tmp;
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!enif_get_double(env, weights_h, &weights_tmp)) Badarg(6004,"weights");
        weights_vec.push_back(weights_tmp);
        weights_l = weights_t;
    };
    weights = weights_vec.data();
  }
  weglWeightdvARB(size,weights);
}

void ecb_glWeightubvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLubyte *weights;
  std::vector <GLubyte> weights_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(6005,"size");
  if(!enif_is_list(env, argv[1])) Badarg(6005, "weights")
  else {
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    GLubyte weights_tmp;
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_ubyte(env, weights_h, &weights_tmp)) Badarg(6005,"weights");
        weights_vec.push_back(weights_tmp);
        weights_l = weights_t;
    };
    weights = weights_vec.data();
  }
  weglWeightubvARB(size,weights);
}

void ecb_glWeightusvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLushort *weights;
  std::vector <GLushort> weights_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(6006,"size");
  if(!enif_is_list(env, argv[1])) Badarg(6006, "weights")
  else {
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    GLushort weights_tmp;
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_ushort(env, weights_h, &weights_tmp)) Badarg(6006,"weights");
        weights_vec.push_back(weights_tmp);
        weights_l = weights_t;
    };
    weights = weights_vec.data();
  }
  weglWeightusvARB(size,weights);
}

void ecb_glWeightuivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLuint *weights;
  std::vector <GLuint> weights_vec;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(6007,"size");
  if(!enif_is_list(env, argv[1])) Badarg(6007, "weights")
  else {
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    GLuint weights_tmp;
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!enif_get_uint(env, weights_h, &weights_tmp)) Badarg(6007,"weights");
        weights_vec.push_back(weights_tmp);
        weights_l = weights_t;
    };
    weights = weights_vec.data();
  }
  weglWeightuivARB(size,weights);
}

void ecb_glVertexBlendARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint count;
  if(!enif_get_int(env, argv[0],  &count)) Badarg(6008,"count");
  weglVertexBlendARB(count);
}

void ecb_glGetBufferParameterivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(6009,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(6009,"pname");
  weglGetBufferParameterivARB(target,pname,params);
  for(int ri=0; ri < (int) 16; ri++)
     params_ts[ri] =      enif_make_int(env, params[ri]);
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBindAttribLocationARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  egl_uint64_t programObj;
  GLuint index;
  ErlNifBinary name;
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(6010,"programObj");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(6010,"index");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(6010,"name");
  weglBindAttribLocationARB((GLhandleARB) programObj,index,(GLchar *) name.data);
}

void ecb_glGetActiveAttribARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  egl_uint64_t programObj;
  GLuint index;
  GLsizei maxLength;
  GLsizei length;
  GLint size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(6011,"programObj");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(6011,"index");
  if(!enif_get_int(env, argv[2],  &maxLength)) Badarg(6011,"maxLength");
  name = (unsigned char *) enif_alloc((int) maxLength*sizeof(GLchar));
  weglGetActiveAttribARB((GLhandleARB) programObj,index,maxLength,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

void ecb_glGetAttribLocationARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  egl_uint64_t programObj;
  ErlNifBinary name;
  if(!enif_get_uint64(env, argv[0],  &programObj)) Badarg(6012,"programObj");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(6012,"name");
  result = weglGetAttribLocationARB((GLhandleARB) programObj,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_glBlendBarrierKHR(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglBlendBarrierKHR();
}

void ecb_glMaxShaderCompilerThreadsKHR(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint count;
  if(!enif_get_uint(env, argv[0],  &count)) Badarg(6014,"count");
  weglMaxShaderCompilerThreadsKHR(count);
}

void ecb_glDepthBoundsEXT(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampd zmin;
  GLclampd zmax;
  if(!enif_get_double(env, argv[0],  &zmin)) Badarg(6015,"zmin");
  if(!enif_get_double(env, argv[1],  &zmax)) Badarg(6015,"zmax");
  weglDepthBoundsEXT(zmin,zmax);
}



#include "gl_finit.h"

