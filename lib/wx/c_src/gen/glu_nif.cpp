/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
extern "C" {
 #include "../egl_impl.h"
 #include "gl_fdefs.h"
}

void ecb_gluBuild1DMipmapLevels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLenum format;
  GLenum type;
  GLint level;
  GLint base;
  GLint max;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5010,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5010,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5010,"width");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5010,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5010,"type");
  if(!enif_get_int(env, argv[5],  &level)) Badarg(5010,"level");
  if(!enif_get_int(env, argv[6],  &base)) Badarg(5010,"base");
  if(!enif_get_int(env, argv[7],  &max)) Badarg(5010,"max");
  if(!enif_inspect_binary(env, argv[8], &data)) Badarg(5010,"data");
  result = wegluBuild1DMipmapLevels(target,internalFormat,width,format,type,level,base,max,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluBuild1DMipmaps(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5011,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5011,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5011,"width");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5011,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5011,"type");
  if(!enif_inspect_binary(env, argv[5], &data)) Badarg(5011,"data");
  result = wegluBuild1DMipmaps(target,internalFormat,width,format,type,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluBuild2DMipmapLevels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  GLint level;
  GLint base;
  GLint max;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5012,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5012,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5012,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5012,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5012,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5012,"type");
  if(!enif_get_int(env, argv[6],  &level)) Badarg(5012,"level");
  if(!enif_get_int(env, argv[7],  &base)) Badarg(5012,"base");
  if(!enif_get_int(env, argv[8],  &max)) Badarg(5012,"max");
  if(!enif_inspect_binary(env, argv[9], &data)) Badarg(5012,"data");
  result = wegluBuild2DMipmapLevels(target,internalFormat,width,height,format,type,level,base,max,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluBuild2DMipmaps(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5013,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5013,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5013,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5013,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5013,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5013,"type");
  if(!enif_inspect_binary(env, argv[6], &data)) Badarg(5013,"data");
  result = wegluBuild2DMipmaps(target,internalFormat,width,height,format,type,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluBuild3DMipmapLevels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLenum type;
  GLint level;
  GLint base;
  GLint max;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5014,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5014,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5014,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5014,"height");
  if(!enif_get_int(env, argv[4],  &depth)) Badarg(5014,"depth");
  if(!enif_get_uint(env, argv[5],  &format)) Badarg(5014,"format");
  if(!enif_get_uint(env, argv[6],  &type)) Badarg(5014,"type");
  if(!enif_get_int(env, argv[7],  &level)) Badarg(5014,"level");
  if(!enif_get_int(env, argv[8],  &base)) Badarg(5014,"base");
  if(!enif_get_int(env, argv[9],  &max)) Badarg(5014,"max");
  if(!enif_inspect_binary(env, argv[10], &data)) Badarg(5014,"data");
  result = wegluBuild3DMipmapLevels(target,internalFormat,width,height,depth,format,type,level,base,max,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluBuild3DMipmaps(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5015,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5015,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5015,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5015,"height");
  if(!enif_get_int(env, argv[4],  &depth)) Badarg(5015,"depth");
  if(!enif_get_uint(env, argv[5],  &format)) Badarg(5015,"format");
  if(!enif_get_uint(env, argv[6],  &type)) Badarg(5015,"type");
  if(!enif_inspect_binary(env, argv[7], &data)) Badarg(5015,"data");
  result = wegluBuild3DMipmaps(target,internalFormat,width,height,depth,format,type,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluCheckExtension(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  ErlNifBinary extName;
  ErlNifBinary extString;
  if(!enif_inspect_binary(env, argv[0], &extName)) Badarg(5016,"extName");
  if(!enif_inspect_binary(env, argv[1], &extString)) Badarg(5016,"extString");
  result = wegluCheckExtension((GLubyte *) extName.data,(GLubyte *) extString.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluCylinder(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLdouble base;
  GLdouble top;
  GLdouble height;
  GLint slices;
  GLint stacks;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5017,"quad");
  if(!enif_get_double(env, argv[1],  &base)) Badarg(5017,"base");
  if(!enif_get_double(env, argv[2],  &top)) Badarg(5017,"top");
  if(!enif_get_double(env, argv[3],  &height)) Badarg(5017,"height");
  if(!enif_get_int(env, argv[4],  &slices)) Badarg(5017,"slices");
  if(!enif_get_int(env, argv[5],  &stacks)) Badarg(5017,"stacks");
  wegluCylinder(quad,base,top,height,slices,stacks);
}

void ecb_gluDeleteQuadric(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5018,"quad");
  wegluDeleteQuadric(quad);
}

void ecb_gluDisk(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLdouble inner;
  GLdouble outer;
  GLint slices;
  GLint loops;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5019,"quad");
  if(!enif_get_double(env, argv[1],  &inner)) Badarg(5019,"inner");
  if(!enif_get_double(env, argv[2],  &outer)) Badarg(5019,"outer");
  if(!enif_get_int(env, argv[3],  &slices)) Badarg(5019,"slices");
  if(!enif_get_int(env, argv[4],  &loops)) Badarg(5019,"loops");
  wegluDisk(quad,inner,outer,slices,loops);
}

void ecb_gluErrorString(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  const GLubyte *  result;
  ERL_NIF_TERM reply;
  GLenum error;
  if(!enif_get_uint(env, argv[0],  &error)) Badarg(5020,"error");
  result = wegluErrorString(error);
  reply =      enif_make_string(env, (const char *) result, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluGetString(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  const GLubyte *  result;
  ERL_NIF_TERM reply;
  GLenum name;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5021,"name");
  result = wegluGetString(name);
  reply =      enif_make_string(env, (const char *) result, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluLookAt(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble eyeX;
  GLdouble eyeY;
  GLdouble eyeZ;
  GLdouble centerX;
  GLdouble centerY;
  GLdouble centerZ;
  GLdouble upX;
  GLdouble upY;
  GLdouble upZ;
  if(!enif_get_double(env, argv[0],  &eyeX)) Badarg(5022,"eyeX");
  if(!enif_get_double(env, argv[1],  &eyeY)) Badarg(5022,"eyeY");
  if(!enif_get_double(env, argv[2],  &eyeZ)) Badarg(5022,"eyeZ");
  if(!enif_get_double(env, argv[3],  &centerX)) Badarg(5022,"centerX");
  if(!enif_get_double(env, argv[4],  &centerY)) Badarg(5022,"centerY");
  if(!enif_get_double(env, argv[5],  &centerZ)) Badarg(5022,"centerZ");
  if(!enif_get_double(env, argv[6],  &upX)) Badarg(5022,"upX");
  if(!enif_get_double(env, argv[7],  &upY)) Badarg(5022,"upY");
  if(!enif_get_double(env, argv[8],  &upZ)) Badarg(5022,"upZ");
  wegluLookAt(eyeX,eyeY,eyeZ,centerX,centerY,centerZ,upX,upY,upZ);
}

void ecb_gluNewQuadric(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *  result;
  ERL_NIF_TERM reply;
  result = wegluNewQuadric();
  reply =      enif_make_uint64(env, (egl_uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluOrtho2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble left;
  GLdouble right;
  GLdouble bottom;
  GLdouble top;
  if(!enif_get_double(env, argv[0],  &left)) Badarg(5024,"left");
  if(!enif_get_double(env, argv[1],  &right)) Badarg(5024,"right");
  if(!enif_get_double(env, argv[2],  &bottom)) Badarg(5024,"bottom");
  if(!enif_get_double(env, argv[3],  &top)) Badarg(5024,"top");
  wegluOrtho2D(left,right,bottom,top);
}

void ecb_gluPartialDisk(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLdouble inner;
  GLdouble outer;
  GLint slices;
  GLint loops;
  GLdouble start;
  GLdouble sweep;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5025,"quad");
  if(!enif_get_double(env, argv[1],  &inner)) Badarg(5025,"inner");
  if(!enif_get_double(env, argv[2],  &outer)) Badarg(5025,"outer");
  if(!enif_get_int(env, argv[3],  &slices)) Badarg(5025,"slices");
  if(!enif_get_int(env, argv[4],  &loops)) Badarg(5025,"loops");
  if(!enif_get_double(env, argv[5],  &start)) Badarg(5025,"start");
  if(!enif_get_double(env, argv[6],  &sweep)) Badarg(5025,"sweep");
  wegluPartialDisk(quad,inner,outer,slices,loops,start,sweep);
}

void ecb_gluPerspective(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble fovy;
  GLdouble aspect;
  GLdouble zNear;
  GLdouble zFar;
  if(!enif_get_double(env, argv[0],  &fovy)) Badarg(5026,"fovy");
  if(!enif_get_double(env, argv[1],  &aspect)) Badarg(5026,"aspect");
  if(!enif_get_double(env, argv[2],  &zNear)) Badarg(5026,"zNear");
  if(!enif_get_double(env, argv[3],  &zFar)) Badarg(5026,"zFar");
  wegluPerspective(fovy,aspect,zNear,zFar);
}

void ecb_gluPickMatrix(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble delX;
  GLdouble delY;
  GLint viewport[4];
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5027,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5027,"y");
  if(!enif_get_double(env, argv[2],  &delX)) Badarg(5027,"delX");
  if(!enif_get_double(env, argv[3],  &delY)) Badarg(5027,"delY");
  {
   int viewport_a;
   const ERL_NIF_TERM *viewport_t;
   if(!enif_get_tuple(env, argv[4], &viewport_a, &viewport_t) || viewport_a != 4) {
     Badarg(5027,"viewport");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, viewport_t[i1++], &viewport[0])) Badarg(5027,"viewport");
     if(!enif_get_int(env, viewport_t[i1++], &viewport[1])) Badarg(5027,"viewport");
     if(!enif_get_int(env, viewport_t[i1++], &viewport[2])) Badarg(5027,"viewport");
     if(!enif_get_int(env, viewport_t[i1++], &viewport[3])) Badarg(5027,"viewport");
   }};
  wegluPickMatrix(x,y,delX,delY,viewport);
}

void ecb_gluProject(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLdouble objX;
  GLdouble objY;
  GLdouble objZ;
  GLdouble model[16];
  GLdouble proj[16];
  GLint view[4];
  GLdouble winX;
  GLdouble winY;
  GLdouble winZ;
  if(!enif_get_double(env, argv[0],  &objX)) Badarg(5028,"objX");
  if(!enif_get_double(env, argv[1],  &objY)) Badarg(5028,"objY");
  if(!enif_get_double(env, argv[2],  &objZ)) Badarg(5028,"objZ");
  {
   int model_a;
   const ERL_NIF_TERM *model_t;
   if(!enif_get_tuple(env, argv[3], &model_a, &model_t)
       || (model_a != 12 && model_a != 16)) {
     Badarg(5028,"model");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, model_t[i1++], &model[0])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[1])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[2])) Badarg(5028,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[3])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[4])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[5])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[6])) Badarg(5028,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[7])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[8])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[9])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[10])) Badarg(5028,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[11])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[12])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[13])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[14])) Badarg(5028,"model");
     if(model_a == 16) {
        if(!enif_get_double(env, model_t[i1++], &model[15])) Badarg(5028,"model");
     } else {
       model[3] = 0.0; model[7] = 0.0; model[11] = 0.0; model[15] = 1.0;
     }
   }};
  {
   int proj_a;
   const ERL_NIF_TERM *proj_t;
   if(!enif_get_tuple(env, argv[4], &proj_a, &proj_t)
       || (proj_a != 12 && proj_a != 16)) {
     Badarg(5028,"proj");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, proj_t[i1++], &proj[0])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[1])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[2])) Badarg(5028,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[3])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[4])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[5])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[6])) Badarg(5028,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[7])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[8])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[9])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[10])) Badarg(5028,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[11])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[12])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[13])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[14])) Badarg(5028,"proj");
     if(proj_a == 16) {
        if(!enif_get_double(env, proj_t[i1++], &proj[15])) Badarg(5028,"proj");
     } else {
       proj[3] = 0.0; proj[7] = 0.0; proj[11] = 0.0; proj[15] = 1.0;
     }
   }};
  {
   int view_a;
   const ERL_NIF_TERM *view_t;
   if(!enif_get_tuple(env, argv[5], &view_a, &view_t) || view_a != 4) {
     Badarg(5028,"view");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, view_t[i1++], &view[0])) Badarg(5028,"view");
     if(!enif_get_int(env, view_t[i1++], &view[1])) Badarg(5028,"view");
     if(!enif_get_int(env, view_t[i1++], &view[2])) Badarg(5028,"view");
     if(!enif_get_int(env, view_t[i1++], &view[3])) Badarg(5028,"view");
   }};
  result = wegluProject(objX,objY,objZ,model,proj,view,&winX,&winY,&winZ);
  reply = enif_make_tuple4(env,
          enif_make_int(env, result),
     enif_make_double(env, winX),
     enif_make_double(env, winY),
     enif_make_double(env, winZ) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluQuadricDrawStyle(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLenum draw;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5029,"quad");
  if(!enif_get_uint(env, argv[1],  &draw)) Badarg(5029,"draw");
  wegluQuadricDrawStyle(quad,draw);
}

void ecb_gluQuadricNormals(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLenum normal;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5030,"quad");
  if(!enif_get_uint(env, argv[1],  &normal)) Badarg(5030,"normal");
  wegluQuadricNormals(quad,normal);
}

void ecb_gluQuadricOrientation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLenum orientation;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5031,"quad");
  if(!enif_get_uint(env, argv[1],  &orientation)) Badarg(5031,"orientation");
  wegluQuadricOrientation(quad,orientation);
}

void ecb_gluQuadricTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLboolean texture;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5032,"quad");
  if(!egl_get_ubyte(env, argv[1],  &texture)) Badarg(5032,"texture");
  wegluQuadricTexture(quad,texture);
}

void ecb_gluScaleImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum format;
  GLsizei wIn;
  GLsizei hIn;
  GLenum typeIn;
  ErlNifBinary dataIn;
  GLsizei wOut;
  GLsizei hOut;
  GLenum typeOut;
  ErlNifBinary dataOut;
  if(!enif_get_uint(env, argv[0],  &format)) Badarg(5033,"format");
  if(!enif_get_int(env, argv[1],  &wIn)) Badarg(5033,"wIn");
  if(!enif_get_int(env, argv[2],  &hIn)) Badarg(5033,"hIn");
  if(!enif_get_uint(env, argv[3],  &typeIn)) Badarg(5033,"typeIn");
  if(!enif_inspect_binary(env, argv[4], &dataIn)) Badarg(5033,"dataIn");
  if(!enif_get_int(env, argv[5],  &wOut)) Badarg(5033,"wOut");
  if(!enif_get_int(env, argv[6],  &hOut)) Badarg(5033,"hOut");
  if(!enif_get_uint(env, argv[7],  &typeOut)) Badarg(5033,"typeOut");
  if(enif_is_binary(env, argv[8]))
    enif_inspect_binary(env, argv[8], &dataOut);
  else if(enif_is_tuple(env, argv[8])) {
    int dataOut_a;
    const ERL_NIF_TERM *dataOut_t;
    if(enif_get_tuple(env, argv[8], &dataOut_a, &dataOut_t) &&
         enif_is_binary(env, dataOut_t[1]))
       enif_inspect_binary(env, dataOut_t[1], &dataOut);
    else Badarg(5033, "dataOut");
  } else Badarg(5033, "dataOut");
  result = wegluScaleImage(format,wIn,hIn,typeIn,(void *) dataIn.data,wOut,hOut,typeOut,(GLvoid *) dataOut.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluSphere(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLdouble radius;
  GLint slices;
  GLint stacks;
  if(!egl_get_ptr(env, argv[0], (void **) &quad)) Badarg(5034,"quad");
  if(!enif_get_double(env, argv[1],  &radius)) Badarg(5034,"radius");
  if(!enif_get_int(env, argv[2],  &slices)) Badarg(5034,"slices");
  if(!enif_get_int(env, argv[3],  &stacks)) Badarg(5034,"stacks");
  wegluSphere(quad,radius,slices,stacks);
}

void ecb_gluUnProject(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLdouble winX;
  GLdouble winY;
  GLdouble winZ;
  GLdouble model[16];
  GLdouble proj[16];
  GLint view[4];
  GLdouble objX;
  GLdouble objY;
  GLdouble objZ;
  if(!enif_get_double(env, argv[0],  &winX)) Badarg(5035,"winX");
  if(!enif_get_double(env, argv[1],  &winY)) Badarg(5035,"winY");
  if(!enif_get_double(env, argv[2],  &winZ)) Badarg(5035,"winZ");
  {
   int model_a;
   const ERL_NIF_TERM *model_t;
   if(!enif_get_tuple(env, argv[3], &model_a, &model_t)
       || (model_a != 12 && model_a != 16)) {
     Badarg(5035,"model");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, model_t[i1++], &model[0])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[1])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[2])) Badarg(5035,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[3])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[4])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[5])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[6])) Badarg(5035,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[7])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[8])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[9])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[10])) Badarg(5035,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[11])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[12])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[13])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[14])) Badarg(5035,"model");
     if(model_a == 16) {
        if(!enif_get_double(env, model_t[i1++], &model[15])) Badarg(5035,"model");
     } else {
       model[3] = 0.0; model[7] = 0.0; model[11] = 0.0; model[15] = 1.0;
     }
   }};
  {
   int proj_a;
   const ERL_NIF_TERM *proj_t;
   if(!enif_get_tuple(env, argv[4], &proj_a, &proj_t)
       || (proj_a != 12 && proj_a != 16)) {
     Badarg(5035,"proj");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, proj_t[i1++], &proj[0])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[1])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[2])) Badarg(5035,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[3])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[4])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[5])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[6])) Badarg(5035,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[7])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[8])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[9])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[10])) Badarg(5035,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[11])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[12])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[13])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[14])) Badarg(5035,"proj");
     if(proj_a == 16) {
        if(!enif_get_double(env, proj_t[i1++], &proj[15])) Badarg(5035,"proj");
     } else {
       proj[3] = 0.0; proj[7] = 0.0; proj[11] = 0.0; proj[15] = 1.0;
     }
   }};
  {
   int view_a;
   const ERL_NIF_TERM *view_t;
   if(!enif_get_tuple(env, argv[5], &view_a, &view_t) || view_a != 4) {
     Badarg(5035,"view");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, view_t[i1++], &view[0])) Badarg(5035,"view");
     if(!enif_get_int(env, view_t[i1++], &view[1])) Badarg(5035,"view");
     if(!enif_get_int(env, view_t[i1++], &view[2])) Badarg(5035,"view");
     if(!enif_get_int(env, view_t[i1++], &view[3])) Badarg(5035,"view");
   }};
  result = wegluUnProject(winX,winY,winZ,model,proj,view,&objX,&objY,&objZ);
  reply = enif_make_tuple4(env,
          enif_make_int(env, result),
     enif_make_double(env, objX),
     enif_make_double(env, objY),
     enif_make_double(env, objZ) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

void ecb_gluUnProject4(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLdouble winX;
  GLdouble winY;
  GLdouble winZ;
  GLdouble clipW;
  GLdouble model[16];
  GLdouble proj[16];
  GLint view[4];
  GLdouble nearVal;
  GLdouble farVal;
  GLdouble objX;
  GLdouble objY;
  GLdouble objZ;
  GLdouble objW;
  if(!enif_get_double(env, argv[0],  &winX)) Badarg(5036,"winX");
  if(!enif_get_double(env, argv[1],  &winY)) Badarg(5036,"winY");
  if(!enif_get_double(env, argv[2],  &winZ)) Badarg(5036,"winZ");
  if(!enif_get_double(env, argv[3],  &clipW)) Badarg(5036,"clipW");
  {
   int model_a;
   const ERL_NIF_TERM *model_t;
   if(!enif_get_tuple(env, argv[4], &model_a, &model_t)
       || (model_a != 12 && model_a != 16)) {
     Badarg(5036,"model");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, model_t[i1++], &model[0])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[1])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[2])) Badarg(5036,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[3])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[4])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[5])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[6])) Badarg(5036,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[7])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[8])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[9])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[10])) Badarg(5036,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[11])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[12])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[13])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[14])) Badarg(5036,"model");
     if(model_a == 16) {
        if(!enif_get_double(env, model_t[i1++], &model[15])) Badarg(5036,"model");
     } else {
       model[3] = 0.0; model[7] = 0.0; model[11] = 0.0; model[15] = 1.0;
     }
   }};
  {
   int proj_a;
   const ERL_NIF_TERM *proj_t;
   if(!enif_get_tuple(env, argv[5], &proj_a, &proj_t)
       || (proj_a != 12 && proj_a != 16)) {
     Badarg(5036,"proj");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, proj_t[i1++], &proj[0])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[1])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[2])) Badarg(5036,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[3])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[4])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[5])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[6])) Badarg(5036,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[7])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[8])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[9])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[10])) Badarg(5036,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[11])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[12])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[13])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[14])) Badarg(5036,"proj");
     if(proj_a == 16) {
        if(!enif_get_double(env, proj_t[i1++], &proj[15])) Badarg(5036,"proj");
     } else {
       proj[3] = 0.0; proj[7] = 0.0; proj[11] = 0.0; proj[15] = 1.0;
     }
   }};
  {
   int view_a;
   const ERL_NIF_TERM *view_t;
   if(!enif_get_tuple(env, argv[6], &view_a, &view_t) || view_a != 4) {
     Badarg(5036,"view");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, view_t[i1++], &view[0])) Badarg(5036,"view");
     if(!enif_get_int(env, view_t[i1++], &view[1])) Badarg(5036,"view");
     if(!enif_get_int(env, view_t[i1++], &view[2])) Badarg(5036,"view");
     if(!enif_get_int(env, view_t[i1++], &view[3])) Badarg(5036,"view");
   }};
  if(!enif_get_double(env, argv[7],  &nearVal)) Badarg(5036,"nearVal");
  if(!enif_get_double(env, argv[8],  &farVal)) Badarg(5036,"farVal");
  result = wegluUnProject4(winX,winY,winZ,clipW,model,proj,view,nearVal,farVal,&objX,&objY,&objZ,&objW);
  reply = enif_make_tuple5(env,
          enif_make_int(env, result),
     enif_make_double(env, objX),
     enif_make_double(env, objY),
     enif_make_double(env, objZ),
     enif_make_double(env, objW) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

