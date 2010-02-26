/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd% 
*/
/***** This file is generated do not edit ****/

#include <stdio.h>
#include <string.h>
#include "../wxe_impl.h"
#include "../wxe_gl.h"
#include "gl_fdefs.h"

int gl_error_op;
void gl_dispatch(int op, char *bp,ErlDrvTermData caller,WXEBinRef *bins[]){
 gl_error_op = op;
 if(caller != gl_active) {
   wxGLCanvas * current = glc[caller];
   if(current) { gl_active = caller; current->SetCurrent();}
   else {
       ErlDrvTermData rt[] = // Error msg
      {ERL_DRV_ATOM, driver_mk_atom((char *) "_wxe_error_"),
       ERL_DRV_INT,  op,
       ERL_DRV_ATOM, driver_mk_atom((char *) "no_gl_context"),
       ERL_DRV_TUPLE,3};
      driver_send_term(WXE_DRV_PORT,caller,rt,8);
      return ;
   }
 };

 switch(op)
{
 case 5000:
   wxe_tess_impl(bp, caller);
   break;
 case WXE_BIN_INCR:
   driver_binary_inc_refc(bins[0]->bin);
   break;
 case WXE_BIN_DECR:
   driver_binary_dec_refc(bins[0]->bin);
   break;
case 5010: { // gluBuild1DMipmapLevels
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *internalFormat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *base = (GLint *) bp; bp += 4;
 GLint *max = (GLint *) bp; bp += 4;
 void *data = (void *) bins[0]->base;
 GLint result = wegluBuild1DMipmapLevels(*target,*internalFormat,*width,*format,*type,*level,*base,*max,data);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5011: { // gluBuild1DMipmaps
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *internalFormat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 void *data = (void *) bins[0]->base;
 GLint result = wegluBuild1DMipmaps(*target,*internalFormat,*width,*format,*type,data);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5012: { // gluBuild2DMipmapLevels
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *internalFormat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *base = (GLint *) bp; bp += 4;
 GLint *max = (GLint *) bp; bp += 4;
 void *data = (void *) bins[0]->base;
 GLint result = wegluBuild2DMipmapLevels(*target,*internalFormat,*width,*height,*format,*type,*level,*base,*max,data);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5013: { // gluBuild2DMipmaps
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *internalFormat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 void *data = (void *) bins[0]->base;
 GLint result = wegluBuild2DMipmaps(*target,*internalFormat,*width,*height,*format,*type,data);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5014: { // gluBuild3DMipmapLevels
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *internalFormat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *base = (GLint *) bp; bp += 4;
 GLint *max = (GLint *) bp; bp += 4;
 void *data = (void *) bins[0]->base;
 GLint result = wegluBuild3DMipmapLevels(*target,*internalFormat,*width,*height,*depth,*format,*type,*level,*base,*max,data);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5015: { // gluBuild3DMipmaps
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *internalFormat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 void *data = (void *) bins[0]->base;
 GLint result = wegluBuild3DMipmaps(*target,*internalFormat,*width,*height,*depth,*format,*type,data);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5016: { // gluCheckExtension
 int * extNameLen = (int *) bp; bp += 4;
 GLubyte * extName = (GLubyte *) bp;  bp += (8-((*extNameLen*1+4)%8))%8;
 int * extStringLen = (int *) bp; bp += 4;
 GLubyte * extString = (GLubyte *) bp;  bp += (8-((*extStringLen*1+4)%8))%8;
 GLboolean result = wegluCheckExtension(extName,extString);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5017: { // gluCylinder
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 GLdouble *base = (GLdouble *) bp; bp += 8;
 GLdouble *top = (GLdouble *) bp; bp += 8;
 GLdouble *height = (GLdouble *) bp; bp += 8;
 GLint *slices = (GLint *) bp; bp += 4;
 GLint *stacks = (GLint *) bp; bp += 4;
 wegluCylinder(quad,*base,*top,*height,*slices,*stacks);
}; break;
case 5018: { // gluDeleteQuadric
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 wegluDeleteQuadric(quad);
}; break;
case 5019: { // gluDisk
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 GLdouble *inner = (GLdouble *) bp; bp += 8;
 GLdouble *outer = (GLdouble *) bp; bp += 8;
 GLint *slices = (GLint *) bp; bp += 4;
 GLint *loops = (GLint *) bp; bp += 4;
 wegluDisk(quad,*inner,*outer,*slices,*loops);
}; break;
case 5020: { // gluErrorString
 GLenum *error = (GLenum *) bp; bp += 4;
 const GLubyte *  result = wegluErrorString(*error);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) result; rt[AP++] = strlen((char *) result);
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5021: { // gluGetString
 GLenum *name = (GLenum *) bp; bp += 4;
 const GLubyte *  result = wegluGetString(*name);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) result; rt[AP++] = strlen((char *) result);
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5022: { // gluLookAt
 GLdouble *eyeX = (GLdouble *) bp; bp += 8;
 GLdouble *eyeY = (GLdouble *) bp; bp += 8;
 GLdouble *eyeZ = (GLdouble *) bp; bp += 8;
 GLdouble *centerX = (GLdouble *) bp; bp += 8;
 GLdouble *centerY = (GLdouble *) bp; bp += 8;
 GLdouble *centerZ = (GLdouble *) bp; bp += 8;
 GLdouble *upX = (GLdouble *) bp; bp += 8;
 GLdouble *upY = (GLdouble *) bp; bp += 8;
 GLdouble *upZ = (GLdouble *) bp; bp += 8;
 wegluLookAt(*eyeX,*eyeY,*eyeZ,*centerX,*centerY,*centerZ,*upX,*upY,*upZ);
}; break;
case 5023: { // gluNewQuadric
 GLUquadric *  result = wegluNewQuadric();
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5024: { // gluOrtho2D
 GLdouble *left = (GLdouble *) bp; bp += 8;
 GLdouble *right = (GLdouble *) bp; bp += 8;
 GLdouble *bottom = (GLdouble *) bp; bp += 8;
 GLdouble *top = (GLdouble *) bp; bp += 8;
 wegluOrtho2D(*left,*right,*bottom,*top);
}; break;
case 5025: { // gluPartialDisk
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 GLdouble *inner = (GLdouble *) bp; bp += 8;
 GLdouble *outer = (GLdouble *) bp; bp += 8;
 GLint *slices = (GLint *) bp; bp += 4;
 GLint *loops = (GLint *) bp; bp += 4;
 GLdouble *start = (GLdouble *) bp; bp += 8;
 GLdouble *sweep = (GLdouble *) bp; bp += 8;
 wegluPartialDisk(quad,*inner,*outer,*slices,*loops,*start,*sweep);
}; break;
case 5026: { // gluPerspective
 GLdouble *fovy = (GLdouble *) bp; bp += 8;
 GLdouble *aspect = (GLdouble *) bp; bp += 8;
 GLdouble *zNear = (GLdouble *) bp; bp += 8;
 GLdouble *zFar = (GLdouble *) bp; bp += 8;
 wegluPerspective(*fovy,*aspect,*zNear,*zFar);
}; break;
case 5027: { // gluPickMatrix
 GLdouble *x = (GLdouble *) bp; bp += 8;
 GLdouble *y = (GLdouble *) bp; bp += 8;
 GLdouble *delX = (GLdouble *) bp; bp += 8;
 GLdouble *delY = (GLdouble *) bp; bp += 8;
 GLint * viewport = (GLint *) bp; bp += 16;
 wegluPickMatrix(*x,*y,*delX,*delY,viewport);
}; break;
case 5028: { // gluProject
 GLdouble *objX = (GLdouble *) bp; bp += 8;
 GLdouble *objY = (GLdouble *) bp; bp += 8;
 GLdouble *objZ = (GLdouble *) bp; bp += 8;
 GLdouble * model = (GLdouble *) bp; bp += 128;
 GLdouble * proj = (GLdouble *) bp; bp += 128;
 GLint * view = (GLint *) bp; bp += 16;
 GLdouble winX[1] = {0.0};
 GLdouble winY[1] = {0.0};
 GLdouble winZ[1] = {0.0};
 GLint result = wegluProject(*objX,*objY,*objZ,model,proj,view,winX,winY,winZ);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) winX;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) winY;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) winZ;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5029: { // gluQuadricDrawStyle
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 GLenum *draw = (GLenum *) bp; bp += 4;
 wegluQuadricDrawStyle(quad,*draw);
}; break;
case 5030: { // gluQuadricNormals
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 GLenum *normal = (GLenum *) bp; bp += 4;
 wegluQuadricNormals(quad,*normal);
}; break;
case 5031: { // gluQuadricOrientation
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 GLenum *orientation = (GLenum *) bp; bp += 4;
 wegluQuadricOrientation(quad,*orientation);
}; break;
case 5032: { // gluQuadricTexture
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 GLboolean *texture = (GLboolean *) bp; bp += 1;
 wegluQuadricTexture(quad,*texture);
}; break;
case 5033: { // gluScaleImage
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *wIn = (GLsizei *) bp; bp += 4;
 GLsizei *hIn = (GLsizei *) bp; bp += 4;
 GLenum *typeIn = (GLenum *) bp; bp += 4;
 void *dataIn = (void *) bins[0]->base;
 GLsizei *wOut = (GLsizei *) bp; bp += 4;
 GLsizei *hOut = (GLsizei *) bp; bp += 4;
 GLenum *typeOut = (GLenum *) bp; bp += 4;
 GLvoid *dataOut = (GLvoid *) bins[1]->base;
 GLint result = wegluScaleImage(*format,*wIn,*hIn,*typeIn,dataIn,*wOut,*hOut,*typeOut,dataOut);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5034: { // gluSphere
 GLUquadric * quad = (GLUquadric *) * (GLuint64EXT *) bp; bp += 8;
 GLdouble *radius = (GLdouble *) bp; bp += 8;
 GLint *slices = (GLint *) bp; bp += 4;
 GLint *stacks = (GLint *) bp; bp += 4;
 wegluSphere(quad,*radius,*slices,*stacks);
}; break;
case 5035: { // gluUnProject
 GLdouble *winX = (GLdouble *) bp; bp += 8;
 GLdouble *winY = (GLdouble *) bp; bp += 8;
 GLdouble *winZ = (GLdouble *) bp; bp += 8;
 GLdouble * model = (GLdouble *) bp; bp += 128;
 GLdouble * proj = (GLdouble *) bp; bp += 128;
 GLint * view = (GLint *) bp; bp += 16;
 GLdouble objX[1] = {0.0};
 GLdouble objY[1] = {0.0};
 GLdouble objZ[1] = {0.0};
 GLint result = wegluUnProject(*winX,*winY,*winZ,model,proj,view,objX,objY,objZ);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) objX;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) objY;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) objZ;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5036: { // gluUnProject4
 GLdouble *winX = (GLdouble *) bp; bp += 8;
 GLdouble *winY = (GLdouble *) bp; bp += 8;
 GLdouble *winZ = (GLdouble *) bp; bp += 8;
 GLdouble *clipW = (GLdouble *) bp; bp += 8;
 GLdouble * model = (GLdouble *) bp; bp += 128;
 GLdouble * proj = (GLdouble *) bp; bp += 128;
 GLint * view = (GLint *) bp; bp += 16;
 GLdouble *nearVal = (GLdouble *) bp; bp += 8;
 GLdouble *farVal = (GLdouble *) bp; bp += 8;
 GLdouble objX[1] = {0.0};
 GLdouble objY[1] = {0.0};
 GLdouble objZ[1] = {0.0};
 GLdouble objW[1] = {0.0};
 GLint result = wegluUnProject4(*winX,*winY,*winZ,*clipW,model,proj,view,*nearVal,*farVal,objX,objY,objZ,objW);
 int AP = 0; ErlDrvTermData rt[16];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) objX;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) objY;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) objZ;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) objW;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 5;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 16 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,16);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5037: { // glAccum
 GLenum *op = (GLenum *) bp; bp += 4;
 GLfloat *value = (GLfloat *) bp; bp += 4;
 weglAccum(*op,*value);
}; break;
case 5038: { // glAlphaFunc
 GLenum *func = (GLenum *) bp; bp += 4;
 GLclampf *ref = (GLclampf *) bp; bp += 4;
 weglAlphaFunc(*func,*ref);
}; break;
case 5039: { // glAreTexturesResident
 int * texturesLen = (int *) bp; bp += 4;
 GLuint * textures = (GLuint *) bp;  bp += (8-((*texturesLen*4+4)%8))%8;
 GLboolean *residences;
 residences = (GLboolean *) driver_alloc(sizeof(GLboolean) * *texturesLen);
 GLboolean result = weglAreTexturesResident(*texturesLen,textures,residences);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(11 + (*texturesLen)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 for(int i=0; i < *texturesLen; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) residences[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*texturesLen)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 11 + (*texturesLen)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,11 + (*texturesLen)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(residences);
}; break;
case 5040: { // glArrayElement
 GLint *i = (GLint *) bp; bp += 4;
 weglArrayElement(*i);
}; break;
case 5041: { // glBegin
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglBegin(*mode);
}; break;
case 5042: { // glBindTexture
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *texture = (GLuint *) bp; bp += 4;
 weglBindTexture(*target,*texture);
}; break;
case 5043: { // glBitmap
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLfloat *xorig = (GLfloat *) bp; bp += 4;
 GLfloat *yorig = (GLfloat *) bp; bp += 4;
 GLfloat *xmove = (GLfloat *) bp; bp += 4;
 GLfloat *ymove = (GLfloat *) bp; bp += 4;
 GLubyte *bitmap = (GLubyte *) * (int *) bp; bp += 4;
 weglBitmap(*width,*height,*xorig,*yorig,*xmove,*ymove,bitmap);
}; break;
case 5044: { // glBitmap
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLfloat *xorig = (GLfloat *) bp; bp += 4;
 GLfloat *yorig = (GLfloat *) bp; bp += 4;
 GLfloat *xmove = (GLfloat *) bp; bp += 4;
 GLfloat *ymove = (GLfloat *) bp; bp += 4;
 GLubyte *bitmap = (GLubyte *) bins[0]->base;
 weglBitmap(*width,*height,*xorig,*yorig,*xmove,*ymove,bitmap);
}; break;
case 5045: { // glBlendFunc
 GLenum *sfactor = (GLenum *) bp; bp += 4;
 GLenum *dfactor = (GLenum *) bp; bp += 4;
 weglBlendFunc(*sfactor,*dfactor);
}; break;
case 5046: { // glCallList
 GLuint *list = (GLuint *) bp; bp += 4;
 weglCallList(*list);
}; break;
case 5047: { // glCallLists
 int * listsLen = (int *) bp; bp += 4;
 GLuint * lists = (GLuint *) bp;  bp += (8-((*listsLen*4+4)%8))%8;
 weglCallLists(*listsLen,GL_UNSIGNED_INT,lists);
}; break;
case 5048: { // glClear
 GLbitfield *mask = (GLbitfield *) bp; bp += 4;
 weglClear(*mask);
}; break;
case 5049: { // glClearAccum
 GLfloat *red = (GLfloat *) bp; bp += 4;
 GLfloat *green = (GLfloat *) bp; bp += 4;
 GLfloat *blue = (GLfloat *) bp; bp += 4;
 GLfloat *alpha = (GLfloat *) bp; bp += 4;
 weglClearAccum(*red,*green,*blue,*alpha);
}; break;
case 5050: { // glClearColor
 GLclampf *red = (GLclampf *) bp; bp += 4;
 GLclampf *green = (GLclampf *) bp; bp += 4;
 GLclampf *blue = (GLclampf *) bp; bp += 4;
 GLclampf *alpha = (GLclampf *) bp; bp += 4;
 weglClearColor(*red,*green,*blue,*alpha);
}; break;
case 5051: { // glClearDepth
 GLclampd *depth = (GLclampd *) bp; bp += 8;
 weglClearDepth(*depth);
}; break;
case 5052: { // glClearIndex
 GLfloat *c = (GLfloat *) bp; bp += 4;
 weglClearIndex(*c);
}; break;
case 5053: { // glClearStencil
 GLint *s = (GLint *) bp; bp += 4;
 weglClearStencil(*s);
}; break;
case 5054: { // glClipPlane
 GLenum *plane = (GLenum *) bp; bp += 4;
 bp += 4;
 GLdouble * equation = (GLdouble *) bp; bp += 32;
 weglClipPlane(*plane,equation);
}; break;
case 5055: { // glColor3bv
 GLbyte *v = (GLbyte *) bp; bp += 1;
 weglColor3bv(v);
}; break;
case 5056: { // glColor3dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglColor3dv(v);
}; break;
case 5057: { // glColor3fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglColor3fv(v);
}; break;
case 5058: { // glColor3iv
 GLint *v = (GLint *) bp; bp += 4;
 weglColor3iv(v);
}; break;
case 5059: { // glColor3sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglColor3sv(v);
}; break;
case 5060: { // glColor3ubv
 GLubyte *v = (GLubyte *) bp; bp += 1;
 weglColor3ubv(v);
}; break;
case 5061: { // glColor3uiv
 GLuint *v = (GLuint *) bp; bp += 4;
 weglColor3uiv(v);
}; break;
case 5062: { // glColor3usv
 GLushort *v = (GLushort *) bp; bp += 2;
 weglColor3usv(v);
}; break;
case 5063: { // glColor4bv
 GLbyte *v = (GLbyte *) bp; bp += 1;
 weglColor4bv(v);
}; break;
case 5064: { // glColor4dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglColor4dv(v);
}; break;
case 5065: { // glColor4fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglColor4fv(v);
}; break;
case 5066: { // glColor4iv
 GLint *v = (GLint *) bp; bp += 4;
 weglColor4iv(v);
}; break;
case 5067: { // glColor4sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglColor4sv(v);
}; break;
case 5068: { // glColor4ubv
 GLubyte *v = (GLubyte *) bp; bp += 1;
 weglColor4ubv(v);
}; break;
case 5069: { // glColor4uiv
 GLuint *v = (GLuint *) bp; bp += 4;
 weglColor4uiv(v);
}; break;
case 5070: { // glColor4usv
 GLushort *v = (GLushort *) bp; bp += 2;
 weglColor4usv(v);
}; break;
case 5071: { // glColorMask
 GLboolean *red = (GLboolean *) bp; bp += 1;
 GLboolean *green = (GLboolean *) bp; bp += 1;
 GLboolean *blue = (GLboolean *) bp; bp += 1;
 GLboolean *alpha = (GLboolean *) bp; bp += 1;
 weglColorMask(*red,*green,*blue,*alpha);
}; break;
case 5072: { // glColorMaterial
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglColorMaterial(*face,*mode);
}; break;
case 5073: { // glColorPointer
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglColorPointer(*size,*type,*stride,pointer);
}; break;
case 5074: { // glColorPointer
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglColorPointer(*size,*type,*stride,pointer);
}; break;
case 5075: { // glCopyPixels
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 weglCopyPixels(*x,*y,*width,*height,*type);
}; break;
case 5076: { // glCopyTexImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *internalFormat = (GLenum *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 weglCopyTexImage1D(*target,*level,*internalFormat,*x,*y,*width,*border);
}; break;
case 5077: { // glCopyTexImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *internalFormat = (GLenum *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 weglCopyTexImage2D(*target,*level,*internalFormat,*x,*y,*width,*height,*border);
}; break;
case 5078: { // glCopyTexSubImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 weglCopyTexSubImage1D(*target,*level,*xoffset,*x,*y,*width);
}; break;
case 5079: { // glCopyTexSubImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 weglCopyTexSubImage2D(*target,*level,*xoffset,*yoffset,*x,*y,*width,*height);
}; break;
case 5080: { // glCullFace
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglCullFace(*mode);
}; break;
case 5081: { // glDeleteLists
 GLuint *list = (GLuint *) bp; bp += 4;
 GLsizei *range = (GLsizei *) bp; bp += 4;
 weglDeleteLists(*list,*range);
}; break;
case 5082: { // glDeleteTextures
 int * texturesLen = (int *) bp; bp += 4;
 GLuint * textures = (GLuint *) bp;  bp += (8-((*texturesLen*4+4)%8))%8;
 weglDeleteTextures(*texturesLen,textures);
}; break;
case 5083: { // glDepthFunc
 GLenum *func = (GLenum *) bp; bp += 4;
 weglDepthFunc(*func);
}; break;
case 5084: { // glDepthMask
 GLboolean *flag = (GLboolean *) bp; bp += 1;
 weglDepthMask(*flag);
}; break;
case 5085: { // glDepthRange
 GLclampd *zNear = (GLclampd *) bp; bp += 8;
 GLclampd *zFar = (GLclampd *) bp; bp += 8;
 weglDepthRange(*zNear,*zFar);
}; break;
case 5086: { // glDisable
 GLenum *cap = (GLenum *) bp; bp += 4;
 weglDisable(*cap);
}; break;
case 5087: { // glDisableClientState
 GLenum *array = (GLenum *) bp; bp += 4;
 weglDisableClientState(*array);
}; break;
case 5088: { // glDrawArrays
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLint *first = (GLint *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 weglDrawArrays(*mode,*first,*count);
}; break;
case 5089: { // glDrawBuffer
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglDrawBuffer(*mode);
}; break;
case 5090: { // glDrawElements
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *indices = (GLvoid *) * (int *) bp; bp += 4;
 weglDrawElements(*mode,*count,*type,indices);
}; break;
case 5091: { // glDrawElements
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *indices = (GLvoid *) bins[0]->base;
 weglDrawElements(*mode,*count,*type,indices);
}; break;
case 5092: { // glDrawPixels
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) * (int *) bp; bp += 4;
 weglDrawPixels(*width,*height,*format,*type,pixels);
}; break;
case 5093: { // glDrawPixels
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglDrawPixels(*width,*height,*format,*type,pixels);
}; break;
case 5094: { // glEdgeFlagv
 GLboolean *flag = (GLboolean *) bp; bp += 1;
 weglEdgeFlagv(flag);
}; break;
case 5095: { // glEdgeFlagPointer
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglEdgeFlagPointer(*stride,pointer);
}; break;
case 5096: { // glEdgeFlagPointer
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglEdgeFlagPointer(*stride,pointer);
}; break;
case 5097: { // glEnable
 GLenum *cap = (GLenum *) bp; bp += 4;
 weglEnable(*cap);
}; break;
case 5098: { // glEnableClientState
 GLenum *array = (GLenum *) bp; bp += 4;
 weglEnableClientState(*array);
}; break;
case 5099: { // glEnd
 weglEnd();
}; break;
case 5100: { // glEndList
 weglEndList();
}; break;
case 5101: { // glEvalCoord1dv
 GLdouble *u = (GLdouble *) bp; bp += 8;
 weglEvalCoord1dv(u);
}; break;
case 5102: { // glEvalCoord1fv
 GLfloat *u = (GLfloat *) bp; bp += 4;
 weglEvalCoord1fv(u);
}; break;
case 5103: { // glEvalCoord2dv
 GLdouble *u = (GLdouble *) bp; bp += 8;
 weglEvalCoord2dv(u);
}; break;
case 5104: { // glEvalCoord2fv
 GLfloat *u = (GLfloat *) bp; bp += 4;
 weglEvalCoord2fv(u);
}; break;
case 5105: { // glEvalMesh1
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLint *i1 = (GLint *) bp; bp += 4;
 GLint *i2 = (GLint *) bp; bp += 4;
 weglEvalMesh1(*mode,*i1,*i2);
}; break;
case 5106: { // glEvalMesh2
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLint *i1 = (GLint *) bp; bp += 4;
 GLint *i2 = (GLint *) bp; bp += 4;
 GLint *j1 = (GLint *) bp; bp += 4;
 GLint *j2 = (GLint *) bp; bp += 4;
 weglEvalMesh2(*mode,*i1,*i2,*j1,*j2);
}; break;
case 5107: { // glEvalPoint1
 GLint *i = (GLint *) bp; bp += 4;
 weglEvalPoint1(*i);
}; break;
case 5108: { // glEvalPoint2
 GLint *i = (GLint *) bp; bp += 4;
 GLint *j = (GLint *) bp; bp += 4;
 weglEvalPoint2(*i,*j);
}; break;
case 5109: { // glFeedbackBuffer
 GLsizei *size = (GLsizei *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLfloat *buffer = (GLfloat *) bins[0]->base;
 weglFeedbackBuffer(*size,*type,buffer);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5110: { // glFinish
 weglFinish();
}; break;
case 5111: { // glFlush
 weglFlush();
}; break;
case 5112: { // glFogf
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglFogf(*pname,*param);
}; break;
case 5113: { // glFogfv
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+0)%2*4;
 weglFogfv(*pname,params);
}; break;
case 5114: { // glFogi
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglFogi(*pname,*param);
}; break;
case 5115: { // glFogiv
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+0)%2*4;
 weglFogiv(*pname,params);
}; break;
case 5116: { // glFrontFace
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglFrontFace(*mode);
}; break;
case 5117: { // glFrustum
 GLdouble *left = (GLdouble *) bp; bp += 8;
 GLdouble *right = (GLdouble *) bp; bp += 8;
 GLdouble *bottom = (GLdouble *) bp; bp += 8;
 GLdouble *top = (GLdouble *) bp; bp += 8;
 GLdouble *zNear = (GLdouble *) bp; bp += 8;
 GLdouble *zFar = (GLdouble *) bp; bp += 8;
 weglFrustum(*left,*right,*bottom,*top,*zNear,*zFar);
}; break;
case 5118: { // glGenLists
 GLsizei *range = (GLsizei *) bp; bp += 4;
 GLuint result = weglGenLists(*range);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5119: { // glGenTextures
 GLsizei *n = (GLsizei *) bp; bp += 4;
 GLuint *textures;
 textures = (GLuint *) driver_alloc(sizeof(GLuint) * *n);
 weglGenTextures(*n,textures);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*n)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *n; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) textures[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*n)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*n)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*n)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(textures);
}; break;
case 5120: { // glGetBooleanv
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLboolean params[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
 weglGetBooleanv(*pname,params);
 int AP = 0; ErlDrvTermData rt[39];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLboolean *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = 16+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 39 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,39);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5121: { // glGetClipPlane
 GLenum *plane = (GLenum *) bp; bp += 4;
 GLdouble equation[4] = {0.0,0.0,0.0,0.0};
 weglGetClipPlane(*plane,equation);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble *equationTmp = equation;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) equationTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) equationTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) equationTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) equationTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5122: { // glGetDoublev
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLdouble params[16] = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
 weglGetDoublev(*pname,params);
 int AP = 0; ErlDrvTermData rt[39];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble *paramsTmp = params;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = 16+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 39 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,39);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5123: { // glGetError
 GLenum result = weglGetError();
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5124: { // glGetFloatv
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[16] = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
 weglGetFloatv(*pname,params);
 int AP = 0; ErlDrvTermData rt[39];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[16], *paramsTmp = paramsConv; 
 for(int i=0; i < 16; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = 16+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 39 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,39);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5125: { // glGetIntegerv
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
 weglGetIntegerv(*pname,params);
 int AP = 0; ErlDrvTermData rt[39];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = 16+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 39 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,39);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5126: { // glGetLightfv
 GLenum *light = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetLightfv(*light,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5127: { // glGetLightiv
 GLenum *light = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetLightiv(*light,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5128: { // glGetMapdv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *query = (GLenum *) bp; bp += 4;
 GLdouble *v = (GLdouble *) bins[0]->base;
 weglGetMapdv(*target,*query,v);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5129: { // glGetMapfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *query = (GLenum *) bp; bp += 4;
 GLfloat *v = (GLfloat *) bins[0]->base;
 weglGetMapfv(*target,*query,v);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5130: { // glGetMapiv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *query = (GLenum *) bp; bp += 4;
 GLint *v = (GLint *) bins[0]->base;
 weglGetMapiv(*target,*query,v);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5131: { // glGetMaterialfv
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetMaterialfv(*face,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5132: { // glGetMaterialiv
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetMaterialiv(*face,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5133: { // glGetPixelMapfv
 GLenum *map = (GLenum *) bp; bp += 4;
 GLfloat *values = (GLfloat *) bins[0]->base;
 weglGetPixelMapfv(*map,values);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5134: { // glGetPixelMapuiv
 GLenum *map = (GLenum *) bp; bp += 4;
 GLuint *values = (GLuint *) bins[0]->base;
 weglGetPixelMapuiv(*map,values);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5135: { // glGetPixelMapusv
 GLenum *map = (GLenum *) bp; bp += 4;
 GLushort *values = (GLushort *) bins[0]->base;
 weglGetPixelMapusv(*map,values);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5136: { // glGetPolygonStipple
 GLubyte mask[128];
 weglGetPolygonStipple(mask);
 int AP = 0; ErlDrvTermData rt[8];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 ErlDrvBinary * BinCopy = driver_alloc_binary(128);
 memcpy(BinCopy->orig_bytes, mask, 128);
 rt[AP++] = ERL_DRV_BINARY; rt[AP++] = (ErlDrvTermData) BinCopy; rt[AP++] = 128; rt[AP++] = 0;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 8 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,8);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free_binary(BinCopy);
}; break;
case 5137: { // glGetString
 GLenum *name = (GLenum *) bp; bp += 4;
 const GLubyte *  result = weglGetString(*name);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) result; rt[AP++] = strlen((char *) result);
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5138: { // glGetTexEnvfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetTexEnvfv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5139: { // glGetTexEnviv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetTexEnviv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5140: { // glGetTexGendv
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLdouble params[4] = {0.0,0.0,0.0,0.0};
 weglGetTexGendv(*coord,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble *paramsTmp = params;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5141: { // glGetTexGenfv
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetTexGenfv(*coord,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5142: { // glGetTexGeniv
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetTexGeniv(*coord,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5143: { // glGetTexImage
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglGetTexImage(*target,*level,*format,*type,pixels);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5144: { // glGetTexLevelParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[1] = {0.0};
 weglGetTexLevelParameterfv(*target,*level,*pname,params);
 int AP = 0; ErlDrvTermData rt[8];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[1], *paramsTmp = paramsConv; 
 for(int i=0; i < 1; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 8 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,8);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5145: { // glGetTexLevelParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetTexLevelParameteriv(*target,*level,*pname,params);
 int AP = 0; ErlDrvTermData rt[8];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 8 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,8);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5146: { // glGetTexParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetTexParameterfv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5147: { // glGetTexParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetTexParameteriv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5148: { // glHint
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglHint(*target,*mode);
}; break;
case 5149: { // glIndexMask
 GLuint *mask = (GLuint *) bp; bp += 4;
 weglIndexMask(*mask);
}; break;
case 5150: { // glIndexPointer
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglIndexPointer(*type,*stride,pointer);
}; break;
case 5151: { // glIndexPointer
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglIndexPointer(*type,*stride,pointer);
}; break;
case 5152: { // glIndexdv
 GLdouble *c = (GLdouble *) bp; bp += 8;
 weglIndexdv(c);
}; break;
case 5153: { // glIndexfv
 GLfloat *c = (GLfloat *) bp; bp += 4;
 weglIndexfv(c);
}; break;
case 5154: { // glIndexiv
 GLint *c = (GLint *) bp; bp += 4;
 weglIndexiv(c);
}; break;
case 5155: { // glIndexsv
 GLshort *c = (GLshort *) bp; bp += 2;
 weglIndexsv(c);
}; break;
case 5156: { // glIndexubv
 GLubyte *c = (GLubyte *) bp; bp += 1;
 weglIndexubv(c);
}; break;
case 5157: { // glInitNames
 weglInitNames();
}; break;
case 5158: { // glInterleavedArrays
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglInterleavedArrays(*format,*stride,pointer);
}; break;
case 5159: { // glInterleavedArrays
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglInterleavedArrays(*format,*stride,pointer);
}; break;
case 5160: { // glIsEnabled
 GLenum *cap = (GLenum *) bp; bp += 4;
 GLboolean result = weglIsEnabled(*cap);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5161: { // glIsList
 GLuint *list = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsList(*list);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5162: { // glIsTexture
 GLuint *texture = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsTexture(*texture);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5163: { // glLightModelf
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglLightModelf(*pname,*param);
}; break;
case 5164: { // glLightModelfv
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+0)%2*4;
 weglLightModelfv(*pname,params);
}; break;
case 5165: { // glLightModeli
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglLightModeli(*pname,*param);
}; break;
case 5166: { // glLightModeliv
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+0)%2*4;
 weglLightModeliv(*pname,params);
}; break;
case 5167: { // glLightf
 GLenum *light = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglLightf(*light,*pname,*param);
}; break;
case 5168: { // glLightfv
 GLenum *light = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglLightfv(*light,*pname,params);
}; break;
case 5169: { // glLighti
 GLenum *light = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglLighti(*light,*pname,*param);
}; break;
case 5170: { // glLightiv
 GLenum *light = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglLightiv(*light,*pname,params);
}; break;
case 5171: { // glLineStipple
 GLint *factor = (GLint *) bp; bp += 4;
 GLushort *pattern = (GLushort *) bp; bp += 2;
 weglLineStipple(*factor,*pattern);
}; break;
case 5172: { // glLineWidth
 GLfloat *width = (GLfloat *) bp; bp += 4;
 weglLineWidth(*width);
}; break;
case 5173: { // glListBase
 GLuint *base = (GLuint *) bp; bp += 4;
 weglListBase(*base);
}; break;
case 5174: { // glLoadIdentity
 weglLoadIdentity();
}; break;
case 5175: { // glLoadMatrixd
 GLdouble * m = (GLdouble *) bp; bp += 128;
 weglLoadMatrixd(m);
}; break;
case 5176: { // glLoadMatrixf
 GLfloat * m = (GLfloat *) bp; bp += 64;
 weglLoadMatrixf(m);
}; break;
case 5177: { // glLoadName
 GLuint *name = (GLuint *) bp; bp += 4;
 weglLoadName(*name);
}; break;
case 5178: { // glLogicOp
 GLenum *opcode = (GLenum *) bp; bp += 4;
 weglLogicOp(*opcode);
}; break;
case 5179: { // glMap1d
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLdouble *u1 = (GLdouble *) bp; bp += 8;
 GLdouble *u2 = (GLdouble *) bp; bp += 8;
 GLint *stride = (GLint *) bp; bp += 4;
 GLint *order = (GLint *) bp; bp += 4;
 GLdouble *points = (GLdouble *) bins[0]->base;
 weglMap1d(*target,*u1,*u2,*stride,*order,points);
}; break;
case 5180: { // glMap1f
 GLenum *target = (GLenum *) bp; bp += 4;
 GLfloat *u1 = (GLfloat *) bp; bp += 4;
 GLfloat *u2 = (GLfloat *) bp; bp += 4;
 GLint *stride = (GLint *) bp; bp += 4;
 GLint *order = (GLint *) bp; bp += 4;
 GLfloat *points = (GLfloat *) bins[0]->base;
 weglMap1f(*target,*u1,*u2,*stride,*order,points);
}; break;
case 5181: { // glMap2d
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLdouble *u1 = (GLdouble *) bp; bp += 8;
 GLdouble *u2 = (GLdouble *) bp; bp += 8;
 GLint *ustride = (GLint *) bp; bp += 4;
 GLint *uorder = (GLint *) bp; bp += 4;
 GLdouble *v1 = (GLdouble *) bp; bp += 8;
 GLdouble *v2 = (GLdouble *) bp; bp += 8;
 GLint *vstride = (GLint *) bp; bp += 4;
 GLint *vorder = (GLint *) bp; bp += 4;
 GLdouble *points = (GLdouble *) bins[0]->base;
 weglMap2d(*target,*u1,*u2,*ustride,*uorder,*v1,*v2,*vstride,*vorder,points);
}; break;
case 5182: { // glMap2f
 GLenum *target = (GLenum *) bp; bp += 4;
 GLfloat *u1 = (GLfloat *) bp; bp += 4;
 GLfloat *u2 = (GLfloat *) bp; bp += 4;
 GLint *ustride = (GLint *) bp; bp += 4;
 GLint *uorder = (GLint *) bp; bp += 4;
 GLfloat *v1 = (GLfloat *) bp; bp += 4;
 GLfloat *v2 = (GLfloat *) bp; bp += 4;
 GLint *vstride = (GLint *) bp; bp += 4;
 GLint *vorder = (GLint *) bp; bp += 4;
 GLfloat *points = (GLfloat *) bins[0]->base;
 weglMap2f(*target,*u1,*u2,*ustride,*uorder,*v1,*v2,*vstride,*vorder,points);
}; break;
case 5183: { // glMapGrid1d
 GLint *un = (GLint *) bp; bp += 4;
 bp += 4;
 GLdouble *u1 = (GLdouble *) bp; bp += 8;
 GLdouble *u2 = (GLdouble *) bp; bp += 8;
 weglMapGrid1d(*un,*u1,*u2);
}; break;
case 5184: { // glMapGrid1f
 GLint *un = (GLint *) bp; bp += 4;
 GLfloat *u1 = (GLfloat *) bp; bp += 4;
 GLfloat *u2 = (GLfloat *) bp; bp += 4;
 weglMapGrid1f(*un,*u1,*u2);
}; break;
case 5185: { // glMapGrid2d
 GLint *un = (GLint *) bp; bp += 4;
 bp += 4;
 GLdouble *u1 = (GLdouble *) bp; bp += 8;
 GLdouble *u2 = (GLdouble *) bp; bp += 8;
 GLint *vn = (GLint *) bp; bp += 4;
 bp += 4;
 GLdouble *v1 = (GLdouble *) bp; bp += 8;
 GLdouble *v2 = (GLdouble *) bp; bp += 8;
 weglMapGrid2d(*un,*u1,*u2,*vn,*v1,*v2);
}; break;
case 5186: { // glMapGrid2f
 GLint *un = (GLint *) bp; bp += 4;
 GLfloat *u1 = (GLfloat *) bp; bp += 4;
 GLfloat *u2 = (GLfloat *) bp; bp += 4;
 GLint *vn = (GLint *) bp; bp += 4;
 GLfloat *v1 = (GLfloat *) bp; bp += 4;
 GLfloat *v2 = (GLfloat *) bp; bp += 4;
 weglMapGrid2f(*un,*u1,*u2,*vn,*v1,*v2);
}; break;
case 5187: { // glMaterialf
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglMaterialf(*face,*pname,*param);
}; break;
case 5188: { // glMaterialfv
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglMaterialfv(*face,*pname,params);
}; break;
case 5189: { // glMateriali
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglMateriali(*face,*pname,*param);
}; break;
case 5190: { // glMaterialiv
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglMaterialiv(*face,*pname,params);
}; break;
case 5191: { // glMatrixMode
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglMatrixMode(*mode);
}; break;
case 5192: { // glMultMatrixd
 GLdouble * m = (GLdouble *) bp; bp += 128;
 weglMultMatrixd(m);
}; break;
case 5193: { // glMultMatrixf
 GLfloat * m = (GLfloat *) bp; bp += 64;
 weglMultMatrixf(m);
}; break;
case 5194: { // glNewList
 GLuint *list = (GLuint *) bp; bp += 4;
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglNewList(*list,*mode);
}; break;
case 5195: { // glNormal3bv
 GLbyte *v = (GLbyte *) bp; bp += 1;
 weglNormal3bv(v);
}; break;
case 5196: { // glNormal3dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglNormal3dv(v);
}; break;
case 5197: { // glNormal3fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglNormal3fv(v);
}; break;
case 5198: { // glNormal3iv
 GLint *v = (GLint *) bp; bp += 4;
 weglNormal3iv(v);
}; break;
case 5199: { // glNormal3sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglNormal3sv(v);
}; break;
case 5200: { // glNormalPointer
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglNormalPointer(*type,*stride,pointer);
}; break;
case 5201: { // glNormalPointer
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglNormalPointer(*type,*stride,pointer);
}; break;
case 5202: { // glOrtho
 GLdouble *left = (GLdouble *) bp; bp += 8;
 GLdouble *right = (GLdouble *) bp; bp += 8;
 GLdouble *bottom = (GLdouble *) bp; bp += 8;
 GLdouble *top = (GLdouble *) bp; bp += 8;
 GLdouble *zNear = (GLdouble *) bp; bp += 8;
 GLdouble *zFar = (GLdouble *) bp; bp += 8;
 weglOrtho(*left,*right,*bottom,*top,*zNear,*zFar);
}; break;
case 5203: { // glPassThrough
 GLfloat *token = (GLfloat *) bp; bp += 4;
 weglPassThrough(*token);
}; break;
case 5204: { // glPixelMapfv
 GLenum *map = (GLenum *) bp; bp += 4;
 GLsizei *mapsize = (GLsizei *) bp; bp += 4;
 GLfloat *values = (GLfloat *) bins[0]->base;
 weglPixelMapfv(*map,*mapsize,values);
}; break;
case 5205: { // glPixelMapuiv
 GLenum *map = (GLenum *) bp; bp += 4;
 GLsizei *mapsize = (GLsizei *) bp; bp += 4;
 GLuint *values = (GLuint *) bins[0]->base;
 weglPixelMapuiv(*map,*mapsize,values);
}; break;
case 5206: { // glPixelMapusv
 GLenum *map = (GLenum *) bp; bp += 4;
 GLsizei *mapsize = (GLsizei *) bp; bp += 4;
 GLushort *values = (GLushort *) bins[0]->base;
 weglPixelMapusv(*map,*mapsize,values);
}; break;
case 5207: { // glPixelStoref
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglPixelStoref(*pname,*param);
}; break;
case 5208: { // glPixelStorei
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglPixelStorei(*pname,*param);
}; break;
case 5209: { // glPixelTransferf
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglPixelTransferf(*pname,*param);
}; break;
case 5210: { // glPixelTransferi
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglPixelTransferi(*pname,*param);
}; break;
case 5211: { // glPixelZoom
 GLfloat *xfactor = (GLfloat *) bp; bp += 4;
 GLfloat *yfactor = (GLfloat *) bp; bp += 4;
 weglPixelZoom(*xfactor,*yfactor);
}; break;
case 5212: { // glPointSize
 GLfloat *size = (GLfloat *) bp; bp += 4;
 weglPointSize(*size);
}; break;
case 5213: { // glPolygonMode
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglPolygonMode(*face,*mode);
}; break;
case 5214: { // glPolygonOffset
 GLfloat *factor = (GLfloat *) bp; bp += 4;
 GLfloat *units = (GLfloat *) bp; bp += 4;
 weglPolygonOffset(*factor,*units);
}; break;
case 5215: { // glPolygonStipple
 GLubyte *mask = (GLubyte *) bins[0]->base;
 weglPolygonStipple(mask);
}; break;
case 5216: { // glPopAttrib
 weglPopAttrib();
}; break;
case 5217: { // glPopClientAttrib
 weglPopClientAttrib();
}; break;
case 5218: { // glPopMatrix
 weglPopMatrix();
}; break;
case 5219: { // glPopName
 weglPopName();
}; break;
case 5220: { // glPrioritizeTextures
 int * texturesLen = (int *) bp; bp += 4;
 GLuint * textures = (GLuint *) bp;  bp += (8-((*texturesLen*4+4)%8))%8;
 int * prioritiesLen = (int *) bp; bp += 4;
 GLclampf * priorities = (GLclampf *) bp;  bp += (8-((*prioritiesLen*4+4)%8))%8;
 weglPrioritizeTextures(*texturesLen,textures,priorities);
}; break;
case 5221: { // glPushAttrib
 GLbitfield *mask = (GLbitfield *) bp; bp += 4;
 weglPushAttrib(*mask);
}; break;
case 5222: { // glPushClientAttrib
 GLbitfield *mask = (GLbitfield *) bp; bp += 4;
 weglPushClientAttrib(*mask);
}; break;
case 5223: { // glPushMatrix
 weglPushMatrix();
}; break;
case 5224: { // glPushName
 GLuint *name = (GLuint *) bp; bp += 4;
 weglPushName(*name);
}; break;
case 5225: { // glRasterPos2dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglRasterPos2dv(v);
}; break;
case 5226: { // glRasterPos2fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglRasterPos2fv(v);
}; break;
case 5227: { // glRasterPos2iv
 GLint *v = (GLint *) bp; bp += 4;
 weglRasterPos2iv(v);
}; break;
case 5228: { // glRasterPos2sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglRasterPos2sv(v);
}; break;
case 5229: { // glRasterPos3dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglRasterPos3dv(v);
}; break;
case 5230: { // glRasterPos3fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglRasterPos3fv(v);
}; break;
case 5231: { // glRasterPos3iv
 GLint *v = (GLint *) bp; bp += 4;
 weglRasterPos3iv(v);
}; break;
case 5232: { // glRasterPos3sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglRasterPos3sv(v);
}; break;
case 5233: { // glRasterPos4dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglRasterPos4dv(v);
}; break;
case 5234: { // glRasterPos4fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglRasterPos4fv(v);
}; break;
case 5235: { // glRasterPos4iv
 GLint *v = (GLint *) bp; bp += 4;
 weglRasterPos4iv(v);
}; break;
case 5236: { // glRasterPos4sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglRasterPos4sv(v);
}; break;
case 5237: { // glReadBuffer
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglReadBuffer(*mode);
}; break;
case 5238: { // glReadPixels
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglReadPixels(*x,*y,*width,*height,*format,*type,pixels);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5239: { // glRectd
 GLdouble *x1 = (GLdouble *) bp; bp += 8;
 GLdouble *y1 = (GLdouble *) bp; bp += 8;
 GLdouble *x2 = (GLdouble *) bp; bp += 8;
 GLdouble *y2 = (GLdouble *) bp; bp += 8;
 weglRectd(*x1,*y1,*x2,*y2);
}; break;
case 5240: { // glRectdv
 GLdouble * v1 = (GLdouble *) bp; bp += 16;
 GLdouble * v2 = (GLdouble *) bp; bp += 16;
 weglRectdv(v1,v2);
}; break;
case 5241: { // glRectf
 GLfloat *x1 = (GLfloat *) bp; bp += 4;
 GLfloat *y1 = (GLfloat *) bp; bp += 4;
 GLfloat *x2 = (GLfloat *) bp; bp += 4;
 GLfloat *y2 = (GLfloat *) bp; bp += 4;
 weglRectf(*x1,*y1,*x2,*y2);
}; break;
case 5242: { // glRectfv
 GLfloat * v1 = (GLfloat *) bp; bp += 8;
 GLfloat * v2 = (GLfloat *) bp; bp += 8;
 weglRectfv(v1,v2);
}; break;
case 5243: { // glRecti
 GLint *x1 = (GLint *) bp; bp += 4;
 GLint *y1 = (GLint *) bp; bp += 4;
 GLint *x2 = (GLint *) bp; bp += 4;
 GLint *y2 = (GLint *) bp; bp += 4;
 weglRecti(*x1,*y1,*x2,*y2);
}; break;
case 5244: { // glRectiv
 GLint * v1 = (GLint *) bp; bp += 8;
 GLint * v2 = (GLint *) bp; bp += 8;
 weglRectiv(v1,v2);
}; break;
case 5245: { // glRects
 GLshort *x1 = (GLshort *) bp; bp += 2;
 GLshort *y1 = (GLshort *) bp; bp += 2;
 GLshort *x2 = (GLshort *) bp; bp += 2;
 GLshort *y2 = (GLshort *) bp; bp += 2;
 weglRects(*x1,*y1,*x2,*y2);
}; break;
case 5246: { // glRectsv
 GLshort * v1 = (GLshort *) bp; bp += 4;
 GLshort * v2 = (GLshort *) bp; bp += 4;
 weglRectsv(v1,v2);
}; break;
case 5247: { // glRenderMode
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLint result = weglRenderMode(*mode);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5248: { // glRotated
 GLdouble *angle = (GLdouble *) bp; bp += 8;
 GLdouble *x = (GLdouble *) bp; bp += 8;
 GLdouble *y = (GLdouble *) bp; bp += 8;
 GLdouble *z = (GLdouble *) bp; bp += 8;
 weglRotated(*angle,*x,*y,*z);
}; break;
case 5249: { // glRotatef
 GLfloat *angle = (GLfloat *) bp; bp += 4;
 GLfloat *x = (GLfloat *) bp; bp += 4;
 GLfloat *y = (GLfloat *) bp; bp += 4;
 GLfloat *z = (GLfloat *) bp; bp += 4;
 weglRotatef(*angle,*x,*y,*z);
}; break;
case 5250: { // glScaled
 GLdouble *x = (GLdouble *) bp; bp += 8;
 GLdouble *y = (GLdouble *) bp; bp += 8;
 GLdouble *z = (GLdouble *) bp; bp += 8;
 weglScaled(*x,*y,*z);
}; break;
case 5251: { // glScalef
 GLfloat *x = (GLfloat *) bp; bp += 4;
 GLfloat *y = (GLfloat *) bp; bp += 4;
 GLfloat *z = (GLfloat *) bp; bp += 4;
 weglScalef(*x,*y,*z);
}; break;
case 5252: { // glScissor
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 weglScissor(*x,*y,*width,*height);
}; break;
case 5253: { // glSelectBuffer
 GLsizei *size = (GLsizei *) bp; bp += 4;
 GLuint *buffer = (GLuint *) bins[0]->base;
 weglSelectBuffer(*size,buffer);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5254: { // glShadeModel
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglShadeModel(*mode);
}; break;
case 5255: { // glStencilFunc
 GLenum *func = (GLenum *) bp; bp += 4;
 GLint *ref = (GLint *) bp; bp += 4;
 GLuint *mask = (GLuint *) bp; bp += 4;
 weglStencilFunc(*func,*ref,*mask);
}; break;
case 5256: { // glStencilMask
 GLuint *mask = (GLuint *) bp; bp += 4;
 weglStencilMask(*mask);
}; break;
case 5257: { // glStencilOp
 GLenum *fail = (GLenum *) bp; bp += 4;
 GLenum *zfail = (GLenum *) bp; bp += 4;
 GLenum *zpass = (GLenum *) bp; bp += 4;
 weglStencilOp(*fail,*zfail,*zpass);
}; break;
case 5258: { // glTexCoord1dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglTexCoord1dv(v);
}; break;
case 5259: { // glTexCoord1fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglTexCoord1fv(v);
}; break;
case 5260: { // glTexCoord1iv
 GLint *v = (GLint *) bp; bp += 4;
 weglTexCoord1iv(v);
}; break;
case 5261: { // glTexCoord1sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglTexCoord1sv(v);
}; break;
case 5262: { // glTexCoord2dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglTexCoord2dv(v);
}; break;
case 5263: { // glTexCoord2fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglTexCoord2fv(v);
}; break;
case 5264: { // glTexCoord2iv
 GLint *v = (GLint *) bp; bp += 4;
 weglTexCoord2iv(v);
}; break;
case 5265: { // glTexCoord2sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglTexCoord2sv(v);
}; break;
case 5266: { // glTexCoord3dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglTexCoord3dv(v);
}; break;
case 5267: { // glTexCoord3fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglTexCoord3fv(v);
}; break;
case 5268: { // glTexCoord3iv
 GLint *v = (GLint *) bp; bp += 4;
 weglTexCoord3iv(v);
}; break;
case 5269: { // glTexCoord3sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglTexCoord3sv(v);
}; break;
case 5270: { // glTexCoord4dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglTexCoord4dv(v);
}; break;
case 5271: { // glTexCoord4fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglTexCoord4fv(v);
}; break;
case 5272: { // glTexCoord4iv
 GLint *v = (GLint *) bp; bp += 4;
 weglTexCoord4iv(v);
}; break;
case 5273: { // glTexCoord4sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglTexCoord4sv(v);
}; break;
case 5274: { // glTexCoordPointer
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglTexCoordPointer(*size,*type,*stride,pointer);
}; break;
case 5275: { // glTexCoordPointer
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglTexCoordPointer(*size,*type,*stride,pointer);
}; break;
case 5276: { // glTexEnvf
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglTexEnvf(*target,*pname,*param);
}; break;
case 5277: { // glTexEnvfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglTexEnvfv(*target,*pname,params);
}; break;
case 5278: { // glTexEnvi
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglTexEnvi(*target,*pname,*param);
}; break;
case 5279: { // glTexEnviv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglTexEnviv(*target,*pname,params);
}; break;
case 5280: { // glTexGend
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLdouble *param = (GLdouble *) bp; bp += 8;
 weglTexGend(*coord,*pname,*param);
}; break;
case 5281: { // glTexGendv
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 8;
 GLdouble *params = (GLdouble *) bp; bp += *paramsLen*8;
 weglTexGendv(*coord,*pname,params);
}; break;
case 5282: { // glTexGenf
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglTexGenf(*coord,*pname,*param);
}; break;
case 5283: { // glTexGenfv
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglTexGenfv(*coord,*pname,params);
}; break;
case 5284: { // glTexGeni
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglTexGeni(*coord,*pname,*param);
}; break;
case 5285: { // glTexGeniv
 GLenum *coord = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglTexGeniv(*coord,*pname,params);
}; break;
case 5286: { // glTexImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *internalformat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) * (int *) bp; bp += 4;
 weglTexImage1D(*target,*level,*internalformat,*width,*border,*format,*type,pixels);
}; break;
case 5287: { // glTexImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *internalformat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglTexImage1D(*target,*level,*internalformat,*width,*border,*format,*type,pixels);
}; break;
case 5288: { // glTexImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *internalformat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) * (int *) bp; bp += 4;
 weglTexImage2D(*target,*level,*internalformat,*width,*height,*border,*format,*type,pixels);
}; break;
case 5289: { // glTexImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *internalformat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglTexImage2D(*target,*level,*internalformat,*width,*height,*border,*format,*type,pixels);
}; break;
case 5290: { // glTexParameterf
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglTexParameterf(*target,*pname,*param);
}; break;
case 5291: { // glTexParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglTexParameterfv(*target,*pname,params);
}; break;
case 5292: { // glTexParameteri
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglTexParameteri(*target,*pname,*param);
}; break;
case 5293: { // glTexParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglTexParameteriv(*target,*pname,params);
}; break;
case 5294: { // glTexSubImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) * (int *) bp; bp += 4;
 weglTexSubImage1D(*target,*level,*xoffset,*width,*format,*type,pixels);
}; break;
case 5295: { // glTexSubImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglTexSubImage1D(*target,*level,*xoffset,*width,*format,*type,pixels);
}; break;
case 5296: { // glTexSubImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) * (int *) bp; bp += 4;
 weglTexSubImage2D(*target,*level,*xoffset,*yoffset,*width,*height,*format,*type,pixels);
}; break;
case 5297: { // glTexSubImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglTexSubImage2D(*target,*level,*xoffset,*yoffset,*width,*height,*format,*type,pixels);
}; break;
case 5298: { // glTranslated
 GLdouble *x = (GLdouble *) bp; bp += 8;
 GLdouble *y = (GLdouble *) bp; bp += 8;
 GLdouble *z = (GLdouble *) bp; bp += 8;
 weglTranslated(*x,*y,*z);
}; break;
case 5299: { // glTranslatef
 GLfloat *x = (GLfloat *) bp; bp += 4;
 GLfloat *y = (GLfloat *) bp; bp += 4;
 GLfloat *z = (GLfloat *) bp; bp += 4;
 weglTranslatef(*x,*y,*z);
}; break;
case 5300: { // glVertex2dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglVertex2dv(v);
}; break;
case 5301: { // glVertex2fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglVertex2fv(v);
}; break;
case 5302: { // glVertex2iv
 GLint *v = (GLint *) bp; bp += 4;
 weglVertex2iv(v);
}; break;
case 5303: { // glVertex2sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglVertex2sv(v);
}; break;
case 5304: { // glVertex3dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglVertex3dv(v);
}; break;
case 5305: { // glVertex3fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglVertex3fv(v);
}; break;
case 5306: { // glVertex3iv
 GLint *v = (GLint *) bp; bp += 4;
 weglVertex3iv(v);
}; break;
case 5307: { // glVertex3sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglVertex3sv(v);
}; break;
case 5308: { // glVertex4dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglVertex4dv(v);
}; break;
case 5309: { // glVertex4fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglVertex4fv(v);
}; break;
case 5310: { // glVertex4iv
 GLint *v = (GLint *) bp; bp += 4;
 weglVertex4iv(v);
}; break;
case 5311: { // glVertex4sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglVertex4sv(v);
}; break;
case 5312: { // glVertexPointer
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglVertexPointer(*size,*type,*stride,pointer);
}; break;
case 5313: { // glVertexPointer
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglVertexPointer(*size,*type,*stride,pointer);
}; break;
case 5314: { // glViewport
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 weglViewport(*x,*y,*width,*height);
}; break;
case 5315: { // glBlendColor
 GLclampf *red = (GLclampf *) bp; bp += 4;
 GLclampf *green = (GLclampf *) bp; bp += 4;
 GLclampf *blue = (GLclampf *) bp; bp += 4;
 GLclampf *alpha = (GLclampf *) bp; bp += 4;
 weglBlendColor(*red,*green,*blue,*alpha);
}; break;
case 5316: { // glBlendEquation
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglBlendEquation(*mode);
}; break;
case 5317: { // glDrawRangeElements
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLuint *start = (GLuint *) bp; bp += 4;
 GLuint *end = (GLuint *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *indices = (GLvoid *) * (int *) bp; bp += 4;
 weglDrawRangeElements(*mode,*start,*end,*count,*type,indices);
}; break;
case 5318: { // glDrawRangeElements
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLuint *start = (GLuint *) bp; bp += 4;
 GLuint *end = (GLuint *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *indices = (GLvoid *) bins[0]->base;
 weglDrawRangeElements(*mode,*start,*end,*count,*type,indices);
}; break;
case 5319: { // glTexImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *internalformat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) * (int *) bp; bp += 4;
 weglTexImage3D(*target,*level,*internalformat,*width,*height,*depth,*border,*format,*type,pixels);
}; break;
case 5320: { // glTexImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *internalformat = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglTexImage3D(*target,*level,*internalformat,*width,*height,*depth,*border,*format,*type,pixels);
}; break;
case 5321: { // glTexSubImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLint *zoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) * (int *) bp; bp += 4;
 weglTexSubImage3D(*target,*level,*xoffset,*yoffset,*zoffset,*width,*height,*depth,*format,*type,pixels);
}; break;
case 5322: { // glTexSubImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLint *zoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *pixels = (GLvoid *) bins[0]->base;
 weglTexSubImage3D(*target,*level,*xoffset,*yoffset,*zoffset,*width,*height,*depth,*format,*type,pixels);
}; break;
case 5323: { // glCopyTexSubImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLint *zoffset = (GLint *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 weglCopyTexSubImage3D(*target,*level,*xoffset,*yoffset,*zoffset,*x,*y,*width,*height);
}; break;
case 5324: { // glColorTable
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *table = (GLvoid *) * (int *) bp; bp += 4;
 weglColorTable(*target,*internalformat,*width,*format,*type,table);
}; break;
case 5325: { // glColorTable
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *table = (GLvoid *) bins[0]->base;
 weglColorTable(*target,*internalformat,*width,*format,*type,table);
}; break;
case 5326: { // glColorTableParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat * params = (GLfloat *) bp; bp += 16;
 weglColorTableParameterfv(*target,*pname,params);
}; break;
case 5327: { // glColorTableParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint * params = (GLint *) bp; bp += 16;
 weglColorTableParameteriv(*target,*pname,params);
}; break;
case 5328: { // glCopyColorTable
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 weglCopyColorTable(*target,*internalformat,*x,*y,*width);
}; break;
case 5329: { // glGetColorTable
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *table = (GLvoid *) bins[0]->base;
 weglGetColorTable(*target,*format,*type,table);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5330: { // glGetColorTableParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetColorTableParameterfv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5331: { // glGetColorTableParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetColorTableParameteriv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5332: { // glColorSubTable
 GLenum *target = (GLenum *) bp; bp += 4;
 GLsizei *start = (GLsizei *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 weglColorSubTable(*target,*start,*count,*format,*type,data);
}; break;
case 5333: { // glColorSubTable
 GLenum *target = (GLenum *) bp; bp += 4;
 GLsizei *start = (GLsizei *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglColorSubTable(*target,*start,*count,*format,*type,data);
}; break;
case 5334: { // glCopyColorSubTable
 GLenum *target = (GLenum *) bp; bp += 4;
 GLsizei *start = (GLsizei *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 weglCopyColorSubTable(*target,*start,*x,*y,*width);
}; break;
case 5335: { // glConvolutionFilter1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *image = (GLvoid *) * (int *) bp; bp += 4;
 weglConvolutionFilter1D(*target,*internalformat,*width,*format,*type,image);
}; break;
case 5336: { // glConvolutionFilter1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *image = (GLvoid *) bins[0]->base;
 weglConvolutionFilter1D(*target,*internalformat,*width,*format,*type,image);
}; break;
case 5337: { // glConvolutionFilter2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *image = (GLvoid *) * (int *) bp; bp += 4;
 weglConvolutionFilter2D(*target,*internalformat,*width,*height,*format,*type,image);
}; break;
case 5338: { // glConvolutionFilter2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *image = (GLvoid *) bins[0]->base;
 weglConvolutionFilter2D(*target,*internalformat,*width,*height,*format,*type,image);
}; break;
case 5339: { // glConvolutionParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglConvolutionParameterfv(*target,*pname,params);
}; break;
case 5340: { // glConvolutionParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglConvolutionParameteriv(*target,*pname,params);
}; break;
case 5341: { // glCopyConvolutionFilter1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 weglCopyConvolutionFilter1D(*target,*internalformat,*x,*y,*width);
}; break;
case 5342: { // glCopyConvolutionFilter2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLint *x = (GLint *) bp; bp += 4;
 GLint *y = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 weglCopyConvolutionFilter2D(*target,*internalformat,*x,*y,*width,*height);
}; break;
case 5343: { // glGetConvolutionFilter
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *image = (GLvoid *) bins[0]->base;
 weglGetConvolutionFilter(*target,*format,*type,image);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5344: { // glGetConvolutionParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetConvolutionParameterfv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5345: { // glGetConvolutionParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetConvolutionParameteriv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5346: { // glSeparableFilter2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *row = (GLvoid *) * (int *) bp; bp += 4;
 GLvoid *column = (GLvoid *) * (int *) bp; bp += 4;
 weglSeparableFilter2D(*target,*internalformat,*width,*height,*format,*type,row,column);
}; break;
case 5347: { // glSeparableFilter2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *row = (GLvoid *) bins[0]->base;
 GLvoid *column = (GLvoid *) bins[1]->base;
 weglSeparableFilter2D(*target,*internalformat,*width,*height,*format,*type,row,column);
}; break;
case 5348: { // glGetHistogram
 GLenum *target = (GLenum *) bp; bp += 4;
 GLboolean *reset = (GLboolean *) bp; bp += 1;
 bp += 3;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *values = (GLvoid *) bins[0]->base;
 weglGetHistogram(*target,*reset,*format,*type,values);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5349: { // glGetHistogramParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[1] = {0.0};
 weglGetHistogramParameterfv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[8];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[1], *paramsTmp = paramsConv; 
 for(int i=0; i < 1; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 8 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,8);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5350: { // glGetHistogramParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetHistogramParameteriv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[8];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 8 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,8);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5351: { // glGetMinmax
 GLenum *target = (GLenum *) bp; bp += 4;
 GLboolean *reset = (GLboolean *) bp; bp += 1;
 bp += 3;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *values = (GLvoid *) bins[0]->base;
 weglGetMinmax(*target,*reset,*format,*type,values);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5352: { // glGetMinmaxParameterfv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[1] = {0.0};
 weglGetMinmaxParameterfv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[8];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[1], *paramsTmp = paramsConv; 
 for(int i=0; i < 1; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 8 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,8);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5353: { // glGetMinmaxParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetMinmaxParameteriv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[8];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 8 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,8);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5354: { // glHistogram
 GLenum *target = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLboolean *sink = (GLboolean *) bp; bp += 1;
 weglHistogram(*target,*width,*internalformat,*sink);
}; break;
case 5355: { // glMinmax
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLboolean *sink = (GLboolean *) bp; bp += 1;
 weglMinmax(*target,*internalformat,*sink);
}; break;
case 5356: { // glResetHistogram
 GLenum *target = (GLenum *) bp; bp += 4;
 weglResetHistogram(*target);
}; break;
case 5357: { // glResetMinmax
 GLenum *target = (GLenum *) bp; bp += 4;
 weglResetMinmax(*target);
}; break;
case 5358: { // glActiveTexture
 GLenum *texture = (GLenum *) bp; bp += 4;
 weglActiveTexture(*texture);
}; break;
case 5359: { // glSampleCoverage
 GLclampf *value = (GLclampf *) bp; bp += 4;
 GLboolean *invert = (GLboolean *) bp; bp += 1;
 weglSampleCoverage(*value,*invert);
}; break;
case 5360: { // glCompressedTexImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 weglCompressedTexImage3D(*target,*level,*internalformat,*width,*height,*depth,*border,*imageSize,data);
}; break;
case 5361: { // glCompressedTexImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglCompressedTexImage3D(*target,*level,*internalformat,*width,*height,*depth,*border,*imageSize,data);
}; break;
case 5362: { // glCompressedTexImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 weglCompressedTexImage2D(*target,*level,*internalformat,*width,*height,*border,*imageSize,data);
}; break;
case 5363: { // glCompressedTexImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglCompressedTexImage2D(*target,*level,*internalformat,*width,*height,*border,*imageSize,data);
}; break;
case 5364: { // glCompressedTexImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 weglCompressedTexImage1D(*target,*level,*internalformat,*width,*border,*imageSize,data);
}; break;
case 5365: { // glCompressedTexImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLint *border = (GLint *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglCompressedTexImage1D(*target,*level,*internalformat,*width,*border,*imageSize,data);
}; break;
case 5366: { // glCompressedTexSubImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLint *zoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 weglCompressedTexSubImage3D(*target,*level,*xoffset,*yoffset,*zoffset,*width,*height,*depth,*format,*imageSize,data);
}; break;
case 5367: { // glCompressedTexSubImage3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLint *zoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLsizei *depth = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglCompressedTexSubImage3D(*target,*level,*xoffset,*yoffset,*zoffset,*width,*height,*depth,*format,*imageSize,data);
}; break;
case 5368: { // glCompressedTexSubImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 weglCompressedTexSubImage2D(*target,*level,*xoffset,*yoffset,*width,*height,*format,*imageSize,data);
}; break;
case 5369: { // glCompressedTexSubImage2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLint *yoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglCompressedTexSubImage2D(*target,*level,*xoffset,*yoffset,*width,*height,*format,*imageSize,data);
}; break;
case 5370: { // glCompressedTexSubImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 weglCompressedTexSubImage1D(*target,*level,*xoffset,*width,*format,*imageSize,data);
}; break;
case 5371: { // glCompressedTexSubImage1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *xoffset = (GLint *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLsizei *imageSize = (GLsizei *) bp; bp += 4;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglCompressedTexSubImage1D(*target,*level,*xoffset,*width,*format,*imageSize,data);
}; break;
case 5372: { // glGetCompressedTexImage
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLvoid *img = (GLvoid *) bins[0]->base;
 weglGetCompressedTexImage(*target,*level,img);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5373: { // glClientActiveTexture
 GLenum *texture = (GLenum *) bp; bp += 4;
 weglClientActiveTexture(*texture);
}; break;
case 5374: { // glMultiTexCoord1dv
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglMultiTexCoord1dv(*target,v);
}; break;
case 5375: { // glMultiTexCoord1fv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglMultiTexCoord1fv(*target,v);
}; break;
case 5376: { // glMultiTexCoord1iv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *v = (GLint *) bp; bp += 4;
 weglMultiTexCoord1iv(*target,v);
}; break;
case 5377: { // glMultiTexCoord1sv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLshort *v = (GLshort *) bp; bp += 2;
 weglMultiTexCoord1sv(*target,v);
}; break;
case 5378: { // glMultiTexCoord2dv
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglMultiTexCoord2dv(*target,v);
}; break;
case 5379: { // glMultiTexCoord2fv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglMultiTexCoord2fv(*target,v);
}; break;
case 5380: { // glMultiTexCoord2iv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *v = (GLint *) bp; bp += 4;
 weglMultiTexCoord2iv(*target,v);
}; break;
case 5381: { // glMultiTexCoord2sv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLshort *v = (GLshort *) bp; bp += 2;
 weglMultiTexCoord2sv(*target,v);
}; break;
case 5382: { // glMultiTexCoord3dv
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglMultiTexCoord3dv(*target,v);
}; break;
case 5383: { // glMultiTexCoord3fv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglMultiTexCoord3fv(*target,v);
}; break;
case 5384: { // glMultiTexCoord3iv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *v = (GLint *) bp; bp += 4;
 weglMultiTexCoord3iv(*target,v);
}; break;
case 5385: { // glMultiTexCoord3sv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLshort *v = (GLshort *) bp; bp += 2;
 weglMultiTexCoord3sv(*target,v);
}; break;
case 5386: { // glMultiTexCoord4dv
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglMultiTexCoord4dv(*target,v);
}; break;
case 5387: { // glMultiTexCoord4fv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglMultiTexCoord4fv(*target,v);
}; break;
case 5388: { // glMultiTexCoord4iv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLint *v = (GLint *) bp; bp += 4;
 weglMultiTexCoord4iv(*target,v);
}; break;
case 5389: { // glMultiTexCoord4sv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLshort *v = (GLshort *) bp; bp += 2;
 weglMultiTexCoord4sv(*target,v);
}; break;
case 5390: { // glLoadTransposeMatrixf
 GLfloat * m = (GLfloat *) bp; bp += 64;
 weglLoadTransposeMatrixf(m);
}; break;
case 5391: { // glLoadTransposeMatrixd
 GLdouble * m = (GLdouble *) bp; bp += 128;
 weglLoadTransposeMatrixd(m);
}; break;
case 5392: { // glMultTransposeMatrixf
 GLfloat * m = (GLfloat *) bp; bp += 64;
 weglMultTransposeMatrixf(m);
}; break;
case 5393: { // glMultTransposeMatrixd
 GLdouble * m = (GLdouble *) bp; bp += 128;
 weglMultTransposeMatrixd(m);
}; break;
case 5394: { // glBlendFuncSeparate
 GLenum *sfactorRGB = (GLenum *) bp; bp += 4;
 GLenum *dfactorRGB = (GLenum *) bp; bp += 4;
 GLenum *sfactorAlpha = (GLenum *) bp; bp += 4;
 GLenum *dfactorAlpha = (GLenum *) bp; bp += 4;
 weglBlendFuncSeparate(*sfactorRGB,*dfactorRGB,*sfactorAlpha,*dfactorAlpha);
}; break;
case 5395: { // glMultiDrawArrays
 GLenum *mode = (GLenum *) bp; bp += 4;
 int * firstLen = (int *) bp; bp += 4;
 GLint * first = (GLint *) bp;  bp += (8-((*firstLen*4+0)%8))%8;
 int * countLen = (int *) bp; bp += 4;
 GLsizei * count = (GLsizei *) bp;  bp += (8-((*countLen*4+4)%8))%8;
 weglMultiDrawArrays(*mode,first,count,*firstLen);
}; break;
case 5396: { // glPointParameterf
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat *param = (GLfloat *) bp; bp += 4;
 weglPointParameterf(*pname,*param);
}; break;
case 5397: { // glPointParameterfv
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLfloat *params = (GLfloat *) bp; bp += *paramsLen*4+((*paramsLen)+0)%2*4;
 weglPointParameterfv(*pname,params);
}; break;
case 5398: { // glPointParameteri
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *param = (GLint *) bp; bp += 4;
 weglPointParameteri(*pname,*param);
}; break;
case 5399: { // glPointParameteriv
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+0)%2*4;
 weglPointParameteriv(*pname,params);
}; break;
case 5400: { // glFogCoordfv
 GLfloat *coord = (GLfloat *) bp; bp += 4;
 weglFogCoordfv(coord);
}; break;
case 5401: { // glFogCoorddv
 GLdouble *coord = (GLdouble *) bp; bp += 8;
 weglFogCoorddv(coord);
}; break;
case 5402: { // glFogCoordPointer
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglFogCoordPointer(*type,*stride,pointer);
}; break;
case 5403: { // glFogCoordPointer
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglFogCoordPointer(*type,*stride,pointer);
}; break;
case 5404: { // glSecondaryColor3bv
 GLbyte *v = (GLbyte *) bp; bp += 1;
 weglSecondaryColor3bv(v);
}; break;
case 5405: { // glSecondaryColor3dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglSecondaryColor3dv(v);
}; break;
case 5406: { // glSecondaryColor3fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglSecondaryColor3fv(v);
}; break;
case 5407: { // glSecondaryColor3iv
 GLint *v = (GLint *) bp; bp += 4;
 weglSecondaryColor3iv(v);
}; break;
case 5408: { // glSecondaryColor3sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglSecondaryColor3sv(v);
}; break;
case 5409: { // glSecondaryColor3ubv
 GLubyte *v = (GLubyte *) bp; bp += 1;
 weglSecondaryColor3ubv(v);
}; break;
case 5410: { // glSecondaryColor3uiv
 GLuint *v = (GLuint *) bp; bp += 4;
 weglSecondaryColor3uiv(v);
}; break;
case 5411: { // glSecondaryColor3usv
 GLushort *v = (GLushort *) bp; bp += 2;
 weglSecondaryColor3usv(v);
}; break;
case 5412: { // glSecondaryColorPointer
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglSecondaryColorPointer(*size,*type,*stride,pointer);
}; break;
case 5413: { // glSecondaryColorPointer
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglSecondaryColorPointer(*size,*type,*stride,pointer);
}; break;
case 5414: { // glWindowPos2dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglWindowPos2dv(v);
}; break;
case 5415: { // glWindowPos2fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglWindowPos2fv(v);
}; break;
case 5416: { // glWindowPos2iv
 GLint *v = (GLint *) bp; bp += 4;
 weglWindowPos2iv(v);
}; break;
case 5417: { // glWindowPos2sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglWindowPos2sv(v);
}; break;
case 5418: { // glWindowPos3dv
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglWindowPos3dv(v);
}; break;
case 5419: { // glWindowPos3fv
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglWindowPos3fv(v);
}; break;
case 5420: { // glWindowPos3iv
 GLint *v = (GLint *) bp; bp += 4;
 weglWindowPos3iv(v);
}; break;
case 5421: { // glWindowPos3sv
 GLshort *v = (GLshort *) bp; bp += 2;
 weglWindowPos3sv(v);
}; break;
case 5422: { // glGenQueries
 GLsizei *n = (GLsizei *) bp; bp += 4;
 GLuint *ids;
 ids = (GLuint *) driver_alloc(sizeof(GLuint) * *n);
 weglGenQueries(*n,ids);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*n)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *n; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) ids[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*n)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*n)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*n)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(ids);
}; break;
case 5423: { // glDeleteQueries
 int * idsLen = (int *) bp; bp += 4;
 GLuint * ids = (GLuint *) bp;  bp += (8-((*idsLen*4+4)%8))%8;
 weglDeleteQueries(*idsLen,ids);
}; break;
case 5424: { // glIsQuery
 GLuint *id = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsQuery(*id);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5425: { // glBeginQuery
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *id = (GLuint *) bp; bp += 4;
 weglBeginQuery(*target,*id);
}; break;
case 5426: { // glEndQuery
 GLenum *target = (GLenum *) bp; bp += 4;
 weglEndQuery(*target);
}; break;
case 5427: { // glGetQueryiv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetQueryiv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5428: { // glGetQueryObjectiv
 GLuint *id = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetQueryObjectiv(*id,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5429: { // glGetQueryObjectuiv
 GLuint *id = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLuint params[1] = {0};
 weglGetQueryObjectuiv(*id,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5430: { // glBindBuffer
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *buffer = (GLuint *) bp; bp += 4;
 weglBindBuffer(*target,*buffer);
}; break;
case 5431: { // glDeleteBuffers
 int * buffersLen = (int *) bp; bp += 4;
 GLuint * buffers = (GLuint *) bp;  bp += (8-((*buffersLen*4+4)%8))%8;
 weglDeleteBuffers(*buffersLen,buffers);
}; break;
case 5432: { // glGenBuffers
 GLsizei *n = (GLsizei *) bp; bp += 4;
 GLuint *buffers;
 buffers = (GLuint *) driver_alloc(sizeof(GLuint) * *n);
 weglGenBuffers(*n,buffers);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*n)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *n; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) buffers[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*n)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*n)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*n)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(buffers);
}; break;
case 5433: { // glIsBuffer
 GLuint *buffer = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsBuffer(*buffer);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5434: { // glBufferData
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLsizeiptr size = (GLsizeiptr) * (GLuint64EXT *) bp; bp += 8;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 GLenum *usage = (GLenum *) bp; bp += 4;
 weglBufferData(*target,size,data,*usage);
}; break;
case 5435: { // glBufferData
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLsizeiptr size = (GLsizeiptr) * (GLuint64EXT *) bp; bp += 8;
 GLvoid *data = (GLvoid *) bins[0]->base;
 GLenum *usage = (GLenum *) bp; bp += 4;
 weglBufferData(*target,size,data,*usage);
}; break;
case 5436: { // glBufferSubData
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLintptr offset = (GLintptr) * (GLuint64EXT *) bp; bp += 8;
 GLsizeiptr size = (GLsizeiptr) * (GLuint64EXT *) bp; bp += 8;
 GLvoid *data = (GLvoid *) * (int *) bp; bp += 4;
 weglBufferSubData(*target,offset,size,data);
}; break;
case 5437: { // glBufferSubData
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLintptr offset = (GLintptr) * (GLuint64EXT *) bp; bp += 8;
 GLsizeiptr size = (GLsizeiptr) * (GLuint64EXT *) bp; bp += 8;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglBufferSubData(*target,offset,size,data);
}; break;
case 5438: { // glGetBufferSubData
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLintptr offset = (GLintptr) * (GLuint64EXT *) bp; bp += 8;
 GLsizeiptr size = (GLsizeiptr) * (GLuint64EXT *) bp; bp += 8;
 GLvoid *data = (GLvoid *) bins[0]->base;
 weglGetBufferSubData(*target,offset,size,data);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5439: { // glGetBufferParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetBufferParameteriv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5440: { // glBlendEquationSeparate
 GLenum *modeRGB = (GLenum *) bp; bp += 4;
 GLenum *modeAlpha = (GLenum *) bp; bp += 4;
 weglBlendEquationSeparate(*modeRGB,*modeAlpha);
}; break;
case 5441: { // glDrawBuffers
 int * bufsLen = (int *) bp; bp += 4;
 GLenum * bufs = (GLenum *) bp;  bp += (8-((*bufsLen*4+4)%8))%8;
 weglDrawBuffers(*bufsLen,bufs);
}; break;
case 5442: { // glStencilOpSeparate
 GLenum *face = (GLenum *) bp; bp += 4;
 GLenum *sfail = (GLenum *) bp; bp += 4;
 GLenum *dpfail = (GLenum *) bp; bp += 4;
 GLenum *dppass = (GLenum *) bp; bp += 4;
 weglStencilOpSeparate(*face,*sfail,*dpfail,*dppass);
}; break;
case 5443: { // glStencilFuncSeparate
 GLenum *frontfunc = (GLenum *) bp; bp += 4;
 GLenum *backfunc = (GLenum *) bp; bp += 4;
 GLint *ref = (GLint *) bp; bp += 4;
 GLuint *mask = (GLuint *) bp; bp += 4;
 weglStencilFuncSeparate(*frontfunc,*backfunc,*ref,*mask);
}; break;
case 5444: { // glStencilMaskSeparate
 GLenum *face = (GLenum *) bp; bp += 4;
 GLuint *mask = (GLuint *) bp; bp += 4;
 weglStencilMaskSeparate(*face,*mask);
}; break;
case 5445: { // glAttachShader
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *shader = (GLuint *) bp; bp += 4;
 weglAttachShader(*program,*shader);
}; break;
case 5446: { // glBindAttribLocation
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLchar *name = (GLchar *) bp;
 int nameLen = strlen((char *)name); bp += nameLen+1+((8-((1+nameLen+0)%8))%8);
 weglBindAttribLocation(*program,*index,name);
}; break;
case 5447: { // glCompileShader
 GLuint *shader = (GLuint *) bp; bp += 4;
 weglCompileShader(*shader);
}; break;
case 5448: { // glCreateProgram
 GLuint result = weglCreateProgram();
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5449: { // glCreateShader
 GLenum *type = (GLenum *) bp; bp += 4;
 GLuint result = weglCreateShader(*type);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5450: { // glDeleteProgram
 GLuint *program = (GLuint *) bp; bp += 4;
 weglDeleteProgram(*program);
}; break;
case 5451: { // glDeleteShader
 GLuint *shader = (GLuint *) bp; bp += 4;
 weglDeleteShader(*shader);
}; break;
case 5452: { // glDetachShader
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *shader = (GLuint *) bp; bp += 4;
 weglDetachShader(*program,*shader);
}; break;
case 5453: { // glDisableVertexAttribArray
 GLuint *index = (GLuint *) bp; bp += 4;
 weglDisableVertexAttribArray(*index);
}; break;
case 5454: { // glEnableVertexAttribArray
 GLuint *index = (GLuint *) bp; bp += 4;
 weglEnableVertexAttribArray(*index);
}; break;
case 5455: { // glGetActiveAttrib
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLsizei *bufSize = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLint size[1] = {0};
 GLenum type[1] = {0};
 GLchar *name;
 name = (GLchar *) driver_alloc(sizeof(GLchar) * *bufSize);
 weglGetActiveAttrib(*program,*index,*bufSize,length,size,type,name);
 int AP = 0; ErlDrvTermData rt[13];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *size;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *type;
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) name; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 3;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 13 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,13);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(name);
}; break;
case 5456: { // glGetActiveUniform
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLsizei *bufSize = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLint size[1] = {0};
 GLenum type[1] = {0};
 GLchar *name;
 name = (GLchar *) driver_alloc(sizeof(GLchar) * *bufSize);
 weglGetActiveUniform(*program,*index,*bufSize,length,size,type,name);
 int AP = 0; ErlDrvTermData rt[13];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *size;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *type;
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) name; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 3;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 13 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,13);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(name);
}; break;
case 5457: { // glGetAttachedShaders
 GLuint *program = (GLuint *) bp; bp += 4;
 GLsizei *maxCount = (GLsizei *) bp; bp += 4;
 GLsizei count[1] = {0};
 GLuint *obj;
 obj = (GLuint *) driver_alloc(sizeof(GLuint) * *maxCount);
 weglGetAttachedShaders(*program,*maxCount,count,obj);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*count)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *count; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) obj[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*count)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*count)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*count)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(obj);
}; break;
case 5458: { // glGetAttribLocation
 GLuint *program = (GLuint *) bp; bp += 4;
 GLchar *name = (GLchar *) bp;
 int nameLen = strlen((char *)name); bp += nameLen+1+((8-((1+nameLen+4)%8))%8);
 GLint result = weglGetAttribLocation(*program,name);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5459: { // glGetProgramiv
 GLuint *program = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetProgramiv(*program,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5460: { // glGetProgramInfoLog
 GLuint *program = (GLuint *) bp; bp += 4;
 GLsizei *bufSize = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLchar *infoLog;
 infoLog = (GLchar *) driver_alloc(sizeof(GLchar) * *bufSize);
 weglGetProgramInfoLog(*program,*bufSize,length,infoLog);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) infoLog; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(infoLog);
}; break;
case 5461: { // glGetShaderiv
 GLuint *shader = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetShaderiv(*shader,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5462: { // glGetShaderInfoLog
 GLuint *shader = (GLuint *) bp; bp += 4;
 GLsizei *bufSize = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLchar *infoLog;
 infoLog = (GLchar *) driver_alloc(sizeof(GLchar) * *bufSize);
 weglGetShaderInfoLog(*shader,*bufSize,length,infoLog);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) infoLog; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(infoLog);
}; break;
case 5463: { // glGetShaderSource
 GLuint *shader = (GLuint *) bp; bp += 4;
 GLsizei *bufSize = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLchar *source;
 source = (GLchar *) driver_alloc(sizeof(GLchar) * *bufSize);
 weglGetShaderSource(*shader,*bufSize,length,source);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) source; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(source);
}; break;
case 5464: { // glGetUniformLocation
 GLuint *program = (GLuint *) bp; bp += 4;
 GLchar *name = (GLchar *) bp;
 int nameLen = strlen((char *)name); bp += nameLen+1+((8-((1+nameLen+4)%8))%8);
 GLint result = weglGetUniformLocation(*program,name);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5465: { // glGetUniformfv
 GLuint *program = (GLuint *) bp; bp += 4;
 GLint *location = (GLint *) bp; bp += 4;
 GLfloat params[16] = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
 weglGetUniformfv(*program,*location,params);
 int AP = 0; ErlDrvTermData rt[38];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[16], *paramsTmp = paramsConv; 
 for(int i=0; i < 16; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 16;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 38 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,38);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5466: { // glGetUniformiv
 GLuint *program = (GLuint *) bp; bp += 4;
 GLint *location = (GLint *) bp; bp += 4;
 GLint params[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
 weglGetUniformiv(*program,*location,params);
 int AP = 0; ErlDrvTermData rt[38];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 16;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 38 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,38);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5467: { // glGetVertexAttribdv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLdouble params[4] = {0.0,0.0,0.0,0.0};
 weglGetVertexAttribdv(*index,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble *paramsTmp = params;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5468: { // glGetVertexAttribfv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetVertexAttribfv(*index,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5469: { // glGetVertexAttribiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetVertexAttribiv(*index,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5470: { // glIsProgram
 GLuint *program = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsProgram(*program);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5471: { // glIsShader
 GLuint *shader = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsShader(*shader);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5472: { // glLinkProgram
 GLuint *program = (GLuint *) bp; bp += 4;
 weglLinkProgram(*program);
}; break;
case 5473: { // glShaderSource
 GLuint *shader = (GLuint *) bp; bp += 4;
 int * stringLen = (int *) bp; bp += 4;
 int * stringTotSize = (int *) bp; bp += 4;
 GLchar **string;
 string = (GLchar **) driver_alloc(sizeof(GLchar *) * *stringLen);
 for(int i=0;i<*stringLen;i++) {
    string[i] = (GLchar *) bp; bp += 1+strlen(bp);};
 bp += (8 - ((0 + *stringTotSize) % 8)) % 8;
 weglShaderSource(*shader,*stringLen,(const GLchar **) string,NULL);
 driver_free(string);
}; break;
case 5474: { // glUseProgram
 GLuint *program = (GLuint *) bp; bp += 4;
 weglUseProgram(*program);
}; break;
case 5475: { // glUniform1f
 GLint *location = (GLint *) bp; bp += 4;
 GLfloat *v0 = (GLfloat *) bp; bp += 4;
 weglUniform1f(*location,*v0);
}; break;
case 5476: { // glUniform2f
 GLint *location = (GLint *) bp; bp += 4;
 GLfloat *v0 = (GLfloat *) bp; bp += 4;
 GLfloat *v1 = (GLfloat *) bp; bp += 4;
 weglUniform2f(*location,*v0,*v1);
}; break;
case 5477: { // glUniform3f
 GLint *location = (GLint *) bp; bp += 4;
 GLfloat *v0 = (GLfloat *) bp; bp += 4;
 GLfloat *v1 = (GLfloat *) bp; bp += 4;
 GLfloat *v2 = (GLfloat *) bp; bp += 4;
 weglUniform3f(*location,*v0,*v1,*v2);
}; break;
case 5478: { // glUniform4f
 GLint *location = (GLint *) bp; bp += 4;
 GLfloat *v0 = (GLfloat *) bp; bp += 4;
 GLfloat *v1 = (GLfloat *) bp; bp += 4;
 GLfloat *v2 = (GLfloat *) bp; bp += 4;
 GLfloat *v3 = (GLfloat *) bp; bp += 4;
 weglUniform4f(*location,*v0,*v1,*v2,*v3);
}; break;
case 5479: { // glUniform1i
 GLint *location = (GLint *) bp; bp += 4;
 GLint *v0 = (GLint *) bp; bp += 4;
 weglUniform1i(*location,*v0);
}; break;
case 5480: { // glUniform2i
 GLint *location = (GLint *) bp; bp += 4;
 GLint *v0 = (GLint *) bp; bp += 4;
 GLint *v1 = (GLint *) bp; bp += 4;
 weglUniform2i(*location,*v0,*v1);
}; break;
case 5481: { // glUniform3i
 GLint *location = (GLint *) bp; bp += 4;
 GLint *v0 = (GLint *) bp; bp += 4;
 GLint *v1 = (GLint *) bp; bp += 4;
 GLint *v2 = (GLint *) bp; bp += 4;
 weglUniform3i(*location,*v0,*v1,*v2);
}; break;
case 5482: { // glUniform4i
 GLint *location = (GLint *) bp; bp += 4;
 GLint *v0 = (GLint *) bp; bp += 4;
 GLint *v1 = (GLint *) bp; bp += 4;
 GLint *v2 = (GLint *) bp; bp += 4;
 GLint *v3 = (GLint *) bp; bp += 4;
 weglUniform4i(*location,*v0,*v1,*v2,*v3);
}; break;
case 5483: { // glUniform1fv
 GLint *location = (GLint *) bp; bp += 4;
 int * valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp;  bp += (8-((*valueLen*4+0)%8))%8;
 weglUniform1fv(*location,*valueLen,value);
}; break;
case 5484: { // glUniform2fv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*8;
 weglUniform2fv(*location,*valueLen,value);
}; break;
case 5485: { // glUniform3fv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*12;
 weglUniform3fv(*location,*valueLen,value);
}; break;
case 5486: { // glUniform4fv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*16;
 weglUniform4fv(*location,*valueLen,value);
}; break;
case 5487: { // glUniform1iv
 GLint *location = (GLint *) bp; bp += 4;
 int * valueLen = (int *) bp; bp += 4;
 GLint * value = (GLint *) bp;  bp += (8-((*valueLen*4+0)%8))%8;
 weglUniform1iv(*location,*valueLen,value);
}; break;
case 5488: { // glUniform2iv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLint * value = (GLint *) bp; bp += *valueLen*8;
 weglUniform2iv(*location,*valueLen,value);
}; break;
case 5489: { // glUniform3iv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLint * value = (GLint *) bp; bp += *valueLen*12;
 weglUniform3iv(*location,*valueLen,value);
}; break;
case 5490: { // glUniform4iv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLint * value = (GLint *) bp; bp += *valueLen*16;
 weglUniform4iv(*location,*valueLen,value);
}; break;
case 5491: { // glUniformMatrix2fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*16;
 weglUniformMatrix2fv(*location,*valueLen,*transpose,value);
}; break;
case 5492: { // glUniformMatrix3fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*36;
 weglUniformMatrix3fv(*location,*valueLen,*transpose,value);
}; break;
case 5493: { // glUniformMatrix4fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*64;
 weglUniformMatrix4fv(*location,*valueLen,*transpose,value);
}; break;
case 5494: { // glValidateProgram
 GLuint *program = (GLuint *) bp; bp += 4;
 weglValidateProgram(*program);
}; break;
case 5495: { // glVertexAttrib1dv
 GLuint *index = (GLuint *) bp; bp += 4;
 bp += 4;
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglVertexAttrib1dv(*index,v);
}; break;
case 5496: { // glVertexAttrib1fv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglVertexAttrib1fv(*index,v);
}; break;
case 5497: { // glVertexAttrib1sv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLshort *v = (GLshort *) bp; bp += 2;
 weglVertexAttrib1sv(*index,v);
}; break;
case 5498: { // glVertexAttrib2dv
 GLuint *index = (GLuint *) bp; bp += 4;
 bp += 4;
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglVertexAttrib2dv(*index,v);
}; break;
case 5499: { // glVertexAttrib2fv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglVertexAttrib2fv(*index,v);
}; break;
case 5500: { // glVertexAttrib2sv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLshort *v = (GLshort *) bp; bp += 2;
 weglVertexAttrib2sv(*index,v);
}; break;
case 5501: { // glVertexAttrib3dv
 GLuint *index = (GLuint *) bp; bp += 4;
 bp += 4;
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglVertexAttrib3dv(*index,v);
}; break;
case 5502: { // glVertexAttrib3fv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglVertexAttrib3fv(*index,v);
}; break;
case 5503: { // glVertexAttrib3sv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLshort *v = (GLshort *) bp; bp += 2;
 weglVertexAttrib3sv(*index,v);
}; break;
case 5504: { // glVertexAttrib4Nbv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLbyte * v = (GLbyte *) bp; bp += 4;
 weglVertexAttrib4Nbv(*index,v);
}; break;
case 5505: { // glVertexAttrib4Niv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint * v = (GLint *) bp; bp += 16;
 weglVertexAttrib4Niv(*index,v);
}; break;
case 5506: { // glVertexAttrib4Nsv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLshort * v = (GLshort *) bp; bp += 8;
 weglVertexAttrib4Nsv(*index,v);
}; break;
case 5507: { // glVertexAttrib4Nubv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLubyte * v = (GLubyte *) bp; bp += 4;
 weglVertexAttrib4Nubv(*index,v);
}; break;
case 5508: { // glVertexAttrib4Nuiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint * v = (GLuint *) bp; bp += 16;
 weglVertexAttrib4Nuiv(*index,v);
}; break;
case 5509: { // glVertexAttrib4Nusv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLushort * v = (GLushort *) bp; bp += 8;
 weglVertexAttrib4Nusv(*index,v);
}; break;
case 5510: { // glVertexAttrib4bv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLbyte * v = (GLbyte *) bp; bp += 4;
 weglVertexAttrib4bv(*index,v);
}; break;
case 5511: { // glVertexAttrib4dv
 GLuint *index = (GLuint *) bp; bp += 4;
 bp += 4;
 GLdouble * v = (GLdouble *) bp; bp += 32;
 weglVertexAttrib4dv(*index,v);
}; break;
case 5512: { // glVertexAttrib4fv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat * v = (GLfloat *) bp; bp += 16;
 weglVertexAttrib4fv(*index,v);
}; break;
case 5513: { // glVertexAttrib4iv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint * v = (GLint *) bp; bp += 16;
 weglVertexAttrib4iv(*index,v);
}; break;
case 5514: { // glVertexAttrib4sv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLshort * v = (GLshort *) bp; bp += 8;
 weglVertexAttrib4sv(*index,v);
}; break;
case 5515: { // glVertexAttrib4ubv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLubyte * v = (GLubyte *) bp; bp += 4;
 weglVertexAttrib4ubv(*index,v);
}; break;
case 5516: { // glVertexAttrib4uiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint * v = (GLuint *) bp; bp += 16;
 weglVertexAttrib4uiv(*index,v);
}; break;
case 5517: { // glVertexAttrib4usv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLushort * v = (GLushort *) bp; bp += 8;
 weglVertexAttrib4usv(*index,v);
}; break;
case 5518: { // glVertexAttribPointer
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLboolean *normalized = (GLboolean *) bp; bp += 1;
 bp += 3;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglVertexAttribPointer(*index,*size,*type,*normalized,*stride,pointer);
}; break;
case 5519: { // glVertexAttribPointer
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLboolean *normalized = (GLboolean *) bp; bp += 1;
 bp += 3;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglVertexAttribPointer(*index,*size,*type,*normalized,*stride,pointer);
}; break;
case 5520: { // glUniformMatrix2x3fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*24;
 weglUniformMatrix2x3fv(*location,*valueLen,*transpose,value);
}; break;
case 5521: { // glUniformMatrix3x2fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*24;
 weglUniformMatrix3x2fv(*location,*valueLen,*transpose,value);
}; break;
case 5522: { // glUniformMatrix2x4fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*32;
 weglUniformMatrix2x4fv(*location,*valueLen,*transpose,value);
}; break;
case 5523: { // glUniformMatrix4x2fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*32;
 weglUniformMatrix4x2fv(*location,*valueLen,*transpose,value);
}; break;
case 5524: { // glUniformMatrix3x4fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*48;
 weglUniformMatrix3x4fv(*location,*valueLen,*transpose,value);
}; break;
case 5525: { // glUniformMatrix4x3fv
 GLint *location = (GLint *) bp; bp += 4;
 GLboolean *transpose = (GLboolean *) bp; bp += 1;
 bp += 3;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat * value = (GLfloat *) bp; bp += *valueLen*48;
 weglUniformMatrix4x3fv(*location,*valueLen,*transpose,value);
}; break;
case 5526: { // glColorMaski
 GLuint *index = (GLuint *) bp; bp += 4;
 GLboolean *r = (GLboolean *) bp; bp += 1;
 GLboolean *g = (GLboolean *) bp; bp += 1;
 GLboolean *b = (GLboolean *) bp; bp += 1;
 GLboolean *a = (GLboolean *) bp; bp += 1;
 weglColorMaski(*index,*r,*g,*b,*a);
}; break;
case 5527: { // glGetBooleani_v
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLboolean data[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
 weglGetBooleani_v(*target,*index,data);
 int AP = 0; ErlDrvTermData rt[39];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLboolean *dataTmp = data;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = 16+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 39 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,39);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5528: { // glGetIntegeri_v
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint data[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
 weglGetIntegeri_v(*target,*index,data);
 int AP = 0; ErlDrvTermData rt[39];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *dataTmp = data;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *dataTmp++;
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = 16+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 39 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,39);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5529: { // glEnablei
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 weglEnablei(*target,*index);
}; break;
case 5530: { // glDisablei
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 weglDisablei(*target,*index);
}; break;
case 5531: { // glIsEnabledi
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsEnabledi(*target,*index);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5532: { // glBeginTransformFeedback
 GLenum *primitiveMode = (GLenum *) bp; bp += 4;
 weglBeginTransformFeedback(*primitiveMode);
}; break;
case 5533: { // glEndTransformFeedback
 weglEndTransformFeedback();
}; break;
case 5534: { // glBindBufferRange
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint *buffer = (GLuint *) bp; bp += 4;
 bp += 4;
 GLintptr offset = (GLintptr) * (GLuint64EXT *) bp; bp += 8;
 GLsizeiptr size = (GLsizeiptr) * (GLuint64EXT *) bp; bp += 8;
 weglBindBufferRange(*target,*index,*buffer,offset,size);
}; break;
case 5535: { // glBindBufferBase
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint *buffer = (GLuint *) bp; bp += 4;
 weglBindBufferBase(*target,*index,*buffer);
}; break;
case 5536: { // glTransformFeedbackVaryings
 GLuint *program = (GLuint *) bp; bp += 4;
 int * varyingsLen = (int *) bp; bp += 4;
 int * varyingsTotSize = (int *) bp; bp += 4;
 GLchar **varyings;
 varyings = (GLchar **) driver_alloc(sizeof(GLchar *) * *varyingsLen);
 for(int i=0;i<*varyingsLen;i++) {
    varyings[i] = (GLchar *) bp; bp += 1+strlen(bp);};
 bp += (8 - ((0 + *varyingsTotSize) % 8)) % 8;
 GLenum *bufferMode = (GLenum *) bp; bp += 4;
 weglTransformFeedbackVaryings(*program,*varyingsLen,(const GLchar **) varyings,*bufferMode);
 driver_free(varyings);
}; break;
case 5537: { // glGetTransformFeedbackVarying
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLsizei *bufSize = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLsizei size[1] = {0};
 GLenum type[1] = {0};
 GLchar *name;
 name = (GLchar *) driver_alloc(sizeof(GLchar) * *bufSize);
 weglGetTransformFeedbackVarying(*program,*index,*bufSize,length,size,type,name);
 int AP = 0; ErlDrvTermData rt[13];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *size;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *type;
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) name; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 3;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 13 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,13);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(name);
}; break;
case 5538: { // glClampColor
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *clamp = (GLenum *) bp; bp += 4;
 weglClampColor(*target,*clamp);
}; break;
case 5539: { // glBeginConditionalRender
 GLuint *id = (GLuint *) bp; bp += 4;
 GLenum *mode = (GLenum *) bp; bp += 4;
 weglBeginConditionalRender(*id,*mode);
}; break;
case 5540: { // glEndConditionalRender
 weglEndConditionalRender();
}; break;
case 5541: { // glVertexAttribIPointer
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) * (int *) bp; bp += 4;
 weglVertexAttribIPointer(*index,*size,*type,*stride,pointer);
}; break;
case 5542: { // glVertexAttribIPointer
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint *size = (GLint *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLsizei *stride = (GLsizei *) bp; bp += 4;
 GLvoid *pointer = (GLvoid *) bins[0]->base;
 weglVertexAttribIPointer(*index,*size,*type,*stride,pointer);
}; break;
case 5543: { // glGetVertexAttribIiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetVertexAttribIiv(*index,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5544: { // glGetVertexAttribIuiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLuint params[4] = {0,0,0,0};
 weglGetVertexAttribIuiv(*index,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLuint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5545: { // glGetUniformuiv
 GLuint *program = (GLuint *) bp; bp += 4;
 GLint *location = (GLint *) bp; bp += 4;
 GLuint params[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
 weglGetUniformuiv(*program,*location,params);
 int AP = 0; ErlDrvTermData rt[38];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLuint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 16;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 38 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,38);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5546: { // glBindFragDataLocation
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *color = (GLuint *) bp; bp += 4;
 GLchar *name = (GLchar *) bp;
 int nameLen = strlen((char *)name); bp += nameLen+1+((8-((1+nameLen+0)%8))%8);
 weglBindFragDataLocation(*program,*color,name);
}; break;
case 5547: { // glGetFragDataLocation
 GLuint *program = (GLuint *) bp; bp += 4;
 GLchar *name = (GLchar *) bp;
 int nameLen = strlen((char *)name); bp += nameLen+1+((8-((1+nameLen+4)%8))%8);
 GLint result = weglGetFragDataLocation(*program,name);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5548: { // glUniform1ui
 GLint *location = (GLint *) bp; bp += 4;
 GLuint *v0 = (GLuint *) bp; bp += 4;
 weglUniform1ui(*location,*v0);
}; break;
case 5549: { // glUniform2ui
 GLint *location = (GLint *) bp; bp += 4;
 GLuint *v0 = (GLuint *) bp; bp += 4;
 GLuint *v1 = (GLuint *) bp; bp += 4;
 weglUniform2ui(*location,*v0,*v1);
}; break;
case 5550: { // glUniform3ui
 GLint *location = (GLint *) bp; bp += 4;
 GLuint *v0 = (GLuint *) bp; bp += 4;
 GLuint *v1 = (GLuint *) bp; bp += 4;
 GLuint *v2 = (GLuint *) bp; bp += 4;
 weglUniform3ui(*location,*v0,*v1,*v2);
}; break;
case 5551: { // glUniform4ui
 GLint *location = (GLint *) bp; bp += 4;
 GLuint *v0 = (GLuint *) bp; bp += 4;
 GLuint *v1 = (GLuint *) bp; bp += 4;
 GLuint *v2 = (GLuint *) bp; bp += 4;
 GLuint *v3 = (GLuint *) bp; bp += 4;
 weglUniform4ui(*location,*v0,*v1,*v2,*v3);
}; break;
case 5552: { // glUniform1uiv
 GLint *location = (GLint *) bp; bp += 4;
 int * valueLen = (int *) bp; bp += 4;
 GLuint * value = (GLuint *) bp;  bp += (8-((*valueLen*4+0)%8))%8;
 weglUniform1uiv(*location,*valueLen,value);
}; break;
case 5553: { // glUniform2uiv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLuint * value = (GLuint *) bp; bp += *valueLen*8;
 weglUniform2uiv(*location,*valueLen,value);
}; break;
case 5554: { // glUniform3uiv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLuint * value = (GLuint *) bp; bp += *valueLen*12;
 weglUniform3uiv(*location,*valueLen,value);
}; break;
case 5555: { // glUniform4uiv
 GLint *location = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLuint * value = (GLuint *) bp; bp += *valueLen*16;
 weglUniform4uiv(*location,*valueLen,value);
}; break;
case 5556: { // glTexParameterIiv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLint *params = (GLint *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglTexParameterIiv(*target,*pname,params);
}; break;
case 5557: { // glTexParameterIuiv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 int *paramsLen = (int *) bp; bp += 4;
 GLuint *params = (GLuint *) bp; bp += *paramsLen*4+((*paramsLen)+1)%2*4;
 weglTexParameterIuiv(*target,*pname,params);
}; break;
case 5558: { // glGetTexParameterIiv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[4] = {0,0,0,0};
 weglGetTexParameterIiv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5559: { // glGetTexParameterIuiv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLuint params[4] = {0,0,0,0};
 weglGetTexParameterIuiv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLuint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5560: { // glClearBufferiv
 GLenum *buffer = (GLenum *) bp; bp += 4;
 GLint *drawbuffer = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLint *value = (GLint *) bp; bp += *valueLen*4+((*valueLen)+1)%2*4;
 weglClearBufferiv(*buffer,*drawbuffer,value);
}; break;
case 5561: { // glClearBufferuiv
 GLenum *buffer = (GLenum *) bp; bp += 4;
 GLint *drawbuffer = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLuint *value = (GLuint *) bp; bp += *valueLen*4+((*valueLen)+1)%2*4;
 weglClearBufferuiv(*buffer,*drawbuffer,value);
}; break;
case 5562: { // glClearBufferfv
 GLenum *buffer = (GLenum *) bp; bp += 4;
 GLint *drawbuffer = (GLint *) bp; bp += 4;
 int *valueLen = (int *) bp; bp += 4;
 GLfloat *value = (GLfloat *) bp; bp += *valueLen*4+((*valueLen)+1)%2*4;
 weglClearBufferfv(*buffer,*drawbuffer,value);
}; break;
case 5563: { // glClearBufferfi
 GLenum *buffer = (GLenum *) bp; bp += 4;
 GLint *drawbuffer = (GLint *) bp; bp += 4;
 GLfloat *depth = (GLfloat *) bp; bp += 4;
 GLint *stencil = (GLint *) bp; bp += 4;
 weglClearBufferfi(*buffer,*drawbuffer,*depth,*stencil);
}; break;
case 5564: { // glGetStringi
 GLenum *name = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 const GLubyte *  result = weglGetStringi(*name,*index);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) result; rt[AP++] = strlen((char *) result);
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5565: { // glVertexAttribI1iv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint *v = (GLint *) bp; bp += 4;
 weglVertexAttribI1iv(*index,v);
}; break;
case 5566: { // glVertexAttribI2iv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint *v = (GLint *) bp; bp += 4;
 weglVertexAttribI2iv(*index,v);
}; break;
case 5567: { // glVertexAttribI3iv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint *v = (GLint *) bp; bp += 4;
 weglVertexAttribI3iv(*index,v);
}; break;
case 5568: { // glVertexAttribI4iv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLint * v = (GLint *) bp; bp += 16;
 weglVertexAttribI4iv(*index,v);
}; break;
case 5569: { // glVertexAttribI1uiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint *v = (GLuint *) bp; bp += 4;
 weglVertexAttribI1uiv(*index,v);
}; break;
case 5570: { // glVertexAttribI2uiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint *v = (GLuint *) bp; bp += 4;
 weglVertexAttribI2uiv(*index,v);
}; break;
case 5571: { // glVertexAttribI3uiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint *v = (GLuint *) bp; bp += 4;
 weglVertexAttribI3uiv(*index,v);
}; break;
case 5572: { // glVertexAttribI4uiv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint * v = (GLuint *) bp; bp += 16;
 weglVertexAttribI4uiv(*index,v);
}; break;
case 5573: { // glVertexAttribI4bv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLbyte * v = (GLbyte *) bp; bp += 4;
 weglVertexAttribI4bv(*index,v);
}; break;
case 5574: { // glVertexAttribI4sv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLshort * v = (GLshort *) bp; bp += 8;
 weglVertexAttribI4sv(*index,v);
}; break;
case 5575: { // glVertexAttribI4ubv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLubyte * v = (GLubyte *) bp; bp += 4;
 weglVertexAttribI4ubv(*index,v);
}; break;
case 5576: { // glVertexAttribI4usv
 GLuint *index = (GLuint *) bp; bp += 4;
 GLushort * v = (GLushort *) bp; bp += 8;
 weglVertexAttribI4usv(*index,v);
}; break;
case 5577: { // glDrawArraysInstanced
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLint *first = (GLint *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLsizei *primcount = (GLsizei *) bp; bp += 4;
 weglDrawArraysInstanced(*mode,*first,*count,*primcount);
}; break;
case 5578: { // glDrawElementsInstanced
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *indices = (GLvoid *) * (int *) bp; bp += 4;
 GLsizei *primcount = (GLsizei *) bp; bp += 4;
 weglDrawElementsInstanced(*mode,*count,*type,indices,*primcount);
}; break;
case 5579: { // glDrawElementsInstanced
 GLenum *mode = (GLenum *) bp; bp += 4;
 GLsizei *count = (GLsizei *) bp; bp += 4;
 GLenum *type = (GLenum *) bp; bp += 4;
 GLvoid *indices = (GLvoid *) bins[0]->base;
 GLsizei *primcount = (GLsizei *) bp; bp += 4;
 weglDrawElementsInstanced(*mode,*count,*type,indices,*primcount);
}; break;
case 5580: { // glTexBuffer
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLuint *buffer = (GLuint *) bp; bp += 4;
 weglTexBuffer(*target,*internalformat,*buffer);
}; break;
case 5581: { // glPrimitiveRestartIndex
 GLuint *index = (GLuint *) bp; bp += 4;
 weglPrimitiveRestartIndex(*index);
}; break;
case 5582: { // glLoadTransposeMatrixfARB
 GLfloat * m = (GLfloat *) bp; bp += 64;
 weglLoadTransposeMatrixfARB(m);
}; break;
case 5583: { // glLoadTransposeMatrixdARB
 GLdouble * m = (GLdouble *) bp; bp += 128;
 weglLoadTransposeMatrixdARB(m);
}; break;
case 5584: { // glMultTransposeMatrixfARB
 GLfloat * m = (GLfloat *) bp; bp += 64;
 weglMultTransposeMatrixfARB(m);
}; break;
case 5585: { // glMultTransposeMatrixdARB
 GLdouble * m = (GLdouble *) bp; bp += 128;
 weglMultTransposeMatrixdARB(m);
}; break;
case 5586: { // glWeightbvARB
 int * weightsLen = (int *) bp; bp += 4;
 GLbyte * weights = (GLbyte *) bp;  bp += (8-((*weightsLen*1+4)%8))%8;
 weglWeightbvARB(*weightsLen,weights);
}; break;
case 5587: { // glWeightsvARB
 int * weightsLen = (int *) bp; bp += 4;
 GLshort * weights = (GLshort *) bp;  bp += (8-((*weightsLen*2+4)%8))%8;
 weglWeightsvARB(*weightsLen,weights);
}; break;
case 5588: { // glWeightivARB
 int * weightsLen = (int *) bp; bp += 4;
 GLint * weights = (GLint *) bp;  bp += (8-((*weightsLen*4+4)%8))%8;
 weglWeightivARB(*weightsLen,weights);
}; break;
case 5589: { // glWeightfvARB
 int * weightsLen = (int *) bp; bp += 4;
 GLfloat * weights = (GLfloat *) bp;  bp += (8-((*weightsLen*4+4)%8))%8;
 weglWeightfvARB(*weightsLen,weights);
}; break;
case 5590: { // glWeightdvARB
 int * weightsLen = (int *) bp; bp += 8;
 GLdouble * weights = (GLdouble *) bp;  bp += (8-((*weightsLen*8+0)%8))%8;
 weglWeightdvARB(*weightsLen,weights);
}; break;
case 5591: { // glWeightubvARB
 int * weightsLen = (int *) bp; bp += 4;
 GLubyte * weights = (GLubyte *) bp;  bp += (8-((*weightsLen*1+4)%8))%8;
 weglWeightubvARB(*weightsLen,weights);
}; break;
case 5592: { // glWeightusvARB
 int * weightsLen = (int *) bp; bp += 4;
 GLushort * weights = (GLushort *) bp;  bp += (8-((*weightsLen*2+4)%8))%8;
 weglWeightusvARB(*weightsLen,weights);
}; break;
case 5593: { // glWeightuivARB
 int * weightsLen = (int *) bp; bp += 4;
 GLuint * weights = (GLuint *) bp;  bp += (8-((*weightsLen*4+4)%8))%8;
 weglWeightuivARB(*weightsLen,weights);
}; break;
case 5594: { // glVertexBlendARB
 GLint *count = (GLint *) bp; bp += 4;
 weglVertexBlendARB(*count);
}; break;
case 5595: { // glCurrentPaletteMatrixARB
 GLint *index = (GLint *) bp; bp += 4;
 weglCurrentPaletteMatrixARB(*index);
}; break;
case 5596: { // glMatrixIndexubvARB
 int * indicesLen = (int *) bp; bp += 4;
 GLubyte * indices = (GLubyte *) bp;  bp += (8-((*indicesLen*1+4)%8))%8;
 weglMatrixIndexubvARB(*indicesLen,indices);
}; break;
case 5597: { // glMatrixIndexusvARB
 int * indicesLen = (int *) bp; bp += 4;
 GLushort * indices = (GLushort *) bp;  bp += (8-((*indicesLen*2+4)%8))%8;
 weglMatrixIndexusvARB(*indicesLen,indices);
}; break;
case 5598: { // glMatrixIndexuivARB
 int * indicesLen = (int *) bp; bp += 4;
 GLuint * indices = (GLuint *) bp;  bp += (8-((*indicesLen*4+4)%8))%8;
 weglMatrixIndexuivARB(*indicesLen,indices);
}; break;
case 5599: { // glProgramStringARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *format = (GLenum *) bp; bp += 4;
 GLvoid *string = (GLvoid *) bp;
 int stringLen = strlen((char *)string); bp += stringLen+1+((8-((1+stringLen+0)%8))%8);
 weglProgramStringARB(*target,*format,stringLen,string);
}; break;
case 5600: { // glBindProgramARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *program = (GLuint *) bp; bp += 4;
 weglBindProgramARB(*target,*program);
}; break;
case 5601: { // glDeleteProgramsARB
 int * programsLen = (int *) bp; bp += 4;
 GLuint * programs = (GLuint *) bp;  bp += (8-((*programsLen*4+4)%8))%8;
 weglDeleteProgramsARB(*programsLen,programs);
}; break;
case 5602: { // glGenProgramsARB
 GLsizei *n = (GLsizei *) bp; bp += 4;
 GLuint *programs;
 programs = (GLuint *) driver_alloc(sizeof(GLuint) * *n);
 weglGenProgramsARB(*n,programs);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*n)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *n; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) programs[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*n)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*n)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*n)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(programs);
}; break;
case 5603: { // glProgramEnvParameter4dARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLdouble *x = (GLdouble *) bp; bp += 8;
 GLdouble *y = (GLdouble *) bp; bp += 8;
 GLdouble *z = (GLdouble *) bp; bp += 8;
 GLdouble *w = (GLdouble *) bp; bp += 8;
 weglProgramEnvParameter4dARB(*target,*index,*x,*y,*z,*w);
}; break;
case 5604: { // glProgramEnvParameter4dvARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLdouble * params = (GLdouble *) bp; bp += 32;
 weglProgramEnvParameter4dvARB(*target,*index,params);
}; break;
case 5605: { // glProgramEnvParameter4fARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat *x = (GLfloat *) bp; bp += 4;
 GLfloat *y = (GLfloat *) bp; bp += 4;
 GLfloat *z = (GLfloat *) bp; bp += 4;
 GLfloat *w = (GLfloat *) bp; bp += 4;
 weglProgramEnvParameter4fARB(*target,*index,*x,*y,*z,*w);
}; break;
case 5606: { // glProgramEnvParameter4fvARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat * params = (GLfloat *) bp; bp += 16;
 weglProgramEnvParameter4fvARB(*target,*index,params);
}; break;
case 5607: { // glProgramLocalParameter4dARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLdouble *x = (GLdouble *) bp; bp += 8;
 GLdouble *y = (GLdouble *) bp; bp += 8;
 GLdouble *z = (GLdouble *) bp; bp += 8;
 GLdouble *w = (GLdouble *) bp; bp += 8;
 weglProgramLocalParameter4dARB(*target,*index,*x,*y,*z,*w);
}; break;
case 5608: { // glProgramLocalParameter4dvARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLdouble * params = (GLdouble *) bp; bp += 32;
 weglProgramLocalParameter4dvARB(*target,*index,params);
}; break;
case 5609: { // glProgramLocalParameter4fARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat *x = (GLfloat *) bp; bp += 4;
 GLfloat *y = (GLfloat *) bp; bp += 4;
 GLfloat *z = (GLfloat *) bp; bp += 4;
 GLfloat *w = (GLfloat *) bp; bp += 4;
 weglProgramLocalParameter4fARB(*target,*index,*x,*y,*z,*w);
}; break;
case 5610: { // glProgramLocalParameter4fvARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat * params = (GLfloat *) bp; bp += 16;
 weglProgramLocalParameter4fvARB(*target,*index,params);
}; break;
case 5611: { // glGetProgramEnvParameterdvARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLdouble params[4] = {0.0,0.0,0.0,0.0};
 weglGetProgramEnvParameterdvARB(*target,*index,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble *paramsTmp = params;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5612: { // glGetProgramEnvParameterfvARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetProgramEnvParameterfvARB(*target,*index,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5613: { // glGetProgramLocalParameterdvARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLdouble params[4] = {0.0,0.0,0.0,0.0};
 weglGetProgramLocalParameterdvARB(*target,*index,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble *paramsTmp = params;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5614: { // glGetProgramLocalParameterfvARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLfloat params[4] = {0.0,0.0,0.0,0.0};
 weglGetProgramLocalParameterfvARB(*target,*index,params);
 int AP = 0; ErlDrvTermData rt[14];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[4], *paramsTmp = paramsConv; 
 for(int i=0; i < 4; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 4;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 14 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,14);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5615: { // glGetProgramStringARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLvoid *string = (GLvoid *) bins[0]->base;
 weglGetProgramStringARB(*target,*pname,string);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5616: { // glDeleteObjectARB
 GLhandleARB obj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 weglDeleteObjectARB(obj);
}; break;
case 5617: { // glGetHandleARB
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLhandleARB result = weglGetHandleARB(*pname);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5618: { // glDetachObjectARB
 GLhandleARB containerObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLhandleARB attachedObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 weglDetachObjectARB(containerObj,attachedObj);
}; break;
case 5619: { // glCreateShaderObjectARB
 GLenum *shaderType = (GLenum *) bp; bp += 4;
 GLhandleARB result = weglCreateShaderObjectARB(*shaderType);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5620: { // glShaderSourceARB
 GLhandleARB shaderObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 int * stringLen = (int *) bp; bp += 4;
 int * stringTotSize = (int *) bp; bp += 4;
 GLchar **string;
 string = (GLchar **) driver_alloc(sizeof(GLchar *) * *stringLen);
 for(int i=0;i<*stringLen;i++) {
    string[i] = (GLchar *) bp; bp += 1+strlen(bp);};
 bp += (8 - ((4 + *stringTotSize) % 8)) % 8;
 weglShaderSourceARB(shaderObj,*stringLen,(const GLchar **) string,NULL);
 driver_free(string);
}; break;
case 5621: { // glCompileShaderARB
 GLhandleARB shaderObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 weglCompileShaderARB(shaderObj);
}; break;
case 5622: { // glCreateProgramObjectARB
 GLhandleARB result = weglCreateProgramObjectARB();
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5623: { // glAttachObjectARB
 GLhandleARB containerObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLhandleARB obj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 weglAttachObjectARB(containerObj,obj);
}; break;
case 5624: { // glLinkProgramARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 weglLinkProgramARB(programObj);
}; break;
case 5625: { // glUseProgramObjectARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 weglUseProgramObjectARB(programObj);
}; break;
case 5626: { // glValidateProgramARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 weglValidateProgramARB(programObj);
}; break;
case 5627: { // glGetObjectParameterfvARB
 GLhandleARB obj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLfloat params[1] = {0.0};
 weglGetObjectParameterfvARB(obj,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv = (double) *params; 
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) &paramsConv;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5628: { // glGetObjectParameterivARB
 GLhandleARB obj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetObjectParameterivARB(obj,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5629: { // glGetInfoLogARB
 GLhandleARB obj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLsizei *maxLength = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLchar *infoLog;
 infoLog = (GLchar *) driver_alloc(sizeof(GLchar) * *maxLength);
 weglGetInfoLogARB(obj,*maxLength,length,infoLog);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) infoLog; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(infoLog);
}; break;
case 5630: { // glGetAttachedObjectsARB
 GLhandleARB containerObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLsizei *maxCount = (GLsizei *) bp; bp += 4;
 GLsizei count[1] = {0};
 GLhandleARB *obj;
 obj = (GLhandleARB *) driver_alloc(sizeof(GLhandleARB) * *maxCount);
 weglGetAttachedObjectsARB(containerObj,*maxCount,count,obj);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*count)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *count; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) obj[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*count)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*count)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*count)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(obj);
}; break;
case 5631: { // glGetUniformLocationARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLchar *name = (GLchar *) bp;
 int nameLen = strlen((char *)name); bp += nameLen+1+((8-((1+nameLen+0)%8))%8);
 GLint result = weglGetUniformLocationARB(programObj,name);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5632: { // glGetActiveUniformARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLsizei *maxLength = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLint size[1] = {0};
 GLenum type[1] = {0};
 GLchar *name;
 name = (GLchar *) driver_alloc(sizeof(GLchar) * *maxLength);
 weglGetActiveUniformARB(programObj,*index,*maxLength,length,size,type,name);
 int AP = 0; ErlDrvTermData rt[13];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *size;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *type;
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) name; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 3;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 13 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,13);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(name);
}; break;
case 5633: { // glGetUniformfvARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLint *location = (GLint *) bp; bp += 4;
 GLfloat params[16] = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
 weglGetUniformfvARB(programObj,*location,params);
 int AP = 0; ErlDrvTermData rt[38];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLdouble paramsConv[16], *paramsTmp = paramsConv; 
 for(int i=0; i < 16; i++) paramsConv[i] = (GLdouble) params[i];
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 16;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 38 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,38);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5634: { // glGetUniformivARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLint *location = (GLint *) bp; bp += 4;
 GLint params[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
 weglGetUniformivARB(programObj,*location,params);
 int AP = 0; ErlDrvTermData rt[38];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 GLint *paramsTmp = params;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *paramsTmp++;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 16;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 38 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,38);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5635: { // glGetShaderSourceARB
 GLhandleARB obj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLsizei *maxLength = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLchar *source;
 source = (GLchar *) driver_alloc(sizeof(GLchar) * *maxLength);
 weglGetShaderSourceARB(obj,*maxLength,length,source);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) source; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(source);
}; break;
case 5636: { // glBindAttribLocationARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLchar *name = (GLchar *) bp;
 int nameLen = strlen((char *)name); bp += nameLen+1+((8-((1+nameLen+4)%8))%8);
 weglBindAttribLocationARB(programObj,*index,name);
}; break;
case 5637: { // glGetActiveAttribARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLuint *index = (GLuint *) bp; bp += 4;
 GLsizei *maxLength = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLint size[1] = {0};
 GLenum type[1] = {0};
 GLchar *name;
 name = (GLchar *) driver_alloc(sizeof(GLchar) * *maxLength);
 weglGetActiveAttribARB(programObj,*index,*maxLength,length,size,type,name);
 int AP = 0; ErlDrvTermData rt[13];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *size;
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *type;
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) name; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 3;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 13 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,13);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(name);
}; break;
case 5638: { // glGetAttribLocationARB
 GLhandleARB programObj = (GLhandleARB) * (GLuint64EXT *) bp; bp += 8;
 GLchar *name = (GLchar *) bp;
 int nameLen = strlen((char *)name); bp += nameLen+1+((8-((1+nameLen+0)%8))%8);
 GLint result = weglGetAttribLocationARB(programObj,name);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5639: { // glIsRenderbuffer
 GLuint *renderbuffer = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsRenderbuffer(*renderbuffer);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5640: { // glBindRenderbuffer
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *renderbuffer = (GLuint *) bp; bp += 4;
 weglBindRenderbuffer(*target,*renderbuffer);
}; break;
case 5641: { // glDeleteRenderbuffers
 int * renderbuffersLen = (int *) bp; bp += 4;
 GLuint * renderbuffers = (GLuint *) bp;  bp += (8-((*renderbuffersLen*4+4)%8))%8;
 weglDeleteRenderbuffers(*renderbuffersLen,renderbuffers);
}; break;
case 5642: { // glGenRenderbuffers
 GLsizei *n = (GLsizei *) bp; bp += 4;
 GLuint *renderbuffers;
 renderbuffers = (GLuint *) driver_alloc(sizeof(GLuint) * *n);
 weglGenRenderbuffers(*n,renderbuffers);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*n)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *n; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) renderbuffers[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*n)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*n)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*n)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(renderbuffers);
}; break;
case 5643: { // glRenderbufferStorage
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 weglRenderbufferStorage(*target,*internalformat,*width,*height);
}; break;
case 5644: { // glGetRenderbufferParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetRenderbufferParameteriv(*target,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5645: { // glIsFramebuffer
 GLuint *framebuffer = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsFramebuffer(*framebuffer);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5646: { // glBindFramebuffer
 GLenum *target = (GLenum *) bp; bp += 4;
 GLuint *framebuffer = (GLuint *) bp; bp += 4;
 weglBindFramebuffer(*target,*framebuffer);
}; break;
case 5647: { // glDeleteFramebuffers
 int * framebuffersLen = (int *) bp; bp += 4;
 GLuint * framebuffers = (GLuint *) bp;  bp += (8-((*framebuffersLen*4+4)%8))%8;
 weglDeleteFramebuffers(*framebuffersLen,framebuffers);
}; break;
case 5648: { // glGenFramebuffers
 GLsizei *n = (GLsizei *) bp; bp += 4;
 GLuint *framebuffers;
 framebuffers = (GLuint *) driver_alloc(sizeof(GLuint) * *n);
 weglGenFramebuffers(*n,framebuffers);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*n)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *n; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) framebuffers[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*n)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*n)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*n)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(framebuffers);
}; break;
case 5649: { // glCheckFramebufferStatus
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum result = weglCheckFramebufferStatus(*target);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5650: { // glFramebufferTexture1D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *attachment = (GLenum *) bp; bp += 4;
 GLenum *textarget = (GLenum *) bp; bp += 4;
 GLuint *texture = (GLuint *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 weglFramebufferTexture1D(*target,*attachment,*textarget,*texture,*level);
}; break;
case 5651: { // glFramebufferTexture2D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *attachment = (GLenum *) bp; bp += 4;
 GLenum *textarget = (GLenum *) bp; bp += 4;
 GLuint *texture = (GLuint *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 weglFramebufferTexture2D(*target,*attachment,*textarget,*texture,*level);
}; break;
case 5652: { // glFramebufferTexture3D
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *attachment = (GLenum *) bp; bp += 4;
 GLenum *textarget = (GLenum *) bp; bp += 4;
 GLuint *texture = (GLuint *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *zoffset = (GLint *) bp; bp += 4;
 weglFramebufferTexture3D(*target,*attachment,*textarget,*texture,*level,*zoffset);
}; break;
case 5653: { // glFramebufferRenderbuffer
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *attachment = (GLenum *) bp; bp += 4;
 GLenum *renderbuffertarget = (GLenum *) bp; bp += 4;
 GLuint *renderbuffer = (GLuint *) bp; bp += 4;
 weglFramebufferRenderbuffer(*target,*attachment,*renderbuffertarget,*renderbuffer);
}; break;
case 5654: { // glGetFramebufferAttachmentParameteriv
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *attachment = (GLenum *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint params[1] = {0};
 weglGetFramebufferAttachmentParameteriv(*target,*attachment,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *params;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5655: { // glGenerateMipmap
 GLenum *target = (GLenum *) bp; bp += 4;
 weglGenerateMipmap(*target);
}; break;
case 5656: { // glBlitFramebuffer
 GLint *srcX0 = (GLint *) bp; bp += 4;
 GLint *srcY0 = (GLint *) bp; bp += 4;
 GLint *srcX1 = (GLint *) bp; bp += 4;
 GLint *srcY1 = (GLint *) bp; bp += 4;
 GLint *dstX0 = (GLint *) bp; bp += 4;
 GLint *dstY0 = (GLint *) bp; bp += 4;
 GLint *dstX1 = (GLint *) bp; bp += 4;
 GLint *dstY1 = (GLint *) bp; bp += 4;
 GLbitfield *mask = (GLbitfield *) bp; bp += 4;
 GLenum *filter = (GLenum *) bp; bp += 4;
 weglBlitFramebuffer(*srcX0,*srcY0,*srcX1,*srcY1,*dstX0,*dstY0,*dstX1,*dstY1,*mask,*filter);
}; break;
case 5657: { // glRenderbufferStorageMultisample
 GLenum *target = (GLenum *) bp; bp += 4;
 GLsizei *samples = (GLsizei *) bp; bp += 4;
 GLenum *internalformat = (GLenum *) bp; bp += 4;
 GLsizei *width = (GLsizei *) bp; bp += 4;
 GLsizei *height = (GLsizei *) bp; bp += 4;
 weglRenderbufferStorageMultisample(*target,*samples,*internalformat,*width,*height);
}; break;
case 5658: { // glFramebufferTextureLayer
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *attachment = (GLenum *) bp; bp += 4;
 GLuint *texture = (GLuint *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLint *layer = (GLint *) bp; bp += 4;
 weglFramebufferTextureLayer(*target,*attachment,*texture,*level,*layer);
}; break;
case 5659: { // glProgramParameteriARB
 GLuint *program = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *value = (GLint *) bp; bp += 4;
 weglProgramParameteriARB(*program,*pname,*value);
}; break;
case 5660: { // glFramebufferTextureARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *attachment = (GLenum *) bp; bp += 4;
 GLuint *texture = (GLuint *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 weglFramebufferTextureARB(*target,*attachment,*texture,*level);
}; break;
case 5661: { // glFramebufferTextureFaceARB
 GLenum *target = (GLenum *) bp; bp += 4;
 GLenum *attachment = (GLenum *) bp; bp += 4;
 GLuint *texture = (GLuint *) bp; bp += 4;
 GLint *level = (GLint *) bp; bp += 4;
 GLenum *face = (GLenum *) bp; bp += 4;
 weglFramebufferTextureFaceARB(*target,*attachment,*texture,*level,*face);
}; break;
case 5662: { // glVertexAttribDivisorARB
 GLuint *index = (GLuint *) bp; bp += 4;
 GLuint *divisor = (GLuint *) bp; bp += 4;
 weglVertexAttribDivisorARB(*index,*divisor);
}; break;
case 5663: { // glFlushMappedBufferRange
 GLenum *target = (GLenum *) bp; bp += 4;
 bp += 4;
 GLintptr offset = (GLintptr) * (GLuint64EXT *) bp; bp += 8;
 GLsizeiptr length = (GLsizeiptr) * (GLuint64EXT *) bp; bp += 8;
 weglFlushMappedBufferRange(*target,offset,length);
}; break;
case 5664: { // glBindVertexArray
 GLuint *array = (GLuint *) bp; bp += 4;
 weglBindVertexArray(*array);
}; break;
case 5665: { // glDeleteVertexArrays
 int * arraysLen = (int *) bp; bp += 4;
 GLuint * arrays = (GLuint *) bp;  bp += (8-((*arraysLen*4+4)%8))%8;
 weglDeleteVertexArrays(*arraysLen,arrays);
}; break;
case 5666: { // glGenVertexArrays
 GLsizei *n = (GLsizei *) bp; bp += 4;
 GLuint *arrays;
 arrays = (GLuint *) driver_alloc(sizeof(GLuint) * *n);
 weglGenVertexArrays(*n,arrays);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*n)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *n; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) arrays[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*n)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*n)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*n)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(arrays);
}; break;
case 5667: { // glIsVertexArray
 GLuint *array = (GLuint *) bp; bp += 4;
 GLboolean result = weglIsVertexArray(*array);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5668: { // glGetUniformIndices
 GLuint *program = (GLuint *) bp; bp += 4;
 int * uniformNamesLen = (int *) bp; bp += 4;
 int * uniformNamesTotSize = (int *) bp; bp += 4;
 GLchar **uniformNames;
 uniformNames = (GLchar **) driver_alloc(sizeof(GLchar *) * *uniformNamesLen);
 for(int i=0;i<*uniformNamesLen;i++) {
    uniformNames[i] = (GLchar *) bp; bp += 1+strlen(bp);};
 bp += (8 - ((0 + *uniformNamesTotSize) % 8)) % 8;
 GLuint *uniformIndices;
 uniformIndices = (GLuint *) driver_alloc(sizeof(GLuint) * *uniformNamesLen);
 weglGetUniformIndices(*program,*uniformNamesLen,(const GLchar **) uniformNames,uniformIndices);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*uniformNamesLen)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *uniformNamesLen; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) uniformIndices[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*uniformNamesLen)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*uniformNamesLen)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*uniformNamesLen)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(uniformIndices);
 driver_free(uniformNames);
}; break;
case 5669: { // glGetActiveUniformsiv
 GLuint *program = (GLuint *) bp; bp += 4;
 int * uniformIndicesLen = (int *) bp; bp += 4;
 GLuint * uniformIndices = (GLuint *) bp;  bp += (8-((*uniformIndicesLen*4+0)%8))%8;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *params;
 params = (GLint *) driver_alloc(sizeof(GLint) * *uniformIndicesLen);
 weglGetActiveUniformsiv(*program,*uniformIndicesLen,uniformIndices,*pname,params);
 int AP = 0; ErlDrvTermData *rt;
 rt = (ErlDrvTermData *) driver_alloc(sizeof(ErlDrvTermData)*(7 + (*uniformIndicesLen)*2));
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 for(int i=0; i < *uniformIndicesLen; i++) {
    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) params[i];}
 rt[AP++] = ERL_DRV_NIL; rt[AP++] = ERL_DRV_LIST; rt[AP++] = (*uniformIndicesLen)+1;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 + (*uniformIndicesLen)*2 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7 + (*uniformIndicesLen)*2);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(rt);
 driver_free(params);
}; break;
case 5670: { // glGetActiveUniformName
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *uniformIndex = (GLuint *) bp; bp += 4;
 GLsizei *bufSize = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLchar *uniformName;
 uniformName = (GLchar *) driver_alloc(sizeof(GLchar) * *bufSize);
 weglGetActiveUniformName(*program,*uniformIndex,*bufSize,length,uniformName);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) uniformName; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(uniformName);
}; break;
case 5671: { // glGetUniformBlockIndex
 GLuint *program = (GLuint *) bp; bp += 4;
 GLchar *uniformBlockName = (GLchar *) bp;
 int uniformBlockNameLen = strlen((char *)uniformBlockName); bp += uniformBlockNameLen+1+((8-((1+uniformBlockNameLen+4)%8))%8);
 GLuint result = weglGetUniformBlockIndex(*program,uniformBlockName);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) result;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 6 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,6);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5672: { // glGetActiveUniformBlockiv
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *uniformBlockIndex = (GLuint *) bp; bp += 4;
 GLenum *pname = (GLenum *) bp; bp += 4;
 GLint *params = (GLint *) bins[0]->base;
 weglGetActiveUniformBlockiv(*program,*uniformBlockIndex,*pname,params);
 int AP = 0; ErlDrvTermData rt[6];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "ok");
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
}; break;
case 5673: { // glGetActiveUniformBlockName
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *uniformBlockIndex = (GLuint *) bp; bp += 4;
 GLsizei *bufSize = (GLsizei *) bp; bp += 4;
 GLsizei length[1] = {0};
 GLchar *uniformBlockName;
 uniformBlockName = (GLchar *) driver_alloc(sizeof(GLchar) * *bufSize);
 weglGetActiveUniformBlockName(*program,*uniformBlockIndex,*bufSize,length,uniformBlockName);
 int AP = 0; ErlDrvTermData rt[7];
 rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) "_wxe_result_");
 rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) uniformBlockName; rt[AP++] = *length;
 rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;
 if (AP != 7 )  fprintf(stderr, "%d: ERROR AP mismatch %d %d\r\n",__LINE__,AP,7);
 driver_send_term(WXE_DRV_PORT,caller,rt,AP);
 driver_free(uniformBlockName);
}; break;
case 5674: { // glUniformBlockBinding
 GLuint *program = (GLuint *) bp; bp += 4;
 GLuint *uniformBlockIndex = (GLuint *) bp; bp += 4;
 GLuint *uniformBlockBinding = (GLuint *) bp; bp += 4;
 weglUniformBlockBinding(*program,*uniformBlockIndex,*uniformBlockBinding);
}; break;
case 5675: { // glCopyBufferSubData
 GLenum *readTarget = (GLenum *) bp; bp += 4;
 GLenum *writeTarget = (GLenum *) bp; bp += 4;
 GLintptr readOffset = (GLintptr) * (GLuint64EXT *) bp; bp += 8;
 GLintptr writeOffset = (GLintptr) * (GLuint64EXT *) bp; bp += 8;
 GLsizeiptr size = (GLsizeiptr) * (GLuint64EXT *) bp; bp += 8;
 weglCopyBufferSubData(*readTarget,*writeTarget,readOffset,writeOffset,size);
}; break;
case 5676: { // glResizeBuffersMESA
 weglResizeBuffersMESA();
}; break;
case 5677: { // glWindowPos4dvMESA
 GLdouble *v = (GLdouble *) bp; bp += 8;
 weglWindowPos4dvMESA(v);
}; break;
case 5678: { // glWindowPos4fvMESA
 GLfloat *v = (GLfloat *) bp; bp += 4;
 weglWindowPos4fvMESA(v);
}; break;
case 5679: { // glWindowPos4ivMESA
 GLint *v = (GLint *) bp; bp += 4;
 weglWindowPos4ivMESA(v);
}; break;
case 5680: { // glWindowPos4svMESA
 GLshort *v = (GLshort *) bp; bp += 2;
 weglWindowPos4svMESA(v);
}; break;
case 5681: { // glDepthBoundsEXT
 GLclampd *zmin = (GLclampd *) bp; bp += 8;
 GLclampd *zmax = (GLclampd *) bp; bp += 8;
 weglDepthBoundsEXT(*zmin,*zmax);
}; break;
case 5682: { // glStencilClearTagEXT
 GLsizei *stencilTagBits = (GLsizei *) bp; bp += 4;
 GLuint *stencilClearTag = (GLuint *) bp; bp += 4;
 weglStencilClearTagEXT(*stencilTagBits,*stencilClearTag);
}; break;
}} /* The End */

