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

#ifndef _WXE_GL_H
#define _WXE_GL_H

// #include "egl_impl.h"

void setActiveGL(wxeMemEnv *memenv, ErlNifPid caller, wxGLCanvas *canvas, wxGLContext *context);
void deleteActiveGL(wxGLCanvas *canvas);
void gl_dispatch(wxeCommand *);
extern "C" {
    void wxe_initOpenGL(void * fptr, void *name_fptr);
}


typedef struct _wxe_glc
{
    wxGLCanvas *canvas;
    wxGLContext *context;
} wxe_glc;


WX_DECLARE_HASH_MAP(ErlNifUInt64, wxe_glc*, wxIntegerHash, wxIntegerEqual, wxeGLC);
extern wxeGLC glc;

#endif
