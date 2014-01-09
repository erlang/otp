/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
#ifndef _ERLSRV_GLOBAL_H
#define _ERLSRV_GLOBAL_H

#define APP_NAME L"ErlSrv"

#define ERLANG_MACHINE L"erl.exe"

#define SERVICE_ENV L"ERLSRV_SERVICE_NAME"
#define EXECUTABLE_ENV L"ERLSRV_EXECUTABLE"
#define DEBUG_ENV L"ERLSRV_DEBUG"

#ifdef _DEBUG
#define HARDDEBUG 1
#define DEBUG 1
#else
#define NDEBUG 1
#endif

#endif /* _ERLSRV_GLOBAL_H */
