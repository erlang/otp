/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

/* 
 * External API for the windows console (aka werl window)
 * used by ttsl_drv.c
 */
#ifndef _WIN_CON_H_VISITED
#define _WIN_CON_H_VISITED 1
void ConNormalExit(void);
void ConWaitForExit(void);
void ConSetCtrlHandler(BOOL (WINAPI *handler)(DWORD));
int ConPutChar(Uint32 c);
void ConSetCursor(int from, int to);
void ConPrintf(char *format, ...);
void ConVprintf(char *format, va_list va);
void ConBeep(void);
int ConReadInput(Uint32 *data, int nbytes);
int ConGetKey(void);
int ConGetColumns(void);
int ConGetRows(void);
void ConInit(void);
#endif /* _WIN_CON_H_VISITED */
