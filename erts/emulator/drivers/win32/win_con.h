/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
