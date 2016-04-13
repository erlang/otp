/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
#ifndef _ELOG_PIPE_STDIN_H
#define _ELOG_PIPE_STDIN_H
/*
 * Module: elog_pipe_stdin
 * Purpouse: Read data from stdin when stdin is a pipe
 * and deliver events only when data is really availabel or 
 * end of file is reached.
 * If we would wait on an ordinary pipe handle, we 
 * would return immediately as it's always "signaled".
 * some kind of asyncronous I/O in the win32 way is 
 * not possible as it's not supported on anonymous pipes
 * (besides we have not opened the file ourselves so we 
 *  cannot specify that we want async I/O...).
 * ToDo: The reading is inneficcient, the buffering 
 * goes on forever, which would be dangerous if to much 
 * data was passed into stdin. The handling of 
 * Console stdin should be transparent instead of
 * forcing the user of the module to check if this is a 
 * console for selecting between ReadFile and 
 * read_pipe_stdin.
 * The handling of the event object is somewhat strange 
 * because I want to know about EOF before I've read
 * to it.
 */

BOOL peek_pipe_stdin_eof(void);
/*
 * Returns TRUE if eof is reached, regardless of 
 * if there still is unread data in the buffer.
 * Should not be called if console_stdin() returns TRUE.
 * Resets the event object if it returns TRUE.
 */

int read_pipe_stdin(char *buff, int max);
/*
 * Reads from stdin, minimum 1 byte and
 * maximum max bytes into buff. If EOF
 * is reached and no bytes were read, 
 * the return value is 0.
 * Should not be called if console_stdin() returns TRUE.
 * The event object for stdin will get unsignaled if
 * end of file is not reached (if peek_pipe_stdin_eof()
 * would return false).
 */

BOOL setup_pipe_stdin(void);
/*
 * Initializes the module, returns TRUE if OK.
 * If stdin is a console, no thread is created
 * and the event objet returned by get_Stdin_event 
 * will be the console handle.
 * Check if stdin was a console with the console_stdin() 
 * function.
 */

BOOL console_stdin(void);
/*
 * Returns true if stdin was a console, in which case
 * normal Win32 console I/O functions have to
 * be used. 
 * get_stdin_event() will return the console handle,
 * which is signalled whenever an event reaches
 * the console window (like mouse events etc).
 */

HANDLE get_stdin_event(void);
/*
 * Returns a event handle that can be waited upon with
 * WaitForSingleObject and friends. It is possibly a console
 * handle, see console_stdin().
 */
#endif /* _ELOG_PIPE_STDIN_H */
