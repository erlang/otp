/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
#ifndef _ERL_TIMEOUT_H
#define _ERL_TIMEOUT_H

#if !defined (__WIN32__) && !defined (VXWORKS)

#include <setjmp.h>

/*
  use timeout like this (delay in ms):

  if (timeout(delay,fun(a,b,c))) {
      printf("timeout occurred\n");
  }
  else {
      ... 
  }

If the call to fun() has not returned before 'delay' ms, it will be
interrupted and and timeout() will return a non-zero value.

If fun() finishes before 'delay' ms, then timeout will return 0.

If you need the return value from fun then assign it like this:

  if (timeout(delay,(x = fun(...)))) {
  }

These functions work by setting and catching SIGALRM, and although it
saves and restores the signal handler, it may not work in situations
where you are already using SIGALRM (this includes calls to sleep(3)).

Note that although recursive calls to timeout will not fail, they may
not give the expected results. All invocations of timeout use the same
timer, which is set on entrance to timeout and restored on exit from
timeout. So although an inner call to timeout will restart the timer
for any pending outer call when it exits, any time that has already
elapsed against the outer timeout is forgotten. In addition, the alarm
signal will always go to the innermost (last called) timeout, which
may or may not be the intention in recursive cases. 

*/

#define JMPVAL 997 /* magic */

#define timeout(ms,funcall) \
      (setjmp(*timeout_setup(ms)) == JMPVAL ? -1: \
       ((void)(funcall), timeout_cancel()))


/* don't call any of these directly - use the macro! see above! */   
jmp_buf *timeout_setup(int ms);
int timeout_cancel(void);

#endif /* WIN32 && VXWORKS */

#endif  /* _ERL_TIMEOUT_H */
