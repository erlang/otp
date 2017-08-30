/* example usage: dtrace -q -s /path/to/user-probe.d */
/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2016. All Rights Reserved.
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

erlang*:::user_trace-n0
{
    printf("probe n0: %s %s %d %d %d %d '%s' '%s' '%s' '%s'\n",
           copyinstr(arg0),
           arg1 == NULL ? "" : copyinstr(arg1),
           arg2, arg3, arg4, arg5,
           arg6 == NULL ? "" : copyinstr(arg6),
           arg7 == NULL ? "" : copyinstr(arg7),
           arg8 == NULL ? "" : copyinstr(arg8),
           arg9 == NULL ? "" : copyinstr(arg9));
}

erlang*:::user_trace-n1
{
    printf("probe n1: %s %s %d %d %d %d '%s' '%s' '%s' '%s'\n",
           copyinstr(arg0),
           arg1 == NULL ? "" : copyinstr(arg1),
           arg2, arg3, arg4, arg5,
           arg6 == NULL ? "" : copyinstr(arg6),
           arg7 == NULL ? "" : copyinstr(arg7),
           arg8 == NULL ? "" : copyinstr(arg8),
           arg9 == NULL ? "" : copyinstr(arg9));
}

