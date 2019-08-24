/* example usage: dtrace -q -s /path/to/spawn-exit.d */
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

erlang*:::process-spawn
{
    printf("pid %s mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::process-exit
{
    printf("pid %s reason %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::process-exit_signal
{
    printf("sender %s -> pid %s reason %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2));
}

erlang*:::process-exit_signal-remote
{
    printf("sender %s -> node %s pid %s reason %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), copyinstr(arg3));
}
