/* example usage: dtrace -q -s /path/to/function-calls.d */
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

erlang*:::local-function-entry
{
    printf("pid %s enter (local)  %s depth %d\n",
	   copyinstr(arg0), copyinstr(arg1), arg2);
}

erlang*:::global-function-entry
{
    printf("pid %s enter (global)  %s depth %d\n",
	   copyinstr(arg0), copyinstr(arg1), arg2);
}

erlang*:::function-return
{
    printf("pid %s return %s depth %d\n",
	   copyinstr(arg0), copyinstr(arg1), arg2);
}

erlang*:::bif-entry
{
    printf("pid %s BIF entry  mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::bif-return
{
    printf("pid %s BIF return mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::nif-entry
{
    printf("pid %s NIF entry  mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::nif-return
{
    printf("pid %s NIF return mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}
