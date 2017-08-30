/* example usage: dtrace -q -s /path/to/memory1.d */
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

erlang*:::copy-struct
{
    printf("copy_struct %d bytes\n", arg0);
}

erlang*:::copy-object
{
    printf("copy_object pid %s %d bytes\n", copyinstr(arg0), arg1);
}

erlang*:::process-heap_grow
{
    printf("proc heap grow pid %s %d -> %d bytes\n", copyinstr(arg0),
	   arg1, arg2);
}

erlang*:::process-heap_shrink
{
    printf("proc heap shrink pid %s %d -> %d bytes\n", copyinstr(arg0),
	   arg1, arg2);
}
