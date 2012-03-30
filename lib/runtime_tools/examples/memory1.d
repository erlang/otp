/* example usage: dtrace -q -s /path/to/memory1.d */
/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2012. All Rights Reserved.
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
