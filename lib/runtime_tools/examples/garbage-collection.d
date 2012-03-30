/* example usage: dtrace -q -s /path/to/garbage-collection.d */
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

erlang*:::gc_major-start
{
    printf("GC major start pid %s need %d words\n", copyinstr(arg0), arg1);
}

erlang*:::gc_minor-start
{
    printf("GC minor start pid %s need %d words\n", copyinstr(arg0), arg1);
}

erlang*:::gc_major-end
{
    printf("GC major end pid %s reclaimed %d words\n", copyinstr(arg0), arg1);
}

erlang*:::gc_minor-start
{
    printf("GC minor end pid %s reclaimed %d words\n", copyinstr(arg0), arg1);
}
