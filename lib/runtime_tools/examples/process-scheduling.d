/* example usage: dtrace -q -s /path/to/process-scheduling.d */
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

erlang*:::process-scheduled
{
    printf("  Schedule pid %s mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::process-unscheduled
{
    printf("Unschedule pid %s\n", copyinstr(arg0));
}

erlang*:::process-hibernate
{
    printf("  Hibernate pid %s resume mfa %s\n",
           copyinstr(arg0), copyinstr(arg1));
}
