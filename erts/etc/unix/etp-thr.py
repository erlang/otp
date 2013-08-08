#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2013. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%
#

def get_thread_name(t):
    f = gdb.newest_frame();
    while f:
        if f.name() == "async_main":
            return "async";
        elif f.name() == "erts_sys_main_thread":
            return "main";
        elif f.name() == "signal_dispatcher_thread_func":
            return "signal_dispatcher";
        elif f.name() == "sys_msg_dispatcher_func":
            return "sys_msg_dispatcher";
        elif f.name() == "child_waiter":
            return "child_waiter";
        elif f.name() == "sched_thread_func":
            return "scheduler";
        elif f.name() == "aux_thread":
            return "aux";
        f = f.older();
    return "unknown";


curr_thread = gdb.selected_thread();

for i in gdb.inferiors():
    gdb.write(" Id   Thread Name           Frame\n");
    for t in i.threads():
        t.switch();
        if curr_thread == t:
            gdb.write("*");
        else:
            gdb.write(" ");
        gdb.write("{0:<3}  {1:20} {2}\n".format(
                t.num,get_thread_name(t),
                gdb.newest_frame().name()));
        
curr_thread.switch();
