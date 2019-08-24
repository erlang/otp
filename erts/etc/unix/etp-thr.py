#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2013-2016. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
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
