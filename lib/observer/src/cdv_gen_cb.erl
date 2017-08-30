%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-module(cdv_gen_cb).

-export([get_info/0]).

-include("crashdump_viewer.hrl").

get_info() ->
    {ok,Info,TW} = crashdump_viewer:general_info(),
    Fields = info_fields(),
    Proplist =
	crashdump_viewer:to_proplist(record_info(fields,general_info),Info),
    {Fields,Proplist,TW}.

info_fields() ->
    [{"General Information",
      [{"Slogan",slogan},
       {"Node name",node_name},
       {"Crashdump created on",created},
       {"System version",system_vsn},
       {"Compiled",compile_time},
       {"Taints",taints},
       {"Memory allocated",{bytes,mem_tot}},
       {"Memory maximum",{bytes,mem_max}},
       {"Atoms",num_atoms},
       {"Processes",num_procs},
       {"ETS tables",num_ets},
       {"Timers",num_timers},
       {"Funs",num_fun},
       {"Calling Thread", thread}
      ]}].
