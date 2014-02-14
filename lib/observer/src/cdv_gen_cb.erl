%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
       {"Funs",num_fun}]}].
