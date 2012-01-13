%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
%%

%%
-module(mnesia_bench_SUITE).
-author('lukas@erix.ericsson.se').
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]}].


all() -> 
    [{group,tpcb}].

groups() -> 
    [{tpcb,[{repeat,2}],[tpcb_conflict_ramcopies,
			 tpcb_conflict_disk_only_copies]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_Func, Conf) ->
    Conf.

end_per_testcase(_Func, _Conf) ->
    ok.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tpcb_conflict_ramcopies(_Config) ->
    mnesia_tpcb:conflict_benchmark(ram_copies).

tpcb_conflict_disk_only_copies(_Config) ->
    mnesia_tpcb:conflict_benchmark(disc_only_copies).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    


