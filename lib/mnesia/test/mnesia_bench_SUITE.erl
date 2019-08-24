%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
%%

%%
-module(mnesia_bench_SUITE).
-author('lukas@erix.ericsson.se').

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         suite/0, all/0, groups/0]).

-export([tpcb_conflict_ramcopies/1, tpcb_conflict_disk_only_copies/1]).

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


    


