%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2013. All Rights Reserved.
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

-module(alloc_SUITE).
-author('rickard.green@uab.ericsson.se').
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([basic/1,
	 coalesce/1,
	 threads/1,
	 realloc_copy/1,
	 bucket_index/1,
	 bucket_mask/1,
	 rbtree/1,
	 mseg_clear_cache/1,
	 erts_mmap/1,
	 cpool/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("test_server/include/test_server.hrl").

-define(DEFAULT_TIMETRAP_SECS, 240).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, coalesce, threads, realloc_copy, bucket_index,
     bucket_mask, rbtree, mseg_clear_cache, erts_mmap, cpool].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



init_per_testcase(Case, Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:seconds(?DEFAULT_TIMETRAP_SECS)),
    [{watchdog, Dog},{testcase, Case}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Testcases                                                              %%
%%                                                                        %%

basic(suite) -> [];
basic(doc) ->   [];
basic(Cfg) -> ?line drv_case(Cfg).

coalesce(suite) -> [];
coalesce(doc) ->   [];
coalesce(Cfg) -> ?line drv_case(Cfg).

threads(suite) -> [];
threads(doc) ->   [];
threads(Cfg) -> ?line drv_case(Cfg).

realloc_copy(suite) -> [];
realloc_copy(doc) ->   [];
realloc_copy(Cfg) -> ?line drv_case(Cfg).

bucket_index(suite) -> [];
bucket_index(doc) ->   [];
bucket_index(Cfg) -> ?line drv_case(Cfg).

bucket_mask(suite) -> [];
bucket_mask(doc) ->   [];
bucket_mask(Cfg) -> ?line drv_case(Cfg).

rbtree(suite) -> [];
rbtree(doc) ->   [];
rbtree(Cfg) -> ?line drv_case(Cfg).

mseg_clear_cache(suite) -> [];
mseg_clear_cache(doc) ->   [];
mseg_clear_cache(Cfg) -> ?line drv_case(Cfg).

cpool(suite) -> [];
cpool(doc) ->   [];
cpool(Cfg) -> ?line drv_case(Cfg).

erts_mmap(Config) when is_list(Config) ->
    case ?t:os_type() of
	{unix, _} ->
	    [erts_mmap_do(Config, SCO, SCRPM, SCRFSD)
	     || SCO <-[true,false], SCRFSD <-[1234,0], SCRPM <- [true,false]];
	{SkipOs,_} ->
	    ?line {skipped,
		   lists:flatten(["Not run on "
				  | io_lib:format("~p",[SkipOs])])}
    end.


erts_mmap_do(Config, SCO, SCRPM, SCRFSD) ->
    %% We use the number of schedulers + 1 * approx main carriers size
    %% to calculate how large the super carrier has to be
    %% and then use a minimum of 100 for systems with a low amount of
    %% schedulers
    Schldr = erlang:system_info(schedulers_online)+1,
    SCS = max(round((262144 * 6 + 3 * 1048576) * Schldr / 1024 / 1024),100),
    O1 = "+MMscs" ++ integer_to_list(SCS)
	++ " +MMsco" ++ atom_to_list(SCO)
	++ " +MMscrpm" ++ atom_to_list(SCRPM),
    Opts = case SCRFSD of
	       0 -> O1;
	       _ -> O1 ++ " +MMscrfsd"++integer_to_list(SCRFSD)
	   end,
    {ok, Node} = start_node(Config, Opts),
    Self = self(),
    Ref = make_ref(),
    F = fun () ->
		SI = erlang:system_info({allocator,mseg_alloc}),
		{erts_mmap,EM} = lists:keyfind(erts_mmap, 1, SI),
		{supercarrier,SC} = lists:keyfind(supercarrier, 1, EM),
		{sizes,Sizes} = lists:keyfind(sizes, 1, SC),
		{free_segs,Segs} = lists:keyfind(free_segs,1,SC),
		{total,Total} = lists:keyfind(total,1,Sizes),
		Total = SCS*1024*1024,

		{reserved,Reserved} = lists:keyfind(reserved,1,Segs),
		true = (Reserved >= SCRFSD),

		case {SCO,lists:keyfind(os,1,EM)} of
		    {true, false} -> ok;
		    {false, {os,_}} -> ok
		end,

		Self ! {Ref, ok}
	end,

    spawn_link(Node, F),
    Result = receive {Ref, Rslt} -> Rslt end,
    stop_node(Node),
    Result.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Internal functions                                                     %%
%%                                                                        %%

drv_case(Config) ->
    drv_case(Config, "").

drv_case(Config, Command) when is_list(Config),
			       is_list(Command) ->
    case ?t:os_type() of
	{Family, _} when Family == unix; Family == win32 ->
	    ?line {ok, Node} = start_node(Config),
	    ?line Self = self(),
	    ?line Ref = make_ref(),
	    ?line spawn_link(Node,
			     fun () ->
				     Res = run_drv_case(Config, Command),
				     Self ! {Ref, Res}
			     end),
	    ?line Result = receive {Ref, Rslt} -> Rslt end,
	    ?line stop_node(Node),
	    ?line Result;
	SkipOs ->
	    ?line {skipped,
		   lists:flatten(["Not run on "
				  | io_lib:format("~p",[SkipOs])])}
    end.

run_drv_case(Config, Command) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line CaseName = ?config(testcase,Config),
    case erl_ddll:load_driver(DataDir, CaseName) of
	ok -> ok;
	{error, Error} ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    ?line ?t:fail()
    end,
    ?line Port = open_port({spawn, atom_to_list(CaseName)}, []),
    ?line true = is_port(Port),
    ?line Port ! {self(), {command, Command}},
    ?line Result = receive_drv_result(Port, CaseName),
    ?line Port ! {self(), close},
    ?line receive 
	      {Port, closed} ->
		  ok
	  end,
    ?line ok = erl_ddll:unload_driver(CaseName),
    ?line Result.

receive_drv_result(Port, CaseName) ->
    ?line receive
	      {print, Port, CaseName, Str} ->
		  ?line ?t:format("~s", [Str]),
		  ?line receive_drv_result(Port, CaseName);
	      {'EXIT', Port, Error} ->
		  ?line ?t:fail(Error);
	      {'EXIT', error, Error} ->
		  ?line ?t:fail(Error);
	      {failed, Port, CaseName, Comment} ->
		  ?line ?t:fail(Comment);
	      {skipped, Port, CaseName, Comment} ->
		  ?line {skipped, Comment};
	      {succeeded, Port, CaseName, ""} ->
		  ?line succeeded;
	      {succeeded, Port, CaseName, Comment} ->
		  ?line {comment, Comment}
	  end.

start_node(Config) ->
    start_node(Config, []).
start_node(Config, Opts) when is_list(Config), is_list(Opts) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(?config(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(seconds))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    ?t:start_node(Name, slave, [{args, Opts++" -pa "++Pa}]).

stop_node(Node) ->
    ?t:stop_node(Node).
