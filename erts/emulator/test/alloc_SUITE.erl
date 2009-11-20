%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

-module(alloc_SUITE).
-author('rickard.green@uab.ericsson.se').
-export([all/1]).

-export([basic/1,
	 coalesce/1,
	 threads/1,
	 realloc_copy/1,
	 bucket_index/1,
	 bucket_mask/1,
	 rbtree/1,
	 mseg_clear_cache/1]).

-export([init_per_testcase/2, fin_per_testcase/2]).

-include("test_server.hrl").

-define(DEFAULT_TIMETRAP_SECS, 240).

all(doc) -> [];
all(suite) -> [basic,
	       coalesce,
	       threads,
	       realloc_copy,
	       bucket_index,
	       bucket_mask,
	       rbtree,
	       mseg_clear_cache].


init_per_testcase(Case, Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:seconds(?DEFAULT_TIMETRAP_SECS)),
    [{watchdog, Dog},{testcase, Case}|Config].

fin_per_testcase(_Case, Config) when is_list(Config) ->
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

start_node(Config) when is_list(Config) ->
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line {A, B, C} = now(),
    ?line Name = list_to_atom(atom_to_list(?MODULE)
			      ++ "-"
			      ++ atom_to_list(?config(testcase, Config))
			      ++ "-"
			      ++ integer_to_list(A)
			      ++ "-"
			      ++ integer_to_list(B)
			      ++ "-"
			      ++ integer_to_list(C)),
    ?line ?t:start_node(Name, slave, [{args, "-pa "++Pa}]).

stop_node(Node) ->
    ?t:stop_node(Node).
