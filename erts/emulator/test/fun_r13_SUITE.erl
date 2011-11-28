%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

-module(fun_r13_SUITE).
-compile(r13).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,dist_old_release/1]).

-define(default_timeout, ?t:minutes(1)).
-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [dist_old_release].

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


init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

dist_old_release(Config) when is_list(Config) ->
    case ?t:is_release_available("r12b") of
	true -> do_dist_old(Config);
	false -> {skip,"No R12B found"}
    end.

do_dist_old(Config) when is_list(Config) ->
    ?line Pa = filename:dirname(code:which(?MODULE)),
    Name = fun_dist_r12,
    ?line {ok,Node} = ?t:start_node(Name, peer,
				    [{args,"-pa "++Pa},
				     {erl,[{release,"r12b"}]}]),

    ?line Pid = spawn_link(Node,
			   fun() ->
				   receive
				       Fun when is_function(Fun) ->
					   R12BFun = fun(H) -> cons(H, [b,c]) end,
					   Fun(Fun, R12BFun)
				   end
			   end),
    Self = self(),
    Fun = fun(F, R12BFun) ->
		  {pid,Self} = erlang:fun_info(F, pid),
		  {module,?MODULE} = erlang:fun_info(F, module),
		  Self ! {ok,F,R12BFun}
	  end,
    ?line Pid ! Fun,
    ?line receive
	      {ok,Fun,R12BFun} ->
		  ?line [a,b,c] = R12BFun(a);
	      Other ->
		  ?line ?t:fail({bad_message,Other})
	  end,
    ok.

cons(H, T) ->
    [H|T].
