%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : Tests the safe_fixtable functions in both ets and dets.
%%%----------------------------------------------------------------------

-module(fixtable_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
%%% Test cases
-export([multiple_fixes/1, multiple_processes/1,
	 other_process_deletes/1, owner_dies/1,
	 other_process_closes/1,insert_same_key/1]).
-export([fixbag/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
%%% Internal exports
-export([command_loop/0,start_commander/0]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [multiple_fixes, multiple_processes,
     other_process_deletes, owner_dies, other_process_closes,
     insert_same_key, fixbag].

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


-include_lib("common_test/include/ct.hrl").

%%% I wrote this thinking I would use more than one temporary at a time, but 
%%% I wasn't... Well, maybe in the future...
-define(DETS_TEMPORARIES, [tmp1]).
-define(ETS_TEMPORARIES, [gurksmetsmedaljong]).
-define(DETS_TMP1,hd(?DETS_TEMPORARIES)).
-define(ETS_TMP1,hd(?ETS_TEMPORARIES)).

-define(HELPER_NODE, (atom_to_list(?MODULE) ++ "_helper1")).

init_per_testcase(_Func, Config) ->
    PrivDir = proplists:get_value(priv_dir,Config),
    file:make_dir(PrivDir),
    Config.

end_per_testcase(_Func, Config) ->
    lists:foreach(fun(X) ->
			  (catch dets:close(X)),
			  (catch file:delete(dets_filename(X,Config)))
		  end,
		  ?DETS_TEMPORARIES),
    lists:foreach(fun(X) ->
			  (catch ets:delete(X))
		  end,
		  ?ETS_TEMPORARIES).


-ifdef(DEBUG).
-define(LOG(X), show(X,?LINE)).

show(Term, Line) ->
    io:format("~p: ~p~n", [Line,Term]),
    Term.
-else.
-define(LOG(X),X).
-endif.


%% Check for bug OTP-5087; safe_fixtable for bags could give incorrect
%% lookups.
fixbag(Config) when is_list(Config) ->
    T = ets:new(x,[bag]),
    ets:insert(T,{a,1}),
    ets:insert(T,{a,2}),
    ets:safe_fixtable(T,true),
    ets:match_delete(T,{a,2}),
    ets:insert(T,{a,3}),
    Res = ets:lookup(T,a),
    ets:safe_fixtable(T,false),
    Res = ets:lookup(T,a),
    ok.



%% Check correct behaviour if a key is deleted and reinserted during
%% fixation.
insert_same_key(Config) when is_list(Config) ->
    {ok,Dets1} = dets:open_file(?DETS_TMP1,
				[{file, dets_filename(?DETS_TMP1,Config)}]),
    Ets1 = ets:new(ets,[]),
    insert_same_key(Dets1,dets,Config),
    insert_same_key(Ets1,ets,Config),
    ets:insert(Ets1,{1,2}),
    1 = ets:info(Ets1,size),
    dets:insert(Dets1,{1,2}),
    1 = dets:info(Dets1,size),
    dets:close(Dets1),
    (catch file:delete(dets_filename(Dets1,Config))),
    ets:delete(Ets1),
    {ok,Dets2} = dets:open_file(?DETS_TMP1,
				[{type,bag},{file, dets_filename(?DETS_TMP1,Config)}]),
    Ets2 = ets:new(ets,[bag]),
    insert_same_key(Dets2,dets,Config),
    insert_same_key(Ets2,ets,Config),
    ets:insert(Ets2,{1,2}),
    2 = ets:info(Ets2,size),
    ets:insert(Ets2,{1,2}),
    2 = ets:info(Ets2,size),
    dets:insert(Dets2,{1,2}),
    2 = dets:info(Dets2,size),
    dets:insert(Dets2,{1,2}),
    2 = dets:info(Dets2,size),
    dets:close(Dets2),
    (catch file:delete(dets_filename(Dets2,Config))),
    ets:delete(Ets2),
    {ok,Dets3} = dets:open_file(?DETS_TMP1,
				[{type,duplicate_bag},
				 {file, dets_filename(?DETS_TMP1,Config)}]),
    Ets3 = ets:new(ets,[duplicate_bag]),
    insert_same_key(Dets3,dets,Config),
    insert_same_key(Ets3,ets,Config),
    ets:insert(Ets3,{1,2}),
    2 = ets:info(Ets3,size),
    ets:insert(Ets3,{1,2}),
    3 = ets:info(Ets3,size),
    dets:insert(Dets3,{1,2}),
    2 = dets:info(Dets3,size),
    dets:insert(Dets3,{1,2}),
    3 = dets:info(Dets3,size),
    dets:close(Dets3),
    (catch file:delete(dets_filename(Dets3,Config))),
    ets:delete(Ets3),
    ok.

insert_same_key(Tab,Mod,_Config) ->
    Mod:insert(Tab,{1,1}),
    Mod:insert(Tab,{1,2}),
    Mod:insert(Tab,{2,2}),
    Mod:insert(Tab,{2,2}),
    Mod:safe_fixtable(Tab,true),
    Mod:delete(Tab,1),
    Mod:insert(Tab,{1,1}),
    Expect = case Mod:info(Tab,type) of
		 bag ->
		     Mod:insert(Tab,{1,2}),
		     2;
		 _ ->
		     1
	     end,
    Mod:delete(Tab,2),
    Mod:safe_fixtable(Tab,false),
    case Mod:info(Tab,size) of
	Expect ->
	    ok;
	_ ->
	    exit({size_field_wrong,{Mod,Mod:info(Tab)}})
    end.




%% Check correct behaviour if the table owner dies.
owner_dies(Config) when is_list(Config) ->
    P1 = start_commander(),
    Ets1 = command(P1,{ets,new,[ets,[]]}),
    command(P1,{ets,safe_fixtable,[Ets1,true]}),
    {_,[{P1,1}]} = ets:info(Ets1, safe_fixed),
    stop_commander(P1),
    undefined = ets:info(Ets1, safe_fixed),
    P2 = start_commander(),
    Ets2 = command(P2,{ets,new,[ets,[public]]}),
    command(P2,{ets,safe_fixtable,[Ets2,true]}),
    ets:safe_fixtable(Ets2,true),
    true = ets:info(Ets2, fixed),
    {_,[{_,1},{_,1}]} = ets:info(Ets2, safe_fixed),
    stop_commander(P2),
    undefined = ets:info(Ets2, safe_fixed),
    undefined = ets:info(Ets2, fixed),
    P3 = start_commander(),
    {ok,Dets} = ?LOG(command(P3, {dets, open_file,
				  [?DETS_TMP1,
				   [{file,
				     dets_filename(?DETS_TMP1,
						   Config)}]]})),
    command(P3, {dets, safe_fixtable, [Dets, true]}),
    {_,[{P3,1}]} = dets:info(Dets, safe_fixed),
    true = dets:info(Dets, fixed),
    stop_commander(P3),
    undefined = dets:info(Dets, safe_fixed),
    undefined = dets:info(Dets, fixed),
    P4 = start_commander(),
    {ok,Dets} = command(P4, {dets, open_file,
			     [?DETS_TMP1,
			      [{file, dets_filename(?DETS_TMP1,Config)}]]}),
    {ok,Dets} = dets:open_file(?DETS_TMP1,
			       [{file, dets_filename(?DETS_TMP1,Config)}]),
    false = dets:info(Dets, safe_fixed),
    command(P4, {dets, safe_fixtable, [Dets, true]}),
    dets:safe_fixtable(Dets, true),
    {_,[{_,1},{_,1}]} = dets:info(Dets, safe_fixed),
    dets:safe_fixtable(Dets, true),
    stop_commander(P4),
    S = self(),
    {_,[{S,2}]} = dets:info(Dets, safe_fixed),
    true = dets:info(Dets, fixed),
    dets:close(Dets),
    undefined = dets:info(Dets, fixed),
    undefined = dets:info(Dets, safe_fixed),
    ok.


%% When another process closes an dets table, different things should
%% happen depending on if it has opened it before.
other_process_closes(Config) when is_list(Config) ->
    {ok,Dets} = dets:open_file(?DETS_TMP1,
			       [{file, dets_filename(tmp1,Config)}]),
    P2 = start_commander(),
    dets:safe_fixtable(Dets,true),
    S = self(),
    {_,[{S,1}]} = dets:info(Dets, safe_fixed),
    command(P2,{dets, safe_fixtable, [Dets, true]}),
    {_,[_,_]} = dets:info(Dets, safe_fixed),
    {error, not_owner} = command(P2,{dets, close, [Dets]}),
    {_,[_,_]} = dets:info(Dets, safe_fixed),
    command(P2,{dets, open_file,[?DETS_TMP1,
				 [{file, 
				   dets_filename(?DETS_TMP1, Config)}]]}),
    {_,[_,_]} = dets:info(Dets, safe_fixed),
    command(P2,{dets, close, [Dets]}),
    stop_commander(P2),
    {_,[{S,1}]} = dets:info(Dets, safe_fixed),
    true = dets:info(Dets,fixed),
    dets:close(Dets),
    undefined = dets:info(Dets,fixed),
    undefined = dets:info(Dets, safe_fixed),
    ok.

%% Check that fixtable structures are cleaned up if another process
%% deletes an ets table.
other_process_deletes(Config) when is_list(Config) ->
    Ets = ets:new(ets,[public]),
    P = start_commander(),
    ets:safe_fixtable(Ets,true),
    ets:safe_fixtable(Ets,true),
    true = ets:info(Ets, fixed),
    {_,_} = ets:info(Ets, safe_fixed),
    command(P,{ets,delete,[Ets]}),
    stop_commander(P),
    undefined = ets:info(Ets, fixed),
    undefined = ets:info(Ets, safe_fixed),
    ok.

%% Check that multiple safe_fixtable keeps the reference counter.
multiple_fixes(Config) when is_list(Config) ->
    {ok,Dets} = dets:open_file(?DETS_TMP1,
			       [{file, dets_filename(?DETS_TMP1,Config)}]),
    Ets = ets:new(ets,[]),
    multiple_fixes(Dets,dets),
    multiple_fixes(Ets,ets),
    dets:close(Dets),
    ok.

multiple_fixes(Tab, Mod) ->
    false = Mod:info(Tab,fixed),
    false = Mod:info(Tab, safe_fixed),
    Mod:safe_fixtable(Tab, true),
    true = Mod:info(Tab,fixed),
    S = self(),
    {_,[{S,1}]} = Mod:info(Tab, safe_fixed),
    Mod:safe_fixtable(Tab, true),
    Mod:safe_fixtable(Tab, true),
    {_,[{S,3}]} = Mod:info(Tab, safe_fixed),
    true = Mod:info(Tab,fixed),
    Mod:safe_fixtable(Tab, false),
    {_,[{S,2}]} = Mod:info(Tab, safe_fixed),
    true = Mod:info(Tab,fixed),
    Mod:safe_fixtable(Tab, false),
    {_,[{S,1}]} = Mod:info(Tab, safe_fixed),
    true = Mod:info(Tab,fixed),
    Mod:safe_fixtable(Tab, false),
    false = Mod:info(Tab, safe_fixed),
    false = Mod:info(Tab,fixed).

%% Check that multiple safe_fixtable across processes are reference
%% counted OK.
multiple_processes(Config) when is_list(Config) ->
    {ok,Dets} = dets:open_file(?DETS_TMP1,[{file,
					    dets_filename(?DETS_TMP1,
							  Config)}]),
    Ets = ets:new(ets,[public]),
    multiple_processes(Dets,dets),
    multiple_processes(Ets,ets),
    ok.

multiple_processes(Tab, Mod) ->
    io:format("Mod = ~p\n", [Mod]),
    P1 = start_commander(),
    P2 = start_commander(),
    false = Mod:info(Tab,fixed),
    false = Mod:info(Tab, safe_fixed),
    command(P1, {Mod, safe_fixtable, [Tab,true]}),
    true = Mod:info(Tab,fixed),
    {_,[{P1,1}]} = Mod:info(Tab, safe_fixed),
    command(P2, {Mod, safe_fixtable, [Tab,true]}),
    true = Mod:info(Tab,fixed),
    {_,L} = Mod:info(Tab,safe_fixed),
    true = (lists:sort(L) == lists:sort([{P1,1},{P2,1}])),
    command(P2, {Mod, safe_fixtable, [Tab,true]}),
    {_,L2} = Mod:info(Tab,safe_fixed),
    true = (lists:sort(L2) == lists:sort([{P1,1},{P2,2}])),
    command(P2, {Mod, safe_fixtable, [Tab,false]}),
    true = Mod:info(Tab,fixed),
    {_,L3} = Mod:info(Tab,safe_fixed),
    true = (lists:sort(L3) == lists:sort([{P1,1},{P2,1}])),
    command(P2, {Mod, safe_fixtable, [Tab,false]}),
    true = Mod:info(Tab,fixed),
    {_,[{P1,1}]} = Mod:info(Tab, safe_fixed),
    stop_commander(P1),
    receive after 1000 -> ok end,
    false = Mod:info(Tab,fixed),
    false = Mod:info(Tab, safe_fixed),
    command(P2, {Mod, safe_fixtable, [Tab,true]}),
    true = Mod:info(Tab,fixed),
    {_,[{P2,1}]} = Mod:info(Tab, safe_fixed),
    case Mod of
	dets ->
	    dets:close(Tab);
	ets ->
	    ets:delete(Tab)
    end,
    stop_commander(P2),
    receive after 1000 -> ok end,
    undefined = Mod:info(Tab, safe_fixed),
    ok.



%%% Helpers
dets_filename(Base, Config) when is_atom(Base) ->
    dets_filename(atom_to_list(Base) ++ ".dat", Config);
dets_filename(Basename, Config) ->
    PrivDir = proplists:get_value(priv_dir,Config),
    filename:join(PrivDir, Basename).

command_loop() ->
    receive 
	{From, command, {M,F,A}} ->
	    Res = (catch apply(M, F, A)),
	    From ! {self(), Res},
	    command_loop();
	die ->
	    ok
    end.

start_commander() ->
    spawn(?MODULE, command_loop, []).

stop_commander(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    Pid ! die,
    receive
	{'EXIT',Pid,_} ->
            timer:sleep(1), % let other processes handle the signal as well
	    true
    after 5000 ->
	    exit(stop_timeout)
    end.

command(Pid,MFA) ->
    Pid ! {self(), command, MFA},
    receive
	{Pid, Res} ->
	    Res
    after 20000 ->
	    exit(command_timeout)
    end.



