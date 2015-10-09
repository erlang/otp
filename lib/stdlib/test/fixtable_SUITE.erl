%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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

suite() -> [{ct_hooks,[ts_install_cth]}].

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


-include_lib("test_server/include/test_server.hrl").

%%% I wrote this thinking I would use more than one temporary at a time, but 
%%% I wasn't... Well, maybe in the future...
-define(DETS_TEMPORARIES, [tmp1]).
-define(ETS_TEMPORARIES, [gurksmetsmedaljong]).
-define(DETS_TMP1,hd(?DETS_TEMPORARIES)).
-define(ETS_TMP1,hd(?ETS_TEMPORARIES)).

-define(HELPER_NODE, (atom_to_list(?MODULE) ++ "_helper1")).

init_per_testcase(_Func, Config) ->
    PrivDir = ?config(priv_dir,Config),    
    file:make_dir(PrivDir),
    Dog=test_server:timetrap(test_server:seconds(60)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
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


fixbag(doc) ->
    ["Check for bug OTP-5087, safe_fixtable for bags could give "
     "incorrect lookups"];
fixbag(suite) ->
    [];
fixbag(Config) when is_list(Config) ->
    ?line T = ets:new(x,[bag]),
    ?line ets:insert(T,{a,1}),
    ?line ets:insert(T,{a,2}),
    ?line ets:safe_fixtable(T,true),
    ?line ets:match_delete(T,{a,2}),
    ?line ets:insert(T,{a,3}),
    ?line Res = ets:lookup(T,a),
    ?line ets:safe_fixtable(T,false),
    ?line Res = ets:lookup(T,a),
    ok.



insert_same_key(doc) ->
    ["Check correct behaviour if a key is deleted and reinserted during fixation."];
insert_same_key(suite) ->
    [];
insert_same_key(Config) when is_list(Config) ->
    ?line {ok,Dets1} = dets:open_file(?DETS_TMP1,
			       [{file, dets_filename(?DETS_TMP1,Config)}]),
    ?line Ets1 = ets:new(ets,[]),
    ?line insert_same_key(Dets1,dets,Config),
    ?line insert_same_key(Ets1,ets,Config),
    ?line ets:insert(Ets1,{1,2}),
    ?line 1 = ets:info(Ets1,size),
    ?line dets:insert(Dets1,{1,2}),
    ?line 1 = dets:info(Dets1,size),
    ?line dets:close(Dets1),
    ?line (catch file:delete(dets_filename(Dets1,Config))),
    ?line ets:delete(Ets1),
    ?line {ok,Dets2} = dets:open_file(?DETS_TMP1,
			       [{type,bag},{file, dets_filename(?DETS_TMP1,Config)}]),
    ?line Ets2 = ets:new(ets,[bag]),
    ?line insert_same_key(Dets2,dets,Config),
    ?line insert_same_key(Ets2,ets,Config),
    ?line ets:insert(Ets2,{1,2}),
    ?line 2 = ets:info(Ets2,size),
    ?line ets:insert(Ets2,{1,2}),
    ?line 2 = ets:info(Ets2,size),
    ?line dets:insert(Dets2,{1,2}),
    ?line 2 = dets:info(Dets2,size),
    ?line dets:insert(Dets2,{1,2}),
    ?line 2 = dets:info(Dets2,size),
    ?line dets:close(Dets2),
    ?line (catch file:delete(dets_filename(Dets2,Config))),
    ?line ets:delete(Ets2),
    ?line {ok,Dets3} = dets:open_file(?DETS_TMP1,
			       [{type,duplicate_bag},
				{file, dets_filename(?DETS_TMP1,Config)}]),
    ?line Ets3 = ets:new(ets,[duplicate_bag]),
    ?line insert_same_key(Dets3,dets,Config),
    ?line insert_same_key(Ets3,ets,Config),
    ?line ets:insert(Ets3,{1,2}),
    ?line 2 = ets:info(Ets3,size),
    ?line ets:insert(Ets3,{1,2}),
    ?line 3 = ets:info(Ets3,size),
    ?line dets:insert(Dets3,{1,2}),
    ?line 2 = dets:info(Dets3,size),
    ?line dets:insert(Dets3,{1,2}),
    ?line 3 = dets:info(Dets3,size),
    ?line dets:close(Dets3),
    ?line (catch file:delete(dets_filename(Dets3,Config))),
    ?line ets:delete(Ets3),
    ok.

insert_same_key(Tab,Mod,_Config) ->
    ?line Mod:insert(Tab,{1,1}),
    ?line Mod:insert(Tab,{1,2}),
    ?line Mod:insert(Tab,{2,2}),
    ?line Mod:insert(Tab,{2,2}),
    ?line Mod:safe_fixtable(Tab,true),
    ?line Mod:delete(Tab,1),
    ?line Mod:insert(Tab,{1,1}),
    ?line Expect = case Mod:info(Tab,type) of
	      bag ->
		  Mod:insert(Tab,{1,2}),
		  2;
	      _ -> 
		  1
	  end,
    ?line Mod:delete(Tab,2),
    ?line Mod:safe_fixtable(Tab,false),
    ?line case Mod:info(Tab,size) of
	      Expect ->
		  ok;
	      _ -> 
		  exit({size_field_wrong,{Mod,Mod:info(Tab)}})
	  end.
    
    


owner_dies(doc) ->
    ["Check correct behaviour if the table owner dies."];
owner_dies(suite) ->
    [];
owner_dies(Config) when is_list(Config) ->
    ?line P1 = start_commander(),
    ?line Ets1 = command(P1,{ets,new,[ets,[]]}),
    ?line command(P1,{ets,safe_fixtable,[Ets1,true]}),
    ?line {_,[{P1,1}]} = ets:info(Ets1, safe_fixed),
    ?line stop_commander(P1),
    ?line undefined = ets:info(Ets1, safe_fixed),
    ?line P2 = start_commander(),
    ?line Ets2 = command(P2,{ets,new,[ets,[public]]}),
    ?line command(P2,{ets,safe_fixtable,[Ets2,true]}),
    ?line ets:safe_fixtable(Ets2,true),
    ?line true = ets:info(Ets2, fixed),
    ?line {_,[{_,1},{_,1}]} = ets:info(Ets2, safe_fixed),
    ?line stop_commander(P2),
    ?line undefined = ets:info(Ets2, safe_fixed),
    ?line undefined = ets:info(Ets2, fixed),
    ?line P3 = start_commander(),
    ?line {ok,Dets} = ?LOG(command(P3, {dets, open_file, 
					[?DETS_TMP1,
					 [{file, 
					   dets_filename(?DETS_TMP1,
							 Config)}]]})),
    ?line command(P3, {dets, safe_fixtable, [Dets, true]}),
    ?line {_,[{P3,1}]} = dets:info(Dets, safe_fixed),
    ?line true = dets:info(Dets, fixed),
    ?line stop_commander(P3),
    ?line undefined = dets:info(Dets, safe_fixed),
    ?line undefined = dets:info(Dets, fixed),
    ?line P4 = start_commander(),
    ?line {ok,Dets} = command(P4, {dets, open_file, 
			     [?DETS_TMP1,
			      [{file, dets_filename(?DETS_TMP1,Config)}]]}),
    ?line {ok,Dets} = dets:open_file(?DETS_TMP1,
			       [{file, dets_filename(?DETS_TMP1,Config)}]),
    ?line false = dets:info(Dets, safe_fixed),
    ?line command(P4, {dets, safe_fixtable, [Dets, true]}),
    ?line dets:safe_fixtable(Dets, true),
    ?line {_,[{_,1},{_,1}]} = dets:info(Dets, safe_fixed),
    ?line dets:safe_fixtable(Dets, true),
    ?line stop_commander(P4),
    ?line S = self(),
    ?line {_,[{S,2}]} = dets:info(Dets, safe_fixed),
    ?line true = dets:info(Dets, fixed),
    ?line dets:close(Dets),
    ?line undefined = dets:info(Dets, fixed),
    ?line undefined = dets:info(Dets, safe_fixed),
    ok.
   

other_process_closes(doc) ->
    ["When another process closes an dets table, different "
     "things should happen depending on if it has opened it before."];

other_process_closes(suite) ->
    [];

other_process_closes(Config) when is_list(Config) ->
    ?line {ok,Dets} = dets:open_file(?DETS_TMP1,
			       [{file, dets_filename(tmp1,Config)}]),
    ?line P2 = start_commander(),
    ?line dets:safe_fixtable(Dets,true),
    ?line S = self(),
    ?line {_,[{S,1}]} = dets:info(Dets, safe_fixed),
    ?line command(P2,{dets, safe_fixtable, [Dets, true]}),
    ?line {_,[_,_]} = dets:info(Dets, safe_fixed),
    ?line {error, not_owner} = command(P2,{dets, close, [Dets]}),
    ?line {_,[_,_]} = dets:info(Dets, safe_fixed),
    ?line command(P2,{dets, open_file,[?DETS_TMP1,
				 [{file, 
				   dets_filename(?DETS_TMP1, Config)}]]}),
    ?line {_,[_,_]} = dets:info(Dets, safe_fixed),
    ?line command(P2,{dets, close, [Dets]}),
    ?line stop_commander(P2),
    ?line {_,[{S,1}]} = dets:info(Dets, safe_fixed),
    ?line true = dets:info(Dets,fixed),
    ?line dets:close(Dets),
    ?line undefined = dets:info(Dets,fixed),
    ?line undefined = dets:info(Dets, safe_fixed),
    ok.
    
other_process_deletes(doc) ->
    ["Check that fixtable structures are cleaned up if another process "
     "deletes an ets table"];
other_process_deletes(suite) ->
    [];
other_process_deletes(Config) when is_list(Config) ->
    ?line Ets = ets:new(ets,[public]),
    ?line P = start_commander(),
    ?line ets:safe_fixtable(Ets,true),
    ?line ets:safe_fixtable(Ets,true),
    ?line true = ets:info(Ets, fixed),
    ?line {_,_} = ets:info(Ets, safe_fixed),
    ?line command(P,{ets,delete,[Ets]}),
    ?line stop_commander(P),
    ?line undefined = ets:info(Ets, fixed),
    ?line undefined = ets:info(Ets, safe_fixed),
    ok.

multiple_fixes(doc) ->
    ["Check that multiple safe_fixtable keeps the reference counter."];
multiple_fixes(suite) ->
    [];
multiple_fixes(Config) when is_list(Config) ->
    ?line {ok,Dets} = dets:open_file(?DETS_TMP1,
			       [{file, dets_filename(?DETS_TMP1,Config)}]),
    ?line Ets = ets:new(ets,[]),
    ?line multiple_fixes(Dets,dets),
    ?line multiple_fixes(Ets,ets),
    ?line dets:close(Dets),
    ok.

multiple_fixes(Tab, Mod) ->
    ?line false = Mod:info(Tab,fixed),
    ?line false = Mod:info(Tab, safe_fixed),
    ?line Mod:safe_fixtable(Tab, true),
    ?line true = Mod:info(Tab,fixed),
    ?line S = self(),
    ?line {_,[{S,1}]} = Mod:info(Tab, safe_fixed),
    ?line Mod:safe_fixtable(Tab, true),
    ?line Mod:safe_fixtable(Tab, true),
    ?line {_,[{S,3}]} = Mod:info(Tab, safe_fixed),
    ?line true = Mod:info(Tab,fixed),
    ?line Mod:safe_fixtable(Tab, false),
    ?line {_,[{S,2}]} = Mod:info(Tab, safe_fixed),
    ?line true = Mod:info(Tab,fixed),
    ?line Mod:safe_fixtable(Tab, false),
    ?line {_,[{S,1}]} = Mod:info(Tab, safe_fixed),
    ?line true = Mod:info(Tab,fixed),
    ?line Mod:safe_fixtable(Tab, false),
    ?line false = Mod:info(Tab, safe_fixed),
    ?line false = Mod:info(Tab,fixed).

multiple_processes(doc) ->
    ["Check that multiple safe_fixtable across processes are reference "
     "counted OK"];
multiple_processes(suite) ->
    [];
multiple_processes(Config) when is_list(Config) ->
    ?line {ok,Dets} = dets:open_file(?DETS_TMP1,[{file, 
					    dets_filename(?DETS_TMP1,
							  Config)}]),
    ?line Ets = ets:new(ets,[public]),
    ?line multiple_processes(Dets,dets),
    ?line multiple_processes(Ets,ets),
    ok.

multiple_processes(Tab, Mod) ->
    ?line io:format("Mod = ~p\n", [Mod]),
    ?line P1 = start_commander(),
    ?line P2 = start_commander(),
    ?line false = Mod:info(Tab,fixed),
    ?line false = Mod:info(Tab, safe_fixed),
    ?line command(P1, {Mod, safe_fixtable, [Tab,true]}),
    ?line true = Mod:info(Tab,fixed),
    ?line {_,[{P1,1}]} = Mod:info(Tab, safe_fixed),
    ?line command(P2, {Mod, safe_fixtable, [Tab,true]}),
    ?line true = Mod:info(Tab,fixed),
    ?line {_,L} = Mod:info(Tab,safe_fixed),
    ?line true = (lists:sort(L) == lists:sort([{P1,1},{P2,1}])),
    ?line command(P2, {Mod, safe_fixtable, [Tab,true]}),
    ?line {_,L2} = Mod:info(Tab,safe_fixed),
    ?line true = (lists:sort(L2) == lists:sort([{P1,1},{P2,2}])),
    ?line command(P2, {Mod, safe_fixtable, [Tab,false]}),
    ?line true = Mod:info(Tab,fixed),
    ?line {_,L3} = Mod:info(Tab,safe_fixed),
    ?line true = (lists:sort(L3) == lists:sort([{P1,1},{P2,1}])),
    ?line command(P2, {Mod, safe_fixtable, [Tab,false]}),
    ?line true = Mod:info(Tab,fixed),
    ?line {_,[{P1,1}]} = Mod:info(Tab, safe_fixed),
    ?line stop_commander(P1),
    ?line receive after 1000 -> ok end,
    ?line false = Mod:info(Tab,fixed),
    ?line false = Mod:info(Tab, safe_fixed),
    ?line command(P2, {Mod, safe_fixtable, [Tab,true]}),
    ?line true = Mod:info(Tab,fixed),
    ?line {_,[{P2,1}]} = Mod:info(Tab, safe_fixed),
    case Mod of
	dets ->
	    ?line dets:close(Tab);
	ets ->
	    ?line ets:delete(Tab)
    end,
    ?line stop_commander(P2),
    ?line receive after 1000 -> ok end,
    ?line undefined = Mod:info(Tab, safe_fixed),
    ok.
    
    

%%% Helpers
dets_filename(Base, Config) when is_atom(Base) ->
    dets_filename(atom_to_list(Base) ++ ".dat", Config);
dets_filename(Basename, Config) ->
    PrivDir = ?config(priv_dir,Config),
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



