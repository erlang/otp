%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: 
%%----------------------------------------------------------------------
-module(megaco_tc_controller).

-export([start_link/0, stop/0, 
	 insert/2, lookup/1, delete/1]).
-export([init/2]).

-include("megaco_test_lib.hrl").

-define(SERVER, ?MODULE).
-define(TABLE,  ?MODULE).

start_link() ->
    p("start_link -> entry"),
    Pid = spawn_link(?MODULE, init, [self(),get(dbg)]),
    receive
	{tc_started, Pid, Reply} ->
	    p("start_link -> ~n   ~p", [Reply]),
	    Reply
    end.

stop() ->
    request(stop, undefined).

insert(Key, Val) ->
    request(insert, {Key, Val}).

lookup(Key) ->
    case request(lookup, Key) of
	{value, _} = Value ->
	    Value;
	_ ->
	    false
    end.

delete(Key) ->
    request(delete, Key).

request(Tag, Data) ->
    p("request -> entry with"
      "~n   Tag:  ~p"
      "~n   Data: ~p", [Tag, Data]),
    case global:whereis_name(?SERVER) of
	Pid when is_pid(Pid) ->
	    Pid ! {Tag, self(), Data},
	    receive
		{Tag, Pid, Reply} ->
		    p("request -> response: "
		      "~n   Reply: ~p", [Reply]),
		    Reply
	    end;
	_ ->
	    {error, not_started}
    end.
	    
init(Parent, true) ->
    put(dbg,true),
    init(Parent);
init(Parent, _) ->
    init(Parent).

init(Parent) ->
    p("init -> entry with"
      "~n   Parent: ~p", [Parent]),
    ets:new(?TABLE, [named_table, protected, set]),
    case global:register_name(?SERVER, self()) of
	yes ->
	    p("init -> registration ok"),
	    Parent ! {tc_started, self(), ok},
	    loop(Parent);
	no ->
	    p("init -> registration failed"),
	    Parent ! {tc_started, self(), {error, already_registered}},
	    exit(normal)
    end.

loop(Parent) ->
    p("loop -> entry"),
    receive
	{insert, Parent, {Key, Val}} ->
	    p("loop -> received insert request when"
	      "~n   Key: ~p"
	      "~n   Val: ~p", [Key, Val]),
	    ets:insert(?TABLE, {Key, Val}),
	    Parent ! {insert, self(), ok};
	{delete, Parent, Key} ->
	    p("loop -> received delete request when"
	      "~n   Key: ~p", [Key]),
	    ets:delete(?TABLE, Key),
	    Parent ! {delete, self(), ok};
	{lookup, From, Key} when is_pid(From) ->
	    p("loop -> received lookup request when"
	      "~n   Key: ~p", [Key]),
	    case ets:lookup(?TABLE, Key) of
		[{Key, Val}] ->
		    p("loop -> lookup: ~p", [Val]),
		    From ! {lookup, self(), {value, Val}};
		_ ->
		    p("loop -> lookup unsuccessful"),
		    From ! {lookup, self(), false}
	    end;
	{stop, Parent, _} ->
	    p("loop -> received stop request"),
	    ets:delete(?TABLE),
	    global:unregister_name(?SERVER),
	    Parent ! {stop, self(), ok},
	    exit(normal);

	{'EXIT', Parent, Reason} when is_pid(Parent) ->
	    p("loop -> received exit signal from parent"
	      "~n   Reason: ~p", [Reason]),
	    exit(Reason);

	{UnknownRequest, Parent, UnknownData} when is_pid(Parent) ->
	    p("loop -> received unknown request when"
	      "~n   UnknownRequest: ~p"
	      "~n   UnknownData:    ~p", [UnknownRequest, UnknownData]),
	    Error = {error, {unknown_request, {UnknownRequest, UnknownData}}},
	    Parent ! {UnknownRequest, self(), Error};

	{Request, From, Data} when is_pid(From) ->
	    p("loop -> received request from unknown when"
	      "~n   Request: ~p"
	      "~n   From:    ~p"
	      "~n   Data:    ~p", [Request, From, Data]),
	    Error = {error, {unknown_request, {Request, Data}}},
	    Parent ! {Request, self(), Error};

	Crap ->
	    p("loop -> received crap: "
	      "~n   Crap:   ~p"
	      "~nwhen"
	      "~n   Parent: ~p", [Crap, Parent]),
	    ok
    end,
    loop(Parent).

	    
p(F) ->
    p(F, []).

p(F, A) ->
    p(get(dbg), F, A).

p(true, F, A) ->
    io:format("~w:~p:" ++ F ++ "~n", [?MODULE, self()|A]);
p(_, _, _) ->
    ok.

