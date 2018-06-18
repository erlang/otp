%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2018. All Rights Reserved.
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
%%% Purpose : Test wrap_log_reader.erl
%%%----------------------------------------------------------------------

-module(wrap_log_test).

-export([init/0, stop/0]).
-define(fsize, 80).
-define(fno, 4).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-else.
-define(format(S, A), ok).
-endif.

init() ->
    spawn(fun() -> start(wlr_logger) end),
    spawn(fun() -> start2(wlt) end),
    wait_registered(wlr_logger),
    wait_registered(wlt),
    ok.

wait_registered(Name) ->
    case whereis(Name) of
	undefined ->
	    timer:sleep(100),
	    wait_registered(Name);
	_Pid ->
	    ok
    end.

stop() ->
    catch wlr_logger ! exit,
    catch wlt ! exit,
    wait_unregistered(wlr_logger),
    wait_unregistered(wlt),
    ok.

wait_unregistered(Name) ->
    case whereis(Name) of
	undefined ->
	    ok;
	_Pid ->
	    timer:sleep(100),
	    wait_unregistered(Name)
    end.

start(Name) ->
    ?format("Starting ~p~n", [Name]),
    register(Name, self()),
    loop().

start2(Name) ->
    ?format("Starting ~p~n", [Name]),
    register(Name, self()),
    loop2(eof, Name).

loop() ->
    receive
	{open, Pid, Name, File} ->
	    R = disk_log:open([{name, Name}, {type, wrap}, {file, File},
			       {size, {?fsize, ?fno}}]),
	    ?format("wlr_logger: open ~p -> ~p~n", [Name, R]),
	    Pid ! R,
	    loop();
	
	{open_ext, Pid, Name, File} ->
	    R = disk_log:open([{name, Name}, {type, wrap}, {file, File},
			       {format, external}, {size, {?fsize, ?fno}}]),
	    ?format("wlr_logger: open ~p -> ~p~n", [Name, R]),
	    Pid ! R,
	    loop();
	
	{close, Pid, Name} ->
	    R = disk_log:close(Name),
	    ?format("wlr_logger: close ~p -> ~p~n", [Name, R]),
	    Pid ! R,
	    loop();
	
	{sync, Pid, Name} ->
	    R = disk_log:sync(Name),
	    ?format("wlr_logger: sync ~p -> ~p~n", [Name, R]),
	    Pid ! R,
	    loop();
	
	{log_terms, Pid, Name, Terms} ->
	    R = disk_log:log_terms(Name, Terms),
	    ?format("wlr_logger: log_terms ~p -> ~p~n", [Name, R]),
	    Pid ! R,
	    loop();

	{blog_terms, Pid, Name, Terms} ->
	    R = disk_log:blog_terms(Name, Terms),
	    ?format("wlr_logger: blog_terms ~p -> ~p~n", [Name, R]),
	    Pid ! R,
	    loop();

	exit ->
	    ?format("Stopping wlr_logger~n", []),
	    exit(normal);

	_Else ->
	    ?format("wlr_logger: ignored: ~p~n", [_Else]),
	    loop()
    end.

loop2(C, Wlt) ->
    receive
	{open, Pid, Name} ->
	    case wrap_log_reader:open(Name) of
		{ok, R} ->
		    ?format("~p: open ~p -> ~p~n", [Wlt, Name, {ok, R}]),
		    Pid ! {ok, R},
		    loop2(R, Wlt);
		E ->
		    ?format("~p: open ~p -> ~p~n", [Wlt, Name, E]),
		    Pid ! E,
		    loop2(C, Wlt)
	    end;
	
	{open, Pid, Name, No} ->
	    case wrap_log_reader:open(Name, No) of
		{ok, R} ->
		    ?format("~p: open ~p, file ~p -> ~p~n", 
			    [Wlt, Name, No, {ok, R}]),
		    Pid ! {ok, R},
		    loop2(R, Wlt);
		E ->
		    ?format("~p: open ~p, file ~p -> ~p~n", 
			    [Wlt, Name, No, E]),
		    Pid ! E,
		    loop2(C, Wlt)
	    end;
	
	{close, Pid, WR} ->
	    R = wrap_log_reader:close(WR),
	    ?format("~p: close -> ~p~n", [Wlt, R]),
	    Pid ! R,
	    loop2(eof, Wlt);
	
	{chunk, Pid, WR} ->
	    did_chunk(wrap_log_reader:chunk(WR), Pid, Wlt);
	
	{chunk, Pid, WR, N} ->
	    did_chunk(wrap_log_reader:chunk(WR, N), Pid, Wlt);

	exit ->
	    ?format("Stopping ~p~n", [Wlt]),
	    exit(normal);

	_Else ->
	    ?format("~p: ignored: ~p~n", [Wlt, _Else]),
	    loop2(C, Wlt)
    end.

did_chunk({C1, L}, Pid, Wlt) ->
    ?format("~p: chunk -> ~p~n", [Wlt, {C1, L}]),
    Pid ! {C1, L},
    loop2(C1, Wlt);
did_chunk({C1, L, _Bad}, Pid, Wlt) ->
    ?format("~p: chunk -> ~p (bad)~n", [Wlt, {C1, L, _Bad}]),
    Pid ! {C1, L},
    loop2(C1, Wlt).
