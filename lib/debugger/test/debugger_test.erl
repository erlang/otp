%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : This is the test module to be used in with the test cases
%%%           in the debugger test document.
%%%----------------------------------------------------------------------

-module(debugger_test).


-export ([installation_ok/0,
	  list/0,
	  fac/1,
	  c_break/1]).


-define (INT_DIR, "misc").      % The directory of the Interpreter directory
-define (INT, "interpreter").   % The Interpreter directory name.
-define (DBG, "debugger").      % Debugger name


%%% installation_ok  /0
%%%

installation_ok () ->
    debugger_ok (),
    interpreter_ok ().



%%% debugger_ok  /0
%%%

debugger_ok () ->
    L = code:get_path (),

    case debugger_in_codepath (L) of
	{true, Msg} ->
	    out_put (Msg);

	Other ->
	    out_put (Other)
    end.



%%% debugger_in_codepath  /2
%%%

debugger_in_codepath ([]) ->
    Msg = io_lib:format ("False: ~s not in code path", [?DBG]),
    lists:flatten (Msg);

debugger_in_codepath ([Path | T]) ->
    case string:str (Path, ?DBG) =/= 0 of
	true ->
	    Msg = io_lib:format ("Ok: ~s in code path (~s)", [?DBG, Path]),
	    Msg1 = lists:flatten (Msg),
	    {true, Msg1};

	_Other ->
	    debugger_in_codepath (T)
    end.



%%% interpreter_ok  /0
%%%

interpreter_ok () ->
    Root_dir = code:root_dir (),
    Misc_dir = filename:join (Root_dir, ?INT_DIR),

    In_misc = case file:list_dir (Misc_dir) of
		  {ok, L} ->
		      lists:member (?INT, L);

		  Other ->
		      Other
	      end,

    case In_misc of
	true ->
	    Msg =  io_lib:format ("Ok: ~s is in ~s", [?INT, ?INT_DIR]),
	    Msg1 = lists:flatten (Msg),
	    out_put (Msg1);

	Other1 ->
	    Msg = io_lib:format ("Error: interpreter in misc - ~s", [Other1]),
	    Msg1 = lists:flatten (Msg),
	    out_put (Msg1)
    end.



%%% list  /0
%%%

list () ->
    A = [1, 2, 3, 4, 5],
    B = [a, b, c, d, e],
    lists:append (A, B).



%%% fac  /1
%%%

fac (0) ->
    1;


fac (N) ->
    N * fac (N - 1).



%%% c_break  /1
%%%

c_break (Bindings) ->
    case int:get_binding ('N', Bindings) of
	{value, 3} ->
	    true;

	_Other ->
	    false
    end.



%%% out_put  /1
%%%

out_put (X) ->
    io:format ("~n~p~n", [X]).
