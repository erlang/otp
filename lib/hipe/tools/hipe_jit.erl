%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2002 by Erik Johansson.  
%% ====================================================================
%%  Module   :	hipe_jit
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2002-03-14 Erik Johansson (happi@it.uu.se): Created.
%% ====================================================================
%% @doc
%%    A tool to enable using the HiPE compiler as an automatic JIT
%%    compiler rather than a user-controlled one.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_jit).

-export([start/0]).

-record(state, {mode = start     :: 'sleep' | 'start' | 'wait',
	       	threshold = 5000 :: non_neg_integer(),
		sleep = 5000     :: non_neg_integer(),
		time = 1000      :: non_neg_integer()}).

%%---------------------------------------------------------------------

-spec start() -> pid().
%% @doc
%%    Starts an Erlang process which calls the HiPE compiler every
%%    now and then (when it sees it fit to do so).
%% @end
start() ->
  spawn(fun () -> loop(#state{}) end).

loop(State) ->
  case State#state.mode of
    start ->
      start(State);
    wait ->
      wait(State);
    _ ->
      sleep(State)
  end.

sleep(State) ->
  receive
    quit -> ok
  after State#state.sleep ->
    loop(State#state{mode=start})
  end.

start(State) ->
  catch hipe_profile:prof(),
  catch hipe_profile:clear(),
  loop(State#state{mode=wait}).

wait(State) ->
  receive
    quit -> ok
  after State#state.time ->
    R = [M || {M,C} <- (catch hipe_profile:mods_res()),
			C > State#state.threshold],
    catch hipe_profile:prof_off(),
    lists:foreach(fun(M) ->
		    io:format("Compile ~w\n",[M]),
		    hipe:c(M,[o2,verbose])
		  end, R)
  end,
  loop(State#state{mode=sleep}).
