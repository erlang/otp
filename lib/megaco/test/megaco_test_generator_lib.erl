%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Utility module for the generator users
%%----------------------------------------------------------------------

-module(megaco_test_generator_lib).

-export([
	 await_completion/1, await_completion/2
	]).



%%%=========================================================================
%%%  API
%%%=========================================================================

await_completion(Tags) ->
    await_completion(Tags, infinity).

await_completion(Tags, Timeout) ->
    TmrRef = start_timer(Timeout),
    await_completion(TmrRef, Tags, [], []).

await_completion(TmrRef, [], OK, []) ->
    stop_timer(TmrRef),
    {ok, OK};
await_completion(TmrRef, [], OK, ERROR) ->
    stop_timer(TmrRef),
    {error, {OK, ERROR}};
await_completion(TmrRef, Tags, OK, ERROR) ->
    receive
	exec_complete_timeout ->
	    {error, {timeout, Tags, OK, ERROR}};

	{exec_complete, Tag, ok, Result} ->
	    case lists:delete(Tag, Tags) of
		Tags ->
		    %% Unknown => ignore
		    await_completion(TmrRef, Tags, OK, ERROR);
		Tags2 ->
		    await_completion(TmrRef, Tags2, [{Tag, Result}|OK], ERROR)
	    end;

	{exec_complete, Tag, error, Reason} ->
	    case lists:delete(Tag, Tags) of
		Tags ->
		    %% Unknown => ignore
		    await_completion(TmrRef, Tags, OK, ERROR);
		Tags2 ->
		    await_completion(TmrRef, Tags2, OK, [{Tag, Reason}|ERROR])
	    end
    end.

start_timer(infinity) ->
    undefined;
start_timer(Timeout) when is_integer(Timeout) andalso (Timeout > 0) ->
    Msg = exec_complete_timeout,
    erlang:send_after(Timeout, self(), Msg).

stop_timer(undefined) ->
    ok;
stop_timer(TmrRef) ->
    erlang:cancel_timer(TmrRef).
