%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2020. All Rights Reserved.
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
            error_msg("Exec complete timeout:"
                      "~n   Tags:  ~p"
                      "~n   OK:    ~p"
                      "~n   ERROR: ~p", [Tags, OK, ERROR]),
	    {error, {timeout, Tags, OK, ERROR}};

	{exec_complete, Tag, ok, Result} ->
	    case lists:delete(Tag, Tags) of
		Tags ->
		    %% Unknown => ignore
                    warning_msg("Exec unknown complete with success:"
                                "~n   Tag:    ~p"
                                "~n   Result: ~p", [Tag, Result]),
		    await_completion(TmrRef, Tags, OK, ERROR);
		Tags2 ->
                    info_msg("Exec complete with success:"
                             "~n   Tag:    ~p"
                             "~n   Result: ~p", [Tag, Result]),
		    await_completion(TmrRef, Tags2, [{Tag, Result}|OK], ERROR)
	    end;

	{exec_complete, Tag, error, Reason} ->
	    case lists:delete(Tag, Tags) of
		Tags ->
		    %% Unknown => ignore
                    warning_msg("Exec unknown complete with failure:"
                                "~n   Tag:    ~p"
                                "~n   Reason: ~p", [Tag, Reason]),
		    await_completion(TmrRef, Tags, OK, ERROR);
		Tags2 ->
                    error_msg("Exec complete with failure:"
                              "~n   Tag:    ~p"
                              "~n   Reason: ~p", [Tag, Reason]),
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


info_msg(F, A) ->
    error_logger:info_msg(F ++ "~n", A).

warning_msg(F, A) ->
    error_logger:warning_msg(F ++ "~n", A).

error_msg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).

