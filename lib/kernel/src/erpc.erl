%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
%% Author: Rickard Green
%%

-module(erpc).

%% Exported API

-export([call/4,
         call/5,
	 cast/4,
         send_request/4,
         receive_response/1,
         receive_response/2,
         wait_response/1,
         wait_response/2,
         check_response/2,
         multicall/4,
	 multicall/5]).

-export_type([request_id/0]).

%% Internal exports (also used by the 'rpc' module)

-export([execute_call/4,
         execute_call/3,
         is_arg_error/4,
         trim_stack/4,
         call_result/4]).

%%------------------------------------------------------------------------

-compile({inline,[{result,4}]}). %% Nicer error stack trace...

-define(MAX_INT_TIMEOUT, 4294967295).
-define(TIMEOUT_TYPE, 0..?MAX_INT_TIMEOUT | 'infinity').
-define(IS_VALID_TMO_INT(TI_), (is_integer(TI_)
                                andalso (0 =< TI_)
                                andalso (TI_ =< ?MAX_INT_TIMEOUT))).
-define(IS_VALID_TMO(T_), ((T_ == infinity) orelse ?IS_VALID_TMO_INT(T_))).

%%------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------

-spec call(Node, Module, Function, Args) -> Result when
      Node :: node(),
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Result :: term().

call(N, M, F, A) ->
    call(N, M, F, A, infinity).

-dialyzer([{nowarn_function, call/5}, no_return]).

-spec call(Node, Module, Function, Args, Timeout) -> Result when
      Node :: node(),
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Timeout :: ?TIMEOUT_TYPE,
      Result :: term().

call(N, M, F, A, infinity) when node() =:= N,  %% Optimize local call
                                is_atom(M),
                                is_atom(F),
                                is_list(A) ->
    try
        {return, Return} = execute_call(M,F,A),
        Return
    catch
        exit:Reason ->
            exit({exception, Reason});
        error:Reason:Stack ->
            case is_arg_error(Reason, M, F, A) of
                true ->
                    error({?MODULE, Reason});
                false ->
                    ErpcStack = trim_stack(Stack, M, F, A),
                    error({exception, Reason, ErpcStack})
            end
    end;
call(N, _M, _F, _A, infinity) when node() =:= N ->
    error({?MODULE, badarg});
call(N, M, F, A, T) when ?IS_VALID_TMO(T) ->
    try
        Res = make_ref(),
        ReqId = erlang:spawn_request(N, ?MODULE, execute_call,
                                     [Res, M, F, A],
                                     [{reply, error_only},
                                      monitor]),
        receive
            {spawn_reply, ReqId, error, Reason} ->
                result(spawn_reply, ReqId, Res, Reason);
            {'DOWN', ReqId, process, _Pid, Reason} ->
                result(down, ReqId, Res, Reason)
        after T ->
                result(timeout, ReqId, Res, undefined)
        end
    catch
        error:badarg ->
            error({?MODULE, badarg})
    end;
call(_N, _M, _F, _A, _T) ->
    error({?MODULE, badarg}).

%% Asynchronous call

-opaque request_id() :: {reference(), reference()}.

-spec send_request(Node, Module, Function, Args) -> RequestId when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      RequestId :: request_id().

send_request(N, M, F, A) ->
    try
        Res = make_ref(),
        ReqId = erlang:spawn_request(N, ?MODULE, execute_call,
                                     [Res, M, F, A],
                                     [{reply, error_only},
                                      monitor]),
        {Res, ReqId}
    catch
        error:badarg ->
            error({?MODULE, badarg})
    end.

-spec receive_response(RequestId) -> Result when
      RequestId :: request_id(),
      Result :: term().

receive_response({Res, ReqId} = RId) when is_reference(Res),
                                          is_reference(ReqId) ->
    receive_response(RId, infinity);
receive_response(_) ->
    error({?MODULE, badarg}).

-dialyzer([{nowarn_function, receive_response/2}, no_return]).

-spec receive_response(RequestId, Timeout) -> Result when
      RequestId :: request_id(),
      Timeout :: ?TIMEOUT_TYPE,
      Result :: term().

receive_response({Res, ReqId}, Tmo) when is_reference(Res),
                                         is_reference(ReqId),
                                         ?IS_VALID_TMO(Tmo) ->
    receive
        {spawn_reply, ReqId, error, Reason} ->
            result(spawn_reply, ReqId, Res, Reason);
        {'DOWN', ReqId, process, _Pid, Reason} ->
            result(down, ReqId, Res, Reason)
    after Tmo ->
            result(timeout, ReqId, Res, undefined)
    end;
receive_response(_, _) ->
    error({?MODULE, badarg}).

-spec wait_response(RequestId) -> {'response', Result} | 'no_response' when
      RequestId :: request_id(),
      Result :: term().

wait_response({Res, ReqId} = RId) when is_reference(Res),
                                       is_reference(ReqId) ->
    wait_response(RId, 0).
    
-dialyzer([{nowarn_function, wait_response/2}, no_return]).

-spec wait_response(RequestId, WaitTime) ->
          {'response', Result} | 'no_response' when
      RequestId :: request_id(),
      WaitTime :: ?TIMEOUT_TYPE,
      Result :: term().

wait_response({Res, ReqId}, WT) when is_reference(Res),
                                     is_reference(ReqId),
                                     ?IS_VALID_TMO(WT) ->
    receive
        {spawn_reply, ReqId, error, Reason} ->
            result(spawn_reply, ReqId, Res, Reason);
        {'DOWN', ReqId, process, _Pid, Reason} ->
            {response, result(down, ReqId, Res, Reason)}
    after WT ->
            no_response
    end;
wait_response(_, _) ->
    error({?MODULE, badarg}).

-dialyzer([{nowarn_function, check_response/2}, no_return]).

-spec check_response(Message, RequestId) ->
          {'response', Result} | 'no_response' when
      Message :: term(),
      RequestId :: request_id(),
      Result :: term().

check_response({spawn_reply, ReqId, error, Reason},
               {Res, ReqId}) when is_reference(Res),
                                  is_reference(ReqId) ->
    result(spawn_reply, ReqId, Res, Reason);
check_response({'DOWN', ReqId, process, _Pid, Reason},
               {Res, ReqId}) when is_reference(Res),
                                  is_reference(ReqId) ->
    {response, result(down, ReqId, Res, Reason)};
check_response(_Msg, {Res, ReqId}) when is_reference(Res),
                                        is_reference(ReqId) ->
    no_response;
check_response(_, _) ->
    error({?MODULE, badarg}).

-type stack_item() ::
        {Module :: atom(),
         Function :: atom(),
         Arity :: arity() | (Args :: [term()]),
         Location :: [{file, Filename :: string()} |
                      {line, Line :: pos_integer()}]}.

-type caught_call_exception() ::
        {throw, Throw :: term()}
      | {exit, {exception, Reason :: term()}}
      | {error, {exception, Reason :: term(), StackTrace :: [stack_item()]}}
      | {exit, {signal, Reason :: term()}}
      | {error, {?MODULE, Reason :: term()}}.


-spec multicall(Nodes, Module, Function, Args) -> Result when
      Nodes :: [atom()],
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Result :: [{ok, ReturnValue :: term()} | caught_call_exception()].

multicall(Ns, M, F, A) ->
    multicall(Ns, M, F, A, infinity).

-spec multicall(Nodes, Module, Function, Args, Timeout) -> Result when
      Nodes :: [atom()],
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Timeout :: ?TIMEOUT_TYPE,
      Result :: [{ok, ReturnValue :: term()} | caught_call_exception()].

multicall(Ns, M, F, A, T) when ?IS_VALID_TMO(T) ->
    EndTime = if T == infinity ->
                      infinity;
                 T == 0 ->
                      erlang:monotonic_time(millisecond);
                 T == ?MAX_INT_TIMEOUT ->
                      erlang:monotonic_time(millisecond)
                          + ?MAX_INT_TIMEOUT;
                 true ->
                      Start = erlang:monotonic_time(),
                      NTmo = erlang:convert_time_unit(T,
                                                      millisecond,
                                                      native),
                      erlang:convert_time_unit(Start + NTmo - 1,
                                               native,
                                               millisecond) + 1
              end,
    try
        ReqIds = mcall_send_requests(Ns, M, F, A, []),
        mcall_wait_replies(ReqIds, [], EndTime)
    catch
        error:{?MODULE, badarg} = Reason ->
            error(Reason)
    end;
multicall(_Ns, _M, _F, _A, _T) ->
    error({?MODULE, badarg}).

-spec cast(Node, Module, Function, Args) -> ok when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

cast(Node, Mod, Fun, Args) ->
    %% Fire and forget...
    try
        _ = erlang:spawn_request(Node, Mod, Fun, Args, [{reply, no}]),
        ok
    catch
        error:badarg ->
            error({?MODULE, badarg})
    end.

%%------------------------------------------------------------------------
%% Exported internals
%%------------------------------------------------------------------------

%% Note that most of these are used by 'rpc' as well...

execute_call(Ref, M, F, A) ->
    Reply = try
                {Ref, return, apply(M, F, A)}
            catch
                throw:Reason ->
                    {Ref, throw, Reason};
                exit:Reason ->
                    {Ref, exit, Reason};
                error:Reason:Stack ->
                    case is_arg_error(Reason, M, F, A) of
                        true ->
                            {Ref, error, {?MODULE, Reason}};
                        false ->
                            ErpcStack = trim_stack(Stack, M, F, A),
                            {Ref, error, Reason, ErpcStack}
                    end
            end,
    exit(Reply).

execute_call(M,F,A) ->
    {return, apply(M, F, A)}.

call_result(Type, ReqId, Res, Reason) ->
    result(Type, ReqId, Res, Reason).

is_arg_error(badarg, M, F, A) ->
    try
        true = is_atom(M),
        true = is_atom(F),
        true = is_integer(length(A)),
        false
    catch
        error:badarg ->
            true
    end;
is_arg_error(system_limit, _M, _F, A) ->
    try
        apply(?MODULE, nonexisting, A),
        false
    catch
        error:system_limit -> true;
        _:_ -> false
    end;
is_arg_error(_R, _M, _F, _A) ->
    false.

trim_stack([{?MODULE, execute_call, _, _} | _], M, F, A) ->
    [{M, F, A, []}];
trim_stack([{M, F, A, _} = SF, {?MODULE, execute_call, _, _} | _], M, F, A) ->
    [SF];
trim_stack(S, M, F, A) ->
    try
        trim_stack_aux(S, M, F, A)
    catch
        throw:use_all -> S
    end.

%%------------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------------

trim_stack_aux([], _M, _F, _A) ->
    throw(use_all);
trim_stack_aux([{M, F, AL, _} = SF, {?MODULE, execute_call, _, _} | _],
               M, F, A) when AL == length(A) ->
    [SF];
trim_stack_aux([{?MODULE, execute_call, _, _} | _], M, F, A) ->
    [{M, F, length(A), []}];
trim_stack_aux([SF|SFs], M, F, A) ->
    [SF|trim_stack_aux(SFs, M, F, A)].

call_abandon(ReqId) ->
    case erlang:spawn_request_abandon(ReqId) of
        true -> true;
        false -> erlang:demonitor(ReqId, [info])
    end.

-dialyzer([{nowarn_function, result/4}, no_return]).

-spec result('down', ReqId, Res, Reason) -> term() when
      ReqId :: reference(),
      Res :: reference(),
      Reason :: term();
                  ('spawn_reply', ReqId, Res, Reason) -> no_return() when
      ReqId :: reference(),
      Res :: reference(),
      Reason :: term();
                  ('timeout', ReqId, Res, Reason) -> term() when
      ReqId :: reference(),
      Res :: reference(),
      Reason :: term().

result(down, _ReqId, Res, {Res, return, Return}) ->
    Return;
result(down, _ReqId, Res, {Res, throw, Throw}) ->
    throw(Throw);
result(down, _ReqId, Res, {Res, exit, Exit}) ->
    exit({exception, Exit});
result(down, _ReqId, Res, {Res, error, Error, Stack}) ->
    error({exception, Error, Stack});
result(down, _ReqId, Res, {Res, error, {?MODULE, _} = ErpcErr}) ->
    error(ErpcErr);
result(down, _ReqId, _Res, noconnection) ->
    error({?MODULE, noconnection});
result(down, _ReqId, _Res, Reason) ->
    exit({signal, Reason});
result(spawn_reply, _ReqId, _Res, Reason) ->
    error({?MODULE, Reason});
result(timeout, ReqId, Res, _Reason) ->
    case call_abandon(ReqId) of
        true ->
            error({?MODULE, timeout});
        false ->
            %% Spawn error or DOWN has arrived. Return
            %% a result instead of a timeout since we
            %% just got the result...
            receive
                {spawn_reply, ReqId, error, Reason} ->
                    result(spawn_reply, ReqId, Res, Reason);
                {'DOWN', ReqId, process, _Pid, Reason} ->
                    result(down, ReqId, Res, Reason)
            after
                0 ->
                    %% Invalid request id...
                    error({?MODULE, badarg})
            end
    end.

mcall_send_requests([], _M, _F, _A, RIDs) ->
    RIDs;
mcall_send_requests([N|Ns], M, F, A, RIDs) ->
    RID = send_request(N, M, F, A),
    mcall_send_requests(Ns, M, F, A, [RID|RIDs]);
mcall_send_requests(_, _M, _F, _A, _RIDs) ->
    error({?MODULE, badarg}).

mcall_wait_replies([], Replies, _Tmo) ->
    Replies;
mcall_wait_replies([RID|RIDs], Replies, infinity) ->
    Reply = mcall_wait_reply(RID, infinity),
    mcall_wait_replies(RIDs, [Reply|Replies], infinity);
mcall_wait_replies([RID|RIDs], Replies, EndTime) ->
    Now = erlang:monotonic_time(millisecond),
    Tmo = case EndTime - Now of
              T when T =< 0 -> 0;
              T -> T
          end,
    Reply = mcall_wait_reply(RID, Tmo),
    mcall_wait_replies(RIDs, [Reply|Replies], EndTime).

mcall_wait_reply(RID, Tmo) ->
    try
        {ok, receive_response(RID, Tmo)}
    catch
        Class:Reason ->
            {Class, Reason}
    end.
    
