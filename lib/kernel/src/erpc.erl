%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2024. All Rights Reserved.
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
-moduledoc """
Enhanced Remote Procedure Call

This module provide services similar to Remote Procedure Calls. A remote
procedure call is a method to call a function on a remote node and collect the
answer. It is used for collecting information on a remote node, or for running a
function with some specific side effects on the remote node.

This is an enhanced subset of the operations provided by the `m:rpc` module.
Enhanced in the sense that it makes it possible to distinguish between returned
value, raised exceptions, and other errors. `erpc` also has better performance
and scalability than the original `rpc` implementation. However, current `rpc`
module will utilize `erpc` in order to also provide these properties when
possible.

In order for an `erpc` operation to succeed, the remote node also needs to
support `erpc`. Typically only ordinary Erlang nodes as of OTP 23 have `erpc`
support.

Note that it is up to the user to ensure that correct code to execute via `erpc`
is available on the involved nodes.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [Blocking Signaling Over Distribution](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_. Blocking
> signaling can, for example, cause timeouts in `erpc` to be significantly
> delayed.
""".
-moduledoc(#{since => "OTP 23.0"}).

%% Exported API

-export([call/2,
         call/3,
         call/4,
         call/5,
         cast/2,
	 cast/4,
         send_request/2,
         send_request/4,
         send_request/6,
         receive_response/1,
         receive_response/2,
         receive_response/3,
         wait_response/1,
         wait_response/2,
         wait_response/3,
         check_response/2,
         check_response/3,
         multicall/2,
         multicall/3,
         multicall/4,
	 multicall/5,
         multicast/2,
	 multicast/4,
         reqids_new/0,
         reqids_size/1,
         reqids_add/3,
         reqids_to_list/1]).

-export_type([request_id/0, request_id_collection/0, timeout_time/0]).

%% Internal exports (also used by the 'rpc' module)

-export([execute_call/4,
         execute_call/3,
         execute_cast/3,
         is_arg_error/4,
         trim_stack/4,
         call_result/4]).

%%------------------------------------------------------------------------

%% Nicer error stack trace...
-compile({inline,[{result,4},{collection_result,6},{timeout_value,1}]}).

-define(MAX_INT_TIMEOUT, 4294967295).
-define(IS_VALID_TMO_INT(TI_), (is_integer(TI_)
                                andalso (0 =< TI_)
                                andalso (TI_ =< ?MAX_INT_TIMEOUT))).

-doc """
- **`0..4294967295`** - Timeout relative to current time in milliseconds.

- **`infinity`** - Infinite timeout. That is, the operation will never time out.

- **`{abs, Timeout}`** - An absolute
  [Erlang monotonic time](`erlang:monotonic_time/1`) timeout in milliseconds.
  That is, the operation will time out when
  [`erlang:monotonic_time(millisecond)`](`erlang:monotonic_time/1`) returns a
  value larger than or equal to `Timeout`. `Timeout` is not allowed to identify
  a time further into the future than `4294967295` milliseconds. Identifying the
  timeout using an absolute timeout value is especially handy when you have a
  deadline for responses corresponding to a complete collection of requests
  (`t:request_id_collection/0`) , since you do not have to recalculate the
  relative time until the deadline over and over again.
""".
-type timeout_time() :: 0..?MAX_INT_TIMEOUT | 'infinity' | {abs, integer()}.

%%------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------

-doc(#{equiv => call/3}).
-doc(#{since => <<"OTP 23.0">>}).
-spec call(Node, Fun) -> Result when
      Node :: node(),
      Fun :: function(),
      Result :: term().

call(N, Fun) ->
    call(N, Fun, infinity).

-doc """
The same as calling
[`erpc:call(Node, erlang, apply, [Fun,[]], Timeout)`](`call/5`). May raise all
the same exceptions as [`call/5`](`call/5`) plus an `{erpc, badarg}` `error`
exception if `Fun` is not a fun of zero arity.

The call `erpc:call(Node,Fun)` is the same as the call
`erpc:call(Node,Fun,infinity)`.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec call(Node, Fun, Timeout) -> Result when
      Node :: node(),
      Fun :: function(),
      Timeout :: timeout_time(),
      Result :: term().

call(N, Fun, Timeout) when is_function(Fun, 0) ->
    call(N, erlang, apply, [Fun, []], Timeout);
call(_N, _Fun, _Timeout) ->
    error({?MODULE, badarg}).

-doc(#{equiv => call/5}).
-doc(#{since => <<"OTP 23.0">>}).
-spec call(Node, Module, Function, Args) -> Result when
      Node :: node(),
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Result :: term().

call(N, M, F, A) ->
    call(N, M, F, A, infinity).

-dialyzer([{nowarn_function, call/5}, no_return]).

-doc """
Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node` and
returns the corresponding value `Result`. `Timeout` sets an upper time limit for
the `call` operation to complete.

The call `erpc:call(Node, Module, Function, Args)` is equivalent to the call
`erpc:call(Node, Module, Function, Args, infinity)`

The `call()` function only returns if the applied function successfully returned
without raising any uncaught exceptions, the operation did not time out, and no
failures occurred. In all other cases an exception is raised. The following
exceptions, listed by exception class, can currently be raised by `call()`:

- **`throw`** - The applied function called [`throw(Value)`](`throw/1`) and did
  not catch this exception. The exception reason `Value` equals the argument
  passed to [`throw/1`](`throw/1`).

- **`exit`** - Exception reason:

  - **`{exception, ExitReason}`** - The applied function called
    [`exit(ExitReason)`](`exit/1`) and did not catch this exception. The exit
    reason `ExitReason` equals the argument passed to [`exit/1`](`exit/1`).

  - **`{signal, ExitReason}`** - The process that applied the function received
    an exit signal and terminated due to this signal. The process terminated
    with exit reason `ExitReason`.

- **`error`** - Exception reason:

  - **`{exception, ErrorReason, StackTrace}`** - A runtime error occurred which
    raised an error exception while applying the function, and the applied
    function did not catch the exception. The error reason `ErrorReason`
    indicates the type of error that occurred. `StackTrace` is formatted as when
    caught in a `try/catch` construct. The `StackTrace` is limited to the
    applied function and functions called by it.

  - **`{erpc, ERpcErrorReason}`** - The `erpc` operation failed. The following
    `ERpcErrorReason`s are the most common ones:

    - **`badarg`** - If any one of these are true:

      - `Node` is not an atom.
      - `Module` is not an atom.
      - `Function` is not an atom.
      - `Args` is not a list. Note that the list is not verified to be a proper
        list at the client side.
      - `Timeout` is invalid.

    - **`noconnection`** - The connection to `Node` was lost or could not be
      established. The function may or may not be applied.

    - **`system_limit`** - The `erpc` operation failed due to some system limit
      being reached. This typically due to failure to create a process on the
      remote node `Node`, but can be other things as well.

    - **`timeout`** - The `erpc` operation timed out. The function may or may
      not be applied.

    - **`notsup`** - The remote node `Node` does not support this `erpc`
      operation.

If the `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a timeout or a connection loss), the caller will not receive
any further information about the result if/when the applied function completes.
If the applied function explicitly communicates with the calling process, such
communication may, of course, reach the calling process.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be the calling process itself, a server, or a freshly
> spawned process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec call(Node, Module, Function, Args, Timeout) -> Result when
      Node :: node(),
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Timeout :: timeout_time(),
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
call(N, M, F, A, T) when is_atom(N),
                         is_atom(M),
                         is_atom(F),
                         is_list(A) ->
    Timeout = timeout_value(T),
    Res = make_ref(),
    ReqId = spawn_request(N, ?MODULE, execute_call, [Res, M, F, A],
                          [{reply, error_only}, monitor]),
    receive
        {spawn_reply, ReqId, error, Reason} ->
            result(spawn_reply, ReqId, Res, Reason);
        {'DOWN', ReqId, process, _Pid, Reason} ->
            result(down, ReqId, Res, Reason)
    after Timeout ->
            result(timeout, ReqId, Res, undefined)
    end;
call(_N, _M, _F, _A, _T) ->
    error({?MODULE, badarg}).

%% Asynchronous call

-doc "An opaque request identifier. For more information see `send_request/4`.".
-opaque request_id() :: nonempty_improper_list(reference(), reference()).
-doc """
An opaque collection of request identifiers (`t:request_id/0`) where each
request identifier can be associated with a label chosen by the user. For more
information see `reqids_new/0`.
""".
-opaque request_id_collection() :: #{ reference() => [reference() | term()] }.

-doc """
The same as calling
[`erpc:send_request(Node, erlang, apply, [Fun, []])`](`send_request/4`).

Fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Fun` is not a fun of zero arity.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be a server, or a freshly spawned process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec send_request(Node, Fun) -> RequestId when
      Node :: node(),
      Fun :: function(),
      RequestId :: request_id().

send_request(N, F) when is_atom(N), is_function(F, 0) ->
    send_request(N, erlang, apply, [F, []]);
send_request(_N, _F) ->
    error({?MODULE, badarg}).

-dialyzer({no_improper_lists, send_request/4}).

-doc """
Send an asynchronous `call` request to the node `Node`.
[`send_request/4`](`send_request/4`) returns a request identifier that later is
to be passed to either `receive_response/2`, `wait_response/2`, or,
`check_response/2` in order to get the response of the call request. Besides
passing the request identifier directly to these functions, it can also be added
in a request identifier collection using `reqids_add/3`. Such a collection of
request identifiers can later be used in order to get one response corresponding
to a request in the collection by passing the collection as argument to
`receive_response/3`, `wait_response/3`, or, `check_response/3`. If you are
about to save the request identifier in a request identifier collection, you may
want to consider using `send_request/6` instead.

A call to the function `my_call(Node, Module, Function, Args, Timeout)` below is
equivalent to the call
[`erpc:call(Node, Module, Function, Args, Timeout)`](`call/5`) if one disregards
performance. `call()` can utilize a selective receive optimization which removes
the need to scan the message queue from the beginning in order to find a
matching message. The `send_request()/receive_response()` combination can,
however, not utilize this optimization.

```erlang
my_call(Node, Module, Function, Args, Timeout) ->
  RequestId = erpc:send_request(Node, Module, Function, Args),
  erpc:receive_response(RequestId, Timeout).
```

Fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be a server, or a freshly spawned process.

The same as calling
[`erpc:send_request(Node, erlang, apply, [Fun,[]]), Label, RequestIdCollection)`](`send_request/6`).

Fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Fun` is not a fun of zero arity.
- `RequestIdCollection` is detected not to be request identifier collection.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be a server, or a freshly spawned process.
""".
-doc(#{since => <<"OTP 23.0, OTP 25.0">>}).
-spec send_request(Node, Module, Function, Args) -> RequestId when
      Node :: node(),
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      RequestId :: request_id();
                  (Node, Fun, Label, RequestIdCollection) ->
          NewRequestIdCollection when
      Node :: node(),
      Fun :: function(),
      Label :: term(),
      RequestIdCollection :: request_id_collection(),
      NewRequestIdCollection :: request_id_collection().

send_request(N, M, F, A) when is_atom(N),
                              is_atom(M),
                              is_atom(F),
                              is_list(A) ->
    Res = make_ref(),
    ReqId = spawn_request(N, ?MODULE, execute_call, [Res, M, F, A],
                          [{reply, error_only}, monitor]),
    [Res|ReqId];
send_request(N, F, L, C) when is_atom(N), is_function(F, 0), is_map(C) ->
    send_request(N, erlang, apply, [F, []], L, C);
send_request(_, _, _, _) ->
    error({?MODULE, badarg}).

-dialyzer({no_improper_lists, send_request/6}).

-doc """
Send an asynchronous `call` request to the node `Node`. The `Label` will be
associated with the request identifier of the operation and added to the
returned request identifier collection `NewRequestIdCollection`. The collection
can later be used in order to get one response corresponding to a request in the
collection by passing the collection as argument to `receive_response/3`,
`wait_response/3`, or, `check_response/3`.

The same as calling
[`erpc:reqids_add`](`reqids_add/3`)([`erpc:send_request`](`send_request/4`)`(Node, Module, Function, Args), Label, RequestIdCollection)`,
but calling [`send_request/6`](`send_request/6`) is slightly more efficient.

Fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.
- `RequestIdCollection` is detected not to be request identifier collection.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be a server, or a freshly spawned process.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec send_request(Node, Module, Function, Args,
                   Label, RequestIdCollection) ->
          NewRequestIdCollection when
      Node :: node(),
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Label :: term(),
      RequestIdCollection :: request_id_collection(),
      NewRequestIdCollection :: request_id_collection().

send_request(N, M, F, A, L, C) when is_atom(N),
                                    is_atom(M),
                                    is_atom(F),
                                    is_list(A),
                                    is_map(C) ->
    Res = make_ref(),
    ReqId = spawn_request(N, ?MODULE, execute_call, [Res, M, F, A],
                          [{reply, error_only}, monitor]),
    maps:put(ReqId, [Res|L], C);
send_request(_N, _M, _F, _A, _L, _C) ->
    error({?MODULE, badarg}).

-doc """
The same as calling
[`erpc:receive_response(RequestId, infinity)`](`receive_response/2`).
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec receive_response(RequestId) -> Result when
      RequestId :: request_id(),
      Result :: term().

receive_response([Res|ReqId] = RId) when is_reference(Res),
                                         is_reference(ReqId) ->
    receive_response(RId, infinity);
receive_response(_) ->
    error({?MODULE, badarg}).

-dialyzer([{nowarn_function, receive_response/2}, no_return]).

-doc """
Receive a response to a `call` request previously made by the calling process
using `send_request/4`. `RequestId` should be the value returned from the
previously made [`send_request/4`](`send_request/4`) call, and the corresponding
response should not already have been received and handled to completion by
`receive_response()`, [`check_response/4`](`check_response/2`), or
[`wait_response/4`](`wait_response/2`).

`Timeout` sets an upper time limit on how long to wait for a response. If the
operation times out, the request identified by `RequestId` will be abandoned,
then an `{erpc, timeout}` `error` exception will be raised. That is, no response
corresponding to the request will ever be received after a timeout. If a
response is received, the `call` operation is completed and either the result is
returned or an exception is raised. The exceptions that can be raised
corresponds to the same exceptions as can be raised by `call/5`.
[`receive_response/2`](`receive_response/2`) will fail with an `{erpc, badarg}`
exception if/when an invalid `RequestId` is detected or if an invalid `Timeout`
is passed.

A call to the function `my_call(Node, Module, Function, Args, Timeout)` below is
equivalent to the call
[`erpc:call(Node, Module, Function, Args, Timeout)`](`call/5`) if one disregards
performance. `call()` can utilize a selective receive optimization which removes
the need to scan the message queue from the beginning in order to find a
matching message. The `send_request()/receive_response()` combination can,
however, not utilize this optimization.

```erlang
my_call(Node, Module, Function, Args, Timeout) ->
  RequestId = erpc:send_request(Node, Module, Function, Args),
  erpc:receive_response(RequestId, Timeout).
```

If the `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a timeout, or a connection loss), the caller will not receive
any further information about the result if/when the applied function completes.
If the applied function explicitly communicates with the calling process, such
communication may, of course, reach the calling process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec receive_response(RequestId, Timeout) -> Result when
      RequestId :: request_id(),
      Timeout :: timeout_time(),
      Result :: term().

receive_response([Res|ReqId], Tmo) when is_reference(Res),
                                        is_reference(ReqId) ->
    Timeout = timeout_value(Tmo),
    receive
        {spawn_reply, ReqId, error, Reason} ->
            result(spawn_reply, ReqId, Res, Reason);
        {'DOWN', ReqId, process, _Pid, Reason} ->
            result(down, ReqId, Res, Reason)
    after Timeout ->
            result(timeout, ReqId, Res, undefined)
    end;
receive_response(_, _) ->
    error({?MODULE, badarg}).

-dialyzer([{nowarn_function, receive_response/3}, no_return]).

-doc """
Receive a response to a `call` request corresponding to a request identifier
saved in `RequestIdCollection`. All request identifiers of `RequestIdCollection`
must correspond to requests that have been made using `send_request/4` or
`send_request/6`, and all requests must have been made by the process calling
this function.

`Label` is the label associated with the request identifier of the request that
the response corresponds to. A request identifier is associated with a label
when [adding a request identifier](`reqids_add/3`) in a
[request identifier collection](`t:request_id_collection/0`), or when sending
the request using `send_request/6`.

Compared to `receive_response/2`, the returned result associated with a specific
request identifier or an exception associated with a specific request identifier
will be wrapped in a 3-tuple. The first element of this tuple equals the value
that would have been produced by [`receive_response/2`](`receive_response/2`),
the second element equals the `Label` associated with the specific request
identifier, and the third element `NewRequestIdCollection` is a possibly
modified request identifier collection. The `error` exceptions `{erpc, badarg}`
and `{erpc, timeout}` are not associated with any specific request identifiers,
and will hence not be wrapped.

If `RequestIdCollection` is empty, the atom `no_request` will be returned.

If the operation times out, all requests identified by `RequestIdCollection`
will be abandoned, then an `{erpc, timeout}` `error` exception will be raised.
That is, no responses corresponding to any of the request identifiers in
`RequestIdCollection` will ever be received after a timeout. The difference
between [`receive_response/3`](`receive_response/3`) and `wait_response/3` is
that [`receive_response/3`](`receive_response/3`) abandons the requests at
timeout so that any potential future responses are ignored, while
[`wait_response/3`](`wait_response/3`) does not.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `RequestIdCollection` in the resulting `NewRequestIdCollection`. If
`Delete` equals `false`, `NewRequestIdCollection` will equal
`RequestIdCollection`. Note that deleting an association is not for free and
that a collection containing already handled requests can still be used by
subsequent calls to [`receive_response/3`](`receive_response/3`),
`check_response/3`, and `wait_response/3`. However, without deleting handled
associations, the above calls will not be able to detect when there are no more
outstanding requests to handle, so you will have to keep track of this some
other way than relying on a `no_request` return. Note that if you pass a
collection only containing associations of already handled or abandoned requests
to [`receive_response/3`](`receive_response/3`), it will always block until a
timeout determined by `Timeout` is triggered.

Note that a response might have been consumed uppon an `{erpc, badarg}`
exception and if so, will be lost for ever.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec receive_response(RequestIdCollection, Timeout, Delete) ->
          {Result, Label, NewRequestIdCollection} | 'no_request' when
      RequestIdCollection :: request_id_collection(),
      Timeout :: timeout_time(),
      Delete :: boolean(),
      Result :: term(),
      Label :: term(),
      NewRequestIdCollection :: request_id_collection().

receive_response(ReqIdCol, WT, Del) when map_size(ReqIdCol) == 0,
                                         is_boolean(Del) ->
    _ = timeout_value(WT),
    no_request;
receive_response(ReqIdCol, Tmo, Del) when is_map(ReqIdCol),
                                          is_boolean(Del) ->
    Timeout = timeout_value(Tmo),
    receive
        {spawn_reply, ReqId, error, Reason}
          when is_map_key(ReqId, ReqIdCol), is_reference(ReqId) ->
            collection_result(spawn_reply, ReqId, Reason, ReqIdCol, false, Del);
        {'DOWN', ReqId, process, _Pid, Reason}
          when is_map_key(ReqId, ReqIdCol), is_reference(ReqId) ->
            collection_result(down, ReqId, Reason, ReqIdCol, false, Del)
    after Timeout ->
            collection_result(timeout, ok, ok, ReqIdCol, false, Del)
    end;
receive_response(_, _, _) ->
    error({?MODULE, badarg}).

-doc """
The same as calling [`erpc:wait_response(RequestId, 0)`](`wait_response/2`).
That is, poll for a response message to a `call` request previously made by the
calling process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec wait_response(RequestId) ->
          {'response', Result} | 'no_response' when
      RequestId :: request_id(),
      Result :: term().

wait_response([Res|ReqId] = RId) when is_reference(Res),
                                      is_reference(ReqId) ->
    wait_response(RId, 0);
wait_response(_) ->
    error({?MODULE, badarg}).
    
-dialyzer([{nowarn_function, wait_response/2}, no_return]).

-doc """
Wait or poll for a response message to a `call` request previously made by the
calling process using `send_request/4`. `RequestId` should be the value returned
from the previously made `send_request()` call, and the corresponding response
should not already have been received and handled to completion by
`check_response/2`, `receive_response/2`, or `wait_response()`.

`WaitTime` sets an upper time limit on how long to wait for a response. If no
response is received before the `WaitTime` timeout has triggered, the atom
`no_response` is returned. It is valid to continue waiting for a response as
many times as needed up until a response has been received and completed by
`check_response()`, `receive_response()`, or `wait_response()`. If a response is
received, the `call` operation is completed and either the result is returned as
`{response, Result}` where `Result` corresponds to the value returned from the
applied function or an exception is raised. The exceptions that can be raised
corresponds to the same exceptions as can be raised by `call/4`. That is, no
`{erpc, timeout}` `error` exception can be raised.
[`wait_response/2`](`wait_response/2`) will fail with an `{erpc, badarg}`
exception if/when an invalid `RequestId` is detected or if an invalid `WaitTime`
is passed.

If the `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a too large wait time value, or a connection loss), the caller
will not receive any further information about the result if/when the applied
function completes. If the applied function explicitly communicates with the
calling process, such communication may, of course, reach the calling process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec wait_response(RequestId, WaitTime) ->
          {'response', Result} | 'no_response' when
      RequestId :: request_id(),
      WaitTime :: timeout_time(),
      Result :: term().

wait_response([Res|ReqId], WT) when is_reference(Res),
                                    is_reference(ReqId) ->
    Timeout = timeout_value(WT),
    receive
        {spawn_reply, ReqId, error, Reason} ->
            result(spawn_reply, ReqId, Res, Reason);
        {'DOWN', ReqId, process, _Pid, Reason} ->
            {response, result(down, ReqId, Res, Reason)}
    after Timeout ->
            no_response
    end;
wait_response(_, _) ->
    error({?MODULE, badarg}).

-doc """
Wait or poll for a response to a `call` request corresponding to a request
identifier saved in `RequestIdCollection`. All request identifiers of
`RequestIdCollection` must correspond to requests that have been made using
`send_request/4` or `send_request/6`, and all requests must have been made by
the process calling this function.

`Label` is the label associated with the request identifier of the request that
the response corresponds to. A request identifier is associated with a label
when [adding a request identifier](`reqids_add/3`) in a
[request identifier collection](`t:request_id_collection/0`), or when sending
the request using `send_request/6`.

Compared to `wait_response/2`, the returned result associated with a specific
request identifier or an exception associated with a specific request identifier
will be wrapped in a 3-tuple. The first element of this tuple equals the value
that would have been produced by [`wait_response/2`](`wait_response/2`), the
second element equals the `Label` associated with the specific request
identifier, and the third element `NewRequestIdCollection` is a possibly
modified request identifier collection. The `error` exception `{erpc, badarg}`
is not associated with any specific request identifier, and will hence not be
wrapped.

If `RequestIdCollection` is empty, `no_request` will be returned. If no response
is received before the `WaitTime` timeout has triggered, the atom `no_response`
is returned. It is valid to continue waiting for a response as many times as
needed up until a response has been received and completed by
`check_response()`, `receive_response()`, or `wait_response()`. The difference
between `receive_response/3` and [`wait_response/3`](`wait_response/3`) is that
[`receive_response/3`](`receive_response/3`) abandons requests at timeout so
that any potential future responses are ignored, while
[`wait_response/3`](`wait_response/3`) does not.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `RequestIdCollection` in the resulting `NewRequestIdCollection`. If
`Delete` equals `false`, `NewRequestIdCollection` will equal
`RequestIdCollection`. Note that deleting an association is not for free and
that a collection containing already handled requests can still be used by
subsequent calls to [`wait_response/3`](`wait_response/3`), `check_response/3`,
and `receive_response/3`. However, without deleting handled associations, the
above calls will not be able to detect when there are no more outstanding
requests to handle, so you will have to keep track of this some other way than
relying on a `no_request` return. Note that if you pass a collection only
containing associations of already handled or abandoned requests to
[`wait_response/3`](`wait_response/3`), it will always block until a timeout
determined by `WaitTime` is triggered and then return `no_response`.

Note that a response might have been consumed uppon an `{erpc, badarg}`
exception and if so, will be lost for ever.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec wait_response(RequestIdCollection, WaitTime, Delete) ->
          {{'response', Result}, Label, NewRequestIdCollection} |
          'no_response' |
          'no_request' when
      RequestIdCollection :: request_id_collection(),
      WaitTime :: timeout_time(),
      Delete :: boolean(),
      Label :: term(),
      NewRequestIdCollection :: request_id_collection(),
      Result :: term().

wait_response(ReqIdCol, WT, Del) when map_size(ReqIdCol) == 0,
                                       is_boolean(Del) ->
    _ = timeout_value(WT),
    no_request;
wait_response(ReqIdCol, WT, Del) when is_map(ReqIdCol),
                                      is_boolean(Del) ->
    Timeout = timeout_value(WT),
    receive
        {spawn_reply, ReqId, error, Reason}
          when is_map_key(ReqId, ReqIdCol), is_reference(ReqId) ->
            collection_result(spawn_reply, ReqId, Reason, ReqIdCol, true, Del);
        {'DOWN', ReqId, process, _Pid, Reason}
          when is_map_key(ReqId, ReqIdCol), is_reference(ReqId) ->
            collection_result(down, ReqId, Reason, ReqIdCol, true, Del)
    after Timeout ->
            no_response
    end;
wait_response(_, _, _) ->
    error({?MODULE, badarg}).

-dialyzer([{nowarn_function, check_response/2}, no_return]).

-doc """
Check if a message is a response to a `call` request previously made by the
calling process using `send_request/4`. `RequestId` should be the value returned
from the previously made [`send_request/4`](`send_request/4`) call, and the
corresponding response should not already have been received and handled to
completion by [`check_response/2`](`check_response/2`), `receive_response/2`, or
`wait_response/2`. `Message` is the message to check.

If `Message` does not correspond to the response, the atom `no_response` is
returned. If `Message` corresponds to the response, the `call` operation is
completed and either the result is returned as `{response, Result}` where
`Result` corresponds to the value returned from the applied function or an
exception is raised. The exceptions that can be raised corresponds to the same
exceptions as can be raised by `call/4`. That is, no `{erpc, timeout}` `error`
exception can be raised. `check_response()` will fail with an `{erpc, badarg}`
exception if/when an invalid `RequestId` is detected.

If the `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a connection loss), the caller will not receive any further
information about the result if/when the applied function completes. If the
applied function explicitly communicates with the calling process, such
communication may, of course, reach the calling process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec check_response(Message, RequestId) ->
          {'response', Result} | 'no_response' when
      Message :: term(),
      RequestId :: request_id(),
      Result :: term().
          
check_response({spawn_reply, ReqId, error, Reason},
               [Res|ReqId]) when is_reference(Res),
                                 is_reference(ReqId) ->
    result(spawn_reply, ReqId, Res, Reason);
check_response({'DOWN', ReqId, process, _Pid, Reason},
               [Res|ReqId]) when is_reference(Res),
                                 is_reference(ReqId) ->
    {response, result(down, ReqId, Res, Reason)};
check_response(_Msg, [Res|ReqId]) when is_reference(Res),
                                       is_reference(ReqId) ->
    no_response;
check_response(_, _) ->
    error({?MODULE, badarg}).

-doc """
Check if a message is a response to a `call` request corresponding to a request
identifier saved in `RequestIdCollection`. All request identifiers of
`RequestIdCollection` must correspond to requests that have been made using
`send_request/4` or `send_request/6`, and all requests must have been made by
the process calling this function.

`Label` is the label associated with the request identifier of the request that
the response corresponds to. A request identifier is associated with a label
when [adding a request identifier](`reqids_add/3`) in a
[request identifier collection](`t:request_id_collection/0`), or when sending
the request using `send_request/6`.

Compared to `check_response/2`, the returned result associated with a specific
request identifier or an exception associated with a specific request identifier
will be wrapped in a 3-tuple. The first element of this tuple equals the value
that would have been produced by [`check_response/2`](`check_response/2`), the
second element equals the `Label` associated with the specific request
identifier, and the third element `NewRequestIdCollection` is a possibly
modified request identifier collection. The `error` exception `{erpc, badarg}`
is not associated with any specific request identifier, and will hence not be
wrapped.

If `RequestIdCollection` is empty, the atom `no_request` will be returned. If
`Message` does not correspond to any of the request identifiers in
`RequestIdCollection`, the atom `no_response` is returned.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `RequestIdCollection` in the resulting `NewRequestIdCollection`. If
`Delete` equals `false`, `NewRequestIdCollection` will equal
`RequestIdCollection`. Note that deleting an association is not for free and
that a collection containing already handled requests can still be used by
subsequent calls to [`check_response/3`](`check_response/3`),
`receive_response/3`, and `wait_response/3`. However, without deleting handled
associations, the above calls will not be able to detect when there are no more
outstanding requests to handle, so you will have to keep track of this some
other way than relying on a `no_request` return. Note that if you pass a
collection only containing associations of already handled or abandoned requests
to [`check_response/3`](`check_response/3`), it will always return
`no_response`.

Note that a response might have been consumed uppon an `{erpc, badarg}`
exception and if so, will be lost for ever.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec check_response(Message, RequestIdCollection, Delete) ->
          {{'response', Result}, Label, NewRequestIdCollection} |
          'no_response' |
          'no_request' when
      Message :: term(),
      RequestIdCollection :: request_id_collection(),
      Delete :: boolean(),
      Result :: term(),
      Label :: term(),
      NewRequestIdCollection :: request_id_collection().

check_response(_Msg, ReqIdCol, Del) when map_size(ReqIdCol) == 0,
                                         is_boolean(Del) ->
    no_request;
check_response({spawn_reply, ReqId, error, Reason},
               ReqIdCol, Del) when is_reference(ReqId),
                                   is_map_key(ReqId, ReqIdCol),
                                   is_boolean(Del) ->
    collection_result(spawn_reply, ReqId, Reason, ReqIdCol, true, Del);
check_response({'DOWN', ReqId, process, _Pid, Reason},
               ReqIdCol, Del) when is_reference(ReqId),
                                   is_map_key(ReqId, ReqIdCol),
                                   is_boolean(Del) ->
    collection_result(down, ReqId, Reason, ReqIdCol, true, Del);
check_response(_Msg, ReqIdCol, Del) when is_map(ReqIdCol),
                                         is_boolean(Del) ->
    no_response;
check_response(_, _, _) ->
    error({?MODULE, badarg}).

-doc """
Returns a new empty request identifier collection. A request identifier
collection can be utilized in order the handle multiple outstanding requests.

Request identifiers of requests made by `send_request/4` can be saved in a
request identifier collection using `reqids_add/3`. Such a collection of request
identifiers can later be used in order to get one response corresponding to a
request in the collection by passing the collection as argument to
`check_response/3`, `receive_response/3`, and `wait_response/3`.

`reqids_size/1` can be used to determine the amount of request identifiers in a
request identifier collection.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_new() ->
          NewRequestIdCollection::request_id_collection().

reqids_new() ->
    maps:new().

-doc "Returns the amount of request identifiers saved in `RequestIdCollection`.".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_size(RequestIdCollection::request_id_collection()) ->
          non_neg_integer().
reqids_size(ReqIdCollection) ->
    try
        maps:size(ReqIdCollection)
    catch
        _:_ ->
            error({?MODULE, badarg})
    end.

-dialyzer({no_improper_lists, reqids_add/3}).

-doc """
Saves `RequestId` and associates a `Label` with the request identifier by adding
this information to `RequestIdCollection` and returning the resulting request
identifier collection.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_add(RequestId::request_id(), Label::term(),
                 RequestIdCollection::request_id_collection()) ->
          NewRequestIdCollection::request_id_collection().

reqids_add([_|ReqId], _, ReqIdCollection) when is_reference(ReqId),
                                               is_map_key(ReqId,
                                                          ReqIdCollection) ->
    error({?MODULE, badarg});
reqids_add([Res|ReqId], Label, ReqIdCollection) when is_reference(Res),
                                                     is_reference(ReqId),
                                                     is_map(ReqIdCollection) ->
    maps:put(ReqId, [Res|Label], ReqIdCollection);
reqids_add(_, _, _) ->
    error({?MODULE, badarg}).

-dialyzer({no_improper_lists, reqids_to_list/1}).

-doc """
Returns a list of `{RequestId, Label}` tuples which corresponds to all request
identifiers with their associated labels present in the `RequestIdCollection`
collection.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_to_list(RequestIdCollection::request_id_collection()) ->
          [{RequestId::request_id(), Label::term()}].

reqids_to_list(ReqIdCollection) when is_map(ReqIdCollection) ->
    try
        maps:fold(fun (ReqId, [Res|Label], Acc) when is_reference(ReqId),
                                                     is_reference(Res) ->
                          [{[Res|ReqId], Label}|Acc];
                      (_, _, _) ->
                          throw(badarg)
                  end,
                  [],
                  ReqIdCollection)
    catch
        throw:badarg ->
            error({?MODULE, badarg})
    end;
reqids_to_list(_) ->
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


-doc(#{equiv => multicall/3}).
-doc(#{since => <<"OTP 23.0">>}).
-spec multicall(Nodes, Fun) -> Result when
      Nodes :: [atom()],
      Fun :: function(),
      Result :: term().

multicall(Ns, Fun) ->
    multicall(Ns, Fun, infinity).

-doc """
The same as calling
[`erpc:multicall(Nodes, erlang, apply, [Fun,[]], Timeout)`](`multicall/5`). May
raise all the same exceptions as [`multicall/5`](`multicall/5`) plus an
`{erpc, badarg}` `error` exception if `Fun` is not a fun of zero arity.

The call `erpc:multicall(Nodes,Fun)` is the same as the call
`erpc:multicall(Nodes,Fun, infinity)`.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec multicall(Nodes, Fun, Timeout) -> Result when
      Nodes :: [atom()],
      Fun :: function(),
      Timeout :: timeout_time(),
      Result :: term().

multicall(Ns, Fun, Timeout) when is_function(Fun, 0) ->
    multicall(Ns, erlang, apply, [Fun, []], Timeout);
multicall(_Ns, _Fun, _Timeout) ->
    error({?MODULE, badarg}).

-doc(#{equiv => multicall/5}).
-doc(#{since => <<"OTP 23.0">>}).
-spec multicall(Nodes, Module, Function, Args) -> Result when
      Nodes :: [atom()],
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Result :: [{ok, ReturnValue :: term()} | caught_call_exception()].

multicall(Ns, M, F, A) ->
    multicall(Ns, M, F, A, infinity).

-doc """
Performs multiple `call` operations in parallel on multiple nodes. That is,
evaluates [`apply(Module, Function, Args)`](`apply/3`) on the nodes `Nodes` in
parallel. `Timeout` sets an upper time limit for all `call` operations to
complete. The result is returned as a list where the result from each node is
placed at the same position as the node name is placed in `Nodes`. Each item in
the resulting list is formatted as either:

- **`{ok, Result}`** - The `call` operation for this specific node returned
  `Result`.

- **`{Class, ExceptionReason}`** - The `call` operation for this specific node
  raised an exception of class `Class` with exception reason `ExceptionReason`.
  These correspond to the exceptions that `call/5` can raise.

[`multicall/5`](`multicall/5`) fails with an `{erpc, badarg}` `error` exception
if:

- `Nodes` is not a proper list of atoms. Note that some requests may already
  have been sent when the failure occurs. That is, the function may or may not
  be applied on some nodes.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.

The call `erpc:multicall(Nodes, Module, Function, Args)` is equivalent to the
call `erpc:multicall(Nodes, Module, Function, Args, infinity)`. These calls are
also equivalent to calling `my_multicall(Nodes, Module, Function, Args)` below
if one disregard performance and failure behavior. `multicall()` can utilize a
selective receive optimization which removes the need to scan the message queue
from the beginning in order to find a matching message. The
`send_request()/receive_response()` combination can, however, not utilize this
optimization.

```erlang
my_multicall(Nodes, Module, Function, Args) ->
  ReqIds = lists:map(fun (Node) ->
                       erpc:send_request(Node, Module, Function, Args)
                     end,
                     Nodes),
  lists:map(fun (ReqId) ->
              try
                {ok, erpc:receive_response(ReqId, infinity)}
              catch
                Class:Reason ->
                  {Class, Reason}
              end
            end,
            ReqIds).
```

If an `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a timeout, connection loss, or an improper `Nodes` list), the
caller will not receive any further information about the result if/when the
applied function completes. If the applied function communicates with the
calling process, such communication may, of course, reach the calling process.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be the calling process itself, a server, or a freshly
> spawned process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec multicall(Nodes, Module, Function, Args, Timeout) -> Result when
      Nodes :: [atom()],
      Module :: atom(),
      Function :: atom(),
      Args :: [term()],
      Timeout :: timeout_time(),
      Result :: [{ok, ReturnValue :: term()} | caught_call_exception()].

multicall(Ns, M, F, A, T) ->
    try
        true = is_atom(M),
        true = is_atom(F),
        true = is_list(A),
        Tag = make_ref(),
        Timeout = timeout_value(T),
        SendState = mcall_send_requests(Tag, Ns, M, F, A, Timeout),
        mcall_receive_replies(Tag, SendState)
    catch
        error:NotIErr when NotIErr /= internal_error ->
            error({?MODULE, badarg})
    end.

-doc """
The same as calling
[`erpc:multicast(Nodes,erlang,apply,[Fun,[]])`](`multicast/4`).

[`multicast/2`](`multicast/2`) fails with an `{erpc, badarg}` `error` exception
if:

- `Nodes` is not a proper list of atoms.
- `Fun` is not a a fun of zero arity.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec multicast(Nodes, Fun) -> 'ok' when
      Nodes :: [node()],
      Fun :: function().

multicast(N, Fun) ->
    multicast(N, erlang, apply, [Fun, []]).

-doc """
Evaluates [`apply(Module, Function, Args)`](`apply/3`) on the nodes `Nodes`. No
response is delivered to the calling process. `multicast()` returns immediately
after the cast requests have been sent. Any failures beside bad arguments are
silently ignored.

[`multicast/4`](`multicast/4`) fails with an `{erpc, badarg}` `error` exception
if:

- `Nodes` is not a proper list of atoms. Note that some requests may already
  have been sent when the failure occurs. That is, the function may or may not
  be applied on some nodes.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be a server, or a freshly spawned process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec multicast(Nodes, Module, Function, Args) -> 'ok' when
      Nodes :: [node()],
      Module :: atom(),
      Function :: atom(),
      Args :: [term()].

multicast(Nodes, Mod, Fun, Args) ->
    try
        true = is_atom(Mod),
        true = is_atom(Fun),
        true = is_list(Args),
        multicast_send_requests(Nodes, Mod, Fun, Args)
    catch
        error:_ ->
            error({?MODULE, badarg})
    end.

multicast_send_requests([], _Mod, _Fun, _Args) ->
    ok;
multicast_send_requests([Node|Nodes], Mod, Fun, Args) ->
    _ = spawn_request(Node, ?MODULE, execute_cast, [Mod, Fun, Args],
                      [{reply, no}]),
    multicast_send_requests(Nodes, Mod, Fun, Args).

-doc """
The same as calling [`erpc:cast(Node,erlang,apply,[Fun,[]])`](`cast/4`).

[`cast/2`](`cast/2`) fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Fun` is not a a fun of zero arity.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec cast(Node, Fun) -> 'ok' when
      Node :: node(),
      Fun :: function().

cast(N, Fun) ->
    cast(N, erlang, apply, [Fun, []]).

-doc """
Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node`. No
response is delivered to the calling process. `cast()` returns immediately after
the cast request has been sent. Any failures beside bad arguments are silently
ignored.

[`cast/4`](`cast/4`) fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be a server, or a freshly spawned process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec cast(Node, Module, Function, Args) -> 'ok' when
      Node :: node(),
      Module :: atom(),
      Function :: atom(),
      Args :: [term()].

cast(Node, Mod, Fun, Args) when is_atom(Node),
                                is_atom(Mod),
                                is_atom(Fun),
                                is_list(Args) ->
    _ = spawn_request(Node, ?MODULE, execute_cast, [Mod, Fun, Args],
                      [{reply, no}]),
    ok;
cast(_Node, _Mod, _Fun, _Args) ->
    error({?MODULE, badarg}).

%%------------------------------------------------------------------------
%% Exported internals
%%------------------------------------------------------------------------

%% Note that most of these are used by 'rpc' as well...

-doc false.
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

-doc false.
execute_call(M,F,A) ->
    {return, apply(M, F, A)}.

-doc false.
execute_cast(M, F, A) ->
    try
        apply(M, F, A)
    catch
        error:Reason:Stack ->
            %% Produce error reports with error
            %% exceptions produced for calls...
            case is_arg_error(Reason, M, F, A) of
                true ->
                    error({?MODULE, Reason});
                false ->
                    ErpcStack = trim_stack(Stack, M, F, A),
                    error({exception, {Reason, ErpcStack}})
            end
    end.

-doc false.
call_result(Type, ReqId, Res, Reason) ->
    result(Type, ReqId, Res, Reason).

-doc false.
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

-define(IS_CUT_FRAME(F),
        ((element(1, (F)) == ?MODULE)
         andalso ((element(2, (F)) == execute_call)
                  orelse ((element(2, (F)) == execute_cast))))).

-doc false.
trim_stack([CF | _], M, F, A) when ?IS_CUT_FRAME(CF) ->
    [{M, F, A, []}];
trim_stack([{M, F, A, _} = SF, CF | _], M, F, A) when ?IS_CUT_FRAME(CF) ->
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
trim_stack_aux([{M, F, AL, _} = SF, CF | _], M, F, A) when ?IS_CUT_FRAME(CF),
                                                            AL == length(A) ->
    [SF];
trim_stack_aux([CF | _], M, F, A) when ?IS_CUT_FRAME(CF) ->
    try
        [{M, F, length(A), []}]
    catch
        _:_ ->
            []
    end;
trim_stack_aux([SF|SFs], M, F, A) ->
    [SF|trim_stack_aux(SFs, M, F, A)].

call_abandon(ReqId) ->
    case spawn_request_abandon(ReqId) of
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

collection_result(timeout, _, _, ReqIdCollection, _, _) ->
    Abandon = fun (ReqId, [Res|_Label]) when is_reference(ReqId),
                                             is_reference(Res) ->
                      case call_abandon(ReqId) of
                          true ->
                              ok;
                          false ->
                              %% Spawn error or DOWN has arrived if
                              %% ReqId corresponds to an outstanding
                              %% request; fetch and drop it...
                              receive
                                  {spawn_reply, ReqId, error, _} ->
                                      ok;
                                  {'DOWN', ReqId, process, _, _} ->
                                      ok
                              after
                                  0 ->
                                      ok %% Already handled...
                              end
                      end;
                  (_, _) ->
                      %% Invalid request id collection...
                      throw(badarg)
              end,
    try
        maps:foreach(Abandon, ReqIdCollection)
    catch
        throw:badarg -> error({?MODULE, badarg})
    end,
    error({?MODULE, timeout});
collection_result(Type, ReqId, ResultReason, ReqIdCol, WrapResponse, Delete) ->
    ReqIdInfo = case Delete of
                    true -> maps:take(ReqId, ReqIdCol);
                    false -> {maps:get(ReqId, ReqIdCol), ReqIdCol}
                end,
    case ReqIdInfo of
        {[Res|Label], NewReqIdCol} when is_reference(Res) ->
            try
                Result = result(Type, ReqId, Res, ResultReason),
                Response = if WrapResponse -> {response, Result};
                              true -> Result
                           end,
                {Response, Label, NewReqIdCol}
            catch
                Class:Reason ->
                    erlang:Class({Reason, Label, NewReqIdCol})
            end;
        _ ->
            %% Invalid request id collection...
            error({?MODULE, badarg})
    end.

timeout_value(infinity) ->
    infinity;
timeout_value(Timeout) when ?IS_VALID_TMO_INT(Timeout) ->
    Timeout;
timeout_value({abs, Timeout}) when is_integer(Timeout) ->
    case Timeout - erlang:monotonic_time(millisecond) of
        TMO when TMO < 0 ->
            0;
        TMO when TMO > ?MAX_INT_TIMEOUT ->
            error({?MODULE, badarg});
        TMO ->
            TMO
    end;
timeout_value(_) ->
    error({?MODULE, badarg}).

deadline(infinity) ->
    infinity;
deadline(?MAX_INT_TIMEOUT) ->
    erlang:convert_time_unit(erlang:monotonic_time(millisecond)
                             + ?MAX_INT_TIMEOUT,
                             millisecond,
                             native);
deadline(T) when ?IS_VALID_TMO_INT(T) ->
    Now = erlang:monotonic_time(),
    NativeTmo = erlang:convert_time_unit(T, millisecond, native),
    Now + NativeTmo.

time_left(infinity) ->
    infinity;
time_left(expired) ->
    0;
time_left(Deadline) ->
    case Deadline - erlang:monotonic_time() of
        TimeLeft when TimeLeft =< 0 ->
            0;
        TimeLeft ->
            erlang:convert_time_unit(TimeLeft-1, native, millisecond) + 1
    end.

mcall_local_call(M, F, A) ->
    try
        {return, Return} = execute_call(M, F, A),
        {ok, Return}
    catch
        throw:Thrown ->
            {throw, Thrown};
        exit:Reason ->
            {exit, {exception, Reason}};
        error:Reason:Stack ->
            case is_arg_error(Reason, M, F, A) of
                true ->
                    {error, {?MODULE, Reason}};
                false ->
                    ErpcStack = trim_stack(Stack, M, F, A),
                    {error, {exception, Reason, ErpcStack}}
            end
    end.

mcall_send_request(T, N, M, F, A) when is_reference(T),
                                       is_atom(N),
                                       is_atom(M),
                                       is_atom(F),
                                       is_list(A) ->
    spawn_request(N, ?MODULE, execute_call, [T, M, F, A],
                  [{reply, error_only},
                   {reply_tag, T},
                   {monitor, [{tag, T}]}]).

mcall_send_requests(Tag, Ns, M, F, A, Tmo) ->
    DL = deadline(Tmo),
    mcall_send_requests(Tag, Ns, M, F, A, [], DL, undefined, 0).

mcall_send_requests(_Tag, [], M, F, A, RIDs, DL, local_call, NRs) ->
    %% Timeout infinity and call on local node wanted;
    %% execute local call in this process...
    LRes = mcall_local_call(M, F, A),
    {ok, RIDs, #{local_call => LRes}, NRs, DL};
mcall_send_requests(_Tag, [], _M, _F, _A, RIDs, DL, _LC, NRs) ->
    {ok, RIDs, #{}, NRs, DL};
mcall_send_requests(Tag, [N|Ns], M, F, A, RIDs,
                    infinity, undefined, NRs) when N == node() ->
    mcall_send_requests(Tag, Ns, M, F, A, [local_call|RIDs],
                        infinity, local_call, NRs);
mcall_send_requests(Tag, [N|Ns], M, F, A, RIDs, DL, LC, NRs) ->
    try mcall_send_request(Tag, N, M, F, A) of
        RID ->
            mcall_send_requests(Tag, Ns, M, F, A, [RID|RIDs],
                                DL, LC, NRs+1)
    catch
        _:_ ->
            %% Bad argument... Abandon requests and cleanup
            %% any responses by receiving replies with a zero
            %% timeout and then fail...
            {badarg, RIDs, #{}, NRs, expired}
    end;
mcall_send_requests(_Tag, _Ns, _M, _F, _A, RIDs, _DL, _LC, NRs) ->
    %% Bad nodes list... Abandon requests and cleanup any responses
    %% by receiving replies with a zero timeout and then fail...
    {badarg, RIDs, #{}, NRs, expired}.

mcall_receive_replies(Tag, {SendRes, RIDs, Rpls, NRs, DL}) ->
    ResRpls = mcall_receive_replies(Tag, RIDs, Rpls, NRs, DL),
    if SendRes /= ok ->
            error(SendRes); %% Cleanup done; fail...
       true ->
            mcall_map_replies(RIDs, ResRpls, [])
    end.

mcall_receive_replies(_Tag, _ReqIds, Rpls, 0, _DL) ->
    Rpls;
mcall_receive_replies(Tag, ReqIDs, Rpls, NRs, DL) ->
    Tmo = time_left(DL),
    receive
        {Tag, ReqId, error, Reason} ->
            Res = mcall_result(spawn_reply, ReqId, Tag, Reason),
            mcall_receive_replies(Tag, ReqIDs, Rpls#{ReqId => Res},
                                  NRs-1, DL);
        {Tag, ReqId, process, _Pid, Reason} ->
            Res = mcall_result(down, ReqId, Tag, Reason),
            mcall_receive_replies(Tag, ReqIDs, Rpls#{ReqId => Res},
                                  NRs-1, DL)
    after Tmo ->
            if ReqIDs == [] ->
                    Rpls;
               true ->
                    NewNRs = mcall_abandon(Tag, ReqIDs, Rpls, NRs),
                    mcall_receive_replies(Tag, [], Rpls, NewNRs, expired)
            end
    end.

mcall_result(ResType, ReqId, Tag, ResultReason) ->
    try
        {ok, result(ResType, ReqId, Tag, ResultReason)}
    catch
        Class:Reason ->
            {Class, Reason}
    end.

mcall_abandon(_Tag, [], _Rpls, NRs) ->
    NRs;
mcall_abandon(Tag, [local_call | RIDs], Rpls, NRs) ->
    mcall_abandon(Tag, RIDs, Rpls, NRs);
mcall_abandon(Tag, [RID | RIDs], Rpls, NRs) ->
    NewNRs = case maps:is_key(RID, Rpls) of
                 true ->
                     NRs;
                 false ->
                     case call_abandon(RID) of
                         true -> NRs-1;
                         false -> NRs
                     end
             end,
    mcall_abandon(Tag, RIDs, Rpls, NewNRs).

mcall_map_replies([], _Rpls, Res) ->
    Res;
mcall_map_replies([RID|RIDs], Rpls, Res) ->
    Timeout = {error, {?MODULE, timeout}},
    mcall_map_replies(RIDs, Rpls, [maps:get(RID, Rpls, Timeout) | Res]).
    
