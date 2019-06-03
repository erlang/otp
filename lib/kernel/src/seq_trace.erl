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

-module(seq_trace).

-define(SEQ_TRACE_SEND, 1).       %(1 << 0)
-define(SEQ_TRACE_RECEIVE, 2).    %(1 << 1)
-define(SEQ_TRACE_PRINT, 4).      %(1 << 2)
-define(SEQ_TRACE_NOW_TIMESTAMP, 8). %(1 << 3)
-define(SEQ_TRACE_STRICT_MON_TIMESTAMP, 16). %(1 << 4)
-define(SEQ_TRACE_MON_TIMESTAMP, 32). %(1 << 5)

-export([set_token/1,
	 set_token/2,
	 get_token/0,
	 get_token/1,
	 print/1,
	 print/2,
	 reset_trace/0,
	 set_system_tracer/1,
	 get_system_tracer/0]).

%%---------------------------------------------------------------------------

-type flag()       :: 'send' | 'receive' | 'print' | 'timestamp' | 'monotonic_timestamp' | 'strict_monotonic_timestamp'.
-type component()  :: 'label' | 'serial' | flag().
-type value()      :: (Label :: term())
                    | {Previous :: non_neg_integer(),
                       Current :: non_neg_integer()}
                    | (Bool :: boolean()).

%%---------------------------------------------------------------------------

-type token() :: {integer(), boolean(), _, _, _}.
-spec set_token(Token) -> PreviousToken | 'ok' when
      Token :: [] | token(),
      PreviousToken :: [] | token().

set_token([]) ->
    erlang:seq_trace(sequential_trace_token,[]);
set_token({Flags,Label,Serial,_From,Lastcnt}) ->
    F = decode_flags(Flags),
    set_token2([{label,Label},{serial,{Lastcnt, Serial}} | F]).

-spec set_token(Component, Val) -> OldVal when
      Component :: component(),
      Val :: value(),
      OldVal :: value().

set_token(Type, Val) ->
    erlang:seq_trace(Type, Val).

-spec get_token() -> [] | token().

get_token() ->
    element(2,process_info(self(),sequential_trace_token)).

-spec get_token(Component) -> {Component, Val} when
      Component :: component(),
      Val :: value().
get_token(Type) ->
    erlang:seq_trace_info(Type).

-spec print(TraceInfo) -> 'ok' when
      TraceInfo :: term().

print(Term) ->
    erlang:seq_trace_print(Term),
    ok.

-spec print(Label, TraceInfo) -> 'ok' when
      Label :: integer(),
      TraceInfo :: term().

print(Label, Term) when is_atom(Label) ->
    erlang:error(badarg, [Label, Term]);
print(Label, Term) ->
    erlang:seq_trace_print(Label, Term),
    ok.

-spec reset_trace() -> 'true'.

reset_trace() ->
    erlang:system_flag(reset_seq_trace, true).

%% reset_trace(Pid) -> % this might be a useful function too

-type tracer() :: (Pid :: pid()) | port() |
                  (TracerModule :: {module(), term()}) |
                  'false'.

-spec set_system_tracer(Tracer) -> OldTracer when
      Tracer :: tracer(),
      OldTracer :: tracer().

set_system_tracer({Module, State} = Tracer) ->
    case erlang:module_loaded(Module) of
        false ->
            Module:enabled(trace_status, erlang:self(), State);
        true ->
            ok
    end,
    erlang:system_flag(sequential_tracer, Tracer);
set_system_tracer(Tracer) ->
    erlang:system_flag(sequential_tracer, Tracer).

-spec get_system_tracer() -> Tracer when
      Tracer :: tracer().

get_system_tracer() ->
    element(2, erlang:system_info(sequential_tracer)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal help functions 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_token2([{Type,Val}|T]) ->
    _ = erlang:seq_trace(Type, Val),
    set_token2(T);
set_token2([]) ->
    ok.

decode_flags(Flags) ->
    Print = (Flags band ?SEQ_TRACE_PRINT) > 0,
    Send = (Flags band ?SEQ_TRACE_SEND) > 0,
    Rec = (Flags band ?SEQ_TRACE_RECEIVE) > 0,
    NowTs = (Flags band ?SEQ_TRACE_NOW_TIMESTAMP) > 0,
    StrictMonTs = (Flags band ?SEQ_TRACE_STRICT_MON_TIMESTAMP) > 0,
    MonTs = (Flags band ?SEQ_TRACE_MON_TIMESTAMP) > 0,
    [{print,Print},{send,Send},{'receive',Rec},{timestamp,NowTs},
     {strict_monotonic_timestamp, StrictMonTs},
     {monotonic_timestamp, MonTs}].
