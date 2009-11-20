%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

-module(seq_trace).

-define(SEQ_TRACE_SEND, 1).       %(1 << 0)
-define(SEQ_TRACE_RECEIVE, 2).    %(1 << 1)
-define(SEQ_TRACE_PRINT, 4).      %(1 << 2)
-define(SEQ_TRACE_TIMESTAMP, 8).  %(1 << 3)

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

-type flag()       :: 'send' | 'receive' | 'print' | 'timestamp'.
-type component()  :: 'label' | 'serial' | flag().
-type value()      :: non_neg_integer()
                    | {non_neg_integer(), non_neg_integer()}
                    | boolean().
-type token_pair() :: {component(), value()}.

%%---------------------------------------------------------------------------

-type token() :: [] | {integer(), boolean(), _, _, _}.
-spec set_token(token()) -> token() | 'ok'.

set_token([]) ->
    erlang:seq_trace(sequential_trace_token,[]);
set_token({Flags,Label,Serial,_From,Lastcnt}) ->
    F = decode_flags(Flags),
    set_token2([{label,Label},{serial,{Lastcnt, Serial}} | F]).

%% We limit the label type to always be a small integer because erl_interface
%% expects that, the BIF can however "unofficially" handle atoms as well, and
%% atoms can be used if only Erlang nodes are involved

-spec set_token(component(), value()) -> token_pair().

set_token(Type, Val) ->
    erlang:seq_trace(Type, Val).

-spec get_token() -> term().

get_token() ->
    element(2,process_info(self(),sequential_trace_token)).

-spec get_token(component()) -> token_pair().

get_token(Type) ->
    erlang:seq_trace_info(Type).

-spec print(term()) -> 'ok'.

print(Term) ->
    erlang:seq_trace_print(Term),
    ok.

-spec print(integer(), term()) -> 'ok'.

print(Label, Term) when is_atom(Label) ->
    erlang:error(badarg, [Label, Term]);
print(Label, Term) ->
    erlang:seq_trace_print(Label, Term),
    ok.

-spec reset_trace() -> 'true'.

reset_trace() ->
    erlang:system_flag(1, 0).

%% reset_trace(Pid) -> % this might be a useful function too

-type tracer() :: pid() | port() | 'false'.

-spec set_system_tracer(tracer()) -> tracer().

set_system_tracer(Pid) ->
    erlang:system_flag(sequential_tracer, Pid).

-spec get_system_tracer() -> tracer().

get_system_tracer() ->
    element(2, erlang:system_info(sequential_tracer)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal help functions 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_token2([{Type,Val}|T]) ->
    erlang:seq_trace(Type, Val),
    set_token2(T);
set_token2([]) ->
    ok.

decode_flags(Flags) ->
    Print = (Flags band ?SEQ_TRACE_PRINT) > 0,
    Send = (Flags band ?SEQ_TRACE_SEND) > 0,
    Rec = (Flags band ?SEQ_TRACE_RECEIVE) > 0,
    Ts = (Flags band ?SEQ_TRACE_TIMESTAMP) > 0,
    [{print,Print},{send,Send},{'receive',Rec},{timestamp,Ts}].
