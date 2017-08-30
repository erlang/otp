%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(orber_test_server_impl).
-include_lib("orber/include/corba.hrl").
-include("idl_output/orber_test_server.hrl").

%%--------------- specified functions ------------------------
-export([stop_normal/2, 
	 stop_brutal/2, 
	 print/2, 
	 %% Testing code and decode arguments
	 testing_iiop_float/3,
	 testing_iiop_double/3,
	 testing_iiop_short/3,
	 testing_iiop_ushort/3,
	 testing_iiop_long/3,
	 testing_iiop_longlong/3,
	 testing_iiop_ulong/3,
	 testing_iiop_ulonglong/3,
	 testing_iiop_char/3,
	 testing_iiop_wchar/3,
	 testing_iiop_bool/3,
	 testing_iiop_octet/3,
	 testing_iiop_any/3,
	 testing_iiop_obj/3,
	 testing_iiop_string/3,
	 testing_iiop_wstring/3,
	 testing_iiop_struct/3,
	 testing_iiop_union/3,
	 testing_iiop_union_d/3,
	 testing_iiop_enum/3,
	 testing_iiop_seq/3,
	 testing_iiop_uni_seq/3,
	 testing_iiop_struc_seq/3,
	 testing_iiop_array/3,
	 testing_iiop_fixed/3,
	 testing_iiop_void/2,
	 testing_iiop_context/2,
	 testing_iiop_server_marshal/3,
	 testing_iiop_rec_any/3,
	 testing_iiop_rec_struct/3,
	 testing_iiop_rec_union/3,
	 relay_call/3,
	 relay_cast/3,
	 %% Testing pseudo calls.
	 pseudo_call/2,
	 pseudo_cast/2,
	 pseudo_call_delay/3,
	 pseudo_cast_delay/3,
	 pseudo_call_raise_exc/3,
	 %% Testing raise locally defined exception.
	 raise_local_exception/2,
	 raise_complex_local_exception/2,
	 %% Test timeout functionality
	 testing_iiop_oneway_delay/3,
	 testing_iiop_twoway_delay/3]).


%%--------------- gen_server specific ------------------------
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%--------------- LOCAL DATA ---------------------------------

%%------------------------------------------------------------
%% function : init, terminate
%%------------------------------------------------------------
init(State) ->
    process_flag(trap_exit,true),
    {ok, State}.

terminate(Reason, State) ->
    io:format("orber_test_server:terminate(~p  ~p)~n",[Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(_,_, State) ->
    {noreply, State}.
handle_cast(_, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------- SERVER FUNCTIONS ---------------------------

print(Self, State) ->
    io:format("orber_test_server:print(~p  ~p)~n",[Self, State]),
    {reply, ok, State}.

stop_normal(_Self, State) ->
    {stop, normal, ok, State}.

stop_brutal(_Self, _State) ->
    exit("killed_brutal").


%% Testing code and decode arguments
testing_iiop_float(_Self, State, Float) ->
    {reply, {ok, Float}, State}.

testing_iiop_double(_Self, State, Double) ->
    {reply, {ok, Double}, State}.

testing_iiop_short(_Self, State, Short) ->
    {reply, {ok, Short}, State}.

testing_iiop_ushort(_Self, State, Ushort) ->
    {reply, {ok, Ushort}, State}.

testing_iiop_long(_Self, State, Long) ->
    {reply, {ok, Long}, State}.

testing_iiop_longlong(_Self, State, LLong) ->
    {reply, {ok, LLong}, State}.

testing_iiop_ulong(_Self, State, Ulong) ->
    {reply, {ok, Ulong}, State}.

testing_iiop_ulonglong(_Self, State, ULlong) ->
    {reply, {ok, ULlong}, State}.

testing_iiop_char(_Self, State, Char) ->
    {reply, {ok, Char}, State}.

testing_iiop_wchar(_Self, State, WChar) ->
    {reply, {ok, WChar}, State}.

testing_iiop_bool(_Self, State, Boolean) ->
    {reply, {ok, Boolean}, State}.

testing_iiop_octet(_Self, State, Octet) ->
    {reply, {ok, Octet}, State}.

testing_iiop_any(_Self, State, Any) ->
    {reply, {ok, Any}, State}.

testing_iiop_obj(_Self, State, Obj) ->
    {reply, {ok, Obj}, State}.

testing_iiop_string(_Self, State, String) ->
    {reply, {ok, String}, State}.

testing_iiop_wstring(_Self, State, WString) ->
    {reply, {ok, WString}, State}.

testing_iiop_struct(_Self, State, Struct) ->
    {reply, {ok, Struct}, State}.

testing_iiop_union(_Self, State, Union) ->
    {reply, {ok, Union}, State}.

testing_iiop_union_d(_Self, State, Union) ->
    {reply, {ok, Union}, State}.

testing_iiop_enum(_Self, State, Enum) ->
    {reply, {ok, Enum}, State}.

testing_iiop_seq(_Self, State, Sequence) ->
    {reply, {ok, Sequence}, State}.

testing_iiop_uni_seq(_Self, State, Sequence) ->
    {reply, {ok, Sequence}, State}.

testing_iiop_struc_seq(_Self, State, Sequence) ->
    {reply, {ok, Sequence}, State}.

testing_iiop_array(_Self, State, Array) ->
    {reply, {ok, Array}, State}.

testing_iiop_fixed(_Self, State, Fixed) ->
    {reply, {ok, Fixed}, State}.

testing_iiop_void(_Self, State) ->
    {reply, ok, State}.

testing_iiop_context(_Self, State) ->
    Ctx = get(oe_server_in_context),
    io:format("orber_test_server:testing_iiop_context( ~p )~n", [Ctx]),
    {reply, ok, State}.

testing_iiop_server_marshal(_Self, State, _String) ->
    {reply, {ok, false}, State}.

testing_iiop_rec_any(_Self, State, RAny) ->
    {reply, RAny, State}.

testing_iiop_rec_struct(_Self, State, RecS) ->
    {reply, RecS, State}.

testing_iiop_rec_union(_Self, State, RecU) ->
    {reply, RecU, State}.


testing_iiop_oneway_delay(_Self, State, Time) ->
    timer:sleep(Time),
    {noreply, State}.

testing_iiop_twoway_delay(_Self, State, Time) ->
    timer:sleep(Time),
    {reply, ok, State}.

raise_local_exception(_Self, State) ->
    corba:raise(#'orber_test_server_UserDefinedException'{}),
    {reply, ok, State}.

raise_complex_local_exception(_Self, State) ->
    corba:raise(#'orber_test_server_ComplexUserDefinedException'{strseq=
					[#orber_test_server_struc{a=1, b=2}]}),
    {reply, ok, State}.

%% Testing relay calls/casts to, for example, test that sending implicit
%% Contexts works.
relay_call(_Self, State, Target) ->
    io:format("orber_test_server:relay_call( ~p ) Pre~n", [get(oe_server_in_context)]),
    orber_test_server:testing_iiop_context(Target),
    io:format("orber_test_server:relay_call( ~p ) Post~n", [get(oe_server_in_context)]),
    {reply, ok, State}.

relay_cast(_Self, State, Target) ->
    io:format("orber_test_server:relay_cast( ~p ) Pre~n", [get(oe_server_in_context)]),
    orber_test_server:testing_iiop_context(Target),
    io:format("orber_test_server:relay_cast( ~p ) Post~n", [get(oe_server_in_context)]),
    {noreply, State}.

%% Testing pseudo calls.
pseudo_call(_Self, State) ->
    io:format("orber_test_server:pseudo_call( ~p )~n", [erlang:timestamp()]),
    {reply, ok, State}.

pseudo_cast(_Self, State) ->
    io:format("orber_test_server:pseudo_cast( ~p )~n", [erlang:timestamp()]),
    {noreply, State}.
pseudo_call_delay(_Self, State, Time) ->
    io:format("orber_test_server:pseudo_call_delay( ~p )~n", [erlang:timestamp()]),
    timer:sleep(Time),
    io:format("orber_test_server:pseudo_call_delay( ~p )~n", [erlang:timestamp()]),
    {reply, {ok, Time}, State}.

pseudo_cast_delay(_Self, State, Time) ->
    io:format("orber_test_server:pseudo_cast_delay( ~p )~n", [erlang:timestamp()]),
    timer:sleep(Time),
    io:format("orber_test_server:pseudo_cast_delay( ~p )~n", [erlang:timestamp()]),
    {noreply, State}.

pseudo_call_raise_exc(_Self, State, 1) ->
    io:format("orber_test_server:pseudo_call_raise_exc( ~p )~n",[1]),
    {reply, {'EXCEPTION', #'BAD_QOS'{completion_status=?COMPLETED_NO}}, State};
pseudo_call_raise_exc(_Self, State, 2) ->
    io:format("orber_test_server:pseudo_call_raise_exc( ~p )~n",[2]),
    corba:raise(#'BAD_QOS'{completion_status=?COMPLETED_NO}),
    {reply, ok, State}.

%%--------------- LOCAL FUNCTIONS ----------------------------

%%--------------- END OF MODULE ------------------------------

