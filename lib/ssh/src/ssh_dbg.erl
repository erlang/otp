%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(ssh_dbg).

-export([messages/0,
	 messages/1,
	 messages/2,
	 stop/0
	]).

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_connect.hrl").
-include("ssh_auth.hrl").

-record(data, {
	  writer,
	  acc = []}).
%%%================================================================
messages() ->
    messages(fun(String,_D) -> io:format(String) end).

messages(Write) when is_function(Write,2) ->
    messages(Write, fun(X) -> X end).

messages(Write, MangleArg) when is_function(Write,2),
				is_function(MangleArg,1) ->
    catch dbg:start(),
    setup_tracer(Write, MangleArg),
    dbg:p(new,c),
    dbg_ssh_messages().

dbg_ssh_messages() ->
    dbg:tp(ssh_message,encode,1, x),
    dbg:tp(ssh_message,decode,1, x),
    dbg:tpl(ssh_transport,select_algorithm,3, x).

%%%----------------------------------------------------------------
stop() ->
    dbg:stop().

%%%================================================================
msg_formater({trace,Pid,call,{ssh_message,encode,[Msg]}}, D) ->
    fmt("~nSEND ~p ~s~n", [Pid,wr_record(shrink_bin(Msg))], D);
msg_formater({trace,_Pid,return_from,{ssh_message,encode,1},_Res}, D) -> 
    D;
	
msg_formater({trace,_Pid,call,{ssh_message,decode,_}}, D) ->
    D;
msg_formater({trace,Pid,return_from,{ssh_message,decode,1},Msg}, D) -> 
    fmt("~n~p RECV ~s~n", [Pid,wr_record(shrink_bin(Msg))], D);
	
msg_formater({trace,_Pid,call,{ssh_transport,select_algorithm,_}}, D) ->
    D;
msg_formater({trace,Pid,return_from,{ssh_transport,select_algorithm,3},{ok,Alg}}, D) ->
    fmt("~n~p ALGORITHMS~n~s~n", [Pid, wr_record(Alg)], D);


msg_formater({trace,Pid,send,{tcp,Sock,Bytes},Pid}, D) ->
    fmt("~n~p TCP SEND on ~p~n ~p~n", [Pid,Sock, shrink_bin(Bytes)], D);

msg_formater({trace,Pid,send,{tcp,Sock,Bytes},Dest}, D) ->
    fmt("~n~p TCP SEND from ~p TO ~p~n ~p~n", [Pid,Sock,Dest, shrink_bin(Bytes)], D);

msg_formater({trace,Pid,send,ErlangMsg,Dest}, D) ->
    fmt("~n~p ERL MSG SEND TO ~p~n ~p~n", [Pid,Dest, shrink_bin(ErlangMsg)], D);


msg_formater({trace,Pid,'receive',{tcp,Sock,Bytes}}, D) ->
    fmt("~n~p TCP RECEIVE on ~p~n ~p~n", [Pid,Sock,shrink_bin(Bytes)], D);

msg_formater({trace,Pid,'receive',ErlangMsg}, D) ->
    fmt("~n~p ERL MSG RECEIVE~n ~p~n", [Pid,shrink_bin(ErlangMsg)], D);


msg_formater(M, D) ->
    fmt("~nDBG ~n~p~n", [shrink_bin(M)], D).

%% msg_formater(_, D) -> 
%%     D.


fmt(Fmt, Args,  D=#data{writer=Write,acc=Acc}) ->
    D#data{acc = Write(io_lib:format(Fmt, Args), Acc)}.

%%%----------------------------------------------------------------
setup_tracer(Write, MangleArg) ->
    Handler = fun(Arg, D) ->
		      msg_formater(MangleArg(Arg), D)
	      end,
    InitialData = #data{writer = Write},
    {ok,_} = dbg:tracer(process, {Handler, InitialData}),
    ok.

%%%----------------------------------------------------------------
shrink_bin(B) when is_binary(B), size(B)>100 -> {'*** SHRINKED BIN',size(B),element(1,split_binary(B,20)),'***'};
shrink_bin(L) when is_list(L) -> lists:map(fun shrink_bin/1, L);
shrink_bin(T) when is_tuple(T) -> list_to_tuple(shrink_bin(tuple_to_list(T)));
shrink_bin(X) -> X.

%%%----------------------------------------------------------------
-define(wr_record(N,BlackList), wr_record(R=#N{}) -> wr_record(R, record_info(fields,N), BlackList)).

-define(wr_record(N), ?wr_record(N, [])).


?wr_record(alg);

?wr_record(ssh_msg_disconnect);
?wr_record(ssh_msg_ignore);
?wr_record(ssh_msg_unimplemented);
?wr_record(ssh_msg_debug);
?wr_record(ssh_msg_service_request);
?wr_record(ssh_msg_service_accept);
?wr_record(ssh_msg_kexinit);
?wr_record(ssh_msg_kexdh_init);
?wr_record(ssh_msg_kexdh_reply);
?wr_record(ssh_msg_newkeys);
?wr_record(ssh_msg_kex_dh_gex_request);
?wr_record(ssh_msg_kex_dh_gex_request_old);
?wr_record(ssh_msg_kex_dh_gex_group);
?wr_record(ssh_msg_kex_dh_gex_init);
?wr_record(ssh_msg_kex_dh_gex_reply);
?wr_record(ssh_msg_kex_ecdh_init);
?wr_record(ssh_msg_kex_ecdh_reply);

?wr_record(ssh_msg_userauth_request);
?wr_record(ssh_msg_userauth_failure);
?wr_record(ssh_msg_userauth_success);
?wr_record(ssh_msg_userauth_banner);
?wr_record(ssh_msg_userauth_passwd_changereq);
?wr_record(ssh_msg_userauth_pk_ok);
?wr_record(ssh_msg_userauth_info_request);
?wr_record(ssh_msg_userauth_info_response);

?wr_record(ssh_msg_global_request);
?wr_record(ssh_msg_request_success);
?wr_record(ssh_msg_request_failure);
?wr_record(ssh_msg_channel_open);
?wr_record(ssh_msg_channel_open_confirmation);
?wr_record(ssh_msg_channel_open_failure);
?wr_record(ssh_msg_channel_window_adjust);
?wr_record(ssh_msg_channel_data);
?wr_record(ssh_msg_channel_extended_data);
?wr_record(ssh_msg_channel_eof);
?wr_record(ssh_msg_channel_close);
?wr_record(ssh_msg_channel_request);
?wr_record(ssh_msg_channel_success);
?wr_record(ssh_msg_channel_failure);

wr_record(R) -> io_lib:format('~p~n',[R]).


wr_record(T, Fs, BL) when is_tuple(T) ->
    wr_record(tuple_to_list(T), Fs, BL);
wr_record([Name|Values], Fields, BlackL) ->
    W = case Fields of
	    [] -> 0;
	    _ -> lists:max([length(atom_to_list(F)) || F<-Fields])
	end,
    [io_lib:format("~p:~n",[string:to_upper(atom_to_list(Name))])
     | [io_lib:format("  ~*p: ~p~n",[W,Tag,Value]) || {Tag,Value} <- lists:zip(Fields,Values),
						      not lists:member(Tag,BlackL)
       ]
    ].
