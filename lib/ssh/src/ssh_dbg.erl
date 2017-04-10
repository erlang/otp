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

-export([shrink_bin/1,
	 wr_record/3]).

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
    dbg:p(new,[c,timestamp]),
    dbg_ssh_messages().

dbg_ssh_messages() ->
    dbg:tp(ssh_message,encode,1, x),
    dbg:tp(ssh_message,decode,1, x),
    dbg:tpl(ssh_transport,select_algorithm,4, x),
    dbg:tp(ssh_transport,hello_version_msg,1, x),
    dbg:tp(ssh_transport,handle_hello_version,1, x).
   
%%%----------------------------------------------------------------
stop() ->
    dbg:stop().

%%%================================================================
msg_formater({trace_ts,Pid,call,{ssh_message,encode,[Msg]},TS}, D) ->
    fmt("~n~s SEND ~p ~s~n", [ts(TS),Pid,wr_record(shrink_bin(Msg))], D);
msg_formater({trace_ts,_Pid,return_from,{ssh_message,encode,1},_Res,_TS}, D) -> 
    D;
	
msg_formater({trace_ts,_Pid,call,{ssh_message,decode,_},_TS}, D) ->
    D;
msg_formater({trace_ts,Pid,return_from,{ssh_message,decode,1},Msg,TS}, D) -> 
    fmt("~n~s ~p RECV ~s~n", [ts(TS),Pid,wr_record(shrink_bin(Msg))], D);
	
msg_formater({trace_ts,_Pid,call,{ssh_transport,select_algorithm,_},_TS}, D) ->
    D;
msg_formater({trace_ts,Pid,return_from,{ssh_transport,select_algorithm,_},{ok,Alg},TS}, D) ->
    fmt("~n~s ~p ALGORITHMS~n~s~n", [ts(TS),Pid, wr_record(Alg)], D);

msg_formater({trace_ts,_Pid,call,{ssh_transport,hello_version_msg,_},_TS}, D) ->
    D;
msg_formater({trace_ts,Pid,return_from,{ssh_transport,hello_version_msg,1},Hello,TS}, D) -> 
    fmt("~n~s ~p TCP SEND HELLO~n  ~p~n", [ts(TS),Pid,lists:flatten(Hello)], D);

msg_formater({trace_ts,Pid,call,{ssh_transport,handle_hello_version,[Hello]},TS}, D) ->
    fmt("~n~s ~p RECV HELLO~n  ~p~n", [ts(TS),Pid,lists:flatten(Hello)], D);
msg_formater({trace_ts,_Pid,return_from,{ssh_transport,handle_hello_version,1},_,_TS}, D) -> 
    D;

msg_formater({trace_ts,Pid,send,{tcp,Sock,Bytes},Pid,TS}, D) ->
    fmt("~n~s ~p TCP SEND on ~p~n ~p~n", [ts(TS),Pid,Sock, shrink_bin(Bytes)], D);

msg_formater({trace_ts,Pid,send,{tcp,Sock,Bytes},Dest,TS}, D) ->
    fmt("~n~s ~p TCP SEND from ~p TO ~p~n ~p~n", [ts(TS),Pid,Sock,Dest, shrink_bin(Bytes)], D);

msg_formater({trace_ts,Pid,send,ErlangMsg,Dest,TS}, D) ->
    fmt("~n~s ~p ERL MSG SEND TO ~p~n ~p~n", [ts(TS),Pid,Dest, shrink_bin(ErlangMsg)], D);


msg_formater({trace_ts,Pid,'receive',{tcp,Sock,Bytes},TS}, D) ->
    fmt("~n~s ~p TCP RECEIVE on ~p~n ~p~n", [ts(TS),Pid,Sock,shrink_bin(Bytes)], D);

msg_formater({trace_ts,Pid,'receive',ErlangMsg,TS}, D) ->
    fmt("~n~s ~p ERL MSG RECEIVE~n ~p~n", [ts(TS),Pid,shrink_bin(ErlangMsg)], D);


msg_formater(M, D) ->
    fmt("~nDBG ~n~p~n", [shrink_bin(M)], D).

%% msg_formater(_, D) -> 
%%     D.


fmt(Fmt, Args,  D=#data{writer=Write,acc=Acc}) ->
    D#data{acc = Write(io_lib:format(Fmt, Args), Acc)}.

ts({_,_,Usec}=Now) ->
    {_Date,{HH,MM,SS}} = calendar:now_to_local_time(Now),
    io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.6.0w",[HH,MM,SS,Usec]);
ts(_) ->
    "-".
%%%----------------------------------------------------------------
setup_tracer(Write, MangleArg) ->
    Handler = fun(Arg, D) ->
		      msg_formater(MangleArg(Arg), D)
	      end,
    InitialData = #data{writer = Write},
    {ok,_} = dbg:tracer(process, {Handler, InitialData}),
    ok.

%%%----------------------------------------------------------------
shrink_bin(B) when is_binary(B), size(B)>256 -> {'*** SHRINKED BIN',
						 size(B),
						 element(1,split_binary(B,64)),
						 '...',
						 element(2,split_binary(B,size(B)-64))
						};
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
?wr_record(ssh_msg_ext_info);
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
