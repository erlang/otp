%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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

-export([messages/0, messages/1, messages/2, messages/3,
	 auth/0,     auth/1,     auth/2,     auth/3,
	 algs/0,     algs/1,     algs/2,     algs/3,
	 hostkey/0,  hostkey/1,  hostkey/2,  hostkey/3,
	 stop/0
	]).

-export([shrink_bin/1,
	 wr_record/3]).

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_connect.hrl").
-include("ssh_auth.hrl").

%%%================================================================
messages() -> start(msg).
messages(F) -> start(msg,F).
messages(F,X) -> start(msg,F,X).
messages(F,M,I) -> start(msg,F,M,I).

auth() -> start(auth).
auth(F) -> start(auth,F).
auth(F,X) -> start(auth,F,X).
auth(F,M,I) -> start(auth,F,M,I).

algs() -> start(algs).
algs(F) -> start(algs,F).
algs(F,X) -> start(algs,F,X).
algs(F,M,I) -> start(algs,F,M,I).

hostkey() -> start(hostkey).
hostkey(F) -> start(hostkey,F).
hostkey(F,X) -> start(hostkey,F,X).
hostkey(F,M,I) -> start(hostkey,F,M,I).

stop() -> dbg:stop().

%%%----------------------------------------------------------------
start(Type) -> start(Type, fun io:format/2).

start(Type, F) when is_function(F,2) -> start(Type, fmt_fun(F));
start(Type, F) when is_function(F,3) -> start(Type, F, id_fun()).

start(Type, WriteFun, MangleArgFun) when is_function(WriteFun, 3),
                                         is_function(MangleArgFun, 1) ->
    start(Type, WriteFun, MangleArgFun, []);
start(Type, WriteFun, InitValue) ->
    start(Type, WriteFun, id_fun(), InitValue).

start(Type, WriteFun, MangleArgFun, InitValue) when is_function(WriteFun, 3),
                                                    is_function(MangleArgFun, 1) ->
    cond_start(Type, WriteFun, MangleArgFun, InitValue),
    dbg_ssh(Type).

%%%----------------------------------------------------------------
fmt_fun(F) -> fun(Fmt,Args,Data) -> F(Fmt,Args), Data end.

id_fun() ->  fun(X) -> X end.

%%%----------------------------------------------------------------
dbg_ssh(What) ->
    case [E || E <- lists:flatten(dbg_ssh0(What)),
               element(1,E) =/= ok] of
        [] -> ok;
        Other -> Other
    end.
            

dbg_ssh0(auth) ->
    [dbg:tp(ssh_transport,hello_version_msg,1, x),
     dbg:tp(ssh_transport,handle_hello_version,1, x),
     dbg:tp(ssh_message,encode,1, x),
     dbg:tpl(ssh_transport,select_algorithm,4, x),
     dbg:tpl(ssh_connection_handler,ext_info,2, x),
     lists:map(fun(F) -> dbg:tp(ssh_auth, F, x) end,
               [publickey_msg, password_msg, keyboard_interactive_msg])
    ];

dbg_ssh0(algs) ->
    [dbg:tpl(ssh_transport,select_algorithm,4, x),
     dbg:tpl(ssh_connection_handler,ext_info,2, x)
    ];

dbg_ssh0(hostkey) ->
    [dbg:tpl(ssh_transport, verify_host_key, 4, x),
     dbg:tp(ssh_transport, verify, 4, x),
     dbg:tpl(ssh_transport, known_host_key, 3, x),
%%     dbg:tpl(ssh_transport, accepted_host, 4, x),
     dbg:tpl(ssh_transport, add_host_key, 4, x),
     dbg:tpl(ssh_transport, is_host_key, 5, x)
    ];

dbg_ssh0(msg) ->
    [dbg_ssh0(hostkey),
     dbg_ssh0(auth),
     dbg:tp(ssh_message,encode,1, x),
     dbg:tp(ssh_message,decode,1, x),
     dbg:tpl(ssh_transport,select_algorithm,4, x),
     dbg:tp(ssh_transport,hello_version_msg,1, x),
     dbg:tp(ssh_transport,handle_hello_version,1, x),
     dbg:tpl(ssh_connection_handler,ext_info,2, x)
    ].
   
   
%%%================================================================
cond_start(Type, WriteFun, MangleArgFun, Init) ->
    try
        dbg:start(),
        setup_tracer(Type, WriteFun, MangleArgFun, Init),
        dbg:p(new,[c,timestamp])
    catch
        _:_ -> ok
    end.


msg_formater(msg, {trace_ts,Pid,call,{ssh_message,encode,[Msg]},TS}, D) ->
    fmt("~n~s SEND ~p ~s~n", [ts(TS),Pid,wr_record(shrink_bin(Msg))], D);
msg_formater(msg, {trace_ts,_Pid,return_from,{ssh_message,encode,1},_Res,_TS}, D) -> 
    D;
	
msg_formater(msg, {trace_ts,_Pid,call,{ssh_message,decode,_},_TS}, D) ->
    D;
msg_formater(msg, {trace_ts,Pid,return_from,{ssh_message,decode,1},Msg,TS}, D) -> 
    Extra =
        case Msg of
            #ssh_msg_userauth_info_request{data = D0} ->
                try ssh_message:decode_keyboard_interactive_prompts(D0, [])
                of
                    Acc ->
                        io_lib:format("  -- decoded data:~n", []) ++
                        element(1,
                                lists:mapfoldl(
                                  fun({Prompt,Echo}, N) ->
                                          {io_lib:format("     prompt[~p]: \"~s\" (echo=~p)~n",[N,Prompt,Echo]), N+1}
                                  end, 1, Acc))
                catch
                    _:_ ->
                        ""
                end;
            _ ->
                ""
        end,
    fmt("~n~s ~p RECV ~s~s~n", [ts(TS),Pid,wr_record(shrink_bin(Msg)),Extra], D);

msg_formater(_auth, {trace_ts,Pid,return_from,{ssh_message,decode,1},#ssh_msg_userauth_failure{authentications=As},TS}, D) -> 
    fmt("~n~s ~p Client login FAILURE. Try ~s~n", [ts(TS),Pid,As], D);
	
msg_formater(_auth, {trace_ts,Pid,return_from,{ssh_message,decode,1},#ssh_msg_userauth_success{},TS}, D) -> 
    fmt("~n~s ~p Client login SUCCESS~n", [ts(TS),Pid], D);

	
msg_formater(_, {trace_ts,_Pid,call,{ssh_transport,select_algorithm,_},_TS}, D) ->
    D;
msg_formater(_, {trace_ts,Pid,return_from,{ssh_transport,select_algorithm,_},{ok,Alg},TS}, D) ->
    fmt("~n~s ~p ALGORITHMS~n~s~n", [ts(TS),Pid, wr_record(Alg)], D);

msg_formater(_, {trace_ts,_Pid,call,{ssh_transport,hello_version_msg,_},_TS}, D) ->
    D;
msg_formater(_, {trace_ts,Pid,return_from,{ssh_transport,hello_version_msg,1},Hello,TS}, D) -> 
    fmt("~n~s ~p TCP SEND HELLO~n  ~p~n", [ts(TS),Pid,lists:flatten(Hello)], D);

msg_formater(_, {trace_ts,Pid,call,{ssh_transport,handle_hello_version,[Hello]},TS}, D) ->
    fmt("~n~s ~p RECV HELLO~n  ~p~n", [ts(TS),Pid,lists:flatten(Hello)], D);
msg_formater(_, {trace_ts,_Pid,return_from,{ssh_transport,handle_hello_version,1},_,_TS}, D) -> 
    D;

msg_formater(_, {trace_ts,Pid,call,{ssh_connection_handler,ext_info,[{"server-sig-algs",SigAlgs},State]},TS}, D) ->
    try lists:keyfind(ssh, 1, tuple_to_list(State)) of
        false ->
            D;
        #ssh{userauth_pubkeys = PKs} ->
            fmt("~n~s ~p Client got suggestion to use user public key sig-algs~n    ~p~n  and can use~n    ~p~n",
                [ts(TS),Pid,string:tokens(SigAlgs,","),PKs], D)
    catch
        _:_ ->
            D
    end;

msg_formater(_, {trace_ts,Pid,return_from,{ssh_connection_handler,ext_info,2},State,TS}, D) ->
    try lists:keyfind(ssh, 1, tuple_to_list(State)) of
        false ->
            D;
        #ssh{userauth_pubkeys = PKs} ->
            fmt("~n~s ~p Client will try user public key sig-algs~n  ~p~n", [ts(TS),Pid,PKs], D)
    catch
        _:_ ->
            D
    end;

msg_formater(_, {trace_ts,Pid,call, {ssh_transport,verify_host_key,[_Ssh,_PK,_Dgst,{AlgStr,_Sign}]},TS}, D) ->
    fmt("~n~s ~p Client got a ~s hostkey. Will try to verify it~n", [ts(TS),Pid,AlgStr], D);
msg_formater(_, {trace_ts,Pid,return_from, {ssh_transport,verify_host_key,4}, Result, TS}, D) ->
    case Result of
        ok -> fmt("~n~s ~p Hostkey verified.~n", [ts(TS),Pid], D);
        {error,E} ->
              fmt("~n~s ~p ***** Hostkey NOT verified: ~p ******!~n", [ts(TS),Pid,E], D);
        _  -> fmt("~n~s ~p ***** Hostkey is NOT verified: ~p ******!~n", [ts(TS),Pid,Result], D)
    end;
        
msg_formater(_, {trace_ts,Pid,return_from, {ssh_transport,verify,4}, Result, TS}, D) ->
    case Result of
        true -> D;
        _ -> fmt("~n~s ~p Couldn't verify the signature!~n", [ts(TS),Pid], D)
    end;

msg_formater(_, {trace_ts,_Pid,call, {ssh_transport,is_host_key,_}, _TS}, D) -> D;
msg_formater(_, {trace_ts,Pid,return_from, {ssh_transport,is_host_key,5}, {CbMod,Result}, TS}, D) ->
    case Result of
        true -> fmt("~n~s ~p Hostkey found by ~p.~n", [ts(TS),Pid,CbMod], D);
        _    -> fmt("~n~s ~p Hostkey NOT found by ~p.~n", [ts(TS),Pid,CbMod], D)
    end;

msg_formater(_, {trace_ts,_Pid,call, {ssh_transport,add_host_key,_}, _TS}, D) -> D;
msg_formater(_, {trace_ts,Pid,return_from, {ssh_transport,add_host_key,4}, {CbMod,Result}, TS}, D) ->
    case Result of
        ok -> fmt("~n~s ~p New hostkey added by ~p.~n", [ts(TS),Pid,CbMod], D);
        _  -> D
    end;

msg_formater(_, {trace_ts,_Pid,call,{ssh_transport,known_host_key,_},_TS}, D) -> D;
msg_formater(_, {trace_ts,Pid,return_from, {ssh_transport,known_host_key,3}, Result, TS}, D) ->
    case Result of
        ok -> D;
        {error,E} -> fmt("~n~s ~p Hostkey addition failed: ~p~n", [ts(TS),Pid,E], D);
        _ -> fmt("~n~s ~p Hostkey addition: ~p~n", [ts(TS),Pid,Result], D)
    end;

msg_formater(_, {trace_ts,Pid,call,{ssh_auth,publickey_msg,[[SigAlg,#ssh{user=User}]]},TS}, D) ->
     fmt("~n~s ~p Client will try to login user ~p with method: public key algorithm ~p~n", [ts(TS),Pid,User,SigAlg], D);
msg_formater(_, {trace_ts,Pid,return_from,{ssh_auth,publickey_msg,1},{not_ok,#ssh{user=User}},TS}, D) ->
     fmt("~s ~p User ~p can't use that kind of public key~n", [ts(TS),Pid,User], D);
msg_formater(_, {trace_ts,_Pid,return_from,{ssh_auth,publickey_msg,1},_,_TS}, D) -> D;

msg_formater(_, {trace_ts,Pid,call,{ssh_auth,password_msg,[[#ssh{user=User}]]},TS}, D) ->
     fmt("~n~s ~p Client will try to login user ~p with method: password~n", [ts(TS),Pid,User], D);
msg_formater(_, {trace_ts,Pid,return_from,{ssh_auth,password_msg,1},{not_ok,#ssh{user=User}},TS}, D) ->
     fmt("~s ~p User ~p can't use method password as login method~n", [ts(TS),Pid,User], D);
msg_formater(_, {trace_ts,_Pid,return_from,{ssh_auth,password_msg,1},_Result,_TS}, D) -> D;

msg_formater(_, {trace_ts,Pid,call,{ssh_auth,keyboard_interactive_msg,[[#ssh{user=User}]]},TS}, D) ->
     fmt("~n~s ~p Client will try to login user ~p with method: keyboard-interactive~n", [ts(TS),Pid,User], D);
msg_formater(_, {trace_ts,Pid,return_from,{ssh_auth,keyboard_interactive_msg,1},{not_ok,#ssh{user=User}},TS}, D) ->
     fmt("~s ~p User ~p can't use method keyboard-interactive as login method~n", [ts(TS),Pid,User], D);
msg_formater(_, {trace_ts,_Pid,return_from,{ssh_auth,keyboard_interactive_msg,1},_Result,_TS}, D) -> D;

msg_formater(msg, {trace_ts,Pid,send,{tcp,Sock,Bytes},Pid,TS}, D) ->
    fmt("~n~s ~p TCP SEND on ~p~n ~p~n", [ts(TS),Pid,Sock, shrink_bin(Bytes)], D);

msg_formater(msg, {trace_ts,Pid,send,{tcp,Sock,Bytes},Dest,TS}, D) ->
    fmt("~n~s ~p TCP SEND from ~p TO ~p~n ~p~n", [ts(TS),Pid,Sock,Dest, shrink_bin(Bytes)], D);

msg_formater(msg, {trace_ts,Pid,send,ErlangMsg,Dest,TS}, D) ->
    fmt("~n~s ~p ERL MSG SEND TO ~p~n ~p~n", [ts(TS),Pid,Dest, shrink_bin(ErlangMsg)], D);


msg_formater(msg, {trace_ts,Pid,'receive',{tcp,Sock,Bytes},TS}, D) ->
    fmt("~n~s ~p TCP RECEIVE on ~p~n ~p~n", [ts(TS),Pid,Sock,shrink_bin(Bytes)], D);

msg_formater(msg, {trace_ts,Pid,'receive',ErlangMsg,TS}, D) ->
    fmt("~n~s ~p ERL MSG RECEIVE~n ~p~n", [ts(TS),Pid,shrink_bin(ErlangMsg)], D);


msg_formater(_, _M, D) ->
    fmt("~nDBG other ~n~p~n", [shrink_bin(_M)], D),
    D.

%%%----------------------------------------------------------------
-record(data, {writer,
               initialized,
               acc}).

fmt(Fmt, Args,  D=#data{initialized=false}) ->
    fmt(Fmt, Args,
        D#data{acc = (D#data.writer)("~s~n", [initial_info()], D#data.acc),
               initialized = true}
       );
fmt(Fmt, Args,  D=#data{writer=Write, acc=Acc}) ->
    D#data{acc = Write(Fmt,Args,Acc)}.

ts({_,_,Usec}=Now) ->
    {_Date,{HH,MM,SS}} = calendar:now_to_local_time(Now),
    io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.6.0w",[HH,MM,SS,Usec]);
ts(_) ->
    "-".

setup_tracer(Type, WriteFun, MangleArgFun, Init) ->
    Handler = fun(Arg, D) ->
		      msg_formater(Type, MangleArgFun(Arg), D)
	      end,
    InitialData = #data{writer = WriteFun,
                        initialized = false,
                        acc = Init},
    {ok,_} = dbg:tracer(process, {Handler, InitialData}),
    ok.


initial_info() ->
    Lines =
        [ts(erlang:timestamp()),
         "",
         "SSH:"]
        ++ as_list_of_lines(case application:get_key(ssh,vsn) of
                                {ok,Vsn} -> Vsn;
                                _ -> "(ssh not started)"
                            end)
        ++ ["",
            "Cryptolib:"]
        ++ as_list_of_lines(crypto:info_lib())
        ++ ["",
            "Crypto app:"]
        ++ as_list_of_lines(crypto:supports()),
    W = max_len(Lines),
    append_lines([line_of($*, W+4)]
                 ++ prepend_lines("* ", Lines)
                 ++ [line_of($-, W+4)],
                 io_lib:nl()
               ).
    
    
as_list_of_lines(Term) ->
    prepend_lines("  ",
                  string:tokens(lists:flatten(io_lib:format("~p",[Term])),
                                io_lib:nl()  % Get line endings in current OS
                               )
                 ).

line_of(Char,W) -> lists:duplicate(W,Char).
max_len(L) -> lists:max([length(S) || S<-L]).
append_lines(L, X)  -> [S++X || S<-L].
prepend_lines(X, L) -> [X++S || S<-L].

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
