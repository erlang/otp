%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(ssh_dbg_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh.hrl").
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,60}}].

all() -> 
    [{group, dbg},
     {group, circ_buf}
    ].

groups() ->
    [{dbg, [], [dbg_basic,
                dbg_alg_terminate,
                dbg_ssh_messages,
                dbg_connections,
                dbg_channels,
                dbg_authentication,
                all_dbg]},
     {circ_buf, [], [cb_basic,
                     cb_print,
                     cb_macros_print
                    ]}
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(begin
                      ssh:start(),
                      setup_dirs(Config)
                  end).

end_per_suite(_Config) ->
    ssh:stop().

%%--------------------------------------------------------------------
init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, Config) ->
    ssh_dbg:stop(),
    Config.

%%--------------------------------------------------------------------
-define(USR, "foo").
-define(PWD, "bar").

-define(DBG_RECEIVE(ExpectPfx, Ref, C, Pid),
        receive
            {Ref, [_, C, ExpectPfx++_]} ->
                ok

        after 5000 ->
                ct:log("~p:~p Messages:~n~p",
                       [?MODULE,?LINE, process_info(self(),messages)]),
                ssh_dbg:stop(),
                ssh:stop_daemon(Pid),
                ct:fail("No '~s' debug message",[ExpectPfx])
        end
       ).
%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

dbg_basic(_Config) ->
    L0 = ssh_dbg:start(),
    true = is_pid(whereis(ssh_dbg)),
    true = is_list(L0),

    {ok,L0} = ssh_dbg:on(),
    {ok,L0} = ssh_dbg:on(),

    L1 = [hd(L0)],
    {ok,L1} = ssh_dbg:off(tl(L0)),

    {ok,L1} = ssh_dbg:go_on(),

    {ok,[]} = ssh_dbg:off(),
    {ok,[]} = ssh_dbg:off(),
    
    ok = ssh_dbg:stop(),
    undefined = whereis(ssh_dbg).
    

%%--------------------------------------------------------------------
dbg_alg_terminate(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir   = proplists:get_value(priv_dir, Config),
    
    Ref = ssh_dbg_start(),
    {ok,[alg,connections,terminate]} = ssh_dbg:on([alg,terminate,connections]),
    {ok,[alg,terminate]} = ssh_dbg:off(connections), % just testing that terminate is not canceled
    
    Parent = self(),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
                                             {user_passwords, [{?USR,?PWD}]},
                                             {connectfun, fun(_,_,_) ->
                                                                  Parent ! {daemon_c,Ref,self()}
                                                          end},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
                                          {user,?USR},
                                          {password,?PWD},
					  {user_interaction, false}]),

    %% Daemon connection ref (D):
    D = receive
            {daemon_c,Ref,D0} -> D0
        end,
    ct:log("~p:~p~nC = ~p, D=~p",[?MODULE,?LINE, C, D]),

    ?DBG_RECEIVE("Negotiated algorithms:", Ref, C, Pid),
    ?DBG_RECEIVE("Negotiated algorithms:", Ref, D, Pid),
    
    ssh:close(C),
    ?DBG_RECEIVE("Connection Terminating:", Ref, C, Pid),
    ?DBG_RECEIVE("Connection Terminating:", Ref, D, Pid),

    stop_and_fail_if_unhandled_dbg_msgs(Ref, [C,D], Pid).

%%--------------------------------------------------------------------
dbg_connections(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir   = proplists:get_value(priv_dir, Config),
    
    Ref = ssh_dbg_start(),
    {ok,[connections,terminate]} = ssh_dbg:on([connections, terminate]),
    {ok,[connections]} = ssh_dbg:off(terminate), % Just testing that terminate doesn't cancel connections
    
    Parent = self(),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
                                             {user_passwords, [{?USR,?PWD}]},
                                             {connectfun, fun(_,_,_) ->
                                                                  Parent ! {daemon_c,Ref,self()}
                                                          end},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    
    ?DBG_RECEIVE("Starting LISTENER on ", Ref, _, Pid),

    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
                                          {user,?USR},
                                          {password,?PWD},
					  {user_interaction, false}]),

    %% Daemon connection ref (D):
    D = receive
            {daemon_c,Ref,D0} -> D0
        end,
    ct:log("~p:~p~nC = ~p, D=~p",[?MODULE,?LINE, C, D]),

    ?DBG_RECEIVE("Starting server connection:", Ref, D, Pid),
    ?DBG_RECEIVE("Starting client connection:", Ref, C, Pid),
    
    ssh:close(C),
    ?DBG_RECEIVE("Connection Terminating:", Ref, C, Pid),
    ?DBG_RECEIVE("Connection Terminating:", Ref, D, Pid),

    stop_and_fail_if_unhandled_dbg_msgs(Ref, [C,D], Pid).

%%--------------------------------------------------------------------
dbg_authentication(Config) ->
    Ref = ssh_dbg_start(),
    {ok,[authentication]} = ssh_dbg:on([authentication]),
    
    Parent = self(),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, system_dir(Config)},
					     {user_dir, user_dir(Config)},
                                             {user_passwords, [{?USR,?PWD}]},
                                             {connectfun, fun(_,_,_) ->
                                                                  Parent ! {daemon_c,Ref,self()}
                                                          end},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    
    %% ---- Check password ----
    Cpwd = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                             {user_dir, user_dir(Config)},
                                             {user,?USR},
                                             {password,?PWD},
                                             {auth_methods,"password"},
                                             {user_interaction, false}]),
    Cpwd_d = daemon_connection_ref(Ref, Cpwd),

    ?DBG_RECEIVE("AUTH client: Service ssh-userauth accepted", Ref, Cpwd, Pid),
    ?DBG_RECEIVE("AUTH client: Query for accepted methods", Ref, Cpwd, Pid),
    ?DBG_RECEIVE("AUTH srvr: Peer queries auth methods", Ref, Cpwd_d, Pid),
    ?DBG_RECEIVE("AUTH client: Server supports", Ref, Cpwd, Pid),
    ?DBG_RECEIVE("AUTH client: Try auth with", Ref, Cpwd, Pid),
    ?DBG_RECEIVE("AUTH srvr: Peer client authorized", Ref, Cpwd_d, Pid),
    ?DBG_RECEIVE("AUTH client: Success", Ref, Cpwd, Pid),
    ssh:close(Cpwd),
    fail_if_unhandled_dbg_msgs(Ref, [Cpwd,Cpwd_d]),

    %% ---- Check keyboard-interactive ----
    Ckbi = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                             {user_dir, user_dir(Config)},
                                             {user,?USR},
                                             {password,?PWD},
                                             {auth_methods,"keyboard-interactive"},
                                             {user_interaction, false}]),
    Ckbi_d = daemon_connection_ref(Ref, Ckbi),

    ?DBG_RECEIVE("AUTH client: Service ssh-userauth accepted", Ref, Ckbi, Pid),
    ?DBG_RECEIVE("AUTH client: Query for accepted methods", Ref, Ckbi, Pid),
    ?DBG_RECEIVE("AUTH srvr: Peer queries auth methods", Ref, Ckbi_d, Pid),
    ?DBG_RECEIVE("AUTH client: Server supports", Ref, Ckbi, Pid),
    ?DBG_RECEIVE("AUTH client: Try auth with", Ref, Ckbi, Pid),
    ?DBG_RECEIVE("AUTH srvr: Ask peer client for password", Ref, Ckbi_d, Pid),
    ?DBG_RECEIVE("AUTH client: Success", Ref, Ckbi, Pid),
    ssh:close(Ckbi),
    fail_if_unhandled_dbg_msgs(Ref, [Ckbi,Ckbi_d]),

    %% ---- Check publickey ----
    Cpkey = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                             {user_dir, user_dir(Config)},
                                             {auth_methods,"publickey"},
                                             {user_interaction, false}]),
    Cpkey_d = daemon_connection_ref(Ref, Cpkey),

    ?DBG_RECEIVE("AUTH client: Service ssh-userauth accepted", Ref, Cpkey, Pid),
    ?DBG_RECEIVE("AUTH client: Query for accepted methods", Ref, Cpkey, Pid),
    ?DBG_RECEIVE("AUTH srvr: Peer queries auth methods", Ref, Cpkey_d, Pid),
    ?DBG_RECEIVE("AUTH client: Server supports", Ref, Cpkey, Pid),
    ?DBG_RECEIVE("AUTH client: Try auth with", Ref, Cpkey, Pid),
    ?DBG_RECEIVE("AUTH srvr: Peer client authorized", Ref, Cpkey_d, Pid),
    ?DBG_RECEIVE("AUTH client: Success", Ref, Cpkey, Pid),
    ssh:close(Cpkey),
    stop_and_fail_if_unhandled_dbg_msgs(Ref, [Cpkey,Cpkey_d], Pid).


daemon_connection_ref(Ref,C) ->
    D =
        receive
            {daemon_c,Ref,D0} -> D0
        end,
    ct:log("~p:~p~nC = ~p, D=~p",[?MODULE,?LINE, C, D]),
    D.

%%--------------------------------------------------------------------
dbg_ssh_messages(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir   = proplists:get_value(priv_dir, Config),
    
    Parent = self(),
    Ref = make_ref(),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
                                             {user_passwords, [{?USR,?PWD}]},
                                             {connectfun, fun(_,_,_) ->
                                                                  Parent ! {daemon_c,Ref,self()}
                                                          end},
					     {failfun, fun ssh_test_lib:failfun/2}]),

    ssh_dbg_start(Ref),
    {ok,[ssh_messages]} = ssh_dbg:on([ssh_messages]),

    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
                                          {user,?USR},
                                          {password,?PWD},
					  {user_interaction, false}]),

    %% Daemon connection ref (D):
    D = receive
            {daemon_c,Ref,D0} -> D0
        end,
    ct:log("~p:~p~nC = ~p, D=~p",[?MODULE,?LINE, C, D]),

    ?DBG_RECEIVE("Going to send hello message:", Ref, C, Pid),
    ?DBG_RECEIVE("Received hello message:",      Ref, D, Pid),

    ?DBG_RECEIVE("Going to send hello message:", Ref, D, Pid),
    ?DBG_RECEIVE("Received hello message:",      Ref, C, Pid),

    ?DBG_RECEIVE("Going to send SSH_MSG_KEXINIT:", Ref, C, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_KEXINIT:",      Ref, D, Pid),

    ?DBG_RECEIVE("Going to send SSH_MSG_KEXINIT:", Ref, D, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_KEXINIT:",      Ref, C, Pid),

    case atom_to_list( (ssh_connection_handler:alg(C))#alg.kex ) of
        "ecdh-"++_ ->
            ?DBG_RECEIVE("Going to send SSH_MSG_KEX_ECDH_INIT:",  Ref, C, Pid),
            ?DBG_RECEIVE("Received SSH_MSG_KEX_ECDH_INIT:",       Ref, D, Pid),
            ?DBG_RECEIVE("Going to send SSH_MSG_KEX_ECDH_REPLY:", Ref, D, Pid),
            ?DBG_RECEIVE("Received SSH_MSG_KEX_ECDH_REPLY:",      Ref, C, Pid);

        "diffie-hellman-group-exchange-"++_ ->
            ?DBG_RECEIVE("Going to send SSH_MSG_KEX_DH_GEX_REQUEST:", Ref, C, Pid),
            ?DBG_RECEIVE("Received SSH_MSG_KEX_DH_GEX_REQUEST:",      Ref, D, Pid),
            ?DBG_RECEIVE("Going to send SSH_MSG_KEX_DH_GEX_GROUP:",   Ref, D, Pid),
            ?DBG_RECEIVE("Received SSH_MSG_KEX_DH_GEX_GROUP:",        Ref, C, Pid),
            ?DBG_RECEIVE("Going to send SSH_MSG_KEX_DH_GEX_INIT:",    Ref, C, Pid),
            ?DBG_RECEIVE("Received SSH_MSG_KEX_DH_GEX_INIT:",         Ref, D, Pid),
            ?DBG_RECEIVE("Going to send SSH_MSG_KEX_DH_GEX_REPLY:",   Ref, D, Pid),
            ?DBG_RECEIVE("Received SSH_MSG_KEX_DH_GEX_REPLY:",        Ref, C, Pid);

        "diffie-hellman-group"++_ ->
            ?DBG_RECEIVE("Going to send SSH_MSG_KEXDH_INIT:",  Ref, C, Pid),
            ?DBG_RECEIVE("Received SSH_MSG_KEXDH_INIT:",       Ref, D, Pid),
            ?DBG_RECEIVE("Going to send SSH_MSG_KEXDH_REPLY:", Ref, D, Pid),
            ?DBG_RECEIVE("Received SSH_MSG_KEXDH_REPLY:",      Ref, C, Pid)
    end,


    ?DBG_RECEIVE("Going to send SSH_MSG_NEWKEYS:", Ref, C, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_NEWKEYS:",      Ref, D, Pid),

    ?DBG_RECEIVE("Going to send SSH_MSG_NEWKEYS:", Ref, D, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_NEWKEYS:",      Ref, C, Pid),
    
    ?DBG_RECEIVE("Going to send SSH_MSG_SERVICE_REQUEST:", Ref, C, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_SERVICE_REQUEST:",      Ref, D, Pid),
    
    ?DBG_RECEIVE("Going to send SSH_MSG_SERVICE_ACCEPT:", Ref, D, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_SERVICE_ACCEPT:",      Ref, C, Pid),
    
    ?DBG_RECEIVE("Going to send SSH_MSG_USERAUTH_REQUEST:", Ref, C, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_USERAUTH_REQUEST:",      Ref, D, Pid),

    ?DBG_RECEIVE("Going to send SSH_MSG_USERAUTH_FAILURE:", Ref, D, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_USERAUTH_FAILURE:",      Ref, C, Pid),
    
    ?DBG_RECEIVE("Going to send SSH_MSG_USERAUTH_REQUEST:", Ref, C, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_USERAUTH_REQUEST:",      Ref, D, Pid),

    ?DBG_RECEIVE("Going to send SSH_MSG_USERAUTH_SUCCESS:", Ref, D, Pid),
    ?DBG_RECEIVE("Received SSH_MSG_USERAUTH_SUCCESS:",      Ref, C, Pid),


    UnexpectedMsgs =
        dbg_SKIP(Ref,
                 [S_R ++ P ++ ":" || P <- ["SSH_MSG_USERAUTH_REQUEST",
                                           "SSH_MSG_USERAUTH_INFO_REQUEST",
                                           "SSH_MSG_USERAUTH_INFO_RESPONSE",
                                           "SSH_MSG_USERAUTH_FAILURE",
                                           "SSH_MSG_EXT_INFO"
                                          ],
                                     S_R <- ["Going to send ",
                                             "Received "
                                            ]
                 ]),

    ssh:close(C),
    stop_and_fail_if_unhandled_dbg_msgs(UnexpectedMsgs, Ref, [C,D], Pid).

%%--------------------------------------------------------------------
dbg_channels(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir   = proplists:get_value(priv_dir, Config),
    
    Ref = ssh_dbg_start(),
    {ok,[channels,connections]} = ssh_dbg:on([connections, channels]),
    
    Parent = self(),
    TimeoutShell =
        fun() ->
                io:format("TimeoutShell started!~n",[]),
                timer:sleep(1000),
                Parent ! {daemon_channel,Ref,self()},
                ct:log("~p TIMEOUT!",[self()])
        end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
                                             {user_passwords, [{?USR,?PWD}]},
                                             {connectfun, fun(_,_,_) ->
                                                                  Parent ! {daemon_c,Ref,self()}
                                                          end},
                                             {shell, fun(_User) ->
                                                             spawn(TimeoutShell)
                                                     end
                                             },
					     {failfun, fun ssh_test_lib:failfun/2}]),
    
    ?DBG_RECEIVE("Starting LISTENER on ", Ref, _, Pid),

    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                          {user_dir, UserDir},
                                          {user,?USR},
                                          {password,?PWD},
                                          {user_interaction, false}]),
    {ok, Ch0} = ssh_connection:session_channel(C, infinity),
    ok = ssh_connection:shell(C, Ch0),
    
    %% Daemon connection ref (D):
    D = receive {daemon_c,Ref,D0} -> D0 end,

    %% Daemon channel (Dch):
    Dch = receive {daemon_channel,Ref,Dch0} -> Dch0 end,
    ct:log("~p:~p~nC = ~p, D=~p, Dch=~p~n~s",[?MODULE,?LINE, C, D, Dch, ssh_info:string()]),

    ?DBG_RECEIVE("Starting server connection:", Ref, D, Pid),
    ?DBG_RECEIVE("Starting client connection:", Ref, C, Pid),
    ?DBG_RECEIVE("Server Channel Starting:",    Ref, _, Pid),
    ?DBG_RECEIVE("Server Channel Terminating:", Ref, _, Pid),

    stop_and_fail_if_unhandled_dbg_msgs(Ref, [C,D], Pid).

%%--------------------------------------------------------------------
all_dbg(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir   = proplists:get_value(priv_dir, Config),

    Dir0 = filename:join(proplists:get_value(priv_dir,Config), ssh_test_lib:random_chars(10)),
    file:make_dir(Dir0),
    Dir = w2l(Config, Dir0),
    ct:log("~p:~p created the directory~nsDir0 = ~p~nDir  = ~p", [?MODULE,?LINE,Dir0,Dir]),

    AllTags = ssh_dbg:start(),
    {ok,AllTags} = ssh_dbg:on(AllTags),

    {_, Host, Port} =
	ssh_test_lib:daemon([{system_dir, SystemDir},
			     {user_dir,   UserDir},
			     {user_passwords, [{?USR,?PWD}]}
                            ]),

    {ok, ChPid, _C} =
        ssh_sftp:start_channel(Host, Port,
                               [{user_dir, UserDir},
                                {user,?USR},
                                {password,?PWD},
                                {user_interaction, false},
                                {silently_accept_hosts, true}
                               ]),

    {ok, _Files} = ssh_sftp:list_dir(ChPid, Dir).


%%--------------------------------------------------------------------
cb_basic(_Config) ->
    %% Check that the circular buffer is disabled at start:
    [] = ssh_dbg:cbuf_list(),
    disabled = ssh_dbg:cbuf_in(anything),
    [] = ssh_dbg:cbuf_list(),
    %% Start it and enter three values, first is duplicated;
    ok = ssh_dbg:cbuf_start(3),
    ok = ssh_dbg:cbuf_in(v1),
    ok = ssh_dbg:cbuf_in(v1),
    ok = ssh_dbg:cbuf_in(v2),
    ok = ssh_dbg:cbuf_in(v3),
    [{v3,_,1}, {v2,_,1}, {v1,_,2}] = ssh_dbg:cbuf_list(),
    %% Check that a fourth value erase the first entered:
    ok = ssh_dbg:cbuf_in(v4),
    [{v4,_,1}, {v3,_,1}, {v2,_,1}] = ssh_dbg:cbuf_list(),
    %% Check that entering a value that is in the tail but not in the head is treated as a new value:
    ok = ssh_dbg:cbuf_in(v2),
    [{v2,_,1}, {v4,_,1}, {v3,_,1}] = ssh_dbg:cbuf_list(),
    %% Stop and check that the buffer is returned:
    [{v2,_,1}, {v4,_,1}, {v3,_,1}] = ssh_dbg:cbuf_stop_clear(),
    %% Stopping a stopped buffer returns empty:
    [] = ssh_dbg:cbuf_stop_clear(),
    %% Check that a value can't be entered in a stopped buffer:
    disabled = ssh_dbg:cbuf_in(v2).

%%--------------------------------------------------------------------
cb_print(_Config) ->
    ssh_dbg:cbuf_start(),
    [begin
         ssh_dbg:cbuf_in(V),
         ct:log("Enter ~p",[V])
     end || V <- lists:seq(1,10)],
    ct:log("~s",[ssh_dbg:fmt_cbuf_items()]),
    ssh_dbg:cbuf_stop_clear().

%%--------------------------------------------------------------------
cb_macros_print(_Config) ->
    ssh_dbg:cbuf_start(),
    [begin
         V = {test,V0},
         ?CIRC_BUF_IN(V),
         ct:log("Enter ~p",[V])
     end || V0 <- lists:seq(1,5)],
    ct:log("~s",[ssh_dbg:fmt_cbuf_items()]),
    ssh_dbg:cbuf_stop_clear().

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

ssh_dbg_start() ->
    ssh_dbg_start(make_ref()).

ssh_dbg_start(Ref) ->
    Parent = self(),
    [_|_] = ssh_dbg:start(fun(_F,A) ->
                                  Parent ! {Ref,A}
                          end),
    Ref.

%%--------------------------------------------------------------------
setup_dirs(Config) ->
    ct:log("Pub keys setup for: ~p",
           [ssh_test_lib:setup_all_user_host_keys(Config)]),
    Config.

system_dir(Config) -> filename:join(proplists:get_value(priv_dir, Config), system).

user_dir(Config) -> proplists:get_value(priv_dir, Config).

%%--------------------------------------------------------------------
queued_msgs(Ref, Conns) ->
    queued_msgs(Ref, Conns, []).

queued_msgs(Ref, Conns, Acc) ->
    receive
        {Ref, [_, C, _]=Msg} ->
            case is_list(Conns) andalso lists:member(C, Conns) of
                true ->
                    queued_msgs(Ref, [Msg|Acc]);
                false ->
                    queued_msgs(Ref, Conns, Acc)
            end
    after 0 ->
            lists:reverse(Acc)
    end.

%%--------------------------------------------------------------------
fail_if_unhandled_dbg_msgs(Ref, Conns) ->
    fail_if_unhandled_dbg_msgs(queued_msgs(Ref,Conns), Ref, Conns).

fail_if_unhandled_dbg_msgs(Msgs, _Ref, _Conns) ->
    case Msgs of
        [] ->
            ok;
        _ ->
            ct:log("Unexpected messages:~n~p",[Msgs]),
            ct:fail("Unexpected messages")
    end.

%%--------------------------------------------------------------------
stop_and_fail_if_unhandled_dbg_msgs(Ref, Conns, DaemonPid) ->
    stop_and_fail_if_unhandled_dbg_msgs(queued_msgs(Ref,Conns), Ref, Conns, DaemonPid).

stop_and_fail_if_unhandled_dbg_msgs(Msgs, Ref, Conns, DaemonPid) ->
    ssh:stop_daemon(DaemonPid),
    fail_if_unhandled_dbg_msgs(Msgs, Ref, Conns).


%%--------------------------------------------------------------------
dbg_SKIP(Ref, Prefixes) ->
    dbg_SKIP(Ref, Prefixes, []).

dbg_SKIP(Ref, Prefixes, UnexpectedAcc) ->
    receive
        {Ref, [_, _C, Msg]} when is_tuple(Msg) ->
            %% filter non ssh_dbg messages, for example from dbg:tp(..) etc
            dbg_SKIP(Ref, Prefixes, UnexpectedAcc);
        {Ref, [_, _C, Msg]=M} ->
            case lists:any(
                   fun(Pfx) ->
                           try lists:prefix(Pfx, Msg)
                           catch _:_ -> false
                           end
                   end, Prefixes) of
                true ->
                    ct:log("Skip:~n~p", [M]),
                    dbg_SKIP(Ref, Prefixes, UnexpectedAcc);
                false ->
                    dbg_SKIP(Ref, Prefixes, [Msg|UnexpectedAcc])
            end
    after 0 ->
            lists:reverse(UnexpectedAcc)
    end.

%%%----------------------------------------------------------------
w2l(P) ->
    ssh_test_lib:winpath_to_linuxpath(P).

w2l(Config, P) ->
    W2L = proplists:get_value(w2l, Config, fun(X) -> X end),
    W2L(P).

