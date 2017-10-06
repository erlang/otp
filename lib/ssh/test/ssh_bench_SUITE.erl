%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2017. All Rights Reserved.
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
-module(ssh_bench_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct_event.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("ssh/src/ssh.hrl").
-include_lib("ssh/src/ssh_transport.hrl").
-include_lib("ssh/src/ssh_connect.hrl").
-include_lib("ssh/src/ssh_userauth.hrl").

%%%================================================================
%%%
%%% Suite declarations
%%% 

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]},
	    {timetrap,{minutes,1}}
	   ].
all() -> [connect,
          transfer_text
	 ].

-define(UID, "foo").
-define(PWD, "bar").
-define(Nruns, 8).

%%%================================================================
%%%
%%% Init per suite
%%% 

init_per_suite(Config) ->
    catch ssh:stop(),
    try 
	ok = ssh:start()
    of
        ok ->
            DataSize = 1000000,
            SystemDir = proplists:get_value(data_dir, Config),
            Algs = ssh:default_algorithms(),
            {_ServerPid, _Host, Port} =
                ssh_test_lib:daemon([{system_dir, SystemDir},
                                     {user_passwords, [{?UID,?PWD}]},
                                     {failfun, fun ssh_test_lib:failfun/2},
                                     {preferred_algorithms, Algs},
                                     {modify_algorithms,[{prepend,[{cipher,[none]},
                                                                   {mac,[none]}
                                                                  ]},
                                                         {rm, [{cipher,['aes256-gcm@openssh.com',
                                                                        'aes128-gcm@openssh.com']}
                                                              ]}
                                                        ]},
                                     {max_random_length_padding, 0},
                                     {subsystems, [{"/dev/null", {ssh_bench_dev_null,[DataSize]}}]}
                                    ]),
            [{host,"localhost"}, {port,Port}, {uid,?UID}, {pwd,?PWD}, {data_size,DataSize} | Config]
    catch
	C:E ->
	    {skip, io_lib:format("Couldn't start ~p:~p",[C,E])}
    end.
    
end_per_suite(_Config) ->
    catch ssh:stop(),
    ok.

%%%================================================================
%%%
%%% Init per testcase
%%% 

init_per_testcase(_Func, Conf) ->
    Conf.

end_per_testcase(_Func, _Conf) ->
    ok.

%%%================================================================
%%%
%%% Testcases
%%%

%%%----------------------------------------------------------------
%%% Measure the time for an Erlang client to connect to an Erlang
%%% server on the localhost

connect(Config) ->
    KexAlgs = proplists:get_value(kex, ssh:default_algorithms()),
    ct:log("KexAlgs = ~p",[KexAlgs]),
    lists:foreach(
      fun(KexAlg) ->
              PrefAlgs = preferred_algorithms(KexAlg),
              report([{value, measure_connect(Config,
                                              [{preferred_algorithms,PrefAlgs}])},
                      {suite, ?MODULE},
                      {name, mk_name(["Connect erlc erld ",KexAlg," [µs]"])}
                     ])
      end, KexAlgs).


measure_connect(Config, Opts) ->
    Port = proplists:get_value(port, Config),
    ConnectOptions = [{user,     proplists:get_value(uid,      Config)},
                      {password, proplists:get_value(pwd,      Config)},
                      {user_dir, proplists:get_value(priv_dir, Config)},
                      {silently_accept_hosts, true},
                      {user_interaction, false},
                      {max_random_length_padding, 0}
                     ] ++ Opts,
    median(
      [begin
           {Time, {ok,Pid}} = timer:tc(ssh,connect,["localhost", Port, ConnectOptions]),
           ssh:close(Pid),
           Time
       end || _ <- lists:seq(1,?Nruns)]).

%%%----------------------------------------------------------------
%%% Measure the time to transfer a set of data with
%%% and without crypto

transfer_text(Config) ->
    Port = proplists:get_value(port, Config),
    Options = [{user,     proplists:get_value(uid,      Config)},
               {password, proplists:get_value(pwd,      Config)},
               {user_dir, proplists:get_value(priv_dir, Config)},
               {silently_accept_hosts, true},
               {user_interaction, false},
               {max_random_length_padding, 0}
              ],
    Data = gen_data(proplists:get_value(data_size,Config)),

    [connect_measure(Port, Crypto, Mac, Data, Options)
     || {Crypto,Mac} <- [{        none,                    none},
                         {'aes128-ctr',             'hmac-sha1'},
                         {'aes256-ctr',             'hmac-sha1'},
%%                         {'aes128-gcm@openssh.com', 'hmac-sha1'},
                         {'aes128-cbc',             'hmac-sha1'},
                         {'3des-cbc',               'hmac-sha1'},
                         {'aes128-ctr',             'hmac-sha2-256'},
                         {'aes128-ctr',             'hmac-sha2-512'}
                        ],
        crypto_mac_supported(Crypto,Mac)].
    

crypto_mac_supported(none, none) ->
    true;
crypto_mac_supported(C, M) -> 
    Algs = ssh:default_algorithms(),
    [{_,Cs},_] = proplists:get_value(cipher, Algs),
    [{_,Ms},_] = proplists:get_value(mac, Algs),
    lists:member(C,Cs) andalso lists:member(M,Ms).
    

gen_data(DataSz) ->
    Data0 = << <<C>> || _ <- lists:seq(1,DataSz div 256),
                        C <- lists:seq(0,255) >>,
    Data1 = << <<C>> || C <- lists:seq(0,(DataSz rem 256) - 1) >>,
    <<Data0/binary, Data1/binary>>.


%% connect_measure(Port, Cipher, Mac, Data, Options) ->
%%     report([{value, 1},
%%              {suite, ?MODULE},
%%              {name, mk_name(["Transfer 1M bytes ",Cipher,"/",Mac," [µs]"])}]);
connect_measure(Port, Cipher, Mac, Data, Options) ->
    AES_GCM = {cipher,['aes256-gcm@openssh.com',
                       'aes128-gcm@openssh.com']},

    AlgOpt = case {Cipher,Mac} of
                 {none,none} ->
                     [{modify_algorithms,[{prepend, [{cipher,[Cipher]},
                                                     {mac,[Mac]}]},
                                          {rm,[AES_GCM]}
                                         ]}];
                 {none,_} ->
                     [{modify_algorithms,[{prepend, [{cipher,[Cipher]}]},
                                          {rm,[AES_GCM]}
                                         ]},
                      {preferred_algorithms, [{mac,[Mac]}]}];
                 {_,none} ->
                     [{modify_algorithms,[{prepend, [{mac,[Mac]}]},
                                          {rm,[AES_GCM]}
                                         ]},
                      {preferred_algorithms, [{cipher,[Cipher]}]}];
                 _ ->
                     [{preferred_algorithms, [{cipher,[Cipher]},
                                              {mac,[Mac]}]},
                      {modify_algorithms, [{rm,[AES_GCM]}]}
                     ]
             end,
    Times =
        [begin
             {ok,C} = ssh:connect("localhost", Port, AlgOpt ++ Options),
             {ok,Ch} = ssh_connection:session_channel(C, 10000),
             success = ssh_connection:subsystem(C, Ch, "/dev/null", 10000),
             {Time,ok} = timer:tc(?MODULE, send_wait_acc, [C, Ch, Data]),
             ok = ssh_connection:send_eof(C, Ch),
             ssh:close(C),
             Time
         end || _ <- lists:seq(1,?Nruns)],
    
    report([{value, median(Times)},
            {suite, ?MODULE},
            {name, mk_name(["Transfer 1M bytes ",Cipher,"/",Mac," [µs]"])}]).

send_wait_acc(C, Ch, Data) ->
    ssh_connection:send(C, Ch, Data),
    receive
        {ssh_cm, C, {data, Ch, 0, <<"READY">>}} -> ok
    end.
            

%%%================================================================
%%%
%%% Private
%%% 

%%%----------------------------------------------------------------
mk_name(Name) -> [char(C) || C <- lists:concat(Name)].

char($-) -> $_;
char(C) -> C.

%%%----------------------------------------------------------------
preferred_algorithms(KexAlg) ->
     [{kex,         [KexAlg]},
      {public_key,  ['ssh-rsa']},
      {cipher,      ['aes128-ctr']},
      {mac,         ['hmac-sha1']},
      {compression, [none]}
     ].

%%%----------------------------------------------------------------
median(Data) when is_list(Data) ->
    SortedData = lists:sort(Data),
    N = length(Data),
    Median =
        case N rem 2 of
            0 ->
                MeanOfMiddle = (lists:nth(N div 2, SortedData) +
                                    lists:nth(N div 2 + 1, SortedData)) / 2,
                round(MeanOfMiddle);
            1 ->
                lists:nth(N div 2 + 1, SortedData)
        end,
    ct:log("median(~p) = ~p",[SortedData,Median]),
    Median.


report(Data) ->
    ct:log("EventData = ~p",[Data]),
    ct_event:notify(#event{name = benchmark_data,
                           data = Data}).
