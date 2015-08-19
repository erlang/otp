%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
-module(httpd_bench_SUITE).
-author('hans@erix.ericsson.se').
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include_lib("public_key/include/public_key.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]}].


all() -> [{group, http}
	  ,{group, https}
	 ].

groups() -> [{http, [],  data_cases()},
	     {https, [], data_cases()}].

data_cases() -> [no_data
		,big_data
		].
    

init_per_suite(Config) -> start_inets(Config).

end_per_suite(Config) -> stop_inets(Config).


init_per_group(Protocol, Config) -> start_web_server(Protocol, Config).

end_per_group(_GroupName, Config) -> stop_web_server(Config).


init_per_testcase(_Func, Config) -> Config.

end_per_testcase(_Func, _Config) -> ok.

%%%================================================================
no_data(Config) -> measure_get("empty_file", Config).

big_data(Config) -> measure_get("big_file", Config).


measure_get(File, Config) ->
    {ok,TestPerSec} = time_fetch(File, Config),
    Name = lists:concat([?config(protocol,Config), " ", File]),
    ct:comment("~p tps",[TestPerSec]),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, TestPerSec},
				 {suite, ?MODULE},
				 {name, Name}]}),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_inets(Config) ->
    inets:stop(),
    ok = inets:start(),
    Config.

stop_inets(Config) ->
    inets:stop(),
    Config.


start_web_server(Protocol, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    ServerName = "localhost",
    ConfHttpd = case Protocol of
		    http -> 
			[];
		    https -> 
			catch ssl:stop(),
			ok = ssl:start(),
			init_ssl(PrivDir),
			[{socket_type, {essl,
					[{cacertfile, 
					  filename:join(PrivDir, "public_key_cacert.pem")},
					 {certfile, 
					  filename:join(PrivDir, "public_key_cert.pem")},
					 {keyfile,
					  filename:join(PrivDir, "public_key_cert_key.pem")}
					]}}]
		end,
    {ok, Pid} = inets:start(httpd, [{port,0},
				    {server_name,ServerName},
				    {server_root,PrivDir}, 
				    {document_root,DataDir}
				    | ConfHttpd]),
    Port = proplists:get_value(port,httpd:info(Pid)),
    F = fun(File) -> 
%%		lists:concat([Protocol,"://",ServerName,":",Port,"/",File]) 
		lists:concat([http,"://",ServerName,":",Port,"/",File]) 
	end,
    [{httpd_pid,Pid},{urlfun,F},{protocol,Protocol},{host,ServerName},{port,Port} | Config].

init_ssl(PrivDir) ->
    CaKey = {_Trusted,_} = 
	erl_make_certs:make_cert([{key, dsa},
				  {subject, 
				   [{name, "Public Key"},
				    {?'id-at-name', 
				     {printableString, "public_key"}},
				    {?'id-at-pseudonym', 
				     {printableString, "pubkey"}},
				    {city, "Stockholm"},
				    {country, "SE"},
				    {org, "erlang"},
				    {org_unit, "testing dep"}
				   ]}
				 ]),
    ok = erl_make_certs:write_pem(PrivDir, "public_key_cacert", CaKey),
    
    CertK1 = {_Cert1, _} = erl_make_certs:make_cert([{issuer, CaKey}]),
    CertK2 = {_Cert2,_} = erl_make_certs:make_cert([{issuer, CertK1}, 
						   {digest, md5}, 
						   {extensions, false}]),
    ok = erl_make_certs:write_pem(PrivDir, "public_key_cert", CertK2).



stop_web_server(Config) ->
    inets:stop(httpd, ?config(httpd_pid,Config)),
    Config.


time_fetch(File, Config) -> 
    Protocol = ?config(protocol, Config),
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    URL = (?config(urlfun,Config))(File),
    Parent = self(),
    Pid = spawn(fun() ->
			receive
			    go -> 
				Parent ! {self(),
					  do_runs(Protocol, URL, Host, Port, 0, 0)}
			end
		end),
    RunSecs = 15,
    timer:send_after(RunSecs*1000, Pid, stop),
    Pid ! go,
    receive
	{Pid,{MicroSecs,NRuns}} ->
	    Tps = 1000000*NRuns/MicroSecs,
	    ct:pal("~p fetches in ~p seconds after ~ps -> ~p Tps",[NRuns,MicroSecs/1000000,RunSecs,Tps]),
	    {ok, Tps}
    end.

do_runs(Protocol, URL, Host, Port, N, MicroSecs) ->
    receive
	stop ->
	    {MicroSecs, N}
    after 0 ->
	    {Time, {ok,200}} = fetch(Protocol, URL, Host, Port),
	    do_runs(Protocol, URL, Host, Port, N+1, MicroSecs+Time)
    end.


fetch(Protocol, URL, Host, Port) ->
    {ok,S} = connect(Protocol, Host, Port),
    ok = send(Protocol, S, ["GET ",URL," HTTP/1.1\r\n\r\n"]),
    R = timer:tc(fun() -> receive_response(Protocol,S) end),
    close(Protocol, S),
    R.


connect(http, Host, Port) -> 
    gen_tcp:connect(Host, Port, [binary,{active,once},{packet,http_bin},{reuseaddr,true}]);
connect(https, Host, Port) -> 
    ssl:connect(Host, Port, [binary,{active,once},{packet,http_bin},{reuseaddr,true}]).

send(http, Sock, Data) ->
    %%ct:pal("tcp send ~p",[Data]),
    gen_tcp:send(Sock, Data);
send(https, Sock, Data) ->
    %%ct:pal("ssl send ~p",[Data]),
    ssl:send(Sock, Data).

close(http, Sock) ->
    gen_tcp:close(Sock);
close(https, Sock) ->
    ssl:close(Sock).

receive_response(http, S) -> recv_loop(http, S, undefined, undefined, <<>>);
receive_response(https, S) -> recv_loop(ssl, S, undefined, undefined, <<>>).


recv_loop(_, _S, Code, 0, _Acc) ->
    {ok,Code};

recv_loop(P, S, Code, MissingBytes, Acc) ->
    receive
	{P, S,
	 {http_response,_Ver,RespCode,_Slogan}
	} ->
	    %%ct:pal("http_response ~p",[RespCode]),
	    (inet_mod(P)):setopts(S, [{active,once}]),
	    recv_loop(P, S, RespCode, MissingBytes, Acc);

	{P, S, 
	 {http_header,_,'Content-Length',_,BinValue}
	} ->
	    %%ct:pal("Content-Length ~p",[BinValue]),
	    (inet_mod(P)):setopts(S, [{active,once}]),
	    recv_loop(P, S, Code, binary_to_integer(BinValue), Acc);
	    
	{P, S, http_eoh} ->
	    %%ct:pal("http_eoh",[]),
	    (inet_mod(P)):setopts(S, [{active,once},{packet,0}]),
	    recv_loop(P, S, Code, MissingBytes, Acc);

	{ssl, S, Bin} when is_binary(Bin) ->
	    %%ct:pal("ssl bin size ~p~n~s",[size(Bin),Bin]),
	    (inet_mod(P)):setopts(S, [{active,once}]),
	    recv_loop(P, S, Code, MissingBytes-size(Bin), <<Acc/binary,Bin/binary>>);

	{tcp, S, Bin} ->
	    %%ct:pal("tcp bin size ~p~n~s",[size(Bin),Bin]),
	    (inet_mod(P)):setopts(S, [{active,once}]),
	    recv_loop(P, S, Code, MissingBytes-size(Bin), <<Acc/binary,Bin/binary>>);

	{P, S, _Pkt} ->
	    %%ct:pal("other http(s) ~p ~p",[P, _Pkt]),
	    (inet_mod(P)):setopts(S, [{active,once}]),
	    recv_loop(P, S, Code, MissingBytes, Acc);

	{tcp_closed, S} ->
	    %%ct:pal("tcp closed",[]),
	    closed(MissingBytes, Code);

	{ssl_closed, S} -> 
	    %%ct:pal("ssl closed",[]),
	    closed(MissingBytes, Code)

	%% Other -> ct:pal("Unknown ~p",[Other]),
	%% 	 recv_loop(P, S, Code, MissingBytes, Acc)

    after 20000 -> timeout
    end.


closed(MissingBytes, Code) when MissingBytes==0 -> {ok,Code};
closed(MissingBytes, _Code) when MissingBytes>0 -> {missing,MissingBytes};
closed(MissingBytes, _Code) when MissingBytes<0 -> {too_many,MissingBytes}.


    

inet_mod(http) -> inet;
inet_mod(ssl) -> ssl.
    
