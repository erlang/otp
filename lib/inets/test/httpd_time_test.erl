%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(httpd_time_test).

-export([t/3, t1/2, t2/2, t4/2]).

-export([do/1, do/2, do/3, do/4, do/5]).

-export([main/5, poller_main/4, poller_loop/4]).

-include("inets_test_lib.hrl").

-record(stat, {pid, time = undefined, count = undefined, res}).

%% -define(NUM_POLLERS, 10).
-define(NUM_POLLERS, 1).


%%% -----------------------------------------------------------------
%%% Test suite interface
%%%

t1(Host, Port) ->
    t(ip_comm, Host, Port).


t2(Host, Port) ->
    t(ssl, Host, Port).


t4(Host, Port) ->
    t(essl, Host, Port).


t(SocketType, Host, Port) ->
    %% put(dbg,true),
    main(?NUM_POLLERS, SocketType, Host, Port, 60000).
	    


%%% -----------------------------------------------------------------
%%% Public interface when running the time test manually...
%%%

do(Port) ->
    do(ip_comm, hostname(), Port).

do(Port, Time) when is_integer(Port) andalso is_integer(Time) ->
    do(ip_comm, hostname(), Port, Time);

do(Host, Port) ->
    do(ip_comm, Host, Port).

do(Host, Port, Time) when is_integer(Port) andalso is_integer(Time) ->
    do(1, ip_comm, Host, Port, Time);

do(SocketType, Host, Port) when is_integer(Port) ->
    do(1, SocketType, Host, Port, 60000).

do(N, SocketType, Host, Port) when is_integer(N) andalso is_integer(Port) ->
    do(N, SocketType, Host, Port, 60000);

do(SocketType, Host, Port, Time) 
  when is_integer(Port) andalso is_integer(Time) ->
    do(1, SocketType, Host, Port, Time).

do(N, SocketType, Host, Port, Time) 
  when is_integer(N) andalso is_integer(Port) andalso is_integer(Time) ->
    do_it(N, SocketType, Host, Port, Time).

do_it(N, SocketType, Host, Port, Time) ->
    d("do_it -> entry with"
      "~n   N:          ~p"
      "~n   SocketType: ~p"
      "~n   Host:       ~p"
      "~n   Port:       ~p"
      "~n   Time:       ~p", [N, SocketType, Host, Port, Time]),
    proc_lib:spawn(?MODULE, main, [N, SocketType, Host, Port, Time]).



%%% -----------------------------------------------------------------
%%% Controller (main) process
%%%

main(N, SocketType, Host, Port, Time) 
  when is_integer(N) andalso 
       is_atom(SocketType) andalso 
       is_integer(Port) andalso 
       is_integer(Time) ->
    process_flag(trap_exit,true),
    put(sname,ctrl),
    %% put(dbg,true),
    d("main -> entry"),
    Pollers = start_pollers(N, [self(), SocketType, Host, Port]),
    d("main -> Pollers: ~p", [Pollers]),
    loop(Pollers, Time).

loop(Pollers, Timeout) ->
    d("loop -> entry when"
      "~n   Timeout: ~p", [Timeout]),
    Start = erlang:monotonic_time(),

    receive 
	{'EXIT', Pid, {poller_stat_failure, SocketType, Host, Port, Time, Reason}} ->
	    case is_poller(Pid, Pollers) of
		true ->
		    error_msg("received unexpected exit from poller ~p~n"
			      "before completion of test "
			      "after ~p micro sec"
			      "~n   SocketType: ~p"
			      "~n   Host:       ~p"
			      "~n   Port:       ~p"
			      "~n~p~n", 
			      [Pid, SocketType, Host, Port, Time, Reason]),
		    exit({fail, {poller_exit, Pid, Time, Reason}});
		false ->
		    error_msg("received unexpected ~p from ~p"
			      "befor completion of test", [Reason, Pid]),
		    loop(Pollers, Timeout - inets_lib:millisec_passed(Start))
	    end;

	{poller_stat_failure, Pid, {SocketType, Host, Port, Time, Reason}} ->
	    error_msg("received stat failure ~p from poller ~p after ~p "
		      "befor completion of test" 
		      "~n   SocketType: ~p"
		      "~n   Host:       ~p"
		      "~n   Port:       ~p", 
		      [Reason, Pid, Time, SocketType, Host, Port]),
	    exit({fail, {poller_failure, Pid, Time, Reason}});

	{poller_stat_failure, Pid, SocketType, Host, Port, Reason} ->
	    error_msg("received stat failure ~p from poller ~p "
		      "befor completion of test" 
		      "~n   SocketType: ~p"
		      "~n   Host:       ~p"
		      "~n   Port:       ~p", 
		      [Reason, Pid, SocketType, Host, Port]),
	    exit({fail, {poller_failure, Pid, Reason}});

	Any ->
	    error_msg("received unexpected message befor completion of test: "
		      "~n   ~p", [Any]),
	    exit({fail, Any})

    after Timeout ->
	    d("loop -> timeout: stop pollers"),
	    stop_pollers(Pollers),
	    d("loop -> collect poller statistics"),
	    Stats = collect_poller_stat(Pollers, []),
	    d("loop -> Stats: ~p", [Stats]),
	    display_poller_stat(Stats, Timeout),
	    ok
    end.

collect_poller_stat([], PollersStat) ->
    PollersStat;
collect_poller_stat(Pollers, PollersStat) ->
    d("collect_poller_stat -> entry with"
      "~n   Pollers:     ~p"
      "~n   PollersStat: ~p", [Pollers, PollersStat]),
    receive 
	{poller_statistics, Poller, {Time, Count}} ->
	    d("collect_poller_stat -> got statistics from ~p", [Poller]),
	    case lists:keysearch(Poller, 2, Pollers) of
		{value, PollerStat} ->
		    d("collect_poller_stat -> current statistic record: ~p", 
		      [PollerStat]),
		    P = lists:keydelete(Poller, 2, Pollers),
		    d("collect_poller_stat -> P: ~p", [P]),
		    S = PollerStat#stat{time = Time, count = Count, res = ok},
		    d("collect_poller_stat -> S: ~p", [S]),
		    collect_poller_stat(P, [S | PollersStat]);
		false ->
		    error_msg("statistics already received for ~p", [Poller]),
		    collect_poller_stat(Pollers, PollersStat)
	    end;
	{poller_stat_failure, Poller, Else} ->
	    error_msg("poller statistics failure for ~p with ~p", 
		      [Poller, Else]),
	    case lists:keysearch(Poller, 2, Pollers) of
		{value, PollerStat} ->
		    P = lists:keydelete(Poller, 2, Pollers),
		    S = PollerStat#stat{res = {error, Else}},
		    collect_poller_stat(P, [S | PollerStat]);
		false ->
		    error_msg("statistics already received for ~p", [Poller]),
		    collect_poller_stat(Pollers, PollersStat)
	    end
    end.


display_poller_stat(Stats, T) ->
    display_poller_stat(Stats, 1, T, 0).
    
display_poller_stat([], _, TestTime, AccCount) ->
    io:format("Total statistics:~n"
	      "   Accumulated count:   ~w~n"
	      "   Average access time: ~w milli sec~n", 
	      [AccCount, (TestTime/AccCount)]);
display_poller_stat([#stat{res = ok} = Stat | Stats], N, TestTime, AccCount) ->
    #stat{pid = Pid, time = Time, count = Count} = Stat,
    io:format("Statistics for poller ~p (~p):~n"
	      "   time:                ~w seconds~n"
	      "   count:               ~w~n"
	      "   Average access time: ~w milli sec~n", 
	      [Pid, N, Time/(1000*1000), Count, (TestTime/Count)]),
    display_poller_stat(Stats, N + 1, TestTime, AccCount+Count);
display_poller_stat([Stat | Stats], N, TestTime, AccCount) ->
    #stat{pid = Pid, res = Error} = Stat,
    io:format("Statistics failed for poller ~p (~p):~n"
	      "   ~p~n", [Pid, N, Error]),
    display_poller_stat(Stats, N + 1, TestTime, AccCount).
    
    
    
%%% -----------------------------------------------------------------
%%% Poller process
%%%

start_pollers(N, Args) ->
    start_pollers(N, Args, []).

start_pollers(0, _Args, Pollers) ->
    Pollers;
start_pollers(N, Args, Pollers) ->
    Pid = proc_lib:spawn_link(?MODULE, poller_main, Args),
    start_pollers(N-1, Args, [#stat{pid = Pid} | Pollers]).

stop_pollers(Pollers) ->
    [Pid ! stop || #stat{pid = Pid} <- Pollers],
    await_stop_pollers(Pollers).

await_stop_pollers([]) ->
    ok;
await_stop_pollers(Pollers0) ->
    receive
	{'EXIT', Pid, _Reason} ->
	    Pollers = lists:keydelete(Pid, 2, Pollers0),
	    await_stop_pollers(Pollers)
    after 5000 ->
	    [Pid ! shutdown || #stat{pid = Pid} <- Pollers0]
    end.


is_poller(_, []) ->
    false;
is_poller(Pid, [#stat{pid = Pid}|_]) ->
    true;
is_poller(Pid, [_|Rest]) ->
    is_poller(Pid, Rest).


poller_main(Parent, SocketType, Host, Port) ->
    process_flag(trap_exit,true),
    put(sname, poller),
    case timer:tc(?MODULE, poller_loop, [SocketType, Host, Port, uris()]) of
	{Time, Count} when is_integer(Time) andalso is_integer(Count) ->
	    Parent ! {poller_statistics, self(), {Time, Count}};
	{Time, {'EXIT', Reason}} when is_integer(Time) ->
	    exit({poller_stat_failure, SocketType, Host, Port, Time, Reason});
	{Time, Other} when is_integer(Time) ->
	    Parent ! {poller_stat_failure, self(), {SocketType, Host, Port, Time, Other}};
	Else ->
	    Parent ! {poller_stat_failure, self(), SocketType, Host, Port, Else}
    end.

	    
uris() ->
    uris(get(uris)).

uris(L) when is_list(L) ->
    L;
uris(_) ->
    ["/",
     "/index.html"].


poller_loop(SocketType, Host, Port, URIs) ->
    poller_loop(SocketType, Host, Port, URIs, 0).

poller_loop(SocketType, Host, Port, URIs, Count) ->
    receive 
	stop ->
	    Count
    after 0 ->
	    case poller_loop1(SocketType, Host, Port, URIs) of
		done ->
		    poller_loop(SocketType, Host, Port, URIs, 
				Count + length(URIs));
		{error, Reason, FailURI, FailURIs} ->
		    SuccessCount = 
			Count + (length(URIs) - (length(FailURIs) + 1)),
		    exit({Reason, FailURI, SuccessCount})
	    end
    end.


poller_loop1(_SocketType, _Host, _Port, []) ->
    done;
poller_loop1(SocketType, Host, Port, [URI | URIs]) ->
    Res = inets_test_lib:connect_byte(SocketType, Host, Port),
    case (catch poll(Res, SocketType, URI, "200")) of
	ok ->
	    poller_loop1(SocketType, Host, Port, URIs);
	{'EXIT', Reason} ->
	    {error, Reason, URI, URIs}
    end.

poll({ok, Socket}, SocketType, URI, ExpRes) ->
    Req = "GET " ++ URI ++ " HTTP/1.0\r\n\r\n",
    Res = inets_test_lib:send(SocketType, Socket, Req), 
    await_poll_response(Res, SocketType, Socket, ExpRes);
poll({error, Reason}, _SocketType, _URI, _ExpRes) ->
    exit({failed_creating_socket, Reason});
poll(Error, _SocketType, _URI, _ExpRes) ->
    exit({failed_creating_socket, Error}).

await_poll_response(ok, SocketType, Socket, ExpStatusCode) ->
    await_poll_response2(SocketType, Socket, ExpStatusCode, []);
await_poll_response(Error, _SocketType, _Socket, _ExpStatusCode) ->
    exit(Error).

%% The reply *can* be split into two messages (this is a 
%% result of OTP-9757 for ssl), so we read them all until 
%% the sockets closes, then we analyze the response.
await_poll_response2(SocketType, Socket, ExpStatusCode, Data) ->
    receive 
	%% SSL receives
	{ssl, Socket, NewData} ->
	    d("await_poll_response2 -> "
	      "received part (~w bytes) of the response", [sz(NewData)]),
            await_poll_response2(SocketType, Socket, ExpStatusCode, 
				 [NewData | Data]);
	{ssl_closed, Socket} -> 
	    %% We are done or we failed
	    d("await_poll_response2 -> "
	      "we are done after receiving ~w bytes data", [sz(Data)]),
	    validate(ExpStatusCode, SocketType, Socket, 
		     lists:flatten(lists:reverse(Data)));
	{ssl_error, Socket, Error} ->
	    exit({connection_error, Error});

	%% TCP receives
        {tcp, Socket, NewData} ->
	    d("await_poll_response2 -> "
	      "received part (~w bytes) of the response", [sz(NewData)]),
            await_poll_response2(SocketType, Socket, ExpStatusCode, 
				 [NewData | Data]);
        {tcp_closed, Socket} ->
	    %% We are done or we failed
	    d("await_poll_response2 -> "
	      "we are done after receiving ~w bytes data", [sz(Data)]),
	    validate(ExpStatusCode, SocketType, Socket, 
		     lists:flatten(lists:reverse(Data)));
	{tcp_error, Socket, Error} ->
	    exit({connection_error, Error})

    after 10000 ->
	    d("we timed out while waiting for response, "
	      "validate whatever we got so far"),
	    validate(ExpStatusCode, SocketType, Socket, 
		     lists:flatten(lists:reverse(Data)))
	    %% exit(response_timed_out)
    end.

validate(ExpStatusCode, _SocketType, _Socket, Response) ->
    %% Sz = sz(Response),
    %% trash_the_rest(Socket, Sz),
    %% inets_test_lib:close(SocketType, Socket),
    case re:split(Response," ", [{return, list}]) of
        ["HTTP/1.0", ExpStatusCode|_] ->
            ok;
        ["HTTP/1.0", StatusCode|_] -> 
	    error_msg("Unexpected status code: ~p (~s). "
		      "Expected status code: ~p (~s)", 
		      [StatusCode,    status_to_message(StatusCode),
		       ExpStatusCode, status_to_message(ExpStatusCode)]),
            exit({unexpected_response_code, StatusCode, ExpStatusCode});
	["HTTP/1.1", ExpStatusCode|_] ->
            ok;
	["HTTP/1.1", StatusCode|_] -> 
	    error_msg("Unexpected status code: ~p (~s). "
		      "Expected status code: ~p (~s)", 
		      [StatusCode,    status_to_message(StatusCode),
		       ExpStatusCode, status_to_message(ExpStatusCode)]),
            exit({unexpected_response_code, StatusCode, ExpStatusCode});
	{error, Reason} -> 
	    error_msg("Failed processing response: ~p (~s)", 
		      [Reason, Response]),
            exit({failed_response_processing, Reason, Response});
	Unexpected ->
	    error_msg("Unexpected response split: ~p (~s)", 
		      [Unexpected, Response]),
            exit({unexpected_response, Unexpected, Response})
    end.


sz(L) when is_list(L) ->
    length(lists:flatten(L));
sz(B) when is_binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.


%% --------------------------------------------------------------
%%
%% Status code to printable string
%%

status_to_message(L) when is_list(L) ->
    case (catch list_to_integer(L)) of
        I when is_integer(I) ->
            status_to_message(I);
        _ ->
            io_lib:format("UNKNOWN STATUS CODE: '~p'",[L])
    end;
status_to_message(100) -> "Section 10.1.1: Continue";
status_to_message(101) -> "Section 10.1.2: Switching Protocols";
status_to_message(200) -> "Section 10.2.1: OK";
status_to_message(201) -> "Section 10.2.2: Created";
status_to_message(202) -> "Section 10.2.3: Accepted";
status_to_message(203) -> "Section 10.2.4: Non-Authoritative Information";
status_to_message(204) -> "Section 10.2.5: No Content";
status_to_message(205) -> "Section 10.2.6: Reset Content";
status_to_message(206) -> "Section 10.2.7: Partial Content";
status_to_message(300) -> "Section 10.3.1: Multiple Choices";
status_to_message(301) -> "Section 10.3.2: Moved Permanently";
status_to_message(302) -> "Section 10.3.3: Found";
status_to_message(303) -> "Section 10.3.4: See Other";
status_to_message(304) -> "Section 10.3.5: Not Modified";
status_to_message(305) -> "Section 10.3.6: Use Proxy";
status_to_message(307) -> "Section 10.3.8: Temporary Redirect";
status_to_message(400) -> "Section 10.4.1: Bad Request";
status_to_message(401) -> "Section 10.4.2: Unauthorized";
status_to_message(402) -> "Section 10.4.3: Peyment Required";
status_to_message(403) -> "Section 10.4.4: Forbidden";
status_to_message(404) -> "Section 10.4.5: Not Found";
status_to_message(405) -> "Section 10.4.6: Method Not Allowed";
status_to_message(406) -> "Section 10.4.7: Not Acceptable";
status_to_message(407) -> "Section 10.4.8: Proxy Authentication Required";
status_to_message(408) -> "Section 10.4.9: Request Time-Out";
status_to_message(409) -> "Section 10.4.10: Conflict";
status_to_message(410) -> "Section 10.4.11: Gone";
status_to_message(411) -> "Section 10.4.12: Length Required";
status_to_message(412) -> "Section 10.4.13: Precondition Failed";
status_to_message(413) -> "Section 10.4.14: Request Entity Too Large";
status_to_message(414) -> "Section 10.4.15: Request-URI Too Large";
status_to_message(415) -> "Section 10.4.16: Unsupported Media Type";
status_to_message(416) -> "Section 10.4.17: Requested range not satisfiable";
status_to_message(417) -> "Section 10.4.18: Expectation Failed";
status_to_message(500) -> "Section 10.5.1: Internal Server Error";
status_to_message(501) -> "Section 10.5.2: Not Implemented";
status_to_message(502) -> "Section 10.5.3: Bad Gatteway";
status_to_message(503) -> "Section 10.5.4: Service Unavailable";
status_to_message(504) -> "Section 10.5.5: Gateway Time-out";
status_to_message(505) -> "Section 10.5.6: HTTP Version not supported";
status_to_message(Code) -> io_lib:format("Unknown status code: ~p",[Code]).

%% ----------------------------------------------------------------



% close(Socket) ->
%     gen_tcp:close(Socket).

% send(Socket, Data) ->
%     gen_tcp:send(Socket, Data).


hostname() ->    
    {ok, Hostname} = inet:gethostname(),
    hostname(Hostname).

hostname(Hostname) when is_list(Hostname) ->
    list_to_atom(Hostname);
hostname(Hostname) ->
    Hostname.

%% ----------------------------------------------------------------

error_msg(F,A) -> error_logger:error_msg(F ++ "~n",A).

d(F) ->
    d(get(dbg),F,[]).

d(F,A) ->
    d(get(dbg),F,A).

d(true, F, A) ->
    io:format("DBG ~p ~p " ++ F ++ "~n", [self(),get(sname)]++A);
d(_,_,_) ->
    ok.
