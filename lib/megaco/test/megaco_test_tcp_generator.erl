%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: megaco_tcp sequence generator for the megaco test suite
%%----------------------------------------------------------------------

-module(megaco_test_tcp_generator).

-behaviour(megaco_test_generator).

-compile({no_auto_import,[error/1]}).

%% API
-export([
	 start_link/1, start_link/2, 
	 stop/1, 
	 exec/2, exec/3
	]).

%% genarator behaviour callback exports
-export([
	 init/1,
	 handle_parse/2,
	 handle_exec/2,
	 terminate/2
	]).


-record(state,
        {
          listen,     % Listen socket
          connection, % Connection socket
          encode,     % Encode fun
          decode,     % Decode fun
          result = [] % Accumulated results from exec
         }).


%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

start_link(Name) ->
    megaco_test_generator:start_link(?MODULE, [], Name).

start_link(Name, Node) ->
    megaco_test_generator:start_link(?MODULE, [], Name, Node).

stop(Server) ->
    megaco_test_generator:stop(Server).

exec(Server, Instructions) when is_list(Instructions) ->
    megaco_test_generator:exec(Server, Instructions).

exec(Server, Instructions, Timeout) when is_list(Instructions) ->
    megaco_test_generator:exec(Server, Instructions, Timeout).


%%----------------------------------------------------------------------
%% generator callback functions
%%----------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.


%% ----- instruction parser -----

handle_parse({debug, Debug} = Instruction, State) 
  when (Debug == true) orelse (Debug == false) ->
    {ok, Instruction, State};

handle_parse({decode, Decode} = Instruction, State) 
  when is_function(Decode) ->
    {ok, Instruction, State};

handle_parse({encode, Encode} = Instruction, State) 
  when is_function(Encode) ->
    {ok, Instruction, State};

handle_parse(disconnect = Instruction, State) ->
    {ok, Instruction, State};

handle_parse({listen, Port} = Instruction, State) 
  when is_integer(Port) andalso (Port >= 0) ->
    {ok, Instruction, State};

handle_parse({expect_accept, any} = _Instruction, State) ->
    {ok, {expect_accept, {any, infinity}}, State};

handle_parse({expect_accept, {any, To}} = Instruction, State) 
  when (is_integer(To) andalso (To >= 0)) orelse (To == infinity) ->
    {ok, Instruction, State};

handle_parse({expect_accept, {Host, To}} = _Instruction, State) 
  when (is_integer(To) andalso (To >= 0)) orelse (To == infinity) ->
    case inet:getaddr(Host, inet) of
	{ok, Addr} ->
	    Instruction = {expect_accept, {Addr, To}},
	    {ok, Instruction, State};
	{error, Reason} ->
	    {error, {bad_host, Host, Reason}}
    end;

handle_parse({expect_accept, Host} = _Instruction, State) ->
    case inet:getaddr(Host, inet) of
	{ok, Addr} ->
	    Instruction = {expect_accept, {Addr, infinity}},
	    {ok, Instruction, State};
	{error, Reason} ->
	    {error, {bad_host, Host, Reason}}
    end;

handle_parse({active, NewState} = Instruction, State) 
  when (NewState == true)  orelse 
       (NewState == false) orelse 
       (NewState == once) ->
    {ok, Instruction, State};

handle_parse({connect, Port} = _Instruction, State) 
  when is_integer(Port) andalso (Port >= 0) ->
    Host = 
	case inet:gethostname() of
	    {ok, Hostname} ->
		Hostname;
	    {error, Reason1} ->
		error({failed_geting_own_hostname, Reason1})
	end,
    case inet:getaddr(Host, inet) of
	{ok, Address} ->
	    Instruction = {connect, {Address, Port, infinity}},
	    {ok, Instruction, State};
	{error, Reason2} ->
	    {error, {bad_host, Host, Reason2}}
    end;

handle_parse({connect, {Port, To}} = _Instruction, State) 
  when (is_integer(Port) andalso 
	(Port >= 0)) andalso ((is_integer(To) andalso (To >= 0)) orelse 
			      (To == infinity)) ->
    Host = 
	case inet:gethostname() of
	    {ok, Hostname} ->
		Hostname;
	    {error, Reason1} ->
		error({failed_geting_own_hostname, Reason1})
	end,
    case inet:getaddr(Host, inet) of
	{ok, Address} ->
	    Instruction = {connect, {Address, Port, To}},
	    {ok, Instruction, State};
	{error, Reason2} ->
	    {error, {bad_host, Host, Reason2}}
    end;

handle_parse({connect, {Host, Port}} = _Instruction, State) 
  when (is_integer(Port) andalso (Port >= 0)) ->
    case inet:getaddr(Host, inet) of
	{ok, Address} ->
	    Instruction = {connect, {Address, Port, infinity}},
	    {ok, Instruction, State};
	{error, Reason} ->
	    {error, {bad_host, Host, Reason}}
    end;

handle_parse({connect, {Host, Port, To}} = _Instruction, State) 
  when (is_integer(Port) andalso 
	(Port >= 0)) andalso ((is_integer(To) andalso (To >= 0)) orelse 
			      (To == infinity)) ->
    case inet:getaddr(Host, inet) of
	{ok, Address} ->
	    Instruction = {connect, {Address, Port, To}},
	    {ok, Instruction, State};
	{error, Reason} ->
	    {error, {bad_host, Host, Reason}}
    end;

handle_parse({sleep, To} = Instruction, State) 
  when is_integer(To) andalso (To > 0) ->
    {ok, Instruction, State};

handle_parse({expect_nothing, To} = Instruction, State) 
  when is_integer(To) andalso (To > 0) ->
    {ok, Instruction, State};

handle_parse({expect_closed, To} = Instruction, State) 
  when is_integer(To) andalso (To > 0) ->
    {ok, Instruction, State};

handle_parse({expect_receive, Desc, Verify} = _Instruction, State) 
  when is_list(Desc) andalso is_function(Verify)  ->
    Instruction = {expect_receive, Desc, {Verify, infinity}},
    {ok, Instruction, State};

handle_parse({expect_receive, Desc, {Verify, To}} = Instruction, State) 
  when is_list(Desc) andalso 
       is_function(Verify) andalso 
       ((is_integer(To) andalso (To >= 0)) orelse (To == infinity))  ->
    {ok, Instruction, State};

handle_parse({send, Desc, Msg} = Instruction, State) 
  when is_list(Desc) andalso (is_tuple(Msg) orelse is_binary(Msg)) ->
    {ok, Instruction, State};

handle_parse({trigger, Desc, Trigger} = Instruction, State) 
  when is_list(Desc) andalso is_function(Trigger) ->
    {ok, Instruction, State};

handle_parse(Instruction, _State) ->
    {error, {unknown_instruction, Instruction}}.


%% ----- instruction exececutor -----

handle_exec({debug, Debug}, 
     State) ->
    p("debug: ~p", [Debug]),
    put(debug, Debug),
    {ok, State};

handle_exec({encode, Encode}, 
     State) ->
    p("encode: ~p", [Encode]),
    {ok, State#state{encode = Encode}};

handle_exec({decode, Decode}, 
     State) ->
    p("Decode: ~p", [Decode]),
    {ok, State#state{decode = Decode}};

handle_exec(disconnect, 
     #state{listen     = Listen, 
	    connection = Sock,
	    result     = Res} = State) ->
    p("disconnect"),
    (catch gen_tcp:close(Sock)),
    (catch gen_tcp:close(Listen)),
    {ok, State#state{listen     = undefined, 
		     connection = undefined, 
		     result     = [disconnected|Res]}};

handle_exec({listen, Port}, #state{result = Res} = State) ->
    p("listen to ~p", [Port]),
    Opts = [binary, 
	    {packet,    tpkt}, 
	    {active,    false}, 
	    {reuseaddr, true}, 
	    {nodelay,   true}],
    case (catch gen_tcp:listen(Port, Opts)) of
        {ok, Listen} ->
            d("listen -> listen socket created"),
            {ok, State#state{listen = Listen, result = [listening | Res]}};
        {error, Reason} ->
            e("failed creating listen socket: ~p", [Reason]),
	    {error, {failed_creating_listen_socket, Reason, Res}}
    end;

handle_exec({expect_accept, {Addr, To}}, 
     #state{listen = Listen, 
	    result = Res} = State) ->
    p("expect_accept from ~p (~p)", [Addr, To]),
    case (catch gen_tcp:accept(Listen, To)) of
	{ok, Sock} ->
            d("expect_accept -> connection accepted"),
            case (catch inet:peername(Sock)) of
                {ok, {Addr, _Port}} ->
                    d("expect_accept -> valid address"),
                    NewState = 
			State#state{connection = Sock, 
				    result     = [{connected, Addr}|Res]},
		    {ok, NewState};
                {ok, {OtherAddr, _Port}} when Addr == any ->
                    d("expect_accept -> valid (~p)", [OtherAddr]),
                    NewState = 
			State#state{connection = Sock, 
				    result     = [{connected, OtherAddr}|Res]},
		    {ok, NewState};
                {ok, AddrAndPort} ->
		    {error, {invalid_connect, AddrAndPort, Res}};
                {error, Reason} ->
                    e("failed getting peername for socket: ~p", [Reason]),
		    (catch gen_tcp:close(Sock)),
                    {error, {failed_getting_peername, Sock, Reason}}
            end;
	{error, Reason} ->
	    e("failed accepting connection: ~p", [Reason]),
            (catch gen_tcp:close(Listen)),
	    {error, {failed_accepting_conection, Reason, Listen}}
    end;

handle_exec({active, NewState}, 
     #state{connection = Sock,
	    result     = Res} = State) ->
    p("active to ~p", [NewState]),
    case inet:setopts(Sock, [{active, NewState}]) of
        ok ->
            d("active -> state changed"),
            {ok, State#state{result = [{active, NewState}|Res]}};
        {error, Reason} ->
            e("failed changing active state to ~w: ~p", [NewState, Reason]),
            {error, {failed_setting_active, Reason}}
    end;

handle_exec({connect, {Addr, Port, To}}, 
     #state{result = Res} = State) ->
    p("connect to ~p, ~p", [Addr, Port]),
    Opts = [binary, {packet, tpkt}, {active, once}, {nodelay, true}],
    case (catch gen_tcp:connect(Addr, Port, Opts, To)) of
        {ok, Sock} ->
            d("connect -> connected"),
            {ok, State#state{connection = Sock, 
			     result     = [{connected, Addr, Port}|Res]}};
        {error, Reason} ->
            e("failed connecting: ~p", [Reason]),
            {error, {failed_connect, Addr, Port, Reason}}
    end;

%% Already encoded
handle_exec({send, Desc, Bin}, 
     #state{connection = Sock,
	    result     = Res} = State)
  when is_binary(Bin) ->
    p("send ~s message", [Desc]),
    NewBin = add_tpkt_header(Bin),
    d("send -> tpkt header added [~w], now send", [sz(NewBin)]),
    case (catch gen_tcp:send(Sock, NewBin)) of
        ok ->
            d("send -> message sent"),
            {ok, State#state{result = [{sent, Desc}|Res]}};
        {error, Reason} ->
            e("send -> send failed: ~n~p",[Reason]),
	    {error, {failed_send, Reason}}
    end;

handle_exec({send, Desc, Msg}, 
     #state{connection = Sock,
	    encode     = Encode,
	    result     = Res} = State) ->
    p("send ~s message", [Desc]),
    case (catch Encode(Msg)) of
        {ok, Bin} ->
            d("send -> message encoded [~w], now add tpkt header: ~n~s",
              [sz(Bin), binary_to_list(Bin)]),
            NewBin = add_tpkt_header(Bin),
            d("send -> tpkt header added [~w], now send", [sz(NewBin)]),
            case (catch gen_tcp:send(Sock, NewBin)) of
                ok ->
                    d("send -> message sent"),
                    {ok, State#state{result = [{sent, Desc}|Res]}};
                {error, Reason} ->
                    e("send -> send failed: ~n~p", [Reason]),
		    {error, {failed_send, Reason}}
            end;
	Error ->
            e("send -> encode failed: ~n~p", [Error]),
	    {error, {encode_failed, Error}}
    end;

handle_exec({expect_receive, Desc, {Verify, To}},
     #state{connection = Sock,
	    decode     = Decode,
	    result     = Acc} = State) ->
    p("expect_receive ~s message", [Desc]),
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, <<3:8, _X:8, Length:16, Msg/binary>>} ->
            d("expect_receive -> received message: Length = ~p", [Length]),
            case (catch Decode(Msg)) of
                {ok, MegaMsg} when is_tuple(MegaMsg) ->
                    d("expect_receive -> decode successfull, now verify"),
                    case (catch Verify(MegaMsg)) of
                        {ok, Res} ->
                            d("expect_receive -> verify successfull"),
                            {ok, State#state{result = [Res|Acc]}};
                        Else ->
                            e("failed to verify message: ~n~p~n~p",
                              [Else, MegaMsg]),
                            {error, {expect_receive, {verify_failed, Else}}}
                    end;
                Error ->
                    e("failed decoding message: ~p", [Error]),
                    {error, {expect_receive, Error}}
            end;
        Else ->
            e("received unknown message: ~p", [Else]),
            {error, {expect_receive, {unexpected_message, Else}}}
    after To ->
            {error, {expect_receive, timeout}}
    end;

handle_exec({expect_closed, To}, 
     #state{connection = Sock,
	    result     = Acc} = State) ->
    p("expect_closed ~w", [To]),
    inet:setopts(Sock, [{active, once}]),
    p("expect_closed - await closed", []),
    receive
        {tcp_closed, Sock} ->
            p("expect_closed - received closed"),
            {ok, State#state{connection = undefined,
			     result     = [closed|Acc]}}
    after To ->
            e("expect_closed timeout after ~w", [To]),
            {error, {expect_closed, timeout}}
    end;

handle_exec({expect_nothing, To}, 
     #state{connection = Sock,
	    result     = Acc} = State) ->
    p("expect_nothing ~w", [To]),
    inet:setopts(Sock, [{active, once}]),
    p("expect_nothing - await anything", []),
    receive
        Any ->
            e("expect_nothing - received: ~p", [Any]),
            {error, {expect_nothing, Any}}
    after To ->
            p("expect_nothing timeout after ~w", [To]),
            {ok, State#state{result = [{nothing, To}|Acc]}}
    end;

handle_exec({trigger, Desc, Trigger}, 
     #state{result = Acc} = State) when is_function(Trigger) ->
    p("trigger: ~s", [Desc]),
    Trigger(),
    {ok, State#state{result = [triggered|Acc]}};

handle_exec({sleep, To}, 
     #state{result = Acc} = State) ->
    p("sleep ~p", [To]),
    sleep(To),
    {ok, State#state{result = [{slept, To}|Acc]}};

handle_exec(Instruction, _State) ->
    {error, {unknown_instruction, Instruction}}.


%% ----- terminate -----

terminate(normal, #state{listen     = Listen,
				connection = Sock,
				result     = Result}) ->
    (catch gen_tcp:close(Sock)),
    (catch gen_tcp:close(Listen)),
    {ok, Result};
terminate(Reason, #state{listen     = Listen,
				connection = Sock,
				result     = Result}) ->
    (catch gen_tcp:close(Sock)),
    (catch gen_tcp:close(Listen)),
    {error, {Reason, Result}}.


%%----------------------------------------------------------------------
%% internal utility functions
%%----------------------------------------------------------------------

error(Reason) ->
    throw({error, Reason}).


%%% ----------------------------------------------------------------

add_tpkt_header(Bin) when is_binary(Bin) ->
    L = size(Bin) + 4,
    SZ1 = ((L) bsr 8) band 16#ff,
    SZ2 = (L) band 16#ff,
    <<3, 0, SZ1, SZ2, Bin/binary>>;
add_tpkt_header(IOList) when is_list(IOList) ->
    add_tpkt_header(list_to_binary(IOList)).

sleep(X) -> megaco_test_generator:sleep(X).

sz(X) -> megaco_test_generator:sz(X).


%%% ----------------------------------------------------------------

d(F)    -> megaco_test_generator:debug(F).
d(F, A) -> megaco_test_generator:debug(F, A).

e(F, A) -> megaco_test_generator:error(F, A).

p(F      ) -> p("", F, []).
p(F,    A) -> p("", F, A).
p(P, F, A) -> megaco_test_generator:print(P,    F, A).


