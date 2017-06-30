%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(etop_tr).
-author('siri@erix.ericsson.se').

%%-compile(export_all).
-export([setup_tracer/1,stop_tracer/1,reader/1]).
-import(etop,[getopt/2]).

-include("etop_defs.hrl").

setup_tracer(Config) ->
    TraceNode = getopt(node,Config),
    RHost = rpc:call(TraceNode, net_adm, localhost, []),
    Store  = ets:new(?MODULE, [set, public]),

    %% We can only trace one process anyway kill the old one.
    case erlang:whereis(dbg) of
	undefined -> 
	    case rpc:call(TraceNode, erlang, whereis, [dbg]) of
		undefined -> fine;
		Pid ->
		    exit(Pid, kill)
	    end;
	Pid ->
	    exit(Pid,kill)
    end,

    dbg:tracer(TraceNode,port,dbg:trace_port(ip,{getopt(port,Config),5000})),
    dbg:p(all,[running,timestamp]),
    T = dbg:get_tracer(TraceNode),
    Config#opts{tracer=T,host=RHost,store=Store}.

stop_tracer(_Config) ->
    dbg:p(all,clear),
    dbg:stop(),
    ok.
    


reader(Config) ->
    Host = getopt(host, Config),
    Port = getopt(port, Config),    
    
    {ok, Sock} = gen_tcp:connect(Host, Port, [{active, false}]),
    spawn_link(fun() -> reader_init(Sock,getopt(store,Config),[]) end).


%%%%%%%%%%%%%%   Socket reader %%%%%%%%%%%%%%%%%%%%%%%%%%%

reader_init(Sock, Store, Last) ->
    process_flag(priority, high),
    reader(Sock, Store, Last).

reader(Sock, Store, Last) ->
    Data = get_data(Sock),
    New = handle_data(Last, Data, Store),
    reader(Sock, Store, New).

handle_data(Last, {_, Pid, in, _, Time}, _) ->
    [{Pid,Time}|Last];
handle_data([], {_, _, out, _, _}, _Store) ->
    %% ignore - there was probably just a 'drop'
    [];
handle_data(Last, {_, Pid, out, _, Time2} = G, Store) ->
    case lists:keytake(Pid, 1, Last) of
         {_, {_, Time1}, New} ->
             Elapsed = elapsed(Time1, Time2),
             case ets:member(Store,Pid) of
                  true -> ets:update_counter(Store, Pid, Elapsed);
                  false -> ets:insert(Store,{Pid,Elapsed})
             end,
             New;
         false ->
             io:format("Erlang top got garbage ~tp~n", [G]),
             Last
    end;
handle_data(_W, {drop, D}, _) ->  %% Error case we are missing data here!
    io:format("Erlang top dropped data ~p~n", [D]),
    [];
handle_data(Last, G, _) ->
    io:format("Erlang top got garbage ~tp~n", [G]),
    Last.

elapsed({Me1, S1, Mi1}, {Me2, S2, Mi2}) ->
    Me = (Me2 - Me1) * 1000000,
    S  = (S2 - S1 + Me) * 1000000,
    Mi2 - Mi1 + S.


%%%%%% Socket helpers %%%%
get_data(Sock) ->
    [Op | BESiz] = my_ip_read(Sock, 5),
    Siz = get_be(BESiz),
    case Op of
	0 ->
	    B = list_to_binary(my_ip_read(Sock, Siz)),
	    binary_to_term(B);
	1 ->
	    {drop, Siz};
	Else ->
	    exit({'bad trace tag', Else})
    end.
           
get_be([A,B,C,D]) ->
    A * 16777216 + B * 65536 + C * 256 + D.

my_ip_read(Sock,N) ->
    case gen_tcp:recv(Sock, N) of
        {ok, Data} ->
	    case length(Data) of
		N ->
		    Data;
		X ->
		    Data ++ my_ip_read(Sock, N - X)
	    end;
	_Else ->
	    exit(eof)
    end.

