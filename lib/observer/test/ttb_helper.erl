-module(ttb_helper). %%Nodes control
-compile(export_all).

%%API
%%get() -> client:get()
%%put(X) -> client:put(X)
%%msgs(N) -> N times client:put(test_msg)
%%clear() -> restart server
%%ensure_running() / stop() -> start/stop nodes
%%get_node(atom) -> return atom@hostname

-define(NODE_CMD(Name),
	"erl -sname " ++ atom_to_list(Name) ++
	" -pa .. -pa . -detached -run ttb_helper send_ok").
-define(REG_NAME, nc_testing).

new_fun() ->
    fun(_, end_of_trace, _, Dict) -> io:format("~p~n", [dict:to_list(Dict)]);
       (_, T, _, Dict) -> case element(2, T) of
                              {Pid, _, _} ->
                                  dict:update_counter(Pid, 1, Dict);
                              Pid ->
                                  dict:update_counter(Pid, 1, Dict)
                          end
    end.

new_fun_2() ->
    fun(_, end_of_trace, _, Dict) -> io:format("~p~n", [dict:to_list(Dict)]);
       (_, T, _, Dict) ->  case element(2, T) of
                               {_, Name, _} when is_atom(Name)->
                                  dict:update_counter(Name, 1, Dict);
                              Pid ->
                                  dict:update_counter(Pid, 1, Dict)
                          end

    end.


ensure_running() ->
    try_start_node(server),
    try_start_node(client),
    clear().

try_start_node(Node) ->
    global:unregister_name(?REG_NAME),
    global:register_name(?REG_NAME, self()),
    global:sync(),
    N = get_node(Node),
    case net_adm:ping(N) of
	pong ->
	    io:format("Node ~p already running~n", [N]);
	_ ->
	    io:format("Starting node ~p... ~p ", [Node, os:cmd(?NODE_CMD(Node))]),
	    recv()
    end.

clear() ->
    s(server, stop, []),
    init().

stop() ->
    s(init, stop, []),
    c(init, stop, []).

msgs(N) ->
    [c(client, put, [test_msg]) || _ <- lists:seq(1, N)],
    s(server, received, [a,b]),
    [dbg:flush_trace_port(Node) || Node <- [get_node(client), get_node(server)]].

msgs_ip(N) ->
    [c(client, put, [test_msg]) || _ <- lists:seq(1, N)],
    s(server, received, [a,b]),
    timer:sleep(500). %% allow trace messages to arrive over tcp/ip

run() ->
    ttb({local, "A"}),
    msgs(2),
    c(erlang, whereis, [ttbt]).

get() -> c(client, get, []).
put(Thing) -> c(client, put, [Thing]).

get_node(Node) ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(atom_to_list(Node) ++ "@" ++ Host).

trace_setup() ->
    ttb:p(all, call),
    ttb:tp(server, received, []),
    ttb:tp(client, put, []),
    ttb:tp(client, get, []).

ttb() -> ttb("A").
ttb(File) ->
    ttb:tracer([get_node(client), get_node(server)], [{file, File}, resume]),
    ttb:p(all, [call, timestamp]),
    ttb:tp(client, put, []),
    ttb:tp(client, get, []),
    ttb:tp(server, received, []).

tc() ->
    TC = example_config_gen:create_trace_case("dummy comment"),
    Patterns = example_config_gen:create_pattern(client, put, 1, return),
    Flags = example_config_gen:create_flags(all, call),
    Merge = example_config_gen:create_merge_conf(show_handler(), "dummy merge comment"),
    Merge2 = example_config_gen:create_merge_conf(undefined, "dummy merge comment"),
    TC2 = example_config_gen:add_pattern(Patterns, TC),
    TC3 = example_config_gen:add_flags(Flags, TC2),
    TC4 = example_config_gen:add_merge_conf(Merge, TC3),
    TC5 = example_config_gen:add_merge_conf(Merge2, TC4),
    example_config_gen:add_nodes([get_node(client), get_node(server)], TC5).


show(X) ->
    io:format(user, "Showing: ~p~n", [X]).

state_handler() ->
    {fun(_,_,I,S) -> io:format(user, "Got from ~p: ~p~n", [I,S]), S+1 end, 0}.

show_handler() ->
    {fun(A,B,_,_) -> io:format(A, "~p~n", [B]) end, []}.

opts() ->
    [[get_node(client), get_node(server)],
     [{server, received, '_', []},
      {client, put, '_', []},
      {client, get, '_', []}],
     {all, call},
     [{file, "TEST"}]].

overload_check(check) ->
    true;
overload_check(_) ->
    ok.
%%%Internal
s(M, F, A) -> rpc:call(get_node(server), M, F, A).
c(M, F, A) -> rpc:call(get_node(client), M, F, A).

send_ok() ->
    pong = net_adm:ping(get_node(test)),
    global:sync(),
    global:send(?REG_NAME, node()).

init() ->
    True = s(server, start, []),
    io:format("ok1: ~p~n", [True]),
    true = c(client, init, [get_node(server)]).

recv() ->
    receive
	Node ->
	    io:format("Node ~p ready.~n", [Node]),
            ok
    after 5000 ->
	    io:format("Startup failed~n",[]),
	    throw(startup_failed)
    end.
