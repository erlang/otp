-module(deadlock).
-behaviour(application).
-compile(export_all).
-define(SUP,deadlock_sup).
-define(CHILD,deadlock_child).


%%%-----------------------------------------------------------------
%%% application callbacks
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?SUP}, ?MODULE, [sup]).

stop(_State) ->
    ok.



%%%-----------------------------------------------------------------
%%% supervisor callbacks
init([sup]) ->
    {ok, {{one_for_one, 5, 10}, [
        {
            sasl_syslog_dm, {?MODULE, start_link, []},
            permanent, 25000, worker,
            [deadlock]
        }
    ]}};


%%%-----------------------------------------------------------------
%%% gen_server callbacks
init([child]) ->
    case application:get_env(deadlock, fail_start) of
        {ok, false} ->
	    process_flag(trap_exit, true),
	    io:format("~p: Traps exit~n",[?MODULE]),
            %% we must not fail on the first init, otherwise supervisor
            %% terminates immediately
            {ok, []};
        {ok, true}  ->
	    timer:sleep(infinity), % init hangs!!!!
	    {ok, []}
    end.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(restart, State) ->
    {stop, error, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    case application:get_env(deadlock, fail_stop) of
        {ok, false} -> ok;
        {ok, Tester}  ->
	    Tester ! {deadlock, self()},
	    io:format("~p: Waiting in terminate (~p)~n",[?MODULE,Tester]),
	    receive continue -> ok end
    end,
    io:format("~p: terminates~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%-----------------------------------------------------------------
%%% Start child
start_link() ->
    gen_server:start_link({local, ?CHILD}, ?MODULE, [child], []).


%%%-----------------------------------------------------------------
%%% Provoke hanging
restart_and_fail() ->
    application:set_env(deadlock, fail_start, true), % next init will hang
    gen_server:cast(?CHILD, restart).
