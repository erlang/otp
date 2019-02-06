-module(supervisor_deadlock).
-compile([export_all,nowarn_export_all]).


%%%-----------------------------------------------------------------
%%% gen_server callbacks
init([child]) ->
    case ets:lookup(supervisor_deadlock,fail_start) of
        [{fail_start, false}] ->
            %% we must not fail on the first init, otherwise supervisor
            %% terminates immediately
            {ok, []};
        [{fail_start, true}] ->
	    %% A restart frequency of MaxR=8, MaxT=10 should ensure
	    %% that restart intensity is not reached -> restart loop.
	    %% (Note that if we use simple_one_for_one, and start
	    %% 'many' child instances, the restart frequency must be
	    %% ajusted accordingly.)
            timer:sleep(2000), % NOTE: this could be a gen_server call timeout

            {stop, error}
     end.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%% Force a restart
handle_cast(restart, State) ->
    {stop, error, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%-----------------------------------------------------------------
%%% Start child
start_child() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [child], []).

start_child_noreg() ->
    gen_server:start_link(?MODULE, [child], []).

restart_child() ->
    gen_server:cast(supervisor_deadlock, restart).

restart_child(Pid) ->
    gen_server:cast(Pid, restart).
