-module(supervisor_deadlock).
-compile(export_all).


%%%-----------------------------------------------------------------
%%% gen_server callbacks
init([child]) ->
    case ets:lookup(supervisor_deadlock,fail_start) of
        [{fail_start, false}] ->
            %% we must not fail on the first init, otherwise supervisor
            %% terminates immediately
            {ok, []};
        [{fail_start, true}] ->
	    %% Restart frequency is MaxR=8, MaxT=10, so this will
	    %% ensure that restart intensity is not reached -> restart
	    %% loop
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

restart_child() ->
    gen_server:cast(supervisor_deadlock, restart).
