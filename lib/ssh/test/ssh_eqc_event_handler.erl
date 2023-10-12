-module(ssh_eqc_event_handler).

-export([
         add_report_handler/0,
         get_reports/1,
         code_change/3,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         init/1,
         terminate/2
        ]).


-behaviour(gen_event).

add_report_handler() ->
    error_logger:add_report_handler(?MODULE, [self(),Ref=make_ref()]),
    receive
	{event_handler_started,HandlerPid,Ref} ->
	    {ok,HandlerPid}
    end.

get_reports(Pid) ->
    Pid ! {get_reports,self(),Ref=make_ref()},
    receive
	{reports,Reports,Ref} ->
	    {ok,Reports}
    end.

%%%================================================================

-record(state, {
	  reports = []
	 }).

%% error_logger:add_report_handler(ssh_eqc_event_handler, [self()]).

init([CallerPid,Ref]) -> 
    CallerPid ! {event_handler_started,self(),Ref},
    {ok, #state{}}.

handle_event(Event, State) ->
    {ok, State#state{reports = [Event|State#state.reports]}}.

handle_info({get_reports,From,Ref}, State) -> 
    From ! {reports, lists:reverse(State#state.reports), Ref},
    {ok, State#state{reports=[]}}.

handle_call(_Request, State) -> {ok,reply,State}.
terminate(_Arg, _State) -> stop.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
