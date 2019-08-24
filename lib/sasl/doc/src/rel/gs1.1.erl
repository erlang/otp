-module(gs1).
-vsn(1).
-behaviour(gen_server).

-export([get_data/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {data}).

get_data() -> 
    gen_server:call(gs1, get_data).

init([Data]) ->
    {ok, #state{data = Data}}.

handle_call(get_data, _From, State) ->
    {reply, {ok, State#state.data}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
