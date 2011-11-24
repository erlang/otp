-module(dummy_server).
-behaviour(gen_server).

-export([start_link/0, set_state/1, get_state/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_state(What) ->
    gen_server:call(?MODULE, {set_state, What}).

get_state() ->
    gen_server:call(?MODULE, get_state).


%%

init([]) ->
    say("init, setting state to 0", []),
    {ok, 0}.


handle_call({set_state, NewState}, _From, _State) ->
    {reply, {ok, NewState}, NewState};

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast('__not_implemented', State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    say("info ~p, ~p.", [_Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    say("terminate ~p, ~p", [_Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
    {ok, State}.

%% Internal

say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
