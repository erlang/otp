-module(e).
-behaviour(gen_server).

%% External exports
-export([start_link/0]).
-export([hello/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, myserver}, myserver, [], []).

hello() ->
    gen_server:call(myserver, hello).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = case Request of
		char ->
		    $B;
		integer ->
		    17;
		float ->
		    32.76;
		string ->
		    "hi there";
		atom ->
		    hello;
		block ->
		    begin
			a,
			b
		    end;
		binary ->
		    <<1, 2, 3>>
	    end,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) when atom(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when binary(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when not is_tuple(Msg), not is_list(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when float(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when function(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when integer(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when list(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when number(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when pid(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when port(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when reference(Msg) ->
    {noreply, State};
handle_cast(Msg, State) when tuple(Msg) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

