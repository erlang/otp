-module(ssh_subsystem).

%% API to special server side channel that can be pluged into the erlang ssh daemeon
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-callback handle_msg(Msg ::term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_ssh_msg({ssh_cm, ConnectionRef::term(), SshMsg::term()},
			 State::term()) -> {ok, State::term()} |
					   {stop, ChannelId::integer(),
					    State::term()}.

%%% API
-export([start/4, start/5, start_link/4, start_link/5, enter_loop/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).

start(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    ssh_channel:start(ConnectionManager, ChannelId, CallBack, CbInitArgs, undefined).

start(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    ssh_channel:start(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    ssh_channel:start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, undefined).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    ssh_channel:start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec).

enter_loop(State) ->
    ssh_channel:enter_loop(State).

init(Args) ->
    ssh_channel:init(Args).
terminate(Reason, State) ->
    ssh_channel:terminate(Reason, State).
