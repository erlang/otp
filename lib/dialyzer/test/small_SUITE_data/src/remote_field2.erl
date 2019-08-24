-module(remote_field2).

-export([handle_cast/2]).

-record(state, {tcp_socket :: inet:socket()}).

-spec handle_cast(_,_) ->
                         {noreply,_} |
                         {stop,{shutdown,connection_closed},
                          #state{tcp_socket :: port()}}.
handle_cast({send, Message}, #state{tcp_socket = TCPSocket} = State) ->
    case gen_tcp:send(TCPSocket, Message) of
        ok ->
            {noreply, State};
        {error, closed} ->
            {stop, {shutdown, connection_closed}, State}
    end.
