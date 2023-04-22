-module(gen_server_not_exported).

-behaviour(gen_server).

-export([main/0, handle_call/3]).

%% Not exported. Should be a warning.
main() ->
    _ = init(whatever),
    _ = handle_cast(whatever, some_state),
    _ = format_status(<<"xyz">>),
    ok.

init(_) ->
    ok.

%% OK. No warning.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% Not exported. Should be a warning.
-spec handle_cast(any(), any()) -> binary().
handle_cast(_Request, _State) ->
    <<"abc">>.

%% Not exported and conflicting arguments and return value. No warning
%% since format_status/1 is an optional callback.
-spec format_status(binary()) -> binary().
format_status(Bin) when is_binary(Bin) ->
    Bin.
