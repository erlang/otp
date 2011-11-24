%% Dialyzer couldn't infer that monitor_diskspace would go in an infinite loop
%% instead of crashing due to the existence of list comprehensions that have a
%% normal success typing. These were added to the monitor_diskspace's SCC and
%% dialyzer_typesig didn't try to assign unit() to monitor_diskspace, as it
%% required all the members of the SCC to return none().
%%
%% Testcase was submitted in erlang-questions mailing list by Prashanth Mundkur
%% (http://erlang.org/pipermail/erlang-questions/2011-May/058063.html)

-module(no_return_bug).
-export([diskspace/1, monitor_diskspace/2, refresh_tags/1, monitor_launch/0]).

-type diskinfo() :: {non_neg_integer(), non_neg_integer()}.

-spec diskspace(nonempty_string()) -> {'ok', diskinfo()} | {'error', term()}.
diskspace(Path) ->
    case Path of
        "a" -> {ok, {0,0}};
        _ -> {error, error}
    end.

-spec monitor_diskspace(nonempty_string(),
                        [{diskinfo(), nonempty_string()}]) ->
                               no_return().
monitor_diskspace(Root, Vols) ->
    Df = fun(VolName) ->
                 diskspace(filename:join([Root, VolName]))
         end,
    NewVols = [{Space, VolName}
               || {VolName, {ok, Space}}
                      <- [{VolName, Df(VolName)}
                          || {_OldSpace, VolName} <- Vols]],
    monitor_diskspace(Root, NewVols).

-spec refresh_tags(nonempty_string()) -> no_return().
refresh_tags(Root) ->
    {ok, _} = diskspace(Root),
    refresh_tags(Root).

monitor_launch() ->
    spawn_link(fun() -> refresh_tags("abc") end),
    spawn_link(fun() -> monitor_diskspace("root", [{{0,0}, "a"}]) end).
