-module(ssh_cth).

-export([id/1,
         init/2,
         pre_init_per_suite/3,
         pre_end_per_suite/3,
         pre_init_per_group/4,
         pre_end_per_group/4,
         post_end_per_group/5,
         pre_init_per_testcase/3,
         pre_end_per_testcase/4
        ]).

-record(c, {
          suite, % suite name
          groups = [], % Group path in reversed order
          n % test case number
         }).

id(Opts) ->
    proplists:get_value(filename, Opts, "/tmp/file.log").

init(_Id, _Opts) ->
    {ok, #c{n=1}}.

pre_init_per_suite(Suite, Config, CTHState) ->
    {Config, CTHState#c{suite=Suite}}.

pre_end_per_suite(Suite, Config, State) ->
    ct:pal("BEGIN ~p:end_per_suite(...)", [Suite]),
     {Config, State}.


pre_init_per_group(_Suite, Group, Config ,State) ->
     {Config, State#c{groups = (State#c.groups ++ [Group])}}.

pre_end_per_group(Suite, Group, Config, State) ->
    ct:pal("BEGIN ~p:end_per_group(~p,...)", [Suite,Group]),
    {Config, State}.

post_end_per_group(_Suite, Group, _Config, Return, State) ->
     {Return, State#c{groups = lists:reverse(lists:reverse(State#c.groups)--[Group])}}.


pre_init_per_testcase(SuiteName, Config, CTHState) ->
    ct:pal("########## ~p ~p ~s ~p~n", [CTHState#c.suite, CTHState#c.n, groups(Config), SuiteName]),
    {Config, CTHState#c{n = CTHState#c.n + 1}}.

pre_end_per_testcase(Suite,TC,Config,State) ->
    ct:pal("BEGIN ~p:end_per_testcase(~p,...)", [Suite,TC]),
    {Config, State}.


groups(Config) ->
    F = fun(X) -> io_lib:format("~w",[X]) end,
    io_lib:format("~s", [lists:join("/", lists:map(F,get_groups(Config)))]).

get_groups(Config) ->
    P = proplists:get_value(tc_group_path, Config, []) ++
        [proplists:get_value(tc_group_properties, Config, [])],
    [Name || L <- P,
             is_list(L),
             {name,Name} <- L].
    
