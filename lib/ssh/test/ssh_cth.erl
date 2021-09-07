-module(ssh_cth).

-export([id/1,
         init/2,
         pre_init_per_suite/3,
         pre_end_per_suite/3,
         pre_init_per_group/4,
         post_init_per_group/5,
         pre_end_per_group/4,
         post_end_per_group/5,
         pre_init_per_testcase/4,
         post_init_per_testcase/5,
         pre_end_per_testcase/4,
         post_end_per_testcase/5
        ]).

-record(c, {
          known_hosts_file_name = filename:join(os:getenv("HOME"), ".ssh/known_hosts"),
          known_hosts_last_contents = <<>>,
          suite, % suite name
          groups = [], % Group path in reversed order
          n % test case number
         }).

id(Opts) ->
    proplists:get_value(filename, Opts, "/tmp/file.log").

init(_Id, _Opts) ->
    {ok, #c{n=1}}.

pre_init_per_suite(Suite, Config, State0) ->
    {_, State} = read_known_hosts_diff(State0#c{suite=Suite}),
    {Config, State}.

pre_end_per_suite(Suite, Config, State) ->
    ct:pal("BEGIN ~p:end_per_suite(...)", [Suite]),
    {Config, State}.


pre_init_per_group(Suite, Group, Config, State0) ->
    {Diff, State} = read_known_hosts_diff(State0),
    ct:pal("~sBEGIN ~p:init_per_group(~p,...)", [log_diff(Diff),Suite,Group]),
    {Config, State#c{groups = (State#c.groups ++ [Group])}}.

post_init_per_group(Suite, Group, _Config, Return, State0) ->
    {Diff, State} = read_known_hosts_diff(State0),
    ct:pal("~sEND ~p:init_per_group(~p,...)", [log_diff(Diff),Suite,Group]),
    {Return, State}.

pre_end_per_group(Suite, Group, Config, State0) ->
    {Diff, State} = read_known_hosts_diff(State0),
    ct:pal("~sBEGIN ~p:end_per_group(~p,...)", [log_diff(Diff), Suite, Group]),
    {Config, State}.

post_end_per_group(Suite, Group, _Config, Return, State0) ->
    {Diff, State} = read_known_hosts_diff(State0),
    ct:pal("~sEND ~p:end_per_group(~p,...)", [log_diff(Diff),Suite,Group]),
    {Return, State#c{groups = lists:reverse(lists:reverse(State#c.groups)--[Group])}}.


pre_init_per_testcase(SuiteName, _TC, Config, State0) ->
    {Diff, State} = read_known_hosts_diff(State0),
    ct:pal("~s########## ~p ~p ~s ~p", [log_diff(Diff), State0#c.suite, State0#c.n, groups(Config), SuiteName]),
    {Config, State#c{n = State#c.n + 1}}.

post_init_per_testcase(SuiteName, TestcaseName, _Config, Return, State0) ->
    {Diff, State} = read_known_hosts_diff(State0),
    ct:pal("~send ~p:init_per_testcase(~p,...)", [log_diff(Diff), SuiteName, TestcaseName]),
    {Return, State}.

pre_end_per_testcase(Suite, TC, Config, State0) ->
    {Diff, State} = read_known_hosts_diff(State0),
    ct:pal("~sBEGIN ~p:end_per_testcase(~p,...)", [log_diff(Diff), Suite, TC]),
    {Config, State}.

post_end_per_testcase(SuiteName, TC, _Config, Return, State0) ->
    {Diff, State} = read_known_hosts_diff(State0),
    ct:pal("~sEND ~p:end_per_testcase(~p,...)", [log_diff(Diff), SuiteName, TC]),
    {Return, State}.




groups(Config) ->
    F = fun(X) -> io_lib:format("~w",[X]) end,
    io_lib:format("~s", [lists:join("/", lists:map(F,get_groups(Config)))]).

get_groups(Config) ->
    P = proplists:get_value(tc_group_path, Config, []) ++
        [proplists:get_value(tc_group_properties, Config, [])],
    [Name || L <- P,
             is_list(L),
             {name,Name} <- L].
    


read_known_hosts_diff(S = #c{known_hosts_file_name = File,
                             known_hosts_last_contents = Bin0}) ->
    {ok,  <<Bin0:(size(Bin0))/binary, Diff/binary>> = Bin} = file:read_file(File),
    {Diff, S#c{known_hosts_last_contents = Bin}}.

log_diff(<<>>) -> "";
log_diff(Bin) -> io_lib:format("~n++++ ~p~n",[Bin]).
    
