-module(slow).
-behaviour(application).
-behaviour(supervisor).
-behaviour(gen_server).
-compile(export_all).


%%%-----------------------------------------------------------------
%%% application callbacks
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, supervisor).

stop(_State) ->
    ok.

init(supervisor) ->
    Child = #{id => main, start => {gen_server, start_link, [?MODULE, server, []]}},
    {ok, {#{}, [Child]}};
init(server) ->
    {ok, Controller} = application:get_env(slow, controller),
    process_flag(trap_exit, true),
    Controller ! {server_starting, self()},
    receive
        continue -> ok
    end,
    {ok, #{controller => Controller}}.

terminate(_Reason, #{controller := Controller}) ->
    case application:get_env(slow, terminate) of
        {ok, Fun} -> Fun();
        _ -> ok
    end,
    Controller ! server_terminating,
    ok.
