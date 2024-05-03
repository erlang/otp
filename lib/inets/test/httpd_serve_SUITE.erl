%% Tests for the `erl -S httpd serve` functionality.
-module(httpd_serve_SUITE).
-export([suite/0, all/0, groups/0]).
-export([
    argless_start/1,
    argless_serve/1,
    simple_random_port_serve/1,
    serve_on_all_interfaces_v4/1,
    serve_on_localhost_v4/1,
    serve_on_all_interfaces_v6/1,
    serve_on_localhost_v6/1,
    serve_custom_directory/1
]).

%% When starting up servers for tests, these variables define how long to
%% wait for the server to report that it has started up, and after how
%% many retries to quit waiting for its report altogether.
-define(STARTUP_WAIT_NAPTIME_MS, 20).
-define(STARTUP_WAIT_RETRIES, 100).

%% Default assertions to run in all tests.
-define(DEFAULT_ASSERTIONS, [directory_index, random_file]).

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {seconds, 30}}].

all() ->
    [{group, httpd_serve_on_default_port},
     {group, httpd_serve_on_random_ports}].

groups() ->
    [{httpd_serve_on_default_port, [sequence], [
        argless_serve,
        argless_start]},
     {httpd_serve_on_random_ports, [parallel], [
        simple_random_port_serve,
        serve_on_all_interfaces_v4,
        serve_on_localhost_v4,
        serve_on_all_interfaces_v6,
        serve_on_localhost_v6,
        serve_custom_directory
      ]}].

%%
%% Test cases
%%

%% Fixed ports (must be run one at a time)

argless_start(_Config) ->
    ServerFun = fun () -> httpd:start([]) end,
    verify_server(ServerFun).

argless_serve(_Config) ->
    ServerFun = fun () -> httpd:serve([]) end,
    verify_server(ServerFun).

%% Random ports (can run in parallel)

simple_random_port_serve(Config) ->
    verify_server(["--port", "0", suite_data(Config)]).

serve_on_all_interfaces_v4(Config) ->
    verify_server(["--port", "0", "--bind", "0.0.0.0", suite_data(Config)]).

serve_on_localhost_v4(Config) ->
    verify_server(["--port", "0", "--bind", "127.0.0.1", suite_data(Config)]).

serve_on_all_interfaces_v6(Config) ->
    verify_server(["--port", "0", "--bind", "::", suite_data(Config)]).

serve_on_localhost_v6(Config) ->
    verify_server(["--port", "0", "--bind", "::1", suite_data(Config)]).

serve_custom_directory(Config) ->
    SuiteData = suite_data(Config),
    verify_server(["--port", "0", filename:join(SuiteData, "subdir")]).

%%
%% Assertion functions
%%

%% Assert that the server responds properly.
run_server_assertions(Response) ->
    run_server_assertions(Response, ?DEFAULT_ASSERTIONS).

%% Assert that the server responds properly.
run_server_assertions({ok, {Ip0, Port, Path}}, Assertions) when is_integer(Port) ->
    Ip = maybe_convert_to_localhost(Ip0),
    % From the `filelib:wildcard/1` docs:
    % "Directory separators must always be written as /, even on Windows."
    IpToRequest = case Ip of
        {_, _, _, _} -> inet:ntoa(Ip);
        {_, _, _, _, _, _, _, _} -> "[" ++ inet:ntoa(Ip) ++ "]"
    end,

    ct:log("Validating custom assertions"),
    DirectoryUrl = "http://" ++ IpToRequest ++ ":" ++ integer_to_list(Port) ++ "/",
    ServerInfo = #{
        url => DirectoryUrl,
        bind_ip => Ip,
        path => Path
    },
    ok = verify_assertions(Assertions, ServerInfo),
    ct:comment("Ran ~w assertion(s).", [length(Assertions)]).

maybe_convert_to_localhost(Ip) ->
    case os:type() of
        {unix, linux} -> Ip;
        _Other -> convert_to_localhost(Ip)
    end.

convert_to_localhost({0, 0, 0, 0}) ->
    {127, 0, 0, 1};
convert_to_localhost({0, 0, 0, 0, 0, 0, 0, 0}) ->
    {0, 0, 0, 0, 0, 0, 0, 1};
convert_to_localhost(Ip) ->
    Ip.

%%
%% Assertion helper functions
%%

verify_200_at(Url) ->
    HttpcOpts = [{socket_opts, [{ipfamily, inet6fb4}]}],
    Request = {Url, []},
    Response = httpc:request(get, Request, [], HttpcOpts),
    case Response of
        {ok, {{_Version, 200, _}, _Headers, _Body}} ->
            Response;
        {error, {failed_connect, [{to_address, {"::", _Port}},
                                  {inet6, [inet6], eaddrnotavail},
                                  {inet, [inet], nxdomain}]}} ->
            % In this case we could bind on all IPv6 interfaces (::) just
            % fine, but could not issue requests to it due to an OS error.
            % Write it off as a networking misconfiguration and skip.
            exit({skip, "Could not reach host on IPv6 address ::"})
    end.

verify_assertions([], _ServerInfo) ->
    ok;

verify_assertions([directory_index | Assertions], #{url := Url} = ServerInfo) ->
    ct:log("Validating directory index at ~s", [Url]),
    verify_200_at(Url),
    ct:log("Directory index received with a 200"),
    verify_assertions(Assertions, ServerInfo);

verify_assertions([random_file | Assertions],  #{url := Url, path := Path} = ServerInfo) ->
    Files = filelib:wildcard(Path ++ "/*"),
    File = lists:nth(rand:uniform(length(Files)), Files),
    Basename = filename:basename(File),
    FileUrl = Url ++ Basename,
    ct:log("Validating random file at ~s", [FileUrl]),
    verify_200_at(FileUrl),
    ct:log("File received with a 200"),
    verify_assertions(Assertions, ServerInfo).

%%
%% Helper functions
%%

suite_data(Config) ->
    proplists:get_value(data_dir, Config).

verify_server(FunOrArgs) ->
    TestFun = fun run_server_assertions/1,
    with_server(FunOrArgs, TestFun).

with_server(Args, TestFun) when is_list(Args) ->
    ServerFun = fun () -> httpd:serve(Args) end,
    run_with_server(ServerFun, TestFun);

with_server(ServerFun, TestFun) when is_function(ServerFun) ->
    run_with_server(ServerFun, TestFun).

run_with_server(ServerFun, TestFun) ->
    ct:log("Starting server"),
    ct:capture_start(),
    {Child, _Reference} = spawn_monitor(ServerFun),
    StartupResult = wait_for_startup_line(?STARTUP_WAIT_RETRIES),
    ct:capture_stop(),
    {ok, Line} = StartupResult,
    Parsed = parse_startup_line(Line),
    ct:log("Running test function"),
    Result = TestFun(Parsed),
    ct:log("Test function finished, shutting down server"),
    Child ! {self(), shutdown},
    receive done -> ok after 5000 -> ct:fail("No server shutdown after 5s") end,
    ct:log("Server stopped"),
    Result.

%% Wait for `ct:capture_get' to give us the output we're looking for.
wait_for_startup_line(Tries) ->
    wait_for_startup_line([], [], Tries).

wait_for_startup_line([], [], 0) ->
    {error, no_output_at_all};
wait_for_startup_line([], Unexpected, 0) ->
    {error, {no_startup_line, unexpected_output, Unexpected}};
wait_for_startup_line([], Unexpected, Tries) when Tries > 0 ->
    receive
        {'DOWN', _Reference, process, _Child, Info} ->
            case Info of
                {{badmatch, {error, {listen, eaddrnotavail}}}, _} ->
                    exit({skip, "Adress not available to listen"});
                _ ->
                    ct:fail("Child process has died: ~w", [Info])
            end
    after 
        0 -> ok
    end,
    timer:sleep(?STARTUP_WAIT_NAPTIME_MS),
    wait_for_startup_line(ct:capture_get(), Unexpected, Tries - 1);
wait_for_startup_line(["Started HTTP" ++ _Rest = Line | _Lines], _Unexpected, _Tries) ->
    {ok, Line};
wait_for_startup_line([Line | Lines], Unexpected, Tries) ->
    wait_for_startup_line(Lines, [Line | Unexpected], Tries).

%% Parse the given line into a tuple.
%% Example line:
%%   Started HTTP server on http://127.0.0.1:8000 at /path/to/lib/inets/make_test_dir/ct_logs/ct_run.test_server@zulu.2023-06-06_12.07.27\n"
parse_startup_line(Line) ->
    {match, [_, RawIp, RawPort, Path]} = re:run(
        Line, "^Started HTTP server on http://(.+):(\\d+) at (.*)\\n$", [{capture, all, list}]
    ),
    {ok, Ip} = inet:parse_address(RawIp),
    Port = list_to_integer(RawPort),
    {ok, {Ip, Port, Path}}.
