-module(ct_telnet_own_server_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


suite() -> [{require,erl_telnet_server,{unix,[telnet]}}].

all() ->
    [expect,
     expect_repeat].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

expect(_) ->
    {ok, Handle} = ct_telnet:open(erl_telnet_server),
    ok = ct_telnet:send(Handle, "echo ayt"),
    {ok,["ayt"]} = ct_telnet:expect(Handle, ["ayt"]),
    ok = ct_telnet:close(Handle),
    ok.

expect_repeat(_) ->
    {ok, Handle} = ct_telnet:open(erl_telnet_server),
    ok = ct_telnet:send(Handle, "repeat xy"),
    {ok,[["xy"],["xy"]],done} = ct_telnet:expect(Handle, ["xy"],
						 [{repeat,2}]),
    ok = ct_telnet:close(Handle),
    ok.
