%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%%----------------------------------------------------------------
%%% Kernel application test suite.
%%%-----------------------------------------------------------------
-module(kernel_SUITE).
-include_lib("test_server/include/test_server.hrl").


% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

% Test cases must be exported.
-export([app_test/1, appup_test/1]).

%%
%% all/1
%%
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app_test, appup_test].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%
% Test cases starts here.
%
app_test(doc) ->
    ["Tests the applications consistency."];
app_test(suite) ->
    [];
app_test(Config) when is_list(Config) ->
    ?line ok=?t:app_test(kernel),
    ok.


%% Test that appup allows upgrade from/downgrade to a maximum of two
%% major releases back.
appup_test(_Config) ->
    application:load(kernel),
    {_,_,Vsn} = lists:keyfind(kernel,1,application:loaded_applications()),
    AppupFile = filename:join([code:lib_dir(kernel),ebin,"kernel.appup"]),
    {ok,[{Vsn,UpFrom,DownTo}=AppupScript]} = file:consult(AppupFile),
    ct:log("~p~n",[AppupScript]),
    {OkVsns,NokVsns} = create_test_vsns(Vsn),
    check_appup(OkVsns,UpFrom,{ok,[restart_new_emulator]}),
    check_appup(OkVsns,DownTo,{ok,[restart_new_emulator]}),
    check_appup(NokVsns,UpFrom,error),
    check_appup(NokVsns,DownTo,error),
    ok.

create_test_vsns(Current) ->
    [XStr,YStr|Rest] = string:tokens(Current,"."),
    X = list_to_integer(XStr),
    Y = list_to_integer(YStr),
    SecondMajor = vsn(X,Y-2),
    SecondMinor = SecondMajor ++ ".1.3",
    FirstMajor = vsn(X,Y-1),
    FirstMinor = FirstMajor ++ ".57",
    ThisMajor = vsn(X,Y),
    This =
	case Rest of
	    [] ->
		[];
	    ["1"] ->
		[ThisMajor];
	    _ ->
		ThisMinor = ThisMajor ++ ".1",
		[ThisMajor,ThisMinor]
	end,
    OkVsns = This ++ [FirstMajor, FirstMinor, SecondMajor, SecondMinor],

    ThirdMajor = vsn(X,Y-3),
    ThirdMinor = ThirdMajor ++ ".10.12",
    Illegal = ThisMajor ++ ",1",
    Newer1Major = vsn(X,Y+1),
    Newer1Minor = Newer1Major ++ ".1",
    Newer2Major = ThisMajor ++ "1",
    NokVsns = [ThirdMajor,ThirdMinor,
	       Illegal,
	       Newer1Major,Newer1Minor,
	       Newer2Major],
    {OkVsns,NokVsns}.

vsn(X,Y) ->
    integer_to_list(X) ++ "." ++ integer_to_list(Y).

check_appup([Vsn|Vsns],Instrs,Expected) ->
    case systools_relup:appup_search_for_version(Vsn, Instrs) of
	Expected -> check_appup(Vsns,Instrs,Expected);
	Other -> ct:fail({unexpected_result_for_vsn,Vsn,Other})
    end;
check_appup([],_,_) ->
    ok.
