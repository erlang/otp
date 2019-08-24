%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(httpc_cookie_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(URL,        "http://myhost.cookie.test.org").
-define(URL_DOMAIN, "http://myhost2.cookie.test.org").
-define(URL_SECURE, "https://myhost.cookie.test.org").

%% Test server callback functions

suite() -> 
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,15}}
    ].

all() -> 
    [
     session_cookies_only, 
     netscape_cookies, 
     cookie_cancel,
     cookie_expires, 
     persistent_cookie, 
     domain_cookie,
     secure_cookie, 
     update_cookie, 
     update_cookie_session,
     cookie_attributes
    ].

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

init_per_testcase(session_cookies_only = Case, Config0) ->
    Config = init_workdir(Case, Config0), 
    application:start(inets), 
    httpc:set_options([{cookies, verify}]),
    Config;
init_per_testcase(cookie_expires = Case, Config0) ->
    Config = init_workdir(Case, Config0), 
    CaseDir = proplists:get_value(case_top_dir, Config),
    application:start(inets), 
    application:set_env(inets, services, [{httpc, {default, CaseDir}}]),
    application:start(inets), 
    httpc:set_options([{cookies, verify}]),
    Config;
init_per_testcase(Case, Config0) ->
    Config = init_workdir(Case, Config0), 
    CaseDir = proplists:get_value(case_top_dir, Config),
    application:load(inets),
    application:set_env(inets, services, [{httpc, {default, CaseDir}}]),
    application:start(inets), 
    httpc:set_options([{cookies, verify}]),
    Config.

init_workdir(Case, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    SuiteTopDir = filename:join(PrivDir, ?MODULE),
    case file:make_dir(SuiteTopDir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        Error ->
           ct:fail({failed_creating_subsuite_top_dir, Error})
    end,

    CaseTopDir = filename:join(SuiteTopDir, Case),
    ok   = file:make_dir(CaseTopDir),
    [{suite_top_dir, SuiteTopDir}, 
     {case_top_dir,  CaseTopDir} | Config].

end_per_testcase(_, _) ->
    application:stop(inets).


%% Test cases starts here.
%%--------------------------------------------------------------------
session_cookies_only() -> 
    [{doc, "Test that all cookies are handled as session cookies if there"
     "does not exist a directory to save presitent cookies in."}].
session_cookies_only(Config) when is_list(Config) -> 
    SetCookieHeaders = [{"set-cookie", "test_cookie=true; path=/;" 
			 ";max-age=60000"}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    {"cookie", "$Version=0; test_cookie=true; $Path=/"} = 
	httpc:cookie_header(?URL),
    application:stop(inets),
    application:start(inets),
    {"cookie", ""} = httpc:cookie_header(?URL).

netscape_cookies() -> 
    [{doc, "Test that the old (original) format of cookies are accepted."}].
netscape_cookies(Config) when is_list(Config) -> 
    Expires = future_netscape_date(),
    SetCookieHeaders = [{"set-cookie", "test_cookie=true; path=/; " 
			 "expires=" ++ Expires}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    {"cookie", "$Version=0; test_cookie=true; $Path=/"} = 
	httpc:cookie_header(?URL).

cookie_cancel() -> 
    [{doc, "A cookie can be canceld by sending the same cookie with max-age=0 "
     "this test cheks that cookie is canceled."}].
cookie_cancel(Config) when is_list(Config) -> 
    SetCookieHeaders = [{"set-cookie", "test_cookie=true; path=/;" 
			 "max-age=60000"}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    {"cookie", "$Version=0; test_cookie=true; $Path=/"} = 
	httpc:cookie_header(?URL),
    NewSetCookieHeaders = 
	[{"set-cookie", "test_cookie=true; path=/;max-age=0"}],
    httpc:store_cookies(NewSetCookieHeaders, ?URL),
    {"cookie", ""} = httpc:cookie_header(?URL).

cookie_expires() -> 
    [{doc, "Test that a cookie is not used when it has expired"}].
cookie_expires(Config) when is_list(Config) -> 
    SetCookieHeaders = [{"set-cookie", "test_cookie=true; path=/;" 
			 "max-age=5"}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    {"cookie", "$Version=0; test_cookie=true; $Path=/"} = 
	httpc:cookie_header(?URL),
    timer:sleep(10000),
    {"cookie", ""} = httpc:cookie_header(?URL).

persistent_cookie() ->
    [{doc, "Test domian cookie attribute"}].
persistent_cookie(Config) when is_list(Config)->
    SetCookieHeaders = [{"set-cookie", "test_cookie=true; path=/;" 
			 "max-age=60000"}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    {"cookie", "$Version=0; test_cookie=true; $Path=/"} = 
	httpc:cookie_header(?URL),
    CaseDir = proplists:get_value(case_top_dir, Config),
    application:stop(inets),
    application:load(inets),
    application:set_env(inets, services, [{httpc, {default, CaseDir}}]),
    application:start(inets), 
    httpc:set_options([{cookies, enabled}]),
    {"cookie","$Version=0; test_cookie=true; $Path=/"} = httpc:cookie_header(?URL).

domain_cookie() ->
    [{doc, "Test the domian cookie attribute"}].
domain_cookie(Config) when is_list(Config) ->
    SetCookieHeaders = [{"set-cookie", "test_cookie=true; path=/;" 
			 "domain=.cookie.test.org"}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    {"cookie","$Version=0; test_cookie=true; $Path=/; "
     "$Domain=.cookie.test.org"} = 
	httpc:cookie_header(?URL_DOMAIN).

secure_cookie() ->
    [{doc, "Test the secure cookie attribute"}].
secure_cookie(Config) when is_list(Config) ->
    SetCookieHeaders = [{"set-cookie", "test_cookie=true; path=/; secure"}],
    ok = httpc:store_cookies(SetCookieHeaders, ?URL),
    check_cookie("$Version=0; test_cookie=true; $Path=/", ?URL_SECURE), 
    check_cookie("", ?URL),
    SetCookieHeaders1 = [{"set-cookie", "test1_cookie=true; path=/; secure"}],
    ok = httpc:store_cookies(SetCookieHeaders1, ?URL),
    check_cookie("$Version=0; test_cookie=true; $Path=/; "
		 "test1_cookie=true; $Path=/",
		 ?URL_SECURE).
    
update_cookie()->
    [{doc, "Test that a (plain) cookie can be updated."}].
update_cookie(Config) when is_list(Config) ->
    print_cookies("Cookies before store"),

    SetCookieHeaders = 
	[{"set-cookie", "test_cookie=true; path=/; max-age=6500"},
	 {"set-cookie", "test_cookie2=true; path=/; max-age=6500"}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    print_cookies("Cookies after first store"),
    ExpectCookie1 = 
	"$Version=0; "
	"test_cookie=true; $Path=/; "
	"test_cookie2=true; $Path=/", 
    expect_cookie_header(1, ExpectCookie1), 

    NewSetCookieHeaders = 
	[{"set-cookie", "test_cookie=false; path=/; max-age=6500"}],
    httpc:store_cookies(NewSetCookieHeaders, ?URL),
    print_cookies("Cookies after second store"),    
    ExpectCookie2 = 
	"$Version=0; "
	"test_cookie2=true; $Path=/; "
	"test_cookie=false; $Path=/", 
    expect_cookie_header(2, ExpectCookie2).

update_cookie_session()->
    [{doc, "Test that a session cookie can be updated."}].
update_cookie_session(Config) when is_list(Config)->
    print_cookies("Cookies before store"),

    SetCookieHeaders = [{"set-cookie", "test_cookie=true; path=/"},
			{"set-cookie", "test_cookie2=true; path=/"}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    print_cookies("Cookies after first store"),
    ExpectedCookie1 = 
	"$Version=0; test_cookie=true; $Path=/; test_cookie2=true; $Path=/",
    expect_cookie_header(1, ExpectedCookie1), 

    NewSetCookieHeaders = [{"set-cookie", "test_cookie=false; path=/"}],
    httpc:store_cookies(NewSetCookieHeaders, ?URL),
    print_cookies("Cookies after second store"),    
    ExpectedCookie2 = 
	"$Version=0; test_cookie2=true; $Path=/; test_cookie=false; $Path=/", 
    expect_cookie_header(2, ExpectedCookie2).
    

cookie_attributes() -> 
    [{doc, "Test attribute not covered by the other test cases"}].
cookie_attributes(Config) when is_list(Config) -> 
    SetCookieHeaders = [{"set-cookie", "test_cookie=true;version=1;"
			 "comment=foobar; "%% Comment
			 "foo=bar;" %% Nonsense should be ignored
			 "max-age=60000"}],
    httpc:store_cookies(SetCookieHeaders, ?URL),
    {"cookie","$Version=1; test_cookie=true"} = httpc:cookie_header(?URL).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
print_cookies(Pre) ->
    io:format("~s: ", [Pre]),
    print_cookies2(httpc:which_cookies()).

print_cookies2([]) ->
    ok;
print_cookies2([{cookies, Cookies}|Rest]) ->
    print_cookies3("Cookies", Cookies),
    print_cookies2(Rest);
print_cookies2([{session_cookies, Cookies}|Rest]) ->
    print_cookies3("Session Cookies", Cookies),
    print_cookies2(Rest);
print_cookies2([_|Rest]) ->
    print_cookies2(Rest).

print_cookies3(Header, []) ->
    io:format("   ~s: []", [Header]);
print_cookies3(Header, Cookies) ->
    io:format("   ~s: ", [Header]),
    Prefix      = "      ", 
    PrintCookie = 
	fun(Cookie) -> 
		io:format("~s", [httpc_cookie:image_of(Prefix, Cookie)]) 
	end, 
    lists:foreach(PrintCookie, Cookies).

expect_cookie_header(No, ExpectedCookie) ->
    case httpc:cookie_header(?URL) of
	{"cookie", ExpectedCookie} ->
	    ok;
    	{"cookie", BadCookie} ->
	    io:format("Bad Cookie ~w: "
		      "~n   Expected: ~s"
		      "~n   Received: ~s"
		      "~n", [No, ExpectedCookie, BadCookie]),
	    exit({bad_cookie_header, No, ExpectedCookie, BadCookie})
    end.

check_cookie(Expect, URL) ->
    case httpc:cookie_header(URL) of
	{"cookie", Expect} ->
	    ok;
	{"cookie", Unexpected} ->
	    case lists:prefix(Expect, Unexpected) of
		true ->
		    Extra = Unexpected -- Expect, 
		    ct:fail({extra_cookie_info, Extra});
		false ->
		    ct:fail({unknown_cookie, Expect, Unexpected})
	    end;
	Bad ->
	    ct:fail({bad_cookies, Bad})
    end.


future_netscape_date() ->
    [Day, DD, Mon, YYYY] = netscape_date(date()),
    lists:flatten(io_lib:format("~s, ~s ~s ~s 12:30:00 GMT", 
				[Day, DD, Mon, YYYY])).

netscape_date({Year, 12, _}) ->
    NewYear = Year + 1,
    NewMonth = 1,
    NewDay = calendar:last_day_of_the_month(NewYear, NewMonth),
    WeekDay = calendar:day_of_the_week(NewYear, NewMonth, NewDay),
    WeekDayNrStr = day_nr_str(NewDay), 
    NewDayStr = week_day_str(WeekDay),
    MonthStr = month_str(NewMonth),
    [NewDayStr, WeekDayNrStr, MonthStr, integer_to_list(NewYear)];

netscape_date({Year, Month, _})  ->
    NewMonth = Month + 1,
    NewDay = calendar:last_day_of_the_month(Year, NewMonth),
    WeekDay = calendar:day_of_the_week(Year, NewMonth, NewDay),
    WeekDayNrStr = day_nr_str(NewDay), 
    NewDayStr = week_day_str(WeekDay),
    MonthStr = month_str(NewMonth),
    [NewDayStr, WeekDayNrStr, MonthStr, integer_to_list(Year)].

week_day_str(1) ->
    "Mon";
week_day_str(2) ->
    "Tus";
week_day_str(3) ->
    "Wdy";
week_day_str(4) ->
    "Thu";
week_day_str(5) ->
    "Fri";
week_day_str(6) ->
    "Sat";
week_day_str(7) ->
    "Sun".

day_nr_str(1) ->
    "01";
day_nr_str(2) ->
    "02";
day_nr_str(3) ->
    "03";
day_nr_str(4) ->
    "04";
day_nr_str(5) ->
    "05";
day_nr_str(6) ->
    "06";
day_nr_str(7) ->
    "07";
day_nr_str(8) ->
    "08";
day_nr_str(0) ->
    "09";
day_nr_str(N) ->
    integer_to_list(N).

month_str(1) ->"Jan";
month_str(2) ->"Feb";
month_str(3) ->"Mar"; 
month_str(4) ->"Apr";
month_str(5) ->"May";
month_str(6) ->"Jun";
month_str(7) ->"Jul";
month_str(8) ->"Aug";
month_str(9) ->"Sep";
month_str(10) ->"Oct";
month_str(11) ->"Nov";
month_str(12) ->"Dec".


