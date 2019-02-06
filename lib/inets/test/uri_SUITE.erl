%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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

%%
%% ct:run("../inets_test", uri_SUITE).
%%

-module(uri_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(GOOGLE, "www.google.com").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     ipv4,
     ipv6,
     host,
     userinfo,
     scheme,
     queries,
     fragments,
     escaped,
     hexed_query,
     scheme_validation,
     scheme_validation_bin,
     encode_decode
    ].

%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Config.
end_per_testcase(_Case, _Config) ->
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

ipv4(Config) when is_list(Config) ->
    {ok, {http,[],"127.0.0.1",80,"/foobar.html",[]}} =
    http_uri:parse("http://127.0.0.1/foobar.html"),

    {ok, {http,<<>>,<<"127.0.0.1">>,80,<<"/foobar.html">>,<<>>}} =
    http_uri:parse(<<"http://127.0.0.1/foobar.html">>).

ipv6(Config) when is_list(Config) ->
    {ok, {http,[],"2010:836B:4179::836B:4179",80,"/foobar.html",[]}} =
	http_uri:parse("http://[2010:836B:4179::836B:4179]/foobar.html"),
    {ok, {http,[],"[2010:836B:4179::836B:4179]",80,"/foobar.html",[]}} =
	http_uri:parse("http://[2010:836B:4179::836B:4179]/foobar.html",
		       [{ipv6_host_with_brackets, true}]),
    {ok, {http,[],"2010:836B:4179::836B:4179",80,"/foobar.html",[]}} =
	http_uri:parse("http://[2010:836B:4179::836B:4179]/foobar.html",
		       [{ipv6_host_with_brackets, false}]),
    {ok, {http,[],"2010:836B:4179::836B:4179",80,"/foobar.html",[]}} =
	http_uri:parse("http://[2010:836B:4179::836B:4179]/foobar.html",
		       [{foo, false}]),
    {error,
     {malformed_url, _, "http://2010:836B:4179::836B:4179/foobar.html"}} =
    http_uri:parse("http://2010:836B:4179::836B:4179/foobar.html"),

    {ok, {http,<<>>,<<"2010:836B:4179::836B:4179">>,80,<<"/foobar.html">>,<<>>}} =
    http_uri:parse(<<"http://[2010:836B:4179::836B:4179]/foobar.html">>),
    {ok, {http,<<>>,<<"[2010:836B:4179::836B:4179]">>,80,<<"/foobar.html">>,<<>>}} =
    http_uri:parse(<<"http://[2010:836B:4179::836B:4179]/foobar.html">>,
               [{ipv6_host_with_brackets, true}]),
    {ok, {http,<<>>,<<"2010:836B:4179::836B:4179">>,80,<<"/foobar.html">>,<<>>}} =
    http_uri:parse(<<"http://[2010:836B:4179::836B:4179]/foobar.html">>,
               [{ipv6_host_with_brackets, false}]),
    {ok, {http,<<>>,<<"2010:836B:4179::836B:4179">>,80,<<"/foobar.html">>,<<>>}} =
    http_uri:parse(<<"http://[2010:836B:4179::836B:4179]/foobar.html">>,
               [{foo, false}]),
    {error,
     {malformed_url, _, <<"http://2010:836B:4179::836B:4179/foobar.html">>}} =
    http_uri:parse(<<"http://2010:836B:4179::836B:4179/foobar.html">>).

host(Config) when is_list(Config) ->
    {ok, {http,[],"localhost",8888,"/foobar.html",[]}} =
    http_uri:parse("http://localhost:8888/foobar.html"),

    {ok, {http,<<>>,<<"localhost">>,8888,<<"/foobar.html">>,<<>>}} =
    http_uri:parse(<<"http://localhost:8888/foobar.html">>).

userinfo(Config) when is_list(Config) ->
    {ok, {http,"nisse:foobar","localhost",8888,"/foobar.html",[]}} =
    http_uri:parse("http://nisse:foobar@localhost:8888/foobar.html"),

    {ok, {http,<<"nisse:foobar">>,<<"localhost">>,8888,<<"/foobar.html">>,<<>>}} =
    http_uri:parse(<<"http://nisse:foobar@localhost:8888/foobar.html">>).

scheme(Config) when is_list(Config) ->
    {error, no_scheme} = http_uri:parse("localhost/foobar.html"),
    {error, {malformed_url, _, _}} =
    http_uri:parse("localhost:8888/foobar.html"),

    {error, no_scheme} = http_uri:parse(<<"localhost/foobar.html">>),
    {error, {malformed_url, _, _}} =
    http_uri:parse(<<"localhost:8888/foobar.html">>).

queries(Config) when is_list(Config) ->
    {ok, {http,[],"localhost",8888,"/foobar.html","?foo=bar&foobar=42"}} =
    http_uri:parse("http://localhost:8888/foobar.html?foo=bar&foobar=42"),

    {ok, {http,<<>>,<<"localhost">>,8888,<<"/foobar.html">>,<<"?foo=bar&foobar=42">>}} =
    http_uri:parse(<<"http://localhost:8888/foobar.html?foo=bar&foobar=42">>).

fragments(Config) when is_list(Config) ->
    {ok, {http,[],"localhost",80,"/",""}} =
        http_uri:parse("http://localhost#fragment"),
    {ok, {http,[],"localhost",80,"/path",""}} =
        http_uri:parse("http://localhost/path#fragment"),
    {ok, {http,[],"localhost",80,"/","?query"}} =
        http_uri:parse("http://localhost?query#fragment"),
    {ok, {http,[],"localhost",80,"/path","?query"}} =
        http_uri:parse("http://localhost/path?query#fragment"),
    {ok, {http,[],"localhost",80,"/","","#fragment"}} =
        http_uri:parse("http://localhost#fragment", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/path","","#fragment"}} =
        http_uri:parse("http://localhost/path#fragment", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/","?query","#fragment"}} =
        http_uri:parse("http://localhost?query#fragment", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/path","?query","#fragment"}} =
        http_uri:parse("http://localhost/path?query#fragment",
                       [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/","",""}} =
        http_uri:parse("http://localhost", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/path","",""}} =
        http_uri:parse("http://localhost/path", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/","?query",""}} =
        http_uri:parse("http://localhost?query", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/path","?query",""}} =
        http_uri:parse("http://localhost/path?query", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/","","#"}} =
        http_uri:parse("http://localhost#", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/path","","#"}} =
        http_uri:parse("http://localhost/path#", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/","?query","#"}} =
        http_uri:parse("http://localhost?query#", [{fragment,true}]),
    {ok, {http,[],"localhost",80,"/path","?query","#"}} =
        http_uri:parse("http://localhost/path?query#", [{fragment,true}]),


    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<"">>}} =
        http_uri:parse(<<"http://localhost#fragment">>),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/path">>,<<"">>}} =
        http_uri:parse(<<"http://localhost/path#fragment">>),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<"?query">>}} =
        http_uri:parse(<<"http://localhost?query#fragment">>),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/path">>,<<"?query">>}} =
        http_uri:parse(<<"http://localhost/path?query#fragment">>),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<"">>,<<"#fragment">>}} =
        http_uri:parse(<<"http://localhost#fragment">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/path">>,<<"">>,<<"#fragment">>}} =
        http_uri:parse(<<"http://localhost/path#fragment">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<"?query">>,<<"#fragment">>}} =
        http_uri:parse(<<"http://localhost?query#fragment">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/path">>,<<"?query">>,<<"#fragment">>}} =
        http_uri:parse(<<"http://localhost/path?query#fragment">>,
                       [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<"">>,<<"">>}} =
        http_uri:parse(<<"http://localhost">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/path">>,<<"">>,<<"">>}} =
        http_uri:parse(<<"http://localhost/path">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<"?query">>,<<"">>}} =
        http_uri:parse(<<"http://localhost?query">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/path">>,<<"?query">>,<<"">>}} =
        http_uri:parse(<<"http://localhost/path?query">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<"">>,<<"#">>}} =
        http_uri:parse(<<"http://localhost#">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/path">>,<<"">>,<<"#">>}} =
        http_uri:parse(<<"http://localhost/path#">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<"?query">>,<<"#">>}} =
        http_uri:parse(<<"http://localhost?query#">>, [{fragment,true}]),
    {ok, {http,<<>>,<<"localhost">>,80,<<"/path">>,<<"?query">>,<<"#">>}} =
        http_uri:parse(<<"http://localhost/path?query#">>, [{fragment,true}]),
    ok.

escaped(Config) when is_list(Config) ->
       {ok, {http,[],"www.somedomain.com",80,"/%2Eabc",[]}} =
	http_uri:parse("http://www.somedomain.com/%2Eabc"),
    {ok, {http,[],"www.somedomain.com",80,"/%252Eabc",[]}} =
	http_uri:parse("http://www.somedomain.com/%252Eabc"),
    {ok, {http,[],"www.somedomain.com",80,"/%25abc",[]}} =
	http_uri:parse("http://www.somedomain.com/%25abc"),
    {ok, {http,[],"www.somedomain.com",80,"/%25abc", "?foo=bar"}} =
    http_uri:parse("http://www.somedomain.com/%25abc?foo=bar"),

    {ok, {http,<<>>,<<"www.somedomain.com">>,80,<<"/%2Eabc">>,<<>>}} =
    http_uri:parse(<<"http://www.somedomain.com/%2Eabc">>),
    {ok, {http,<<>>,<<"www.somedomain.com">>,80,<<"/%252Eabc">>,<<>>}} =
    http_uri:parse(<<"http://www.somedomain.com/%252Eabc">>),
    {ok, {http,<<>>,<<"www.somedomain.com">>,80,<<"/%25abc">>,<<>>}} =
    http_uri:parse(<<"http://www.somedomain.com/%25abc">>),
    {ok, {http,<<>>,<<"www.somedomain.com">>,80,<<"/%25abc">>, <<"?foo=bar">>}} =
    http_uri:parse(<<"http://www.somedomain.com/%25abc?foo=bar">>).

hexed_query(doc) ->
    [{doc, "Solves OTP-6191"}];
hexed_query(Config) when is_list(Config) ->
    Google = ?GOOGLE,
    GoogleSearch = "http://" ++ Google ++ "/search",
    Search1 = "?hl=en&q=a%D1%85%D1%83%D0%B9&btnG=Google+Search",
    URI1    = GoogleSearch ++ Search1,
    Search2 = "?hl=en&q=%25%25",
    URI2    = GoogleSearch ++ Search2,
    Search3 = "?hl=en&q=%foo",
    URI3    = GoogleSearch ++ Search3,

    Verify1 =
	fun({http, [], ?GOOGLE, 80, "/search", _}) -> ok;
	   (_) -> error
	end,
    Verify2 = Verify1,
    Verify3 = Verify1,
    verify_uri(URI1, Verify1),
    verify_uri(URI2, Verify2),
    verify_uri(URI3, Verify3).

scheme_validation(Config) when is_list(Config) ->
    {ok, {http,[],"localhost",80,"/",""}} =
	http_uri:parse("http://localhost#fragment"),

    ValidationFun =
	fun("http") -> valid;
	   (_) -> {error, bad_scheme}
	end,

    {ok, {http,[],"localhost",80,"/",""}} =
	http_uri:parse("http://localhost#fragment",
		       [{scheme_validation_fun, ValidationFun}]),
    {error, bad_scheme} =
	http_uri:parse("https://localhost#fragment",
		       [{scheme_validation_fun, ValidationFun}]),
    %% non-fun scheme_validation_fun works as no option passed
    {ok, {https,[],"localhost",443,"/",""}} =
	http_uri:parse("https://localhost#fragment",
		       [{scheme_validation_fun, none}]).

scheme_validation_bin(Config) when is_list(Config) ->
    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<>>}} =
        http_uri:parse(<<"http://localhost#fragment">>),

    ValidationFun =
        fun(<<"http">>) -> valid;
           (_) -> {error, bad_scheme}
        end,

    {ok, {http,<<>>,<<"localhost">>,80,<<"/">>,<<>>}} =
        http_uri:parse(<<"http://localhost#fragment">>,
                       [{scheme_validation_fun, ValidationFun}]),
    {error, bad_scheme} =
        http_uri:parse(<<"https://localhost#fragment">>,
                       [{scheme_validation_fun, ValidationFun}]),
    %% non-fun scheme_validation_fun works as no option passed
    {ok, {https,<<>>,<<"localhost">>,443,<<"/">>,<<>>}} =
        http_uri:parse(<<"https://localhost#fragment">>,
                       [{scheme_validation_fun, none}]).

encode_decode(Config) when is_list(Config) ->
    ?assertEqual("foo%20bar", http_uri:encode("foo bar")),
    ?assertEqual(<<"foo%20bar">>, http_uri:encode(<<"foo bar">>)),

    ?assertEqual("foo+bar", http_uri:decode("foo+bar")),
    ?assertEqual(<<"foo+bar">>, http_uri:decode(<<"foo+bar">>)),
    ?assertEqual("foo bar", http_uri:decode("foo%20bar")),
    ?assertEqual(<<"foo bar">>, http_uri:decode(<<"foo%20bar">>)),
    ?assertEqual("foo\r\n", http_uri:decode("foo%0D%0A")),
    ?assertEqual(<<"foo\r\n">>, http_uri:decode(<<"foo%0D%0A">>)).


%%--------------------------------------------------------------------
%% Internal Functions ------------------------------------------------
%%--------------------------------------------------------------------


verify_uri(URI, Verify) ->
    case http_uri:parse(URI) of
	{ok, ParsedURI} ->
	    case Verify(ParsedURI) of
		ok ->
		    ok;
		error ->
		    Reason = {unexpected_parse_result, URI, ParsedURI},
		    ERROR  = {error, Reason},
		    throw(ERROR)
	    end;
	{error, _} = ERROR ->
	    throw(ERROR)
    end.
