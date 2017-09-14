%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
-module(uri_string_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,
         parse_binary_fragment/1, parse_binary_host/1, parse_binary_host_ipv4/1,
         parse_binary_host_ipv6/1,
         parse_binary_path/1, parse_binary_port/1,
         parse_binary_query/1, parse_binary_scheme/1, parse_binary_userinfo/1,
         parse_fragment/1, parse_host/1, parse_host_ipv4/1, parse_host_ipv6/1,
         parse_path/1, parse_port/1,
         parse_query/1, parse_scheme/1, parse_userinfo/1,
	 parse_list/1, parse_binary/1, parse_mixed/1, parse_relative/1
        ]).

suite() ->
    [{timetrap,{minutes,1}}].

all() ->
    [
     parse_binary_scheme,
     parse_binary_userinfo,
     parse_binary_host,
     parse_binary_host_ipv4,
     parse_binary_host_ipv6,
     parse_binary_port,
     parse_binary_path,
     parse_binary_query,
     parse_binary_fragment,
     parse_scheme,
     parse_userinfo,
     parse_host,
     parse_host_ipv4,
     parse_host_ipv6,
     parse_port,
     parse_path,
     parse_query,
     parse_fragment,
     parse_list,
     parse_binary,
     parse_mixed,
     parse_relative
    ].

groups() ->
    [].

parse_binary_scheme(_Config) ->
    #{} = uri_string:parse(<<>>),
    #{path := <<"foo">>} = uri_string:parse(<<"foo">>),
    #{scheme := <<"foo">>} = uri_string:parse(<<"foo:">>),
    #{scheme := <<"foo">>, path := <<"bar:nisse">>} = uri_string:parse(<<"foo:bar:nisse">>),
    #{scheme := <<"foo">>, host := <<"">>} = uri_string:parse(<<"foo://">>),
    #{scheme := <<"foo">>, host := <<"">>, path := <<"/">>} = uri_string:parse(<<"foo:///">>),
    #{scheme := <<"foo">>, host := <<"">>, path := <<"//">>} = uri_string:parse(<<"foo:////">>),

    #{path := <<"/">>} = uri_string:parse(<<"/">>),
    #{host := <<>>} = uri_string:parse(<<"//">>),
    #{host := <<>>, path := <<"/">>} = uri_string:parse(<<"///">>).

parse_binary_userinfo(_Config) ->
    #{scheme := <<"user">>, path := <<"password@localhost">>} =
        uri_string:parse(<<"user:password@localhost">>),
    #{path := <<"user@">>} = uri_string:parse(<<"user@">>),
    #{path := <<"/user@">>} = uri_string:parse(<<"/user@">>),
    #{path := <<"user@localhost">>} = uri_string:parse(<<"user@localhost">>),
    #{userinfo := <<"user">>, host := <<"localhost">>} = uri_string:parse(<<"//user@localhost">>),
    #{userinfo := <<"user:password">>, host := <<"localhost">>} =
        uri_string:parse(<<"//user:password@localhost">>),
    #{scheme := <<"foo">>, path := <<"/user@">>} =
        uri_string:parse(<<"foo:/user@">>),
    #{scheme := <<"foo">>, userinfo := <<"user">>, host := <<"localhost">>} =
        uri_string:parse(<<"foo://user@localhost">>),
    #{scheme := <<"foo">>, userinfo := <<"user:password">>, host := <<"localhost">>} =
        uri_string:parse(<<"foo://user:password@localhost">>),
    uri_parse_error =(catch uri_string:parse("//user@")),
    uri_parse_error = (catch uri_string:parse("foo://user@")).

parse_binary_host(_Config) ->
    #{host := <<"hostname">>} = uri_string:parse(<<"//hostname">>),
    #{host := <<"hostname">>,scheme := <<"foo">>} = uri_string:parse(<<"foo://hostname">>),
    #{host := <<"hostname">>,scheme := <<"foo">>, userinfo := <<"user">>} =
        uri_string:parse(<<"foo://user@hostname">>).

parse_binary_host_ipv4(_Config) ->
    #{host := <<"127.0.0.1">>} = uri_string:parse(<<"//127.0.0.1">>),
    #{host := <<"127.0.0.1">>, path := <<"/over/there">>} =
        uri_string:parse(<<"//127.0.0.1/over/there">>),
    #{host := <<"127.0.0.1">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"//127.0.0.1?name=ferret">>),
    #{host := <<"127.0.0.1">>, fragment := <<"nose">>} = uri_string:parse(<<"//127.0.0.1#nose">>),
    uri_parse_error = (catch uri_string:parse(<<"//127.0.0.x">>)),
    uri_parse_error = (catch uri_string:parse(<<"//1227.0.0.1">>)).

parse_binary_host_ipv6(_Config) ->
    #{host := <<"::127.0.0.1">>} = uri_string:parse(<<"//[::127.0.0.1]">>),
    #{host := <<"2001:0db8:0000:0000:0000:0000:1428:07ab">>} =
        uri_string:parse(<<"//[2001:0db8:0000:0000:0000:0000:1428:07ab]">>),
    #{host := <<"::127.0.0.1">>, path := <<"/over/there">>} =
        uri_string:parse(<<"//[::127.0.0.1]/over/there">>),
    #{host := <<"::127.0.0.1">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"//[::127.0.0.1]?name=ferret">>),
    #{host := <<"::127.0.0.1">>, fragment := <<"nose">>} =
        uri_string:parse(<<"//[::127.0.0.1]#nose">>),
    uri_parse_error = (catch uri_string:parse(<<"//[::127.0.0.x]">>)),
    uri_parse_error = (catch uri_string:parse(<<"//[::1227.0.0.1]">>)),
    uri_parse_error = (catch uri_string:parse(<<"//[2001:0db8:0000:0000:0000:0000:1428:G7ab]">>)).

parse_binary_port(_Config) ->
    #{path:= <<"/:8042">>} =
        uri_string:parse(<<"/:8042">>),
    #{host:= <<>>, port := 8042} =
        uri_string:parse(<<"//:8042">>),
    #{host := <<"example.com">>, port:= 8042} =
        uri_string:parse(<<"//example.com:8042">>),
    #{scheme := <<"foo">>, path := <<"/:8042">>} =
        uri_string:parse(<<"foo:/:8042">>),
    #{scheme := <<"foo">>, host := <<>>, port := 8042} =
        uri_string:parse(<<"foo://:8042">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, port := 8042} =
        uri_string:parse(<<"foo://example.com:8042">>),
    uri_parse_error = (catch uri_string:parse(":600")),
    uri_parse_error = (catch uri_string:parse("//:8042x")).

parse_binary_path(_Config) ->
    #{path := <<"over/there">>} = uri_string:parse(<<"over/there">>),
    #{path := <<"/over/there">>} = uri_string:parse(<<"/over/there">>),
    #{scheme := <<"foo">>, path := <<"/over/there">>} =
        uri_string:parse(<<"foo:/over/there">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/over/there">>} =
        uri_string:parse(<<"foo://example.com/over/there">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/over/there">>, port := 8042} =
        uri_string:parse(<<"foo://example.com:8042/over/there">>).

parse_binary_query(_Config) ->
    #{scheme := <<"foo">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"foo:?name=ferret">>),
    #{scheme := <<"foo">>, path:= <<"over/there">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"foo:over/there?name=ferret">>),
    #{scheme := <<"foo">>, path:= <<"/over/there">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"foo:/over/there?name=ferret">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"foo://example.com?name=ferret">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"foo://example.com/?name=ferret">>),

    #{query := <<"?name=ferret">>} =
        uri_string:parse(<<"?name=ferret">>),
    #{path := <<"over/there">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"over/there?name=ferret">>),
    #{path := <<"/">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"/?name=ferret">>),
    #{path := <<"/over/there">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"/over/there?name=ferret">>),
    #{host := <<"example.com">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"//example.com?name=ferret">>),
    #{host := <<"example.com">>, path := <<"/">>, query := <<"?name=ferret">>} =
        uri_string:parse(<<"//example.com/?name=ferret">>).


parse_binary_fragment(_Config) ->
    #{scheme := <<"foo">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo:#nose">>),
    #{scheme := <<"foo">>, path:= <<"over/there">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo:over/there#nose">>),
    #{scheme := <<"foo">>, path:= <<"/over/there">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo:/over/there#nose">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://example.com#nose">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://example.com/#nose">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://example.com#nose">>),

    #{fragment := <<"nose">>} =
        uri_string:parse(<<"#nose">>),
    #{path := <<"over/there">>, fragment := <<"nose">>} =
        uri_string:parse(<<"over/there#nose">>),
    #{path := <<"/">>, fragment := <<"nose">>} =
        uri_string:parse(<<"/#nose">>),
    #{path := <<"/over/there">>, fragment := <<"nose">>} =
        uri_string:parse(<<"/over/there#nose">>),
    #{host := <<"example.com">>, fragment := <<"nose">>} =
        uri_string:parse(<<"//example.com#nose">>),
    #{host := <<"example.com">>, path := <<"/">>, fragment := <<"nose">>} =
        uri_string:parse(<<"//example.com/#nose">>).

parse_scheme(_Config) ->
    #{} = uri_string:parse(""),
    #{path := "foo"} = uri_string:parse("foo"),
    #{scheme := "foo"} = uri_string:parse("foo:"),
    #{scheme := "foo", path := "bar:nisse"} = uri_string:parse("foo:bar:nisse"),
    #{scheme := "foo", host := ""} = uri_string:parse("foo://"),
    #{scheme := "foo", host := "", path := "/"} = uri_string:parse("foo:///"),
    #{scheme := "foo", host := "", path := "//"} = uri_string:parse("foo:////"),

    #{path := "/"} = uri_string:parse("/"),
    #{host := ""} = uri_string:parse("//"),
    #{host := "", path := "/"} = uri_string:parse("///").

parse_userinfo(_Config) ->
    #{scheme := "user", path := "password@localhost"} = uri_string:parse("user:password@localhost"),
    #{path := "user@"} = uri_string:parse("user@"),
    #{path := "/user@"} = uri_string:parse("/user@"),
    #{path := "user@localhost"} = uri_string:parse("user@localhost"),
    #{userinfo := "user", host := "localhost"} = uri_string:parse("//user@localhost"),
    #{userinfo := "user:password", host := "localhost"} =
        uri_string:parse("//user:password@localhost"),
    #{scheme := "foo", path := "/user@"} =
        uri_string:parse("foo:/user@"),
    #{scheme := "foo", userinfo := "user", host := "localhost"} =
        uri_string:parse("foo://user@localhost"),
    #{scheme := "foo", userinfo := "user:password", host := "localhost"} =
        uri_string:parse("foo://user:password@localhost").

parse_host(_Config) ->
    #{host := "hostname"} = uri_string:parse("//hostname"),
    #{host := "hostname",scheme := "foo"} = uri_string:parse("foo://hostname"),
    #{host := "hostname",scheme := "foo", userinfo := "user"} =
        uri_string:parse("foo://user@hostname").

parse_host_ipv4(_Config) ->
    #{host := "127.0.0.1"} = uri_string:parse("//127.0.0.1"),
    #{host := "2001:0db8:0000:0000:0000:0000:1428:07ab"} =
        uri_string:parse("//[2001:0db8:0000:0000:0000:0000:1428:07ab]"),
    #{host := "127.0.0.1", path := "/over/there"} = uri_string:parse("//127.0.0.1/over/there"),
    #{host := "127.0.0.1", query := "?name=ferret"} = uri_string:parse("//127.0.0.1?name=ferret"),
    #{host := "127.0.0.1", fragment := "nose"} = uri_string:parse("//127.0.0.1#nose"),
    uri_parse_error = (catch uri_string:parse("//127.0.0.x")),
    uri_parse_error = (catch uri_string:parse("//1227.0.0.1")).

parse_host_ipv6(_Config) ->
    #{host := "::127.0.0.1"} = uri_string:parse("//[::127.0.0.1]"),
    #{host := "::127.0.0.1", path := "/over/there"} = uri_string:parse("//[::127.0.0.1]/over/there"),
    #{host := "::127.0.0.1", query := "?name=ferret"} =
        uri_string:parse("//[::127.0.0.1]?name=ferret"),
    #{host := "::127.0.0.1", fragment := "nose"} = uri_string:parse("//[::127.0.0.1]#nose"),
    uri_parse_error = (catch uri_string:parse("//[::127.0.0.x]")),
    uri_parse_error = (catch uri_string:parse("//[::1227.0.0.1]")),
    uri_parse_error = (catch uri_string:parse("//[2001:0db8:0000:0000:0000:0000:1428:G7ab]")).

parse_port(_Config) ->
    #{path:= "/:8042"} =
        uri_string:parse("/:8042"),
    #{host:= "", port := 8042} =
        uri_string:parse("//:8042"),
    #{host := "example.com", port:= 8042} =
        uri_string:parse("//example.com:8042"),
    #{scheme := "foo", path := "/:8042"} =
        uri_string:parse("foo:/:8042"),
    #{scheme := "foo", host := "", port := 8042} =
        uri_string:parse("foo://:8042"),
    #{scheme := "foo", host := "example.com", port := 8042} =
        uri_string:parse("foo://example.com:8042").

parse_path(_Config) ->
    #{path := "over/there"} = uri_string:parse("over/there"),
    #{path := "/over/there"} = uri_string:parse("/over/there"),
    #{scheme := "foo", path := "/over/there"} =
        uri_string:parse("foo:/over/there"),
    #{scheme := "foo", host := "example.com", path := "/over/there"} =
        uri_string:parse("foo://example.com/over/there"),
    #{scheme := "foo", host := "example.com", path := "/over/there", port := 8042} =
        uri_string:parse("foo://example.com:8042/over/there").

parse_query(_Config) ->
    #{scheme := "foo", query := "?name=ferret"} =
        uri_string:parse("foo:?name=ferret"),
    #{scheme := "foo", path:= "over/there", query := "?name=ferret"} =
        uri_string:parse("foo:over/there?name=ferret"),
    #{scheme := "foo", path:= "/over/there", query := "?name=ferret"} =
        uri_string:parse("foo:/over/there?name=ferret"),
    #{scheme := "foo", host := "example.com", query := "?name=ferret"} =
        uri_string:parse("foo://example.com?name=ferret"),
    #{scheme := "foo", host := "example.com", path := "/", query := "?name=ferret"} =
        uri_string:parse("foo://example.com/?name=ferret"),

    #{query := "?name=ferret"} =
        uri_string:parse("?name=ferret"),
    #{path := "over/there", query := "?name=ferret"} =
        uri_string:parse("over/there?name=ferret"),
    #{path := "/", query := "?name=ferret"} =
        uri_string:parse("/?name=ferret"),
    #{path := "/over/there", query := "?name=ferret"} =
        uri_string:parse("/over/there?name=ferret"),
    #{host := "example.com", query := "?name=ferret"} =
        uri_string:parse("//example.com?name=ferret"),
    #{host := "example.com", path := "/", query := "?name=ferret"} =
        uri_string:parse("//example.com/?name=ferret").


parse_fragment(_Config) ->
    #{scheme := "foo", fragment := "nose"} =
        uri_string:parse("foo:#nose"),
    #{scheme := "foo", path:= "over/there", fragment := "nose"} =
        uri_string:parse("foo:over/there#nose"),
    #{scheme := "foo", path:= "/over/there", fragment := "nose"} =
        uri_string:parse("foo:/over/there#nose"),
    #{scheme := "foo", host := "example.com", fragment := "nose"} =
        uri_string:parse("foo://example.com#nose"),
    #{scheme := "foo", host := "example.com", path := "/", fragment := "nose"} =
        uri_string:parse("foo://example.com/#nose"),
    #{scheme := "foo", host := "example.com", fragment := "nose"} =
        uri_string:parse("foo://example.com#nose"),

    #{fragment := "nose"} =
        uri_string:parse("#nose"),
    #{path := "over/there", fragment := "nose"} =
        uri_string:parse("over/there#nose"),
    #{path := "/", fragment := "nose"} =
        uri_string:parse("/#nose"),
    #{path := "/over/there", fragment := "nose"} =
        uri_string:parse("/over/there#nose"),
    #{host := "example.com", fragment := "nose"} =
        uri_string:parse("//example.com#nose"),
    #{host := "example.com", path := "/", fragment := "nose"} =
        uri_string:parse("//example.com/#nose").


parse_list(_Config) ->
    #{scheme := "foo", path := "bar:nisse"} = uri_string:parse("foo:bar:nisse"),
    #{scheme := "foo", host := "example.com", port := 8042,
      path := "/over/there", query := "?name=ferret", fragment := "nose"} =
        uri_string:parse("foo://example.com:8042/over/there?name=ferret#nose"),
    #{scheme := "foo", userinfo := "admin:admin", host := "example.com", port := 8042,
      path := "/over/there", query := "?name=ferret", fragment := "nose"} =
        uri_string:parse("foo://admin:admin@example.com:8042/over/there?name=ferret#nose").

parse_binary(_Config) ->
    #{scheme := <<"foo">>, path := <<"bar:nisse">>} = uri_string:parse(<<"foo:bar:nisse">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, port := 8042,
      path := <<"/over/there">>, query := <<"?name=ferret">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://example.com:8042/over/there?name=ferret#nose">>),
    #{scheme := <<"foo">>, userinfo := <<"admin:admin">>, host := <<"example.com">>, port := 8042,
      path := <<"/over/there">>, query := <<"?name=ferret">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://admin:admin@example.com:8042/over/there?name=ferret#nose">>).


parse_mixed(_Config) ->
    #{scheme := "foo", path := "bar"} =
        uri_string:parse(lists:append("fo",<<"o:bar">>)),
    #{scheme := "foo", path := "bar"} =
        uri_string:parse(lists:append("foo:b",<<"ar">>)),
    #{scheme := "foo", path := "bar:bar"} =
        uri_string:parse([[102],[111,111],<<":bar">>,58,98,97,114]).

parse_relative(_Config) ->
    #{path := "/path"} =
        uri_string:parse(lists:append("/pa",<<"th">>)),
    #{path := "foo"} =
        uri_string:parse(lists:append("fo",<<"o">>)).
