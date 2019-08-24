%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(filename_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([absname/1, absname_2/1, 
	 basename_1/1, basename_2/1,
	 dirname/1, extension/1, join/1, t_nativename/1]).
-export([pathtype/1,rootname/1,split/1,find_src/1]).
-export([absname_bin/1, absname_bin_2/1, 
	 basename_bin_1/1, basename_bin_2/1,
	 dirname_bin/1, extension_bin/1, join_bin/1, t_nativename_bin/1]).
-export([pathtype_bin/1,rootname_bin/1,split_bin/1]).
-export([t_basedir_api/1, t_basedir_xdg/1, t_basedir_windows/1]).
-export([safe_relative_path/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [absname, absname_2,
     find_src,
     absname_bin, absname_bin_2,
     {group,p},
     t_basedir_xdg, t_basedir_windows,
     safe_relative_path].

groups() -> 
    [{p, [parallel],
      [dirname,
       extension, extension_bin,
       join, pathtype, rootname, split, t_nativename,
       basename_1, basename_2,
       basename_bin_1, basename_bin_2, dirname_bin,
       join_bin, pathtype_bin, rootname_bin, split_bin,
       t_nativename_bin,
       t_basedir_api]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

absname(Config) when is_list(Config) ->
    case os:type() of
        {win32, _} -> 
            [Drive|_] = proplists:get_value(priv_dir, Config),
            Temp = filename:join([Drive|":/"], "temp"),
            case file:make_dir(Temp) of
                ok -> ok;
                {error,eexist} -> ok
            end,
            {ok,Cwd} = file:get_cwd(),
            ok = file:set_cwd(Temp),
            [Drive|":/temp/foo"] = filename:absname(foo),
            [Drive|":/temp/foo"] = filename:absname("foo"),
            [Drive|":/temp/../ebin"] = filename:absname("../ebin"),
            [Drive|":/erlang"] = filename:absname("/erlang"),
            [Drive|":/erlang/src"] = filename:absname("/erlang/src"),
            [Drive|":/erlang/src"] = filename:absname("\\erlang\\src"),
            [Drive|":/temp/erlang"] = filename:absname([Drive|":erlang"]),
            [Drive|":/temp/erlang/src"] =
                filename:absname([Drive|":erlang/src"]),
            [Drive|":/temp/erlang/src"] =
                filename:absname([Drive|":erlang\\src\\"]),
            "a:/erlang" = filename:absname("a:erlang"),

            file:set_cwd([Drive|":/"]),
            [Drive|":/foo"] = filename:absname(foo),
            [Drive|":/foo"] = filename:absname("foo"),
            [Drive|":/../ebin"] = filename:absname("../ebin"),
            [Drive|":/erlang"] = filename:absname("/erlang"),
            [Drive|":/erlang/src"] = filename:absname("/erlang/src"),
            [Drive|":/erlang/src"] = filename:absname(["/erlang",'/src']),
            [Drive|":/erlang/src"] = filename:absname("\\erlang\\\\src"),
            [Drive|":/erlang"] = filename:absname([Drive|":erlang"]),
            [Drive|":/erlang/src"] = filename:absname([Drive|":erlang/src"]),
            "a:/erlang" = filename:absname("a:erlang"),

            "//foo" = filename:absname("//foo"),
            "//foo/bar" = filename:absname("//foo/bar"),
            "//foo/\bar" = filename:absname("//foo/\bar"),
            "//foo/bar/baz" = filename:absname("//foo/bar\\baz"),
            "//foo/bar/baz" = filename:absname("//foo\\bar/baz"),
            "//foo" = filename:absname("\\\\foo"),
            "//foo/bar" = filename:absname("\\\\foo/bar"),
            "//foo/\bar" = filename:absname("\\\\foo/\bar"),
            "//foo/bar/baz" = filename:absname("\\\\foo/bar\\baz"),
            "//foo/bar/baz" = filename:absname("\\\\foo\\bar/baz"),

            file:set_cwd(Cwd),
            ok;
        {unix, _} ->
            ok = file:set_cwd("/usr"),
            "/usr/foo" = filename:absname(foo),
            "/usr/foo" = filename:absname("foo"),
            "/usr/../ebin" = filename:absname("../ebin"),

            file:set_cwd("/"),
            "/foo" = filename:absname(foo),
            "/foo" = filename:absname("foo"),
            "/../ebin" = filename:absname("../ebin"),
            "/erlang" = filename:absname("/erlang"),
            "/erlang/src" = filename:absname("/erlang/src"),
            "/erlang/src" = filename:absname(["/erl",'ang/s',"rc"]),
            "/erlang/src" = filename:absname(["/erl",'a','ng',"/",'s',"rc"]),
            "/erlang/src" = filename:absname("/erlang///src"),
            "/file_sorter.erl" = filename:absname([file_sorter|'.erl']),
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

absname_2(Config) when is_list(Config) ->
    case os:type() of
        {win32, _} ->
            [Drive|_] = proplists:get_value(priv_dir, Config),
            [Drive|":/temp/foo"] = filename:absname(foo, [Drive|":/temp"]),
            [Drive|":/temp/foo"] = filename:absname("foo", [Drive|":/temp"]),
            [Drive|":/temp/../ebin"] = filename:absname("../ebin",
                                                        [Drive|":/temp"]),
            [Drive|":/erlang"] = filename:absname("/erlang", [Drive|":/temp"]),
            [Drive|":/erlang/src"] = filename:absname("/erlang/src",
                                                      [Drive|":/temp"]),
            [Drive|":/erlang/src"] = filename:absname("\\erlang\\src",
                                                      [Drive|":/temp"]),
            [Drive|":/temp/erlang"] = filename:absname([Drive|":erlang"],
                                                       [Drive|":/temp"]),
            [Drive|":/temp/erlang/src"] = filename:absname([Drive|":erlang/src"],
                                                           [Drive|":/temp"]),
            [Drive|":/temp/erlang/src"] =
                filename:absname([Drive|":erlang\\src\\"], [Drive|":/temp"]),
            "a:/erlang" = filename:absname("a:erlang", [Drive|":/temp"]),

            file:set_cwd([Drive|":/"]),
            [Drive|":/foo"] = filename:absname(foo, [Drive|":/"]),
            [Drive|":/foo"] = filename:absname("foo", [Drive|":/"]),
            [Drive|":/../ebin"] = filename:absname("../ebin", [Drive|":/"]),
            [Drive|":/erlang"] = filename:absname("/erlang", [Drive|":/"]),
            [Drive|":/erlang/src"] = filename:absname("/erlang/src",
                                                      [Drive|":/"]),
            [Drive|":/erlang/src"] = filename:absname("\\erlang\\\\src",
                                                      [Drive|":/"]),
            [Drive|":/erlang"] = filename:absname([Drive|":erlang"],
                                                  [Drive|":/"]),
            [Drive|":/erlang/src"] = filename:absname([Drive|":erlang/src"],
                                                      [Drive|":/"]),
            "a:/erlang" = filename:absname("a:erlang", [Drive|":/"]),

            "//foo" = filename:absname("foo","//"),
            "//foo/bar" = filename:absname("foo/bar", "//"),
            "//foo/bar" = filename:absname("bar", "//foo"),
            "//bar" = filename:absname("/bar", "//foo"),
            "//foo/bar/baz" = filename:absname("bar/baz", "//foo"),
            "//bar/baz" = filename:absname("//bar/baz", "//foo"),
            "//\bar" = filename:absname("/\bar", "//foo"),
            "//foo" = filename:absname("foo","\\\\"),
            "//foo/bar" = filename:absname("foo/bar", "\\\\"),
            "//foo/bar" = filename:absname("bar", "\\\\foo"),
            "//bar" = filename:absname("/bar", "\\\\foo"),
            "//foo/bar/baz" = filename:absname("bar/baz", "\\\\foo"),
            "//bar/baz" = filename:absname("\\\\bar/baz", "\\\\foo"),
            "//\bar" = filename:absname("/\bar", "\\\\foo"),
            "//bar/baz" = filename:absname("\\\\bar/baz", "//foo"),
            "//bar/baz" = filename:absname("//bar/baz", "\\\\foo"),

            ok;
        _ ->
            "/usr/foo" = filename:absname(foo, "/usr"),
            "/usr/foo" = filename:absname("foo", "/usr"),
            "/usr/../ebin" = filename:absname("../ebin", "/usr"),

            "/foo" = filename:absname(foo, "/"),
            "/foo" = filename:absname("foo", "/"),
            "/../ebin" = filename:absname("../ebin", "/"),
            "/erlang" = filename:absname("/erlang", "/"),
            "/erlang/src" = filename:absname("/erlang/src", "/"),
            "/erlang/src" = filename:absname("/erlang///src", "/"),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basename_1(Config) when is_list(Config) ->
    "." = filename:basename("."),
    "foo" = filename:basename("foo"),
    "foo" = filename:basename("/usr/foo"),
    "foo.erl" = filename:basename("A:usr/foo.erl"),
    "foo" = filename:basename('/usr/foo'),
    "foo" = filename:basename(["/usr","/","f","o","o"]),
    "foo" = filename:basename(["/usr/",foo]),
    "foo" = filename:basename(["/usr/f",oo]),
    "foo" = filename:basename(["usr/", "foo"]),
    "foo" = filename:basename(["usr/"|foo]),
    "foo" = filename:basename(["usr/foo/"]),
    case os:type() of
        {win32, _} ->
            "foo" = filename:basename(["usr\\foo\\"]),
            "foo" = filename:basename("A:\\usr\\foo"),
            "foo" = filename:basename("A:foo");
        _ ->
            "strange\\but\\true" =
                filename:basename("strange\\but\\true")
    end,
    ok.

basename_2(Config) when is_list(Config) ->
    "." = filename:basename(".", ".erl"),
    "foo" = filename:basename("foo.erl", ".erl"),
    "foo" = filename:basename('foo.erl', ".erl"),
    "foo" = filename:basename("foo.erl", '.erl'),
    "foo" = filename:basename(["/usr","/","f","oo"], ".erl"),
    "foo.erl" = filename:basename("/usr/foo.erl", ".hrl"),
    "foo.erl" = filename:basename("/usr.hrl/foo.erl", ".hrl"),
    "foo" = filename:basename("/usr.hrl/foo", ".hrl"),
    "foo" = filename:basename("usr/foo/", ".erl"),
    "foo.erl" = filename:basename("usr/foo.erl/", ".erl"),
    "foo.erl" = filename:basename("usr/foo.erl/", '.erl'),
    "foo" = filename:basename(["/usr",'/','f','oo'], ".erl"),
    "foo.erl" = filename:basename(["usr/foo.e",'rl/'], ".erl"),
    case os:type() of
        {win32, _} ->
            "foo" = filename:basename("A:foo", ".erl"),
            "foo.erl" = filename:basename("a:\\usr\\foo.erl", ".hrl"),
            "foo.erl" = filename:basename("c:\\usr.hrl\\foo.erl", ".hrl"),
            "foo" = filename:basename("A:\\usr\\foo", ".hrl");
        _ ->
            "strange\\but\\true" =
                filename:basename("strange\\but\\true.erl", ".erl"),
            "strange\\but\\true" =
                filename:basename("strange\\but\\true", ".erl")
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dirname(Config) when is_list(Config) ->
    case os:type() of
        {win32,_} ->
            "A:/usr" = filename:dirname("A:/usr/foo.erl"),
            "A:usr" = filename:dirname("A:usr/foo.erl"),
            "/usr" = filename:dirname("\\usr\\foo.erl"),
            "/" = filename:dirname("\\usr"),
            "//foo/bar" = filename:dirname("//foo/bar/baz.erl"),
            "//foo/\bar" = filename:dirname("//foo/\bar/baz.erl"),
            "//foo/bar" = filename:dirname("//foo\\bar/baz.erl"),
            "//foo/bar" = filename:dirname("\\\\foo/bar/baz.erl"),
            "//foo/\bar" = filename:dirname("\\\\foo/\bar/baz.erl"),
            "//foo/bar" = filename:dirname("\\\\foo\\bar/baz.erl"),
            "//foo" = filename:dirname("//foo/baz.erl"),
            "//foo" = filename:dirname("//foo/\baz.erl"),
            "//foo" = filename:dirname("//foo\\baz.erl"),
            "//foo" = filename:dirname("\\\\foo/baz.erl"),
            "//foo" = filename:dirname("\\\\foo/\baz.erl"),
            "//foo" = filename:dirname("\\\\foo\\baz.erl"),
            "A:" = filename:dirname("A:");
        _ -> true
    end,
    "usr" = filename:dirname("usr///foo.erl"),
    "." = filename:dirname("foo.erl"),
    "." = filename:dirname("."),
    "usr" = filename:dirname('usr/foo.erl'),
    "usr" = filename:dirname(['usr','/foo.erl']),
    "usr" = filename:dirname(['us','r/foo.erl']),
    "usr" = filename:dirname(['usr/','/foo.erl']),
    "usr" = filename:dirname(['usr/','foo.erl']),
    "usr" = filename:dirname(['usr/'|'foo.erl']),
    "usr" = filename:dirname(['usr/f','oo.erl']),
    "/" = filename:dirname("/"),
    "/" = filename:dirname("/usr"),
    ok.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extension(Config) when is_list(Config) ->
    ".erl" = filename:extension("A:/usr/foo.erl"),
    ".erl" = filename:extension("A:/usr/foo.nisse.erl"),
    ".erl" = filename:extension(["A:/usr/", 'foo.ni', "sse.erl"]),
    ".erl" = filename:extension(["A:/usr/", 'foo.ni', "sse.e", 'rl']),
    ".erl" = filename:extension(["A:/usr/", 'foo.ni', "sse.e"|'rl']),
    ".erl" = filename:extension("A:/usr.bar/foo.nisse.erl"),
    "" = filename:extension("A:/usr.bar/foo"),
    "" = filename:extension("A:/usr/foo"),
    case os:type() of
        {win32, _} ->
            "" = filename:extension("A:\\usr\\foo"),
            ".erl" = filename:extension("A:/usr.bar/foo.nisse.erl"),
            "" = filename:extension("A:/usr.bar/foo"),
            ok;
        _ -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
join(Config) when is_list(Config) ->
    %% Whenever joining two elements, test the equivalence between
    %% join/1 and join/2 (OTP-12158) by using help function
    %% filename_join/2.
    "/" = filename:join(["/"]),
    "usr/foo.erl" = filename_join("usr","foo.erl"),
    "/src/foo.erl" = filename_join(usr, "/src/foo.erl"),
    "/src/foo.erl" = filename_join("/src/",'foo.erl'),
    "/src/foo.erl" = filename_join(usr, ["/sr", 'c/foo.erl']),
    "/src/foo.erl" = filename_join("usr", "/src/foo.erl"),

    %% Make sure that redundant slashes work too.
    "a/b/c/d/e/f/g" = filename:join(["a//b/c/////d//e/f/g"]),
    "a/b/c/d/e/f/g" = filename_join("a//b/c/", "d//e/f/g"),
    "a/b/c/d/e/f/g" = filename_join("a//b/c", "d//e/f/g"),
    "/d/e/f/g" = filename_join("a//b/c", "/d//e/f/g"),

    "foo/bar" = filename_join([$f,$o,$o,$/,[]], "bar"),

    %% Single dots - should be removed if in the middle of the path,
    %% but not at the end of the path.
    "/." = filename:join(["/."]),
    "/" = filename:join(["/./"]),
    "/." = filename:join(["/./."]),
    "./." = filename:join(["./."]),

    "/a/b" = filename_join("/a/.","b"),
    "/a/b/." = filename_join("/a/.","b/."),
    "/a/." = filename_join("/a/.","."),
    "/a/." = filename_join("/a","."),
    "/a/." = filename_join("/a/.",""),
    "./." = filename_join("./.","."),
    "./." = filename_join("./","."),
    "./." = filename_join("./.",""),
    "." = filename_join(".",""),
    "./." = filename_join(".","."),

    %% Trailing slash shall be removed - except the root
    "/" = filename:join(["/"]),
    "/" = filename:join(["/./"]),
    "/a" = filename:join(["/a/"]),
    "/b" = filename_join("/a/","/b/"),
    "/a/b" = filename_join("/a/","b/"),

    case os:type() of
        {win32, _} ->
            "//" = filename:join(["//"]),
            "d:/" = filename:join(["D:/"]),
            "d:/" = filename:join(["D:\\"]),
            "d:/abc" = filename_join("D:/", "abc"),
            "d:abc" = filename_join("D:", "abc"),
            "a/b/c/d/e/f/g" = filename:join(["a//b\\c//\\/\\d/\\e/f\\g"]),
            "a:usr/foo.erl" = filename:join(["A:","usr","foo.erl"]),
            "/usr/foo.erl" = filename:join(["A:","/usr","foo.erl"]),
            "c:usr" = filename_join("A:","C:usr"),
            "a:usr" = filename_join("A:","usr"),
            "c:/usr" = filename_join("A:", "C:/usr"),
            "c:/usr/foo.erl" = filename:join(["A:","C:/usr","foo.erl"]),
            "c:usr/foo.erl" = filename:join(["A:","C:usr","foo.erl"]),
            "d:/foo" = filename:join([$D, $:, $/, []], "foo"),
            "//" = filename:join("\\\\", ""),
            "//foo" = filename:join("\\\\", "foo"),
            "//foo/bar" = filename:join("\\\\", "foo\\\\bar"),
            "//foo/bar/baz" = filename:join("\\\\foo", "bar\\\\baz"),
            "//foo/bar/baz" = filename:join("\\\\foo", "bar\\baz"),
            "//foo/bar/baz" = filename:join("\\\\foo\\bar", baz),
            "//foo/\bar/baz" = filename:join("\\\\foo/\bar", baz),
            "//foo/bar/baz" = filename:join("\\\\foo/bar", baz),
            "//bar/baz" = filename:join("\\\\foo", "\\\\bar\\baz"),
            "//bar/baz" = filename:join("\\\\foo", "//bar\\baz"),
            "//bar/baz" = filename:join("\\\\foo", "//bar/baz"),
            "//bar/baz" = filename:join("\\\\foo", "\\\\bar/baz"),
            "//d/e/f/g" = filename:join("a//b/c", "//d//e/f/g"),
            "//" = filename:join("//", ""),
            "//foo" = filename:join("//", "foo"),
            "//foo/bar" = filename:join("//", "foo\\\\bar"),
            "//foo/bar/baz" = filename:join("//foo", "bar\\\\baz"),
            "//foo/bar/baz" = filename:join("//foo", "bar\\baz"),
            "//foo/bar/baz" = filename:join("//foo\\bar", baz),
            "//foo/\bar/baz" = filename:join("//foo/\bar", baz),
            "//foo/bar/baz" = filename:join("//foo/bar", baz),
            "//bar/baz" = filename:join("//foo", "\\\\bar\\baz"),
            "//bar/baz" = filename:join("//foo", "//bar\\baz"),
            "//bar/baz" = filename:join("//foo", "//bar/baz"),
            "//bar/baz" = filename:join("//foo", "\\\\bar/baz"),
            ok;
        _ ->
            "/" = filename:join(["//"]),
            "/d/e/f/g" = filename:join("a//b/c", "//d//e/f/g"),
            ok
    end.

%% Make sure join([A,B]) is equivalent to join(A,B) (OTP-12158)
filename_join(A,B) ->
    Res = filename:join(A,B),
    Res = filename:join([A,B]).

pathtype(Config) when is_list(Config) ->
    relative = filename:pathtype(".."),
    relative = filename:pathtype("foo"),
    relative = filename:pathtype("foo/bar"),
    relative = filename:pathtype('foo/bar'),
    relative = filename:pathtype(['f','oo',"/bar"]),
    case os:type() of
        {win32, _} ->
            volumerelative = filename:pathtype("/usr/local/bin"),
            volumerelative = filename:pathtype("A:usr/local/bin"),
            ok;
        _ ->
            absolute = filename:pathtype("/"),
            absolute = filename:pathtype("/usr/local/bin"),
            ok
    end.

rootname(Config) when is_list(Config) ->
    "/jam.src/kalle" = filename:rootname("/jam.src/kalle"),
    "/jam.src/foo" = filename:rootname("/jam.src/foo.erl"),
    "/jam.src/foo" = filename:rootname(["/ja",'m.sr',"c/foo.erl"]),
    "/jam.src/foo" = filename:rootname("/jam.src/foo.erl", ".erl"),
    "/jam.src/foo.jam" = filename:rootname("/jam.src/foo.jam", ".erl"),
    "/jam.src/foo.jam" = filename:rootname(["/jam.sr",'c/foo.j',"am"],".erl"),
    "/jam.src/foo.jam" = filename:rootname(["/jam.sr",'c/foo.j'|am],".erl"),
    ok.

split(Config) when is_list(Config) ->
    ["/","usr","local","bin"] = filename:split("/usr/local/bin"),
    ["foo","bar"]= filename:split("foo/bar"),
    ["foo", "bar", "hello"]= filename:split("foo////bar//hello"),
    ["foo", "bar", "hello"]= filename:split(["foo//",'//bar//h',"ello"]),
    ["foo", "bar", "hello"]= filename:split(["foo//",'//bar//h'|ello]),
    ["/"] = filename:split("/"),
    [] = filename:split(""),
    case os:type() of
        {win32,_} ->
            ["a:/","msdev","include"] =
                filename:split("a:/msdev/include"),
            ["a:/","msdev","include"] =
                filename:split("A:/msdev/include"),
            ["msdev","include"] =
                filename:split("msdev\\include"),
            ["a:/","msdev","include"] =
                filename:split("a:\\msdev\\include"),
            ["a:","msdev","include"] =
                filename:split("a:msdev\\include"),
            ["//","foo"] =
                filename:split("\\\\foo"),
            ["//","foo"] =
                filename:split("//foo"),
            ["//","foo","bar"] =
                filename:split("\\\\foo\\\\bar"),
            ["//","foo","baz"] =
                filename:split("\\\\foo\\baz"),
            ["//","foo","baz"] =
                filename:split("//foo\\baz"),
            ok;
        _ ->
	    ok
    end.

t_nativename(Config) when is_list(Config) ->
    "abcedf" = filename:nativename(abcedf),
    "abcedf" = filename:nativename(["abc", "edf"]),
    "abcgluff" = filename:nativename(["abc", gluff]),
    case os:type() of
        {win32, _} ->
            "a:\\temp\\arne.exe" =
                filename:nativename("A:/temp//arne.exe/");
        _ ->
            "/usr/tmp/arne" =
                filename:nativename("/usr/tmp//arne/")
    end.

find_src(Config) when is_list(Config) ->
    {Source,_} = filename:find_src(file),
    ["file"|_] = lists:reverse(filename:split(Source)),
    {Source,_} = filename:find_src(file, [{"",""}, {"ebin","src"}]),
    {Source,_} = filename:find_src(Source),
    {Source,_} = filename:find_src(Source ++ ".erl"),

    %% Try to find the source for a preloaded module.
    {error,{preloaded,init}} = filename:find_src(init),

    %% Make sure that find_src works for a slim BEAM file.
    OldPath = code:get_path(),
    try
        PrivDir = proplists:get_value(priv_dir, Config),
        code:add_patha(PrivDir),
        Src = "simple",
        SrcPath = filename:join(PrivDir, Src) ++ ".erl",
        SrcContents = "-module(simple).\n",
        ok = file:write_file(SrcPath, SrcContents),
        {ok,simple} = compile:file(SrcPath, [slim,{outdir,PrivDir}]),
        BeamPath = filename:join(PrivDir, Src),
        {BeamPath,[]} = filename:find_src(simple)
    after
        code:set_path(OldPath)
    end,
    ok.

%%
%%
%% With binaries
%%
%%

absname_bin(Config) when is_list(Config) ->
    case os:type() of
        {win32, _} -> 
            [Drive|_] = proplists:get_value(priv_dir, Config),
            Temp = filename:join([Drive|":/"], "temp"),
            case file:make_dir(Temp) of
                ok -> ok;
                {error,eexist} -> ok
            end,
            {ok,Cwd} = file:get_cwd(),
            ok = file:set_cwd(Temp),
            <<Drive:8,":/temp/foo">> = filename:absname(<<"foo">>),
            <<Drive:8,":/temp/../ebin">> = filename:absname(<<"../ebin">>),
            <<Drive:8,":/erlang">> = filename:absname(<<"/erlang">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<"/erlang/src">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<"\\erlang\\src">>),
            <<Drive:8,":/temp/erlang">> = filename:absname(<<Drive:8,":erlang">>),
            <<Drive:8,":/temp/erlang/src">> =
            filename:absname(<<Drive:8,":erlang/src">>),
            <<Drive:8,":/temp/erlang/src">> =
            filename:absname(<<Drive:8,":erlang\\src\\">>),
            <<"a:/erlang">> = filename:absname(<<"a:erlang">>),

            file:set_cwd(<<Drive:8,":/">>),
            <<Drive:8,":/foo">> = filename:absname(<<"foo">>),
            <<Drive:8,":/../ebin">> = filename:absname(<<"../ebin">>),
            <<Drive:8,":/erlang">> = filename:absname(<<"/erlang">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<"/erlang/src">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<"\\erlang\\\\src">>),
            <<Drive:8,":/erlang">> = filename:absname(<<Drive:8,":erlang">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<Drive:8,":erlang/src">>),
            <<"a:/erlang">> = filename:absname(<<"a:erlang">>),

            file:set_cwd(Cwd),
            ok;
        {unix,_} ->
            ok = file:set_cwd(<<"/usr">>),
            <<"/usr/foo">> = filename:absname(<<"foo">>),
            <<"/usr/../ebin">> = filename:absname(<<"../ebin">>),

            file:set_cwd(<<"/">>),
            <<"/foo">> = filename:absname(<<"foo">>),
            <<"/../ebin">> = filename:absname(<<"../ebin">>),
            <<"/erlang">> = filename:absname(<<"/erlang">>),
            <<"/erlang/src">> = filename:absname(<<"/erlang/src">>),
            <<"/erlang/src">> = filename:absname(<<"/erlang///src">>),
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

absname_bin_2(Config) when is_list(Config) ->
    case os:type() of
        {win32, _} ->
            [Drive|_] = proplists:get_value(priv_dir, Config),
            <<Drive:8,":/temp/foo">> = filename:absname(<<"foo">>, <<Drive:8,":/temp">>),
            <<Drive:8,":/temp/../ebin">> = filename:absname(<<"../ebin">>,
                                                            <<Drive:8,":/temp">>),
            <<Drive:8,":/erlang">> = filename:absname(<<"/erlang">>, <<Drive:8,":/temp">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<"/erlang/src">>,
                                                          <<Drive:8,":/temp">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<"\\erlang\\src">>,
                                                          <<Drive:8,":/temp">>),
            <<Drive:8,":/temp/erlang">> = filename:absname(<<Drive:8,":erlang">>,
                                                           <<Drive:8,":/temp">>),
            <<Drive:8,":/temp/erlang/src">> = filename:absname(<<Drive:8,":erlang/src">>,
                                                               <<Drive:8,":/temp">>),
            <<Drive:8,":/temp/erlang/src">> =
                filename:absname(<<Drive:8,":erlang\\src\\">>, <<Drive:8,":/temp">>),
            <<"a:/erlang">> = filename:absname(<<"a:erlang">>, <<Drive:8,":/temp">>),

            file:set_cwd(<<Drive:8,":/">>),
            <<Drive:8,":/foo">> = filename:absname(foo, <<Drive:8,":/">>),
            <<Drive:8,":/foo">> = filename:absname(<<"foo">>, <<Drive:8,":/">>),
            <<Drive:8,":/../ebin">> = filename:absname(<<"../ebin">>, <<Drive:8,":/">>),
            <<Drive:8,":/erlang">> = filename:absname(<<"/erlang">>, <<Drive:8,":/">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<"/erlang/src">>,
                                                          <<Drive:8,":/">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<"\\erlang\\\\src">>,
                                                          <<Drive:8,":/">>),
            <<Drive:8,":/erlang">> = filename:absname(<<Drive:8,":erlang">>,
                                                      <<Drive:8,":/">>),
            <<Drive:8,":/erlang/src">> = filename:absname(<<Drive:8,":erlang/src">>,
                                                          <<Drive:8,":/">>),
            <<"a:/erlang">> = filename:absname(<<"a:erlang">>, <<Drive:8,":/">>),

            ok;
        _ ->
            <<"/usr/foo">> = filename:absname(<<"foo">>, <<"/usr">>),
            <<"/usr/../ebin">> = filename:absname(<<"../ebin">>, <<"/usr">>),
            <<"/foo">> = filename:absname(<<"foo">>, <<"/">>),
            <<"/../ebin">> = filename:absname(<<"../ebin">>, <<"/">>),
            <<"/erlang">> = filename:absname(<<"/erlang">>, <<"/">>),
            <<"/erlang/src">> = filename:absname(<<"/erlang/src">>, <<"/">>),
            <<"/erlang/src">> = filename:absname(<<"/erlang///src">>, <<"/">>),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basename_bin_1(Config) when is_list(Config) ->
    <<".">> = filename:basename(<<".">>),
    <<"foo">> = filename:basename(<<"foo">>),
    <<"foo">> = filename:basename(<<"/usr/foo">>),
    <<"foo.erl">> = filename:basename(<<"A:usr/foo.erl">>),
    case os:type() of
        {win32, _} ->
            <<"foo">> = filename:basename(<<"A:\\usr\\foo">>),
            <<"foo">> = filename:basename(<<"A:foo">>);
        _ ->
            <<"strange\\but\\true">> = filename:basename(<<"strange\\but\\true">>)
    end,
    ok.

basename_bin_2(Config) when is_list(Config) ->
    <<".">> = filename:basename(<<".">>, <<".erl">>),
    <<"foo">> = filename:basename(<<"foo.erl">>, <<".erl">>),
    <<"foo.erl">> = filename:basename(<<"/usr/foo.erl">>, <<".hrl">>),
    <<"foo.erl">> = filename:basename(<<"/usr.hrl/foo.erl">>, <<".hrl">>),
    <<"foo">> = filename:basename(<<"/usr.hrl/foo">>, <<".hrl">>),
    <<"foo">> = filename:basename(<<"usr/foo/">>, <<".erl">>),
    <<"foo.erl">> = filename:basename(<<"usr/foo.erl/">>, <<".erl">>),
    case os:type() of
        {win32, _} ->
            <<"foo">> = filename:basename(<<"A:foo">>, <<".erl">>),
            <<"foo.erl">> = filename:basename(<<"a:\\usr\\foo.erl">>, <<".hrl">>),
            <<"foo.erl">> = filename:basename(<<"c:\\usr.hrl\\foo.erl">>, <<".hrl">>),
            <<"foo">> = filename:basename(<<"A:\\usr\\foo">>, <<".hrl">>);
        _ ->
            <<"strange\\but\\true">> =
                filename:basename(<<"strange\\but\\true.erl">>, <<".erl">>),
            <<"strange\\but\\true">> =
                filename:basename(<<"strange\\but\\true">>, <<".erl">>)
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dirname_bin(Config) when is_list(Config) ->
    case os:type() of
        {win32,_} ->
            <<"A:/usr">> = filename:dirname(<<"A:/usr/foo.erl">>),
            <<"A:usr">> = filename:dirname(<<"A:usr/foo.erl">>),
            <<"/usr">> = filename:dirname(<<"\\usr\\foo.erl">>),
            <<"/">> = filename:dirname(<<"\\usr">>),
            <<"A:">> = filename:dirname(<<"A:">>);
        _ -> true
    end,
    <<"usr">> = filename:dirname(<<"usr///foo.erl">>),
    <<".">> = filename:dirname(<<"foo.erl">>),
    <<".">> = filename:dirname(<<".">>),
    <<"/">> = filename:dirname(<<"/">>),
    <<"/">> = filename:dirname(<<"/usr">>),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extension_bin(Config) when is_list(Config) ->
    <<".erl">> = filename:extension(<<"A:/usr/foo.erl">>),
    <<".erl">> = filename:extension(<<"A:/usr/foo.nisse.erl">>),
    <<".erl">> = filename:extension(<<"A:/usr.bar/foo.nisse.erl">>),
    <<"">> = filename:extension(<<"A:/usr.bar/foo">>),
    <<"">> = filename:extension(<<"A:/usr/foo">>),
    case os:type() of
        {win32, _} ->
            <<"">> = filename:extension(<<"A:\\usr\\foo">>),
            <<".erl">> = filename:extension(<<"A:/usr.bar/foo.nisse.erl">>),
            <<"">> = filename:extension(<<"A:/usr.bar/foo">>),
            ok;
        _ -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
join_bin(Config) when is_list(Config) ->
    <<"/">> = filename:join([<<"/">>]),
    <<"usr/foo.erl">> = filename:join(<<"usr">>,<<"foo.erl">>),
    <<"/src/foo.erl">> = filename:join(usr, <<"/src/foo.erl">>),
    <<"/src/foo.erl">> = filename:join([<<"/src/">>,'foo.erl']),
    <<"/src/foo.erl">> = filename:join(<<"usr">>, ["/sr", 'c/foo.erl']),
    <<"/src/foo.erl">> = filename:join(<<"usr">>, <<"/src/foo.erl">>),

    %% Make sure that redundant slashes work too.
    <<"a/b/c/d/e/f/g">> = filename:join([<<"a//b/c/////d//e/f/g">>]),
    <<"a/b/c/d/e/f/g">> = filename:join([<<"a//b/c/">>, <<"d//e/f/g">>]),
    <<"a/b/c/d/e/f/g">> = filename:join([<<"a//b/c">>, <<"d//e/f/g">>]),
    <<"/d/e/f/g">> = filename:join([<<"a//b/c">>, <<"/d//e/f/g">>]),

    <<"foo/bar">> = filename:join([$f,$o,$o,$/,[]], <<"bar">>),

    %% Single dots - should be removed if in the middle of the path,
    %% but not at the end of the path.
    %% Also test equivalence between join/1 and join/2 (OTP-12158)
    <<"/.">> = filename:join([<<"/.">>]),
    <<"/">> = filename:join([<<"/./">>]),
    <<"/.">> = filename:join([<<"/./.">>]),
    <<"./.">> = filename:join([<<"./.">>]),

    <<"/a/b">> = filename:join([<<"/a/.">>,<<"b">>]),
    <<"/a/b">> = filename:join(<<"/a/.">>,<<"b">>),

    <<"/a/b/.">> = filename:join([<<"/a/.">>,<<"b/.">>]),
    <<"/a/b/.">> = filename:join(<<"/a/.">>,<<"b/.">>),

    <<"/a/.">> = filename:join([<<"/a/.">>,<<".">>]),
    <<"/a/.">> = filename:join(<<"/a/.">>,<<".">>),

    <<"/a/.">> = filename:join([<<"/a">>,<<".">>]),
    <<"/a/.">> = filename:join(<<"/a">>,<<".">>),

    <<"/a/.">> = filename:join([<<"/a/.">>,<<"">>]),
    <<"/a/.">> = filename:join(<<"/a/.">>,<<"">>),

    <<"./.">> = filename:join([<<"./.">>,<<".">>]),
    <<"./.">> = filename:join(<<"./.">>,<<".">>),

    <<"./.">> = filename:join([<<"./">>,<<".">>]),
    <<"./.">> = filename:join(<<"./">>,<<".">>),

    <<"./.">> = filename:join([<<"./.">>,<<"">>]),
    <<"./.">> = filename:join(<<"./.">>,<<"">>),

    <<".">> = filename:join([<<".">>,<<"">>]),
    <<".">> = filename:join(<<".">>,<<"">>),

    <<"./.">> = filename:join([<<".">>,<<".">>]),
    <<"./.">> = filename:join(<<".">>,<<".">>),

    %% Trailing slash shall be removed - except the root
    <<"/">> = filename:join([<<"/">>]),
    <<"/">> = filename:join([<<"/./">>]),
    <<"/a">> = filename:join([<<"/a/">>]),
    <<"/b">> = filename:join([<<"/a/">>,<<"/b/">>]),
    <<"/b">> = filename:join(<<"/a/">>,<<"/b/">>),
    <<"/a/b">> = filename:join([<<"/a/">>,<<"b/">>]),
    <<"/a/b">> = filename:join(<<"/a/">>,<<"b/">>),

    case os:type() of
        {win32, _} ->
            <<"//">> = filename:join([<<"//">>]),
            <<"d:/">> = filename:join([<<"D:/">>]),
            <<"d:/">> = filename:join([<<"D:\\">>]),
            <<"d:/abc">> = filename:join([<<"D:/">>, <<"abc">>]),
            <<"d:abc">> = filename:join([<<"D:">>, <<"abc">>]),
            <<"a/b/c/d/e/f/g">> = filename:join([<<"a//b\\c//\\/\\d/\\e/f\\g">>]),
            <<"a:usr/foo.erl">> = filename:join([<<"A:">>,<<"usr">>,<<"foo.erl">>]),
            <<"/usr/foo.erl">> = filename:join([<<"A:">>,<<"/usr">>,<<"foo.erl">>]),
            <<"c:usr">> = filename:join(<<"A:">>,<<"C:usr">>),
            <<"a:usr">> = filename:join(<<"A:">>,<<"usr">>),
            <<"c:/usr">> = filename:join(<<"A:">>, <<"C:/usr">>),
            <<"c:/usr/foo.erl">> = filename:join([<<"A:">>,<<"C:/usr">>,<<"foo.erl">>]),
            <<"c:usr/foo.erl">> = filename:join([<<"A:">>,<<"C:usr">>,<<"foo.erl">>]),
            <<"d:/foo">> = filename:join([$D, $:, $/, []], <<"foo">>),
            <<"//">> = filename:join(<<"\\\\">>, <<"">>),
            <<"//foo">> = filename:join(<<"\\\\">>, <<"foo">>),
            <<"//foo/bar">> = filename:join(<<"\\\\">>, <<"foo\\\\bar">>),
            <<"//foo/bar/baz">> = filename:join(<<"\\\\foo">>, <<"bar\\\\baz">>),
            <<"//bar/baz">> = filename:join(<<"\\\\foo">>, <<"\\\\bar\\baz">>),
            <<"//foo/bar/baz">> = filename:join(<<"\\\\foo\\bar">>, baz),
            <<"//foo/\bar/baz">> = filename:join(<<"\\\\foo/\bar">>, baz),
            <<"//foo/bar/baz">> = filename:join(<<"\\\\foo/bar">>, baz),
            <<"//bar/baz">> = filename:join(<<"\\\\foo">>, <<"\\\\bar\\baz">>),
            <<"//bar/baz">> = filename:join(<<"\\\\foo">>, <<"//bar\\baz">>),
            <<"//bar/baz">> = filename:join(<<"\\\\foo">>, <<"//bar/baz">>),
            <<"//bar/baz">> = filename:join(<<"\\\\foo">>, <<"\\\\bar/baz">>),
            <<"//d/e/f/g">> = filename:join([<<"a//b/c">>, <<"//d//e/f/g">>]),
            <<"//">> = filename:join(<<"//">>, <<"">>),
            <<"//foo">> = filename:join(<<"//">>, <<"foo">>),
            <<"//foo/bar">> = filename:join(<<"//">>, <<"foo\\\\bar">>),
            <<"//foo/bar/baz">> = filename:join(<<"//foo">>, <<"bar\\\\baz">>),
            <<"//bar/baz">> = filename:join(<<"//foo">>, <<"\\\\bar\\baz">>),
            <<"//foo/bar/baz">> = filename:join(<<"//foo\\bar">>, baz),
            <<"//foo/\bar/baz">> = filename:join(<<"//foo/\bar">>, baz),
            <<"//foo/bar/baz">> = filename:join(<<"//foo/bar">>, baz),
            <<"//bar/baz">> = filename:join(<<"//foo">>, <<"\\\\bar\\baz">>),
            <<"//bar/baz">> = filename:join(<<"//foo">>, <<"//bar\\baz">>),
            <<"//bar/baz">> = filename:join(<<"//foo">>, <<"//bar/baz">>),
            <<"//bar/baz">> = filename:join(<<"//foo">>, <<"\\\\bar/baz">>),
            ok;
        _ ->
            <<"/">> = filename:join([<<"//">>]),
            <<"/d/e/f/g">> = filename:join([<<"a//b/c">>, <<"//d//e/f/g">>]),
            ok
    end.

pathtype_bin(Config) when is_list(Config) ->
    relative = filename:pathtype(<<"..">>),
    relative = filename:pathtype(<<"foo">>),
    relative = filename:pathtype(<<"foo/bar">>),
    relative = filename:pathtype('foo/bar'),
    case os:type() of
        {win32, _} ->
            volumerelative = filename:pathtype(<<"/usr/local/bin">>),
            volumerelative = filename:pathtype(<<"A:usr/local/bin">>),
            ok;
        _ ->
            absolute = filename:pathtype(<<"/">>),
            absolute = filename:pathtype(<<"/usr/local/bin">>),
            ok
    end.

rootname_bin(Config) when is_list(Config) ->
    <<"/jam.src/kalle">> = filename:rootname(<<"/jam.src/kalle">>),
    <<"/jam.src/foo">> = filename:rootname(<<"/jam.src/foo.erl">>),
    <<"/jam.src/foo">> = filename:rootname(<<"/jam.src/foo.erl">>, <<".erl">>),
    <<"/jam.src/foo.jam">> = filename:rootname(<<"/jam.src/foo.jam">>, <<".erl">>),
    <<"/jam.src/foo.jam">> = filename:rootname(["/jam.sr",'c/foo.j',"am"],<<".erl">>),
    <<"/jam.src/foo.jam">> = filename:rootname(["/jam.sr",'c/foo.j'|am],<<".erl">>),
    ok.

split_bin(Config) when is_list(Config) ->
    [<<"/">>,<<"usr">>,<<"local">>,<<"bin">>] = filename:split(<<"/usr/local/bin">>),
    [<<"foo">>,<<"bar">>]= filename:split(<<"foo/bar">>),
    [<<"foo">>, <<"bar">>, <<"hello">>]= filename:split(<<"foo////bar//hello">>),
    [<<"/">>] = filename:split(<<"/">>),
    [] = filename:split(<<"">>),
    case os:type() of
        {win32,_} ->
            [<<"a:/">>,<<"msdev">>,<<"include">>] =
                filename:split(<<"a:/msdev/include">>),
            [<<"a:/">>,<<"msdev">>,<<"include">>] =
                filename:split(<<"A:/msdev/include">>),
            [<<"msdev">>,<<"include">>] =
                filename:split(<<"msdev\\include">>),
            [<<"a:/">>,<<"msdev">>,<<"include">>] =
                filename:split(<<"a:\\msdev\\include">>),
            [<<"a:">>,<<"msdev">>,<<"include">>] =
                filename:split(<<"a:msdev\\include">>),
            [<<"//">>,<<"foo">>] =
                filename:split(<<"\\\\foo">>),
            [<<"//">>,<<"foo">>] =
                filename:split(<<"//foo">>),
            [<<"//">>,<<"foo">>,<<"bar">>] =
                filename:split(<<"\\\\foo\\\\bar">>),
            [<<"//">>,<<"foo">>,<<"baz">>] =
                filename:split(<<"\\\\foo\\baz">>),
            [<<"//">>,<<"foo">>,<<"baz">>] =
                filename:split(<<"//foo\\baz">>),
            ok;
        _ ->
            ok
    end.

t_nativename_bin(Config) when is_list(Config) ->
    <<"abcedf">> = filename:nativename(<<"abcedf">>),
    case os:type() of
        {win32, _} ->
            <<"a:\\temp\\arne.exe">> =
                filename:nativename(<<"A:/temp//arne.exe/">>);
        _ ->
            <<"/usr/tmp/arne">> =
                filename:nativename(<<"/usr/tmp//arne/">>)
    end.

safe_relative_path(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Root = filename:join(PrivDir, ?FUNCTION_NAME),
    ok = file:make_dir(Root),
    ok = file:set_cwd(Root),

    ok = file:make_dir("a"),
    ok = file:set_cwd("a"),
    ok = file:make_dir("b"),
    ok = file:set_cwd("b"),
    ok = file:make_dir("c"),

    ok = file:set_cwd(Root),

    "a" = test_srp("a"),
    "a/b" = test_srp("a/b"),
    "a/b" = test_srp("a/./b"),
    "a/b" = test_srp("a/./b/."),

    "" = test_srp("a/.."),
    "" = test_srp("a/./.."),
    "" = test_srp("a/../."),
    "a" = test_srp("a/b/.."),
    "a" = test_srp("a/../a"),
    "a" = test_srp("a/../a/../a"),
    "a/b/c" = test_srp("a/../a/b/c"),

    unsafe = test_srp("a/../.."),
    unsafe = test_srp("a/../../.."),
    unsafe = test_srp("a/./../.."),
    unsafe = test_srp("a/././../../.."),
    unsafe = test_srp("a/b/././../../.."),

    unsafe = test_srp(PrivDir),                 %Absolute path.

    ok.

test_srp(RelPath) ->
    Res = do_test_srp(RelPath),
    Res = case do_test_srp(list_to_binary(RelPath)) of
              Bin when is_binary(Bin) ->
                  binary_to_list(Bin);
              Other ->
                  Other
          end.

do_test_srp(RelPath) ->
    {ok,Root} = file:get_cwd(),
    ok = file:set_cwd(RelPath),
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(Root),
    case filename:safe_relative_path(RelPath) of
        unsafe ->
            true = length(Cwd) < length(Root),
            unsafe;
        "" ->
            "";
        SafeRelPath ->
            ok = file:set_cwd(SafeRelPath),
            {ok,Cwd} = file:get_cwd(),
            true = length(Cwd) >= length(Root),
            ok = file:set_cwd(Root),
            SafeRelPath
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% basedirs
t_basedir_api(Config) when is_list(Config) ->
    true = is_list(filename:basedir(site_data, "My App")),
    true = is_list(filename:basedir(site_config, "My App")),
    true = is_list(filename:basedir(user_data, "My App")),
    true = is_list(filename:basedir(user_log, "My App")),
    true = is_list(filename:basedir(user_config, "My App")),
    true = is_list(filename:basedir(user_cache, "My App")),

    true = is_list(filename:basedir(site_data, <<"My App">>)),
    true = is_list(filename:basedir(site_config, <<"My App">>)),
    true = is_binary(filename:basedir(user_data, <<"My App">>)),
    true = is_binary(filename:basedir(user_log, <<"My App">>)),
    true = is_binary(filename:basedir(user_config, <<"My App">>)),
    true = is_binary(filename:basedir(user_cache, <<"My App">>)),

    %% simulate for windows
    case os:type() of
        {win32,_} -> ok;
        _ ->
            os:putenv("APPDATA", "C:\\Documents and Settings\\otptest\\Application Data")
    end,

    true = is_list(filename:basedir(site_data, "My App", #{})),
    true = is_list(filename:basedir(site_config, "My App", #{os=>linux})),
    true = is_list(filename:basedir(user_data, "My App", #{os=>darwin})),
    true = is_list(filename:basedir(user_log, "My App", #{os=>windows})),
    true = is_list(filename:basedir(user_config, "My App",#{author=>"Erl"})),
    true = is_list(filename:basedir(user_config, "My App",#{os=>darwin,
                                                            author=>"Erl"})),
    true = is_list(filename:basedir(user_config, "My App",#{os=>linux,
                                                            author=>"Erl"})),
    true = is_list(filename:basedir(user_cache, "My App",#{os=>windows,
                                                           author=>"Erl"})),
    true = is_list(filename:basedir(user_config, "My App",#{os=>darwin,
                                                            author=>"Erla",
                                                            version=>"1.0"})),
    true = is_list(filename:basedir(user_config, "My App",#{os=>linux,
                                                            version=>"2.0.1",
                                                            author=>"Erl"})),
    true = is_list(filename:basedir(user_cache, "My App",#{os=>windows,
                                                           version=>"3.1.2",
                                                           author=>"Erl"})),
    true = is_binary(filename:basedir(user_config, "My App",#{os=>darwin,
                                                              author=>"Erla",
                                                              version=><<"1.0">>})),
    true = is_binary(filename:basedir(user_config, "My App",#{os=>windows,
                                                              version=>"2.0.1",
                                                              author=><<"Erl">>})),
    true = is_binary(filename:basedir(user_cache, "My App",#{os=>linux,
                                                             version=><<"3.1.2">>,
                                                             author=>"Erl"})),
    %% simulate for windows
    case os:type() of
        {win32,_} -> ok;
        _ -> os:unsetenv("APPDATA")
    end,

    {'EXIT', _} = (catch filename:basedir(wrong_config, "My App")),
    {'EXIT', _} = (catch filename:basedir(user_cache, {bad,name})),
    {'EXIT', _} = (catch filename:basedir(user_cache, "My App", badopts)),
    {'EXIT', _} = (catch filename:basedir(user_cache, "My App", [])),
    ok.

t_basedir_windows(Config) when is_list(Config) ->
    Types = [user_data,user_log,user_config,user_cache],
    case os:type() of
        {win32,_} ->
            ok = check_basedir_windows(Types, #{});
        _ ->
            %% Windows 7 and beyond
            os:putenv("APPDATA", "C:\\Users\\otptest\\AppData\\Roaming"),
            os:putenv("LOCALAPPDATA", "C:\\Users\\otptest\\AppData\\Local"),
            io:format("APPDATA ~p~n", [os:getenv("APPDATA")]),
            io:format("LOCALAPPDATA ~p~n", [os:getenv("LOCALAPPDATA")]),
            ok = check_basedir_windows(Types,#{os=>windows}),
            %% Windows XP
            os:unsetenv("LOCALAPPDATA"),
            os:putenv("APPDATA", "C:\\Documents and Settings\\otptest\\Application Data"),
            io:format("APPDATA ~p~n", [os:getenv("APPDATA")]),
            io:format("APPLOCALDATA ~p~n", [os:getenv("APPLOCALDATA")]),
            ok = check_basedir_windows(Types,#{os=>windows}),
            os:unsetenv("APPDATA")
    end,
    ok.

check_basedir_windows([],_) -> ok;
check_basedir_windows([Type|Types],Opt) ->
    Name = "Some Application",
    io:format("type: ~p~n", [Type]),
    ok = check_basedir_windows_path(Type,
                                    [Name],
                                    filename:basedir(Type, Name, Opt)),
    ok = check_basedir_windows_path(Type,
                                    ["Erl",Name],
                                    filename:basedir(Type, Name, Opt#{author=>"Erl"})),
    ok = check_basedir_windows_path(Type,
                                    [Name,"1.0"],
                                    filename:basedir(Type, Name, Opt#{version=>"1.0"})),
    ok = check_basedir_windows_path(Type,
                                    ["Erl",Name,"1.0"],
                                    filename:basedir(Type, Name, Opt#{author=>"Erl",
                                                                      version=>"1.0"})),
    check_basedir_windows(Types, Opt).

check_basedir_windows_path(Type,Check0,Basedir) ->
    BDR = lists:reverse(filename:split(Basedir)),
    Check = lists:reverse(Check0),
    io:format("~w: ~p ~p~n", [Type,Check,BDR]),
    case Type of
        user_log -> check_basedir_windows_path_split(["Logs"|Check],BDR);
        user_cache -> check_basedir_windows_path_split(["Cache"|Check],BDR);
        _ -> check_basedir_windows_path_split(Check,BDR)
    end.

check_basedir_windows_path_split([],_) -> ok;
check_basedir_windows_path_split([Same|Check],[Same|BDR]) ->
    check_basedir_windows_path_split(Check,BDR).


t_basedir_xdg(Config) when is_list(Config) ->
    check_basedir_xdg([user_data,user_log,user_config,user_cache,
                       site_data,site_config]),
    ok.

check_basedir_xdg([]) -> ok;
check_basedir_xdg([Type|Types]) ->
    Name = "some_app",
    Opt  = #{os=>linux},
    Key  = basedir_xdg_env(Type),
    io:format("type: ~p~n", [Type]),
    Home = os:getenv("HOME"),
    NDir = "/some/absolute/path",
    DefPath = basedir_xdg_def(Type,Home,Name),
    EnvPath = case Type of
                  user_log    -> filename:join([NDir,Name,"log"]);
                  site_data   -> [filename:join([NDir,Name])];
                  site_config -> [filename:join([NDir,Name])];
                  _           -> filename:join([NDir,Name])
              end,
    os:unsetenv(Key),
    ok = check_basedir(Type, DefPath, filename:basedir(Type, Name, Opt)),
    os:putenv(Key, NDir),
    ok = check_basedir(Type, EnvPath, filename:basedir(Type, Name, Opt)),
    os:unsetenv(Key),
    ok = check_basedir(Type, DefPath, filename:basedir(Type, Name, Opt)),
    check_basedir_xdg(Types).

check_basedir(Type, Path, Basedir) ->
    io:format("~w: ~p = ~p~n", [Type,Path,Basedir]),
    Path = Basedir,
    ok.

basedir_xdg_env(Type) ->
    case Type of
        user_data   -> "XDG_DATA_HOME";
        user_config -> "XDG_CONFIG_HOME";
        user_cache  -> "XDG_CACHE_HOME";
        user_log    -> "XDG_CACHE_HOME";
        site_data   -> "XDG_DATA_DIRS";
        site_config -> "XDG_CONFIG_DIRS"
    end.

basedir_xdg_def(Type,Home,Name) ->
    case Type of
        user_data   -> filename:join([Home,".local","share",Name]);
        user_config -> filename:join([Home,".config",Name]);
        user_cache  -> filename:join([Home,".cache",Name]);
        user_log    -> filename:join([Home,".cache",Name,"log"]);
        site_data   -> [filename:join([Dir,Name]) ||
                        Dir <- ["/usr/local/share/","/usr/share/"]];
        site_config -> [filename:join(["/etc/xdg",Name])]
    end.
