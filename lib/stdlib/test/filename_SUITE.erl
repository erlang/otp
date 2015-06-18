%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2014. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [absname, absname_2, basename_1, basename_2, dirname,
     extension,
     join, pathtype, rootname, split, t_nativename, find_src,
     absname_bin, absname_bin_2, basename_bin_1, basename_bin_2, dirname_bin,
     extension_bin,
     join_bin, pathtype_bin, rootname_bin, split_bin, t_nativename_bin].

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

absname(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} -> 
	    ?line [Drive|_] = ?config(priv_dir, Config),
	    ?line Temp = filename:join([Drive|":/"], "temp"),
	    ?line case file:make_dir(Temp) of
		      ok -> ok;
		      {error,eexist} -> ok
		  end,
	    ?line {ok,Cwd} = file:get_cwd(),
	    ?line ok = file:set_cwd(Temp),
	    ?line [Drive|":/temp/foo"] = filename:absname(foo),
	    ?line [Drive|":/temp/foo"] = filename:absname("foo"),
	    ?line [Drive|":/temp/../ebin"] = filename:absname("../ebin"),
	    ?line [Drive|":/erlang"] = filename:absname("/erlang"),
	    ?line [Drive|":/erlang/src"] = filename:absname("/erlang/src"),
	    ?line [Drive|":/erlang/src"] = filename:absname("\\erlang\\src"),
	    ?line [Drive|":/temp/erlang"] = filename:absname([Drive|":erlang"]),
	    ?line [Drive|":/temp/erlang/src"] =
		filename:absname([Drive|":erlang/src"]),
	    ?line [Drive|":/temp/erlang/src"] =
		filename:absname([Drive|":erlang\\src\\"]),
	    ?line "a:/erlang" = filename:absname("a:erlang"),
	    
	    ?line file:set_cwd([Drive|":/"]),
	    ?line [Drive|":/foo"] = filename:absname(foo),
	    ?line [Drive|":/foo"] = filename:absname("foo"),
	    ?line [Drive|":/../ebin"] = filename:absname("../ebin"),
	    ?line [Drive|":/erlang"] = filename:absname("/erlang"),
	    ?line [Drive|":/erlang/src"] = filename:absname("/erlang/src"),
	    ?line [Drive|":/erlang/src"] = filename:absname(["/erlang",'/src']),
	    ?line [Drive|":/erlang/src"] = filename:absname("\\erlang\\\\src"),
	    ?line [Drive|":/erlang"] = filename:absname([Drive|":erlang"]),
	    ?line [Drive|":/erlang/src"] = filename:absname([Drive|":erlang/src"]),
	    ?line "a:/erlang" = filename:absname("a:erlang"),
	    
	    ?line file:set_cwd(Cwd),
	    ok;
	Type ->
	    case Type of
		{unix, _} ->
		    ?line ok = file:set_cwd("/usr"),
		    ?line "/usr/foo" = filename:absname(foo),
		    ?line "/usr/foo" = filename:absname("foo"),
		    ?line "/usr/../ebin" = filename:absname("../ebin");
		{ose, _} ->
		    ?line ok = file:set_cwd("/romfs"),
		    ?line "/romfs/foo" = filename:absname(foo),
		    ?line "/romfs/foo" = filename:absname("foo"),
		    ?line "/romfs/../ebin" = filename:absname("../ebin")
	    end,
	    
	    ?line file:set_cwd("/"),
	    ?line "/foo" = filename:absname(foo),
	    ?line "/foo" = filename:absname("foo"),
	    ?line "/../ebin" = filename:absname("../ebin"),
	    ?line "/erlang" = filename:absname("/erlang"),
	    ?line "/erlang/src" = filename:absname("/erlang/src"),
	    ?line "/erlang/src" = filename:absname(["/erl",'ang/s',"rc"]),
	    ?line "/erlang/src" = filename:absname(["/erl",'a','ng',"/",'s',"rc"]),
	    ?line "/erlang/src" = filename:absname("/erlang///src"),
	    ?line "/file_sorter.erl" = filename:absname([file_sorter|'.erl']),
	    ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

absname_2(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    ?line [Drive|_] = ?config(priv_dir, Config),
	    ?line [Drive|":/temp/foo"] = filename:absname(foo, [Drive|":/temp"]),
	    ?line [Drive|":/temp/foo"] = filename:absname("foo", [Drive|":/temp"]),
	    ?line [Drive|":/temp/../ebin"] = filename:absname("../ebin",
							      [Drive|":/temp"]),
	    ?line [Drive|":/erlang"] = filename:absname("/erlang", [Drive|":/temp"]),
	    ?line [Drive|":/erlang/src"] = filename:absname("/erlang/src",
							    [Drive|":/temp"]),
	    ?line [Drive|":/erlang/src"] = filename:absname("\\erlang\\src",
							    [Drive|":/temp"]),
	    ?line [Drive|":/temp/erlang"] = filename:absname([Drive|":erlang"],
							     [Drive|":/temp"]),
	    ?line [Drive|":/temp/erlang/src"] = filename:absname([Drive|":erlang/src"],
								 [Drive|":/temp"]),
	    ?line [Drive|":/temp/erlang/src"] =
		filename:absname([Drive|":erlang\\src\\"], [Drive|":/temp"]),
	    ?line "a:/erlang" = filename:absname("a:erlang", [Drive|":/temp"]),
	    
	    ?line file:set_cwd([Drive|":/"]),
	    ?line [Drive|":/foo"] = filename:absname(foo, [Drive|":/"]),
	    ?line [Drive|":/foo"] = filename:absname("foo", [Drive|":/"]),
	    ?line [Drive|":/../ebin"] = filename:absname("../ebin", [Drive|":/"]),
	    ?line [Drive|":/erlang"] = filename:absname("/erlang", [Drive|":/"]),
	    ?line [Drive|":/erlang/src"] = filename:absname("/erlang/src",
							    [Drive|":/"]),
	    ?line [Drive|":/erlang/src"] = filename:absname("\\erlang\\\\src",
							    [Drive|":/"]),
	    ?line [Drive|":/erlang"] = filename:absname([Drive|":erlang"],
							[Drive|":/"]),
	    ?line [Drive|":/erlang/src"] = filename:absname([Drive|":erlang/src"],
							    [Drive|":/"]),
	    ?line "a:/erlang" = filename:absname("a:erlang", [Drive|":/"]),
	    
	    ok;
	_ ->
	    ?line "/usr/foo" = filename:absname(foo, "/usr"),
	    ?line "/usr/foo" = filename:absname("foo", "/usr"),
	    ?line "/usr/../ebin" = filename:absname("../ebin", "/usr"),
	    
	    ?line "/foo" = filename:absname(foo, "/"),
	    ?line "/foo" = filename:absname("foo", "/"),
	    ?line "/../ebin" = filename:absname("../ebin", "/"),
	    ?line "/erlang" = filename:absname("/erlang", "/"),
	    ?line "/erlang/src" = filename:absname("/erlang/src", "/"),
	    ?line "/erlang/src" = filename:absname("/erlang///src", "/"),
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basename_1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line "." = filename:basename("."),
    ?line "foo" = filename:basename("foo"),
    ?line "foo" = filename:basename("/usr/foo"),
    ?line "foo.erl" = filename:basename("A:usr/foo.erl"),
    ?line "foo" = filename:basename('/usr/foo'),
    ?line "foo" = filename:basename(["/usr","/","f","o","o"]),
    ?line "foo" = filename:basename(["/usr/",foo]),
    ?line "foo" = filename:basename(["/usr/f",oo]),
    ?line "foo" = filename:basename(["usr/", "foo"]),
    ?line "foo" = filename:basename(["usr/"|foo]),
    ?line "foo" = filename:basename(["usr/foo/"]),
    ?line case os:type() of
	      {win32, _} ->
		  ?line "foo" = filename:basename(["usr\\foo\\"]),
		  ?line "foo" = filename:basename("A:\\usr\\foo"),
		  ?line "foo" = filename:basename("A:foo");
	      _ ->
		  ?line "strange\\but\\true" =
		      filename:basename("strange\\but\\true")
	  end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

basename_2(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line "." = filename:basename(".", ".erl"),
    ?line "foo" = filename:basename("foo.erl", ".erl"),
    ?line "foo" = filename:basename('foo.erl', ".erl"),
    ?line "foo" = filename:basename("foo.erl", '.erl'),
    ?line "foo" = filename:basename(["/usr","/","f","oo"], ".erl"),
    ?line "foo.erl" = filename:basename("/usr/foo.erl", ".hrl"),
    ?line "foo.erl" = filename:basename("/usr.hrl/foo.erl", ".hrl"),
    ?line "foo" = filename:basename("/usr.hrl/foo", ".hrl"),
    ?line "foo" = filename:basename("usr/foo/", ".erl"),
    ?line "foo.erl" = filename:basename("usr/foo.erl/", ".erl"),
    ?line "foo.erl" = filename:basename("usr/foo.erl/", '.erl'),
    ?line "foo" = filename:basename(["/usr",'/','f','oo'], ".erl"),
    ?line "foo.erl" = filename:basename(["usr/foo.e",'rl/'], ".erl"),
    ?line case os:type() of
	      {win32, _} ->
		  ?line "foo" = filename:basename("A:foo", ".erl"),
		  ?line "foo.erl" = filename:basename("a:\\usr\\foo.erl",
						      ".hrl"),
		  ?line "foo.erl" = filename:basename("c:\\usr.hrl\\foo.erl",
						      ".hrl"),
		  ?line "foo" = filename:basename("A:\\usr\\foo", ".hrl");
	      _ ->
		  ?line "strange\\but\\true" =
		      filename:basename("strange\\but\\true.erl", ".erl"),
		  ?line "strange\\but\\true" =
		      filename:basename("strange\\but\\true", ".erl")
	  end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dirname(Config) when is_list(Config) ->
    case os:type() of
       {win32,_} ->
	    "A:/usr" = filename:dirname("A:/usr/foo.erl"),
	    "A:usr" = filename:dirname("A:usr/foo.erl"),
	    "/usr" = filename:dirname("\\usr\\foo.erl"),
	    "/" = filename:dirname("\\usr"),
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
    ?line ".erl" = filename:extension("A:/usr/foo.erl"),
    ?line ".erl" = filename:extension("A:/usr/foo.nisse.erl"),
    ?line ".erl" = filename:extension(["A:/usr/", 'foo.ni', "sse.erl"]),
    ?line ".erl" = filename:extension(["A:/usr/", 'foo.ni', "sse.e", 'rl']),
    ?line ".erl" = filename:extension(["A:/usr/", 'foo.ni', "sse.e"|'rl']),
    ?line ".erl" = filename:extension("A:/usr.bar/foo.nisse.erl"),
    ?line "" = filename:extension("A:/usr.bar/foo"),
    ?line "" = filename:extension("A:/usr/foo"),
    ?line case os:type() of
	      {win32, _} ->
		  ?line "" = filename:extension("A:\\usr\\foo"),
		  ?line ".erl" =
		      filename:extension("A:/usr.bar/foo.nisse.erl"),
		  ?line "" = filename:extension("A:/usr.bar/foo"),
		  ok;
	      _ -> ok
	  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
join(Config) when is_list(Config) ->
    %% Whenever joining two elements, test the equivalence between
    %% join/1 and join/2 (OTP-12158) by using help function
    %% filename_join/2.
    ?line "/" = filename:join(["/"]),
    ?line "/" = filename:join(["//"]),
    "usr/foo.erl" = filename_join("usr","foo.erl"),
    "/src/foo.erl" = filename_join(usr, "/src/foo.erl"),
    "/src/foo.erl" = filename_join("/src/",'foo.erl'),
    "/src/foo.erl" = filename_join(usr, ["/sr", 'c/foo.erl']),
    "/src/foo.erl" = filename_join("usr", "/src/foo.erl"),

    %% Make sure that redundant slashes work too.
    ?line "a/b/c/d/e/f/g" = filename:join(["a//b/c/////d//e/f/g"]),
    "a/b/c/d/e/f/g" = filename_join("a//b/c/", "d//e/f/g"),
    "a/b/c/d/e/f/g" = filename_join("a//b/c", "d//e/f/g"),
    "/d/e/f/g" = filename_join("a//b/c", "/d//e/f/g"),
    "/d/e/f/g" = filename:join("a//b/c", "//d//e/f/g"),

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

    ?line case os:type() of
	      {win32, _} ->
		  ?line "d:/" = filename:join(["D:/"]),
		  ?line "d:/" = filename:join(["D:\\"]),
		  "d:/abc" = filename_join("D:/", "abc"),
		  "d:abc" = filename_join("D:", "abc"),
		  ?line "a/b/c/d/e/f/g" =
		      filename:join(["a//b\\c//\\/\\d/\\e/f\\g"]),
		  ?line "a:usr/foo.erl" =
		      filename:join(["A:","usr","foo.erl"]),
		  ?line "/usr/foo.erl" =
		      filename:join(["A:","/usr","foo.erl"]),
		  "c:usr" = filename_join("A:","C:usr"),
		  "a:usr" = filename_join("A:","usr"),
		  "c:/usr" = filename_join("A:", "C:/usr"),
		  ?line "c:/usr/foo.erl" =
		      filename:join(["A:","C:/usr","foo.erl"]),
		  ?line "c:usr/foo.erl" =
		      filename:join(["A:","C:usr","foo.erl"]),
		  ?line "d:/foo" = filename:join([$D, $:, $/, []], "foo"),
		  ok;
	      _ ->
		  ok
	  end.

%% Make sure join([A,B]) is equivalent to join(A,B) (OTP-12158)
filename_join(A,B) ->
    Res = filename:join(A,B),
    Res = filename:join([A,B]).

pathtype(Config) when is_list(Config) ->
    ?line relative = filename:pathtype(".."),
    ?line relative = filename:pathtype("foo"),
    ?line relative = filename:pathtype("foo/bar"),
    ?line relative = filename:pathtype('foo/bar'),
    ?line relative = filename:pathtype(['f','oo',"/bar"]),
    case os:type() of
	{win32, _} ->
	    ?line volumerelative = filename:pathtype("/usr/local/bin"),
	    ?line volumerelative = filename:pathtype("A:usr/local/bin"),
	    ok;
	_ ->
	    ?line absolute = filename:pathtype("/"),
	    ?line absolute = filename:pathtype("/usr/local/bin"),
	    ok
    end.

rootname(Config) when is_list(Config) ->
    ?line "/jam.src/kalle" = filename:rootname("/jam.src/kalle"),
    ?line "/jam.src/foo" = filename:rootname("/jam.src/foo.erl"),
    ?line "/jam.src/foo" = filename:rootname(["/ja",'m.sr',"c/foo.erl"]),
    ?line "/jam.src/foo" = filename:rootname("/jam.src/foo.erl", ".erl"),
    ?line "/jam.src/foo.jam" = filename:rootname("/jam.src/foo.jam", ".erl"),
    ?line "/jam.src/foo.jam" = filename:rootname(["/jam.sr",'c/foo.j',"am"],".erl"),
    ?line "/jam.src/foo.jam" = filename:rootname(["/jam.sr",'c/foo.j'|am],".erl"),
    ok.

split(Config) when is_list(Config) ->
    ?line ["/","usr","local","bin"] = filename:split("/usr/local/bin"),
    ?line ["foo","bar"]= filename:split("foo/bar"),
    ?line ["foo", "bar", "hello"]= filename:split("foo////bar//hello"),
    ?line ["foo", "bar", "hello"]= filename:split(["foo//",'//bar//h',"ello"]),
    ?line ["foo", "bar", "hello"]= filename:split(["foo//",'//bar//h'|ello]),
    ["/"] = filename:split("/"),
    [] = filename:split(""),
    case os:type() of
       {win32,_} ->
	    ?line ["a:/","msdev","include"] =
		filename:split("a:/msdev/include"),
	    ?line ["a:/","msdev","include"] =
		filename:split("A:/msdev/include"),
	    ?line ["msdev","include"] =
		filename:split("msdev\\include"),
	    ?line ["a:/","msdev","include"] =
		filename:split("a:\\msdev\\include"),
	    ?line ["a:","msdev","include"] =
		filename:split("a:msdev\\include"),
	    ok;
       _ ->
	    ok
    end.

t_nativename(Config) when is_list(Config) ->
    ?line "abcedf" = filename:nativename(abcedf),
    ?line "abcedf" = filename:nativename(["abc", "edf"]),
    ?line "abcgluff" = filename:nativename(["abc", gluff]),
    case os:type() of
	{win32, _} ->
	    ?line "a:\\temp\\arne.exe" =
		filename:nativename("A:/temp//arne.exe/");
	_ ->
	    ?line "/usr/tmp/arne" =
		filename:nativename("/usr/tmp//arne/")
    end.

find_src(Config) when is_list(Config) ->
    ?line {Source,_} = filename:find_src(file),
    ?line ["file"|_] = lists:reverse(filename:split(Source)),
    ?line {_,_} = filename:find_src(init, [{".","."}, {"ebin","src"}]),
    
    %% Try to find the source for a preloaded module.
    ?line {error,{preloaded,init}} = filename:find_src(init),

    %% Make sure that find_src works for a slim BEAM file.
    OldPath = code:get_path(),
    try
	PrivDir = ?config(priv_dir, Config),
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
	    ?line [Drive|_] = ?config(priv_dir, Config),
	    ?line Temp = filename:join([Drive|":/"], "temp"),
	    ?line case file:make_dir(Temp) of
		      ok -> ok;
		      {error,eexist} -> ok
		  end,
	    ?line {ok,Cwd} = file:get_cwd(),
	    ?line ok = file:set_cwd(Temp),
	    ?line <<Drive:8,":/temp/foo">> = filename:absname(<<"foo">>),
	    ?line <<Drive:8,":/temp/../ebin">> = filename:absname(<<"../ebin">>),
	    ?line <<Drive:8,":/erlang">> = filename:absname(<<"/erlang">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<"/erlang/src">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<"\\erlang\\src">>),
	    ?line <<Drive:8,":/temp/erlang">> = filename:absname(<<Drive:8,":erlang">>),
	    ?line <<Drive:8,":/temp/erlang/src">> =
		filename:absname(<<Drive:8,":erlang/src">>),
	    ?line <<Drive:8,":/temp/erlang/src">> =
		filename:absname(<<Drive:8,":erlang\\src\\">>),
	    ?line <<"a:/erlang">> = filename:absname(<<"a:erlang">>),
	    
	    ?line file:set_cwd(<<Drive:8,":/">>),
	    ?line <<Drive:8,":/foo">> = filename:absname(<<"foo">>),
	    ?line <<Drive:8,":/../ebin">> = filename:absname(<<"../ebin">>),
	    ?line <<Drive:8,":/erlang">> = filename:absname(<<"/erlang">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<"/erlang/src">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<"\\erlang\\\\src">>),
	    ?line <<Drive:8,":/erlang">> = filename:absname(<<Drive:8,":erlang">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<Drive:8,":erlang/src">>),
	    ?line <<"a:/erlang">> = filename:absname(<<"a:erlang">>),
	    
	    ?line file:set_cwd(Cwd),
	    ok;
	Type ->
	    case Type of
		{unix,_} ->
		    ?line ok = file:set_cwd(<<"/usr">>),
		    ?line <<"/usr/foo">> = filename:absname(<<"foo">>),
		    ?line <<"/usr/../ebin">> = filename:absname(<<"../ebin">>);
		{ose,_} ->
		    ?line ok = file:set_cwd(<<"/romfs">>),
		    ?line <<"/romfs/foo">> = filename:absname(<<"foo">>),
		    ?line <<"/romfs/../ebin">> = filename:absname(<<"../ebin">>)
	    end,
	    
	    ?line file:set_cwd(<<"/">>),
	    ?line <<"/foo">> = filename:absname(<<"foo">>),
	    ?line <<"/../ebin">> = filename:absname(<<"../ebin">>),
	    ?line <<"/erlang">> = filename:absname(<<"/erlang">>),
	    ?line <<"/erlang/src">> = filename:absname(<<"/erlang/src">>),
	    ?line <<"/erlang/src">> = filename:absname(<<"/erlang///src">>),
	    ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

absname_bin_2(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    ?line [Drive|_] = ?config(priv_dir, Config),
	    ?line <<Drive:8,":/temp/foo">> = filename:absname(<<"foo">>, <<Drive:8,":/temp">>),
	    ?line <<Drive:8,":/temp/../ebin">> = filename:absname(<<"../ebin">>,
							      <<Drive:8,":/temp">>),
	    ?line <<Drive:8,":/erlang">> = filename:absname(<<"/erlang">>, <<Drive:8,":/temp">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<"/erlang/src">>,
							    <<Drive:8,":/temp">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<"\\erlang\\src">>,
							    <<Drive:8,":/temp">>),
	    ?line <<Drive:8,":/temp/erlang">> = filename:absname(<<Drive:8,":erlang">>,
							     <<Drive:8,":/temp">>),
	    ?line <<Drive:8,":/temp/erlang/src">> = filename:absname(<<Drive:8,":erlang/src">>,
								 <<Drive:8,":/temp">>),
	    ?line <<Drive:8,":/temp/erlang/src">> =
		filename:absname(<<Drive:8,":erlang\\src\\">>, <<Drive:8,":/temp">>),
	    ?line <<"a:/erlang">> = filename:absname(<<"a:erlang">>, <<Drive:8,":/temp">>),
	    
	    ?line file:set_cwd(<<Drive:8,":/">>),
	    ?line <<Drive:8,":/foo">> = filename:absname(foo, <<Drive:8,":/">>),
	    ?line <<Drive:8,":/foo">> = filename:absname(<<"foo">>, <<Drive:8,":/">>),
	    ?line <<Drive:8,":/../ebin">> = filename:absname(<<"../ebin">>, <<Drive:8,":/">>),
	    ?line <<Drive:8,":/erlang">> = filename:absname(<<"/erlang">>, <<Drive:8,":/">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<"/erlang/src">>,
							    <<Drive:8,":/">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<"\\erlang\\\\src">>,
							    <<Drive:8,":/">>),
	    ?line <<Drive:8,":/erlang">> = filename:absname(<<Drive:8,":erlang">>,
							<<Drive:8,":/">>),
	    ?line <<Drive:8,":/erlang/src">> = filename:absname(<<Drive:8,":erlang/src">>,
							    <<Drive:8,":/">>),
	    ?line <<"a:/erlang">> = filename:absname(<<"a:erlang">>, <<Drive:8,":/">>),
	    
	    ok;
	_ ->
	    ?line <<"/usr/foo">> = filename:absname(<<"foo">>, <<"/usr">>),
	    ?line <<"/usr/../ebin">> = filename:absname(<<"../ebin">>, <<"/usr">>),
	    
	    ?line <<"/foo">> = filename:absname(<<"foo">>, <<"/">>),
	    ?line <<"/../ebin">> = filename:absname(<<"../ebin">>, <<"/">>),
	    ?line <<"/erlang">> = filename:absname(<<"/erlang">>, <<"/">>),
	    ?line <<"/erlang/src">> = filename:absname(<<"/erlang/src">>, <<"/">>),
	    ?line <<"/erlang/src">> = filename:absname(<<"/erlang///src">>, <<"/">>),
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basename_bin_1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line <<".">> = filename:basename(<<".">>),
    ?line <<"foo">> = filename:basename(<<"foo">>),
    ?line <<"foo">> = filename:basename(<<"/usr/foo">>),
    ?line <<"foo.erl">> = filename:basename(<<"A:usr/foo.erl">>),
    ?line case os:type() of
	      {win32, _} ->
		  ?line <<"foo">> = filename:basename(<<"A:\\usr\\foo">>),
		  ?line <<"foo">> = filename:basename(<<"A:foo">>);
	      _ ->
		  ?line <<"strange\\but\\true">> =
		      filename:basename(<<"strange\\but\\true">>)
	  end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

basename_bin_2(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line <<".">> = filename:basename(<<".">>, <<".erl">>),
    ?line <<"foo">> = filename:basename(<<"foo.erl">>, <<".erl">>),
    ?line <<"foo.erl">> = filename:basename(<<"/usr/foo.erl">>, <<".hrl">>),
    ?line <<"foo.erl">> = filename:basename(<<"/usr.hrl/foo.erl">>, <<".hrl">>),
    ?line <<"foo">> = filename:basename(<<"/usr.hrl/foo">>, <<".hrl">>),
    ?line <<"foo">> = filename:basename(<<"usr/foo/">>, <<".erl">>),
    ?line <<"foo.erl">> = filename:basename(<<"usr/foo.erl/">>, <<".erl">>),
    ?line case os:type() of
	      {win32, _} ->
		  ?line <<"foo">> = filename:basename(<<"A:foo">>, <<".erl">>),
		  ?line <<"foo.erl">> = filename:basename(<<"a:\\usr\\foo.erl">>,
						      <<".hrl">>),
		  ?line <<"foo.erl">> = filename:basename(<<"c:\\usr.hrl\\foo.erl">>,
						      <<".hrl">>),
		  ?line <<"foo">> = filename:basename(<<"A:\\usr\\foo">>, <<".hrl">>);
	      _ ->
		  ?line <<"strange\\but\\true">> =
		      filename:basename(<<"strange\\but\\true.erl">>, <<".erl">>),
		  ?line <<"strange\\but\\true">> =
		      filename:basename(<<"strange\\but\\true">>, <<".erl">>)
	  end,
    ?line test_server:timetrap_cancel(Dog),
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
            ?line <<"">> = filename:extension(<<"A:\\usr\\foo">>),
            ?line <<".erl">> =
                filename:extension(<<"A:/usr.bar/foo.nisse.erl">>),
            ?line <<"">> = filename:extension(<<"A:/usr.bar/foo">>),
            ok;
        _ -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
join_bin(Config) when is_list(Config) ->
    ?line <<"/">> = filename:join([<<"/">>]),
    ?line <<"/">> = filename:join([<<"//">>]),
    ?line <<"usr/foo.erl">> = filename:join(<<"usr">>,<<"foo.erl">>),
    ?line <<"/src/foo.erl">> = filename:join(usr, <<"/src/foo.erl">>),
    ?line <<"/src/foo.erl">> = filename:join([<<"/src/">>,'foo.erl']),
    ?line <<"/src/foo.erl">> = filename:join(<<"usr">>, ["/sr", 'c/foo.erl']),
    ?line <<"/src/foo.erl">> = filename:join(<<"usr">>, <<"/src/foo.erl">>),

    %% Make sure that redundant slashes work too.
    ?line <<"a/b/c/d/e/f/g">> = filename:join([<<"a//b/c/////d//e/f/g">>]),
    ?line <<"a/b/c/d/e/f/g">> = filename:join([<<"a//b/c/">>, <<"d//e/f/g">>]),
    ?line <<"a/b/c/d/e/f/g">> = filename:join([<<"a//b/c">>, <<"d//e/f/g">>]),
    ?line <<"/d/e/f/g">> = filename:join([<<"a//b/c">>, <<"/d//e/f/g">>]),
    ?line <<"/d/e/f/g">> = filename:join([<<"a//b/c">>, <<"//d//e/f/g">>]),

    ?line <<"foo/bar">> = filename:join([$f,$o,$o,$/,[]], <<"bar">>),

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

    ?line case os:type() of
	      {win32, _} ->
		  ?line <<"d:/">> = filename:join([<<"D:/">>]),
		  ?line <<"d:/">> = filename:join([<<"D:\\">>]),
		  ?line <<"d:/abc">> = filename:join([<<"D:/">>, <<"abc">>]),
		  ?line <<"d:abc">> = filename:join([<<"D:">>, <<"abc">>]),
		  ?line <<"a/b/c/d/e/f/g">> =
		      filename:join([<<"a//b\\c//\\/\\d/\\e/f\\g">>]),
		  ?line <<"a:usr/foo.erl">> =
		      filename:join([<<"A:">>,<<"usr">>,<<"foo.erl">>]),
		  ?line <<"/usr/foo.erl">> =
		      filename:join([<<"A:">>,<<"/usr">>,<<"foo.erl">>]),
		  ?line <<"c:usr">> = filename:join(<<"A:">>,<<"C:usr">>),
		  ?line <<"a:usr">> = filename:join(<<"A:">>,<<"usr">>),
		  ?line <<"c:/usr">> = filename:join(<<"A:">>, <<"C:/usr">>),
		  ?line <<"c:/usr/foo.erl">> =
		      filename:join([<<"A:">>,<<"C:/usr">>,<<"foo.erl">>]),
		  ?line <<"c:usr/foo.erl">> =
		      filename:join([<<"A:">>,<<"C:usr">>,<<"foo.erl">>]),
		  ?line <<"d:/foo">> = filename:join([$D, $:, $/, []], <<"foo">>),
		  ok;
	      _ ->
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
	    ok;
       _ ->
	    ok
    end.

t_nativename_bin(Config) when is_list(Config) ->
    ?line <<"abcedf">> = filename:nativename(<<"abcedf">>),
    case os:type() of
	{win32, _} ->
	    ?line <<"a:\\temp\\arne.exe">> =
		filename:nativename(<<"A:/temp//arne.exe/">>);
	_ ->
	    ?line <<"/usr/tmp/arne">> =
		filename:nativename(<<"/usr/tmp//arne/">>)
    end.
