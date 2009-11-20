%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(filename_SUITE).
-export([all/1]).
-export([absname/1, absname_2/1, 
	 basename_1/1, basename_2/1,
	 dirname/1, extension/1, join/1, t_nativename/1]).
-export([pathtype/1,rootname/1,split/1,find_src/1]).
-include("test_server.hrl").

all(suite) ->
    [absname, absname_2, basename_1, basename_2, dirname,
     extension,
     join, pathtype, rootname, split, t_nativename, find_src].

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
	{unix, _} ->  
	    ?line ok = file:set_cwd("/usr"),
	    ?line "/usr/foo" = filename:absname(foo),
	    ?line "/usr/foo" = filename:absname("foo"),
	    ?line "/usr/../ebin" = filename:absname("../ebin"),
	    
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
	    ok;
	vxworks ->
	    Test_dir = ?config(priv_dir, Config),
	    Test1 = Test_dir ++ "/foo",
	    Test2 = Test_dir ++ "/ebin",
	    ?line ok = file:set_cwd(Test_dir),
	    ?line Test1 = filename:absname(foo),
	    ?line Test1= filename:absname("foo"),
	    ?line Test2 = filename:absname("foo/../ebin"),
	    ?line "/erlang" = filename:absname("/erlang"),
	    ?line "/erlang/src" = filename:absname("/erlang/src"),
	    ?line "/erlang/src" = filename:absname(["/erlan",'g/s',"rc"]),
	    ?line "/erlang/src" = filename:absname("/erlang///src"),
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
	{unix, _} ->
	    ?line "/usr/foo" = filename:absname(foo, "/usr"),
	    ?line "/usr/foo" = filename:absname("foo", "/usr"),
	    ?line "/usr/../ebin" = filename:absname("../ebin", "/usr"),
	    
	    ?line "/foo" = filename:absname(foo, "/"),
	    ?line "/foo" = filename:absname("foo", "/"),
	    ?line "/../ebin" = filename:absname("../ebin", "/"),
	    ?line "/erlang" = filename:absname("/erlang", "/"),
	    ?line "/erlang/src" = filename:absname("/erlang/src", "/"),
	    ?line "/erlang/src" = filename:absname("/erlang///src", "/"),
	    ok;
	vxworks ->
	    ?line "/usr/foo" = filename:absname(foo, "/usr"),
	    ?line "/usr/foo" = filename:absname("foo", "/usr"),
	    ?line "/usr/ebin" = filename:absname("../ebin", "/usr"),
	    ?line "/usr/ebin" = filename:absname("../ebin", "/usr/src"),
	    ?line "/erlang" = filename:absname("/erlang", "/usr"),
	    ?line "/erlang/src" = filename:absname("/erlang/src", "/usr"),
	    ?line "/erlang/src" = filename:absname("/erlang///src", "/usr"),
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
	      {unix, _} ->
		  ?line "strange\\but\\true" =
		      filename:basename("strange\\but\\true");
	      vxworks -> 
		  ?line "foo" =  filename:basename(["usr\\foo\\"]),
		  ?line "foo" =  filename:basename("elrond:usr\\foo\\"),
		  ?line "foo" =  filename:basename("disk:/foo")
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
	      {unix, _} ->
		  ?line "strange\\but\\true" =
		      filename:basename("strange\\but\\true.erl", ".erl"),
		  ?line "strange\\but\\true" =
		      filename:basename("strange\\but\\true", ".erl");
	      vxworks ->
		  ?line "foo" = filename:basename("net:foo", ".erl"),
		  ?line "foo.erl" = filename:basename("net:\\usr\\foo.erl",
						      ".hrl"),
		  ?line "foo.erl" =
		      filename:basename("/disk0:\\usr.hrl\\foo.erl",
						      ".hrl"),
		  ?line "foo" = filename:basename("/home\\usr\\foo", ".hrl")
	  end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dirname(Config) when is_list(Config) ->
    case os:type() of
       {win32,_} ->
	    ?line "A:/usr" = filename:dirname("A:/usr/foo.erl"),
	    ?line "A:usr" = filename:dirname("A:usr/foo.erl"),
	    ?line "/usr" = filename:dirname("\\usr\\foo.erl"),
	    ?line "/" = filename:dirname("\\usr"),
	    ?line "A:" = filename:dirname("A:");
       vxworks ->
	    ?line "net:/usr" = filename:dirname("net:/usr/foo.erl"),
	    ?line "/disk0:/usr" = filename:dirname("/disk0:/usr/foo.erl"),
	    ?line "/usr" = filename:dirname("\\usr\\foo.erl"),
	    ?line "/usr" = filename:dirname("\\usr"),
	    ?line "net:" = filename:dirname("net:");
	_ -> true
    end,
    ?line "usr" = filename:dirname("usr///foo.erl"),
    ?line "." = filename:dirname("foo.erl"),
    ?line "." = filename:dirname("."),
    ?line "usr" = filename:dirname('usr/foo.erl'),
    ?line "usr" = filename:dirname(['usr','/foo.erl']),
    ?line "usr" = filename:dirname(['us','r/foo.erl']),
    ?line "usr" = filename:dirname(['usr/','/foo.erl']),
    ?line "usr" = filename:dirname(['usr/','foo.erl']),
    ?line "usr" = filename:dirname(['usr/'|'foo.erl']),
    ?line "usr" = filename:dirname(['usr/f','oo.erl']),
    case os:type() of
	vxworks -> 
	    ?line "/" = filename:dirname("/"),
	    ?line "/usr" = filename:dirname("/usr");
	_ ->
	    ?line "/" = filename:dirname("/"),
	    ?line "/" = filename:dirname("/usr")
    end,
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
	      vxworks ->
		  ?line "" = filename:extension("/disk0:\\usr\\foo"),
		  ?line ".erl" =
		      filename:extension("net:/usr.bar/foo.nisse.erl"),
		  ?line "" = filename:extension("net:/usr.bar/foo"),
		  ok;
	      _ -> ok
	  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
join(Config) when is_list(Config) ->
    ?line "/" = filename:join(["/"]),
    ?line "/" = filename:join(["//"]),
    ?line "usr/foo.erl" = filename:join("usr","foo.erl"),
    ?line "/src/foo.erl" = filename:join(usr, "/src/foo.erl"),
    ?line "/src/foo.erl" = filename:join(["/src/",'foo.erl']),
    ?line "/src/foo.erl" = filename:join(usr, ["/sr", 'c/foo.erl']),
    ?line "/src/foo.erl" = filename:join("usr", "/src/foo.erl"),

    %% Make sure that redundant slashes work too.
    ?line "a/b/c/d/e/f/g" = filename:join(["a//b/c/////d//e/f/g"]),
    ?line "a/b/c/d/e/f/g" = filename:join(["a//b/c/", "d//e/f/g"]),
    ?line "a/b/c/d/e/f/g" = filename:join(["a//b/c", "d//e/f/g"]),
    ?line "/d/e/f/g" = filename:join(["a//b/c", "/d//e/f/g"]),
    ?line "/d/e/f/g" = filename:join(["a//b/c", "//d//e/f/g"]),

    ?line "foo/bar" = filename:join([$f,$o,$o,$/,[]], "bar"),

    ?line case os:type() of
	      {win32, _} ->
		  ?line "d:/" = filename:join(["D:/"]),
		  ?line "d:/" = filename:join(["D:\\"]),
		  ?line "d:/abc" = filename:join(["D:/", "abc"]),
		  ?line "d:abc" = filename:join(["D:", "abc"]),
		  ?line "a/b/c/d/e/f/g" =
		      filename:join(["a//b\\c//\\/\\d/\\e/f\\g"]),
		  ?line "a:usr/foo.erl" =
		      filename:join(["A:","usr","foo.erl"]),
		  ?line "/usr/foo.erl" =
		      filename:join(["A:","/usr","foo.erl"]),
		  ?line "c:usr" = filename:join("A:","C:usr"),
		  ?line "a:usr" = filename:join("A:","usr"),
		  ?line "c:/usr" = filename:join("A:", "C:/usr"),
		  ?line "c:/usr/foo.erl" =
		      filename:join(["A:","C:/usr","foo.erl"]),
		  ?line "c:usr/foo.erl" =
		      filename:join(["A:","C:usr","foo.erl"]),
		  ?line "d:/foo" = filename:join([$D, $:, $/, []], "foo"),
		  ok;
	      vxworks ->
		  ?line "Net:" = filename:join(["Net:/"]),
		  ?line "net:" = filename:join(["net:\\"]),
		  ?line "net:/abc" = filename:join(["net:/", "abc"]),
		  ?line "net:/abc" = filename:join(["net:", "abc"]),
		  ?line "a/b/c/d/e/f/g" =
		      filename:join(["a//b\\c//\\/\\d/\\e/f\\g"]),
		  ?line "net:/usr/foo.erl" =
		      filename:join(["net:","usr","foo.erl"]),
		  ?line "/usr/foo.erl" =
		      filename:join(["net:","/usr","foo.erl"]),
		  ?line "/target:usr" = filename:join("net:","/target:usr"),
		  ?line "kernel:/usr" = filename:join("net:", "kernel:/usr"),
		  ?line "foo:/usr/foo.erl" =
		      filename:join(["A:","foo:/usr","foo.erl"]),
		  ?line "/disk0:usr/foo.erl" =
		      filename:join(["kalle:","/disk0:usr","foo.erl"]),
		  ?line "D:/foo" = filename:join([$D, $:, $/, []], "foo"),
		  ok;
	      {unix, _} ->
		  ok
	  end.

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
	{unix, _} ->
	    ?line absolute = filename:pathtype("/"),
	    ?line absolute = filename:pathtype("/usr/local/bin"),
	    ok;
	vxworks ->
	    ?line absolute = filename:pathtype("/usr/local/bin"),
	    ?line absolute = filename:pathtype("net:usr/local/bin"),
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
    case os:type() of 
	vxworks ->
	    ?line ["/usr","local","bin"] = filename:split("/usr/local/bin");
	_ ->
	    ?line ["/","usr","local","bin"] = filename:split("/usr/local/bin")
    end,
    ?line ["foo","bar"]= filename:split("foo/bar"),
    ?line ["foo", "bar", "hello"]= filename:split("foo////bar//hello"),
    ?line ["foo", "bar", "hello"]= filename:split(["foo//",'//bar//h',"ello"]),
    ?line ["foo", "bar", "hello"]= filename:split(["foo//",'//bar//h'|ello]),
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
	vxworks ->
	    ?line ["net:","msdev","include"] =
		filename:split("net:/msdev/include"),
	    ?line ["Target:","msdev","include"] =
		filename:split("Target:/msdev/include"),
	    ?line ["msdev","include"] =
		filename:split("msdev\\include"),
	    ?line ["/disk0:","msdev","include"] =
		filename:split("/disk0:\\msdev\\include"),
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
    ok.
