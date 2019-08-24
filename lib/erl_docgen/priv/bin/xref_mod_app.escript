#!/usr/bin/env escript
%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

%%% Find all applications and all modules given a root directory.
%%% Output an XML file that can be used for finding which application
%%% a given module belongs to.
%%%
%%% Options:
%%%
%%%  "-topdir <D>"  Applications are found under D/lib/.
%%%                 The default value is $ERL_TOP.
%%%
%%%  "-outfile <F>" Output is written onto F.
%%%                 The default value is "mod2app.xml".
%%%
%%% The output file has the following format:
%%%
%%% <?xml version="1.0"?>
%%% <mod2app>
%%%    <module name="ModName1">AppName1</module>
%%%    ...
%%% <mod2app>
%%%
%%% meaning that module ModName1 resides in application AppName1.

main(Args) ->
    case catch parse(Args, os:getenv("ERL_TOP"), "mod2app.xml") of
        {ok, TopDir, OutFile} ->
            case modapp(TopDir) of
                [] ->
                    io:format("no applications found\n"),
                    halt(3);
                MA ->
                    Layout = layout(MA),
                    XML = xmerl:export_simple(Layout, xmerl_xml),
                    write_file(XML, OutFile)
            end;
        {error, Msg} ->
	    io:format("~s\n", [Msg]),
	    usage()
    end.

parse(["-topdir", TopDir | Opts], _, OutFile) ->
    parse(Opts, TopDir, OutFile);
parse(["-outfile", OutFile | Opts], TopDir, _) ->
    parse(Opts, TopDir, OutFile);
parse([], TopDir, OutFile) ->
    {ok, TopDir, OutFile};
parse([Opt | _], _, _) ->
    {error, io_lib:format("Bad option: ~p", [Opt])}.

usage() ->
    io:format("usage:  ~s [-topdir <dir>] [-outfile <file>]\n",
             [escript:script_name()]),
    halt(1).

modapp(TopDir) ->
    AppDirs = filelib:wildcard(filename:join([TopDir,"lib","*"])),
    AM = [appmods(D) || D <- AppDirs],
    ERTS = [preloaded(TopDir) || lists:keyfind("erts", 1, AM) =:= false],
    lists:keysort(1, [{M,A} || {A,Ms} <- ERTS++AM, M <- Ms]).

preloaded(TopDir) ->
    {"preloaded",Mods} = appmods(filename:join([TopDir,"erts","preloaded"])),
    {"erts",Mods}.

%% It's OK if too much data is generated as long as all applications
%% and all modules are mentioned.
appmods(D) ->
    ErlFiles = filelib:wildcard(filename:join([D,"src","**","*.erl"])),
    AppV = filename:basename(D),
    App = case string:rstr(AppV, "-") of
              0 -> AppV;
              P -> string:sub_string(AppV, 1, P-1)
          end,
    {App, [filename:basename(EF, ".erl") || EF <- ErlFiles]}.

-define(IND(N), lists:duplicate(N, $\s)).
-define(NL, "\n").

layout(MAL) ->
    ML = lists:append([[?IND(2),{module,[{name,M}],[A]},?NL] || {M,A} <- MAL]),
    [?NL,{mod2app,[?NL|ML]},?NL].

write_file(Text, File) ->
    case file:open(File, [write]) of
	{ok, FD} ->
	    io:put_chars(FD, Text),
	    ok = file:close(FD);
	{error, R} ->
	    R1 = file:format_error(R),
	    io:format("could not write file '~s': ~s\n", [File, R1]),
	    halt(2)
    end.
