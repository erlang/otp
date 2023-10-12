#!/usr/bin/env escript
%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2022. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : codeline_preprocessing.escript
%%
%% Created : 10 Sep 2008 by Lars Thorsen 
%%----------------------------------------------------------------------
-mode(compile).
%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: main/1
%% Description:
%%----------------------------------------------------------------------
main([CPath, InFile, OutFile]) ->
    InDev =
	case file:open(InFile, [read]) of
	    {ok,ID} ->
		ID;
	    _ ->
                fail(5, "Could not open ~ts for reading", [InFile])
	end,
    OutDev =
	case file:open(OutFile, [write]) of
	    {ok,OD} ->
		OD;
	    _ ->
                fail(5, "Could not open ~ts for writing", [OutFile])
	end,
    parse(InDev, OutDev, CPath);
main(_) ->
    usage().

%%======================================================================
%% Internal functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function: usage/0
%% Description:
%%----------------------------------------------------------------------
usage() ->
    fail("usage:  codeline_preprocessing.escript <infile> <outfile>\n").


%%======================================================================
%% Internal functions
%%======================================================================

parse(InDev, OutDev, CPath) ->
    case io:get_line(InDev, "") of
	eof ->
	    file:close(OutDev),
	    file:close(InDev);
	Line ->
            file:write(OutDev, parse_line(CPath, Line)),
	    parse(InDev, OutDev, CPath)
    end.


parse_line(CPath, Line) ->
    case re:run(Line, "<codeinclude(.*)(>.*</codeinclude>|/>)", [{capture, [1], list}]) of
        {match,[Attributes]} ->
            File =
                case re:run(Attributes,"file=\"([^\"]+)\"",[{capture, [1], list}]) of
                    {match, [FileMatch]} ->
                        FileMatch;
                    nomatch ->
                        fail("Did not find 'file' attribute in codeinclude")
                end,
            FileContent =
                case file:read_file(filename:join(CPath, File)) of
                    {ok, Bin} ->
                        Bin;
                    {error,Error} ->
                        fail("Could not open file ~ts (~t)~n", [File, Error])
                end,
            Type =
                case re:run(Attributes,"(type=\"[^\"]+\")",[{capture, [1], list}]) of
                    {match,[TypeMatch]} ->
                        TypeMatch;
                    nomatch ->
                        ""
                end,
            TaggedContent =
                case re:run(Attributes,"tag=\"([^\"]+)\"",[{capture, [1], list}]) of
                    {match,[Tag]} ->
                        case re:run(FileContent,"^" ++ Tag ++ "\n((.|\n)*)\n" ++
                                        Tag ++ "\$",[global, multiline, {capture, [1], binary}]) of
                            {match,[[Match]]} ->
                                Match;
                            nomatch ->
                                fail(4, "Could not find the tag ~p in ~ts",
                                     [Tag, File])
                        end;
                    nomatch ->
                        FileContent
                end,
            ["<code " ++ Type ++ ">\n<![CDATA[\n", TaggedContent, "]]></code>\n"];
        nomatch ->
            Line
    end.

%%----------------------------------------------------------------------
%% Function: get_code/2
%% Description:
%%----------------------------------------------------------------------

fail(Fmt) ->
    fail(Fmt, []).
fail(Fmt, Args) ->
    fail(1, Fmt, Args).
fail(Reason, Fmt, Args) ->
    io:format(standard_error, Fmt, Args),
    halt(Reason).
