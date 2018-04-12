#!/usr/bin/env escript
%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
		halt(5)
	end,
    OutDev = 
	case file:open(OutFile, [write]) of
	    {ok,OD} ->
		OD;
	    _ ->
		halt(5)
	end,
    case re:compile("<codeinclude(?:\040|\t)*file=\"([^\"]*)\"(?:(?:(?:\040|\t)*tag=\"([^\"]*)\".*)|(?:.*))(?:/>|/codeinclude>)") of
	{ok, Mp} ->
	    parse(InDev, OutDev, CPath, Mp);
	_ ->
	    halt(2)
    end;
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
    io:format("usage:  codeline_preprocessing.escript <infile> <outfile>\n"),
    halt(1).


%%======================================================================
%% Internal functions
%%======================================================================

parse(InDev, OutDev, CPath, Mp) ->
    case io:get_line(InDev, "") of
	eof ->
	    file:close(OutDev),
	    file:close(InDev);
	String ->
	 case re:run(String, Mp,[{capture, [1,2], list}]) of
	     {match,[File, []]} ->  
		 case file:read_file(filename:join(CPath, File))of
		     {ok, Bin} ->
			 file:write(OutDev, "<code>\n<![CDATA[\n"),
			 file:write(OutDev, Bin),
			 file:write(OutDev, "]]></code>");
		     _ ->
			 halt(3) 
		 end;
	     {match,[File, Tag]} ->
		 String2 = get_code(filename:join(CPath, File), Tag),
		 file:write(OutDev, "<code>\n<![CDATA[\n"),
		 file:write(OutDev, String2),
		 file:write(OutDev, "]]></code>");
		_ -> 
		    file:write(OutDev, String)		    
	    end,
	    parse(InDev, OutDev, CPath, Mp)
    end.
 	    
%%----------------------------------------------------------------------
%% Function: get_code/2
%% Description:
%%----------------------------------------------------------------------
get_code(File, Tag) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    case re:run(Bin,"^" ++ Tag ++ "\n((.|\n)*)\n" ++ 
			Tag ++ "\$",[global, multiline, {capture, [1], binary}]) of
		{match,[[Match]]} -> 
		    Match; 
		_ -> 
		    halt(4) 
	    end; 
	_ ->
	    halt(3) 
    end.
 

        
