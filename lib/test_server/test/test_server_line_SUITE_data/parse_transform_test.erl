%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(parse_transform_test).

-include("test_server_line.hrl").
-no_lines([{excluded,0}]).

-export([excluded/0, func/0]).


excluded() ->
    line1,
    line2,
    ok.


func() ->
    hello,
    func1(),
    case func2() of
	ok ->
	    helloagain,
	    case func3() of
		ok ->
		    ok;
		error ->
		    error
	    end;
	error ->
	    error
    end,
    excluded(),
    func4().

func1() ->
    ok.
func2() ->
    ok.
func3() ->
    error.
func4() ->
    ok.

