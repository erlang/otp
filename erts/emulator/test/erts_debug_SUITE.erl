%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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

-module(erts_debug_SUITE).
-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 flat_size/1,flat_size_big/1,df/1,
	 instructions/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [flat_size, flat_size_big, df, instructions].

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

flat_size(Config) when is_list(Config) ->
    0 = erts_debug:flat_size([]),
    0 = erts_debug:flat_size(42),
    2 = erts_debug:flat_size([a|b]),
    1 = erts_debug:flat_size({}),
    2 = erts_debug:flat_size({[]}),
    3 = erts_debug:flat_size({a,b}),
    7 = erts_debug:flat_size({a,[b,c]}),
    ok.

flat_size_big(Config) when is_list(Config) ->
    %% Build a term whose external size only fits in a big num (on 32-bit CPU).
    flat_size_big_1(16#11111111111111117777777777777777888889999, 0, 16#FFFFFFF).

flat_size_big_1(Term, Size0, Limit) when Size0 < Limit ->
    case erts_debug:flat_size(Term) of
	Size when is_integer(Size), Size0 < Size ->
	    io:format("~p", [Size]),
	    flat_size_big_1([Term|Term], Size, Limit)
    end;
flat_size_big_1(_, _, _) -> ok.

df(Config) when is_list(Config) ->
    ?line P0 = pps(),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line ok = file:set_cwd(PrivDir),
    ?line erts_debug:df(?MODULE),
    ?line Beam = filename:join(PrivDir, ?MODULE_STRING++".dis"),
    ?line {ok,Bin} = file:read_file(Beam),
    ?line ok = io:put_chars(binary_to_list(Bin)),
    ?line ok = file:delete(Beam),
    ?line true = (P0 == pps()),    
    ok.

pps() ->
    {erlang:ports()}.

instructions(Config) when is_list(Config) ->
    ?line Is = erts_debug:instructions(),
    ?line _ = [list_to_atom(I) || I <- Is],
    ok.
