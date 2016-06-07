%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
-module(beam_except_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 multiple_allocs/1,coverage/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() ->
    [{p,[parallel],
      [multiple_allocs,
       coverage]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

multiple_allocs(_Config) ->
    {'EXIT',{{badmatch,#{true:=[p]}},_}} =
	 (catch could(pda, 0.0, {false,true}, {p})),
    {'EXIT',{function_clause,_}} = (catch place(lee)),
    {'EXIT',{{badmatch,wanted},_}} = (catch conditions()),

    ok.

could(Coupons = pda, Favorite = _pleasure = 0.0, {_, true}, {Presents}) ->
  (0 = true) = #{true => [Presents]}.

place(lee) ->
    (pregnancy = presentations) = [hours | [purchase || _ <- 0]] + wine.

conditions() ->
    (talking = going) = storage + [large = wanted].

coverage(_) ->
    File = {file,"fake.erl"},
    ok = fc(a),
    {'EXIT',{function_clause,
	     [{?MODULE,fc,[[x]],[File,{line,2}]}|_]}} =
	(catch fc([x])),
    {'EXIT',{function_clause,
	     [{?MODULE,fc,[y],[File,{line,2}]}|_]}} =
	(catch fc(y)),
    {'EXIT',{function_clause,
	     [{?MODULE,fc,[[a,b,c]],[File,{line,6}]}|_]}} =
	(catch fc([a,b,c])),

    {'EXIT',{undef,[{erlang,error,[a,b,c],_}|_]}} =
	(catch erlang:error(a, b, c)),

    {'EXIT',{badarith,[{?MODULE,bar,1,[File,{line,9}]}|_]}} =
	(catch bar(x)),
    {'EXIT',{{case_clause,{1}},[{?MODULE,bar,1,[File,{line,9}]}|_]}} =
	(catch bar(0)),
    ok.

-file("fake.erl", 1).
fc(a) ->	                                %Line 2
    ok;						%Line 3
fc(L) when length(L) > 2 ->			%Line 4
    %% Not the same as a "real" function_clause error.
    error(function_clause, [L]).		%Line 6
%% Would crash the compiler.
bar(X) ->					%Line 8
    case {X+1} of				%Line 9
	1 -> ok					%Line 10
    end.					%Line 11
