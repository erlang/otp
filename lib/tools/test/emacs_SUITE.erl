%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(emacs_SUITE).

%%-define(line_trace, 1).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([bif_highlight/1]).

all() -> 
    [bif_highlight].

init_per_testcase(_Case, Config) ->
    ErlangEl = filename:join([code:lib_dir(tools),"emacs","erlang.el"]),
    case file:read_file_info(ErlangEl) of
	{ok, _} ->
	    [{el, ErlangEl}|Config];
	_ ->
	    {skip, "Could not find erlang.el"}
    end.

end_per_testcase(_Case, _Config) ->
    ok.

bif_highlight(Config) ->
    ErlangEl = proplists:get_value(el,Config),
    {ok, Bin} = file:read_file(ErlangEl),

    %% All auto-imported bifs
    IntBifs = lists:usort(
		[F  || {F,A} <- erlang:module_info(exports),
		       erl_internal:bif(F,A)]),

    %% all bif which need erlang: prefix and are not operands
    ExtBifs = lists:usort(
		[F  || {F,A} <- erlang:module_info(exports),
		       not erl_internal:bif(F,A) andalso
			   not is_atom(catch erl_internal:op_type(F,A))]),

    check_bif_highlight(Bin, <<"erlang-int-bifs">>, IntBifs),
    check_bif_highlight(Bin, <<"erlang-ext-bifs">>, ExtBifs).
    

check_bif_highlight(Bin, Tag, Compare) ->
    [_H,IntMatch,_T] = 
	re:split(Bin,<<"defvar ",Tag/binary,
		       "[^(]*\\(([^)]*)">>,[]),
    EmacsIntBifs = [list_to_atom(S) || 
		  S <- string:tokens(binary_to_list(IntMatch)," '\"\n")],
    
    ct:log("Emacs ~p",[EmacsIntBifs]),
    ct:log("Int ~p",[Compare]),

    ct:log("Diff1 ~p",[Compare -- EmacsIntBifs]),
    ct:log("Diff2 ~p",[EmacsIntBifs -- Compare]),
    [] = Compare -- EmacsIntBifs,
    [] = EmacsIntBifs -- Compare.
    
    
