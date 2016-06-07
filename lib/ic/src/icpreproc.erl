%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%
-module(icpreproc).



-export([preproc/2]).


-import(lists, [filter/2]).


%%----------------------------------------------------------------------
%%----------------------------------------------------------------------


preproc(G, File) ->
    Cmd		= ic_options:get_opt(G, preproc_cmd),
    Flags	= ic_options:get_opt(G, preproc_flags),


    case Cmd of
	"erl" ->
	    case ic_pp:run(File,Flags) of
		{ok, [$#, $ , $1 | Rest], []} ->
		    [$#, $ , $1 | Rest];
		{ok, [$#, $ , $1 | Rest], Warning} ->
		    print_warning(G,Warning),
		    [$#, $ , $1 | Rest];
		{error,Error} ->
		    print_error(G,Error)
	    end;
	
	_ ->
	    Line	= Cmd++" "++Flags++" "++File,
	    % FIXME: Check status code of command instead of this test
	    case os:cmd(Line) of
		[$#, $ , C | Rest] when is_integer(C), C > $0, C =< $9 ->
		    [$#, $ , C | Rest];
		X ->
		    ic_error:fatal_error(G, {preproc, filter(X)})
	    end
    end.


filter(X) ->  
    X2 = divide_nl(X, []),
    filter_x_switch(X2).


divide_nl([10 | Xs], Out) ->
    [lists:reverse(Out) | divide_nl(Xs, [])];
divide_nl([X | Xs], Out) -> divide_nl(Xs, [X|Out]);
divide_nl([], Out) -> lists:reverse(Out).


filter_x_switch(L) ->
    filter(fun([$g,$c,$c,$:,$ ,$W,$a,$r,$n,$i,$n,$g,$:,$ ,$`,$-,$x,$ | _]) ->
		   false;
	      (_) -> true end, L).


print_error(_G,[]) ->
    ok;
print_error(G,[{File,Line,Text}]) ->
    ErrorText = File++":"++integer_to_list(Line)++": "++Text,
    ic_error:fatal_error(G, {ic_pp_error, ErrorText}),
    ok;
print_error(G,[{File,Line,Text}|T]) ->
    ErrorText = File++":"++integer_to_list(Line)++": "++Text,
    ic_error:error(G, {ic_pp_error, ErrorText}),
    print_error(G,T);
print_error(G,[H]) ->
    ErrorText = H++"\n",
    ic_error:fatal_error(G, {ic_pp_error, ErrorText}),
    ok;
print_error(G,[H|T]) ->
    ErrorText = H++"\n",
    ic_error:error(G, {ic_pp_error, ErrorText}),
    print_error(G,T).


print_warning(_G,[]) ->
    ok;
print_warning(G,[{File,Line,Text}|T]) ->
    WarText = File++":"++integer_to_list(Line)++": "++Text,
    ic_error:warn(G, {ic_pp_warning, WarText}),
    print_warning(G,T);
print_warning(G,[H|T]) ->
    WarText = H++"\n",
    ic_error:warn(G, {ic_pp_warning, WarText}),
    print_warning(G,T).


