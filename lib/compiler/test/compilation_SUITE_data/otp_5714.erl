%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
-module(otp_5714).
-export([?MODULE/0,foo/1,mktree_text/1]).
-binary(<<1,2,3>>).

?MODULE() ->
    [<<1,2,3>>] = proplists:get_value(binary, ?MODULE:module_info(attributes)),
    ok.

-record(foo_record, {key,blabla}).
foo(A) ->
      hd(tl(element(2,element(2,catch erlang:error(apa))))),
       case A of
       A ->
           B =  #foo_record{ key = key1},
            C = B#foo_record{ key = key2},
           {X,Y} = {a,b}
  end.

mktree_text(Val) ->
    case erlang:is_integer(Val) of
        _A = IsInteger ->
            _A;
        _A ->
            IsInteger = erlang:exit({{bug,mktree_text,4},{line,34},match,[_A]})
    end,
    ok;
mktree_text(_A1) ->
    erlang:exit({{bug,mktree_text,4},{line,33},function_clause,[_A1]}).
