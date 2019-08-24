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
