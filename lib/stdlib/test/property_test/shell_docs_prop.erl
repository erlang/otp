%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
-module(shell_docs_prop).
-export([prop_render/0]).

%%% This will include the .hrl file for the installed testing tool:
-include_lib("common_test/include/ct_property_test.hrl").
-include_lib("kernel/include/eep48.hrl").
-compile([export_all]).

prop_render() ->
    numtests(10000,
    ?FORALL(Doc,
            ?LET(Blocks,blocks(),
                 #docs_v1{ module_doc = #{ <<"en">> => Blocks }, docs = [] }
                ),
            begin
                try
                    shell_docs:render(test, Doc),
                    shell_docs:validate(Doc),
                    N1 = shell_docs:normalize(maps:get(<<"en">>,Doc#docs_v1.module_doc)),
                    N1 = shell_docs:normalize(N1),
                    true
                catch C:E:ST ->
                        ct:pal("~p ~p:~p ~p~n",[Doc,C,E,ST]),
                        false
                end
            end)).

-define(LIMIT,5).

blocks() ->
    blocks([]).

blocks([l|_] = S) ->
    ?LAZY(
       frequency(
         [{2,[]},
          {2,[{li,[],blocks([li | S])}]}])
       );
blocks([dl|_] = S) ->
    ?LAZY(
       frequency(
         [{2,[]},
          {2,[{dt,[],blocks([dt | S])}]},
          {2,[{dd,[],blocks([dd | S])}]}])
       );
blocks(S) ->
    ?LAZY(
       frequency(
         [{?LIMIT div 2,[]},
          {max(1,?LIMIT - length(S)),
           ?LET(Lst,[block(S)|blocks(S)],lists:flatten(Lst))},
          {max(1,?LIMIT - length(S)),
           ?LET(Lst,[inlines()|blocks(S)],lists:flatten(Lst))}
          ]
         )).

inlines() ->
    inlines([]).
inlines(S) ->
    ?LAZY(
       frequency(
         [{?LIMIT,[]},
          {max(1,?LIMIT - length(S)),[inline(S)|inlines(S)]}
          ]
         )).

block(S) ->
    frequency(
      fmax('div',3,S,{'div',oneof([[],[{class,<<"Warning">>}]]), blocks(['div'|S])}) ++
      fmax(p,1,S,{p,[],blocks([p|S])}) ++
      fmax(l,3,S,{ul,[],blocks([l|S])}) ++
      fmax(l,3,S,{ol,[],blocks([l|S])}) ++
      fmax(dl,3,S,{dl,[],blocks([dl|S])}) ++
      fmax(['div',l,dl],0,S,{h1,[],inlines(['div'|S])}) ++
      fmax(['div',l,dl],0,S,{h2,[],inlines(['div'|S])}) ++
      fmax(['div',l,dl],1,S,{h3,[],inlines(['div'|S])}) ++
          [{5,{br,[],[]}}]
     ).

inline(S) ->
    frequency(
      fmax(i,1,S,{i,[],?LAZY(inlines([i|S]))}) ++
      fmax(code,1,S,{code,[],?LAZY(inlines([code|S]))}) ++
      fmax(a,1,S,{a,[],?LAZY(inlines([a|S]))}) ++
      fmax(em,1,S,{em,[],?LAZY(inlines([em|S]))}) ++
          [{10,characters()}]).

characters() ->
    ?LET(Str,list(frequency([{10,printable_character()},{1,char()}])),
         unicode:characters_to_binary(Str)).

printable_character() ->
    oneof([integer($\040,$\176),
           integer(16#A0, 16#D800-1),
           integer(16#DFFF+1,16#FFFE-1),
           integer(16#FFFF+1,16#10FFFF),
           $\n,$\r,$\t,$\v,$\b,$\f,$\e]).

fmax(What,Depth,S,E) when not is_list(What) ->
    fmax([What],Depth,S,E);
fmax(What,Depth,S,E) ->
    Cnt =
        lists:foldl(
          fun(E,Cnt) ->
                  case lists:member(E,What) of
                      true ->
                          Cnt+1;
                      false ->
                          Cnt
                  end
          end, 0, S),
    if Depth-Cnt =< 0 ->
            [];
       true ->
            [{10 - (Depth-Cnt),E}]
    end.
