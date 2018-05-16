%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(logger_filters).

-export([domain/2,
         level/2,
         progress/2,
         remote_gl/2]).

-include("logger_internal.hrl").
-define(IS_ACTION(A), (A==log orelse A==stop)).

-spec domain(Log,Extra) -> logger:filter_return() when
      Log :: logger:log(),
      Extra :: {Action,Compare,MatchDomain},
      Action :: log | stop,
      Compare :: prefix_of | starts_with | equals | no_domain,
      MatchDomain :: list(atom()).
domain(#{meta:=Meta}=Log,{Action,Compare,MatchDomain})
  when ?IS_ACTION(Action) andalso
       (Compare==prefix_of orelse
        Compare==starts_with orelse
        Compare==equals orelse
        Compare==differs orelse
        Compare==no_domain) andalso
       is_list(MatchDomain) ->
    filter_domain(Compare,Meta,MatchDomain,on_match(Action,Log));
domain(Log,Extra) ->
    erlang:error(badarg,[Log,Extra]).

-spec level(Log,Extra) -> logger:filter_return() when
      Log :: logger:log(),
      Extra :: {Action,Operator,MatchLevel},
      Action :: log | stop,
      Operator :: neq | eq | lt | gt | lteq | gteq,
      MatchLevel :: logger:level().
level(#{level:=L1}=Log,{Action,Op,L2})
  when ?IS_ACTION(Action) andalso 
       (Op==neq orelse
        Op==eq orelse
        Op==lt orelse
        Op==gt orelse
        Op==lteq orelse
        Op==gteq) andalso
       ?IS_LEVEL(L2) ->
    filter_level(Op,L1,L2,on_match(Action,Log));
level(Log,Extra) ->
    erlang:error(badarg,[Log,Extra]).

-spec progress(Log,Extra) -> logger:filter_return() when
      Log :: logger:log(),
      Extra :: log | stop.
progress(Log,Action) when ?IS_ACTION(Action) ->
    filter_progress(Log,on_match(Action,Log));
progress(Log,Action) ->
    erlang:error(badarg,[Log,Action]).

-spec remote_gl(Log,Extra) -> logger:filter_return() when
      Log :: logger:log(),
      Extra :: log | stop.
remote_gl(Log,Action) when ?IS_ACTION(Action) ->
    filter_remote_gl(Log,on_match(Action,Log));
remote_gl(Log,Action) ->
    erlang:error(badarg,[Log,Action]).

%%%-----------------------------------------------------------------
%%% Internal
filter_domain(prefix_of,#{domain:=Domain},MatchDomain,OnMatch) ->
    is_prefix(Domain,MatchDomain,OnMatch);
filter_domain(starts_with,#{domain:=Domain},MatchDomain,OnMatch) ->
    is_prefix(MatchDomain,Domain,OnMatch);
filter_domain(equals,#{domain:=Domain},Domain,OnMatch) ->
    OnMatch;
filter_domain(differs,#{domain:=Domain},MatchDomain,OnMatch)
  when Domain=/=MatchDomain ->
    OnMatch;
filter_domain(Action,Meta,_,OnMatch) ->
    case maps:is_key(domain,Meta) of
        false when Action==no_domain; Action==differs -> OnMatch;
        _ -> ignore
    end.

is_prefix(D1,D2,OnMatch) when is_list(D1), is_list(D2) ->
    case lists:prefix(D1,D2) of
        true -> OnMatch;
        false -> ignore
    end;
is_prefix(_,_,_) ->
    ignore.

filter_level(Op,L1,L2,OnMatch) ->
    case logger:compare_levels(L1,L2) of
        eq when Op==eq; Op==lteq; Op==gteq -> OnMatch;
        lt when Op==lt; Op==lteq; Op==neq -> OnMatch;
        gt when Op==gt; Op==gteq; Op==neq -> OnMatch;
        _ -> ignore
    end.

filter_progress(#{msg:={report,#{label:={_,progress}}}},OnMatch) ->
    OnMatch;
filter_progress(_,_) ->
    ignore.

filter_remote_gl(#{meta:=#{gl:=GL}},OnMatch) when node(GL)=/=node() ->
    OnMatch;
filter_remote_gl(_,_) ->
    ignore.

on_match(log,Log) -> Log;
on_match(stop,_) -> stop.
