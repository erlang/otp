%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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

-spec domain(LogEvent,Extra) -> logger:filter_return() when
      LogEvent :: logger:log_event(),
      Extra :: {Action,Compare,MatchDomain},
      Action :: log | stop,
      Compare :: super | sub | equal | not_equal | undefined,
      MatchDomain :: list(atom()).
domain(#{meta:=Meta}=LogEvent,{Action,Compare,MatchDomain})
  when ?IS_ACTION(Action) andalso
       (Compare==super orelse
        Compare==sub orelse
        Compare==equal orelse
        Compare==not_equal orelse
        Compare==undefined) andalso
       is_list(MatchDomain) ->
    filter_domain(Compare,Meta,MatchDomain,on_match(Action,LogEvent));
domain(LogEvent,Extra) ->
    erlang:error(badarg,[LogEvent,Extra]).

-spec level(LogEvent,Extra) -> logger:filter_return() when
      LogEvent :: logger:log_event(),
      Extra :: {Action,Operator,MatchLevel},
      Action :: log | stop,
      Operator :: neq | eq | lt | gt | lteq | gteq,
      MatchLevel :: logger:level().
level(#{level:=L1}=LogEvent,{Action,Op,L2})
  when ?IS_ACTION(Action) andalso 
       (Op==neq orelse
        Op==eq orelse
        Op==lt orelse
        Op==gt orelse
        Op==lteq orelse
        Op==gteq) andalso
       ?IS_LEVEL(L2) ->
    filter_level(Op,L1,L2,on_match(Action,LogEvent));
level(LogEvent,Extra) ->
    erlang:error(badarg,[LogEvent,Extra]).

-spec progress(LogEvent,Extra) -> logger:filter_return() when
      LogEvent :: logger:log_event(),
      Extra :: log | stop.
progress(LogEvent,Action) when ?IS_ACTION(Action) ->
    filter_progress(LogEvent,on_match(Action,LogEvent));
progress(LogEvent,Action) ->
    erlang:error(badarg,[LogEvent,Action]).

-spec remote_gl(LogEvent,Extra) -> logger:filter_return() when
      LogEvent :: logger:log_event(),
      Extra :: log | stop.
remote_gl(LogEvent,Action) when ?IS_ACTION(Action) ->
    filter_remote_gl(LogEvent,on_match(Action,LogEvent));
remote_gl(LogEvent,Action) ->
    erlang:error(badarg,[LogEvent,Action]).

%%%-----------------------------------------------------------------
%%% Internal
filter_domain(super,#{domain:=Domain},MatchDomain,OnMatch) ->
    is_prefix(Domain,MatchDomain,OnMatch);
filter_domain(sub,#{domain:=Domain},MatchDomain,OnMatch) ->
    is_prefix(MatchDomain,Domain,OnMatch);
filter_domain(equal,#{domain:=Domain},Domain,OnMatch) ->
    OnMatch;
filter_domain(not_equal,#{domain:=Domain},MatchDomain,OnMatch)
  when Domain=/=MatchDomain ->
    OnMatch;
filter_domain(Compare,Meta,_,OnMatch) ->
    case maps:is_key(domain,Meta) of
        false when Compare==undefined; Compare==not_equal -> OnMatch;
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

on_match(log,LogEvent) -> LogEvent;
on_match(stop,_) -> stop.
