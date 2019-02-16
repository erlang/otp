%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dgawd_handler.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Find out if Driver Gone Away Without Deselecting
%%%               have been reported.
%%%
%%% Created : 13 Sep 2006 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(dgawd_handler).
-behaviour(gen_event).

%% API
-export([install/0, restore/0]).
-export([got_dgawd_report/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

install() ->
    error_logger:add_report_handler(?MODULE, []).

restore() ->
    error_logger:delete_report_handler(?MODULE).

got_dgawd_report() ->
    gen_event:call(error_logger, ?MODULE, got_dgawd_report, 10*60*1000).

%%====================================================================
%% gen_event callbacks
%%====================================================================

init([]) ->
    {ok, false}.

handle_event(_, true) ->
    {ok, true};
handle_event({_, _, {emulator, _,IOList}}, false) ->
    {ok, dgawd(lists:flatten(IOList))};
handle_event(_, State) ->
    {ok, State}.

handle_call(got_dgawd_report, State) ->
    {ok, State, State};
handle_call(_Query, _State) ->
    {error, bad_query}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%

dgawd([]) ->
    false;
dgawd([$d,$r,$i,$v,$e,$r|Cs]) ->
    gawd(Cs);
dgawd([_|Cs]) ->
    dgawd(Cs).

gawd([]) ->
    false;
gawd([$g,$o,$n,$e|Cs]) ->
    awd(Cs);
gawd([_|Cs]) ->
    gawd(Cs).

awd([]) ->
    false;
awd([$a,$w,$a,$y|Cs]) ->
    wd(Cs);
awd([_|Cs]) ->
    awd(Cs).

wd([]) ->
    false;
wd([$w,$i,$t,$h,$o,$u,$t|Cs]) ->
    d(Cs);
wd([_|Cs]) ->
    wd(Cs).


d([]) ->
    false;
d([$d,$e,$s,$e,$l,$e,$c,$t,$i,$n,$g|_Cs]) ->
    true;
d([_|Cs]) ->
    d(Cs).
