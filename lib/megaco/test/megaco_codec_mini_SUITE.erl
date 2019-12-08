%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2019. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Test mini encoding/decoding (codec) module of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_codec_mini_SUITE).

%% ----

-include_lib("megaco/include/megaco.hrl").
-include("megaco_test_lib.hrl").

%% ----

-export([
 	 suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,

	 otp7672_msg01/1,
 	 otp7672_msg02/1

	]).  


%% ----

-define(SET_DBG(S,D), begin put(severity, S), put(dbg, D) end).
-define(RESET_DBG(),  begin erase(severity),  erase(dbg)  end).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [{group, tickets}].

groups() -> 
    [
     {tickets, [], tickets_cases()}
    ].

tickets_cases() ->
    [
     otp7672_msg01,
     otp7672_msg02
    ].



%%
%% -----
%%

init_per_suite(suite) ->
    [];
init_per_suite(doc) ->
    [];
init_per_suite(Config0) when is_list(Config0) ->

    ?ANNOUNCE_SUITE_INIT(),

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite([{sysmon, false} | Config0]) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->

            p("init_per_suite -> end when"
              "~n      Config: ~p"
              "~n      Nodes:  ~p", [Config1, erlang:nodes()]),

            Config1
    end.

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config0) when is_list(Config0) ->

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    Config1 = ?LIB:end_per_suite(Config0),

    p("end_per_suite -> end when"
      "~n      Nodes:  ~p", [erlang:nodes()]),

    Config1.


%%
%% -----
%%

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%
%% -----
%%

init_per_testcase(Case, Config) ->

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config, erlang:nodes()]),

    put(verbosity,trc),
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config, erlang:nodes()]),

    erase(verbosity),
    megaco_test_lib:end_per_testcase(Case, Config).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
%% Ticket test cases:


%% --------------------------------------------------------------
%% 

otp7672_msg01(suite) ->
    [];
otp7672_msg01(Config) when is_list(Config) ->
    %% ?SET_DBG(trc, true),
    d("otp7672_msg01 -> entry", []),
    ok = otp7672( otp7672_msg01() ),
    %% ?RESET_DBG(), 
    ok.

otp7672_msg02(suite) ->
    [];
otp7672_msg02(Config) when is_list(Config) ->
    %% ?SET_DBG(trc, true),
    d("otp7672_msg02 -> entry", []),
    ok = otp7672( otp7672_msg02() ),
    %% ?RESET_DBG(), 
    ok.


otp7672_msg01() ->
    <<"!/1 <TEST> ">>.

otp7672_msg02() ->
    <<"!/1 <TE> ">>.

otp7672(Msg) ->
    case megaco_text_mini_decoder:decode_message([], Msg) of
	{ok, M} ->
	    t("mini decode successfull: ~n~p", [M]),
	    ok;
	Error ->
	    e("mini decode failed: ~n~p", [Error]),
	    {error, Error}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(F, A) ->
    io:format("*** [~s] ~p ***"
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]).


t(F,A) ->
    p(printable(get(severity),trc),trc,F,A).

d(F,A) ->
    p(printable(get(severity),dbg),dbg,F,A).

%% l(F,A) ->
%%     p(printable(get(severity),log),log,F,A).

e(F,A) ->
    p(printable(get(severity),err),err,F,A).


printable(trc,_) ->
    true;
printable(dbg,trc) ->
    false;
printable(dbg,_) ->
    true;
printable(log,log) ->
    true;
printable(log,err) ->
    true;
printable(err,err) ->
    true;
printable(_,_) ->
    false.


p(true,L,F,A) ->
    io:format("~s:" ++ F ++ "~n", [image_of(L)|A]);
p(_,_,_,_) ->
    ok.

image_of(trc) ->
    "TRC";
image_of(dbg) ->
    "DBG";
image_of(log) ->
    "LOG";
image_of(err) ->
    "ERR";
image_of(L) ->
    io_lib:format("~p",[L]).

