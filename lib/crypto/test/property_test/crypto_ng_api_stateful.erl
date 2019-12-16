%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

-module(crypto_ng_api_stateful).

-compile(export_all).

-include_lib("common_test/include/ct_property_test.hrl").
-include("crypto_prop_generators.hrl").

%%%================================================================
%%% Properties:

prop__crypto_init_multi(Config) ->
    numtests(300,
             ?FORALL(Cmds, parallel_commands(?MODULE),
                     begin
                         RunResult = run_parallel_commands(?MODULE, Cmds),
                         ct_property_test:present_result(?MODULE, Cmds, RunResult, Config)
                     end)).

%%%================================================================
-define(call(F,As), {call,?MODULE,F,As}).

initial_state() ->
    #{}.


command(S) ->
    frequency(
      lists:flatten(
        [
         {max(0,5-maps:size(S)),
         ?call(init_crypto, ?LET(Ciph, cipher(),
                                  [Ciph, key(Ciph), iv(Ciph), block_size(Ciph)]))},

         [{10, ?call(encrypt, [oneof(refs(S)), binary()])
          }
          || refs(S) =/= []],
         
         [{30, ?call(decrypt, ?LET({Refs,{_,[CT|_]}}, oneof(Sc),
                                   [Refs, CT]))
          }
          || Sc <- [[R || {_,{_,Cs}} = R <- maps:to_list(S),
                          Cs =/= []]
                   ],
             Sc =/= [] ],
[]        ])).


precondition(S, {call,?MODULE,decrypt,[Refs,CrT]}) ->
    %% io:format("precondition(1) ~p Args = ~p, S = ~p", [decrypt,[Refs,CrT],S]),
    case maps:get(Refs, S) of
        {_BlockSize,  [CrT|_]} ->
            %% io:format(" (sym) true~n",[]),
            true;
        {_BlockSize,  [CrT|_], _} ->
            %% io:format(" (dyn) true~n",[]),
            true;
        _Other ->
            %% io:format(" _Other=~p false~n",[_Other]),
            false
    end;
precondition(_S, {call,?MODULE,_Name,_Args}) ->
    %% io:format("precondition ~p Args = ~p, S = ~p~n", [_Name,_Args,_S]),
    true.


postcondition(_D, _Call, error) ->
    %% io:format("postcondition ~p:~p error _Call = ~p~n",[?MODULE,?LINE,_Call]),
    false;
postcondition(D, {call,?MODULE,decrypt,[Refs,_CrT]}, Result) -> 
    #{Refs := {_BlockSize, _CT, PT}} = D,
    Size = size(Result),
    <<Expect:Size/binary, _/binary>> = PT,
    Expect == Result;
postcondition(_D, _Call, _Result) ->
    true.


symbolic({var,_}) -> true;
symbolic(_) -> false.
    

next_state(S, Refs, {call,?MODULE,init_crypto,[_Cipher, _Key, _IV, BlockSize]}=_C) ->
    case symbolic(Refs) of
        true ->
            S#{Refs => {BlockSize, []} };
        false ->
            S#{Refs => {BlockSize, [], <<>>} }
    end;
            
next_state(S, Res, {call,?MODULE,encrypt,[Refs,Ptxt]}=_C) ->
%% io:format("next_state enrypt Refs = ~p, S = ~p~n", [Refs,S]),
    case S of
        #{Refs := {BlockSize, Cs}} ->
            S#{Refs := {BlockSize, Cs++[Res]}};

        #{Refs := {BlockSize, Cs,Ps}} ->
            S#{Refs := {BlockSize, Cs++[Res], <<Ps/binary,Ptxt/binary>>}}
    end;

next_state(S, Result, {call,?MODULE,decrypt,[Refs,CrT]}=_C) ->
%% io:format("next_state decrypt Refs = ~p, CrT = ~p, S = ~p~n", [Refs,CrT,S]),
    case S of
        #{Refs := {BlockSize, [CrT|Cs]}} ->
            S#{Refs := {BlockSize, Cs}};

        #{Refs := {BlockSize, [CrT|Cs], Ps0}} ->
            Sz = size(Result),
            <<Result:Sz/binary,Ps/binary>> = Ps0,
            S#{Refs := {BlockSize,Cs,Ps}}
    end.

%%%================================================================
-define(ok_error(X), 
        try X of
            __R -> %% io:format("~p:~p ok! Result = ~p~n", [?MODULE,?LINE,__R]),
                   __R
            catch
                _CC:_EE:_SS ->
                    io:format("******* ~p:~p  ~p ~p~n~p", [?MODULE,?LINE,_CC,_EE,_SS]),
                    error
            end).
                               
init_crypto(Cipher, Key, IV, _BlockSize) ->
    %% io:format("~p:~p init_crypto~n    (~p,~n     ~p,~n     ~p,~n     ~p)", [?MODULE,?LINE,Cipher,Key,IV,_BlockSize]),
    ?ok_error({
                crypto:crypto_init(Cipher, Key, IV, true),
                crypto:crypto_init(Cipher, Key, IV, false)
              }).

encrypt({EncRef,_DecRef}, PlainText) ->
    %% io:format("~p:~p encrypt~n    (~p,~n     ~p) ", [?MODULE,?LINE,EncRef,PlainText]),
    ?ok_error(crypto:crypto_update(EncRef, PlainText)).

decrypt({_EncRef,DecRef}, CT) ->
    %% io:format("~p:~p decrypt~n    (~p,~n     ~p)", [?MODULE,?LINE,DecRef,CT]),
    ?ok_error(crypto:crypto_update(DecRef, CT)).

%%%----------------------------------------------------------------
refs(S) -> [Refs || {Refs,_} <- maps:to_list(S)].

