%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% Purpose: Externalize/internalize Megaco/H.248 messages
%%----------------------------------------------------------------------

-module(megaco_erl_dist_encoder).

-behaviour(megaco_encoder).

-export([encode_message/3, decode_message/3,
         decode_mini_message/3,

	 encode_transaction/3,
	 encode_action_requests/3,
	 encode_action_request/3,
	 encode_command_request/3,
	 encode_action_reply/3
	]).
-export([version_of/2]).

%% Backward compatible funcs:
-export([encode_message/2, decode_message/2]).


-include("megaco_message_internal.hrl").

-define(MC_MOD, megaco_erl_dist_encoder_mc).


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(Config, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(Config, V, MegaMsg).

encode_message([{version3, _}|EC], Vsn, MegaMsg) ->
    encode_message(EC, Vsn, MegaMsg);
encode_message([megaco_compressed|Config], Vsn, MegaMsg) 
  when is_record(MegaMsg, 'MegacoMessage') ->
    {ok, erlang:term_to_binary(?MC_MOD:encode(MegaMsg, Vsn), Config)};
encode_message([{megaco_compressed, Mod}|Config], Vsn, MegaMsg) 
  when is_atom(Mod) and is_record(MegaMsg, 'MegacoMessage') ->
    {ok, erlang:term_to_binary(Mod:encode(MegaMsg, Vsn), Config)};
encode_message(Config, _Vsn, MegaMsg) 
  when is_record(MegaMsg, 'MegacoMessage') ->
    {ok, erlang:term_to_binary(MegaMsg, Config)};
encode_message(_Config, _Vsn, _MegaMsg) ->
    {error, not_a_megaco_message}.


%%----------------------------------------------------------------------
%% Convert a transaction record into a binary
%% Return {ok, Bin} | {error, Reason}
%%----------------------------------------------------------------------

encode_transaction([{version3, _}|EC], Vsn, Trans) ->
    encode_transaction(EC, Vsn, Trans);
encode_transaction([megaco_compressed|Config], _Vsn, Trans) ->
    {ok, erlang:term_to_binary(?MC_MOD:encode(Trans), Config)};
encode_transaction([{megaco_compressed, Mod}|Config], _Vsn, Trans) ->
    {ok, erlang:term_to_binary(Mod:encode(Trans), Config)};
encode_transaction(Config, _Vsn, Trans) ->
    {ok, erlang:term_to_binary(Trans, Config)}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests([{version3, _}|EC], Vsn, ActReqs) ->
    encode_action_requests(EC, Vsn, ActReqs);
encode_action_requests([megaco_compressed|Config], Vsn, ActReqs0) 
  when is_list(ActReqs0) ->
    ActReqs = [?MC_MOD:encode(AR, Vsn) || AR <- ActReqs0],
    {ok, erlang:term_to_binary(ActReqs, Config)};
encode_action_requests([{megaco_compressed, Mod}|Config], Vsn, ActReqs0) 
  when is_list(ActReqs0) ->
    ActReqs = [Mod:encode(AR, Vsn) || AR <- ActReqs0],
    {ok, erlang:term_to_binary(ActReqs, Config)};
encode_action_requests(Config, _Vsn, ActReqs) 
  when is_list(ActReqs) ->
    {ok, erlang:term_to_binary(ActReqs, Config)}.



%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request([{version3, _}|EC], Vsn, ActReq) ->
    encode_action_request(EC, Vsn, ActReq);
encode_action_request([megaco_compressed|Config], Vsn, ActReq) 
  when is_tuple(ActReq) ->
    {ok, erlang:term_to_binary(?MC_MOD:encode(ActReq, Vsn), Config)};
encode_action_request([{megaco_compressed, Mod}|Config], Vsn, ActReq) 
  when is_tuple(ActReq) ->
    {ok, erlang:term_to_binary(Mod:encode(ActReq, Vsn), Config)};
encode_action_request(Config, _Vsn, ActReq) 
  when is_tuple(ActReq) ->
    {ok, erlang:term_to_binary(ActReq, Config)}.



%%----------------------------------------------------------------------
%% Convert a CommandRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_command_request([{version3, _}|EC], Vsn, CmdReq) ->
    encode_command_request(EC, Vsn, CmdReq);
encode_command_request([megaco_compressed|Config], Vsn, CmdReq)
  when is_tuple(CmdReq) ->
    {ok, erlang:term_to_binary(?MC_MOD:encode(CmdReq, Vsn), Config)};
encode_command_request([{megaco_compressed, Mod}|Config], Vsn, CmdReq)
  when is_tuple(CmdReq) ->
    {ok, erlang:term_to_binary(Mod:encode(CmdReq, Vsn), Config)};
encode_command_request(Config, _Vsn, CmdReq)
  when is_tuple(CmdReq) ->
    {ok, erlang:term_to_binary(CmdReq, Config)}.
 

%%----------------------------------------------------------------------
%% Convert a action reply into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_reply([{version3, _}|EC], Vsn, ActRep) ->
    encode_action_reply(EC, Vsn, ActRep);
encode_action_reply([megaco_compressed|Config], Vsn, ActRep) 
  when is_tuple(ActRep) ->
    {ok, erlang:term_to_binary(?MC_MOD:encode(ActRep, Vsn), Config)};
encode_action_reply([{megaco_compressed, Mod}|Config], Vsn, ActRep) 
  when is_tuple(ActRep) ->
    {ok, erlang:term_to_binary(Mod:encode(ActRep, Vsn), Config)};
encode_action_reply(Config, _Vsn, ActRep) 
  when is_tuple(ActRep) ->
    {ok, erlang:term_to_binary(ActRep, Config)}.
 
 
%%----------------------------------------------------------------------
%% Get the megaco version of the message
%% Return {ok, Version} | {error, Reason}
%%----------------------------------------------------------------------

version_of(Config, Bin) when is_binary(Bin) ->
    case decode_message(Config, 1, Bin) of
	{ok, M} ->
	    V = (M#'MegacoMessage'.mess)#'Message'.version,
	    {ok, V};
	Error ->
	    Error
    end.

decode_message(Config, Bin) ->
    decode_message(Config, 1, Bin).
				   
decode_message([{version3, _}|EC], V, Bin) ->
    decode_message(EC, V, Bin);
decode_message([megaco_compressed = MC|_Config], Vsn, Bin) ->
    case catch erlang:binary_to_term(Bin) of
	Msg when is_tuple(Msg) ->
	    case (?MC_MOD:decode(Msg, Vsn)) of
		MegaMsg when is_record(MegaMsg, 'MegacoMessage') ->
		    {ok, dm(MegaMsg, MC, Vsn)};
		_ ->
		    {error, {bad_message, Msg}}
	    end;
	{'EXIT', _Reason} ->
	    {error, bad_binary}
    end;
decode_message([{megaco_compressed, Mod} = MC|_Config], Vsn, Bin) 
  when is_atom(Mod) ->
    case catch erlang:binary_to_term(Bin) of
	Msg when is_tuple(Msg) ->
	    case (Mod:decode(Msg, Vsn)) of
		MegaMsg when is_record(MegaMsg, 'MegacoMessage') ->
		    {ok, dm(MegaMsg, MC, Vsn)};
		_ ->
		    {error, {bad_message, Msg}}
	    end;
	{'EXIT', _Reason} ->
	    {error, bad_binary}
    end;
decode_message(_Config, Vsn, Bin) ->
    case catch erlang:binary_to_term(Bin) of
	MegaMsg when is_record(MegaMsg, 'MegacoMessage') ->
	    {ok, dm(MegaMsg, undefined, Vsn)};
	{'EXIT', _Reason} ->
	    {error, bad_binary}
    end.


decode_mini_message(EC, Vsn, Bin) when is_binary(Bin) ->
    decode_message(EC, Vsn, Bin).


%% This crap is because the transactions or the action-requests
%% might have been encoded separetely

dm(#'MegacoMessage'{mess = Mess} = M, MC, Vsn) ->
    #'Message'{messageBody = Body} = Mess,
    case Body of
	{transactions, Transactions} ->
	    Body2 = {transactions, dmt(Transactions, [], MC, Vsn)},
	    Mess2 = Mess#'Message'{messageBody = Body2},
	    M#'MegacoMessage'{mess = Mess2};
	_ ->
	    M
    end.

dmt([], Acc, _, _Vsn) ->
    lists:reverse(Acc);
dmt([Trans0|Transactions], Acc, MC, Vsn) when is_binary(Trans0) ->
    Trans1 = erlang:binary_to_term(Trans0),
    Trans2 = dmt1(Trans1, MC, Vsn),
    dmt(Transactions, [Trans2|Acc], MC, Vsn);
dmt([{Tag, Trans0}|Transactions], Acc, MC, Vsn) when is_binary(Trans0) ->
    Trans1 = erlang:binary_to_term(Trans0),
    Trans2 = dmt1(Trans1, MC, Vsn),
    dmt(Transactions, [{Tag, Trans2}|Acc], MC, Vsn);
dmt([{transactionRequest, 
      #'TransactionRequest'{actions = Acts0} = TR0}|Transactions], 
    Acc, MC, Vsn) 
  when is_binary(Acts0) ->
    Acts1 = erlang:binary_to_term(Acts0),
    Acts2 = dmt1(Acts1, MC, Vsn),
    TR1 = TR0#'TransactionRequest'{actions = Acts2},
    dmt(Transactions, [{transactionRequest, TR1}|Acc], MC, Vsn);
dmt([{transactionRequest, 
      #'TransactionRequest'{actions = Acts0} = TR0}|Transactions], 
    Acc, MC, Vsn) ->
    Acts2 = [dmt2(AR, MC, Vsn) || AR <- Acts0],
    TR1 = TR0#'TransactionRequest'{actions = Acts2},
    dmt(Transactions, [{transactionRequest, TR1}|Acc], MC, Vsn);
dmt([Trans|Transactions], Acc, MC, Vsn) ->
    dmt(Transactions, [Trans|Acc], MC, Vsn).

dmt1(L, megaco_compressed, Vsn) when is_list(L) ->
    [?MC_MOD:decode(E, Vsn) || E <- L];
dmt1(L, {megaco_compressed, Mod}, Vsn) when is_list(L) ->
    [Mod:decode(E, Vsn) || E <- L];
dmt1(T, megaco_compressed, Vsn) when is_tuple(T) ->
    ?MC_MOD:decode(T, Vsn);
dmt1(T, {megaco_compressed, Mod}, Vsn) when is_tuple(T) ->
    Mod:decode(T, Vsn);
dmt1(Else, _, _Vsn) ->
    Else.
	    
dmt2(Bin, MC, Vsn) when is_binary(Bin) ->
    AR = erlang:binary_to_term(Bin),
    dmt1(AR, MC, Vsn);
dmt2(AR, _MC, _Vsn) ->
    AR.


