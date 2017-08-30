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
%% Purpose : Handle ASN.1 BER encoding of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_binary_term_id).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include_lib("megaco/include/megaco.hrl"). 
-include_lib("megaco/src/engine/megaco_message_internal.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------

-export([encode/2, decode/2]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

-define(default_config, [8,8,8]).

-define(asn_root_termination_id,
	#'TerminationID'{wildcard = [],
			 id       = [16#FF, 16#FF, 16#FF, 16#FF,
				     16#FF, 16#FF, 16#FF, 16#FF]}).

-define(megaco_all_wildcard_termination_id,    
	#megaco_term_id{contains_wildcards = true,
			id                 = [[?megaco_all]]}).
-define(megaco_choose_wildcard_termination_id, 
	#megaco_term_id{contains_wildcards = true,
			id                 = [[?megaco_choose]]}).


%%----------------------------------------------------------------------
%% Convert a 'TerminationId' record into a ASN.1 termination id
%% Return {ok, TermId} | {error, Reason}
%%---------------------------------------------------------------------- 

encode(_Config, TermId) when TermId =:= ?megaco_root_termination_id ->
    {ok, ?asn_root_termination_id};
encode(Config, TermId) when (TermId =:= ?megaco_all_wildcard_termination_id) andalso 
			    (Config =:= ?default_config) ->
    {ok, asn_all_tid()};
encode(Config, TermId) when (TermId =:= ?megaco_choose_wildcard_termination_id) andalso 
                            (Config =:= ?default_config)  ->
    {ok, asn_choose_tid()};
encode(Config, #megaco_term_id{contains_wildcards = false, id = IDs}) ->
    case (catch encode1(IDs,Config)) of
	{'EXIT',Reason} ->
	    {error,Reason};
	EncodedTid ->
	    {ok, EncodedTid}
    end;
encode(Config, #megaco_term_id{contains_wildcards = true, id = IDs}) ->
    case (catch encode2(IDs,Config)) of
	{'EXIT',Reason} ->
	    {error,Reason};
	EncodedTid ->
	    {ok, EncodedTid}
    end;
encode(_Config, TermId) ->
    {error, {bad_type, TermId}}.


first_bit() -> 
    lists:sum(?default_config) - 1.
asn_all_tid() -> 
    #'TerminationID'{wildcard = [[(2#11000000 + first_bit())]],
		     id = [0, 0, 0]}.
asn_choose_tid() -> 
    #'TerminationID'{wildcard = [[(2#01000000 + first_bit())]],
		     id = [0, 0, 0]}.


%%----------------------------------------------------------------------
%% Encode without wildcards
%%----------------------------------------------------------------------
encode1(IDs,LevelConfig) when is_list(LevelConfig) ->
    megaco_binary_term_id_gen:encode_without_wildcards(IDs, LevelConfig);


%% This is only temporary. Eventually a proper encoder for this case
%% should be implemented
encode1(IDs,LevelConfig) when is_integer(LevelConfig) ->
    %% megaco_binary_term_id_8lev:encode_without_wildcards(IDs, LevelConfig).
    encode1(IDs,lists:duplicate(LevelConfig,8)). 


%%----------------------------------------------------------------------
%% Encode with wildcards
%%----------------------------------------------------------------------
encode2(IDs,LevelConfig) when is_list(LevelConfig) ->
    megaco_binary_term_id_gen:encode_with_wildcards(IDs, LevelConfig);


%% This is only temporary. Eventually a proper encoder for this case
%% should be implemented
encode2(IDs,LevelConfig) when is_integer(LevelConfig) ->
    %% megaco_binary_term_id_8lev:encode_with_wildcards(IDs, LevelConfig).
    encode2(IDs,lists:duplicate(LevelConfig,8)).


%%----------------------------------------------------------------------
%% Convert a ASN.1 termination id into a 'TerminationId' record
%% Return {ok, TerminationId} | {error, Reason}
%%----------------------------------------------------------------------

decode(_Config, TermId) when (TermId =:= ?asn_root_termination_id) ->
    {ok, ?megaco_root_termination_id};
decode(Config, #'TerminationID'{wildcard = [], id = IDs}) ->
    case (catch decode1(IDs,Config)) of
	{'EXIT',Reason} ->
	    {error,Reason};
	MegacoTid ->
	    {ok,MegacoTid}
    end;
decode(Config, #'TerminationID'{wildcard = Wildcards, id = IDs}) ->
    case (catch decode2(Wildcards,IDs,Config)) of
	{'EXIT',Reason} ->
	    {error,Reason};
	MegacoTid ->
	    {ok,MegacoTid}
    end;
decode(_Config, TermId) ->
    {error, {bad_type, TermId}}.


%%----------------------------------------------------------------------
%% Decode without wildcards
%%----------------------------------------------------------------------
decode1(IDs, Lc) when is_list(Lc) ->
    megaco_binary_term_id_gen:decode_without_wildcards(IDs, Lc);

%% This is only temporary. Eventually a proper encoder for this case
%% should be implemented
decode1(IDs, Lc) when is_integer(Lc) ->
    %% megaco_binary_term_id_8lev:decode_without_wildcards(IDs, Lc).
    decode1(IDs,lists:duplicate(Lc,8)). 


%%----------------------------------------------------------------------
%% Decode with wildcards
%%----------------------------------------------------------------------
decode2(Wildcards, IDs, Lc) when is_list(Lc) ->
    megaco_binary_term_id_gen:decode_with_wildcards(Wildcards, IDs, Lc);

%% This is only temporary. Eventually a proper encoder for this case
%% should be implemented
decode2(Wildcards, IDs, Lc) when is_integer(Lc) ->
    %% megaco_binary_term_id_8lev:decode_with_wildcards(Wildcards, IDs, Lc);
    decode2(Wildcards, IDs, lists:duplicate(Lc,8)).



