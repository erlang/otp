%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-module(megaco_binary_term_id_gen).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include_lib("megaco/include/megaco.hrl"). 
-include_lib("megaco/src/engine/megaco_message_internal.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------

-export([encode_without_wildcards/2, encode_with_wildcards/2, 
	 decode_without_wildcards/2, decode_with_wildcards/3]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

-define(asn_choose,     ?megaco_choose).
-define(asn_all,        ?megaco_all).


%%----------------------------------------------------------------------
%% Encode without wildcards
%%----------------------------------------------------------------------
encode_without_wildcards(IDs,LevelConfig) when is_list(LevelConfig) ->
    EncodedIDs = encode_ids(false,IDs,LevelConfig),
    #'TerminationID'{wildcard = [], id = EncodedIDs}.

%%----------------------------------------------------------------------
%% Encode with wildcards
%%----------------------------------------------------------------------
encode_with_wildcards(IDs,LevelConfig) when is_list(LevelConfig) ->
    Wildcards  = encode_wildcards(IDs,LevelConfig),
    EncodedIDs = encode_ids(true,IDs,LevelConfig),
    #'TerminationID'{wildcard = Wildcards, id = EncodedIDs}.


%%----------------------------------------------------------------------
%% Decode without wildcards
%%----------------------------------------------------------------------
decode_without_wildcards(IDs,Lc) ->
    DecodedIDs = decode_ids(IDs,Lc),
    #megaco_term_id{contains_wildcards = false, 
		    id                 = DecodedIDs}.


%%----------------------------------------------------------------------
%% Decode with wildcards
%%----------------------------------------------------------------------
decode_with_wildcards(Wildcards,IDs,Lc) ->
    DecodedIDs = decode_ids(Wildcards,IDs,Lc),
    #megaco_term_id{contains_wildcards = true, 
		    id                 = DecodedIDs}.


%%----------------------------------------------------------------------
%% Convert an internal TermId to an external
%%----------------------------------------------------------------------

encode_wildcards(IDs,LevelConfig) ->
    case (catch encode_wildcards1(IDs,LevelConfig)) of
	{'EXIT',id_config_mismatch} ->
	    exit({id_config_mismatch,IDs,LevelConfig});
	{'EXIT',Reason} ->
	    exit(Reason);
	Wildcards ->
	    encode_wildcards2(Wildcards)
    end.

encode_wildcards1(IDs,LevelConfig) ->
    encode_wildcards3(IDs,LevelConfig).

encode_wildcards2(Ws) ->
    F = fun(no_wildcard) -> false; (_) -> true end,
    lists:filter(F,Ws).



encode_wildcards3(IDs,LevelConfig) ->
    encode_wildcards3(IDs,LevelConfig,1,lists:sum(LevelConfig)).

encode_wildcards3([],[],_,_) ->
    [];
encode_wildcards3([Level|Levels],[BitsInLevel|BitsRest],LevelNo,TotSize) ->
    case (catch encode_wildcard(Level,BitsInLevel,TotSize-BitsInLevel,
				length(Levels))) of
	{'EXIT',{Reason,Info}} ->
	    exit({Reason,{LevelNo,Info}});

	no_wildcard ->
	    encode_wildcards3(Levels,BitsRest,LevelNo+1,TotSize-BitsInLevel);
	    
	{level,Wl} ->  
	    [Wl|
	     encode_wildcards3(Levels,BitsRest,LevelNo+1,TotSize-BitsInLevel)];

	{recursive,Wr} ->  
	    [Wr];
	
	Else ->  
	    exit({wildcard_decode_error,Else})
    end;
encode_wildcards3(Levels,[],LevelNo,TotSize) ->
    exit({id_config_mismatch,{Levels,LevelNo,TotSize}});
encode_wildcards3(L,B,N,S) ->
    exit({wildcard_encode_error,{L,B,N,S}}).


encode_wildcard([],0,_TotBits,_RemainingIdLevels) ->
    no_wildcard;
encode_wildcard([],More,_TotBits,_RemainingIdLevels) ->
    exit({to_few_bits_in_level,More});
encode_wildcard(More,0,_TotBits,_RemainingIdLevels) ->
    exit({to_many_bits_in_level,More});
encode_wildcard([$0|R],Pos,TotBits,RemainingIdLevels) ->
    encode_wildcard(R,Pos-1,TotBits,RemainingIdLevels);
encode_wildcard([$1|R],Pos,TotBits,RemainingIdLevels) ->
    encode_wildcard(R,Pos-1,TotBits,RemainingIdLevels);
encode_wildcard([?asn_choose],Pos,TotBits,RemainingIdLevels) ->
    encode_choose(Pos-1,TotBits,RemainingIdLevels);
encode_wildcard([?asn_all],Pos,TotBits,RemainingIdLevels) ->
    encode_all(Pos-1,TotBits,RemainingIdLevels);
encode_wildcard([Val|_Rest],Pos,_TotBits,_RemainingIdLevels) ->
    exit({invalid_level_content,{Pos-1,Val}}).

%% This is the last level specified in the id list.
%% Therefor it is a 'recursive' wildcard, i.e. the 'choose' 
%% apply to this level and all remaining levels.
encode_choose(BitPosInLevel,BitsInRemainingConfigLevels,0) 
    when BitsInRemainingConfigLevels > 0 ->
    {recursive,[16#40 + BitPosInLevel + BitsInRemainingConfigLevels]};

%% The fact that there is no more bits in the level config 
%% means that this is actually the last level.
%% It should not be a 'recursive' case but a 'level' case.
%% Although it is (propably) correct with both.
encode_choose(BitPosInLevel,0,0) ->
    {recursive,[16#00 + BitPosInLevel]};

%% There are more levels specified in the id list.
%% Therefor it is a 'level' wildcard, i.e. the 'choose' 
%% apply to this level only.
encode_choose(BitPosInLevel,BitsInRemainingConfigLevels,RemainingIdLevels)  
    when RemainingIdLevels > 0 ->
    {level,[16#00 + BitPosInLevel + BitsInRemainingConfigLevels]}.

%% This is the last level specified in the id list.
%% Therefor it is a 'recursive' wildcard, i.e. the 'all' 
%% apply to this level and all remaining levels.
encode_all(BitPosInLevel,BitsInRemainingConfigLevels,0) 
    when BitsInRemainingConfigLevels > 0 ->
    {recursive,[16#c0 + BitPosInLevel + BitsInRemainingConfigLevels]};

%% The fact that there is no more bits in the level config 
%% means that this is actually the last level.
%% It should not be a 'recursive' case but a 'level' case.
%% Although it is (propably) correct with both.
encode_all(BitPosInLevel,0,0) ->
    {recursive,[16#80 + BitPosInLevel]};

%% There are more levels specified in the id list.
%% Therefor it is a 'level' wildcard, i.e. the 'all' 
%% apply to this level only.
encode_all(BitPosInLevel,BitsInRemainingConfigLevels,RemainingIdLevels) 
    when RemainingIdLevels > 0 ->
    {level,[16#80 + BitPosInLevel + BitsInRemainingConfigLevels]}.


encode_ids(W,IDs,Config) ->
    encode_ids(W,IDs,Config,8,[0],false).

encode_ids(_,[],[],8,[0|EncodedIDs],_) ->
    lists:reverse(EncodedIDs);
encode_ids(W,IDs,Config,0,EncodedIDs,Wf) ->
    encode_ids(W,IDs,Config,8,[0|EncodedIDs],Wf);
encode_ids(W,[L|Ls],[C|Cs],R,E,_) ->
    case (catch encode_id_level(W,L,C,R,E)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	{true,R1,E1} when (length(Ls) =:= 0) ->
	    encode_ids2(Cs,encode_ids1(R1,E1));
	{WildcardFound1,R1,E1} ->
	    encode_ids(W,Ls,Cs,R1,E1,WildcardFound1);
	{true,E2} when (length(Ls) =:= 0) ->
	    encode_ids2(Cs,E2);
	{WildcardFound2,E2} ->
	    encode_ids(W,Ls,Cs,8,[0|E2],WildcardFound2)
    end;
encode_ids(W,[[]],C,R,E,Wf) when length(C) > 0 ->
    exit({empty_last_level,{W,C,R,E,Wf}});
encode_ids(W,[],C,R,E,false) when length(C) > 0 ->
    exit({unexpected_eof_data,{W,C,R,E}}).

encode_ids1(_R,[0|Es]) ->
    [0|Es];
encode_ids1(R,[E|Es]) ->
    [(E bsl R)|Es].

encode_ids2([],Es) ->
    lists:reverse(Es);
encode_ids2(Cs,Es) ->
    Fill = lists:duplicate(lists:sum(Cs) div 8,0),
    lists:reverse(Fill ++ Es).
    

encode_id_level(W,L,C,R,[E|Es]) ->
    case encode_id_level1(W,L,C,R,E) of
	%% End Of Byte (more bits in level)
	{eob,_WildcardFound,L1,C1,E1} -> 
	    encode_id_level(W,L1,C1,8,[0,E1|Es]);

	%% End Of Level (more room in byte)
	{eol,WildcardFound,R1,E1} -> 
	    {WildcardFound,R1,[E1|Es]};

	%% Done; Level used up all of the byte
	{done,WildcardFound,E1} -> 
	    {WildcardFound,[E1|Es]}
    end.
    

encode_id_level1(_W,[],0,0,E) ->
    {done,false,E};
encode_id_level1(_W,[],0,R,E) ->
    {eol,false,R,E};
encode_id_level1(_W,L,C,0,E) ->
    {eob,false,L,C,E};
encode_id_level1(W,[$0|L],C,R,E) ->
    encode_id_level1(W,L,C-1,R-1,E bsl 1);
encode_id_level1(W,[$1|L],C,R,E) ->
    encode_id_level1(W,L,C-1,R-1,(E bsl 1) + 1);
encode_id_level1(true,[$$],C,R,E) ->
    encode_id_level2(C,R,E,$$);
encode_id_level1(true,[$*],C,R,E) ->
    encode_id_level2(C,R,E,$*);
encode_id_level1(false,[$$],C,R,_E) ->
    exit({wildcard_error,{$$,C,R}});
encode_id_level1(false,[$*],C,R,_E) ->
    exit({wildcard_error,{$*,C,R}});
encode_id_level1(_W,[L|_Ls],C,R,_E) ->
    exit({invalid_level_content,{C,R,L}}).

encode_id_level2(C,C,E,_W) ->
    {done,true,E bsl C};
encode_id_level2(C,R,E,W) when C > R ->
    {eob,true,[W],C-R,E bsl R};
encode_id_level2(C,R,E,_W) ->
    {eol,true,R-C,E bsl C}.


%%----------------------------------------------------------------------
%% Convert an external TermId to an internal
%%----------------------------------------------------------------------

%% Decode ID with wildcards    
decode_ids(Ws,IDs,Config) when is_list(Config) -> 
    IDs1 = decode_ids(IDs,Config),
    Ws1  = decode_wildcards(Ws,(length(IDs)*8) - 1),
    decode_ids1(Ws1,IDs1);

%% This is only temporary. Eventually a proper encoder for this case
%% should be implemented.
%% This is the case when each level is 8 bits = 1 byte and the config
%% simply indicates the number of (1 byte) levels
decode_ids(Ws,IDs,Config) when is_integer(Config) -> 
    decode_ids(Ws,IDs,lists:duplicate(Config,8)).


%% Decode ID without wildcards    
decode_ids(E,Config) when is_list(Config) -> 
    decode_ids(0,E,Config,[]);

%% This is only temporary. Eventually a proper encoder for this case
%% should be implemented.
%% This is the case when each level is 8 bits = 1 byte and the config
%% simply indicates the number of (1 byte) levels
decode_ids(E,Config) when is_integer(Config) -> 
    decode_ids(E,lists:duplicate(Config,8)).
    

%% The [0] is the result of all the bits of the byte has been shifted out.
decode_ids(_B,[0],[],Acc) ->
    lists:reverse(Acc);
decode_ids(_B,[E],[],Acc) ->
    exit({id_config_mismatch,{two_much_data,E,Acc}});
decode_ids(_B,E,[],Acc) ->
    exit({id_config_mismatch,{two_much_data,E,Acc}});
decode_ids(B,E,[L|Ls],Acc) ->
    case (catch decode_id_level(B,E,L,[])) of
	{Level,E1,B1} ->
	    decode_ids(B1,E1,Ls,[Level|Acc]);
	{'EXIT',{id_config_mismatch,{Bx,Ex,Lx,Accx}}} ->
	    exit({id_level_mismatch,{B,Bx,E,Ex,L,Ls,Lx,Acc,Accx}})
    end.
    

decode_wildcards(Ws,NofBits) -> 
    lists:keysort(3,[decode_wildcard(W,NofBits) || W <- Ws]).


%% ----------------------------------------------------------------------
%% A decoded wildcard is a three tuple: 
%% {wildcard_type(), wildcard_level(), wildcard_offset()}
%% wildcard_type()   -> $ | *
%% wildcard_level()  -> level | recursive
%% wildcard_offset() -> integer()
%%
%% The "raw" wildcard offset is measured from the end of the id bytes:
%%
%%      0 1 2 3 4 5 6 7 
%%     -----------------
%%     | | | | | | | | |
%%     -----------------
%%
%%           |<--------|
%%                5
%%
%% The decoded wildcard offset in contrast is measured from the start
%% of the id field:
%%
%%      0 1 2 3 4 5 6 7 
%%     -----------------
%%     | | | | | | | | |
%%     -----------------
%%
%%     |---->|
%%        3
%%

decode_wildcard([W],NofBits) ->
    {decode_wildcard_t(W band 16#80),
     decode_wildcard_l(W band 16#40),
     NofBits - (W band 16#3F)}.

decode_wildcard_t(16#80) -> ?asn_all;
decode_wildcard_t(16#00) -> ?asn_choose.

decode_wildcard_l(16#00) -> level;
decode_wildcard_l(16#40) -> recursive.


decode_ids1(W,IDs) ->
    lists:reverse(decode_ids1(W,IDs,0,[])).

decode_ids1([],IDs,_,Acc) -> 
    lists:reverse(IDs) ++ Acc;
decode_ids1([{Type,recursive,Offset}],IDs,Bp,Acc) ->
    decode_ids2(Type,Offset,IDs,Bp,Acc);
decode_ids1([{Type,level,Offset}|Ws],IDs,Bp,Acc) ->
    {IDs1,IDs2,Bp1} = decode_ids3(Type,Offset,IDs,Bp,[]),
    decode_ids1(Ws,IDs2,Bp1,IDs1++Acc);
decode_ids1(Ws,_,_,_) ->
    exit({invalid_wildcards,Ws}).


%% Called when recursive wildcard found
decode_ids2(Type,Offset,[ID|IDs],Bp,Acc) ->
    LevelSz = length(ID),
    Bp1 = Offset-Bp,
    case Bp1 >= LevelSz of
	true ->
	    decode_ids2(Type,Offset,IDs,Bp+LevelSz,[ID|Acc]);
	false ->
	    [decode_ids4(Type,Bp1,ID)|Acc]
    end.


decode_ids3(Type,Offset,[ID|IDs],Bp,Acc) ->
    LevelSz = length(ID),
    Bp1 = Offset-Bp,
    case Bp1 > LevelSz of
	true ->
	    decode_ids3(Type,Offset,IDs,Bp+LevelSz,[ID|Acc]);
	false ->
	    A1 = decode_ids4(Type,Bp1,ID),
	    {[A1|Acc],IDs,Bp+LevelSz}
    end.


decode_ids4(Type,0,_ID) ->
    [Type];
decode_ids4(Type,O,[H|T]) ->
    [H|decode_ids4(Type,O-1,T)].


%% E: Encoded bits -> [byte()]
%% L: Number of nits in level
decode_id_level(B,E,0,Acc) ->
    {lists:reverse(Acc),E,B};
decode_id_level(8,[_H|T],L,Acc) ->
    decode_id_level(0,T,L,Acc);
decode_id_level(B,[H|T],L,Acc) ->
    Acc1 = [decode_id_level1(H band 16#80) | Acc],
    decode_id_level(B+1,[((H bsl 1) band 16#FF) |T],L-1,Acc1);
decode_id_level(B,E,L,Acc) ->
    exit({id_config_mismatch,{B,E,L,Acc}}).

decode_id_level1(16#80) -> $1;
decode_id_level1(16#00) -> $0.


