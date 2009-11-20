%% -*- erlang-indent-level: 2 -*-
%%=============================================================================
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(hipe_pack_constants).
-export([pack_constants/2, slim_refs/1, slim_constmap/1]).

-include("hipe_consttab.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").

%%-----------------------------------------------------------------------------

-type raw_data() :: binary() | number() | list() | tuple().
-type tbl_ref()  :: {hipe_constlbl(), non_neg_integer()}.

-record(pcm_entry, {mfa       :: mfa(),
		    label     :: hipe_constlbl(),
		    const_num :: non_neg_integer(),
		    start     :: non_neg_integer(),
		    type      :: 0 | 1 | 2,
		    raw_data  :: raw_data()}).

%%-----------------------------------------------------------------------------

-spec pack_constants([{mfa(),[_],hipe_consttab()}], ct_alignment()) ->
	{ct_alignment(),
	 non_neg_integer(),
	 [#pcm_entry{}],
	 [{mfa(),[tbl_ref() | {'sorted',non_neg_integer(),[tbl_ref()]}]}]}.

pack_constants(Data, Align) ->
  pack_constants(Data, 0, Align, 0, [], []).

pack_constants([{MFA,_,ConstTab}|Rest], Size, Align, ConstNo, Acc, Refs) ->
  Labels = hipe_consttab:labels(ConstTab),
  %% RefToLabels = hipe_consttab:referred_labels(ConstTab),
  {NewSize, NewAlign, Map, NewConstNo, RefToLabels} =
    pack_labels(Labels, MFA, ConstTab, Size, Align, ConstNo, Acc, []),
  NewRefs =
    case RefToLabels of
      [] -> Refs;
      _ -> [{MFA,RefToLabels}|Refs]
    end,
  pack_constants(Rest, NewSize, NewAlign, NewConstNo, Map, NewRefs);
pack_constants([], Size, Align, _, Acc, Refs) ->
  {Align, Size, Acc, Refs}.

%%
%% pack_labels converts a ConstTab to a packed ConstMap, which
%% maps {MFA,Label} pairs to information about individual constants,
%% including their ConstNo and start offset in the constants pool.
%%
pack_labels([{_Label,ref}|Labels],MFA,ConstTab,Size,Align,ConstNo,Acc, Refs) ->
  pack_labels(Labels, MFA, ConstTab, Size, Align, ConstNo, Acc, Refs);
pack_labels([Label|Labels],MFA,ConstTab,AccSize,OldAlign,ConstNo, Acc, Refs) ->
  Const = hipe_consttab:lookup(Label, ConstTab),
  Align = hipe_consttab:const_align(Const),
  NewAlign = erlang:max(Align, OldAlign),
  Start = 
    case AccSize rem Align of
      0 -> AccSize;
      N -> AccSize + (Align - N)
    end,
  %% io:format("Const ~w\n", [Const]),
  RawType = hipe_consttab:const_type(Const),
  Type = ?CONST_TYPE2EXT(RawType),
  RawData = hipe_consttab:const_data(Const),
  case RawType of
    term ->
      %% If the constant term is already in the constant map we want
      %% to use the same constant number so that, in the end, the
      %% constant term is not duplicated.
      case lists:keyfind(RawData, 7, Acc) of
	false ->
	  NewInfo = #pcm_entry{mfa=MFA, label=Label, const_num=ConstNo,
			       start=0, type=Type, raw_data=RawData},
	  pack_labels(Labels, MFA, ConstTab, AccSize, OldAlign, ConstNo+1,
		      [NewInfo|Acc], Refs);
	#pcm_entry{const_num=OtherConstNo, type=Type, raw_data=RawData} ->
	  NewInfo = #pcm_entry{mfa=MFA, label=Label, const_num=OtherConstNo,
			       start=0, type=Type, raw_data=RawData},
	  pack_labels(Labels, MFA, ConstTab, AccSize, OldAlign, ConstNo,
		      [NewInfo|Acc], Refs);
	_ ->
	  NewInfo = #pcm_entry{mfa=MFA, label=Label, const_num=ConstNo,
			       start=0, type=Type, raw_data=RawData},
	  pack_labels(Labels, MFA, ConstTab, AccSize, OldAlign, ConstNo+1,
		      [NewInfo|Acc], Refs)
      end;
    sorted_block ->
      Need = hipe_consttab:const_size(Const),
      NewInfo = #pcm_entry{mfa=MFA, label=Label, const_num=ConstNo,
			   start=Start, type=Type, raw_data=RawData},
      pack_labels(Labels, MFA, ConstTab, Start+Need, NewAlign, ConstNo+1,
		  [NewInfo|Acc], Refs);
    block ->
      Need = hipe_consttab:const_size(Const),
      {Data, NewRefs} =
	case RawData of
	  {ElementType, ElementData} ->
	    decompose_block(ElementType, ElementData, Start);
	  {ElementType, ElementData, SortOrder} ->
	    {TblData, TblRefs} = get_sorted_refs(ElementData, SortOrder),
	    {hipe_consttab:decompose({ElementType, TblData}),
	     [{sorted,Start,TblRefs}]}
	end,
      NewInfo = #pcm_entry{mfa=MFA, label=Label, const_num=ConstNo,
			   start=Start, type=Type, raw_data=Data},
      pack_labels(Labels, MFA, ConstTab, Start+Need, NewAlign, ConstNo+1,
		  [NewInfo|Acc], NewRefs++Refs)
  end;
pack_labels([], _, _, Size, Align, ConstNo, Acc, Refs) ->
  {Size, Align, Acc, ConstNo, Refs}.

decompose_block(ElementType, Data, Addr) ->
  ElementSize = hipe_consttab:size_of(ElementType),
  {NewData, Refs} = get_refs(Data, Addr, ElementSize),
  {hipe_consttab:decompose({ElementType, NewData}), Refs}.

get_refs([{label,L}|Rest], Pos, ElementSize) ->
  {NewData, Refs} = get_refs(Rest, Pos+ElementSize, ElementSize),
  {[0|NewData], [{L,Pos}|Refs]};
get_refs([D|Rest], Pos, ElementSize) ->
  {NewData, Refs} = get_refs(Rest, Pos+ElementSize, ElementSize),
  {[D|NewData], Refs};
get_refs([], _, _) ->
  {[],[]}.

get_sorted_refs([{label,L}|Rest], [Ordering|Os]) ->
  {NewData, Refs} = get_sorted_refs(Rest, Os),
  {[0|NewData], [{L,Ordering}|Refs]};
get_sorted_refs([D|Rest], [_Ordering|Os]) ->
  {NewData, Refs} = get_sorted_refs(Rest, Os),
  {[D|NewData], Refs};
get_sorted_refs([], []) ->
  {[], []}.

-type ref_type() :: 0..4.

-spec slim_refs([{ref_type(),non_neg_integer(),term()}]) ->
	[{ref_type(), [{term(), [non_neg_integer()]}]}].
slim_refs([]) -> [];
slim_refs(Refs) ->
  [Ref|Rest] = lists:keysort(1, Refs),
  compact_ref_types(Rest, element(1, Ref), [Ref], []).

compact_ref_types([Ref|Refs], Type, AccofType, Acc) ->
  case element(1, Ref) of
    Type ->
      compact_ref_types(Refs, Type, [Ref|AccofType], Acc);
    NewType ->
      compact_ref_types(Refs, NewType, [Ref],
			[{Type,lists:sort(compact_dests(AccofType))}|Acc])
  end;
compact_ref_types([], Type, AccofType ,Acc) ->
  [{Type,lists:sort(compact_dests(AccofType))}|Acc].


%% compact_dests([]) -> [];	% clause is redundant
compact_dests(Refs) ->
  [Ref|Rest] = lists:keysort(3, Refs),
  compact_dests(Rest, element(3,Ref), [element(2,Ref)], []).

compact_dests([Ref|Refs], Dest, AccofDest, Acc) ->
  case element(3, Ref) of
    Dest ->
      compact_dests(Refs, Dest, [element(2,Ref)|AccofDest], Acc);
    NewDest ->
      compact_dests(Refs, NewDest, [element(2,Ref)], [{Dest,AccofDest}|Acc])
  end;
compact_dests([], Dest, AccofDest, Acc) ->
  [{Dest,AccofDest}|Acc].

%%
%% slim_constmap/1 takes a packed ConstMap, as produced by pack_labels
%% called from hipe_pack_constants:pack_constants/2, and converts it
%% to the slimmed and flattened format ConstMap which is put in object
%% files.
%%
-spec slim_constmap([#pcm_entry{}]) -> [raw_data()].
slim_constmap(Map) ->
  slim_constmap(Map, gb_sets:new(), []).

-spec slim_constmap([#pcm_entry{}], gb_set(), [raw_data()]) -> [raw_data()].
slim_constmap([#pcm_entry{const_num=ConstNo, start=Offset,
			  type=Type, raw_data=Term}|Rest], Inserted, Acc) ->
  case gb_sets:is_member(ConstNo, Inserted) of
    true ->
      slim_constmap(Rest, Inserted, Acc);
    false ->
      NewInserted = gb_sets:insert(ConstNo, Inserted),
      slim_constmap(Rest, NewInserted, [ConstNo, Offset, Type, Term|Acc])
  end;
slim_constmap([], _Inserted, Acc) -> Acc.
