%% -*- erlang-indent-level: 2 -*-
%%=============================================================================
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-module(hipe_pack_constants).
-export([pack_constants/2, slim_refs/1, slim_constmap/1,
        find_const/2, mk_data_relocs/2, slim_sorted_exportmap/3]).

-include("hipe_consttab.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../main/hipe.hrl"). % Needed for the EXIT macro in find_const/2.

%%-----------------------------------------------------------------------------

-type const_num() :: non_neg_integer().
-type raw_data()  :: binary() | number() | list() | tuple().

-type addr()      :: non_neg_integer().
-type ref_p()     :: {DataPos :: hipe_constlbl(), CodeOffset :: addr()}.
-type ref()       :: ref_p() | {'sorted', Base :: addr(), [ref_p()]}.

-type mfa_refs()  :: {mfa(), [ref()]}.

%% XXX: these types may not belong here: FIX!
-type fa()         :: {atom(), arity()}.
-type export_map() :: [{addr(), module(), atom(), arity()}].

-record(pcm_entry, {mfa       :: mfa(),
		    label     :: hipe_constlbl(),
                   const_num :: const_num(),
                   start     :: addr(),
		    type      :: 0 | 1 | 2,
		    raw_data  :: raw_data()}).
-type pcm_entry() :: #pcm_entry{}.

-type label_map() :: gb_trees:tree({mfa(), hipe_constlbl()}, addr()).

%% Some of the following types may possibly need to be exported
-type data_relocs()      :: [ref()].
-type packed_const_map() :: [pcm_entry()].
-type mfa_refs_map()     :: [mfa_refs()].
-type slim_export_map()  :: [addr() | module() | atom() | arity() | boolean()].

%%-----------------------------------------------------------------------------

-spec pack_constants([{mfa(),[_],hipe_consttab()}], ct_alignment()) ->
       {ct_alignment(), non_neg_integer(), packed_const_map(), mfa_refs_map()}.

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
-spec slim_constmap(packed_const_map()) -> [raw_data()].
slim_constmap(Map) ->
  slim_constmap(Map, gb_sets:new(), []).

slim_constmap([#pcm_entry{const_num = ConstNo, start = Offset,
			  type = Type, raw_data = Term}|Rest], Inserted, Acc) ->
  case gb_sets:is_member(ConstNo, Inserted) of
    true ->
      slim_constmap(Rest, Inserted, Acc);
    false ->
      NewInserted = gb_sets:insert(ConstNo, Inserted),
      slim_constmap(Rest, NewInserted, [ConstNo, Offset, Type, Term|Acc])
  end;
slim_constmap([], _Inserted, Acc) -> Acc.

%%
%% Lookup a constant in a ConstMap.
%%
-spec find_const({mfa(), hipe_constlbl()}, packed_const_map()) -> const_num().

find_const({MFA, Label}, [E = #pcm_entry{mfa = MFA, label = Label}|_]) ->
  E#pcm_entry.const_num;
find_const(N, [_|R]) ->
  find_const(N, R);
find_const(C, []) ->
  ?EXIT({constant_not_found, C}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Functions to build and handle Refs, ExportMap and LabelMap.
%% Note: Moved here because they are used by all backends in
%% hipe_{arm,sparc,ppc,x86}_assemble.erl
%% XXX: Is this the right place for them?
%%

-spec mk_data_relocs(mfa_refs_map(), label_map()) -> data_relocs().

mk_data_relocs(RefsFromConsts, LabelMap) ->
  lists:flatten(mk_data_relocs(RefsFromConsts, LabelMap, [])).

mk_data_relocs([{MFA, Labels} | Rest], LabelMap, Acc) ->
  Map = [case Label of
	   {L,Pos} ->
	     Offset = find({MFA,L}, LabelMap),
	     {Pos,Offset};
	   {sorted,Base,OrderedLabels} ->
	     {sorted, Base, [begin
			       Offset = find({MFA,L}, LabelMap),
			       {Order, Offset}
			     end
			     || {L,Order} <- OrderedLabels]}
	 end
	 || Label <- Labels],
  %% msg("Map: ~w Map\n", [Map]),
  mk_data_relocs(Rest, LabelMap, [Map,Acc]);
mk_data_relocs([], _, Acc) -> Acc.

find({MFA,L}, LabelMap) ->
  gb_trees:get({MFA,L}, LabelMap).

-spec slim_sorted_exportmap(export_map(), [mfa()], [fa()]) -> slim_export_map().

slim_sorted_exportmap([{Addr,M,F,A}|Rest], Closures, Exports) ->
  IsClosure = lists:member({M,F,A}, Closures),
  IsExported = is_exported(F, A, Exports),
  [Addr,M,F,A,IsClosure,IsExported | slim_sorted_exportmap(Rest, Closures, Exports)];
slim_sorted_exportmap([], _, _) -> [].

is_exported(F, A, Exports) ->
  lists:member({F,A}, Exports).
