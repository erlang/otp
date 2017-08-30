%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

<<EXPORT:SortItems sortItems/2 SortItems:EXPORT>>

<<SortItems
%% @spec (This::wxListCtrl(), SortCallBack::function()) -> boolean()
%% @doc Sort the items in the list control<br />
%%   <pre>SortCallBack(Item1,Item2) -> integer()</pre>
%%  <br /> SortCallBack receives the client data associated with two items
%%         to compare, and should return 0 if the items are equal, a negative
%%         value if the first item is less than the second one and a positive
%%         value if the first item is greater than the second one.
%%  <br /> NOTE: The callback may not call other (wx) processes.
sortItems(#wx_ref{type=ThisT,ref=ThisRef}, SortCallBack)
  when is_function(SortCallBack, 2) ->
	?CLASS(ThisT,wxListCtrl),
	Sort = fun([Item1,Item2]) ->
			Result = SortCallBack(Item1,Item2),
			<<Result:32/?UI>>
		end,
	SortId = wxe_util:get_cbId(Sort),
	wxe_util:call(~s, <<ThisRef:32/?UI,SortId:32/?UI>>).
SortItems>>

<<EXPORT:wxListCtrl new/0, new/1, new/2 wxListCtrl:EXPORT>>

<<wxListCtrl_new_0
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlwxlistctrl">external documentation</a>.
-spec new() -> wxListCtrl().
new() ->
    wxe_util:construct(~s, <<>>).
wxListCtrl_new_0>>

<<wxListCtrl_new_2
-spec new(Parent) -> wxListCtrl() when
      Parent::wxWindow:wxWindow().
new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @doc Creates a listctrl with optional callback functions:
%%
%% OnGetItemText = (This, Item, Column) -> unicode:charlist()
%% OnGetItemAttr = (This, Item) -> wxListItemAttr:wxListItemAttr()
%% OnGetItemColumnImage = (This, Item, Column) -> integer()
%%
%% See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlwxlistctrl">external documentation</a>.
-spec new(Parent, [Option]) -> wxListCtrl() when
      Parent::wxWindow:wxWindow(),
      Option::{winid, integer()} |
	      {pos, {X::integer(),Y::integer()}} |
	      {size, {W::integer(),H::integer()}} |
	      {style, integer()} |
	      {validator, wx:wx_object()} |
	      {onGetItemText, function()} |
	      {onGetItemAttr, function()} |
	      {onGetItemColumnImage, function()}.

new(#wx_ref{type=ParentT,ref=ParentRef}, Options)
  when is_list(Options)->
    ?CLASS(ParentT,wxWindow),
    MOpts = fun({winid, Winid}, Acc) -> [<<1:32/?UI,Winid:32/?UI>>|Acc];
	       ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
	       ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
	       ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
	       ({validator, #wx_ref{type=ValidatorT,ref=ValidatorRef}}, Acc) ->
		    ?CLASS(ValidatorT,wx),[<<5:32/?UI,ValidatorRef:32/?UI>>|Acc];
	       ({onGetItemText, F}, Acc) when is_function(F) ->
		    Fun = fun([This,Item,Col]) -> unicode:characters_to_binary([F(This,Item,Col),0]) end,
		    [<<6:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onGetItemAttr, F}, Acc) when is_function(F) ->
		    Fun = fun([This,Item]) ->
				  #wx_ref{type=wxListItemAttr,ref=ThisRef} = F(This,Item),
				  <<ThisRef:32/?UI>>
			  end,
		    [<<7:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onGetItemColumnImage, F}, Acc) when is_function(F) ->
		    Fun = fun([This,Item, Col]) -> <<(F(This,Item,Col)):32/?I>> end,
		    [<<8:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
    BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
    wxe_util:construct(~s, <<ParentRef:32/?UI, 0:32,BinOpt/binary>>).

wxListCtrl_new_2>>

<<EXPORT:Create create/2, create/3 Create:EXPORT>>

<<Create
%% @equiv create(This,Parent, [])
-spec create(This, Parent) -> wxListCtrl() when
      This::wxWindow:wxWindow(),
      Parent::wxWindow:wxWindow().
create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlcreate">external documentation</a>.
-spec create(This, Parent, [Option]) -> wxListCtrl() when
      This::wxWindow:wxWindow(),
      Parent::wxWindow:wxWindow(),
      Option::{winid, integer()} |
	      {pos, {X::integer(),Y::integer()}} |
	      {size, {W::integer(),H::integer()}} |
	      {style, integer()} |
	      {validator, wx:wx_object()} |
	      {onGetItemText, function()} |
	      {onGetItemAttr, function()} |
	      {onGetItemColumnImage, function()}.

create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({winid, Winid}, Acc) -> [<<1:32/?UI,Winid:32/?UI>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          ({validator, #wx_ref{type=ValidatorT,ref=ValidatorRef}}, Acc) ->   ?CLASS(ValidatorT,wx),[<<5:32/?UI,ValidatorRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(~s,
  <<ThisRef:32/?UI,ParentRef:32/?UI, BinOpt/binary>>).

Create>>
