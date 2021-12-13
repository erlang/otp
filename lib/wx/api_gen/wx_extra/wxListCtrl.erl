%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2021. All Rights Reserved.
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
-spec sortItems(This::wxListCtrl(), SortCallBack) -> boolean()
              when SortCallBack :: fun((integer(), integer()) -> integer()).
sortItems(#wx_ref{type=ThisT}=This, SortCallBack)
  when is_function(SortCallBack, 2) ->
    ?CLASS(ThisT,wxListCtrl),
    SortId = wxe_util:get_cbId(SortCallBack),
    Op = ~s,
    wxe_util:queue_cmd(This, SortId, ?get_env(), Op),
    wxe_util:rec(Op).

SortItems>>

<<EXPORT:wxListCtrl new/0, new/1, new/2 wxListCtrl:EXPORT>>

<<wxListCtrl_new_0
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlwxlistctrl">external documentation</a>.
-spec new() -> wxListCtrl().
new() ->
    Op = ~s,
    wxe_util:queue_cmd(?get_env(), Op),
    wxe_util:rec(Op).

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

new(#wx_ref{}=Parent, Options)
  when is_list(Options)->
    %% ~s
    ListCtrl = new(),
    true = create(ListCtrl,Parent,Options),
    ListCtrl.

wxListCtrl_new_2>>

<<EXPORT:Create create/2, create/3 Create:EXPORT>>

<<Create
%% @equiv create(This,Parent, [])
-spec create(This, Parent) -> boolean() when
      This::wxWindow:wxWindow(),
      Parent::wxWindow:wxWindow().
create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlcreate">external documentation</a>.
-spec create(This, Parent, [Option]) -> boolean() when
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

create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
    ?CLASS(ThisT,wxListCtrl),
    ?CLASS(ParentT,wxWindow),
    Op = ~s,
    MOpts = fun({winid, _} = Arg) -> Arg;
               ({pos, {_posX,_posY}} = Arg) -> Arg;
               ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
               ({style, _style} = Arg) -> Arg;
               ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
               ({onGetItemText, Fun}) ->
                    ToStr = fun(A,B,C) -> unicode:characters_to_binary(Fun(A,B,C)) end,
                    {onGetItemText, wxe_util:get_cbId(ToStr)};
               ({onGetItemAttr, Fun}) -> {onGetItemAttr, wxe_util:get_cbId(Fun)};
               ({onGetItemColumnImage, Fun}) -> {onGetItemColumnImage, wxe_util:get_cbId(Fun)};
               (BadOpt) -> erlang:error({badoption, BadOpt}) end,
    Opts = lists:map(MOpts, Options),
    wxe_util:queue_cmd(This, Parent, Opts, ?get_env(), Op),
    wxe_util:rec(Op).

Create>>
