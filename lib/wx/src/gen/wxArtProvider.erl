%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

-module(wxArtProvider).
-include("wxe.hrl").
-export([getBitmap/1,getBitmap/2,getIcon/1,getIcon/2]).

%% inherited exports
-export([parent_class/1]).

-type wxArtProvider() :: wx:wx_object().
-export_type([wxArtProvider/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv getBitmap(Id, [])
-spec getBitmap(Id) -> wxBitmap:wxBitmap() when
	Id::unicode:chardata().

getBitmap(Id)
 when ?is_chardata(Id) ->
  getBitmap(Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxartprovider.html#wxartprovidergetbitmap">external documentation</a>.
-spec getBitmap(Id, [Option]) -> wxBitmap:wxBitmap() when
	Id::unicode:chardata(),
	Option :: {'client', unicode:chardata()}
		 | {'size', {W::integer(), H::integer()}}.
getBitmap(Id, Options)
 when ?is_chardata(Id),is_list(Options) ->
  Id_UC = unicode:characters_to_binary(Id),
  MOpts = fun({client, Client}) ->   Client_UC = unicode:characters_to_binary([Client, $_, $C]),{client,Client_UC};
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Id_UC, Opts,?get_env(),?wxArtProvider_GetBitmap),
  wxe_util:rec(?wxArtProvider_GetBitmap).

%% @equiv getIcon(Id, [])
-spec getIcon(Id) -> wxIcon:wxIcon() when
	Id::unicode:chardata().

getIcon(Id)
 when ?is_chardata(Id) ->
  getIcon(Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxartprovider.html#wxartprovidergeticon">external documentation</a>.
-spec getIcon(Id, [Option]) -> wxIcon:wxIcon() when
	Id::unicode:chardata(),
	Option :: {'client', unicode:chardata()}
		 | {'size', {W::integer(), H::integer()}}.
getIcon(Id, Options)
 when ?is_chardata(Id),is_list(Options) ->
  Id_UC = unicode:characters_to_binary(Id),
  MOpts = fun({client, Client}) ->   Client_UC = unicode:characters_to_binary([Client, $_, $C]),{client,Client_UC};
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Id_UC, Opts,?get_env(),?wxArtProvider_GetIcon),
  wxe_util:rec(?wxArtProvider_GetIcon).

