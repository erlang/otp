%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxartprovider.html">wxArtProvider</a>.
%% @type wxArtProvider().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxArtProvider).
-include("wxe.hrl").
-export([getBitmap/1,getBitmap/2,getIcon/1,getIcon/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxArtProvider/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxArtProvider() :: wx:wx_object().
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
  Id_UC = unicode:characters_to_binary([Id,0]),
  MOpts = fun({client, Client}, Acc) ->   Client_UC = unicode:characters_to_binary([Client, $_, $C,0]),[<<1:32/?UI,(byte_size(Client_UC)):32/?UI,(Client_UC)/binary, 0:(((8- ((0+byte_size(Client_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<2:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxArtProvider_GetBitmap,
  <<(byte_size(Id_UC)):32/?UI,(Id_UC)/binary, 0:(((8- ((4+byte_size(Id_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

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
  Id_UC = unicode:characters_to_binary([Id,0]),
  MOpts = fun({client, Client}, Acc) ->   Client_UC = unicode:characters_to_binary([Client, $_, $C,0]),[<<1:32/?UI,(byte_size(Client_UC)):32/?UI,(Client_UC)/binary, 0:(((8- ((0+byte_size(Client_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<2:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxArtProvider_GetIcon,
  <<(byte_size(Id_UC)):32/?UI,(Id_UC)/binary, 0:(((8- ((4+byte_size(Id_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

