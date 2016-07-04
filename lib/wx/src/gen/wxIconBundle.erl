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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html">wxIconBundle</a>.
%% @type wxIconBundle().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxIconBundle).
-include("wxe.hrl").
-export([addIcon/2,addIcon/3,destroy/1,getIcon/1,getIcon/2,new/0,new/1,new/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxIconBundle/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxIconBundle() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
-spec new() -> wxIconBundle().
new() ->
  wxe_util:construct(?wxIconBundle_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
-spec new(Ic) -> wxIconBundle() when
	Ic::wxIconBundle() | wxIcon:wxIcon().
new(#wx_ref{type=IcT,ref=IcRef}) ->
  IcOP = case ?CLASS_T(IcT,wxIconBundle) of
     true ->
       ?wxIconBundle_new_1_1;
     _ -> ?CLASS(IcT,wxIcon),
       ?wxIconBundle_new_1_0
     end,
  wxe_util:construct(IcOP,
  <<IcRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
-spec new(File, Type) -> wxIconBundle() when
	File::unicode:chardata(), Type::integer().
new(File,Type)
 when is_list(File),is_integer(Type) ->
  File_UC = unicode:characters_to_binary([File,0]),
  wxe_util:construct(?wxIconBundle_new_2,
  <<(byte_size(File_UC)):32/?UI,(File_UC)/binary, 0:(((8- ((4+byte_size(File_UC)) band 16#7)) band 16#7))/unit:8,Type:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundleaddicon">external documentation</a>.
-spec addIcon(This, Icon) -> 'ok' when
	This::wxIconBundle(), Icon::wxIcon:wxIcon().
addIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef}) ->
  ?CLASS(ThisT,wxIconBundle),
  ?CLASS(IconT,wxIcon),
  wxe_util:cast(?wxIconBundle_AddIcon_1,
  <<ThisRef:32/?UI,IconRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundleaddicon">external documentation</a>.
-spec addIcon(This, File, Type) -> 'ok' when
	This::wxIconBundle(), File::unicode:chardata(), Type::integer().
addIcon(#wx_ref{type=ThisT,ref=ThisRef},File,Type)
 when is_list(File),is_integer(Type) ->
  ?CLASS(ThisT,wxIconBundle),
  File_UC = unicode:characters_to_binary([File,0]),
  wxe_util:cast(?wxIconBundle_AddIcon_2,
  <<ThisRef:32/?UI,(byte_size(File_UC)):32/?UI,(File_UC)/binary, 0:(((8- ((0+byte_size(File_UC)) band 16#7)) band 16#7))/unit:8,Type:32/?UI>>).

%% @equiv getIcon(This, [])
-spec getIcon(This) -> wxIcon:wxIcon() when
	This::wxIconBundle().

getIcon(This)
 when is_record(This, wx_ref) ->
  getIcon(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlegeticon">external documentation</a>.
%% <br /> Also:<br />
%% getIcon(This, Size) -> wxIcon:wxIcon() when<br />
%% 	This::wxIconBundle(), Size::{W::integer(), H::integer()}.<br />
%% 
-spec getIcon(This, [Option]) -> wxIcon:wxIcon() when
	This::wxIconBundle(),
	Option :: {'size', integer()};
      (This, Size) -> wxIcon:wxIcon() when
	This::wxIconBundle(), Size::{W::integer(), H::integer()}.
getIcon(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxIconBundle),
  MOpts = fun({size, Size}, Acc) -> [<<1:32/?UI,Size:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxIconBundle_GetIcon_1_0,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>);
getIcon(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxIconBundle),
  wxe_util:call(?wxIconBundle_GetIcon_1_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxIconBundle()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxIconBundle),
  wxe_util:destroy(?wxIconBundle_destruct,Obj),
  ok.
