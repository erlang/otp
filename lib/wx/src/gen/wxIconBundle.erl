%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxiconbundle.html">wxIconBundle</a>.
%% @type wxIconBundle().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxIconBundle).
-include("wxe.hrl").
-export([addIcon/2,addIcon/3,destroy/1,getIcon/1,getIcon/2,new/0,new/1,new/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxIconBundle()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
new() ->
  wxe_util:construct(?wxIconBundle_new_0,
  <<>>).

%% @spec (Ic::wxIconBundle() | wxIcon:wxIcon()) -> wxIconBundle()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
new(#wx_ref{type=IcT,ref=IcRef}) ->
  IcOP = case ?CLASS_T(IcT,wxIconBundle) of
     true ->
       ?wxIconBundle_new_1_1;
     _ -> ?CLASS(IcT,wxIcon),
       ?wxIconBundle_new_1_0
     end,
  wxe_util:construct(IcOP,
  <<IcRef:32/?UI>>).

%% @spec (File::string(), Type::integer()) -> wxIconBundle()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
new(File,Type)
 when is_list(File),is_integer(Type) ->
  File_UC = unicode:characters_to_binary([File,0]),
  wxe_util:construct(?wxIconBundle_new_2,
  <<(byte_size(File_UC)):32/?UI,(File_UC)/binary, 0:(((8- ((4+byte_size(File_UC)) band 16#7)) band 16#7))/unit:8,Type:32/?UI>>).

%% @spec (This::wxIconBundle(), Icon::wxIcon:wxIcon()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxiconbundle.html#wxiconbundleaddicon">external documentation</a>.
addIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef}) ->
  ?CLASS(ThisT,wxIconBundle),
  ?CLASS(IconT,wxIcon),
  wxe_util:cast(?wxIconBundle_AddIcon_1,
  <<ThisRef:32/?UI,IconRef:32/?UI>>).

%% @spec (This::wxIconBundle(), File::string(), Type::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxiconbundle.html#wxiconbundleaddicon">external documentation</a>.
addIcon(#wx_ref{type=ThisT,ref=ThisRef},File,Type)
 when is_list(File),is_integer(Type) ->
  ?CLASS(ThisT,wxIconBundle),
  File_UC = unicode:characters_to_binary([File,0]),
  wxe_util:cast(?wxIconBundle_AddIcon_2,
  <<ThisRef:32/?UI,(byte_size(File_UC)):32/?UI,(File_UC)/binary, 0:(((8- ((0+byte_size(File_UC)) band 16#7)) band 16#7))/unit:8,Type:32/?UI>>).

%% @spec (This::wxIconBundle()) -> wxIcon:wxIcon()
%% @equiv getIcon(This, [])
getIcon(This)
 when is_record(This, wx_ref) ->
  getIcon(This, []).

%% @spec (This::wxIconBundle(),X::term()) -> wxIcon:wxIcon()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxiconbundle.html#wxiconbundlegeticon">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% getIcon(This::wxIconBundle(), [Option]) -> wxIcon:wxIcon() </c>
%%<br /> Option = {size, integer()}
%% </p>
%% <p><c>
%% getIcon(This::wxIconBundle(), Size::{W::integer(),H::integer()}) -> wxIcon:wxIcon() </c>
%% </p>
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

%% @spec (This::wxIconBundle()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxIconBundle),
  wxe_util:destroy(?wxIconBundle_destruct,Obj),
  ok.
