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

-module(wxFileDataObject).
-include("wxe.hrl").
-export([addFile/2,destroy/1,getFilenames/1,new/0]).

%% inherited exports
-export([parent_class/1]).

-type wxFileDataObject() :: wx:wx_object().
-export_type([wxFileDataObject/0]).
%% @hidden
parent_class(wxDataObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfiledataobject.html#wxfiledataobjectwxfiledataobject">external documentation</a>.
-spec new() -> wxFileDataObject().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxFileDataObject_new),
  wxe_util:rec(?wxFileDataObject_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfiledataobject.html#wxfiledataobjectaddfile">external documentation</a>.
-spec addFile(This, File) -> 'ok' when
	This::wxFileDataObject(), File::unicode:chardata().
addFile(#wx_ref{type=ThisT}=This,File)
 when ?is_chardata(File) ->
  ?CLASS(ThisT,wxFileDataObject),
  File_UC = unicode:characters_to_binary(File),
  wxe_util:queue_cmd(This,File_UC,?get_env(),?wxFileDataObject_AddFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfiledataobject.html#wxfiledataobjectgetfilenames">external documentation</a>.
-spec getFilenames(This) -> [unicode:charlist()] when
	This::wxFileDataObject().
getFilenames(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDataObject),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDataObject_GetFilenames),
  wxe_util:rec(?wxFileDataObject_GetFilenames).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxFileDataObject()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFileDataObject),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxFileDataObject_destroy),
  ok.
 %% From wxDataObject
