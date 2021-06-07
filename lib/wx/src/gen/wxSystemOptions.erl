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

-module(wxSystemOptions).
-include("wxe.hrl").
-export([getOption/1,getOptionInt/1,hasOption/1,isFalse/1,setOption/2]).

%% inherited exports
-export([parent_class/1]).

-type wxSystemOptions() :: wx:wx_object().
-export_type([wxSystemOptions/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionsgetoption">external documentation</a>.
-spec getOption(Name) -> unicode:charlist() when
	Name::unicode:chardata().
getOption(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_GetOption),
  wxe_util:rec(?wxSystemOptions_GetOption).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionsgetoptionint">external documentation</a>.
-spec getOptionInt(Name) -> integer() when
	Name::unicode:chardata().
getOptionInt(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_GetOptionInt),
  wxe_util:rec(?wxSystemOptions_GetOptionInt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionshasoption">external documentation</a>.
-spec hasOption(Name) -> boolean() when
	Name::unicode:chardata().
hasOption(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_HasOption),
  wxe_util:rec(?wxSystemOptions_HasOption).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionsisfalse">external documentation</a>.
-spec isFalse(Name) -> boolean() when
	Name::unicode:chardata().
isFalse(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_IsFalse),
  wxe_util:rec(?wxSystemOptions_IsFalse).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionssetoption">external documentation</a>.
%% <br /> Also:<br />
%% setOption(Name, Value) -> 'ok' when<br />
%% 	Name::unicode:chardata(), Value::unicode:chardata().<br />
%% 
-spec setOption(Name, Value) -> 'ok' when
	Name::unicode:chardata(), Value::integer();
      (Name, Value) -> 'ok' when
	Name::unicode:chardata(), Value::unicode:chardata().
setOption(Name,Value)
 when ?is_chardata(Name),is_integer(Value) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,Value,?get_env(),?wxSystemOptions_SetOption_2_0);
setOption(Name,Value)
 when ?is_chardata(Name),?is_chardata(Value) ->
  Name_UC = unicode:characters_to_binary(Name),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(Name_UC,Value_UC,?get_env(),?wxSystemOptions_SetOption_2_1).

