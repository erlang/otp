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

<<EXPORT:wxTaskBarIcon new/0, new/1 wxTaskBarIcon:EXPORT>>

<<wxTaskBarIcon_new
-spec new() -> wxTaskBarIcon().
new() ->
    new([]).

%% @doc Creates a TaskBarIcon with a callback function for CreatePopupMenu:
%%   <pre>Callback() -> term()</pre>
%%
-spec new([Option]) -> wxTaskBarIcon() when
      Option :: {'iconType', wx:wx_enum()} |
                {'createPopupMenu', fun(() -> wxMenu:wxMenu())}.

new(Options) when is_list(Options) ->
    Op = ~s,
    MOpts = fun({iconType, _iconType} = Arg) -> Arg;
               ({createPopupMenu, Fun}) when is_function(Fun) -> {createPopupMenu,  wxe_util:get_cbId(Fun)};
               (BadOpt) -> erlang:error({badoption, BadOpt}) end,
    Opts = lists:map(MOpts, Options),
    wxe_util:queue_cmd(Opts,?get_env(), Op),
    wxe_util:rec(Op).

wxTaskBarIcon_new>>
