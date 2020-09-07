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

<<EXPORT:wxTaskBarIcon new/0, new/1 wxTaskBarIcon:EXPORT>>

<<wxTaskBarIcon_new
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html#wxtaskbariconwxtaskbaricon">external documentation</a>.
-spec new() -> wxTaskBarIcon().
new() ->
  wxe_util:construct(~s, <<0:32>>).


%% @doc Creates a TaskBarIcon with a callback function for CreatePopupMenu:
%%   <pre>Callback() -> term()</pre>
%%
%% See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html#wxtaskbariconwxtaskbaricon">external documentation</a>.
-spec new(function()) -> wxTaskBarIcon().
new(F) when is_function(F)->
  Fun = fun([_]) -> 
    #wx_ref{type=wxMenu,ref=ThisRef} = F(),
    <<ThisRef:32/?UI>>
  end,
  BinFun = <<(wxe_util:get_cbId(Fun)):32/?UI, 0:32>>,
  wxe_util:construct(?wxTaskBarIcon_new, BinFun).
wxTaskBarIcon_new>>
