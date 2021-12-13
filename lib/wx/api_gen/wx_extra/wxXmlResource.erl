
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2021. All Rights Reserved.
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
<<EXPORT:xrcctrl xrcctrl/3 xrcctrl:EXPORT>>

<<xrcctrl
-spec xrcctrl(Window, Name, Type) -> wx:wx_object() when
      Window::wxWindow:wxWindow(),
      Name::string(),
      Type::atom().

xrcctrl(Window = #wx_ref{}, Name, Type) when is_list(Name), is_atom(Type) ->
    %% Func Id ~s 
    ID  = wxXmlResource:getXRCID(Name),
    Res = wxWindow:findWindow(Window,ID),
    wx:typeCast(Res, Type).

xrcctrl>>


