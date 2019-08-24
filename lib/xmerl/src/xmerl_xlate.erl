%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%%% Description  : Wrapper to xmerl_scan, which reads an XML document
%%%                from disk (alt. consumes a string), processes it, and
%%%                exports is using the specified Callback module.
-module(xmerl_xlate).


-export([file/3,
	 string/3]).


-include("xmerl.hrl").

file(F, Title, Callback) ->
    case file:read_file(F) of
	{ok, Bin} ->
	    string(binary_to_list(Bin), Title, Callback);
	Error ->
	    Error
    end.

string(Str, Title, Callback) ->
    xmerl_scan:string(Str, [{hook_fun, fun hook/2, {Title, Callback}}]).


hook(E = #xmlElement{parents = []}, S) ->
    {Title, Callback} = xmerl_scan:hook_state(S),
    Data = xmerl:export([E], Callback, [{title, Title}]),
    {Data, S};
hook(X, S) ->
    {X, S}.

