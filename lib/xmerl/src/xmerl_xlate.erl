%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

