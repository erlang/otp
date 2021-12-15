%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2021. All Rights Reserved.
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
-module(format_status_statem).

%% gen_statem callbacks
-export(['$handle_undefined_function'/2, terminate/3, format_status/1]).

'$handle_undefined_function'(format_status, [_,_]) ->
    erlang:error(undef);
'$handle_undefined_function'(Func, Args) ->
    apply(gen_statem_SUITE, Func, Args).

terminate(Reason, State, Data) ->
    gen_statem_SUITE:terminate(Reason, State, Data).

format_status(#{ data := Fun } = S) when is_function(Fun) ->
    Fun(S);
format_status(#{ reason := _, state := State, data := Data } = Map) ->
    ct:pal("format_status(~p)",[Map]),
    Map#{ state := {formatted, State},  data := {formatted, Data}};
format_status(Map) ->
    ct:pal("format_status(~p)",[Map]),
    Map#{ data := format_data, state := format_status_called }.
