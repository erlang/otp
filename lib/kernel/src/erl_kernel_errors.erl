%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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

-module(erl_kernel_errors).
-export([format_error/2]).

-spec format_error(Reason, StackTrace) -> ErrorMap when
      Reason :: term(),
      StackTrace :: [term()],
      ErrorMap :: #{pos_integer() => unicode:chardata()}.

format_error(_Reason, [{M,F,As,Info}|_]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfoMap, none),
    Res = case M of
              erl_ddll ->
                  format_erl_ddll_error(F, As, Cause);
              os ->
                  format_os_error(F, As, Cause);
              _ ->
                  []
          end,
    format_error_map(Res, 1, #{}).

format_erl_ddll_error(_, _, _) ->
    [].

format_os_error(get_env_var, [_], _) ->
    [invalid_env_var_name];
format_os_error(_, _, _) ->
    [].

%% format_error_map([""|Es], ArgNum, Map) ->
%%     format_error_map(Es, ArgNum + 1, Map);
format_error_map([E|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map#{ArgNum => expand_error(E)});
format_error_map([], _, Map) ->
    Map.

expand_error(invalid_env_var_name) ->
    <<"invalid environment variable name">>.
%% expand_error(Other) ->
%%     Other.
