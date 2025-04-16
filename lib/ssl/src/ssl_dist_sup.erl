%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2011-2025. All Rights Reserved.
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

%%

-module(ssl_dist_sup).
-moduledoc false.

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%% Debug
-export([consult/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
			
start_link() ->
    case init:get_argument(ssl_dist_optfile) of
        {ok, [File]} ->
            DistOpts = consult(File),
            TabOpts = [set, protected, named_table],
            Tab = ets:new(ssl_dist_opts, TabOpts),
            true = ets:insert(Tab, DistOpts),
            supervisor:start_link({local, ?MODULE}, ?MODULE, []);
        {ok, BadArg} ->
            error({bad_ssl_dist_optfile, BadArg});
        error ->
            supervisor:start_link({local, ?MODULE}, ?MODULE, [])
    end.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================

init([]) ->    
    SupFlags = #{strategy  => one_for_all, 
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [ssl_admin_child_spec(),
                  ssl_connection_sup()], 
    {ok, {SupFlags, ChildSpecs}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
ssl_admin_child_spec() ->
    #{id       => ssl_dist_admin_sup,
      start    =>  {ssl_dist_admin_sup, start_link , []},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [ssl_dist_admin_sup],
      type     => supervisor
     }.
    
ssl_connection_sup() ->
    #{id       => tls_dist_sup,
      start    => {tls_dist_sup, start_link, []},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [tls_dist_sup],
      type     => supervisor
     }.

consult(File) ->
    case erl_prim_loader:read_file(File) of
        {ok, Binary} ->
            Encoding =
                case epp:read_encoding_from_binary(Binary) of
                    none -> latin1;
                    Enc -> Enc
                end,
            case unicode:characters_to_list(Binary, Encoding) of
                {error, _String, Rest} ->
                    error(
                      {bad_ssl_dist_optfile, {encoding_error, Rest}});
                {incomplete, _String, Rest} ->
                    error(
                      {bad_ssl_dist_optfile, {encoding_incomplete, Rest}});
                String when is_list(String) ->
                    consult_string(String)
            end;
        error ->
            error({bad_ssl_dist_optfile, File})
    end.

consult_string(String) ->
    case erl_scan:string(String) of
        {error, Info, Location} ->
            error({bad_ssl_dist_optfile, {scan_error, Info, Location}});
        {ok, Tokens, _EndLocation} ->
            consult_tokens(Tokens)
    end.

consult_tokens(Tokens) ->
    case erl_parse:parse_exprs(Tokens) of
        {error, Info} ->
            error({bad_ssl_dist_optfile, {parse_error, Info}});
        {ok, [Expr]} ->
            consult_expr(Expr);
        {ok, Other} ->
            error({bad_ssl_dist_optfile, {parse_error, Other}})
    end.

consult_expr(Expr) ->
    {value, Value, Bs} = erl_eval:expr(Expr, erl_eval:new_bindings()),
    case erl_eval:bindings(Bs) of
        [] ->
            Value;
        Other ->
            error({bad_ssl_dist_optfile, {bindings, Other}})
    end.
