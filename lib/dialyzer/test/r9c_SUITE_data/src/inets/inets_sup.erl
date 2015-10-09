%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: inets_sup.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%
-module(inets_sup).

-export([crock/0]).
-export([start/2, stop/1, init/1]).
-export([start_child/2, stop_child/2, which_children/0]).


%% crock (Used for debugging!)

crock() ->
    application:start(sasl),
    application:start(inets).


%% start

start(Type, State) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% stop

stop(State) ->
    ok.


%% start_child

start_child(ConfigFile, Verbosity) ->
    {ok, Spec} = httpd_child_spec(ConfigFile, Verbosity),
    supervisor:start_child(?MODULE, Spec).


%% stop_child

stop_child(Addr, Port) ->
    Name = {httpd_sup, Addr, Port},
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:delete_child(?MODULE, Name);
        Error ->
            Error
    end.


%% which_children

which_children() ->
    supervisor:which_children(?MODULE).


%% init

init([]) ->
    case get_services() of
	{error, Reason} ->
	    {error,Reason};
	Services ->
	    SupFlags = {one_for_one, 10, 3600},
	    {ok, {SupFlags, child_spec(Services, [])}}
    end.

get_services() ->
    case (catch application:get_env(inets, services)) of
	{ok, Services} ->
	    Services;
	_ ->
	    []
    end.


child_spec([], Acc) ->
    Acc;
child_spec([{httpd, ConfigFile, Verbosity}|Rest], Acc) ->
    case httpd_child_spec(ConfigFile, Verbosity) of
	{ok, Spec} ->
	    child_spec(Rest, [Spec | Acc]);
	{error, Reason} ->
	    error_msg("Failed creating child spec "
		      "using ~p for reason: ~p", [ConfigFile, Reason]),
	    child_spec(Rest, Acc)
    end;
child_spec([{httpd, ConfigFile}|Rest], Acc) ->
    case httpd_child_spec(ConfigFile, []) of
	{ok, Spec} ->
	    child_spec(Rest, [Spec | Acc]);
	{error, Reason} ->
	    error_msg("Failed creating child spec "
		      "using ~p for reason: ~p", [ConfigFile, Reason]),
	    child_spec(Rest, Acc)
    end.


httpd_child_spec(ConfigFile, Verbosity) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    Port = httpd_util:key1search(ConfigList, port, 80),
	    Addr = httpd_util:key1search(ConfigList, bind_address),
	    {ok, httpd_child_spec(ConfigFile, Addr, Port, Verbosity)};
	Error ->
	    Error
    end.


httpd_child_spec(ConfigFile, Addr, Port, Verbosity) ->
    {{httpd_sup, Addr, Port},{httpd_sup, start_link,[ConfigFile, Verbosity]},
     permanent, 20000, supervisor,
     [ftp,
      httpd,
      httpd_conf,
      httpd_example,
      httpd_manager,
      httpd_misc_sup,
      httpd_listener,
      httpd_parse,
      httpd_request,
      httpd_response,
      httpd_socket,
      httpd_sup,
      httpd_util,
      httpd_verbosity,
      inets_sup,
      mod_actions,
      mod_alias,
      mod_auth,
      mod_cgi,
      mod_dir,
      mod_disk_log,
      mod_esi,
      mod_get,
      mod_head,
      mod_include,
      mod_log,
      mod_auth_mnesia,
      mod_auth_plain,
      mod_auth_dets,
      mod_security]}.


error_msg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).
