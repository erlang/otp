%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(nif_mod).

-include_lib("common_test/include/ct.hrl").

-export([load_nif_lib/2, load_nif_lib/3, start/0, lib_version/0,
	 get_priv_data_ptr/0, make_new_resource/2, get_resource/2]).

-export([loop/0, upgrade/1]).

-define(nif_stub,nif_stub_error(?LINE)).

-ifdef(USE_ON_LOAD).
-on_load(on_load/0).

on_load() ->
    [{data_dir, Path}] = ets:lookup(nif_SUITE, data_dir),
    [{lib_version, Ver}] = ets:lookup(nif_SUITE, lib_version),
    [{nif_api_version, API}] = ets:lookup(nif_SUITE, nif_api_version),
    R = erlang:load_nif(filename:join(Path,libname(Ver,API)), []),
    check_api_version(R, API).

-endif.

check_api_version(Err, _) when Err =/= ok -> Err;
check_api_version(ok, []) -> ok;
check_api_version(ok, [$., MajC, $_ | MinS]) ->
    {Maj, Min} = {list_to_integer([MajC]), list_to_integer(MinS)},
    {Maj, Min} = nif_api_version(),
    ok.

load_nif_lib(Config, Ver) ->
    load_nif_lib(Config, Ver, []).

load_nif_lib(Config, Ver, LoadInfo) ->
    Path = proplists:get_value(data_dir, Config),
    API = proplists:get_value(nif_api_version, Config, ""),
    R = erlang:load_nif(filename:join(Path,libname(Ver,API)), LoadInfo),
    check_api_version(R, API).

libname(no_init,API) -> libname(3,API);
libname(Ver,API) when is_integer(Ver) ->
    "nif_mod." ++ integer_to_list(Ver) ++ API.
    
start() ->
    spawn_opt(?MODULE,loop,[],
	      [link, monitor]).
    
loop() ->
    receive 
	{Pid,lib_version} ->
	    Pid ! {self(),lib_version()},
	    loop();
	{Pid,upgrade} ->
	    ?MODULE:upgrade(Pid);
        die -> 
	    void
    end.

upgrade(Pid) ->
    Pid ! {self(),upgraded},
    loop().

lib_version() ->  % NIF
    undefined.

nif_api_version() -> %NIF
    {undefined,undefined}.

get_priv_data_ptr() -> ?nif_stub.
make_new_resource(_,_) -> ?nif_stub.
get_resource(_,_) -> ?nif_stub.

nif_stub_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).
