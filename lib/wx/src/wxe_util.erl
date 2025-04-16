%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : wxe_util.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created :  9 Feb 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%% @hidden
-module(wxe_util).
-moduledoc false.

-export([register_pid/1,rec/1,
	 connect_cb/2,disconnect_cb/2,
         color/1,
         get_cbId/1,
         setup_consts/0, get_const/1, get_name/2]).

-export([queue_cmd/1,queue_cmd/2,queue_cmd/3,queue_cmd/4,queue_cmd/5,
         queue_cmd/6,queue_cmd/7,queue_cmd/8,queue_cmd/9,queue_cmd/10,
         queue_cmd/11,queue_cmd/12,queue_cmd/13,queue_cmd/14,queue_cmd/15,
         make_env/0, delete_env/1, get_consts/0,
         debug_ping/0, debug_driver/1,
         init_opengl/2
        ]).

-nifs([queue_cmd/1,queue_cmd/2,queue_cmd/3,queue_cmd/4,queue_cmd/5,
       queue_cmd/6,queue_cmd/7,queue_cmd/8,queue_cmd/9,queue_cmd/10,
       queue_cmd/11,queue_cmd/12,queue_cmd/13,queue_cmd/14,queue_cmd/15,
       make_env/0, delete_env/1, debug_driver/1, get_consts_impl/0,
       init_opengl/2
      ]).

-export([priv_dir/2, opt_error_log/3, init_nif/1]).

-include("wxe.hrl").

-define(NIF_ERROR, erlang:nif_error({nif_not_loaded,{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
                                     {line,?LINE}})).

init_nif(Silent) ->
    {PrivDir, NifDir} = priv_dir("wxe_driver", Silent),
    os:putenv("WX_PRIV_DIR", unicode:characters_to_list(PrivDir)),
    erlang:load_nif(filename:join(NifDir, "wxe_driver"), 0).


color({R,G,B}) -> {R,G,B,255};
color(RGBA) -> RGBA.

get_name(Where, Id) when is_atom(Where), is_integer(Id) ->
    {_Atom2Id, EnumId2Atom} = persistent_term:get(wx_consts),
    maps:get(Id, maps:get(Where, EnumId2Atom, undefined), undefined).

get_const(Atom) when is_atom(Atom) ->
    {Atom2Id, _EnumId2Atom} = persistent_term:get(wx_consts),
    maps:get(Atom, Atom2Id).

setup_consts() ->
    All = get_consts(),
    Atom2Int = [{Key,Val} || {_Enum, Key, Val} <- All],
    EKV = [{Enum, {Key,Val}} || {Enum, Key, Val} <- All],
    Families = sofs:to_external(sofs:relation_to_family(sofs:relation(EKV))),
    MakeId2Enum = fun(List) -> maps:from_list([{V,Key} || {Key,V} <- List]) end,
    FamMaps = [{Enum, MakeId2Enum(KeyList)} || {Enum,KeyList} <- Families, Enum =/= define, Enum =/= global],
    persistent_term:put(wx_consts, {maps:from_list(Atom2Int), maps:from_list(FamMaps)}),
    ok.

get_consts() ->
    get_consts_impl(),
    rec(?WXE_GET_CONSTS).

init_opengl(_,_) -> ?NIF_ERROR.
get_consts_impl() -> ?NIF_ERROR.

debug_ping() -> queue_cmd(?WXE_DEBUG_PING).
debug_driver(_Level) -> ?NIF_ERROR.

make_env() -> ?NIF_ERROR.
delete_env(_EnvRef) -> ?NIF_ERROR.

queue_cmd(_) -> ?NIF_ERROR.
queue_cmd(_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_,_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> ?NIF_ERROR.
queue_cmd(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> ?NIF_ERROR.

rec(Op) ->
    receive
	{'_wxe_result_', Res} -> Res;
	{'_wxe_error_', Op, MFA, Error} ->
	    erlang:error({Error, MFA});
	{'_wxe_error_', Op, {M,F,A}, Error} ->
	    Msg = io_lib:format("~p in ~w:~w/~w", [Error, M, F, A]),
	    wxe_master ! {wxe_driver, error, Msg},
	    rec(Op)
    end.

register_pid(#wx_ref{ref=Index}) ->
    queue_cmd(Index, ?get_env(), ?WXE_REGISTER_OBJECT),
    rec(?WXE_REGISTER_OBJECT).

get_cbId(Fun) ->
    gen_server:call((wx:get_env())#wx_env.sv,{register_cb, Fun}, infinity).

connect_cb(Object,EvData0 = #evh{cb=Callback}) ->
    Server = (wx:get_env())#wx_env.sv,
    case Callback of
	0 -> %% Message api connect from this process
	    case wxEvtHandler:connect_impl(Object,EvData0) of
		{ok, Listener} ->
		    EvData = EvData0#evh{handler=Listener, userdata=undefined},
		    gen_server:call(Server, {connect_cb,Object,EvData}, infinity);
		Error ->
		    Error
	    end;
	_ -> %% callback, fun or pid (pid for wx_object:sync_events masked callbacks)
	    %% let the server do the connect
	    gen_server:call(Server, {connect_cb,Object,EvData0}, infinity)
    end.

disconnect_cb(Object,EvData) ->
    Server = (wx:get_env())#wx_env.sv,
    {try_in_order, Handlers} =
	gen_server:call(Server, {disconnect_cb,Object,EvData}, infinity),
    disconnect(Object, Handlers).

disconnect(Object,[Ev|Evs]) ->
    try wxEvtHandler:disconnect_impl(Object,Ev) of
	true ->  true;
	false -> disconnect(Object, Evs);
	Error -> Error
    catch _:_ ->
	    false
    end;
disconnect(_, []) -> false.

priv_dir(Driver0, Silent) ->
    {file, Path} = code:is_loaded(?MODULE),
    Priv = case filelib:is_regular(Path) of
	       true ->
		   Beam = filename:join(["ebin/",atom_to_list(?MODULE) ++ ".beam"]),
		   filename:join(strip(Path, Beam), "priv");
	       false ->
		   code:priv_dir(wx)
	   end,
    Driver = case os:type() of
		 {win32,_} ->
		     Driver0 ++ ".dll";
		 _ ->
		     Driver0 ++ ".so"
	     end,
    case file:read_file_info(filename:join(Priv, Driver)) of
	{ok, _} ->
	    {Priv, Priv};
	{error, _} ->
	    SrcPriv = filename:join(Priv, erlang:system_info(system_architecture)),
	    case file:read_file_info(filename:join(SrcPriv, Driver)) of
		{ok, _} ->
		    {Priv, SrcPriv};
		{error, _} ->
		    opt_error_log(Silent,
				  "ERROR: Could not find \'~s\' in: ~s~n",
				  [Driver, Priv]),
		    erlang:error({load_driver, "No driver found"})
	    end
    end.

strip(Src, Src) ->
    [];
strip([H|R], Src) ->
    [H| strip(R, Src)].

opt_error_log(false, Format, Args) ->
    logger:log(error, Format, Args, #{domain => [wx]});
opt_error_log(true, _Format, _Args) ->
    ok.
