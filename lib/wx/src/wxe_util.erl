%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-export([call/2,cast/2,construct/2,
	 destroy/2, register_pid/1,
	 connect_cb/2,disconnect_cb/2,
	 send_bin/1, get_cbId/1,
	 get_const/1,colour_bin/1,datetime_bin/1,
	 to_bool/1,from_bool/1]).

-export([wxgl_dl/0, priv_dir/2, opt_error_log/3]).

-include("wxe.hrl").

to_bool(0) -> false;
to_bool(_) -> true.

from_bool(true) -> 1;
from_bool(false) -> 0.

colour_bin({R,G,B}) ->
    <<R:32/?UI,G:32/?UI, B:32/?UI,255:32/?UI>>;
colour_bin({R,G,B,A}) ->
    <<R:32/?UI,G:32/?UI, B:32/?UI,A:32/?UI>>.

datetime_bin({{Y,Mo,D},{H,Mi,S}}) ->
    %% DMY fits wxDaytime constructor
    %% Also wxDaytime:Month is enum from zero
    <<D:32/?UI,(Mo-1):32/?UI,Y:32/?UI,H:32/?UI,Mi:32/?UI,S:32/?UI>>.

get_const(Id) ->
    [{Id, Data}] = ets:lookup(wx_non_consts, Id),
    Data.

cast(Op,Args) ->
    #wx_env{port=Port,debug=Dbg} = wx:get_env(),
    _ = erlang:port_control(Port,Op,Args),
    case Dbg > 0 of
	true ->  debug_cast(Dbg band 15, Op,Args,Port);
	false -> ok
    end,
    ok.

call(Op, Args) ->
    #wx_env{port=Port,debug=Dbg} = wx:get_env(),
    case Dbg > 0 of
	false ->	    
	    _ = erlang:port_control(Port,Op,Args),
	    rec(Op);
	true ->
	    debug_call(Dbg band 15, Op, Args, Port)
    end.

rec(Op) ->
    receive 
	{'_wxe_result_', Res} -> Res;
	{'_wxe_error_', Op, Error} -> 
	    [{_,MF}] = ets:lookup(wx_debug_info,Op),
	    erlang:error({Error, MF});
	{'_wxe_error_', Old, Error} ->
	    [{_,{M,F,A}}] = ets:lookup(wx_debug_info,Old),
	    Msg = io_lib:format("~p in ~w:~w/~w", [Error, M, F, A]),
	    wxe_master ! {wxe_driver, error, Msg},
	    rec(Op)
    end.

construct(Op, Args) ->
    call(Op,Args).

destroy(Op, #wx_ref{ref=Ref}) -> 
    cast(Op,<<Ref:32/?UI>>).

register_pid(#wx_ref{ref=Ref}) ->
    call(?WXE_REGISTER_OBJECT, <<Ref:32/?UI>>).
   
send_bin(Bin) when is_binary(Bin) ->    
    #wx_env{port=Port,debug=Dbg} = wx:get_env(),
    case Dbg > 0 of
	false ->	    
	    erlang:port_command(Port, Bin);
	true ->
	    io:format("WX binary ~p(~p) ~n",[self(), Port]),
	    erlang:port_command(Port, Bin)
    end.

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


debug_cast(1, Op, _Args, _Port) ->
    check_previous(),
    case ets:lookup(wx_debug_info,Op) of
	[{_,{M,F,_}}] ->
	    io:format("WX ~p: ~s:~s(~p) -> ok~n", [self(),M,F,Op]);
	[] -> 
	    io:format("WX ~p: unknown(~p) -> ok~n", [self(),Op])
    end;
debug_cast(2, Op, Args, Port) ->
    check_previous(),
    case ets:lookup(wx_debug_info,Op) of
	[{_,{M,F,_}}] ->
	    io:format("WX ~p(~p): ~s:~s(~p) (~p) -> ok~n", 
		      [self(),Port,M,F,Op,Args]);
	[] ->
	    io:format("WX ~p(~p): unknown(~p) (~p) -> ok~n", 
		      [self(),Port,Op,Args])
    end;
debug_cast(_, _Op, _Args, _Port) ->
    check_previous(),
    ok.

debug_call(1, Op, Args, Port) ->
    check_previous(),
    case ets:lookup(wx_debug_info,Op) of
	[{_,{M,F,_}}] ->
	    io:format("WX ~p: ~s:~s(~p) -> ",[self(),M,F,Op]);
	[] ->
	    io:format("WX ~p: unknown(~p) -> ",[self(),Op])
    end,
    _ = erlang:port_control(Port,Op,Args),    
    debug_rec(1);
debug_call(2, Op, Args, Port) ->
    check_previous(),
    case ets:lookup(wx_debug_info,Op) of
	[{_,{M,F,_}}] ->
	    io:format("WX ~p(~p): ~s:~s(~p) (~p) -> ",
		      [self(), Port, M, F, Op, Args]);
	[] -> 
	    io:format("WX ~p(~p): unknown(~p) (~p) -> ",
		      [self(), Port, Op, Args])
    end,
    _ = erlang:port_control(Port,Op,Args),
    debug_rec(2);
debug_call(_, Op, Args, Port) ->
    check_previous(),
    _ = erlang:port_control(Port,Op,Args),
    rec(Op).

debug_rec(1) ->
    receive 
	{'_wxe_result_', Res} -> 
	    io:format("complete ~n", []),
	    Res;
	{'_wxe_error_', Op2, Error} -> 
	    [{_,MF2}] = ets:lookup(wx_debug_info,Op2),
	    erlang:error({Error, MF2})
    end;
debug_rec(2) ->
    receive 
	{'_wxe_result_', Res} -> 
	    io:format("~p ~n", [Res]),
	    Res;
	{'_wxe_error_', Op, Error} -> 
	    io:format("Error ~p ~n", [Error]),
	    [{_,MF}] = ets:lookup(wx_debug_info,Op),
	    erlang:error({Error, MF})
    end.

check_previous() ->
    receive 
	{'_wxe_error_', Op, Error} -> 
	    [{_,MF={M,F,_}}] = ets:lookup(wx_debug_info,Op),
	    io:format("WX ~p: ERROR in previous command ~s:~s~n",[self(), M,F]),
	    erlang:error({Error, MF})    
    after 0 -> ok
    end.

%% Get gl dynamic library

wxgl_dl() ->
    DynLib0 = "erl_gl",
    PrivDir = priv_dir(DynLib0, false),
    DynLib = case os:type() of
		 {win32,_} ->
		     DynLib0 ++ ".dll";
		 _ ->
		     DynLib0 ++ ".so"
	     end,
    filename:join(PrivDir, DynLib).

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
	    Priv;
	{error, _} ->
	    SrcPriv = filename:join(Priv, erlang:system_info(system_architecture)),
	    case file:read_file_info(filename:join(SrcPriv, Driver)) of
		{ok, _} ->
		    SrcPriv;
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
    error_logger:format(Format, Args);
opt_error_log(true, _Format, _Args) ->
    ok.
