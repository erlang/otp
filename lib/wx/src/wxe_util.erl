%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
	    [{_,MF}] = ets:lookup(wx_debug_info,Old),
	    erlang:exit({Error, MF})
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

connect_cb(Object,EvData) ->
    handle_listener(connect_cb, Object, EvData).

disconnect_cb(Object,EvData) ->
    handle_listener(disconnect_cb, Object, EvData).

handle_listener(Op,Object,EvData) ->
    Listener = gen_server:call((wx:get_env())#wx_env.sv, {Op,Object,EvData}, infinity),
    case Listener of
	{call_impl, connect_cb, EvtList} ->
	    wxEvtHandler:connect_impl(EvtList,Object,EvData);
	Res ->
	    Res
    end.

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
