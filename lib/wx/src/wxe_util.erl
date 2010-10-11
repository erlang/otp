%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-export([wxgl_dl/0, priv_dir/1]).

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


%% If you want anything done, do it yourself.

wxgl_dl() ->
    DynLib0 = "erl_gl",
    PrivDir = priv_dir(DynLib0),
    DynLib = case os:type() of
		 {win32,_} ->
		     DynLib0 ++ ".dll";
		 _ ->
		     DynLib0 ++ ".so"
	     end,
    GLLib = filename:join(PrivDir, DynLib),
    case file:read_file_info(GLLib) of
	{ok, _} ->
	    GLLib;
	{error,_} ->
	    error({enoent, GLLib})
    end.


priv_dir(Driver) ->
    Type = erlang:system_info(system_architecture),
    {file, Path} = code:is_loaded(?MODULE),
    Priv = case filelib:is_regular(Path) of
	       true ->
		   Beam = filename:join(["ebin/",atom_to_list(?MODULE) ++ ".beam"]),
		   filename:join(strip(Path, Beam), "priv");
	       false ->
		   code:priv_dir(wx)
	   end,
    try
	{ok, Dirs0} = file:list_dir(Priv),
	Dirs1 = split_dirs(Dirs0),
	Dirs  = lists:reverse(lists:sort(Dirs1)),

	Best = best_dir(hd(split_dirs([Type])),Dirs, Driver, Priv),
	filename:join(Priv, Best)
    catch _:_ ->
	    error_logger:format("ERROR: Could not find suitable \'~s\' for ~s in: ~s~n",
				[Driver, Type, Priv]),
	    erlang:error({load_driver, "No driver found"})
    end.

best_dir(Dir, Dirs0, Driver, Priv) ->
    Dirs = [{D,D} || D <- Dirs0],
    best_dir(Dir, Dirs, [], Driver, Priv).

best_dir(Pre, [{[],_}|R], Acc, Driver, Priv) -> %% Empty skip'em
    best_dir(Pre, R, Acc, Driver, Priv);
best_dir(Pre, [{Pre,Dir}|R], Acc, Driver, Priv) ->
    Real = dir_app(lists:reverse(Dir)),
    case file:list_dir(filename:join(Priv,Real)) of
	{ok, Fs} ->
	    case lists:any(fun(File) -> filename:rootname(File) =:= Driver end, Fs) of
		true ->  Real; %% Found dir and it contains a driver
		false -> best_dir(Pre, R, Acc, Driver, Priv)
	    end;
	_ ->
	    best_dir(Pre, R, Acc, Driver, Priv)
    end;
best_dir(Pre, [{[_|F],Dir}|R], Acc, Driver, Priv) ->
    best_dir(Pre, R, [{F,Dir}|Acc], Driver, Priv);
best_dir(_Pre, [], [], _,_) -> throw(no_dir);  %% Nothing found
best_dir([_|Pre], [], Acc, Driver, Priv) ->
    best_dir(Pre, lists:reverse(Acc), [], Driver, Priv);
best_dir([], _, _,_,_) -> throw(no_dir).  %% Nothing found

split_dirs(Dirs0) ->
    ToInt = fun(Str) ->
		    try
			list_to_integer(Str)
		    catch _:_ -> Str
		    end
	    end,
    Split = fun(Dir) ->
		    Toks = tokens(Dir,".-"),
		    lists:reverse([ToInt(Str) || Str <- Toks])
	    end,
    lists:map(Split,Dirs0).

dir_app([]) -> [];
dir_app([Dir]) -> Dir;
dir_app(Dir) ->
    dir_app2(Dir).
dir_app2([Int]) when is_integer(Int) ->
    integer_to_list(Int);
dir_app2([Str]) when is_list(Str) ->
    Str;
dir_app2([Head|Rest]) when is_integer(Head) ->
    integer_to_list(Head) ++ dir_app2(Rest);
dir_app2([Head|Rest]) when is_list(Head) ->
    Head ++ dir_app2(Rest).

strip(Src, Src) ->
    [];
strip([H|R], Src) ->
    [H| strip(R, Src)].

tokens(S,Seps) ->
    tokens1(S, Seps, []).

tokens1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
        true -> tokens1(S, Seps, [[C]|Toks]);
        false -> tokens2(S, Seps, Toks, [C])
    end;
tokens1([], _Seps, Toks) ->
    lists:reverse(Toks).

tokens2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
        true -> tokens1(S, Seps, [[C], lists:reverse(Cs) |Toks]);
        false -> tokens2(S, Seps, Toks, [C|Cs])
    end;
tokens2([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

