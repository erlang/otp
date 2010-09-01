%%%-------------------------------------------------------------------
%%% File    : extensionAdditionGroup.erl
%%% Author  : Kenneth Lundin
%%% Description :
%%%
%%% Created : 18 May 2010 by kenneth
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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
-module(extensionAdditionGroup).
-include("Extension-Addition-Group.hrl").


-compile(export_all).

run(Erule) ->
    Val = #'Ax'{a=253, b = true, c= {e,true}, g="123", h = true},
    io:format("~p:~p~n",[Erule,Val]),
    {ok,List}= asn1rt:encode('Extension-Addition-Group','Ax',Val),
    Enc = iolist_to_binary(List),
    io:format("~p~n",[Enc]),
    {ok,Val2} = asn1rt:decode('Extension-Addition-Group','Ax',Enc),
    io:format("~p~n",[Val2]),
    case Val2 of
	Val -> ok;
	_ -> exit({expected,Val, got, Val2})
    end.

run2(Erule) ->
    Val = #'Ax3'{a=253, b = true, s = #'Ax3_s'{sa = 11, sb = true, sextaddgroup = 17}},
    io:format("~p:~p~n",[Erule,Val]),
    {ok,List}= asn1rt:encode('Extension-Addition-Group','Ax3',Val),
    Enc = iolist_to_binary(List),
    io:format("~p~n",[Enc]),
    {ok,Val2} = asn1rt:decode('Extension-Addition-Group','Ax3',Enc),
    io:format("~p~n",[Val2]),
    case Val2 of
	Val -> ok;
	_ -> exit({expected,Val, got, Val2})
    end.
