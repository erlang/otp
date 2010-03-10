%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
%%
%%
-module(testINSTANCE_OF).

-export([compile/3,main/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Opt) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    
    ?line ok = asn1ct:compile(DataDir ++ "INSTANCEOF.asn1",
                              [Rules,{outdir,OutDir}]++Opt).


main(Erule) ->

    ?line {ok,Integer} = asn1_wrapper:encode('INSTANCEOF','Int',3),
    Int = wrap(Erule,Integer),
    ValotherName = {otherName,{'INSTANCE OF',{2,4},Int}},
    VallastName1 = {lastName,{'GeneralName_lastName',{2,4},12}},
    VallastName2 = {lastName,{'GeneralName_lastName',{2,3,4},
			      {'Seq',12,true}}},
    ?line {ok,BytesoN}=
	asn1_wrapper:encode('INSTANCEOF','GeneralName',ValotherName),
    ?line {ok,Res1={otherName,_}} = 
	asn1_wrapper:decode('INSTANCEOF','GeneralName',BytesoN),
    ?line ok = test_encdec(Erule,Int,Res1),

    ?line {ok,ByteslN1}=
	asn1_wrapper:encode('INSTANCEOF','GeneralName',VallastName1),
    ?line {ok,Res2={lastName,_}} = 
	asn1_wrapper:decode('INSTANCEOF','GeneralName',ByteslN1),
    ?line test_encdec(Erule,Res2),

    ?line {ok,ByteslN2}=
	asn1_wrapper:encode('INSTANCEOF','GeneralName',VallastName2),
    ?line {ok,Res3={lastName,_}} = 
	asn1_wrapper:decode('INSTANCEOF','GeneralName',ByteslN2),
    ?line test_encdec(Erule,Res3).

test_encdec(_Erule,Int,{otherName,{'INSTANCE OF',{2,4},Int}}) ->
    ok;
test_encdec(Erule,Int,R={otherName,{'INSTANCE OF',{2,4},_Int2}}) ->
    {error,{Erule,Int,R}}.

test_encdec(_Erule,{lastName,{'GeneralName_lastName',{2,4},12}}) ->
    ok;
test_encdec(_Erule,{lastName,{'GeneralName_lastName',{2,3,4},
			     {'Seq',12,true}}}) ->
    ok;
test_encdec(Erule,Res) ->
    {error,{Erule,Res}}.

wrap(ber,Int) when list(Int) ->
    binary_to_list(list_to_binary(Int));
wrap(per,Int) when list(Int) ->
    binary_to_list(list_to_binary(Int));
wrap(ber_bin,Int) when list(Int) ->
    list_to_binary(Int);
wrap(ber_bin_v2,Int) when list(Int) ->
    list_to_binary(Int);
wrap(per_bin,Int) when list(Int) ->
    list_to_binary(Int);
wrap(uper_bin,Int) when list(Int) ->
    list_to_binary(Int);
wrap(_,Int) ->
    Int.
