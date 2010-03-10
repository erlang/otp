%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(testChoPrim).

-export([compile/3]).
-export([bool/1]).
-export([int/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoPrim",[Rules,{outdir,OutDir}]++Options).



bool(Rules) ->
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('ChoPrim','ChoCon',{bool0,true}),
    ?line {ok,{bool0,true}} = asn1_wrapper:decode('ChoPrim','ChoCon',lists:flatten(Bytes11)),

    ?line {ok,Bytes12} = asn1_wrapper:encode('ChoPrim','ChoCon',{bool1,true}),
    ?line {ok,{bool1,true}} = asn1_wrapper:decode('ChoPrim','ChoCon',lists:flatten(Bytes12)),

    ?line {ok,Bytes13} = asn1_wrapper:encode('ChoPrim','ChoCon',{int2,233}),
    ?line {ok,{int2,233}} = asn1_wrapper:decode('ChoPrim','ChoCon',lists:flatten(Bytes13)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber ->
		  ?line {error,{asn1,{invalid_choice_type,wrong}}} = 
		      case catch asn1_wrapper:encode('ChoPrim','ChoCon',{wrong,233}) of 
			  X1 -> X1 end,
		  ?line {error,{asn1,{invalid_choice_tag,_WrongTag}}} = 
		      case catch asn1_wrapper:decode('ChoPrim','ChoCon',[131,2,0,233]) of 
			  X2 -> X2 end,
		  ok;

	      per ->
		  ok
	  end,

    ok.



int(Rules) ->

    ?line {ok,Bytes21} = asn1_wrapper:encode('ChoPrim','ChoExp',{int10,1}),
    ?line {ok,{int10,first}} = asn1_wrapper:decode('ChoPrim','ChoExp',lists:flatten(Bytes21)),

    ?line {ok,Bytes22} = asn1_wrapper:encode('ChoPrim','ChoExp',{int10,first}),
    ?line {ok,{int10,first}} = asn1_wrapper:decode('ChoPrim','ChoExp',lists:flatten(Bytes22)),

    ?line {ok,Bytes23} = asn1_wrapper:encode('ChoPrim','ChoExp',{int10,last}),
    ?line {ok,{int10,last}} = asn1_wrapper:decode('ChoPrim','ChoExp',lists:flatten(Bytes23)),

    ?line {ok,Bytes24} = asn1_wrapper:encode('ChoPrim','ChoExp',{bool11,true}),
    ?line {ok,{bool11,true}} = asn1_wrapper:decode('ChoPrim','ChoExp',lists:flatten(Bytes24)),


    ?line {ok,Bytes26} = asn1_wrapper:encode('ChoPrim','ChoExp',{enum12,one}),
    ?line {ok,{enum12,one}} = asn1_wrapper:decode('ChoPrim','ChoExp',lists:flatten(Bytes26)),

    ?line {ok,Bytes25} = asn1_wrapper:encode('ChoPrim','ChoExp',{bool11,true}),
    ?line {ok,{bool11,true}} = 
	asn1_wrapper:decode('ChoPrim','ChoExp',lists:flatten(Bytes25)),
    
    ?line {error,{asn1,_}} = 
	case catch asn1_wrapper:encode('ChoPrim','ChoExp',{enum12,four}) of 
	    X3 -> X3 end,
    
    ?line {error,{asn1,_}} = 
	case catch asn1_wrapper:encode('ChoPrim','ChoExp',{wrong,233}) of 
	    X4 -> io:format("error reason = ~p~n",[X4]), X4 end,
		  
    ?line case asn1_wrapper:erule(Rules) of
	      ber ->
		  ?line {error,{asn1,_}} = 
		      case catch asn1_wrapper:decode('ChoPrim','ChoExp',[107,3,2,1,1]) of 
			  X5 -> X5 end,
		  ok;

	      per ->
		  ok
	  end,
    ok.








