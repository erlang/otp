%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(test_undecoded_rest).

-export([compile/3,test/1]).

-include_lib("test_server/include/test_server.hrl").


%% testing OTP-5104

compile(Config,Rules,Opt) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    
    ?line ok = asn1ct:compile(DataDir ++ "P-Record",[Rules,{outdir,OutDir}]++Opt).


test(Opt) ->
    ?line {ok,Msg} = asn1ct:value('P-Record','PersonnelRecord'),
    ?line {ok,Bytes} = asn1_wrapper:encode('P-Record','PersonnelRecord',Msg),
    Bytes2 =
	fun(B) when is_list(B) ->
		B ++ [55,55,55];
	   (B) when is_binary(B) ->
		iolist_to_binary([B,<<55,55,55>>])
	end (Bytes),
    
    case Opt of
	undec_rest ->
	    ?line {ok,Msg,R}=asn1_wrapper:decode('P-Record','PersonnelRecord',
					     Bytes2),
	    ?line case R of
		      <<55,55,55>> ->ok;
		      [55,55,55] -> ok;
		      BStr when is_bitstring(BStr) ->
			  PadLen = (8 - (bit_size(BStr) rem 8)) rem 8,
			  case <<0:PadLen,BStr/bitstring>> of
			      <<0,55,55,55>> -> ok
			  end
		  end;
	_ ->
	    ?line {ok,Msg} = asn1_wrapper:decode('P-Record','PersonnelRecord',
					   Bytes2)
    end,
    ok.
