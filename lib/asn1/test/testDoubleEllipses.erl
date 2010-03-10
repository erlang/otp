%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
-module(testDoubleEllipses).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Seq',{a, c}).
-record('SeqV2',{a, b ,c}).
-record('SeqAlt',{a,d,b,e,c,f,g}).
-record('SeqAltV2',{a,d,b,e,h,i,c,f,g}).

-record('Set',{a, c}).
-record('SetV2',{a, b ,c}).
-record('SetAlt',{a,d,b,e,c,f,g}).
-record('SetAltV2',{a,d,b,e,h,i,c,f,g}).




compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "DoubleEllipses",[Rules,{outdir,OutDir}]++Options).


main(_Rules) ->
    %% SEQUENCE
    ?line {ok,Bytes} = 
	asn1_wrapper:encode('DoubleEllipses','Seq',#'Seq'{a = 10,c = true}),
    ?line {ok,#'SeqV2'{a=10,b = asn1_NOVALUE, c = true}} = 
	asn1_wrapper:decode('DoubleEllipses','SeqV2',Bytes),
    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('DoubleEllipses','SeqV2',
			    #'SeqV2'{a=10,b = false, c = true}),
    ?line {ok,#'Seq'{a = 10, c = true}} =
	asn1_wrapper:decode('DoubleEllipses','Seq',Bytes2),
    
    ?line {ok,Bytes3} =
	asn1_wrapper:encode('DoubleEllipses','SeqAlt',
			    #'SeqAlt'{a = 10, d = 12, 
				      b = [1,0,1,0], e = true,
				      c = false, f = 14, g = 16}),
    ?line {ok,#'SeqAltV2'{a = 10, d = 12, 
			  b = [1,0,1,0], e = true,
			  h = asn1_NOVALUE, i = asn1_NOVALUE,
			  c = false, f = 14, g = 16}} =
	asn1_wrapper:decode('DoubleEllipses','SeqAltV2',Bytes3),
    ?line {ok,Bytes4} =
	asn1_wrapper:encode('DoubleEllipses','SeqAltV2',
			    #'SeqAltV2'{a = 10, d = 12, 
				      b = [1,0,1,0], e = true,
				      h = "PS", i = 13,
				      c = false, f = 14, g = 16}),
     ?line {ok,#'SeqAlt'{a = 10, d = 12, 
			 b = [1,0,1,0], e = true,
			 c = false, f = 14, g = 16}} =
	asn1_wrapper:decode('DoubleEllipses','SeqAlt',Bytes4),
    
    %% SET
    ?line {ok,Bytes5} = 
	asn1_wrapper:encode('DoubleEllipses','Set',#'Set'{a = 10,c = true}),
    ?line {ok,#'SetV2'{a=10,b = asn1_NOVALUE, c = true}} = 
	asn1_wrapper:decode('DoubleEllipses','SetV2',Bytes5),
    ?line {ok,Bytes6} = 
	asn1_wrapper:encode('DoubleEllipses','SetV2',
			    #'SetV2'{a=10,b = false, c = true}),
    ?line {ok,#'Set'{a = 10, c = true}} =
	asn1_wrapper:decode('DoubleEllipses','Set',Bytes6),
    
    ?line {ok,Bytes7} =
	asn1_wrapper:encode('DoubleEllipses','SetAlt',
			    #'SetAlt'{a = 10, d = 12, 
				      b = [1,0,1,0], e = true,
				      c = false, f = 14, g = 16}),
    ?line {ok,#'SetAltV2'{a = 10, d = 12, 
			  b = [1,0,1,0], e = true,
			  h = asn1_NOVALUE, i = asn1_NOVALUE,
			  c = false, f = 14, g = 16}} =
	asn1_wrapper:decode('DoubleEllipses','SetAltV2',Bytes7),
    ?line {ok,Bytes8} =
	asn1_wrapper:encode('DoubleEllipses','SetAltV2',
			    #'SetAltV2'{a = 10, d = 12, 
				      b = [1,0,1,0], e = true,
				      h = "PS", i = 13,
				      c = false, f = 14, g = 16}),
     ?line {ok,#'SetAlt'{a = 10, d = 12, 
			 b = [1,0,1,0], e = true,
			 c = false, f = 14, g = 16}} =
	asn1_wrapper:decode('DoubleEllipses','SetAlt',Bytes8),
    ok.
