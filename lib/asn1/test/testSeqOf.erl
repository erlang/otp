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
-module(testSeqOf).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Seq1',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('Seq2',{seq2 = asn1_DEFAULT, bool2, int2}).
-record('Seq3',{bool3, seq3 = asn1_DEFAULT, int3}).
-record('Seq4',{seq41 = asn1_DEFAULT, seq42 = asn1_DEFAULT, seq43 = asn1_DEFAULT}).
-record('SeqIn',{boolIn, intIn}).
%-record('SeqCho',{bool1, int1, seq1 = asn1_DEFAULT}).
%-record('SeqChoInline',{bool1, int1, seq1 = asn1_DEFAULT}).
%-record('SeqChoOfInline_SEQOF',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqEmp',{seq1}).
-record('Empty',{}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqOf",[Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "SeqOfEnum",[Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "XSeqOf",[Rules,{outdir,OutDir}]++Options).



main(Rules) ->

    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqOf','Seq1',#'Seq1'{bool1 = true,
					     int1 = 17}),
    ?line {ok,{'Seq1',true,17,[]}} = 
	asn1_wrapper:decode('SeqOf','Seq1',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SeqOf','Seq1',#'Seq1'{bool1 = true,
					     int1 = 17,
					     seq1 = [#'SeqIn'{boolIn = true,
							      intIn = 25}]}),
    ?line {ok,{'Seq1',true,17,[{'SeqIn',true,25}]}} = 
	asn1_wrapper:decode('SeqOf','Seq1',lists:flatten(Bytes12)),
    
    
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SeqOf','Seq1',#'Seq1'{bool1 = true,
					     int1 = 17,
					     seq1 = [#'SeqIn'{boolIn = true,
							      intIn = 25},
						     #'SeqIn'{boolIn = false,
							      intIn = 125},
						     #'SeqIn'{boolIn = false,
							      intIn = 225}]}),
    ?line {ok,{'Seq1',true,17,[{'SeqIn',true,25},{'SeqIn',false,125},{'SeqIn',false,225}]}} = 
	asn1_wrapper:decode('SeqOf','Seq1',lists:flatten(Bytes13)),






    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqOf','Seq2',#'Seq2'{bool2 = true,
					     int2 = 17}),
    
    ?line {ok,{'Seq2',[],true,17}} = 
	asn1_wrapper:decode('SeqOf','Seq2',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SeqOf','Seq2',#'Seq2'{bool2 = true,
					     int2 = 17,
					     seq2 = [#'SeqIn'{boolIn = true,
							      intIn = 25}]}),
    ?line {ok,{'Seq2',[{'SeqIn',true,25}],true,17}} = 
	asn1_wrapper:decode('SeqOf','Seq2',lists:flatten(Bytes22)),
    
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SeqOf','Seq2',#'Seq2'{bool2 = true,
					     int2 = 17,
					     seq2 = [#'SeqIn'{boolIn = true,
							      intIn = 25},
						     #'SeqIn'{boolIn = false,
							      intIn = 125},
						     #'SeqIn'{boolIn = false,
							      intIn = 225}]}),
    ?line {ok,{'Seq2',[{'SeqIn',true,25},{'SeqIn',false,125},{'SeqIn',false,225}],true,17}} = 
	asn1_wrapper:decode('SeqOf','Seq2',lists:flatten(Bytes23)),
    
    



    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SeqOf','Seq3',#'Seq3'{bool3 = true,
					     int3 = 17}),
    ?line {ok,{'Seq3',true,[],17}} = 
	asn1_wrapper:decode('SeqOf','Seq3',lists:flatten(Bytes31)),
    
    
    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SeqOf','Seq3',#'Seq3'{bool3 = true,
					     int3 = 17,
					     seq3 = [#'SeqIn'{boolIn = true,
							      intIn = 25}]}),
    ?line {ok,{'Seq3',true,[{'SeqIn',true,25}],17}} = 
	asn1_wrapper:decode('SeqOf','Seq3',lists:flatten(Bytes32)),
    
    
    ?line {ok,Bytes33} = 
	asn1_wrapper:encode('SeqOf','Seq3',#'Seq3'{bool3 = true,
					     int3 = 17,
					     seq3 = [#'SeqIn'{boolIn = true,
							      intIn = 25},
						     #'SeqIn'{boolIn = false,
							      intIn = 125},
						     #'SeqIn'{boolIn = false,
							      intIn = 225}]}),
    ?line {ok,{'Seq3',true,[{'SeqIn',true,25},{'SeqIn',false,125},{'SeqIn',false,225}],17}} = 
	asn1_wrapper:decode('SeqOf','Seq3',lists:flatten(Bytes33)),
    
  



  
    
    ?line {ok,Bytes41} = asn1_wrapper:encode('SeqOf','Seq4',#'Seq4'{}),
    ?line {ok,{'Seq4',[],[],[]}} = asn1_wrapper:decode('SeqOf','Seq4',lists:flatten(Bytes41)),
        
    
    ?line {ok,Bytes42} = 
	asn1_wrapper:encode('SeqOf','Seq4',#'Seq4'{seq41 = [#'SeqIn'{boolIn = true,
							       intIn = 25}]}),
    ?line {ok,{'Seq4',[{'SeqIn',true,25}],[],[]}} = 
	asn1_wrapper:decode('SeqOf','Seq4',lists:flatten(Bytes42)),
    
    
    ?line {ok,Bytes43} = 
	asn1_wrapper:encode('SeqOf','Seq4',#'Seq4'{seq41 = [#'SeqIn'{boolIn = true,
							       intIn = 25},
						      #'SeqIn'{boolIn = false,
							       intIn = 125},
						      #'SeqIn'{boolIn = false,
							       intIn = 225}]}),
    ?line {ok,{'Seq4',[{'SeqIn',true,25},{'SeqIn',false,125},{'SeqIn',false,225}],[],[]}} = 
	asn1_wrapper:decode('SeqOf','Seq4',lists:flatten(Bytes43)),
    
    
    ?line {ok,Bytes44} = 
	asn1_wrapper:encode('SeqOf','Seq4',#'Seq4'{seq42 = [#'SeqIn'{boolIn = true,
								       intIn = 25}]}),
    ?line {ok,{'Seq4',[],[{'SeqIn',true,25}],[]}} = 
	asn1_wrapper:decode('SeqOf','Seq4',lists:flatten(Bytes44)),
    
    
    ?line {ok,Bytes45} = 
	asn1_wrapper:encode('SeqOf','Seq4',#'Seq4'{seq42 = [#'SeqIn'{boolIn = true,
							       intIn = 25},
						      #'SeqIn'{boolIn = false,
							       intIn = 125},
						      #'SeqIn'{boolIn = false,
							       intIn = 225}]}),
    ?line {ok,{'Seq4',[],[{'SeqIn',true,25},{'SeqIn',false,125},{'SeqIn',false,225}],[]}} = 
	asn1_wrapper:decode('SeqOf','Seq4',lists:flatten(Bytes45)),
    
    
    ?line {ok,Bytes46} = 
	asn1_wrapper:encode('SeqOf','Seq4',#'Seq4'{seq43 = [#'SeqIn'{boolIn = true,
							       intIn = 25}]}),
    ?line {ok,{'Seq4',[],[],[{'SeqIn',true,25}]}} = 
	asn1_wrapper:decode('SeqOf','Seq4',lists:flatten(Bytes46)),
    
    
    ?line {ok,Bytes47} = 
	asn1_wrapper:encode('SeqOf','Seq4',#'Seq4'{seq43 = [#'SeqIn'{boolIn = true,
							       intIn = 25},
						      #'SeqIn'{boolIn = false,
							       intIn = 125},
						      #'SeqIn'{boolIn = false,
							       intIn = 225}]}),
    ?line {ok,{'Seq4',[],[],[{'SeqIn',true,25},{'SeqIn',false,125},{'SeqIn',false,225}]}} = 
	asn1_wrapper:decode('SeqOf','Seq4',lists:flatten(Bytes47)),
    
    
    ?line {ok,Bytes51} = asn1_wrapper:encode('SeqOf','SeqEmp',#'SeqEmp'{seq1 = [#'Empty'{}]}),
    ?line {ok,{'SeqEmp',[{'Empty'}]}} = asn1_wrapper:decode('SeqOf','SeqEmp',lists:flatten(Bytes51)),
    
    case Rules of
	ber ->
	    ?line {ok,Bytes52} = asn1_wrapper:encode('SeqOfEnum','SeqOfEnum',
					     {'SeqOfEnum',[{'Enum',a},{'Enum',b}]}),
	    ?line {ok,[a,b]} = asn1_wrapper:decode('SeqOfEnum','SeqOfEnum',
					   lists:flatten(Bytes52));
	_ -> ok
    end,

    %% tests of OTP-4590
    case Rules of
	PER when PER == per; PER == per_bin ->
	    DayNames = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"],
	    ?line {ok,Bytes60} = asn1_wrapper:encode('XSeqOf','DayNames2',DayNames),
	    ?line {ok,Bytes60} = asn1_wrapper:encode('XSeqOf','DayNames4',DayNames),
	    ?line {ok,DayNames} = asn1_wrapper:decode('XSeqOf','DayNames2',Bytes60),
	    ?line {ok,DayNames} = asn1_wrapper:decode('XSeqOf','DayNames4',Bytes60),
	    ?line {ok,Bytes61} = asn1_wrapper:encode('XSeqOf','DayNames1',DayNames),
	    ?line {ok,Bytes61} = asn1_wrapper:encode('XSeqOf','DayNames3',DayNames),
	    ?line {ok,DayNames} = asn1_wrapper:decode('XSeqOf','DayNames1',Bytes61),
	    ?line {ok,DayNames} = asn1_wrapper:decode('XSeqOf','DayNames3',Bytes61);
	_ ->
	    ok
    end,

    ok.


