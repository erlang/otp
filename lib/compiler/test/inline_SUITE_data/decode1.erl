%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
%%
%----------------------------------------------------------------------
% decode1.erl (new version)
%----------------------------------------------------------------------
%    -*- Erlang -*- 
%    File:	decode1.erl  (~jb/decode/decode1.erl)
%    Author:	Johan Bevemyr
%    Created:	Tue Jan 14 09:33:49 1997
%    Purpose:   
%    Notes:     Rewritten for use in ETOS. (Happi)

-module(decode1).

-export([?MODULE/0,
	 decode_ie_heads_setup/1,
	 run_dummy/2,
	 run_orig/2]).

?MODULE() ->
  FrameList = [89,128,0,8,132,0,26,133,133,0,38,148,94,
	       128,0,2,129,128,92,128,0,2,0,0,112,128,0,
	       10,194,69,0,0,0,0,0,18,52,95],
  Frame = list_to_binary([list_to_binary([89]),list_to_binary([128]),
			  list_to_binary([0]),list_to_binary([8]),
			  list_to_binary([132]),list_to_binary([0]),
			  list_to_binary([26]),list_to_binary([133]),
			  list_to_binary([133]),list_to_binary([0]),
			  list_to_binary([38]),list_to_binary([148]),
			  list_to_binary([94]),list_to_binary([128]),
			  list_to_binary([0]),list_to_binary([2]),
			  list_to_binary([129]),list_to_binary([128]),
			  list_to_binary([92]),list_to_binary([128]),
			  list_to_binary([0]),list_to_binary([2]),
			  list_to_binary([0]),list_to_binary([0]),
			  list_to_binary([112]),list_to_binary([128]),
			  list_to_binary([0]),list_to_binary([10]),
			  list_to_binary([194]),list_to_binary([69]),
			  list_to_binary([0]),list_to_binary([0]),
			  list_to_binary([0]),list_to_binary([0]),
			  list_to_binary([0]),list_to_binary([18]),
			  list_to_binary([52]),list_to_binary([95])]),
  
    R = loop(2,0,Frame),
    {R,R =:= {0,[{ie,112,itu_t_standard,ignore,10,<<194,69,0,0,0,0,0,18,52,95>>},
		 {ie,92,itu_t_standard,ignore,2,<<0,0>>},
		 {ie,94,itu_t_standard,ignore,2,<<129,128>>},
		 {ie,89,itu_t_standard,ignore,8,<<132,0,26,133,133,0,38,148>>}]}}.
     
loop(0,R,_) -> R;
loop(N,R,Frame) -> loop(N-1, decode1:decode_ie_heads_setup(Frame),Frame).

run_dummy(0,Frame) ->
   done;
run_dummy(N,Frame) ->
   parse_dummy(Frame),
   run_dummy(N-1,Frame).

parse_dummy(Frame) -> true.

run_orig(0,Frame) -> 
   done;
run_orig(N,Frame) ->
   decode1:decode_ie_heads_setup(Frame),
   run_orig(N-1,Frame).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Macros
%    

-define(VALID_ACTION(Flag), if ((Flag) band 16#10) == 16#10 -> true;
			       true -> false
			    end).
-define(GET_ACTION(Flag),   ((Flag) band 16#03)).

-define(getint16(X1,X0),    (16#100*X1 + X0)).

-define(IS_EXTENDED(X),     (if ((X) band 16#80) == 16#80 -> false;
				true -> true
			     end)).


%%% ----------------------------------------------------------
%%% #           ie
%%% Description: Used to encapsulate the ie head
%%% ----------------------------------------------------------


-record(ie,{identifier,
	    coding,
	    action_ind,
	    length,
	    ie_body = binary}).

%%% ----------------------------------------------------------
%%% #           bbc
%%% Description: BROADBAND BEARER CAPABILITY
%%% ----------------------------------------------------------

-record(scct_bbc,
	{scct_pci,		% parameter compatibility info
	 scct_bearer_class,
	 scct_atm_transfer_capability,
	 scct_user_plane_connection_configuration,
	 scct_susceptibility_to_clipping}).


%%% ----------------------------------------------------------
%%% #           cause
%%% Description: CAUSE
%%% ----------------------------------------------------------

-record(scct_cause,
	{scct_pci,		% parameter compatibility info
	 scct_location,
	 scct_cause_value,
	 scct_diagnostics_list = []}).


%%% ----------------------------------------------------------
%%% #           release_complete_uni
%%% Description: 
%%% ----------------------------------------------------------

-record(release_complete_uni,
	{scct_cause_list=[],   % Cause IE's in a list where each element
			       % is a record of scct_cause{}
         scct_geidt_list=[]}). % Generic Identifier Transport IE's in a list 
                               % where each element is a record of scct_geidt{}


-define(IE_BB_BEARER_CAPABILITY, 		16#5e).
-define(SCCT_PUB_NETW_SERV_LOCAL_USR,        2).
-define(SCCT_ERR_INVALID_IE_CONT,		 	100).
-define(DECRES_THROW_RELCOMP,  		error_throw_relcomp).

-define(SCCT_P_TO_P,                         0).
-define(SCCT_P_TO_MP,                        1).

-define(IE_ENDPT_REF,   			16#54).
-define(IE_BB_REPEAT_INDICATOR, 		16#63).

-define(SCCT_ERR_MAND_IE_MISSING,			96).

-define(A_CLEAR_CALL,            			0).
-define(A_DISCARD_PROCEED,       			1).
-define(A_DISCARD_PROCEED_STATUS,			2).

-define(SCCT_BCOB_A,                         1).
-define(SCCT_BCOB_C,                         3).
-define(SCCT_BCOB_X,                         16).
-define(SCCT_TRANSP_VP_SERV,                 24).

-define(SCCT_CBR,                            5).
-define(SCCT_CBR_WITH_CLR_CLP_0_1,           7).
-define(SCCT_RT_VBR,                         9).
-define(SCCT_RT_VBR_WITH_CLR_CLP_0_1,        19).
-define(SCCT_NON_RT_VBR,                     10).
-define(SCCT_NON_RT_VBR_WITH_CLR_CLP_0_1,    11).
-define(SCCT_ABR,                            12).
-define(SCCT_NOT_SUSCEPT_TO_CLIPPING,        0).
-define(SCCT_SUSCEPT_TO_CLIPPING,            1).



%%% ----------------------------------------------------------
%%% -type decode_ie_heads_setup(Bin)-> 	       	    
%%%                                              
%%% Input: Binary to body of incoming Message
%%% Output:  
%%%                
%%% Exceptions:  
%%% Description:decode_ie_heads_setup is used both p-p and p-mp setup
%%%             Never fails. Needs to be exported to be able to check 
%%%             if setup is p-p or p-mp. 
%%%             Note that if broadband rep indicator is present
%%%             order must be incoming IE's must be preserved, this
%%%             only applicable in msg setup
%%% ----------------------------------------------------------
decode_ie_heads_setup(Bin)->
   decode_ie_heads_setup(Bin,no_bbc_ie,no_epr,[],no_brep).
	
decode_ie_heads_setup(Bin,TypeOfCall,EprFlag,IEList,BrepFlag) when is_binary(Bin),size(Bin) >= 4 ->
   {Bin1,Bin2} = split_binary(Bin,4),
   [Id,F,L1,L0]= binary_to_list(Bin1),
   Action = decode_action(F),
   Coding = decode_ie_coding(F),
   case ?getint16(L1,L0) of
      Len when Len >0 ->
	 %%catch needed we cannot trust indata
	 case catch split_binary(Bin2,Len) of 
	    {'EXIT',_} ->  %%binary unpacked as far as possible
	       decode_ie_heads_setup(not_a_binary,TypeOfCall,EprFlag,
				     IEList,BrepFlag);
	    {Bin3,Bin4} ->
	       IE= #ie {identifier = Id,
			coding = Coding,
			action_ind= Action,
			length= Len,
			ie_body= Bin3},
	       case Id of
		  ?IE_BB_BEARER_CAPABILITY -> 
		     BbcRec=#scct_bbc{},
		     %%catch needed we cannot trust indata
		     case catch 
			dec_bearer_capability(BbcRec,
					      binary_to_list(Bin3)) of
			{'EXIT',_} -> %mand content error
			   CauseRec=#scct_cause{scct_location=
						?SCCT_PUB_NETW_SERV_LOCAL_USR,
						scct_cause_value=
					        ?SCCT_ERR_INVALID_IE_CONT,
						scct_diagnostics_list=
						[?IE_BB_BEARER_CAPABILITY]},
			   RelCompUniMsg =
			      #release_complete_uni{scct_cause_list=
						    [CauseRec]},
			   {?DECRES_THROW_RELCOMP,RelCompUniMsg};
			NewBbcRec ->
			   case NewBbcRec
			      #scct_bbc.scct_user_plane_connection_configuration of
			      ?SCCT_P_TO_P ->
				 decode_ie_heads_setup(Bin4,
						       ?SCCT_P_TO_P,
						       EprFlag,
						       [IE|IEList],
						       BrepFlag);
			      ?SCCT_P_TO_MP ->
				 decode_ie_heads_setup(Bin4,
						       ?SCCT_P_TO_MP,
						       EprFlag,
						       [IE|IEList],
						       BrepFlag)
			   end
		     end;
		  ?IE_ENDPT_REF ->  
		     decode_ie_heads_setup(Bin4,TypeOfCall,yes_epr,
					   [IE|IEList],BrepFlag);
		  ?IE_BB_REPEAT_INDICATOR ->
		     decode_ie_heads_setup(Bin4,TypeOfCall,EprFlag,
					   [IE|IEList],yes_brep);
		  _ ->
		     decode_ie_heads_setup(Bin4,TypeOfCall,EprFlag,
					   [IE|IEList],BrepFlag)
	       end
	 end;
      Len when Len == 0 ->%ie body empty, treat as if whole ie was missing
	 decode_ie_heads_setup(Bin2,TypeOfCall,EprFlag,IEList,BrepFlag)
   end;
decode_ie_heads_setup(_,?SCCT_P_TO_MP,yes_epr,IEList,no_brep) ->
   {?SCCT_P_TO_MP,IEList};
decode_ie_heads_setup(_,?SCCT_P_TO_MP,yes_epr,IEList,yes_brep) ->
%Order of incoming IEs must be preserved since BroadB Repeat Ind is present
   {?SCCT_P_TO_MP,reverse(IEList)}; 
decode_ie_heads_setup(_,?SCCT_P_TO_MP,no_epr,_,no_brep) ->
   CauseRec=#scct_cause{scct_location=?SCCT_PUB_NETW_SERV_LOCAL_USR,
			scct_cause_value=?SCCT_ERR_MAND_IE_MISSING,
			scct_diagnostics_list=[?IE_ENDPT_REF]}, 
   RelCompUniMsg =#release_complete_uni{scct_cause_list=[CauseRec]},
   {?DECRES_THROW_RELCOMP,RelCompUniMsg};
decode_ie_heads_setup(_,?SCCT_P_TO_P,_,IEList,no_brep) ->
   {?SCCT_P_TO_P,IEList};
decode_ie_heads_setup(_,?SCCT_P_TO_P,_,IEList,yes_brep) ->
%Order of incoming IEs must be preserved since BrodB Repeat Ind is present
   {?SCCT_P_TO_P,reverse(IEList)};
decode_ie_heads_setup(_,no_bbc_ie,_,_,_) ->
   CauseRec=#scct_cause{scct_location=?SCCT_PUB_NETW_SERV_LOCAL_USR,
			scct_cause_value=?SCCT_ERR_MAND_IE_MISSING,
			scct_diagnostics_list=[?IE_BB_BEARER_CAPABILITY]},
   RelCompUniMsg =#release_complete_uni{scct_cause_list=[CauseRec]},
   {?DECRES_THROW_RELCOMP,RelCompUniMsg}.



%%%
%%% Decode message type and header
%%%

decode_action(Flag) ->
   case ?VALID_ACTION(Flag) of
      true ->
	 case ?GET_ACTION(Flag) of
	    ?A_CLEAR_CALL -> clear_call;
	    ?A_DISCARD_PROCEED -> discard_proceed;
	    ?A_DISCARD_PROCEED_STATUS -> discard_proceed_status;
	    _ -> undefined
	 end;
      false ->
	 ignore
   end.


%%%
%%% Decode ie coding
%%%

decode_ie_coding(F) ->
   case F band 16#60 of 
      0 -> itu_t_standard;
      16#60 -> atm_forum_specific;
      _ -> undefined
   end.


%%% --------------------------------------------------------------------------
%%%
%%% Decode of INFORMATION ELEMENT: Broadband Bearer Capability
%%%
%%% --------------------------------------------------------------------------

dec_bearer_capability(BbcRec, [Octet5 | Rest]) ->
   NewBbcRec = 
      case Octet5 band 16#1f of
	 16#01 -> 
	    BbcRec#scct_bbc{scct_bearer_class = ?SCCT_BCOB_A};
	 16#03 -> 
	    BbcRec#scct_bbc{scct_bearer_class = ?SCCT_BCOB_C};
	 16#10 -> 
	    BbcRec#scct_bbc{scct_bearer_class = ?SCCT_BCOB_X};
	 16#18 -> 
	    BbcRec#scct_bbc{scct_bearer_class = ?SCCT_TRANSP_VP_SERV}
      end,
   
   case ?IS_EXTENDED(Octet5) of
      true ->
	 dec_bearer_capability_5a(NewBbcRec, Rest);
      false -> 
	 dec_bearer_capability_6(NewBbcRec, Rest)
   end.

dec_bearer_capability_5a(BbcRec,[Octet5a | Rest]) ->    
   NewBbcRec = 
      case Octet5a band 16#7f of  
	 16#05 -> 
	    BbcRec#scct_bbc{scct_atm_transfer_capability = 
			    ?SCCT_CBR};
	 16#07 -> 
	    BbcRec#scct_bbc{scct_atm_transfer_capability = 
			    ?SCCT_CBR_WITH_CLR_CLP_0_1};
	 16#09 -> 
	    BbcRec#scct_bbc{scct_atm_transfer_capability = 
			    ?SCCT_RT_VBR};
	 16#13 -> 
	    BbcRec#scct_bbc{scct_atm_transfer_capability = 
			    ?SCCT_RT_VBR_WITH_CLR_CLP_0_1};
	 16#0a -> 
	    BbcRec#scct_bbc{scct_atm_transfer_capability = 
			    ?SCCT_NON_RT_VBR};
	 16#0b -> 
	    BbcRec#scct_bbc{scct_atm_transfer_capability = 
			    ?SCCT_NON_RT_VBR_WITH_CLR_CLP_0_1};
	 16#0c -> 
	    BbcRec#scct_bbc{scct_atm_transfer_capability = 
			    ?SCCT_ABR}
      end,
   dec_bearer_capability_6(NewBbcRec,Rest).


dec_bearer_capability_6(BbcRec, [Octet6]) ->
   STC = 
      case (Octet6 bsr 5) band 16#03 of
	 16#00 -> 
	    ?SCCT_NOT_SUSCEPT_TO_CLIPPING;
	 16#01 -> 
	    ?SCCT_SUSCEPT_TO_CLIPPING
      end,
   
   UPCC = 
      case Octet6 band 16#03 of
	 16#00 ->
	    ?SCCT_P_TO_P;
	 16#01 ->
	    ?SCCT_P_TO_MP
      end,

   NewBbcRec = BbcRec#scct_bbc{scct_susceptibility_to_clipping = STC,
			       scct_user_plane_connection_configuration = UPCC}.


reverse(L) ->
  reverse(L,[]).

reverse([E|Rest],Acc) ->
  reverse(Rest,[E|acc]);
reverse([],Acc) -> Acc.
