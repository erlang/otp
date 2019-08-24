%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(otp_5481).

-export([?MODULE/0,encode_cc_clpn/1,get_oper_status/2,foo/0]).

?MODULE() ->
    ok.

-record(pchVp, {vplEntry,
                 vplLastChange,
                 vplReceiveTrafficDescrIndex = 0,
                 vplTransmitTrafficDescrIndex = 0,
		vplConnId,
                 vplGroupShapingId}).

-record(pchVc, {vclEntry,
		vclLastChange,
		vplConnId,
		vclConnId,
                 vclShapingMode = 1}).
-record(spvcVpc, {spvcVpcEntry,
		  currentState,
                   spvcRerCap = false,
                   spvcRerStatus = false}).
-record(spvcVpcPerm, {spvcVpcEntry,
                       spvcVpcTargetAddress,
                       spvcVpcTargetSelectType,
                       spvcVpcUserName,
                       spvcVpcProviderName,
                       spvcVpcApplication}).
-record(spvcVcc, {spvcVccEntry,
                   spvcVccTargetAddress,
                   spvcVccTargetSelectType,
                   spvcVccTargetVpi,
                   spvcVccApplication,
                   spvcVccFrKey,
                   spvcVccTranslationMode,
                   spvcRerCap = false,
		   currentState,
                   spvcRerStatus = false}).
-record(spvcVccPerm, {spvcVccEntry,
                       spvcVccTargetAddress,
                       spvcVccTargetSelectType,
                       spvcVccTargetVpi,
                       spvcVccTargetType,
                       spvcVccApplication,
                       spvcVccFrKey,
                       spvcVccTranslationMode = 2}).
-record(spvcObj, {spvcEntry,
                   spvcTargetAddress,
                   spvcTargetSelectType,
                   spvcTargetVpi,
                   spvcTargetVci,
                   spvcLastReleaseCause,
                   spvcLastReleaseDiagnostic,
                   spvcRetryInterval = 1000,
                   spvcRetryTimer = 0,
                   spvcRetryThreshold = 1,
                   spvcRetryFailures = 0,
                   spvcRetryLimit = 15,
                   spvcFrKey,
                   spvcVccTranslationMode = 2,
                   spvcRerCap = false,
                   spvcRerStatus = false}).
-record(spvcTargetVc, {entry,
                        userName = [],
                        providerName = [],
                        opState,
                        rowStatus}).

-record(spvcTargetVp, {entry,
                        userName = [],
                        providerName = [],
                        opState,
                        rowStatus}).

-record(spvcFr, {spvcFrEntry,
                  spvcFrAtmEntry,
                  spvcFrTargetAddress,
                  spvcFrTargetSelectType,
                  spvcFrProviderName,
                  currentState}).

-record(spvcFrPerm, {spvcFrEntry,
                      spvcFrAtmEntry,
                      spvcFrAtmTranslation,
                      spvcFrAdminStatus,
                      spvcFrConnect}).

-record(hci_clpn, {hci_pci,
                    hci_type_of_number,
                    hci_numbering_plan_indicator,
                    hci_presentation_indicator,
                    hci_screening_indicator,
                    hci_number_digits,
                    hci_incomplete_indicator = 0,
                    hci_binary}).

encode_cc_clpn(Spvc) when Spvc#spvcObj.spvcFrKey == undefined ->
     If = case Spvc of
              Spvc when record(Spvc,spvcObj) ->
                  case Spvc#spvcObj.spvcEntry of
                      {If_Value,_,_,_} ->
                          If_Value;
                      {If_Value,_,_} ->
                          If_Value
                  end;
              Spvc when record(Spvc,spvcVcc) ->
                  {If_Value,_,_,_} = Spvc#spvcVcc.spvcVccEntry,
                  If_Value;
              Spvc when record(Spvc,spvcVpc) ->
                  {If_Value,_,_} = Spvc#spvcVpc.spvcVpcEntry,
                  If_Value;
              Spvc when record(Spvc,spvcVpcPerm) ->
                  {If_Value,_,_} = Spvc#spvcVpcPerm.spvcVpcEntry,
                  If_Value;
              Spvc when record(Spvc,spvcVccPerm) ->
                  {If_Value,_,_,_} = Spvc#spvcVccPerm.spvcVccEntry,
                  If_Value;
              Spvc when record(Spvc,spvcTargetVc) ->
                  {If_Value,_,_} = Spvc#spvcTargetVc.entry,
                  If_Value;
              Spvc when record(Spvc,spvcTargetVp) ->
                  {If_Value,_} = Spvc#spvcTargetVp.entry,
                  If_Value;
              Spvc when record(Spvc,pchVc) ->
                  {If_Value,_,_} = Spvc#pchVc.vclEntry,
                  If_Value;
              Spvc when record(Spvc,pchVp) ->
                  {If_Value,_} = Spvc#pchVp.vplEntry,
                  If_Value;
              Spvc when record(Spvc,spvcFr) ->
                  {If_Value,_} = Spvc#spvcFr.spvcFrEntry,
                  If_Value;
              Spvc when record(Spvc,spvcFrPerm) ->
                  {If_Value,_} = Spvc#spvcFrPerm.spvcFrEntry,
                  If_Value;
              {If_Value,_,_,_} ->
                  If_Value;
              {If_Value,_,_} ->
                  If_Value;
              {If_Value,_} ->
                  If_Value;
              [If_Value|_] ->
                  If_Value;
              _ ->
                  error
          end,
     Col = [2],
     SpvcAddress = case x:x(get_next,[If],Col) of
                       [{[2,If,20|Address],_}] ->
                           Address;
                       _ ->
                           lists:duplicate(20,0)
                   end,
     #hci_clpn{hci_type_of_number = 0,
               hci_numbering_plan_indicator = 2,
               hci_presentation_indicator = 0,
               hci_screening_indicator = 1,
               hci_number_digits = SpvcAddress};
encode_cc_clpn(Spvc) ->
     {If,_} = Spvc#spvcObj.spvcFrKey,
     Col = [4],
     SpvcFrAddress = case x:x(get_next,[If],Col) of
                         [{[4,If,20|Address],_}] ->
                             Address;
                         _ ->
                             lists:duplicate(20,0)
                     end,
     #hci_clpn{hci_type_of_number = 0,
               hci_numbering_plan_indicator = 2,
               hci_presentation_indicator = 0,
               hci_screening_indicator = 1,
               hci_number_digits = SpvcFrAddress}.



get_oper_status(spvcVpc,Obj) when record(Obj,spvcVpc) ->
     State = Obj#spvcVpc.currentState,
     LinkState = get_link_opstate(case Obj of
                                      Obj when record(Obj,spvcObj) ->
                                          case Obj#spvcObj.spvcEntry of
                                              {If_Value,_,_,_} ->
                                                  If_Value;
                                              {If_Value,_,_} ->
                                                  If_Value
                                          end;
                                      Obj when record(Obj,spvcVcc) ->
                                          {If_Value,_,_,_} = 
Obj#spvcVcc.spvcVccEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcVpc) ->
                                          {If_Value,_,_} = Obj#spvcVpc.spvcVpcEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcVpcPerm) ->
                                          {If_Value,_,_} = 
Obj#spvcVpcPerm.spvcVpcEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcVccPerm) ->
                                          {If_Value,_,_,_} = 
Obj#spvcVccPerm.spvcVccEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcTargetVc) ->
                                          {If_Value,_,_} = Obj#spvcTargetVc.entry,
                                          If_Value;
                                      Obj when record(Obj,spvcTargetVp) ->
                                          {If_Value,_} = Obj#spvcTargetVp.entry,
                                          If_Value;
                                      Obj when record(Obj,pchVc) ->
                                          {If_Value,_,_} = Obj#pchVc.vclEntry,
                                          If_Value;
                                      Obj when record(Obj,pchVp) ->
                                          {If_Value,_} = Obj#pchVp.vplEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcFr) ->
                                          {If_Value,_} = Obj#spvcFr.spvcFrEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcFrPerm) ->
                                          {If_Value,_} = Obj#spvcFrPerm.spvcFrEntry,
                                          If_Value;
                                      {If_Value,_,_,_} ->
                                          If_Value;
                                      {If_Value,_,_} ->
                                          If_Value;
                                      {If_Value,_} ->
                                          If_Value;
                                      [If_Value|_] ->
                                          If_Value;
                                      _ ->
                                          error
                                  end),
     debug_disabled,
     case {State,LinkState} of
         {not_in_service,_} ->
             10;
         {created,_} ->
             10;
         {_,disabled} ->
             6;
         {wait,_} ->
             2;
         {outgoing_callproceeding,_} ->
             2;
         {release_at_restart,_} ->
             2;
         {active,_} ->
             3;
         {rest_in_peace,_} ->
             4;
         {_Other,_} ->
             1
     end;
get_oper_status(spvcVpc,_) ->
     debug_disabled,
     1;
get_oper_status(spvcVcc,Obj) when record(Obj,spvcVcc) ->
     State = Obj#spvcVcc.currentState,
     LinkState = get_link_opstate(case Obj of
                                      Obj when record(Obj,spvcObj) ->
                                          case Obj#spvcObj.spvcEntry of
                                              {If_Value,_,_,_} ->
                                                  If_Value;
                                              {If_Value,_,_} ->
                                                  If_Value
                                          end;
                                      Obj when record(Obj,spvcVcc) ->
                                          {If_Value,_,_,_} = 
Obj#spvcVcc.spvcVccEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcVpc) ->
                                          {If_Value,_,_} = Obj#spvcVpc.spvcVpcEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcVpcPerm) ->
                                          {If_Value,_,_} = 
Obj#spvcVpcPerm.spvcVpcEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcVccPerm) ->
                                          {If_Value,_,_,_} = 
Obj#spvcVccPerm.spvcVccEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcTargetVc) ->
                                          {If_Value,_,_} = Obj#spvcTargetVc.entry,
                                          If_Value;
                                      Obj when record(Obj,spvcTargetVp) ->
                                          {If_Value,_} = Obj#spvcTargetVp.entry,
                                          If_Value;
                                      Obj when record(Obj,pchVc) ->
                                          {If_Value,_,_} = Obj#pchVc.vclEntry,
                                          If_Value;
                                      Obj when record(Obj,pchVp) ->
                                          {If_Value,_} = Obj#pchVp.vplEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcFr) ->
                                          {If_Value,_} = Obj#spvcFr.spvcFrEntry,
                                          If_Value;
                                      Obj when record(Obj,spvcFrPerm) ->
                                          {If_Value,_} = Obj#spvcFrPerm.spvcFrEntry,
                                          If_Value;
                                      {If_Value,_,_,_} ->
                                          If_Value;
                                      {If_Value,_,_} ->
                                          If_Value;
                                      {If_Value,_} ->
                                          If_Value;
                                      [If_Value|_] ->
                                          If_Value;
                                      _ ->
                                          error
                                  end),
     debug_disabled,
     case {State,LinkState} of
         {not_in_service,_} ->
             10;
         {created,_} ->
             10;
         {_,disabled} ->
             6;
         {wait,_} ->
             2;
         {outgoing_callproceeding,_} ->
             2;
         {release_at_restart,_} ->
             2;
         {active,_} ->
             3;
         {rest_in_peace,_} ->
             4;
         {_Other,_} ->
             1
     end;
get_oper_status(spvcVcc,_) ->
     debug_disabled,
     1;
get_oper_status(spvcTargetVp,Obj) when record(Obj,spvcTargetVp) ->
     debug_disabled,
     Key = Obj#spvcTargetVp.entry,
     case get_link_opstate(case Key of
                               Key when record(Key,spvcObj) ->
                                   case Key#spvcObj.spvcEntry of
                                       {If_Value,_,_,_} ->
                                           If_Value;
                                       {If_Value,_,_} ->
                                           If_Value
                                   end;
                               Key when record(Key,spvcVcc) ->
                                   {If_Value,_,_,_} = Key#spvcVcc.spvcVccEntry,
                                   If_Value;
                               Key when record(Key,spvcVpc) ->
                                   {If_Value,_,_} = Key#spvcVpc.spvcVpcEntry,
                                   If_Value;
                               Key when record(Key,spvcVpcPerm) ->
                                   {If_Value,_,_} = Key#spvcVpcPerm.spvcVpcEntry,
                                   If_Value;
                               Key when record(Key,spvcVccPerm) ->
                                   {If_Value,_,_,_} = Key#spvcVccPerm.spvcVccEntry,
                                   If_Value;
                               Key when record(Key,spvcTargetVc) ->
                                   {If_Value,_,_} = Key#spvcTargetVc.entry,
                                   If_Value;
                               Key when record(Key,spvcTargetVp) ->
                                   {If_Value,_} = Key#spvcTargetVp.entry,
                                   If_Value;
                               Key when record(Key,pchVc) ->
                                   {If_Value,_,_} = Key#pchVc.vclEntry,
                                   If_Value;
                               Key when record(Key,pchVp) ->
                                   {If_Value,_} = Key#pchVp.vplEntry,
                                   If_Value;
                               Key when record(Key,spvcFr) ->
                                   {If_Value,_} = Key#spvcFr.spvcFrEntry,
                                   If_Value;
                               Key when record(Key,spvcFrPerm) ->
                                   {If_Value,_} = Key#spvcFrPerm.spvcFrEntry,
                                   If_Value;
                               {If_Value,_,_,_} ->
                                   If_Value;
                               {If_Value,_,_} ->
                                   If_Value;
                               {If_Value,_} ->
                                   If_Value;
                               [If_Value|_] ->
                                   If_Value;
                               _ ->
                                   error
                           end) of
         disabled ->
             debug_disabled,
             4;
         enabled ->
             debug_disabled,
             case (x:x({pchVp,Key}))#pchVp.vplConnId of
                 undefined ->
                     debug_disabled,
                     3;
                 _ ->
                     debug_disabled,
                     2
             end
     end;
get_oper_status(spvcTargetVp,_) ->
     debug_disabled,
     1;
get_oper_status(spvcTargetVc,Obj) when record(Obj,spvcTargetVc) ->
     debug_disabled,
     Key = Obj#spvcTargetVc.entry,
     case get_link_opstate(case Key of
                               Key when record(Key,spvcObj) ->
                                   case Key#spvcObj.spvcEntry of
                                       {If_Value,_,_,_} ->
                                           If_Value;
                                       {If_Value,_,_} ->
                                           If_Value
                                   end;
                               Key when record(Key,spvcVcc) ->
                                   {If_Value,_,_,_} = Key#spvcVcc.spvcVccEntry,
                                   If_Value;
                               Key when record(Key,spvcVpc) ->
                                   {If_Value,_,_} = Key#spvcVpc.spvcVpcEntry,
                                   If_Value;
                               Key when record(Key,spvcVpcPerm) ->
                                   {If_Value,_,_} = Key#spvcVpcPerm.spvcVpcEntry,
                                   If_Value;
                               Key when record(Key,spvcVccPerm) ->
                                   {If_Value,_,_,_} = Key#spvcVccPerm.spvcVccEntry,
                                   If_Value;
                               Key when record(Key,spvcTargetVc) ->
                                   {If_Value,_,_} = Key#spvcTargetVc.entry,
                                   If_Value;
                               Key when record(Key,spvcTargetVp) ->
                                   {If_Value,_} = Key#spvcTargetVp.entry,
                                   If_Value;
                               Key when record(Key,pchVc) ->
                                   {If_Value,_,_} = Key#pchVc.vclEntry,
                                   If_Value;
                               Key when record(Key,pchVp) ->
                                   {If_Value,_} = Key#pchVp.vplEntry,
                                   If_Value;
                               Key when record(Key,spvcFr) ->
                                   {If_Value,_} = Key#spvcFr.spvcFrEntry,
                                   If_Value;
                               Key when record(Key,spvcFrPerm) ->
                                   {If_Value,_} = Key#spvcFrPerm.spvcFrEntry,
                                   If_Value;
                               {If_Value,_,_,_} ->
                                   If_Value;
                               {If_Value,_,_} ->
                                   If_Value;
                               {If_Value,_} ->
                                   If_Value;
                               [If_Value|_] ->
                                   If_Value;
                               _ ->
                                   error
                           end) of
         disabled ->
             debug_disabled,
             4;
         enabled ->
             debug_disabled,
             case (x:x({pchVc,Key}))#pchVc.vclConnId of
                 undefined ->
                     debug_disabled,
                     3;
                 _ ->
                     debug_disabled,
                     2
             end
     end;
get_oper_status(spvcTargetVc,_) ->
     debug_disabled,
     1.

get_link_opstate(If) ->
     debug_disabled,
     case x:x(x:x(If),cnhChi,get_link_opstate,[If]) of
         {genError,_} ->
             debug_disabled,
             disabled;
         Return ->
             debug_disabled,
             Return
     end.

-record(record_A,{
	  field_1,
	  field_2,
	  field_3,
	  field_4,
	  field_5
	 }).
-record(record_B, { field_1 }).
-record(record_C, { }).

foo() ->
     case something of
	[#record_A{} = A] ->
  	    B = foo3(#record_C{}),
	    C = element(B, A),
	    foo2(A),
   	    D = C#record_B.field_1,
	    foo4(A#record_A.field_4,
		 B,
		 #record_C{},
		 D)
     end.

foo2(_) -> ok.
foo3(_) -> 1.
foo4(_,_,_,_) -> ok.

