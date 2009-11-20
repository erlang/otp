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
%%

%%

-ifndef(public_key).
-define(public_key, true).

-include("OTP-PUB-KEY.hrl").

-record('SubjectPublicKeyInfoAlgorithm', {
 	  algorithm, 
 	  parameters = asn1_NOVALUE}).

-record(path_validation_state, {
	  valid_policy_tree,
	  explicit_policy,
	  inhibit_any_policy,
	  policy_mapping,
	  cert_num,
	  last_cert = false,
	  permitted_subtrees = no_constraints, %% Name constraints
	  excluded_subtrees = [],      %% Name constraints   
	  working_public_key_algorithm,
	  working_public_key,
	  working_public_key_parameters,
	  working_issuer_name,
	  max_path_length,
	  acc_errors,  %% If verify_none option is set
	  user_state
	 }).

-record(policy_tree_node, {
	  valid_policy,
	  qualifier_set,
	  criticality_indicator,
	  expected_policy_set
	 }).

-record(revoke_state, {
	  reasons_mask,
	  cert_status,
	  interim_reasons_mask
	 }).

-endif. % -ifdef(public_key).
