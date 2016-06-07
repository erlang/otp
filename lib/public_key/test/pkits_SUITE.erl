%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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


%%  Se specification here:
%%  http://csrc.nist.gov/groups/ST/crypto_apps_infra/pki/pkitesting.html

-module(pkits_SUITE).

-include_lib("public_key/include/public_key.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(error(Format,Args), error(Format,Args,?FILE,?LINE)).
-define(warning(Format,Args), warning(Format,Args,?FILE,?LINE)).

-define(CERTS, "pkits/certs").
-define(MIME,  "pkits/smime").
-define(CONV,  "pkits/smime-pem").
-define(CRL,   "pkits/crls").

-define(NIST1, "2.16.840.1.101.3.2.1.48.1").
-define(NIST2, "2.16.840.1.101.3.2.1.48.2").
-define(NIST3, "2.16.840.1.101.3.2.1.48.3").
-define(NIST4, "2.16.840.1.101.3.2.1.48.4").
-define(NIST5, "2.16.840.1.101.3.2.1.48.5").
-define(NIST6, "2.16.840.1.101.3.2.1.48.6").

-record(verify_state, {
	  crls,
	  crl_paths,
	  revoke_state}).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [].

all() -> 
    [{group, signature_verification},
     {group, validity_periods},
     {group, verifying_name_chaining},
     {group, verifying_paths_with_self_issued_certificates},
     {group, basic_certificate_revocation_tests},
     {group, delta_crls},
     {group, distribution_points},
     {group, verifying_basic_constraints},
     {group, key_usage},
     {group, name_constraints},
     {group, private_certificate_extensions}].

groups() -> 
    [{signature_verification, [], [valid_rsa_signature,
				   invalid_rsa_signature, valid_dsa_signature,
				   invalid_dsa_signature]},
     {validity_periods, [],
      [not_before_invalid, not_before_valid, not_after_invalid, not_after_valid]},
     {verifying_name_chaining, [],
      [invalid_name_chain, whitespace_name_chain, capitalization_name_chain,
       uid_name_chain, attrib_name_chain, string_name_chain]},
     {verifying_paths_with_self_issued_certificates, [],
      [basic_valid, basic_invalid, crl_signing_valid, crl_signing_invalid]},
     {basic_certificate_revocation_tests, [],
      [missing_CRL,
       revoked_CA,
       revoked_peer,
       invalid_CRL_signature,
       invalid_CRL_issuer, invalid_CRL, valid_CRL,
       unknown_CRL_extension, old_CRL, fresh_CRL, valid_serial,
       invalid_serial, valid_seperate_keys, invalid_separate_keys]},
     {delta_crls, [], [delta_without_crl, valid_delta_crls, invalid_delta_crls]},
     {distribution_points, [], [valid_distribution_points,
				valid_distribution_points_no_issuing_distribution_point,
				invalid_distribution_points, valid_only_contains,
				invalid_only_contains, valid_only_some_reasons,
				invalid_only_some_reasons, valid_indirect_crl,
				invalid_indirect_crl, valid_crl_issuer, invalid_crl_issuer]},
     {verifying_basic_constraints,[],
      [missing_basic_constraints, valid_basic_constraint, invalid_path_constraints,
       valid_path_constraints]},
     {key_usage, [],
      [invalid_key_usage, valid_key_usage]},
     {name_constraints, [],
      [valid_DN_name_constraints, invalid_DN_name_constraints,
       valid_rfc822_name_constraints,
       invalid_rfc822_name_constraints, valid_DN_and_rfc822_name_constraints,
       invalid_DN_and_rfc822_name_constraints, valid_dns_name_constraints,
       invalid_dns_name_constraints, valid_uri_name_constraints,
       invalid_uri_name_constraints]},
     {private_certificate_extensions, [],
      [unknown_critical_extension, unknown_not_critical_extension]}
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:stop(crypto),
    try crypto:start() of
	ok ->
	    application:start(asn1),
	    crypto_support_check(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(asn1),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.
%%--------------------------------------------------------------------
init_per_testcase(_Func, Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    put(datadir, Datadir),
    Config.

end_per_testcase(_Func, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

%%--------------------------- signature_verification--------------------------------------------------
valid_rsa_signature() ->
    [{doc, "Test rsa signatur verification"}].
valid_rsa_signature(Config) when is_list(Config) ->
    run([{ "4.1.1", "Valid Certificate Path Test1 EE", ok}]).

invalid_rsa_signature() ->
    [{doc,"Test rsa signatur verification"}].
invalid_rsa_signature(Config) when is_list(Config) ->
    run([{ "4.1.2", "Invalid CA Signature Test2 EE", {bad_cert,invalid_signature}},
	 { "4.1.3", "Invalid EE Signature Test3 EE", {bad_cert,invalid_signature}}]).

valid_dsa_signature() ->
    [{doc,"Test dsa signatur verification"}].
valid_dsa_signature(Config) when is_list(Config) ->
    run([{ "4.1.4", "Valid DSA Signatures Test4 EE", ok},
	 { "4.1.5", "Valid DSA Parameter Inheritance Test5 EE", ok}]).

invalid_dsa_signature() ->
    [{doc,"Test dsa signatur verification"}].
invalid_dsa_signature(Config) when is_list(Config) ->
    run([{ "4.1.6", "Invalid DSA Signature Test6 EE",{bad_cert,invalid_signature}}]).

%%-----------------------------validity_periods------------------------------------------------
not_before_invalid() ->
    [{doc,"Test valid periods"}].
not_before_invalid(Config) when is_list(Config) ->
    run([{ "4.2.1", "Invalid CA notBefore Date Test1 EE",{bad_cert, cert_expired}},
	 { "4.2.2", "Invalid EE notBefore Date Test2 EE",{bad_cert, cert_expired}}]).

not_before_valid() ->
    [{doc,"Test valid periods"}].
not_before_valid(Config) when is_list(Config) ->
    run([{ "4.2.3", "Valid pre2000 UTC notBefore Date Test3 EE", ok},
	 { "4.2.4", "Valid GeneralizedTime notBefore Date Test4 EE", ok}]).

not_after_invalid() ->
    [{doc,"Test valid periods"}].
not_after_invalid(Config) when is_list(Config) ->
    run([{ "4.2.5", "Invalid CA notAfter Date Test5 EE", {bad_cert, cert_expired}},
	 { "4.2.6", "Invalid EE notAfter Date Test6 EE", {bad_cert, cert_expired}},
	 { "4.2.7", "Invalid pre2000 UTC EE notAfter Date Test7 EE",{bad_cert, cert_expired}}]).

not_after_valid() ->
    [{doc,"Test valid periods"}].
not_after_valid(Config) when is_list(Config) ->
    run([{ "4.2.8", "Valid GeneralizedTime notAfter Date Test8 EE", ok}]).

%%----------------------------verifying_name_chaining-------------------------------------------------
invalid_name_chain() ->
    [{doc,"Test name chaining"}].
invalid_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.1", "Invalid Name Chaining Test1 EE", {bad_cert, invalid_issuer}},
	 { "4.3.2", "Invalid Name Chaining Order Test2 EE", {bad_cert, invalid_issuer}}]).

whitespace_name_chain() ->
    [{doc,"Test name chaining"}].
whitespace_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.3", "Valid Name Chaining Whitespace Test3 EE", ok},
	 { "4.3.4", "Valid Name Chaining Whitespace Test4 EE", ok}]).

capitalization_name_chain() ->
    [{doc,"Test name chaining"}].
capitalization_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.5", "Valid Name Chaining Capitalization Test5 EE",ok}]).

uid_name_chain() ->
    [{doc,"Test name chaining"}].
uid_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.6", "Valid Name UIDs Test6 EE",ok}]).

attrib_name_chain() ->
    [{doc,"Test name chaining"}].
attrib_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.7", "Valid RFC3280 Mandatory Attribute Types Test7 EE", ok},
	 { "4.3.8", "Valid RFC3280 Optional Attribute Types Test8 EE",  ok}]).

string_name_chain() ->
    [{doc,"Test name chaining"}].
string_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.9", "Valid UTF8String Encoded Names Test9 EE", ok},
	 %%{ "4.3.10", "Valid Rollover from PrintableString to UTF8String Test10 EE", ok},
	 { "4.3.11", "Valid UTF8String Case Insensitive Match Test11 EE", ok}]).

%%----------------------------verifying_paths_with_self_issued_certificates-------------------------------------------------
basic_valid() ->
    [{doc,"Test self issued certificates"}].
basic_valid(Config) when is_list(Config) ->
    run([{ "4.5.1",  "Valid Basic Self-Issued Old With New Test1 EE", ok},
	 { "4.5.3",  "Valid Basic Self-Issued New With Old Test3 EE", ok},
	 { "4.5.4",  "Valid Basic Self-Issued New With Old Test4 EE", ok}
	]).

basic_invalid() ->
    [{doc,"Test self issued certificates"}].
basic_invalid(Config) when is_list(Config) ->
    run([{"4.5.2",  "Invalid Basic Self-Issued Old With New Test2 EE",
	  {bad_cert, {revoked, keyCompromise}}},
	 {"4.5.5",  "Invalid Basic Self-Issued New With Old Test5 EE",
	  {bad_cert, {revoked, keyCompromise}}}
	]).

crl_signing_valid() ->
    [{doc,"Test self issued certificates"}].
crl_signing_valid(Config) when is_list(Config) ->
    run([{ "4.5.6",  "Valid Basic Self-Issued CRL Signing Key Test6 EE", ok}]).

crl_signing_invalid() ->
    [{doc,"Test self issued certificates"}].
crl_signing_invalid(Config) when is_list(Config) ->
    run([{ "4.5.7",  "Invalid Basic Self-Issued CRL Signing Key Test7 EE",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.5.8",  "Invalid Basic Self-Issued CRL Signing Key Test8 EE",
	   {bad_cert, invalid_key_usage}}
	]).

%%-----------------------------basic_certificate_revocation_tests------------------------------------------------
missing_CRL() ->
    [{doc,"Test basic CRL handling"}].
missing_CRL(Config) when is_list(Config) ->
    run([{ "4.4.1", "Invalid Missing CRL Test1 EE",{bad_cert,
    revocation_status_undetermined}}]).

revoked_CA() ->
    [{doc,"Test basic CRL handling"}].
revoked_CA(Config) when is_list(Config) ->
    run([{ "4.4.2", "Invalid Revoked CA Test2 EE", {bad_cert,
    {revoked, keyCompromise}}}]).

revoked_peer() ->
    [{doc,"Test basic CRL handling"}].
revoked_peer(Config) when is_list(Config) ->
    run([{ "4.4.3", "Invalid Revoked EE Test3 EE",
	   {bad_cert, {revoked, keyCompromise}}}]).

invalid_CRL_signature() ->
    [{doc,"Test basic CRL handling"}].
invalid_CRL_signature(Config) when is_list(Config) ->
    run([{ "4.4.4", "Invalid Bad CRL Signature Test4 EE",
		   {bad_cert, revocation_status_undetermined}}]).
invalid_CRL_issuer() ->
    [{doc,"Test basic CRL handling"}].
invalid_CRL_issuer(Config) when is_list(Config) ->
    run({ "4.4.5", "Invalid Bad CRL Issuer Name Test5 EE",
	  {bad_cert, revocation_status_undetermined}}).

invalid_CRL() ->
    [{doc,"Test basic CRL handling"}].
invalid_CRL(Config) when is_list(Config) ->
    run([{ "4.4.6", "Invalid Wrong CRL Test6 EE",
	   {bad_cert, revocation_status_undetermined}}]).

valid_CRL() ->
    [{doc,"Test basic CRL handling"}].
valid_CRL(Config) when is_list(Config) ->
    run([{ "4.4.7", "Valid Two CRLs Test7 EE", ok}]).

unknown_CRL_extension() ->
    [{doc,"Test basic CRL handling"}].
unknown_CRL_extension(Config) when is_list(Config) ->
    run([{ "4.4.8", "Invalid Unknown CRL Entry Extension Test8 EE",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.4.9", "Invalid Unknown CRL Extension Test9 EE",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.4.10", "Invalid Unknown CRL Extension Test10 EE",
	   {bad_cert, revocation_status_undetermined}}]).

old_CRL() ->
    [{doc,"Test basic CRL handling"}].
old_CRL(Config) when is_list(Config) ->
    run([{ "4.4.11", "Invalid Old CRL nextUpdate Test11 EE",
	   {bad_cert, revocation_status_undetermined}},
	 { "4.4.12", "Invalid pre2000 CRL nextUpdate Test12 EE",
	   {bad_cert, revocation_status_undetermined}}]).

fresh_CRL() ->
    [{doc,"Test basic CRL handling"}].
fresh_CRL(Config) when is_list(Config) ->
    run([{ "4.4.13", "Valid GeneralizedTime CRL nextUpdate Test13 EE", ok}]).

valid_serial() ->
    [{doc,"Test basic CRL handling"}].
valid_serial(Config) when is_list(Config) ->
    run([
	 { "4.4.14", "Valid Negative Serial Number Test14 EE",ok},
	 { "4.4.16", "Valid Long Serial Number Test16 EE", ok},
	 { "4.4.17", "Valid Long Serial Number Test17 EE", ok}
	]).

invalid_serial() ->
    [{doc,"Test basic CRL handling"}].
invalid_serial(Config) when is_list(Config) ->
    run([{ "4.4.15", "Invalid Negative Serial Number Test15 EE",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.4.18", "Invalid Long Serial Number Test18 EE",
	   {bad_cert, {revoked, keyCompromise}}}]).

valid_seperate_keys() ->
    [{doc,"Test basic CRL handling"}].
valid_seperate_keys(Config) when is_list(Config) ->
    run([{ "4.4.19", "Valid Separate Certificate and CRL Keys Test19 EE",   ok}]).

invalid_separate_keys() ->
    [{doc,"Test basic CRL handling"}].
invalid_separate_keys(Config) when is_list(Config) ->
    run([{ "4.4.20", "Invalid Separate Certificate and CRL Keys Test20 EE",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.4.21", "Invalid Separate Certificate and CRL Keys Test21 EE",
	   {bad_cert, revocation_status_undetermined}}
	]).
%%----------------------------verifying_basic_constraints-------------------------------------------------
missing_basic_constraints() ->
    [{doc,"Basic constraint tests"}].
missing_basic_constraints(Config) when is_list(Config) ->
    run([{ "4.6.1",  "Invalid Missing basicConstraints Test1 EE",
	   {bad_cert, missing_basic_constraint}},
	 { "4.6.2",  "Invalid cA False Test2 EE",
	   {bad_cert, missing_basic_constraint}},
	 { "4.6.3",  "Invalid cA False Test3 EE",
	   {bad_cert, missing_basic_constraint}}]).

valid_basic_constraint() ->
    [{doc,"Basic constraint tests"}].
valid_basic_constraint(Config) when is_list(Config) ->
    run([{"4.6.4", "Valid basicConstraints Not Critical Test4 EE", ok}]).

invalid_path_constraints() ->
    [{doc,"Basic constraint tests"}].
invalid_path_constraints(Config) when is_list(Config) ->
    run([{ "4.6.5", "Invalid pathLenConstraint Test5 EE", {bad_cert, max_path_length_reached}},
	 { "4.6.6", "Invalid pathLenConstraint Test6 EE", {bad_cert, max_path_length_reached}},
	 { "4.6.9", "Invalid pathLenConstraint Test9 EE", {bad_cert, max_path_length_reached}},
	 { "4.6.10", "Invalid pathLenConstraint Test10 EE", {bad_cert, max_path_length_reached}},
	 { "4.6.11", "Invalid pathLenConstraint Test11 EE", {bad_cert, max_path_length_reached}},
	 { "4.6.12", "Invalid pathLenConstraint Test12 EE", {bad_cert, max_path_length_reached}},
	 { "4.6.16", "Invalid Self-Issued pathLenConstraint Test16 EE",
	   {bad_cert, max_path_length_reached}}]).

valid_path_constraints() ->
    [{doc,"Basic constraint tests"}].
valid_path_constraints(Config) when is_list(Config) ->
    run([{ "4.6.7",  "Valid pathLenConstraint Test7 EE", ok},
	 { "4.6.8",  "Valid pathLenConstraint Test8 EE", ok},
	 { "4.6.13", "Valid pathLenConstraint Test13 EE", ok},
	 { "4.6.14", "Valid pathLenConstraint Test14 EE", ok},
	 { "4.6.15", "Valid Self-Issued pathLenConstraint Test15 EE", ok},
	 { "4.6.17", "Valid Self-Issued pathLenConstraint Test17 EE", ok}]).

%%-----------------------------key_usage------------------------------------------------
invalid_key_usage() ->
    [{doc,"Key usage tests"}].
invalid_key_usage(Config) when is_list(Config) ->
    run([{ "4.7.1",  "Invalid keyUsage Critical keyCertSign False Test1 EE",
	   {bad_cert,invalid_key_usage} },
	 { "4.7.2",  "Invalid keyUsage Not Critical keyCertSign False Test2 EE",
	   {bad_cert,invalid_key_usage}},
	 { "4.7.4",  "Invalid keyUsage Critical cRLSign False Test4 EE",
	   {bad_cert, invalid_key_usage}},
	 { "4.7.5",  "Invalid keyUsage Not Critical cRLSign False Test5 EE",
	   {bad_cert, invalid_key_usage}}
	]).

valid_key_usage() ->
    [{doc,"Key usage tests"}].
valid_key_usage(Config) when is_list(Config) ->
    run([{ "4.7.3",  "Valid keyUsage Not Critical Test3 EE", ok}]).

%%-----------------------------------------------------------------------------
certificate_policies() ->
    [{doc,"Not supported yet"}].
certificate_policies(Config) when is_list(Config) ->
    run(certificate_policies_tests()).
%%-----------------------------------------------------------------------------
require_explicit_policy() ->
    [{doc,"Not supported yet"}].
require_explicit_policy(Config) when is_list(Config) ->
    run(require_explicit_policy_tests()).
%%-----------------------------------------------------------------------------
policy_mappings() ->
    [{doc,"Not supported yet"}].
policy_mappings(Config) when is_list(Config) ->
    run(policy_mappings_tests()).
%%-----------------------------------------------------------------------------
inhibit_policy_mapping() ->
    [{doc,"Not supported yet"}].
inhibit_policy_mapping(Config) when is_list(Config) ->
    run(inhibit_policy_mapping_tests()).
%%-----------------------------------------------------------------------------
inhibit_any_policy() ->
    [{doc,"Not supported yet"}].
inhibit_any_policy(Config) when is_list(Config) ->
    run(inhibit_any_policy_tests()).
%%-------------------------------name_constraints----------------------------------------------

valid_DN_name_constraints() ->
    [{doc, "Name constraints tests"}].
valid_DN_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.1",  "Valid DN nameConstraints Test1 EE", ok},
	 { "4.13.4",  "Valid DN nameConstraints Test4 EE", ok},
	 { "4.13.5",  "Valid DN nameConstraints Test5 EE", ok},
	 { "4.13.6",  "Valid DN nameConstraints Test6 EE", ok},
	 { "4.13.11", "Valid DN nameConstraints Test11 EE", ok},
	 { "4.13.14", "Valid DN nameConstraints Test14 EE", ok},
	 { "4.13.18", "Valid DN nameConstraints Test18 EE", ok},
	 { "4.13.19", "Valid DN nameConstraints Test19 EE", ok}]).

invalid_DN_name_constraints() ->
    [{doc,"Name constraints tests"}].
invalid_DN_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.2", "Invalid DN nameConstraints Test2 EE", {bad_cert, name_not_permitted}},
	 { "4.13.3",  "Invalid DN nameConstraints Test3 EE", {bad_cert, name_not_permitted}},
	 { "4.13.7",  "Invalid DN nameConstraints Test7 EE", {bad_cert, name_not_permitted}},
	 { "4.13.8",  "Invalid DN nameConstraints Test8 EE", {bad_cert, name_not_permitted}},
	 { "4.13.9",  "Invalid DN nameConstraints Test9 EE", {bad_cert, name_not_permitted}},
	 { "4.13.10", "Invalid DN nameConstraints Test10 EE",{bad_cert, name_not_permitted}},
	 { "4.13.12", "Invalid DN nameConstraints Test12 EE",{bad_cert, name_not_permitted}},
	 { "4.13.13", "Invalid DN nameConstraints Test13 EE",{bad_cert, name_not_permitted}},
	 { "4.13.15", "Invalid DN nameConstraints Test15 EE",{bad_cert, name_not_permitted}},
	 { "4.13.16", "Invalid DN nameConstraints Test16 EE",{bad_cert, name_not_permitted}},
	 { "4.13.17", "Invalid DN nameConstraints Test17 EE",{bad_cert, name_not_permitted}},
	 { "4.13.20", "Invalid DN nameConstraints Test20 EE",
	   {bad_cert, name_not_permitted}}]).

valid_rfc822_name_constraints() ->
    [{doc,"Name constraints tests"}].
valid_rfc822_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.21", "Valid RFC822 nameConstraints Test21 EE", ok},
	 { "4.13.23", "Valid RFC822 nameConstraints Test23 EE", ok},
	 { "4.13.25", "Valid RFC822 nameConstraints Test25 EE", ok}]).

invalid_rfc822_name_constraints() ->
    [{doc,"Name constraints tests"}].
invalid_rfc822_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.22", "Invalid RFC822 nameConstraints Test22 EE",
	   {bad_cert, name_not_permitted}},
	 { "4.13.24", "Invalid RFC822 nameConstraints Test24 EE",
	   {bad_cert, name_not_permitted}},
	 { "4.13.26", "Invalid RFC822 nameConstraints Test26 EE",
	   {bad_cert, name_not_permitted}}]).

valid_DN_and_rfc822_name_constraints() ->
    [{doc,"Name constraints tests"}].
valid_DN_and_rfc822_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.27", "Valid DN and RFC822 nameConstraints Test27 EE", ok}]).

invalid_DN_and_rfc822_name_constraints() ->
    [{doc,"Name constraints tests"}].
invalid_DN_and_rfc822_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.28", "Invalid DN and RFC822 nameConstraints Test28 EE",
	   {bad_cert, name_not_permitted}},
	 { "4.13.29", "Invalid DN and RFC822 nameConstraints Test29 EE",
	   {bad_cert, name_not_permitted}}]).

valid_dns_name_constraints() ->
    [{doc,"Name constraints tests"}].
valid_dns_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.30", "Valid DNS nameConstraints Test30 EE", ok},
	 { "4.13.32", "Valid DNS nameConstraints Test32 EE", ok}]).

invalid_dns_name_constraints() ->
    [{doc,"Name constraints tests"}].
invalid_dns_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.31", "Invalid DNS nameConstraints Test31 EE", {bad_cert, name_not_permitted}},
	 { "4.13.33", "Invalid DNS nameConstraints Test33 EE", {bad_cert, name_not_permitted}},
	 { "4.13.38", "Invalid DNS nameConstraints Test38 EE", {bad_cert, name_not_permitted}}]).

valid_uri_name_constraints() ->
    [{doc,"Name constraints tests"}].
valid_uri_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.34", "Valid URI nameConstraints Test34 EE", ok},
	 { "4.13.36", "Valid URI nameConstraints Test36 EE", ok}]).

invalid_uri_name_constraints() ->
    [{doc,"Name constraints tests"}].
invalid_uri_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.35", "Invalid URI nameConstraints Test35 EE",{bad_cert, name_not_permitted}},
	 { "4.13.37", "Invalid URI nameConstraints Test37 EE",{bad_cert, name_not_permitted}}]).

%%------------------------------delta_crls-----------------------------------------------
delta_without_crl() ->
    [{doc,"Delta CRL tests"}].
delta_without_crl(Config) when is_list(Config) ->
  run([{ "4.15.1",  "Invalid deltaCRLIndicator No Base Test1 EE",{bad_cert,
							       revocation_status_undetermined}},
       {"4.15.10", "Invalid delta-CRL Test10 EE", {bad_cert,
						   revocation_status_undetermined}}]).
valid_delta_crls() ->
    [{doc,"Delta CRL tests"}].
valid_delta_crls(Config) when is_list(Config) ->
    run([{ "4.15.2",  "Valid delta-CRL Test2 EE", ok},
	 { "4.15.5",  "Valid delta-CRL Test5 EE", ok},
	 { "4.15.7",  "Valid delta-CRL Test7 EE", ok},
	 { "4.15.8",  "Valid delta-CRL Test8 EE", ok}
	]).

invalid_delta_crls() ->
    [{doc,"Delta CRL tests"}].
invalid_delta_crls(Config) when is_list(Config) ->
    run([{ "4.15.3",  "Invalid delta-CRL Test3 EE", {bad_cert,{revoked, keyCompromise}}},
	 { "4.15.4",  "Invalid delta-CRL Test4 EE", {bad_cert,{revoked, keyCompromise}}},
	 { "4.15.6",  "Invalid delta-CRL Test6 EE", {bad_cert,{revoked, keyCompromise}}},
	 { "4.15.9",  "Invalid delta-CRL Test9 EE", {bad_cert,{revoked, keyCompromise}}}]).

%%---------------------------distribution_points--------------------------------------------------
valid_distribution_points() ->
    [{doc,"CRL Distribution Point tests"}].
valid_distribution_points(Config) when is_list(Config) ->
    run([{ "4.14.1",  "Valid distributionPoint Test1 EE", ok},
	 { "4.14.4",  "Valid distributionPoint Test4 EE", ok},
	 { "4.14.5",  "Valid distributionPoint Test5 EE", ok},
	 { "4.14.7",  "Valid distributionPoint Test7 EE", ok}
	]).

valid_distribution_points_no_issuing_distribution_point() ->
    [{doc,"CRL Distribution Point tests"}].
valid_distribution_points_no_issuing_distribution_point(Config) when is_list(Config) ->
    run([{ "4.14.10", "Valid No issuingDistributionPoint Test10 EE", ok}
	]).

invalid_distribution_points() ->
    [{doc,"CRL Distribution Point tests"}].
invalid_distribution_points(Config) when is_list(Config) ->
    run([{ "4.14.2",  "Invalid distributionPoint Test2 EE", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.3",  "Invalid distributionPoint Test3 EE", {bad_cert,
							  revocation_status_undetermined}},
	 { "4.14.6",  "Invalid distributionPoint Test6 EE", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.8",  "Invalid distributionPoint Test8 EE", {bad_cert,
							  revocation_status_undetermined}},
	 { "4.14.9",  "Invalid distributionPoint Test9 EE", {bad_cert,
							  revocation_status_undetermined}}
	]).

valid_only_contains() ->
    [{doc,"CRL Distribution Point tests"}].
valid_only_contains(Config) when is_list(Config) ->
    run([{ "4.14.13", "Valid only Contains CA Certs Test13 EE", ok}]).

invalid_only_contains() ->
    [{doc,"CRL Distribution Point tests"}].
invalid_only_contains(Config) when is_list(Config) ->
    run([{ "4.14.11", "Invalid onlyContainsUserCerts Test11 EE",
	   {bad_cert, revocation_status_undetermined}},
	 { "4.14.12", "Invalid onlyContainsCACerts Test12 EE",
	   {bad_cert, revocation_status_undetermined}},
	 { "4.14.14", "Invalid onlyContainsAttributeCerts Test14 EE",
	   {bad_cert, revocation_status_undetermined}}
	]).

valid_only_some_reasons() ->
    [{doc,"CRL Distribution Point tests"}].
valid_only_some_reasons(Config) when is_list(Config) ->
    run([{ "4.14.18", "Valid onlySomeReasons Test18 EE", ok},
	 { "4.14.19", "Valid onlySomeReasons Test19 EE", ok}
	]).

invalid_only_some_reasons() ->
    [{doc,"CRL Distribution Point tests"}].
invalid_only_some_reasons(Config) when is_list(Config) ->
    run([{ "4.14.15", "Invalid onlySomeReasons Test15 EE",
	   {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.16", "Invalid onlySomeReasons Test16 EE",
	   {bad_cert,{revoked, certificateHold}}},
	 { "4.14.17", "Invalid onlySomeReasons Test17 EE",
	   {bad_cert, revocation_status_undetermined}},
	 { "4.14.20", "Invalid onlySomeReasons Test20 EE",
	   {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.21", "Invalid onlySomeReasons Test21 EE",
	   {bad_cert,{revoked, affiliationChanged}}}
	]).

valid_indirect_crl() ->
    [{doc,"CRL Distribution Point tests"}].
valid_indirect_crl(Config) when is_list(Config) ->
    run([{ "4.14.22", "Valid IDP with indirectCRL Test22 EE", ok},
	 { "4.14.24", "Valid IDP with indirectCRL Test24 EE", ok},
	 { "4.14.25", "Valid IDP with indirectCRL Test25 EE", ok}
	]).

invalid_indirect_crl() ->
    [{doc,"CRL Distribution Point tests"}].
invalid_indirect_crl(Config) when is_list(Config) ->
    run([{ "4.14.23", "Invalid IDP with indirectCRL Test23 EE",
	   {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.26", "Invalid IDP with indirectCRL Test26 EE",
	   {bad_cert, revocation_status_undetermined}}
	]).

valid_crl_issuer() ->
    [{doc,"CRL Distribution Point tests"}].
valid_crl_issuer(Config) when is_list(Config) ->
    run([{ "4.14.28", "Valid cRLIssuer Test28 EE", ok},
	 { "4.14.29", "Valid cRLIssuer Test29 EE", ok},
	 { "4.14.33", "Valid cRLIssuer Test33 EE", ok}
	]).

invalid_crl_issuer() ->
    [{doc,"CRL Distribution Point tests"}].
invalid_crl_issuer(Config) when is_list(Config) ->
    run([
	 { "4.14.27", "Invalid cRLIssuer Test27 EE", {bad_cert, revocation_status_undetermined}},
	 { "4.14.31", "Invalid cRLIssuer Test31 EE", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.32", "Invalid cRLIssuer Test32 EE", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.34", "Invalid cRLIssuer Test34 EE", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.35", "Invalid cRLIssuer Test35 EE", {bad_cert, revocation_status_undetermined}}
	]).

%% Although this test is valid it has a circular dependency. As a result
%% an attempt is made to reursively checks a CRL path and rejected due to
%% a CRL path validation error. PKITS notes suggest this test does not
%% need to be run due to this issue.
%%	 { "4.14.30", "Valid cRLIssuer Test30", 54 }


%%-------------------------------private_certificate_extensions----------------------------------------------

unknown_critical_extension() ->
    [{doc,"Test that a cert with an unknown critical extension is recjected"}].
unknown_critical_extension(Config) when is_list(Config) ->
    run([{ "4.16.2",  "Invalid Unknown Critical Certificate Extension Test2 EE",
	   {bad_cert,unknown_critical_extension}}]).

unknown_not_critical_extension() ->
    [{doc,"Test that a not critical unknown extension is ignored"}].
unknown_not_critical_extension(Config) when is_list(Config) ->
    run([{ "4.16.1",  "Valid Unknown Not Critical Certificate Extension Test1 EE", ok}]).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

run(Tests) ->    
    [TA] = read_certs("Trust Anchor Root Certificate"),
    run(Tests, TA).

run({Chap, Test, Result}, TA) ->
    CertChain = cas(Chap) ++ read_certs(Test),
    Options = path_validation_options(TA, Chap,Test),
    try public_key:pkix_path_validation(TA, CertChain, Options) of
	{Result, _} -> ok;
	{error,Result} when Result =/= ok ->
	    ok;
	{error, Error}  ->
	    ?error(" ~p ~p~n  Expected ~p got ~p ~n", [Chap, Test, Result, Error]),
	    fail;
	{ok, _OK} when Result =/= ok ->
	    ?error(" ~p ~p~n  Expected ~p got ~p ~n", [Chap, Test, Result, ok]),
	    fail
    catch Type:Reason ->
	    Stack = erlang:get_stacktrace(),
	    io:format("Crash ~p:~p in ~p~n",[Type,Reason,Stack]),
	    io:format("   ~p ~p Expected ~p ~n", [Chap, Test, Result]),
            exit(crash)
    end;

run([Test|Rest],TA) ->
    run(Test,TA),
    run(Rest,TA);
run([],_) -> ok.

path_validation_options(TA, Chap, Test) ->
    case needs_crl_options(Chap) of
	true ->
	    crl_options(TA, Chap, Test);
	false ->
	     Fun =
		fun(_,{bad_cert, _} = Reason, _) ->
			{fail, Reason};
		   (_,{extension, _}, UserState) ->
			{unknown, UserState};
		   (_, Valid, UserState) when Valid == valid;
					      Valid == valid_peer ->
			{valid, UserState}
		 end,
	    [{verify_fun, {Fun, []}}]
    end.

read_certs(Test) ->
    File = cert_file(Test),
    Ders = erl_make_certs:pem_to_der(File),
    [Cert || {'Certificate', Cert, not_encrypted} <- Ders].

read_crls(Test) ->
    File = crl_file(Test),
    Ders = erl_make_certs:pem_to_der(File),
    [CRL || {'CertificateList', CRL, not_encrypted} <- Ders].

cert_file(Test) ->
    file(?CONV, lists:append(string:tokens(Test, " -")) ++ ".pem").

crl_file(Test) ->
    file(?CRL, lists:append(string:tokens(Test, " -")) ++ ".pem").


file(Sub,File) ->
    TestDir = case get(datadir) of
		  undefined -> "./pkits_SUITE_data";
		  Dir when is_list(Dir) ->
		      Dir
	      end,
    AbsFile = filename:join([TestDir,Sub,File]),
    case filelib:is_file(AbsFile) of
	true -> ok;
	false ->
	    ?error("Couldn't read data from ~p ~n",[AbsFile])
    end,
    AbsFile.

error(Format, Args, File0, Line) ->
    File = filename:basename(File0),
    Pid = group_leader(),
    Pid ! {failed, File, Line},
    io:format(Pid, "~s(~p): ERROR"++Format, [File,Line|Args]).

warning(Format, Args, File0, Line) ->
    File = filename:basename(File0),
    io:format("~s(~p): Warning "++Format, [File,Line|Args]).

crypto_support_check(Config) ->
    CryptoSupport = crypto:supports(),
    Hashs = proplists:get_value(hashs, CryptoSupport),
    case proplists:get_bool(sha256, Hashs) of
	true ->
	    Config;
	false ->
	    {skip, "To old version of openssl"}
    end.

needs_crl_options("4.4" ++ _) ->
    true;
needs_crl_options("4.5" ++ _) ->
    true;
needs_crl_options("4.7.4" ++ _) ->
    true;
needs_crl_options("4.7.5" ++ _) ->
    true;
needs_crl_options("4.14" ++ _) ->
    true;
needs_crl_options("4.15" ++ _) ->
    true;
needs_crl_options(_) ->
    false.

crl_options(_TA, Chap, _Test) ->
    CRLNames = crl_names(Chap),
    CRLs = crls(CRLNames),
    Paths = lists:map(fun(CRLName) -> crl_path(CRLName) end, CRLNames),

    ct:print("Paths ~p ~n  Names ~p ~n", [Paths, CRLNames]),
    Fun =
	fun(_,{bad_cert, _} = Reason, _) ->
		{fail, Reason};
	   (_,{extension,
	       #'Extension'{extnID = ?'id-ce-cRLDistributionPoints',
			    extnValue = Value}}, UserState0) ->
		UserState = update_crls(Value, UserState0),
		{valid, UserState};
	   (_,{extension, _}, UserState) ->
		{unknown, UserState};
	   (OtpCert, Valid, UserState) when Valid == valid;
					    Valid == valid_peer ->
		DerCRLs = UserState#verify_state.crls,
		Paths =  UserState#verify_state.crl_paths,
		Crls = [{DerCRL, public_key:der_decode('CertificateList',
						       DerCRL)} || DerCRL <- DerCRLs],

		CRLInfo0 = crl_info(OtpCert, Crls, []),
		CRLInfo = lists:reverse(CRLInfo0),
		PathDb = crl_path_db(lists:reverse(Crls), Paths, []),

		Fun = fun(DP, CRLtoValidate, Id, PathDb0) ->
			      trusted_cert_and_path(DP, CRLtoValidate, Id, PathDb0)
		      end,

		case CRLInfo of
		    [] ->
			{valid, UserState};
		    [_|_] ->
			case public_key:pkix_crls_validate(OtpCert, CRLInfo,
							   [{issuer_fun,{Fun, PathDb}}]) of
			    valid ->
				{valid, UserState};
			    Reason  ->
				{fail, Reason}
			end
		end
	end,

    [{verify_fun, {Fun, #verify_state{crls = CRLs,
				      crl_paths = Paths}}}].

crl_path_db([], [], Acc) ->
    Acc;
crl_path_db([{_, CRL} |CRLs], [Path | Paths], Acc) ->
    CertPath = lists:flatten(lists:map(fun([]) ->
					       [];
					  (CertFile) ->
					       ct:print("Certfile ~p", [CertFile]),
					       read_certs(CertFile)
				       end, Path)),
    crl_path_db(CRLs, Paths, [{CRL, CertPath}| Acc]).


crl_info(_, [], Acc) ->
    Acc;
crl_info(OtpCert, [{_, #'CertificateList'{tbsCertList =
						     #'TBSCertList'{issuer = Issuer,
								    crlExtensions = CRLExtensions}}}
				= CRL | Rest], Acc) ->
    OtpTBSCert =  OtpCert#'OTPCertificate'.tbsCertificate,
    Extensions = OtpTBSCert#'OTPTBSCertificate'.extensions,
    ExtList = pubkey_cert:extensions_list(CRLExtensions),
    DPs  = case pubkey_cert:select_extension(?'id-ce-cRLDistributionPoints', Extensions) of
	      #'Extension'{extnValue = Value} ->
		   lists:foldl(fun(Point, Acc0) ->
				       Dp = pubkey_cert_records:transform(Point, decode),
				       IDP = pubkey_cert:select_extension(?'id-ce-issuingDistributionPoint',
									  Extensions),
				       case Dp#'DistributionPoint'.cRLIssuer of
					   asn1_NOVALUE ->
					       [Dp | Acc0];
					   DpCRLIssuer ->
					       CRLIssuer = dp_crlissuer_to_issuer(DpCRLIssuer),
					       CertIssuer =  OtpTBSCert#'OTPTBSCertificate'.issuer,
					       case  pubkey_cert:is_issuer(CRLIssuer, CertIssuer) of
						   true ->
						       [Dp | Acc0];
						   false when (IDP =/= undefined) ->
						       Acc0;
						   false ->
						       [Dp | Acc0]
					       end
				       end
			       end, [], Value);
	       _ ->
		   case same_issuer(OtpCert, Issuer) of
		       true ->
			   [make_dp(ExtList, asn1_NOVALUE, Issuer)];
		       false ->
			   [make_dp(ExtList, Issuer, ignore)]
		   end
	   end,
    DPsCRLs = lists:map(fun(DP) -> {DP, CRL} end, DPs),
    crl_info(OtpCert, Rest, DPsCRLs ++ Acc).

same_issuer(OTPCert, Issuer) ->
    DecIssuer = pubkey_cert_records:transform(Issuer, decode),
    OTPTBSCert =  OTPCert#'OTPCertificate'.tbsCertificate,
    CertIssuer =  OTPTBSCert#'OTPTBSCertificate'.issuer,
    pubkey_cert:is_issuer(DecIssuer, CertIssuer).

make_dp(Extensions, Issuer0, DpInfo) ->
    {Issuer, Point} = mk_issuer_dp(Issuer0, DpInfo),
    case pubkey_cert:select_extension('id-ce-cRLReason', Extensions) of
	#'Extension'{extnValue = Reasons} ->
	    #'DistributionPoint'{cRLIssuer = Issuer,
				 reasons = Reasons,
				 distributionPoint = Point};
	_ ->
	    #'DistributionPoint'{cRLIssuer = Issuer,
				 reasons = [unspecified, keyCompromise,
					    cACompromise, affiliationChanged, superseded,
					    cessationOfOperation, certificateHold,
					    removeFromCRL, privilegeWithdrawn, aACompromise],
				 distributionPoint = Point}
    end.

mk_issuer_dp(asn1_NOVALUE, Issuer) ->
    {asn1_NOVALUE, {fullName, [{directoryName, Issuer}]}};
mk_issuer_dp(Issuer, _) ->
    {[{directoryName, Issuer}], asn1_NOVALUE}.

update_crls(_, State) ->
    State.

trusted_cert_and_path(_, #'CertificateList'{} = CRL, _, PathDb) ->
    [TrustedDERCert] =  read_certs(crl_root_cert()),
    TrustedCert =  public_key:pkix_decode_cert(TrustedDERCert, otp),

    case lists:keysearch(CRL, 1, PathDb) of
	{_, {CRL, [ _| _] = Path}} ->
		      {ok, TrustedCert, [TrustedDERCert | Path]};
	{_, {CRL, []}} ->
	    {ok, TrustedCert, [TrustedDERCert]}
    end.


dp_crlissuer_to_issuer(DPCRLIssuer) ->
    [{directoryName, Issuer}] = pubkey_cert_records:transform(DPCRLIssuer, decode),
    Issuer.

%%%%%%%%%%%%%%% CA mappings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cas(Chap) ->
    CAS = intermidiate_cas(Chap),
    lists:foldl(fun([], Acc) ->
			Acc;
		   (CA, Acc) -> 
			[CACert] = read_certs(CA),
			[CACert | Acc]
		end, [], CAS).
 
intermidiate_cas(Chap) when Chap == "4.1.1";
			    Chap == "4.1.3";
			    Chap == "4.2.2";
			    Chap == "4.2.3";
			    Chap == "4.2.4";
			    Chap == "4.2.6";
			    Chap == "4.2.7";
			    Chap == "4.2.8";
			    Chap == "4.3.1";
			    Chap == "4.3.3";
			    Chap == "4.3.4";
			    Chap == "4.3.5";
			    Chap == "4.4.3"
			    ->
    ["Good CA Cert"];

intermidiate_cas(Chap) when Chap == "4.1.2" ->
    ["Bad Signed CA Cert"];

intermidiate_cas(Chap) when Chap == "4.1.4";
			    Chap == "4.1.6" ->
    ["DSA CA Cert"];

intermidiate_cas(Chap) when Chap == "4.1.5" ->
    ["DSA Parameters Inherited CA Cert", "DSA CA Cert"];

intermidiate_cas(Chap) when Chap == "4.2.1";
			   Chap == "4.2.5" ->
    ["Bad notBefore Date CA Cert"];

intermidiate_cas(Chap) when  Chap == "4.16.1";
			    Chap == "4.16.2" ->
    ["Trust Anchor Root Certificate"];			    

intermidiate_cas(Chap) when Chap == "4.3.2" ->
    ["Name Ordering CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.34";
			    Chap == "4.13.35" ->
    ["nameConstraints URI1 CA Cert"];
intermidiate_cas(Chap) when Chap == "4.13.36";
			   Chap == "4.13.37" ->
    ["nameConstraints URI2 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.30";
			    Chap == "4.13.31";
			    Chap == "4.13.38"
			    ->
    ["nameConstraints DNS1 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.32";
			   Chap == "4.13.33" ->
    ["nameConstraints DNS2 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.27";
			    Chap == "4.13.28";
			    Chap == "4.13.29" ->
    ["nameConstraints DN1 subCA3 Cert", 
     "nameConstraints DN1 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.21";
			    Chap == "4.13.22" ->
    ["nameConstraints RFC822 CA1 Cert"];

intermidiate_cas(Chap) when Chap == "4.13.23";
			    Chap == "4.13.24" ->
    ["nameConstraints RFC822 CA2 Cert"];

intermidiate_cas(Chap) when Chap == "4.13.25";
			    Chap == "4.13.26" ->
    ["nameConstraints RFC822 CA3 Cert"];

intermidiate_cas(Chap) when Chap == "4.6.1" ->
    ["Missing basicConstraints CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.2" ->
    ["basicConstraints Critical cA False CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.3" ->
    ["basicConstraints Not Critical cA False CA Cert"];

intermidiate_cas(Chap) when Chap == "4.5.1";
			    Chap == "4.5.2" ->
    ["Basic Self-Issued New Key OldWithNew CA Cert", "Basic Self-Issued New Key CA Cert"];

intermidiate_cas(Chap) when	Chap == "4.5.3" ->
    ["Basic Self-Issued Old Key NewWithOld CA Cert", "Basic Self-Issued Old Key CA Cert"];

intermidiate_cas(Chap) when Chap == "4.5.4";
			    Chap == "4.5.5" ->
    ["Basic Self-Issued Old Key CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.1"; 
			    Chap == "4.13.2";
			    Chap == "4.13.3";
			    Chap == "4.13.4";
			    Chap == "4.13.20"
			    ->
    ["nameConstraints DN1 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.5" ->
    ["nameConstraints DN2 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.6";
			    Chap == "4.13.7" ->
    ["nameConstraints DN3 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.8";
			    Chap == "4.13.9" ->
    ["nameConstraints DN4 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.10";
			    Chap == "4.13.11" ->
    ["nameConstraints DN5 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.12" ->
    ["nameConstraints DN1 subCA1 Cert",
     "nameConstraints DN1 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.13";
			    Chap == "4.13.14" ->
    ["nameConstraints DN1 subCA2 Cert",
     "nameConstraints DN1 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.15";
			    Chap == "4.13.16" ->
    ["nameConstraints DN3 subCA1 Cert",
     "nameConstraints DN3 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.17";
			    Chap == "4.13.18" ->
    ["nameConstraints DN3 subCA2 Cert",
     "nameConstraints DN3 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.13.19" ->
    ["nameConstraints DN1 Self-Issued CA Cert",
     "nameConstraints DN1 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.7.1";
			    Chap == "4.7.4" -> 
    ["keyUsage Critical keyCertSign False CA Cert"];

intermidiate_cas(Chap) when Chap == "4.7.2";
			    Chap == "4.7.5" -> 
    ["keyUsage Not Critical keyCertSign False CA Cert"];

intermidiate_cas(Chap) when Chap == "4.7.3" -> 
    ["keyUsage Not Critical CA Cert"];

intermidiate_cas(Chap) when Chap == "4.3.7" -> 
    ["RFC3280 Mandatory Attribute Types CA Cert"];
intermidiate_cas(Chap) when Chap == "4.3.8" -> 
    ["RFC3280 Optional Attribute Types CA Cert"];

intermidiate_cas(Chap) when Chap == "4.3.6" -> 
    ["UIDCACert"];

intermidiate_cas(Chap) when Chap == "4.6.4" -> 
    ["basicConstraints Not Critical CA Cert"];

intermidiate_cas(Chap) when Chap == "4.1.26" -> 
    ["nameConstraints RFC822 CA3 Cert"];

intermidiate_cas(Chap) when Chap == "4.3.9" -> 
    ["UTF8String Encoded Names CA Cert"];

intermidiate_cas(Chap) when Chap == "4.3.10" -> 
    ["Rollover from PrintableString to UTF8String CA Cert"];

intermidiate_cas(Chap) when Chap == "4.3.11" -> 
    ["UTF8String Case Insensitive Match CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.7";
			    Chap == "4.6.8"
			    -> 
    ["pathLenConstraint0 CA Cert"];
intermidiate_cas(Chap) when Chap == "4.6.13" -> 
    [ "pathLenConstraint6 subsubsubCA41X Cert",
      "pathLenConstraint6 subsubCA41 Cert", 
      "pathLenConstraint6 subCA4 Cert", 
      "pathLenConstraint6 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.14" -> 
    [ "pathLenConstraint6 subsubsubCA41X Cert",
      "pathLenConstraint6 subsubCA41 Cert", 
      "pathLenConstraint6 subCA4 Cert", 
      "pathLenConstraint6 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.15" -> 
     [ "pathLenConstraint0 Self-Issued CA Cert",
       "pathLenConstraint0 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.17" -> 
    ["pathLenConstraint1 Self-Issued subCA Cert",
     "pathLenConstraint1 subCA Cert", 
     "pathLenConstraint1 Self-Issued CA Cert", 
     "pathLenConstraint1 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.5";
			    Chap == "4.6.6" -> 
    ["pathLenConstraint0 subCA Cert", 
     "pathLenConstraint0 CA Cert"]; 

intermidiate_cas(Chap) when Chap == "4.6.9";
			    Chap == "4.6.10" -> 
    ["pathLenConstraint6 subsubCA00 Cert",
     "pathLenConstraint6 subCA0 Cert",
     "pathLenConstraint6 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.11";
			    Chap == "4.6.12" -> 
    ["pathLenConstraint6 subsubsubCA11X Cert",
     "pathLenConstraint6 subsubCA11 Cert",
     "pathLenConstraint6 subCA1 Cert",
     "pathLenConstraint6 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.6.16" ->
    ["pathLenConstraint0 subCA2 Cert",
     "pathLenConstraint0 Self-Issued CA Cert",
     "pathLenConstraint0 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.1" ->
    ["No CRL CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.2" ->
    ["Revoked subCA Cert", "Good CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.3" ->
    ["Good CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.4" ->
    ["Bad CRL Signature CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.5" ->
    ["Bad CRL Issuer Name CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.6" ->
    ["Wrong CRL CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.7" ->
    ["Two CRLs CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.8" ->
    ["Unknown CRL Entry Extension CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.9";
			    Chap == "4.4.10" ->
    ["Unknown CRL Extension CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.11" ->
    ["Old CRL nextUpdate CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.12" ->
    ["pre2000 CRL nextUpdate CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.13" ->
    ["GeneralizedTime CRL nextUpdate CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.14";
			    Chap == "4.4.15" ->
    ["Negative Serial Number CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.16";
			    Chap == "4.4.17";
			    Chap == "4.4.18" ->
    ["Long Serial Number CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.19";
			    Chap == "4.4.20" ->
    ["Separate Certificate and CRL Keys Certificate Signing CA Cert"];

intermidiate_cas(Chap) when Chap == "4.4.21" ->
    ["Separate Certificate and CRL Keys CA2 Certificate Signing CA Cert"];

intermidiate_cas(Chap) when Chap == "4.14.1";
			    Chap == "4.14.2";
			    Chap == "4.14.3";
			    Chap == "4.14.4" ->
    ["distributionPoint1 CA Cert"];
intermidiate_cas(Chap) when Chap == "4.14.5";
			    Chap == "4.14.6";
			    Chap == "4.14.7";
			    Chap == "4.14.8";
			    Chap == "4.14.9" ->
    ["distributionPoint2 CA Cert"];

intermidiate_cas(Chap) when Chap == "4.14.10" ->
    ["No issuingDistributionPoint CA Cert"];

intermidiate_cas(Chap) when Chap == "4.14.11" ->
    ["onlyContainsUserCerts CA Cert"];

intermidiate_cas(Chap) when Chap == "4.14.12";
			    Chap == "4.14.13" ->
    ["onlyContainsCACerts CA Cert"];

intermidiate_cas(Chap) when Chap == "4.14.14" ->
    ["onlyContainsAttributeCerts CA Cert"];

intermidiate_cas(Chap) when Chap == "4.14.15";
			    Chap == "4.14.16" ->
    ["onlySomeReasons CA1 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.17" ->
    ["onlySomeReasons CA2 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.18" ->
    ["onlySomeReasons CA3 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.19";
			    Chap == "4.14.20";
			    Chap == "4.14.21" ->
    ["onlySomeReasons CA4 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.22";
			    Chap == "4.14.23" ->
    ["indirectCRL CA1 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.24";
			    Chap == "4.14.25";
			    Chap == "4.14.26" ->
    ["indirectCRL CA2 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.27" ->
    ["indirectCRL CA2 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.28";
			    Chap == "4.14.29" ->
    ["indirectCRL CA3 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.31";
			    Chap == "4.14.32";
			    Chap == "4.14.33" ->
    ["indirectCRL CA6 Cert"];

intermidiate_cas(Chap) when Chap == "4.14.34";
			    Chap == "4.14.35" ->
    ["indirectCRL CA5 Cert"];

intermidiate_cas(Chap) when Chap == "4.15.1" ->
    ["deltaCRLIndicator No Base CA Cert"];

intermidiate_cas(Chap) when Chap == "4.15.2";
			    Chap == "4.15.3";
			    Chap == "4.15.4";
			    Chap == "4.15.5";
			    Chap == "4.15.6";
			    Chap == "4.15.7" ->
    ["deltaCRL CA1 Cert"];

intermidiate_cas(Chap) when Chap == "4.15.8";
			    Chap == "4.15.9" ->
    ["deltaCRL CA2 Cert"];

intermidiate_cas(Chap) when Chap == "4.15.10" ->
    ["deltaCRL CA3 Cert"];

intermidiate_cas(Chap) when Chap == "4.5.6";
			    Chap == "4.5.7" ->
    ["Basic Self-Issued CRL Signing Key CA Cert"];
intermidiate_cas(Chap) when Chap == "4.5.8" ->
    ["Basic Self-Issued CRL Signing Key CRL Cert"].


%%%%%%%%%%%%%%% CRL mappings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crl_names("4.4.1") ->
    ["Trust Anchor Root CRL"];
crl_names("4.4.2") ->
    ["Trust Anchor Root CRL", "Good CA CRL", "Revoked subCA CRL"];
crl_names("4.4.3") ->
    ["Trust Anchor Root CRL", "Good CA CRL", "Revoked subCA CRL"];
crl_names("4.4.4") ->
     ["Trust Anchor Root CRL", "Bad CRL Signature CA CRL"];
crl_names("4.4.5") ->
      ["Trust Anchor Root CRL", "Bad CRL Issuer Name CA CRL"];
crl_names("4.4.6") ->
     ["Trust Anchor Root CRL", "Wrong CRL CA CRL"];
crl_names("4.4.7") ->
   ["Trust Anchor Root CRL", "Two CRLs CA Good CRL", "Two CRLs CA Bad CRL"];
crl_names("4.4.8") ->
   ["Trust Anchor Root CRL", "Unknown CRL Entry Extension CA CRL"];
crl_names(Chap) when Chap == "4.4.9";
		Chap == "4.4.10"->
   ["Trust Anchor Root CRL", "Unknown CRL Extension CA CRL"];
crl_names("4.4.11") ->
   ["Trust Anchor Root CRL", "Old CRL nextUpdate CA CRL"];
crl_names("4.4.12") ->
   ["Trust Anchor Root CRL", "pre2000 CRL nextUpdate CA CRL"];
crl_names("4.4.13") ->
   ["Trust Anchor Root CRL", "GeneralizedTime CRL nextUpdate CA CRL"];
crl_names(Chap) when Chap == "4.4.14";
		Chap == "4.4.15"->
    ["Trust Anchor Root CRL", "Negative Serial Number CA CRL"];
crl_names(Chap) when Chap == "4.4.16";
		Chap == "4.4.17";
		Chap == "4.4.18" ->
    ["Trust Anchor Root CRL", "Long Serial Number CA CRL"];
crl_names(Chap)when Chap == "4.4.19";
	       Chap == "4.4.20" ->
    ["Trust Anchor Root CRL", "Separate Certificate and CRL Keys CRL"];
crl_names("4.4.21") ->
   ["Trust Anchor Root CRL", "Separate Certificate and CRL Keys CA2 CRL"];
crl_names(Chap) when Chap == "4.5.1";
		     Chap == "4.5.2"->
   ["Trust Anchor Root CRL", "Basic Self-Issued New Key CA CRL"];
crl_names(Chap) when Chap == "4.5.3";
		Chap == "4.5.4";
		Chap == "4.5.5" ->
   ["Trust Anchor Root CRL", "Basic Self-Issued Old Key Self-Issued Cert CRL",
    "Basic Self-Issued Old Key CA CRL"];
crl_names(Chap) when  Chap == "4.5.6";
		      Chap == "4.5.7";
		      Chap == "4.5.8" ->
   ["Trust Anchor Root CRL", "Basic Self-Issued CRL Signing Key CRL Cert CRL",
	"Basic Self-Issued CRL Signing Key CA CRL"
   ];
crl_names("4.7.4") ->
   ["Trust Anchor Root CRL", "keyUsage Critical cRLSign False CA CRL"];
crl_names("4.7.5") ->
   ["Trust Anchor Root CRL", "keyUsage Not Critical cRLSign False CA CRL"];
crl_names(Chap) when Chap == "4.14.1";
		Chap == "4.14.2";
		Chap == "4.14.3";
		Chap == "4.14.4" ->
    ["Trust Anchor Root CRL", "distributionPoint1 CA CRL"];
crl_names(Chap) when Chap == "4.14.5";
		Chap == "4.14.6";
		Chap == "4.14.7";
		Chap == "4.14.8";
		Chap == "4.14.9" ->
    ["Trust Anchor Root CRL", "distributionPoint2 CA CRL"];
crl_names("4.14.10") ->
   ["Trust Anchor Root CRL", "No issuingDistributionPoint CA CRL"];
crl_names("4.14.11") ->
   ["Trust Anchor Root CRL", "onlyContainsUserCerts CA CRL"];
crl_names(Chap) when Chap == "4.14.12";
		Chap == "4.14.13" ->
    ["Trust Anchor Root CRL", "onlyContainsCACerts CA CRL"];
crl_names("4.14.14") ->
    ["Trust Anchor Root CRL", "onlyContainsAttributeCerts CA CRL"];
crl_names(Chap) when Chap == "4.14.15";
		Chap == "4.14.16" ->
   ["Trust Anchor Root CRL", "onlySomeReasons CA1 compromise CRL",
    "onlySomeReasons CA1 other reasons CRL"];
crl_names("4.14.17") ->
   ["Trust Anchor Root CRL",
    "onlySomeReasons CA2 CRL1", "onlySomeReasons CA2 CRL2"];
crl_names("4.14.18") ->
   ["Trust Anchor Root CRL",
    "onlySomeReasons CA3 compromise CRL", "onlySomeReasons CA3 other reasons CRL"];
crl_names(Chap) when Chap == "4.14.19";
		Chap == "4.14.20";
		Chap == "4.14.21" ->
   ["Trust Anchor Root CRL", "onlySomeReasons CA4 compromise CRL",
    "onlySomeReasons CA4 other reasons CRL"];
crl_names(Chap) when Chap == "4.14.22";
		Chap == "4.14.23";
		Chap == "4.14.24";
		Chap == "4.14.25";
		Chap == "4.14.26" ->
   ["Trust Anchor Root CRL", "indirectCRL CA1 CRL"];
crl_names("4.14.27") ->
   ["Trust Anchor Root CRL", "Good CA CRL"];

crl_names(Chap) when Chap == "4.14.28";
		     Chap == "4.14.29" ->
    ["Trust Anchor Root CRL", "indirectCRL CA3 CRL", "indirectCRL CA3 cRLIssuer CRL"];
crl_names("4.14.30") ->
   ["Trust Anchor Root CRL", "indirectCRL CA4 cRLIssuer CRL"];
crl_names(Chap) when Chap == "4.14.31";
		Chap == "4.14.32";
		Chap == "4.14.33";
		Chap == "4.14.34";
		Chap == "4.14.35" ->
   ["Trust Anchor Root CRL", "indirectCRL CA5 CRL"];
crl_names("4.15.1") ->
   ["Trust Anchor Root CRL", "deltaCRLIndicator No Base CA CRL"];
crl_names(Chap)  when Chap == "4.15.2";
		 Chap == "4.15.3";
		 Chap == "4.15.4";
		 Chap == "4.15.5";
		 Chap == "4.15.6";
		 Chap == "4.15.7" ->
    ["Trust Anchor Root CRL", "deltaCRL CA1 CRL", "deltaCRL CA1 deltaCRL"];
crl_names(Chap) when Chap == "4.15.8";
		Chap == "4.15.9" ->
   ["Trust Anchor Root CRL", "deltaCRL CA2 CRL", "deltaCRL CA2 deltaCRL"];
crl_names("4.15.10") ->
    ["Trust Anchor Root CRL", "deltaCRL CA3 CRL", "deltaCRL CA3 deltaCRL"].

crl_root_cert() ->
    "Trust Anchor Root Certificate".

crl_path("Trust Anchor Root CRL") ->
    []; %% Signed directly by crl_root_cert
crl_path("Revoked subCA CRL") ->
    ["Good CA Cert", "Revoked subCA Cert"];
crl_path("indirectCRL CA3 cRLIssuer CRL") ->
    ["indirectCRL CA3 Cert", "indirectCRL CA3 cRLIssuer Cert"];
crl_path("Two CRLs CA Good CRL") ->
    ["Two CRLs CA Cert"];
crl_path("Two CRLs CA Bad CRL") ->
    ["Two CRLs CA Cert"];
crl_path("Separate Certificate and CRL Keys CRL") ->
    ["Separate Certificate and CRL Keys CRL Signing Cert"];
crl_path("Separate Certificate and CRL Keys CA2 CRL") ->
    ["Separate Certificate and CRL Keys CA2 CRL Signing Cert"];
crl_path("Basic Self-Issued Old Key Self-Issued Cert CRL") ->
    ["Basic Self-Issued Old Key CA Cert"];
crl_path("Basic Self-Issued Old Key CA CRL") ->
      ["Basic Self-Issued Old Key CA Cert", "Basic Self-Issued Old Key NewWithOld CA Cert"];

crl_path("Basic Self-Issued CRL Signing Key CRL Cert CRL") ->
    ["Basic Self-Issued CRL Signing Key CA Cert"];
crl_path("Basic Self-Issued CRL Signing Key CA CRL") ->
    ["Basic Self-Issued CRL Signing Key CA Cert", "Basic Self-Issued CRL Signing Key CRL Cert"];

crl_path("onlySomeReasons CA1 compromise CRL") ->
    ["onlySomeReasons CA1 Cert"];
crl_path("onlySomeReasons CA1 other reasons CRL") ->
    ["onlySomeReasons CA1 Cert"];
crl_path("onlySomeReasons CA3 other reasons CRL") ->
    ["onlySomeReasons CA3 Cert"];
crl_path("onlySomeReasons CA3 compromise CRL") ->
    ["onlySomeReasons CA3 Cert"];
crl_path("onlySomeReasons CA4 compromise CRL") ->
    ["onlySomeReasons CA4 Cert"];
crl_path("onlySomeReasons CA4 other reasons CRL") ->
    ["onlySomeReasons CA4 Cert"];
crl_path("Basic Self-Issued New Key CA CRL") ->
    ["Basic Self-Issued New Key CA Cert"];
crl_path("deltaCRL CA1 deltaCRL") ->
    crl_path("deltaCRL CA2 CRL");
crl_path("deltaCRL CA2 deltaCRL") ->
    crl_path("deltaCRL CA2 CRL");
crl_path("deltaCRL CA3 deltaCRL") ->
    crl_path("deltaCRL CA3 CRL");
crl_path(CRL) when CRL == "onlySomeReasons CA2 CRL1";
		   CRL == "onlySomeReasons CA2 CRL2" ->
    ["onlySomeReasons CA2 Cert"];

crl_path(CRL) ->
    L = length(CRL),
    Base = string:sub_string(CRL, 1, L -3),
    [Base ++ "Cert"].

crls(CRLS) ->
     lists:foldl(fun([], Acc) ->
			Acc;
		   (CRLFile, Acc) ->
			[CRL] = read_crls(CRLFile),
			[CRL | Acc]
		end, [], CRLS).


%% TODO: If we implement policy support
%% Certificate policy tests need special handling. They can have several
%% sub tests and we need to check the outputs are correct.

certificate_policies_tests() ->
    %%{ "4.8", "Certificate Policies" },
    [{"4.8.1.1", "All Certificates Same Policy Test1", "-policy anyPolicy -explicit_policy", "True", ?NIST1, ?NIST1, 0},
     {"4.8.1.2", "All Certificates Same Policy Test1", "-policy ?NIST1BasicSelfIssuedCRLSigningKeyCACert.pem -explicit_policy", "True", ?NIST1, ?NIST1, 0},
     {"4.8.1.3", "All Certificates Same Policy Test1", "-policy ?NIST2 -explicit_policy", "True", ?NIST1, "<empty>", 43},
     {"4.8.1.4", "All Certificates Same Policy Test1", "-policy ?NIST1 -policy ?NIST2 -explicit_policy", "True", ?NIST1, ?NIST1, 0},
     {"4.8.2.1", "All Certificates No Policies Test2", "-policy anyPolicy", "False", "<empty>", "<empty>", 0},
     {"4.8.2.2", "All Certificates No Policies Test2", "-policy anyPolicy -explicit_policy", "True", "<empty>", "<empty>", 43},
     {"4.8.3.1", "Different Policies Test3", "-policy anyPolicy", "False", "<empty>", "<empty>", 0},
    {"4.8.3.2", "Different Policies Test3", "-policy anyPolicy -explicit_policy", "True", "<empty>", "<empty>", 43},
     {"4.8.3.3", "Different Policies Test3", "-policy ?NIST1 -policy ?NIST2 -explicit_policy", "True", "<empty>", "<empty>", 43},
     {"4.8.4", "Different Policies Test4", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.8.5", "Different Policies Test5", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.8.6.1", "Overlapping Policies Test6", "-policy anyPolicy", "True", ?NIST1, ?NIST1, 0},
     {"4.8.6.2", "Overlapping Policies Test6", "-policy ?NIST1", "True", ?NIST1, ?NIST1, 0},
     {"4.8.6.3", "Overlapping Policies Test6", "-policy ?NIST2", "True", ?NIST1, "<empty>", 43},
     {"4.8.7", "Different Policies Test7", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.8.8", "Different Policies Test8", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.8.9", "Different Policies Test9", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.8.10.1", "All Certificates Same Policies Test10", "-policy ?NIST1", "True", "?NIST1:?NIST2", "?NIST1", 0},
     {"4.8.10.2", "All Certificates Same Policies Test10", "-policy ?NIST2", "True", "?NIST1:?NIST2", "?NIST2", 0},
     {"4.8.10.3", "All Certificates Same Policies Test10", "-policy anyPolicy", "True", "?NIST1:?NIST2", "?NIST1:?NIST2", 0},
     {"4.8.11.1", "All Certificates AnyPolicy Test11", "-policy anyPolicy", "True", "$apolicy", "$apolicy", 0},
    {"4.8.11.2", "All Certificates AnyPolicy Test11", "-policy ?NIST1", "True", "$apolicy", "?NIST1", 0},
     {"4.8.12", "Different Policies Test12", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.8.13.1", "All Certificates Same Policies Test13", "-policy ?NIST1", "True", "?NIST1:?NIST2:?NIST3", "?NIST1", 0},
     {"4.8.13.2", "All Certificates Same Policies Test13", "-policy ?NIST2", "True", "?NIST1:?NIST2:?NIST3", "?NIST2", 0},
     {"4.8.13.3", "All Certificates Same Policies Test13", "-policy ?NIST3", "True", "?NIST1:?NIST2:?NIST3", "?NIST3", 0},
     {"4.8.14.1",       "AnyPolicy Test14", "-policy ?NIST1", "True", "?NIST1",         "?NIST1", 0},
     {"4.8.14.2",       "AnyPolicy Test14", "-policy ?NIST2", "True", "?NIST1",         "<empty>", 43},
     {"4.8.15", "User Notice Qualifier Test15", "-policy anyPolicy", "False", "?NIST1", "?NIST1", 0},
     {"4.8.16", "User Notice Qualifier Test16", "-policy anyPolicy", "False", "?NIST1", "?NIST1", 0},
    {"4.8.17", "User Notice Qualifier Test17", "-policy anyPolicy", "False", "?NIST1", "?NIST1", 0},
     {"4.8.18.1", "User Notice Qualifier Test18", "-policy ?NIST1", "True", "?NIST1:?NIST2", "?NIST1", 0},
     {"4.8.18.2", "User Notice Qualifier Test18", "-policy ?NIST2", "True", "?NIST1:?NIST2", "?NIST2", 0},
     {"4.8.19", "User Notice Qualifier Test19", "-policy anyPolicy", "False", "?NIST1", "?NIST1", 0},
     {"4.8.20", "CPS Pointer Qualifier Test20", "-policy anyPolicy -explicit_policy", "True", "?NIST1", "?NIST1", 0}].
require_explicit_policy_tests() ->
    %%{ "4.9", "Require Explicit Policy" },
    [{"4.9.1", "Valid RequireExplicitPolicy Test1", "-policy anyPolicy", "False", "<empty>", "<empty>", 0},
     {"4.9.2", "Valid RequireExplicitPolicy Test2", "-policy anyPolicy", "False", "<empty>", "<empty>", 0},
     {"4.9.3", "Invalid RequireExplicitPolicy Test3", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.9.4", "Valid RequireExplicitPolicy Test4", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},
     {"4.9.5", "Invalid RequireExplicitPolicy Test5", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.9.6", "Valid Self-Issued requireExplicitPolicy Test6", "-policy anyPolicy", "False", "<empty>", "<empty>", 0},
     {"4.9.7", "Invalid Self-Issued requireExplicitPolicy Test7", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.9.8", "Invalid Self-Issued requireExplicitPolicy Test8", "-policy anyPolicy", "True", "<empty>", "<empty>", 43}].
policy_mappings_tests() ->
    %%{ "4.10", "Policy Mappings" },
    [{"4.10.1.1", "Valid Policy Mapping Test1", "-policy ?NIST1", "True", "?NIST1", "?NIST1", 0},
     {"4.10.1.2", "Valid Policy Mapping Test1", "-policy ?NIST2", "True", "?NIST1", "<empty>", 43},
     {"4.10.1.3", "Valid Policy Mapping Test1", "-policy anyPolicy -inhibit_map", "True", "<empty>", "<empty>", 43},
     {"4.10.2.1", "Invalid Policy Mapping Test2", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.10.2.2", "Invalid Policy Mapping Test2", "-policy anyPolicy -inhibit_map", "True", "<empty>", "<empty>", 43},
     {"4.10.3.1", "Valid Policy Mapping Test3", "-policy ?NIST1", "True", "?NIST2", "<empty>", 43},
     {"4.10.3.2", "Valid Policy Mapping Test3", "-policy ?NIST2", "True", "?NIST2", "?NIST2", 0},
     {"4.10.4", "Invalid Policy Mapping Test4", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.10.5.1", "Valid Policy Mapping Test5", "-policy ?NIST1", "True", "?NIST1", "?NIST1", 0},
     {"4.10.5.2", "Valid Policy Mapping Test5", "-policy ?NIST6", "True", "?NIST1", "<empty>", 43},
     {"4.10.6.1", "Valid Policy Mapping Test6", "-policy ?NIST1", "True", "?NIST1", "?NIST1", 0},
     {"4.10.6.2", "Valid Policy Mapping Test6", "-policy ?NIST6", "True", "?NIST1", "<empty>", 43},
     { "4.10.7", "Invalid Mapping From anyPolicy Test7", 42 },
     { "4.10.8", "Invalid Mapping To anyPolicy Test8",   42 },
     {"4.10.9", "Valid Policy Mapping Test9", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},
     {"4.10.10", "Invalid Policy Mapping Test10", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.10.11", "Valid Policy Mapping Test11", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},

     %% TODO: check notice display
     {"4.10.12.1", "Valid Policy Mapping Test12", "-policy ?NIST1", "True", "?NIST1:?NIST2", "?NIST1", 0},

     %% TODO: check notice display
     {"4.10.12.2", "Valid Policy Mapping Test12", "-policy ?NIST2", "True", "?NIST1:?NIST2", "?NIST2", 0},
     {"4.10.13", "Valid Policy Mapping Test13", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},

     %% TODO: check notice display
     {"4.10.14", "Valid Policy Mapping Test14", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0}].

inhibit_policy_mapping_tests() ->
    %%{ "4.11", "Inhibit Policy Mapping" },
    [{"4.11.1", "Invalid inhibitPolicyMapping Test1", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.11.2", "Valid inhibitPolicyMapping Test2", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},
     {"4.11.3", "Invalid inhibitPolicyMapping Test3", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.11.4", "Valid inhibitPolicyMapping Test4", "-policy anyPolicy", "True", "?NIST2", "?NIST2", 0},
     {"4.11.5", "Invalid inhibitPolicyMapping Test5", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.11.6", "Invalid inhibitPolicyMapping Test6", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.11.7", "Valid Self-Issued inhibitPolicyMapping Test7", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},
     {"4.11.8", "Invalid Self-Issued inhibitPolicyMapping Test8", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.11.9", "Invalid Self-Issued inhibitPolicyMapping Test9", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.11.10", "Invalid Self-Issued inhibitPolicyMapping Test10", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.11.11", "Invalid Self-Issued inhibitPolicyMapping Test11", "-policy anyPolicy", "True", "<empty>", "<empty>", 43}].
inhibit_any_policy_tests() ->
    %%{ "4.12", "Inhibit Any Policy" },
    [{"4.12.1", "Invalid inhibitAnyPolicy Test1", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.12.2", "Valid inhibitAnyPolicy Test2", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},
     {"4.12.3.1", "inhibitAnyPolicy Test3", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},
     {"4.12.3.2", "inhibitAnyPolicy Test3", "-policy anyPolicy -inhibit_any", "True", "<empty>", "<empty>", 43},
     {"4.12.4", "Invalid inhibitAnyPolicy Test4", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.12.5", "Invalid inhibitAnyPolicy Test5", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.12.6", "Invalid inhibitAnyPolicy Test6", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.12.7",  "Valid Self-Issued inhibitAnyPolicy Test7",      ok},
     {"4.12.8",  "Invalid Self-Issued inhibitAnyPolicy Test8",    43 },
     {"4.12.9",  "Valid Self-Issued inhibitAnyPolicy Test9",      ok},
     {"4.12.10", "Invalid Self-Issued inhibitAnyPolicy Test10",   43 }].
