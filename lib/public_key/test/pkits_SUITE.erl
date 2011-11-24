%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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


%%  Se specification here:
%%  http://csrc.nist.gov/groups/ST/crypto_apps_infra/pki/pkitesting.html

-module(pkits_SUITE).

-compile(export_all).

-include_lib("public_key/include/public_key.hrl").

-define(error(Format,Args), error(Format,Args,?FILE,?LINE)).
-define(warning(Format,Args), warning(Format,Args,?FILE,?LINE)).

-define(CERTS, "pkits/certs").
-define(MIME,  "pkits/smime").
-define(CONV,  "pkits/smime-pem").

-define(NIST1, "2.16.840.1.101.3.2.1.48.1").
-define(NIST2, "2.16.840.1.101.3.2.1.48.2").
-define(NIST3, "2.16.840.1.101.3.2.1.48.3").
-define(NIST4, "2.16.840.1.101.3.2.1.48.4").
-define(NIST5, "2.16.840.1.101.3.2.1.48.5").
-define(NIST6, "2.16.840.1.101.3.2.1.48.6").

-record(verify_state, {
	  certs_db,
	  crl_info,
	  revoke_state}).
%%
suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, signature_verification},
     {group, validity_periods},
     {group, verifying_name_chaining},
     {group, verifying_paths_with_self_issued_certificates},
     %%{group, basic_certificate_revocation_tests},
     %%{group, delta_crls},
     %%{group, distribution_points},
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
     %% {basic_certificate_revocation_tests, [],
     %%  [missing_CRL, revoked_CA, revoked_peer, invalid_CRL_signature,
     %%   invalid_CRL_issuer, invalid_CRL, valid_CRL,
     %%   unknown_CRL_extension, old_CRL, fresh_CRL, valid_serial,
     %%   invalid_serial, valid_seperate_keys, invalid_separate_keys]},
     %% {delta_crls, [], [delta_without_crl, valid_delta_crls, invalid_delta_crls]},
     %% {distribution_points, [], [valid_distribution_points,
     %% 				valid_distribution_points_no_issuing_distribution_point,
     %% 				invalid_distribution_points, valid_only_contains,
     %% 				invalid_only_contains, valid_only_some_reasons,
     %% 				invalid_only_some_reasons, valid_indirect_crl,
     %% 				invalid_indirect_crl, valid_crl_issuer, invalid_crl_issuer]},
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

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Func, Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    put(datadir, Datadir),
    Config.

end_per_testcase(_Func, Config) ->
    Config.

init_per_suite(Config) ->
    {skip, "PKIX Conformance test certificates expired 14 of April 2011,"
     " new conformance test suite uses new format so skip until PKCS-12 support is implemented"}.
    %% try crypto:start() of
    %% 	ok ->
    %% 	    Config
    %% catch _:_ ->
    %% 	    {skip, "Crypto did not start"}
    %% end.

end_per_suite(_Config) ->
    application:stop(crypto).

%%-----------------------------------------------------------------------------
valid_rsa_signature(doc) ->
    ["Test rsa signatur verification"];
valid_rsa_signature(suite) ->
    [];
valid_rsa_signature(Config) when is_list(Config) ->
    run([{ "4.1.1", "Valid Signatures Test1", ok}]).

invalid_rsa_signature(doc) ->
    ["Test rsa signatur verification"];
invalid_rsa_signature(suite) ->
    [];
invalid_rsa_signature(Config) when is_list(Config) ->
    run([{ "4.1.2", "Invalid CA Signature Test2", {bad_cert,invalid_signature}},
	 { "4.1.3", "Invalid EE Signature Test3", {bad_cert,invalid_signature}}]).

valid_dsa_signature(doc) ->
    ["Test dsa signatur verification"];
valid_dsa_signature(suite) ->
    [];
valid_dsa_signature(Config) when is_list(Config) ->
    run([{ "4.1.4", "Valid DSA Signatures Test4", ok},
	 { "4.1.5", "Valid DSA Parameter Inheritance Test5", ok}]).

invalid_dsa_signature(doc) ->
    ["Test dsa signatur verification"];
invalid_dsa_signature(suite) ->
    [];
invalid_dsa_signature(Config) when is_list(Config) ->
    run([{ "4.1.6", "Invalid DSA Signature Test6",{bad_cert,invalid_signature}}]).
%%-----------------------------------------------------------------------------
not_before_invalid(doc) ->
    [""];
not_before_invalid(suite) ->
    [];
not_before_invalid(Config) when is_list(Config) ->
    run([{ "4.2.1", "Invalid CA notBefore Date Test1",{bad_cert, cert_expired}},
	 { "4.2.2", "Invalid EE notBefore Date Test2",{bad_cert, cert_expired}}]).

not_before_valid(doc) ->
    [""];
not_before_valid(suite) ->
    [];
not_before_valid(Config) when is_list(Config) ->
    run([{ "4.2.3", "Valid pre2000 UTC notBefore Date Test3", ok},
	 { "4.2.4", "Valid GeneralizedTime notBefore Date Test4", ok}]).

not_after_invalid(doc) ->
    [""];
not_after_invalid(suite) ->
    [];
not_after_invalid(Config) when is_list(Config) ->
    run([{ "4.2.5", "Invalid CA notAfter Date Test5", {bad_cert, cert_expired}},
	 { "4.2.6", "Invalid EE notAfter Date Test6", {bad_cert, cert_expired}},
	 { "4.2.7", "Invalid pre2000 UTC EE notAfter Date Test7",{bad_cert, cert_expired}}]).

not_after_valid(doc) ->
    [""];
not_after_valid(suite) ->
    [];
not_after_valid(Config) when is_list(Config) ->
    run([{ "4.2.8", "Valid GeneralizedTime notAfter Date Test8", ok}]).
%%-----------------------------------------------------------------------------
invalid_name_chain(doc) ->
    [""];
invalid_name_chain(suite) ->
    [];
invalid_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.1", "Invalid Name Chaining EE Test1", {bad_cert, invalid_issuer}},
	 { "4.3.2", "Invalid Name Chaining Order Test2", {bad_cert, invalid_issuer}}]).

whitespace_name_chain(doc) ->
    [""];
whitespace_name_chain(suite) ->
    [];
whitespace_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.3", "Valid Name Chaining Whitespace Test3", ok},
	 { "4.3.4", "Valid Name Chaining Whitespace Test4", ok}]).

capitalization_name_chain(doc) ->
    [""];
capitalization_name_chain(suite) ->
    [];
capitalization_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.5", "Valid Name Chaining Capitalization Test5",ok}]).

uid_name_chain(doc) ->
    [""];
uid_name_chain(suite) ->
    [];
uid_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.6", "Valid Name Chaining UIDs Test6",ok}]).

attrib_name_chain(doc) ->
    [""];
attrib_name_chain(suite) ->
    [];
attrib_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.7", "Valid RFC3280 Mandatory Attribute Types Test7", ok},
	 { "4.3.8", "Valid RFC3280 Optional Attribute Types Test8",  ok}]).

string_name_chain(doc) ->
    [""];
string_name_chain(suite) ->
    [];
string_name_chain(Config) when is_list(Config) ->
    run([{ "4.3.9", "Valid UTF8String Encoded Names Test9", ok},
	 { "4.3.10", "Valid Rollover from PrintableString to UTF8String Test10", ok},
	 { "4.3.11", "Valid UTF8String Case Insensitive Match Test11", ok}]).

%%-----------------------------------------------------------------------------

basic_valid(doc) ->
    [""];
basic_valid(suite) ->
    [];
basic_valid(Config) when is_list(Config) ->
    run([{ "4.5.1",  "Valid Basic Self-Issued Old With New Test1", ok},
	 { "4.5.3",  "Valid Basic Self-Issued New With Old Test3", ok},
	 { "4.5.4",  "Valid Basic Self-Issued New With Old Test4", ok}
	]).

basic_invalid(doc) ->
    [""];
basic_invalid(suite) ->
    [];
basic_invalid(Config) when is_list(Config) ->
    run([{"4.5.2",  "Invalid Basic Self-Issued Old With New Test2",
	  {bad_cert, {revoked, keyCompromise}}},
	 {"4.5.5",  "Invalid Basic Self-Issued New With Old Test5",
	  {bad_cert, {revoked, keyCompromise}}}
	]).

crl_signing_valid(doc) ->
    [""];
crl_signing_valid(suite) ->
    [];
crl_signing_valid(Config) when is_list(Config) ->
    run([{ "4.5.6",  "Valid Basic Self-Issued CRL Signing Key Test6", ok}]).

crl_signing_invalid(doc) ->
    [""];
crl_signing_invalid(suite) ->
    [];
crl_signing_invalid(Config) when is_list(Config) ->
    run([{ "4.5.7",  "Invalid Basic Self-Issued CRL Signing Key Test7",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.5.8",  "Invalid Basic Self-Issued CRL Signing Key Test8",
	   {bad_cert, invalid_key_usage}}
	]).

%%-----------------------------------------------------------------------------
missing_CRL(doc) ->
    [""];
missing_CRL(suite) ->
    [];
missing_CRL(Config) when is_list(Config) ->
    run([{ "4.4.1", "Missing CRL Test1",{bad_cert,
    revocation_status_undetermined}}]).

revoked_CA(doc) ->
    [""];
revoked_CA(suite) ->
    [];
revoked_CA(Config) when is_list(Config) ->
    run([{ "4.4.2", "Invalid Revoked CA Test2", {bad_cert,
    {revoked, keyCompromise}}}]).

revoked_peer(doc) ->
    [""];
revoked_peer(suite) ->
    [];
revoked_peer(Config) when is_list(Config) ->
    run([{ "4.4.3", "Invalid Revoked EE Test3", {bad_cert,
    {revoked, keyCompromise}}}]).

invalid_CRL_signature(doc) ->
    [""];
invalid_CRL_signature(suite) ->
    [];
invalid_CRL_signature(Config) when is_list(Config) ->
    run([{ "4.4.4", "Invalid Bad CRL Signature Test4",
	   {bad_cert, revocation_status_undetermined}}]).

invalid_CRL_issuer(doc) ->
    [""];
invalid_CRL_issuer(suite) ->
    [];
invalid_CRL_issuer(Config) when is_list(Config) ->
    run({ "4.4.5", "Invalid Bad CRL Issuer Name Test5",
	  {bad_cert, revocation_status_undetermined}}).

invalid_CRL(doc) ->
    [""];
invalid_CRL(suite) ->
    [];
invalid_CRL(Config) when is_list(Config) ->
    run([{ "4.4.6", "Invalid Wrong CRL Test6",
	   {bad_cert, revocation_status_undetermined}}]).

valid_CRL(doc) ->
    [""];
valid_CRL(suite) ->
    [];
valid_CRL(Config) when is_list(Config) ->
    run([{ "4.4.7", "Valid Two CRLs Test7", ok}]).

unknown_CRL_extension(doc) ->
    [""];
unknown_CRL_extension(suite) ->
    [];
unknown_CRL_extension(Config) when is_list(Config) ->
    run([{ "4.4.8", "Invalid Unknown CRL Entry Extension Test8",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.4.9", "Invalid Unknown CRL Extension Test9",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.4.10", "Invalid Unknown CRL Extension Test10",
	   {bad_cert, revocation_status_undetermined}}]).

old_CRL(doc) ->
    [""];
old_CRL(suite) ->
    [];
old_CRL(Config) when is_list(Config) ->
    run([{ "4.4.11", "Invalid Old CRL nextUpdate Test11",
	   {bad_cert, revocation_status_undetermined}},
	 { "4.4.12", "Invalid pre2000 CRL nextUpdate Test12",
	   {bad_cert, revocation_status_undetermined}}]).

fresh_CRL(doc) ->
    [""];
fresh_CRL(suite) ->
    [];
fresh_CRL(Config) when is_list(Config) ->
    run([{ "4.4.13", "Valid GeneralizedTime CRL nextUpdate Test13", ok}]).

valid_serial(doc) ->
    [""];
valid_serial(suite) ->
    [];
valid_serial(Config) when is_list(Config) ->
    run([
	 { "4.4.14", "Valid Negative Serial Number Test14",ok},
	 { "4.4.16", "Valid Long Serial Number Test16", ok},
	 { "4.4.17", "Valid Long Serial Number Test17", ok}
	]).

invalid_serial(doc) ->
    [""];
invalid_serial(suite) ->
    [];
invalid_serial(Config) when is_list(Config) ->
    run([{ "4.4.15", "Invalid Negative Serial Number Test15",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.4.18", "Invalid Long Serial Number Test18",
	   {bad_cert, {revoked, keyCompromise}}}]).

valid_seperate_keys(doc) ->
    [""];
valid_seperate_keys(suite) ->
    [];
valid_seperate_keys(Config) when is_list(Config) ->
    run([{ "4.4.19", "Valid Separate Certificate and CRL Keys Test19",   ok}]).

invalid_separate_keys(doc) ->
    [""];
invalid_separate_keys(suite) ->
    [];
invalid_separate_keys(Config) when is_list(Config) ->
    run([{ "4.4.20", "Invalid Separate Certificate and CRL Keys Test20",
	   {bad_cert, {revoked, keyCompromise}}},
	 { "4.4.21", "Invalid Separate Certificate and CRL Keys Test21",
	   {bad_cert, revocation_status_undetermined}}
	]).
%%-----------------------------------------------------------------------------
missing_basic_constraints(doc) ->
    [""];
missing_basic_constraints(suite) ->
    [];
missing_basic_constraints(Config) when is_list(Config) ->
    run([{ "4.6.1",  "Invalid Missing basicConstraints Test1",
	   {bad_cert, missing_basic_constraint}},
	 { "4.6.2",  "Invalid cA False Test2",
	   {bad_cert, missing_basic_constraint}},
	 { "4.6.3",  "Invalid cA False Test3",
	   {bad_cert, missing_basic_constraint}}]).

valid_basic_constraint(doc) ->
    [""];
valid_basic_constraint(suite) ->
    [];
valid_basic_constraint(Config) when is_list(Config) ->
    run([{"4.6.4", "Valid basicConstraints Not Critical Test4", ok}]).

invalid_path_constraints(doc) ->
    [""];
invalid_path_constraints(suite) ->
    [];
invalid_path_constraints(Config) when is_list(Config) ->
    run([{ "4.6.5", "Invalid pathLenConstraint Test5", {bad_cert, max_path_length_reached}},
	 { "4.6.6", "Invalid pathLenConstraint Test6", {bad_cert, max_path_length_reached}},
	 { "4.6.9", "Invalid pathLenConstraint Test9", {bad_cert, max_path_length_reached}},
	 { "4.6.10", "Invalid pathLenConstraint Test10", {bad_cert, max_path_length_reached}},
	 { "4.6.11", "Invalid pathLenConstraint Test11", {bad_cert, max_path_length_reached}},
	 { "4.6.12", "Invalid pathLenConstraint Test12", {bad_cert, max_path_length_reached}},
	 { "4.6.16", "Invalid Self-Issued pathLenConstraint Test16",
	   {bad_cert, max_path_length_reached}}]).

valid_path_constraints(doc) ->
    [""];
valid_path_constraints(suite) ->
    [];
valid_path_constraints(Config) when is_list(Config) ->
    run([{ "4.6.7",  "Valid pathLenConstraint Test7", ok},
	 { "4.6.8",  "Valid pathLenConstraint Test8", ok},
	 { "4.6.13", "Valid pathLenConstraint Test13", ok},
	 { "4.6.14", "Valid pathLenConstraint Test14", ok},
	 { "4.6.15", "Valid Self-Issued pathLenConstraint Test15", ok},
	 { "4.6.17", "Valid Self-Issued pathLenConstraint Test17", ok}]).

%%-----------------------------------------------------------------------------
invalid_key_usage(doc) ->
    [""];
invalid_key_usage(suite) ->
    [];
invalid_key_usage(Config) when is_list(Config) ->
    run([{ "4.7.1",  "Invalid keyUsage Critical keyCertSign False Test1",
	   {bad_cert,invalid_key_usage} },
	 { "4.7.2",  "Invalid keyUsage Not Critical keyCertSign False Test2",
	   {bad_cert,invalid_key_usage}},
	{ "4.7.4",  "Invalid keyUsage Critical cRLSign False Test4",
	  {bad_cert, revocation_status_undetermined}},
	 { "4.7.5",  "Invalid keyUsage Not Critical cRLSign False Test5",
	   {bad_cert, revocation_status_undetermined}}
	]).

valid_key_usage(doc) ->
    [""];
valid_key_usage(suite) ->
    [];
valid_key_usage(Config) when is_list(Config) ->
    run([{ "4.7.3",  "Valid keyUsage Not Critical Test3", ok}]).

%%-----------------------------------------------------------------------------
certificate_policies(doc) ->    [""];
certificate_policies(suite) -> [];
certificate_policies(Config) when is_list(Config) ->
    run(certificate_policies()).
%%-----------------------------------------------------------------------------
require_explicit_policy(doc) ->    [""];
require_explicit_policy(suite) -> [];
require_explicit_policy(Config) when is_list(Config) ->
    run(require_explicit_policy()).
%%-----------------------------------------------------------------------------
policy_mappings(doc) ->     [""];
policy_mappings(suite) -> [];
policy_mappings(Config) when is_list(Config) ->
    run(policy_mappings()).
%%-----------------------------------------------------------------------------
inhibit_policy_mapping(doc) ->    [""];
inhibit_policy_mapping(suite) -> [];
inhibit_policy_mapping(Config) when is_list(Config) ->
    run(inhibit_policy_mapping()).
%%-----------------------------------------------------------------------------
inhibit_any_policy(doc) ->    [""];
inhibit_any_policy(suite) -> [];
inhibit_any_policy(Config) when is_list(Config) ->
    run(inhibit_any_policy()).
%%-----------------------------------------------------------------------------

valid_DN_name_constraints(doc) ->
    [""];
valid_DN_name_constraints(suite) ->
    [];
valid_DN_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.1",  "Valid DN nameConstraints Test1", ok},
	 { "4.13.4",  "Valid DN nameConstraints Test4", ok},
	 { "4.13.5",  "Valid DN nameConstraints Test5", ok},
	 { "4.13.6",  "Valid DN nameConstraints Test6", ok},
	 { "4.13.11", "Valid DN nameConstraints Test11", ok},
	 { "4.13.14", "Valid DN nameConstraints Test14", ok},
	 { "4.13.18", "Valid DN nameConstraints Test18", ok},
	 { "4.13.19", "Valid Self-Issued DN nameConstraints Test19", ok}]).

invalid_DN_name_constraints(doc) ->
    [""];
invalid_DN_name_constraints(suite) ->
    [];
invalid_DN_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.2", "Invalid DN nameConstraints Test2", {bad_cert, name_not_permitted}},
	 { "4.13.3",  "Invalid DN nameConstraints Test3", {bad_cert, name_not_permitted}},
	 { "4.13.7",  "Invalid DN nameConstraints Test7", {bad_cert, name_not_permitted}},
	 { "4.13.8",  "Invalid DN nameConstraints Test8", {bad_cert, name_not_permitted}},
	 { "4.13.9",  "Invalid DN nameConstraints Test9", {bad_cert, name_not_permitted}},
	 { "4.13.10", "Invalid DN nameConstraints Test10",{bad_cert, name_not_permitted}},
	 { "4.13.12", "Invalid DN nameConstraints Test12",{bad_cert, name_not_permitted}},
	 { "4.13.13", "Invalid DN nameConstraints Test13",{bad_cert, name_not_permitted}},
	 { "4.13.15", "Invalid DN nameConstraints Test15",{bad_cert, name_not_permitted}},
	 { "4.13.16", "Invalid DN nameConstraints Test16",{bad_cert, name_not_permitted}},
	 { "4.13.17", "Invalid DN nameConstraints Test17",{bad_cert, name_not_permitted}},
	 { "4.13.20", "Invalid Self-Issued DN nameConstraints Test20",
	   {bad_cert, name_not_permitted}}]).

valid_rfc822_name_constraints(doc) ->
    [""];
valid_rfc822_name_constraints(suite) ->
    [];
valid_rfc822_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.21", "Valid RFC822 nameConstraints Test21", ok},
	 { "4.13.23", "Valid RFC822 nameConstraints Test23", ok},
	 { "4.13.25", "Valid RFC822 nameConstraints Test25", ok}]).


invalid_rfc822_name_constraints(doc) ->
    [""];
invalid_rfc822_name_constraints(suite) ->
    [];
invalid_rfc822_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.22", "Invalid RFC822 nameConstraints Test22",
	   {bad_cert, name_not_permitted}},
	 { "4.13.24", "Invalid RFC822 nameConstraints Test24",
	   {bad_cert, name_not_permitted}},
	 { "4.13.26", "Invalid RFC822 nameConstraints Test26",
	   {bad_cert, name_not_permitted}}]).

valid_DN_and_rfc822_name_constraints(doc) ->
    [""];
valid_DN_and_rfc822_name_constraints(suite) ->
    [];
valid_DN_and_rfc822_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.27", "Valid DN and RFC822 nameConstraints Test27", ok}]).

invalid_DN_and_rfc822_name_constraints(doc) ->
    [""];
invalid_DN_and_rfc822_name_constraints(suite) ->
    [];
invalid_DN_and_rfc822_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.28", "Invalid DN and RFC822 nameConstraints Test28",
	   {bad_cert, name_not_permitted}},
	 { "4.13.29", "Invalid DN and RFC822 nameConstraints Test29",
	   {bad_cert, name_not_permitted}}]).

valid_dns_name_constraints(doc) ->
    [""];
valid_dns_name_constraints(suite) ->
    [];
valid_dns_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.30", "Valid DNS nameConstraints Test30", ok},
	 { "4.13.32", "Valid DNS nameConstraints Test32", ok}]).

invalid_dns_name_constraints(doc) ->
    [""];
invalid_dns_name_constraints(suite) ->
    [];
invalid_dns_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.31", "Invalid DNS nameConstraints Test31", {bad_cert, name_not_permitted}},
	 { "4.13.33", "Invalid DNS nameConstraints Test33", {bad_cert, name_not_permitted}},
	 { "4.13.38", "Invalid DNS nameConstraints Test38", {bad_cert, name_not_permitted}}]).

valid_uri_name_constraints(doc) ->
    [""];
valid_uri_name_constraints(suite) ->
    [];
valid_uri_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.34", "Valid URI nameConstraints Test34", ok},
	 { "4.13.36", "Valid URI nameConstraints Test36", ok}]).

invalid_uri_name_constraints(doc) ->
    [""];
invalid_uri_name_constraints(suite) ->
    [];
invalid_uri_name_constraints(Config) when is_list(Config) ->
    run([{ "4.13.35", "Invalid URI nameConstraints Test35",{bad_cert, name_not_permitted}},
	 { "4.13.37", "Invalid URI nameConstraints Test37",{bad_cert, name_not_permitted}}]).

%%-----------------------------------------------------------------------------
delta_without_crl(doc) ->
    [""];
delta_without_crl(suite) ->
    [];
delta_without_crl(Config) when is_list(Config) ->
  run([{ "4.15.1",  "Invalid deltaCRLIndicator No Base Test1",{bad_cert,
							       revocation_status_undetermined}},
       {"4.15.10", "Invalid delta-CRL Test10", {bad_cert,
						revocation_status_undetermined}}]).

valid_delta_crls(doc) ->
    [""];
valid_delta_crls(suite) ->
    [];
valid_delta_crls(Config) when is_list(Config) ->
    run([{ "4.15.2",  "Valid delta-CRL Test2", ok},
	 { "4.15.5",  "Valid delta-CRL Test5", ok},
	 { "4.15.7",  "Valid delta-CRL Test7", ok},
	 { "4.15.8",  "Valid delta-CRL Test8", ok}
	]).

invalid_delta_crls(doc) ->
    [""];
invalid_delta_crls(suite) ->
    [];
invalid_delta_crls(Config) when is_list(Config) ->
    run([{ "4.15.3",  "Invalid delta-CRL Test3", {bad_cert,{revoked, keyCompromise}}},
	 { "4.15.4",  "Invalid delta-CRL Test4", {bad_cert,{revoked, keyCompromise}}},
	 { "4.15.6",  "Invalid delta-CRL Test6", {bad_cert,{revoked, keyCompromise}}},
	 { "4.15.9",  "Invalid delta-CRL Test9", {bad_cert,{revoked, keyCompromise}}}]).

%%-----------------------------------------------------------------------------

valid_distribution_points(doc) ->
    [""];
valid_distribution_points(suite) ->
    [];
valid_distribution_points(Config) when is_list(Config) ->
    run([{ "4.14.1",  "Valid distributionPoint Test1", ok},
	 { "4.14.4",  "Valid distributionPoint Test4", ok},
	 { "4.14.5",  "Valid distributionPoint Test5", ok},
	 { "4.14.7",  "Valid distributionPoint Test7", ok}
	]).

valid_distribution_points_no_issuing_distribution_point(doc) ->
    [""];
valid_distribution_points_no_issuing_distribution_point(suite) ->
    [];
valid_distribution_points_no_issuing_distribution_point(Config) when is_list(Config) ->
    run([{ "4.14.10", "Valid No issuingDistributionPoint Test10", ok}
	]).

invalid_distribution_points(doc) ->
    [""];
invalid_distribution_points(suite) ->
    [];
invalid_distribution_points(Config) when is_list(Config) ->
    run([{ "4.14.2",  "Invalid distributionPoint Test2", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.3",  "Invalid distributionPoint Test3", {bad_cert,
							  revocation_status_undetermined}},
	 { "4.14.6",  "Invalid distributionPoint Test6", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.8",  "Invalid distributionPoint Test8", {bad_cert,
							  revocation_status_undetermined}},
	 { "4.14.9",  "Invalid distributionPoint Test9", {bad_cert,
							  revocation_status_undetermined}}
	]).

valid_only_contains(doc) ->
    [""];
valid_only_contains(suite) ->
    [];
valid_only_contains(Config) when is_list(Config) ->
    run([{ "4.14.13", "Valid onlyContainsCACerts CRL Test13", ok}]).


invalid_only_contains(doc) ->
    [""];
invalid_only_contains(suite) ->
    [];
invalid_only_contains(Config) when is_list(Config) ->
    run([{ "4.14.11", "Invalid onlyContainsUserCerts CRL Test11",
	   {bad_cert, revocation_status_undetermined}},
	 { "4.14.12", "Invalid onlyContainsCACerts CRL Test12",
	   {bad_cert, revocation_status_undetermined}},
	 { "4.14.14", "Invalid onlyContainsAttributeCerts Test14",
	   {bad_cert, revocation_status_undetermined}}
	]).

valid_only_some_reasons(doc) ->
    [""];
valid_only_some_reasons(suite) ->
    [];
valid_only_some_reasons(Config) when is_list(Config) ->
    run([{ "4.14.18", "Valid onlySomeReasons Test18", ok},
	 { "4.14.19", "Valid onlySomeReasons Test19", ok}
	]).

invalid_only_some_reasons(doc) ->
    [""];
invalid_only_some_reasons(suite) ->
    [];
invalid_only_some_reasons(Config) when is_list(Config) ->
    run([{ "4.14.15", "Invalid onlySomeReasons Test15",
	   {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.16", "Invalid onlySomeReasons Test16",
	   {bad_cert,{revoked, certificateHold}}},
	 { "4.14.17", "Invalid onlySomeReasons Test17",
	   {bad_cert, revocation_status_undetermined}},
	 { "4.14.20", "Invalid onlySomeReasons Test20",
	   {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.21", "Invalid onlySomeReasons Test21",
	   {bad_cert,{revoked, affiliationChanged}}}
	]).

valid_indirect_crl(doc) ->
    [""];
valid_indirect_crl(suite) ->
    [];
valid_indirect_crl(Config) when is_list(Config) ->
    run([{ "4.14.22", "Valid IDP with indirectCRL Test22", ok},
	 { "4.14.24", "Valid IDP with indirectCRL Test24", ok},
	 { "4.14.25", "Valid IDP with indirectCRL Test25", ok}
	]).

invalid_indirect_crl(doc) ->
    [""];
invalid_indirect_crl(suite) ->
    [];
invalid_indirect_crl(Config) when is_list(Config) ->
    run([{ "4.14.23", "Invalid IDP with indirectCRL Test23",
	   {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.26", "Invalid IDP with indirectCRL Test26",
	   {bad_cert, revocation_status_undetermined}}
	]).

valid_crl_issuer(doc) ->
    [""];
valid_crl_issuer(suite) ->
    [];
valid_crl_issuer(Config) when is_list(Config) ->
    run([{ "4.14.28", "Valid cRLIssuer Test28", ok}%%,
	 %%{ "4.14.29", "Valid cRLIssuer Test29", ok},
	 %%{ "4.14.33", "Valid cRLIssuer Test33", ok}
	]).

invalid_crl_issuer(doc) ->
    [""];
invalid_crl_issuer(suite) ->
    [];
invalid_crl_issuer(Config) when is_list(Config) ->
    run([
	 { "4.14.27", "Invalid cRLIssuer Test27", {bad_cert, revocation_status_undetermined}},
	 { "4.14.31", "Invalid cRLIssuer Test31", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.32", "Invalid cRLIssuer Test32", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.34", "Invalid cRLIssuer Test34", {bad_cert,{revoked, keyCompromise}}},
	 { "4.14.35", "Invalid cRLIssuer Test35", {bad_cert, revocation_status_undetermined}}
	]).


%%distribution_points() ->
    %%{ "4.14",    "Distribution Points" },
%%    [
	 %% Although this test is valid it has a circular dependency. As a result
	 %% an attempt is made to reursively checks a CRL path and rejected due to
	 %% a CRL path validation error. PKITS notes suggest this test does not
	 %% need to be run due to this issue.
%%	 { "4.14.30", "Valid cRLIssuer Test30", 54 }].


%%-----------------------------------------------------------------------------

unknown_critical_extension(doc) ->
    [""];
unknown_critical_extension(suite) ->
    [];
unknown_critical_extension(Config) when is_list(Config) ->
    run([{ "4.16.2",  "Invalid Unknown Critical Certificate Extension Test2",
	   {bad_cert,unknown_critical_extension}}]).

unknown_not_critical_extension(doc) ->
    [""];
unknown_not_critical_extension(suite) ->
    [];
unknown_not_critical_extension(Config) when is_list(Config) ->
    run([{ "4.16.1",  "Valid Unknown Not Critical Certificate Extension Test1", ok}]).

%%-----------------------------------------------------------------------------
run(Tests) ->    
    File = file(?CERTS,"TrustAnchorRootCertificate.crt"),
    {ok, TA} = file:read_file(File),
    run(Tests, TA).

run({Chap, Test, Result}, TA) ->
    CertChain = sort_chain(read_certs(Test),TA, [], false, Chap),
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
	    crl_options(TA, Test);
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

crl_options(TA, Test) ->
     case read_crls(Test) of
	[] ->
	    [];
        CRLs ->
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
			{ErlCerts, CRLs} = UserState#verify_state.crl_info,
			CRLInfo0 =
			    crl_info(OtpCert,
				     ErlCerts,[{DerCRL, public_key:der_decode('CertificateList',
									      DerCRL)} || DerCRL <- CRLs],
				     []),
			CRLInfo = lists:reverse(CRLInfo0),
			Certs = UserState#verify_state.certs_db,
			Fun = fun(DP, CRLtoValidate, Id, CertsDb) ->
				      trusted_cert_and_path(DP, CRLtoValidate, Id, CertsDb)
			      end,
			Ignore = ignore_sign_test_when_building_path(Test),
			case public_key:pkix_crls_validate(OtpCert, CRLInfo,
							   [{issuer_fun,{Fun, {Ignore, Certs}}}]) of
			    valid ->
				{valid, UserState};
			    Reason  ->
				{fail, Reason}
			end
		end,

	    Certs = read_certs(Test),
	    ErlCerts = [public_key:pkix_decode_cert(Cert, otp) || Cert <- Certs],

	    [{verify_fun, {Fun, #verify_state{certs_db = [TA| Certs],
					      crl_info = {ErlCerts, CRLs}}}}]
    end.

crl_info(_, _, [], Acc) ->
    Acc;
crl_info(OtpCert, Certs, [{_, #'CertificateList'{tbsCertList =
						     #'TBSCertList'{issuer = Issuer,
								    crlExtensions = CRLExtensions}}}
				= CRL | Rest], Acc) ->
    OtpTBSCert =  OtpCert#'OTPCertificate'.tbsCertificate,
    Extensions = OtpTBSCert#'OTPTBSCertificate'.extensions,
    ExtList = pubkey_cert:extensions_list(CRLExtensions),
    DPs  = case pubkey_cert:select_extension(?'id-ce-cRLDistributionPoints', Extensions) of
	      #'Extension'{extnValue = Value} ->
		   lists:map(fun(Point) -> pubkey_cert_records:transform(Point, decode) end, Value);
	      _ ->
		  case same_issuer(OtpCert, Issuer) of
		      true ->
			  [make_dp(ExtList, asn1_NOVALUE, Issuer)];
		      false ->
			  [make_dp(ExtList, Issuer, ignore)]
		  end
	   end,
    DPsCRLs = lists:map(fun(DP) -> {DP, CRL} end, DPs),
    crl_info(OtpCert, Certs, Rest, DPsCRLs ++ Acc).

ignore_sign_test_when_building_path("Invalid Bad CRL Signature Test4") ->
    true;
ignore_sign_test_when_building_path(_) ->
    false.

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

trusted_cert_and_path(DP, CRL, Id, {Ignore, CertsList}) ->
    case crl_issuer(crl_issuer_name(DP), CRL, Id, CertsList, CertsList, Ignore) of
	{ok, IssuerCert, DerIssuerCert} ->
	    Certs = [{public_key:pkix_decode_cert(Cert, otp), Cert} || Cert <- CertsList],
	    CertChain = build_chain(Certs, Certs, IssuerCert, Ignore, [DerIssuerCert]),
	    {ok, public_key:pkix_decode_cert(hd(CertChain), otp), CertChain};
	Other ->
	    Other
    end.

crl_issuer_name(#'DistributionPoint'{cRLIssuer = asn1_NOVALUE}) ->
    undefined;
crl_issuer_name(#'DistributionPoint'{cRLIssuer = [{directoryName, Issuer}]}) ->
    pubkey_cert_records:transform(Issuer, decode).

build_chain([],_, _, _,Acc) ->
    Acc;

build_chain([{First, DerFirst}|Certs], All, Cert, Ignore, Acc) ->
    case public_key:pkix_is_self_signed(Cert) andalso is_test_root(Cert) of
	true ->
	    Acc;
	false ->
	    case public_key:pkix_is_issuer(Cert, First)
		%%andalso check_extension_cert_signer(First)
		andalso is_signer(First, Cert, Ignore)
	    of
		true ->
		    build_chain(All, All, First, Ignore, [DerFirst | Acc]);
		false ->
		    build_chain(Certs, All, Cert, Ignore, Acc)
	    end
    end.

is_signer(_,_, true) ->
    true;
is_signer(Signer, #'OTPCertificate'{} = Cert,_) ->
    TBSCert = Signer#'OTPCertificate'.tbsCertificate,
    PublicKeyInfo = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PublicKey = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    AlgInfo = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    PublicKeyParams = AlgInfo#'PublicKeyAlgorithm'.parameters,
    try pubkey_cert:validate_signature(Cert, public_key:pkix_encode('OTPCertificate',
								Cert, otp),
				   PublicKey, PublicKeyParams, true, ?DEFAULT_VERIFYFUN) of
	true ->
	    true
    catch
	_:_ ->
	    false
    end;
is_signer(Signer, #'CertificateList'{} = CRL, _) ->
    TBSCert = Signer#'OTPCertificate'.tbsCertificate,
    PublicKeyInfo = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PublicKey = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    AlgInfo = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    PublicKeyParams = AlgInfo#'PublicKeyAlgorithm'.parameters,
    pubkey_crl:verify_crl_signature(CRL, public_key:pkix_encode('CertificateList',
								CRL, plain),
				    PublicKey, PublicKeyParams).

is_test_root(OtpCert) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    {rdnSequence, AtterList} = TBSCert#'OTPTBSCertificate'.issuer,
    lists:member([{'AttributeTypeAndValue',{2,5,4,3},{printableString,"Trust Anchor"}}],
			AtterList).

check_extension_cert_signer(OtpCert) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
     Extensions = TBSCert#'OTPTBSCertificate'.extensions,
     case pubkey_cert:select_extension(?'id-ce-keyUsage', Extensions) of
	#'Extension'{extnValue = KeyUse} ->
	     lists:member(keyCertSign, KeyUse);
	 _ ->
	     true
    end.

check_extension_crl_signer(OtpCert) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
     Extensions = TBSCert#'OTPTBSCertificate'.extensions,
     case pubkey_cert:select_extension(?'id-ce-keyUsage', Extensions) of
	#'Extension'{extnValue = KeyUse} ->
	     lists:member(cRLSign, KeyUse);
	 _ ->
	     true
    end.

crl_issuer(undefined, CRL, issuer_not_found, _, CertsList, Ignore) ->
    crl_issuer(CRL, CertsList, Ignore);

crl_issuer(IssuerName, CRL, issuer_not_found, CertsList, CertsList, Ignore) ->
    crl_issuer(IssuerName, CRL, IssuerName, CertsList, CertsList, Ignore);

crl_issuer(undefined, CRL, Id, [Cert | Rest], All, false) ->
    ErlCert = public_key:pkix_decode_cert(Cert, otp),
    TBSCertificate = ErlCert#'OTPCertificate'.tbsCertificate,
    SerialNumber = TBSCertificate#'OTPTBSCertificate'.serialNumber,
    Issuer = public_key:pkix_normalize_name(
		    TBSCertificate#'OTPTBSCertificate'.subject),
    Bool = is_signer(ErlCert, CRL, false),
    case {SerialNumber, Issuer} of
	Id when Bool == true ->
	    {ok, ErlCert, Cert};
	_ ->
	    crl_issuer(undefined, CRL, Id, Rest, All, false)
    end;

crl_issuer(IssuerName, CRL, Id, [Cert | Rest], All, false) ->
    ErlCert = public_key:pkix_decode_cert(Cert, otp),
    TBSCertificate = ErlCert#'OTPCertificate'.tbsCertificate,
    SerialNumber = TBSCertificate#'OTPTBSCertificate'.serialNumber,
    %%Issuer = public_key:pkix_normalize_name(
    %%		    TBSCertificate#'OTPTBSCertificate'.subject),
    Bool = is_signer(ErlCert, CRL, false),
    case {SerialNumber, IssuerName} of
	Id when Bool == true ->
	    {ok, ErlCert, Cert};
	{_, IssuerName}  when Bool == true ->
	    {ok, ErlCert, Cert};
	_ ->
	    crl_issuer(IssuerName, CRL, Id, Rest, All, false)
    end;

crl_issuer(undefined, CRL, _, [], CertsList, Ignore) ->
    crl_issuer(CRL, CertsList, Ignore);
crl_issuer(CRLName, CRL, _, [], CertsList, Ignore) ->
    crl_issuer(CRLName, CRL, CertsList, Ignore).


crl_issuer(_, [],_) ->
    {error, issuer_not_found};
crl_issuer(CRL, [Cert | Rest], Ignore) ->
    ErlCert = public_key:pkix_decode_cert(Cert, otp),
    case public_key:pkix_is_issuer(CRL, ErlCert) andalso
	check_extension_crl_signer(ErlCert) andalso
	is_signer(ErlCert, CRL, Ignore)
    of
        true ->
	    {ok, ErlCert,Cert};
        false ->
            crl_issuer(CRL, Rest, Ignore)
    end.

crl_issuer(_,_, [],_) ->
    {error, issuer_not_found};
crl_issuer(IssuerName, CRL, [Cert | Rest], Ignore) ->
    ErlCert = public_key:pkix_decode_cert(Cert, otp),
    TBSCertificate = ErlCert#'OTPCertificate'.tbsCertificate,
    Issuer = public_key:pkix_normalize_name(
		    TBSCertificate#'OTPTBSCertificate'.subject),

    case
	public_key:pkix_is_issuer(CRL, ErlCert) andalso
	check_extension_crl_signer(ErlCert) andalso
	is_signer(ErlCert, CRL, Ignore)
    of
        true ->
	    case  pubkey_cert:is_issuer(Issuer, IssuerName) of
		true ->
		    {ok, ErlCert,Cert};
		false ->
		    crl_issuer(IssuerName, CRL, Rest, Ignore)
	    end;
	false ->
            crl_issuer(IssuerName, CRL, Rest, Ignore)
    end.

read_certs(Test) ->
    File = test_file(Test),
    Ders = erl_make_certs:pem_to_der(File),
    [Cert || {'Certificate', Cert, not_encrypted} <- Ders].

read_crls(Test) ->
    File = test_file(Test),
    Ders = erl_make_certs:pem_to_der(File),
    [CRL || {'CertificateList', CRL, not_encrypted} <- Ders].

test_file(Test) ->
    file(?CONV, lists:append(string:tokens(Test, " -")) ++ ".pem").

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

sort_chain(Certs, TA, Acc, Bool, Chap) when Chap == "4.5.3"->
     [CA, Entity, Self] = do_sort_chain(Certs, TA, Acc, Bool, Chap),
     [CA, Self, Entity];
sort_chain(Certs, TA, Acc, Bool, Chap) when Chap == "4.5.4";
					    Chap == "4.5.5" ->
     [CA, Entity, _Self] = do_sort_chain(Certs, TA, Acc, Bool, Chap),
     [CA, Entity];

sort_chain(Certs, TA, Acc, Bool, Chap) when Chap == "4.14.24";
					    Chap == "4.14.25";
					    Chap == "4.14.26";
					    Chap == "4.14.27";
					    Chap == "4.14.31";
					    Chap == "4.14.32";
					    Chap == "4.14.33" ->
    [_OtherCA, Entity, CA] = do_sort_chain(Certs, TA, Acc, Bool, Chap),
    [CA, Entity];

sort_chain(Certs, TA, Acc, Bool, Chap) when Chap == "4.14.28";
					    Chap == "4.14.29" ->
    [CA, _OtherCA, Entity] = do_sort_chain(Certs, TA, Acc, Bool, Chap),
    [CA, Entity];


sort_chain(Certs, TA, Acc, Bool, Chap) when Chap == "4.14.33" ->
    [Entity, CA, _OtherCA] = do_sort_chain(Certs, TA, Acc, Bool, Chap),
    [CA, Entity];


sort_chain(Certs, TA, Acc, Bool, Chap) ->
    do_sort_chain(Certs, TA, Acc, Bool, Chap).

do_sort_chain([First], TA, Try, Found, Chap) when Chap == "4.5.6";
						  Chap == "4.5.7";
						  Chap == "4.4.19";
						  Chap == "4.4.20";
						  Chap == "4.4.21"->
    case public_key:pkix_is_issuer(First,TA) of
	true -> 
	    [First|do_sort_chain([],First,Try,true, Chap)];
	false ->
	    do_sort_chain([],TA,[First|Try],Found, Chap)
    end;
do_sort_chain([First|Certs], TA, Try, Found, Chap) when Chap == "4.5.6";
							Chap == "4.5.7";
							Chap == "4.4.19";
							Chap  == "4.4.20";
							Chap == "4.4.21"->
%%    case check_extension_cert_signer(public_key:pkix_decode_cert(First, otp)) of
%%	true ->
    case public_key:pkix_is_issuer(First,TA) of
	true ->
	    [First|do_sort_chain(Certs,First,Try,true, Chap)];
	false ->
	    do_sort_chain(Certs,TA,[First|Try],Found, Chap)
    end;
%%	false ->
%%	    do_sort_chain(Certs, TA, Try, Found, Chap)
%%   end;

do_sort_chain([First|Certs], TA, Try, Found, Chap) ->
    case public_key:pkix_is_issuer(First,TA) of
	true ->
	    [First|do_sort_chain(Certs,First,Try,true, Chap)];
	false ->
	    do_sort_chain(Certs,TA,[First|Try],Found, Chap)
    end;

do_sort_chain([], _, [],_, _) -> [];
do_sort_chain([], Valid, Check, true, Chap) ->
    do_sort_chain(lists:reverse(Check), Valid, [], false, Chap);
do_sort_chain([], _Valid, Check, false, _) ->
    Check.

error(Format, Args, File0, Line) ->
    File = filename:basename(File0),
    Pid = group_leader(),
    Pid ! {failed, File, Line},
    io:format(Pid, "~s(~p): ERROR"++Format, [File,Line|Args]).

warning(Format, Args, File0, Line) ->
    File = filename:basename(File0),
    io:format("~s(~p): Warning "++Format, [File,Line|Args]).

%% Certificate policy tests need special handling. They can have several
%% sub tests and we need to check the outputs are correct.

certificate_policies() ->
    %%{ "4.8", "Certificate Policies" },
    [{"4.8.1.1", "All Certificates Same Policy Test1", "-policy anyPolicy -explicit_policy", "True", ?NIST1, ?NIST1, 0},
     {"4.8.1.2", "All Certificates Same Policy Test1", "-policy ?NIST1 -explicit_policy", "True", ?NIST1, ?NIST1, 0},
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
require_explicit_policy() ->
    %%{ "4.9", "Require Explicit Policy" },
    [{"4.9.1", "Valid RequireExplicitPolicy Test1", "-policy anyPolicy", "False", "<empty>", "<empty>", 0},
     {"4.9.2", "Valid RequireExplicitPolicy Test2", "-policy anyPolicy", "False", "<empty>", "<empty>", 0},
     {"4.9.3", "Invalid RequireExplicitPolicy Test3", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.9.4", "Valid RequireExplicitPolicy Test4", "-policy anyPolicy", "True", "?NIST1", "?NIST1", 0},
     {"4.9.5", "Invalid RequireExplicitPolicy Test5", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.9.6", "Valid Self-Issued requireExplicitPolicy Test6", "-policy anyPolicy", "False", "<empty>", "<empty>", 0},
     {"4.9.7", "Invalid Self-Issued requireExplicitPolicy Test7", "-policy anyPolicy", "True", "<empty>", "<empty>", 43},
     {"4.9.8", "Invalid Self-Issued requireExplicitPolicy Test8", "-policy anyPolicy", "True", "<empty>", "<empty>", 43}].
policy_mappings() ->
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

inhibit_policy_mapping() ->
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
inhibit_any_policy() ->
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
