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


%%  Se specification here:
%%  http://csrc.nist.gov/groups/ST/crypto_apps_infra/pki/pkitesting.html

-module(pkits_SUITE).

-compile(export_all).

%%-include_lib("public_key/include/public_key.hrl").
-include("public_key.hrl").

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

%%
all(doc) ->
    ["PKITS tests for RFC3280 compliance"];
all(suite) ->    
    [signature_verification,
     validity_periods,
     verifying_name_chaining,
     %% basic_certificate_revocation_tests,
     verifying_paths_with_self_issued_certificates,
     verifying_basic_constraints,
     key_usage,
%%      certificate_policies,
%%      require_explicit_policy,
%%      policy_mappings,
%%      inhibit_policy_mapping,
%%      inhibit_any_policy,
     name_constraints,	     
%%      distribution_points,
%%      delta_crls,
     private_certificate_extensions].

signature_verification(doc) ->    [""];
signature_verification(suite) -> [];
signature_verification(Config) when is_list(Config) ->
    run(signature_verification()).
validity_periods(doc) ->    [""];
validity_periods(suite) -> [];
validity_periods(Config) when is_list(Config) ->
    run(validity_periods()).
verifying_name_chaining(doc) ->    [""];
verifying_name_chaining(suite) -> [];
verifying_name_chaining(Config) when is_list(Config) ->
    run(verifying_name_chaining()).
basic_certificate_revocation_tests(doc) ->    [""];
basic_certificate_revocation_tests(suite) -> [];
basic_certificate_revocation_tests(Config) when is_list(Config) ->
    run(basic_certificate_revocation_tests()).
verifying_paths_with_self_issued_certificates(doc) ->    [""];
verifying_paths_with_self_issued_certificates(suite) -> [];
verifying_paths_with_self_issued_certificates(Config) when is_list(Config) ->
    run(verifying_paths_with_self_issued_certificates()).
verifying_basic_constraints(doc) ->    [""];
verifying_basic_constraints(suite) -> [];
verifying_basic_constraints(Config) when is_list(Config) ->
    run(verifying_basic_constraints()).
key_usage(doc) ->    [""];
key_usage(suite) -> [];
key_usage(Config) when is_list(Config) ->
    run(key_usage()).
certificate_policies(doc) ->    [""];
certificate_policies(suite) -> [];
certificate_policies(Config) when is_list(Config) ->
    run(certificate_policies()).
require_explicit_policy(doc) ->    [""];
require_explicit_policy(suite) -> [];
require_explicit_policy(Config) when is_list(Config) ->
    run(require_explicit_policy()).
policy_mappings(doc) ->     [""];
policy_mappings(suite) -> [];
policy_mappings(Config) when is_list(Config) ->
    run(policy_mappings()).
inhibit_policy_mapping(doc) ->    [""];
inhibit_policy_mapping(suite) -> [];
inhibit_policy_mapping(Config) when is_list(Config) ->
    run(inhibit_policy_mapping()).
inhibit_any_policy(doc) ->    [""];
inhibit_any_policy(suite) -> [];
inhibit_any_policy(Config) when is_list(Config) ->
    run(inhibit_any_policy()).
name_constraints(doc) ->    [""];
name_constraints(suite) -> [];
name_constraints(Config) when is_list(Config) ->
    run(name_constraints()).
distribution_points(doc) ->    [""];
distribution_points(suite) -> [];
distribution_points(Config) when is_list(Config) ->
    run(distribution_points()).
delta_crls(doc) ->    [""];
delta_crls(suite) -> [];
delta_crls(Config) when is_list(Config) ->
    run(delta_crls()).
private_certificate_extensions(doc) ->    [""];
private_certificate_extensions(suite) -> [];
private_certificate_extensions(Config) when is_list(Config) ->
    run(private_certificate_extensions()).
    
run() ->
    catch crypto:start(),
    Tests = 
	[signature_verification(),
	 validity_periods(),
	 verifying_name_chaining(),
	 %%basic_certificate_revocation_tests(),
	 verifying_paths_with_self_issued_certificates(),
	 verifying_basic_constraints(),
	 key_usage(),
	 %%certificate_policies(),
	 %%require_explicit_policy(),
	 %%policy_mappings(),
	 %%inhibit_policy_mapping(),
	 %%inhibit_any_policy(),
	 name_constraints(),	     
	 %distribution_points(),
	 %delta_crls(),
	 private_certificate_extensions()
	],
    run(lists:append(Tests)).

run(Tests) ->    
    File = file(?CERTS,"TrustAnchorRootCertificate.crt"),
    {ok, TA} = file:read_file(File),
    run(Tests, TA).

run({Chap, Test, Result}, TA) ->
    CertChain = sort_chain(read_certs(Test),TA, [], false),    
    try public_key:pkix_path_validation(TA, CertChain, []) of	
	{Result, _} -> ok;	
	{error,Result} when Result =/= ok ->
	    ok;
	{error,Error} when is_integer(Result) ->
	    ?warning(" ~p~n  Got ~p expected ~p~n",[Test, Error, Result]);
	{error,Error} when Result =/= ok ->
	    ?error(" minor ~p~n  Got ~p expected ~p~n",[Test, Error, Result]);
	{error, Error}  ->
	    ?error(" ~p ~p~n  Expected ~p got ~p ~n", [Chap, Test, Result, Error]),
	    fail;
	{ok, _} when Result =/= ok ->
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


read_certs(Test) ->
    File = test_file(Test),
    %% io:format("Read ~p ",[File]),
    {ok, Ders} = public_key:pem_to_der(File),
    %% io:format("Ders ~p ~n",[length(Ders)]),
    [Cert || {cert,Cert,not_encrypted} <- Ders].

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

sort_chain([First|Certs], TA, Try, Found) -> 
    case public_key:pkix_is_issuer(First,TA) of
	true -> 
	    [First|sort_chain(Certs,First,Try,true)];
	false ->
	    sort_chain(Certs,TA,[First|Try],Found)
    end;
sort_chain([], _, [],_) -> [];
sort_chain([], Valid, Check, true) ->
    sort_chain(lists:reverse(Check), Valid, [], false);
sort_chain([], _Valid, Check, false) ->
    Check.

signature_verification() ->    
    %%  "4.1", "Signature Verification" ,
    [{ "4.1.1", "Valid Signatures Test1",                        ok},
     { "4.1.2", "Invalid CA Signature Test2",                    {bad_cert,invalid_signature}},
     { "4.1.3", "Invalid EE Signature Test3",                    {bad_cert,invalid_signature}},
     { "4.1.4", "Valid DSA Signatures Test4",                    ok},
     { "4.1.5", "Valid DSA Parameter Inheritance Test5",         ok},
     { "4.1.6", "Invalid DSA Signature Test6",                   {bad_cert,invalid_signature}}].
validity_periods() ->
    %% { "4.2",   "Validity Periods" },
    [{ "4.2.1", "Invalid CA notBefore Date Test1",               {bad_cert, cert_expired}},
     { "4.2.2", "Invalid EE notBefore Date Test2",               {bad_cert, cert_expired}},
     { "4.2.3", "Valid pre2000 UTC notBefore Date Test3",        ok},
     { "4.2.4", "Valid GeneralizedTime notBefore Date Test4",    ok},
     { "4.2.5", "Invalid CA notAfter Date Test5",                {bad_cert, cert_expired}},
     { "4.2.6", "Invalid EE notAfter Date Test6",                {bad_cert, cert_expired}},
     { "4.2.7", "Invalid pre2000 UTC EE notAfter Date Test7",    {bad_cert, cert_expired}},
     { "4.2.8", "Valid GeneralizedTime notAfter Date Test8",     ok}].
verifying_name_chaining() ->
    %%{ "4.3",   "Verifying Name Chaining" },
    [{ "4.3.1", "Invalid Name Chaining EE Test1",                {bad_cert, invalid_issuer}},
     { "4.3.2", "Invalid Name Chaining Order Test2",             {bad_cert, invalid_issuer}},
     { "4.3.3", "Valid Name Chaining Whitespace Test3",          ok},
     { "4.3.4", "Valid Name Chaining Whitespace Test4",          ok},
     { "4.3.5", "Valid Name Chaining Capitalization Test5",      ok},
     { "4.3.6", "Valid Name Chaining UIDs Test6",                ok},
     { "4.3.7", "Valid RFC3280 Mandatory Attribute Types Test7", ok},
     { "4.3.8", "Valid RFC3280 Optional Attribute Types Test8",  ok},
     { "4.3.9", "Valid UTF8String Encoded Names Test9",          ok},
     { "4.3.10", "Valid Rollover from PrintableString to UTF8String Test10", ok},
     { "4.3.11", "Valid UTF8String Case Insensitive Match Test11",           ok}].
basic_certificate_revocation_tests() ->
    %%{ "4.4",    "Basic Certificate Revocation Tests" },
    [{ "4.4.1",  "Missing CRL Test1",                 3 },
     { "4.4.2", "Invalid Revoked CA Test2",          23 },
     { "4.4.3", "Invalid Revoked EE Test3",          23 },
     { "4.4.4", "Invalid Bad CRL Signature Test4",   8 },
     { "4.4.5", "Invalid Bad CRL Issuer Name Test5", 3 },
     { "4.4.6", "Invalid Wrong CRL Test6",           3 },
     { "4.4.7", "Valid Two CRLs Test7",              ok},

     %% The test document suggests these should return certificate revoked...
     %% Subsquent discussion has concluded they should not due to unhandle
     %% critical CRL extensions.
     { "4.4.8", "Invalid Unknown CRL Entry Extension Test8", 36 },
     { "4.4.9", "Invalid Unknown CRL Extension Test9",       36 },

     { "4.4.10", "Invalid Unknown CRL Extension Test10",             36 },
     { "4.4.11", "Invalid Old CRL nextUpdate Test11",                12 },
     { "4.4.12", "Invalid pre2000 CRL nextUpdate Test12",            12 },
     { "4.4.13", "Valid GeneralizedTime CRL nextUpdate Test13",      ok},
     { "4.4.14", "Valid Negative Serial Number Test14",              ok},
     { "4.4.15", "Invalid Negative Serial Number Test15",            23 },
     { "4.4.16", "Valid Long Serial Number Test16",                  ok},
     { "4.4.17", "Valid Long Serial Number Test17",                  ok},
     { "4.4.18", "Invalid Long Serial Number Test18",                23 },
     { "4.4.19", "Valid Separate Certificate and CRL Keys Test19",   ok},
     { "4.4.20", "Invalid Separate Certificate and CRL Keys Test20", 23 },

     %% CRL path is revoked so get a CRL path validation error
     { "4.4.21", "Invalid Separate Certificate and CRL Keys Test21",      54 }].
verifying_paths_with_self_issued_certificates() ->
    %%{ "4.5",    "Verifying Paths with Self-Issued Certificates" },
    [{ "4.5.1",  "Valid Basic Self-Issued Old With New Test1",            ok},
     %%{ "4.5.2",  "Invalid Basic Self-Issued Old With New Test2",          23 },
     %%{ "4.5.3",  "Valid Basic Self-Issued New With Old Test3",            ok},
     %%{ "4.5.4",  "Valid Basic Self-Issued New With Old Test4",            ok},
     { "4.5.5",  "Invalid Basic Self-Issued New With Old Test5",          23 },
     %%{ "4.5.6",  "Valid Basic Self-Issued CRL Signing Key Test6",         ok},
     { "4.5.7",  "Invalid Basic Self-Issued CRL Signing Key Test7",       23 },
     { "4.5.8",  "Invalid Basic Self-Issued CRL Signing Key Test8",       {bad_cert,invalid_key_usage} }].
verifying_basic_constraints() ->
    [%%{ "4.6",    "Verifying Basic Constraints" },
     { "4.6.1",  "Invalid Missing basicConstraints Test1",                
       {bad_cert, missing_basic_constraint} },
     { "4.6.2",  "Invalid cA False Test2",                                {bad_cert, missing_basic_constraint}},
     { "4.6.3",  "Invalid cA False Test3",                                {bad_cert, missing_basic_constraint}},
     { "4.6.4",  "Valid basicConstraints Not Critical Test4",             ok},
     { "4.6.5",  "Invalid pathLenConstraint Test5",                       {bad_cert, max_path_length_reached}},
     { "4.6.6",  "Invalid pathLenConstraint Test6",                       {bad_cert, max_path_length_reached}},
     { "4.6.7",  "Valid pathLenConstraint Test7",                         ok},
     { "4.6.8",  "Valid pathLenConstraint Test8",                         ok},
     { "4.6.9",  "Invalid pathLenConstraint Test9",                       {bad_cert, max_path_length_reached}},
     { "4.6.10", "Invalid pathLenConstraint Test10",                      {bad_cert, max_path_length_reached}},
     { "4.6.11", "Invalid pathLenConstraint Test11",                      {bad_cert, max_path_length_reached}},
     { "4.6.12", "Invalid pathLenConstraint Test12",                      {bad_cert, max_path_length_reached}},
     { "4.6.13", "Valid pathLenConstraint Test13",                        ok},
     { "4.6.14", "Valid pathLenConstraint Test14",                        ok},
     { "4.6.15", "Valid Self-Issued pathLenConstraint Test15",            ok},
     { "4.6.16", "Invalid Self-Issued pathLenConstraint Test16",          {bad_cert, max_path_length_reached}},
     { "4.6.17", "Valid Self-Issued pathLenConstraint Test17",            ok}].
key_usage() ->
    %%{ "4.7",    "Key Usage" },
    [{ "4.7.1",  "Invalid keyUsage Critical keyCertSign False Test1",     {bad_cert,invalid_key_usage} },
     { "4.7.2",  "Invalid keyUsage Not Critical keyCertSign False Test2", {bad_cert,invalid_key_usage} },
     { "4.7.3",  "Valid keyUsage Not Critical Test3",                     ok}
     %%,{ "4.7.4",  "Invalid keyUsage Critical cRLSign False Test4",         35 }
     %%,{ "4.7.5",  "Invalid keyUsage Not Critical cRLSign False Test5",     35 }
    ].

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

name_constraints() ->
    %%{ "4.13",    "Name Constraints" },
    [{ "4.13.1",  "Valid DN nameConstraints Test1",                ok},
     { "4.13.2",  "Invalid DN nameConstraints Test2",              {bad_cert, name_not_permitted}},
     { "4.13.3",  "Invalid DN nameConstraints Test3",              {bad_cert, name_not_permitted}},
     { "4.13.4",  "Valid DN nameConstraints Test4",                ok},
     { "4.13.5",  "Valid DN nameConstraints Test5",                ok},
     { "4.13.6",  "Valid DN nameConstraints Test6",                ok},
     { "4.13.7",  "Invalid DN nameConstraints Test7",              {bad_cert, name_not_permitted}},
     { "4.13.8",  "Invalid DN nameConstraints Test8",              {bad_cert, name_not_permitted}},
     { "4.13.9",  "Invalid DN nameConstraints Test9",              {bad_cert, name_not_permitted}},
     { "4.13.10", "Invalid DN nameConstraints Test10",             {bad_cert, name_not_permitted}},
     { "4.13.11", "Valid DN nameConstraints Test11",               ok},
     { "4.13.12", "Invalid DN nameConstraints Test12",             {bad_cert, name_not_permitted}},
     { "4.13.13", "Invalid DN nameConstraints Test13",             {bad_cert, name_not_permitted}},
     { "4.13.14", "Valid DN nameConstraints Test14",               ok},
     { "4.13.15", "Invalid DN nameConstraints Test15",             {bad_cert, name_not_permitted}},
     { "4.13.16", "Invalid DN nameConstraints Test16",             {bad_cert, name_not_permitted}},
     { "4.13.17", "Invalid DN nameConstraints Test17",             {bad_cert, name_not_permitted}},
     { "4.13.18", "Valid DN nameConstraints Test18",               ok},
     { "4.13.19", "Valid Self-Issued DN nameConstraints Test19",   ok},
     { "4.13.20", "Invalid Self-Issued DN nameConstraints Test20", {bad_cert, name_not_permitted} },
     { "4.13.21", "Valid RFC822 nameConstraints Test21",           ok},
     { "4.13.22", "Invalid RFC822 nameConstraints Test22",         {bad_cert, name_not_permitted} },
     { "4.13.23", "Valid RFC822 nameConstraints Test23",           ok},
     { "4.13.24", "Invalid RFC822 nameConstraints Test24",         {bad_cert, name_not_permitted} },
     { "4.13.25", "Valid RFC822 nameConstraints Test25",           ok},
     { "4.13.26", "Invalid RFC822 nameConstraints Test26",         {bad_cert, name_not_permitted}},
     { "4.13.27", "Valid DN and RFC822 nameConstraints Test27",    ok},
     { "4.13.28", "Invalid DN and RFC822 nameConstraints Test28",  {bad_cert, name_not_permitted} },
     { "4.13.29", "Invalid DN and RFC822 nameConstraints Test29",  {bad_cert, name_not_permitted} },
     { "4.13.30", "Valid DNS nameConstraints Test30",              ok},
     { "4.13.31", "Invalid DNS nameConstraints Test31",            {bad_cert, name_not_permitted} },
     { "4.13.32", "Valid DNS nameConstraints Test32",              ok},
     { "4.13.33", "Invalid DNS nameConstraints Test33",            {bad_cert, name_not_permitted}},
     { "4.13.34", "Valid URI nameConstraints Test34",              ok},
     { "4.13.35", "Invalid URI nameConstraints Test35",            {bad_cert, name_not_permitted} },
     { "4.13.36", "Valid URI nameConstraints Test36",              ok},
     { "4.13.37", "Invalid URI nameConstraints Test37",            {bad_cert, name_not_permitted}},
     { "4.13.38", "Invalid DNS nameConstraints Test38",            {bad_cert, name_not_permitted} }].
distribution_points() ->
     %%{ "4.14",    "Distribution Points" },
    [{ "4.14.1",  "Valid distributionPoint Test1",                 ok},
     { "4.14.2",  "Invalid distributionPoint Test2",               23 },
     { "4.14.3",  "Invalid distributionPoint Test3",               44 },
     { "4.14.4",  "Valid distributionPoint Test4",                 ok},
     { "4.14.5",  "Valid distributionPoint Test5",                 ok},
     { "4.14.6",  "Invalid distributionPoint Test6",               23 },
     { "4.14.7",  "Valid distributionPoint Test7",                 ok},
     { "4.14.8",  "Invalid distributionPoint Test8",               44 },
     { "4.14.9",  "Invalid distributionPoint Test9",               44 },
     { "4.14.10", "Valid No issuingDistributionPoint Test10",      ok},
     { "4.14.11", "Invalid onlyContainsUserCerts CRL Test11",      44 },
     { "4.14.12", "Invalid onlyContainsCACerts CRL Test12",        44 },
     { "4.14.13", "Valid onlyContainsCACerts CRL Test13",          ok},
     { "4.14.14", "Invalid onlyContainsAttributeCerts Test14",     44 },
     { "4.14.15", "Invalid onlySomeReasons Test15",                23 },
     { "4.14.16", "Invalid onlySomeReasons Test16",                23 },
     { "4.14.17", "Invalid onlySomeReasons Test17",                3 },
     { "4.14.18", "Valid onlySomeReasons Test18",                  ok},
     { "4.14.19", "Valid onlySomeReasons Test19",                  ok},
     { "4.14.20", "Invalid onlySomeReasons Test20",                23 },
     { "4.14.21", "Invalid onlySomeReasons Test21",                23 },
     { "4.14.22", "Valid IDP with indirectCRL Test22",             ok},
     { "4.14.23", "Invalid IDP with indirectCRL Test23",           23 },
     { "4.14.24", "Valid IDP with indirectCRL Test24",             ok},
     { "4.14.25", "Valid IDP with indirectCRL Test25",             ok},
     { "4.14.26", "Invalid IDP with indirectCRL Test26",           44 },
     { "4.14.27", "Invalid cRLIssuer Test27",                      3 },
     { "4.14.28", "Valid cRLIssuer Test28",                        ok},
     { "4.14.29", "Valid cRLIssuer Test29",                        ok},

     %% Although this test is valid it has a circular dependency. As a result
     %% an attempt is made to reursively checks a CRL path and rejected due to
     %% a CRL path validation error. PKITS notes suggest this test does not
     %% need to be run due to this issue.
     { "4.14.30", "Valid cRLIssuer Test30",                                 54 },
     { "4.14.31", "Invalid cRLIssuer Test31",                               23 },
     { "4.14.32", "Invalid cRLIssuer Test32",                               23 },
     { "4.14.33", "Valid cRLIssuer Test33",                                 ok},
     { "4.14.34", "Invalid cRLIssuer Test34",                               23 },
     { "4.14.35", "Invalid cRLIssuer Test35",                               44 }].
delta_crls() ->
    %%{ "4.15",    "Delta-CRLs" },
    [{ "4.15.1",  "Invalid deltaCRLIndicator No Base Test1",                3 },
     { "4.15.2",  "Valid delta-CRL Test2",                                  ok},
     { "4.15.3",  "Invalid delta-CRL Test3",                                23 },
     { "4.15.4",  "Invalid delta-CRL Test4",                                23 },
     { "4.15.5",  "Valid delta-CRL Test5",                                  ok},
     { "4.15.6",  "Invalid delta-CRL Test6",                                23 },
     { "4.15.7",  "Valid delta-CRL Test7",                                  ok},
     { "4.15.8",  "Valid delta-CRL Test8",                                  ok},
     { "4.15.9",  "Invalid delta-CRL Test9",                                23 },
     { "4.15.10", "Invalid delta-CRL Test10",                               12 }].
private_certificate_extensions() ->
    %%{ "4.16",    "Private Certificate Extensions" },
    [{ "4.16.1",  "Valid Unknown Not Critical Certificate Extension Test1", ok},
     { "4.16.2",  "Invalid Unknown Critical Certificate Extension Test2",   
       {bad_cert,unknown_critical_extension}}].


convert() ->
    Tests = [signature_verification(),
	     validity_periods(),
	     verifying_name_chaining(),
	     basic_certificate_revocation_tests(),
	     verifying_paths_with_self_issued_certificates(),
	     verifying_basic_constraints(),
	     key_usage(),
	     certificate_policies(),
	     require_explicit_policy(),
	     policy_mappings(),
	     inhibit_policy_mapping(),
	     inhibit_any_policy(),
	     name_constraints(),	     
	     distribution_points(),
	     delta_crls(),
	     private_certificate_extensions()],    
    [convert(Test) || Test <- lists:flatten(Tests)].

convert({_,Test,_}) ->
    convert1(Test);
convert({_,Test,_,_,_,_,_}) ->
    convert1(Test).

convert1(Test) ->
    FName = lists:append(string:tokens(Test, " -")),
    File = filename:join(?MIME, "Signed" ++ FName ++ ".eml"),
    io:format("Convert ~p~n",[File]),
    {ok, Mail} = file:read_file(File),
    Base64  = skip_lines(Mail),
    %%io:format("~s",[Base64]),
    Tmp = base64:mime_decode(Base64),
    file:write_file("pkits/smime-pem/tmp-pkcs7.der", Tmp),
    Cmd = "openssl pkcs7 -inform der -in pkits/smime-pem/tmp-pkcs7.der"
	" -print_certs -out pkits/smime-pem/" ++ FName ++ ".pem",
    case os:cmd(Cmd) of
	"" -> ok;
	Err -> 
	    io:format("~s",[Err]),
	    erlang:error(bad_cmd)
    end.

skip_lines(<<"\r\n\r\n", Rest/binary>>) -> Rest;
skip_lines(<<"\n\n", Rest/binary>>) -> Rest;
skip_lines(<<_:8, Rest/binary>>) ->
    skip_lines(Rest).

init_per_testcase(_Func, Config) ->    
    Datadir = proplists:get_value(data_dir, Config),
    put(datadir, Datadir),
    Config.

fin_per_testcase(_Func, Config) ->
    %% Nodes = select_nodes(all, Config, ?FILE, ?LINE),
    %% rpc:multicall(Nodes, mnesia, lkill, []),
    Config.

init_per_suite(Config) ->
    crypto:start(),
    Config.

end_per_suite(_Config) ->
    crypto:stop().

error(Format, Args, File0, Line) ->
    File = filename:basename(File0),
    Pid = group_leader(),
    Pid ! {failed, File, Line},
    io:format(Pid, "~s(~p): ERROR"++Format, [File,Line|Args]).

warning(Format, Args, File0, Line) ->
    File = filename:basename(File0),
    io:format("~s(~p): Warning "++Format, [File,Line|Args]).
