%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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

-module(make_certs).
-compile([export_all]).

%-export([all/1, all/2, rootCA/2, intermediateCA/3, endusers/3, enduser/3, revoke/3, gencrl/2, verify/3]).

-record(config, {commonName, 
	     organizationalUnitName = "Erlang OTP",
	     organizationName = "Ericsson AB",
	     localityName = "Stockholm",
	     countryName = "SE",
	     emailAddress = "peter@erix.ericsson.se",
	     default_bits = 2048,
	     v2_crls = true,
	     ecc_certs = false,
	     issuing_distribution_point = false,
	     crl_port = 8000,
	     openssl_cmd = "openssl"}).


default_config() ->
    #config{}.

make_config(Args) ->
    make_config(Args, #config{}).

make_config([], C) ->
    C;
make_config([{organizationalUnitName, Name}|T], C) when is_list(Name) ->
    make_config(T, C#config{organizationalUnitName = Name});
make_config([{organizationName, Name}|T], C) when is_list(Name) ->
    make_config(T, C#config{organizationName = Name});
make_config([{localityName, Name}|T], C) when is_list(Name) ->
    make_config(T, C#config{localityName = Name});
make_config([{countryName, Name}|T], C) when is_list(Name) ->
    make_config(T, C#config{countryName = Name});
make_config([{emailAddress, Name}|T], C) when is_list(Name) ->
    make_config(T, C#config{emailAddress = Name});
make_config([{default_bits, Bits}|T], C) when is_integer(Bits) ->
    make_config(T, C#config{default_bits = Bits});
make_config([{v2_crls, Bool}|T], C) when is_boolean(Bool) ->
    make_config(T, C#config{v2_crls = Bool});
make_config([{crl_port, Port}|T], C) when is_integer(Port) ->
    make_config(T, C#config{crl_port = Port});
make_config([{ecc_certs, Bool}|T], C) when is_boolean(Bool) ->
    make_config(T, C#config{ecc_certs = Bool});
make_config([{issuing_distribution_point, Bool}|T], C) when is_boolean(Bool) ->
    make_config(T, C#config{issuing_distribution_point = Bool});
make_config([{openssl_cmd, Cmd}|T], C) when is_list(Cmd) ->
    make_config(T, C#config{openssl_cmd = Cmd}).


all([DataDir, PrivDir]) ->
    all(DataDir, PrivDir).

all(DataDir, PrivDir) ->
    all(DataDir, PrivDir, #config{}).

all(DataDir, PrivDir, C) when is_list(C) ->
    all(DataDir, PrivDir, make_config(C));
all(DataDir, PrivDir, C = #config{}) ->
    ok = filelib:ensure_dir(filename:join(PrivDir, "erlangCA")),
    create_rnd(DataDir, PrivDir),			% For all requests
    rootCA(PrivDir, "erlangCA", C),
    intermediateCA(PrivDir, "otpCA", "erlangCA", C),
    endusers(PrivDir, "otpCA", ["client", "server", "revoked", "a.server", "b.server"], C),
    endusers(PrivDir, "erlangCA", ["localhost"], C),
    %% Create keycert files 
    SDir = filename:join([PrivDir, "server"]),
    SC = filename:join([SDir, "cert.pem"]),
    SK = filename:join([SDir, "key.pem"]),
    SKC = filename:join([SDir, "keycert.pem"]),
    append_files([SK, SC], SKC),
    CDir = filename:join([PrivDir, "client"]),
    CC = filename:join([CDir, "cert.pem"]),
    CK = filename:join([CDir, "key.pem"]),
    CKC = filename:join([CDir, "keycert.pem"]),
    append_files([CK, CC], CKC),
    RDir = filename:join([PrivDir, "revoked"]),
    RC = filename:join([RDir, "cert.pem"]),
    RK = filename:join([RDir, "key.pem"]),
    RKC = filename:join([RDir, "keycert.pem"]),
    revoke(PrivDir, "otpCA", "revoked", C),
    append_files([RK, RC], RKC),
    remove_rnd(PrivDir),
    {ok, C}.

append_files(FileNames, ResultFileName) ->
    {ok, ResultFile} = file:open(ResultFileName, [write]),
    do_append_files(FileNames, ResultFile).

do_append_files([], RF) ->
    ok = file:close(RF);
do_append_files([F|Fs], RF) ->
    {ok, Data} = file:read_file(F),
    ok = file:write(RF, Data),
    do_append_files(Fs, RF).

rootCA(Root, Name, C) ->
    create_ca_dir(Root, Name, ca_cnf(Root, C#config{commonName = Name})),
    create_self_signed_cert(Root, Name, req_cnf(Root, C#config{commonName = Name}), C),
    file:copy(filename:join([Root, Name, "cert.pem"]), filename:join([Root, Name, "cacerts.pem"])),
    gencrl(Root, Name, C).

intermediateCA(Root, CA, ParentCA, C) ->
    create_ca_dir(Root, CA, ca_cnf(Root, C#config{commonName = CA})),
    CARoot = filename:join([Root, CA]),
    CnfFile = filename:join([CARoot, "req.cnf"]),
    file:write_file(CnfFile, req_cnf(Root, C#config{commonName = CA})),
    KeyFile = filename:join([CARoot, "private", "key.pem"]), 
    ReqFile =  filename:join([CARoot, "req.pem"]), 
    create_req(Root, CnfFile, KeyFile, ReqFile, C),
    CertFile = filename:join([CARoot, "cert.pem"]),
    sign_req(Root, ParentCA, "ca_cert", ReqFile, CertFile, C),
    CACertsFile = filename:join(CARoot, "cacerts.pem"),
    file:copy(filename:join([Root, ParentCA, "cacerts.pem"]), CACertsFile),
    %% append this CA's cert to the cacerts file
    {ok, Bin} = file:read_file(CertFile),
    {ok, FD} = file:open(CACertsFile, [append]),
    file:write(FD, ["\n", Bin]),
    file:close(FD),
    gencrl(Root, CA, C).

endusers(Root, CA, Users, C) ->
    [enduser(Root, CA, User, C) || User <- Users].

enduser(Root, CA, User, C) ->
    UsrRoot = filename:join([Root, User]),
    file:make_dir(UsrRoot),
    CnfFile = filename:join([UsrRoot, "req.cnf"]),
    file:write_file(CnfFile, req_cnf(Root, C#config{commonName = User})),
    KeyFile = filename:join([UsrRoot, "key.pem"]), 
    ReqFile =  filename:join([UsrRoot, "req.pem"]), 
    create_req(Root, CnfFile, KeyFile, ReqFile, C),
    %create_req(Root, CnfFile, KeyFile, ReqFile),
    CertFileAllUsage =  filename:join([UsrRoot, "cert.pem"]),
    sign_req(Root, CA, "user_cert", ReqFile, CertFileAllUsage, C),
    CertFileDigitalSigOnly =  filename:join([UsrRoot, "digital_signature_only_cert.pem"]),
    sign_req(Root, CA, "user_cert_digital_signature_only", ReqFile, CertFileDigitalSigOnly, C),
    CACertsFile = filename:join(UsrRoot, "cacerts.pem"),
    file:copy(filename:join([Root, CA, "cacerts.pem"]), CACertsFile),
    ok.

revoke(Root, CA, User, C) ->
    UsrCert = filename:join([Root, User, "cert.pem"]),
    CACnfFile = filename:join([Root, CA, "ca.cnf"]),
    Cmd = [C#config.openssl_cmd, " ca"
	   " -revoke ", UsrCert,
	   [" -crl_reason keyCompromise" || C#config.v2_crls ],
	   " -config ", CACnfFile],
    Env = [{"ROOTDIR", filename:absname(Root)}], 
    cmd(Cmd, Env),
    gencrl(Root, CA, C).

gencrl(Root, CA, C) ->
    %% By default, the CRL is valid for a week from now.
    gencrl(Root, CA, C, 24*7).

gencrl(Root, CA, C, CrlHours) ->
    CACnfFile = filename:join([Root, CA, "ca.cnf"]),
    CACRLFile = filename:join([Root, CA, "crl.pem"]),
    Cmd = [C#config.openssl_cmd, " ca"
	   " -gencrl ",
	   " -crlhours ", integer_to_list(CrlHours),
	   " -out ", CACRLFile,
	   " -config ", CACnfFile],
    Env = [{"ROOTDIR", filename:absname(Root)}], 
    cmd(Cmd, Env).

can_generate_expired_crls(C) ->
    %% OpenSSL can generate CRLs with an expiration date in the past,
    %% if we pass a negative number for -crlhours.  However, LibreSSL
    %% rejects this with the error "invalid argument -24: too small".
    %% Let's check which one we have.
    Cmd = [C#config.openssl_cmd, " ca -crlhours -24"],
    Output = os:cmd(Cmd),
    0 =:= string:str(Output, "too small").

verify(Root, CA, User, C) ->
    CAFile = filename:join([Root, User, "cacerts.pem"]),
    CACRLFile = filename:join([Root, CA, "crl.pem"]),
    CertFile = filename:join([Root, User, "cert.pem"]),
    Cmd = [C#config.openssl_cmd, " verify"
	   " -CAfile ", CAFile,
	   " -CRLfile ", CACRLFile, %% this is undocumented, but seems to work
	   " -crl_check ",
	   CertFile],
    Env = [{"ROOTDIR", filename:absname(Root)}],
    try cmd(Cmd, Env) catch
	   exit:{eval_cmd, _, _} ->
		invalid
    end.

create_self_signed_cert(Root, CAName, Cnf, C = #config{ecc_certs = true}) ->
    CARoot = filename:join([Root, CAName]),
    CnfFile = filename:join([CARoot, "req.cnf"]),
    file:write_file(CnfFile, Cnf),
    KeyFile = filename:join([CARoot, "private", "key.pem"]), 
    CertFile = filename:join([CARoot, "cert.pem"]), 
    Cmd = [C#config.openssl_cmd, " ecparam"
	   " -out ", KeyFile,
	   " -name secp521r1 ",
	   %" -name sect283k1 ",
	   " -genkey "],
    Env = [{"ROOTDIR", filename:absname(Root)}], 
    cmd(Cmd, Env),

    Cmd2 = [C#config.openssl_cmd, " req"
	   " -new"
	   " -x509"
	   " -config ", CnfFile,
	   " -key ", KeyFile, 
		 " -outform PEM ",
	   " -out ", CertFile], 
    cmd(Cmd2, Env);
create_self_signed_cert(Root, CAName, Cnf, C) ->
    CARoot = filename:join([Root, CAName]),
    CnfFile = filename:join([CARoot, "req.cnf"]),
    file:write_file(CnfFile, Cnf),
    KeyFile = filename:join([CARoot, "private", "key.pem"]), 
    CertFile = filename:join([CARoot, "cert.pem"]), 
    Cmd = [C#config.openssl_cmd, " req"
	   " -new"
	   " -x509"
	   " -config ", CnfFile,
	   " -keyout ", KeyFile,
	   " -outform PEM",
	   " -out ", CertFile], 
    Env = [{"ROOTDIR", filename:absname(Root)}],  
    cmd(Cmd, Env).


create_ca_dir(Root, CAName, Cnf) ->
    CARoot = filename:join([Root, CAName]),
    ok = filelib:ensure_dir(CARoot),
    file:make_dir(CARoot),
    create_dirs(CARoot, ["certs", "crl", "newcerts", "private"]),
    create_rnd(Root, filename:join([CAName, "private"])),
    create_files(CARoot, [{"serial", "01\n"},
			  {"crlnumber", "01"},
			  {"index.txt", ""},
			  {"ca.cnf", Cnf}]).

create_req(Root, CnfFile, KeyFile, ReqFile, C = #config{ecc_certs = true}) ->
    Cmd = [C#config.openssl_cmd, " ecparam"
	   " -out ", KeyFile,
	   " -name secp521r1 ",
	   %" -name sect283k1 ",
	   " -genkey "],
    Env = [{"ROOTDIR", filename:absname(Root)}], 
    cmd(Cmd, Env),
    Cmd2 = [C#config.openssl_cmd, " req"
	   " -new ",
	   " -key ", KeyFile,
	   " -outform PEM ",
	   " -out ", ReqFile,
	   " -config ", CnfFile],
    cmd(Cmd2, Env);
    %fix_key_file(KeyFile).
create_req(Root, CnfFile, KeyFile, ReqFile, C) ->
    Cmd = [C#config.openssl_cmd, " req"
	   " -new"
	   " -config ", CnfFile,
	   " -outform PEM ",
	   " -keyout ", KeyFile, 
	   " -out ", ReqFile], 
    Env = [{"ROOTDIR", filename:absname(Root)}], 
    cmd(Cmd, Env).
    %fix_key_file(KeyFile).


sign_req(Root, CA, CertType, ReqFile, CertFile, C) ->
    CACnfFile = filename:join([Root, CA, "ca.cnf"]),
    Cmd = [C#config.openssl_cmd, " ca"
	   " -batch"
	   " -notext"
	   " -config ", CACnfFile, 
	   " -extensions ", CertType,
	   " -in ", ReqFile, 
	   " -out ", CertFile],
    Env = [{"ROOTDIR", filename:absname(Root)}], 
    cmd(Cmd, Env).
    
%%
%%  Misc
%%
    
create_dirs(Root, Dirs) ->
    lists:foreach(fun(Dir) ->
			  file:make_dir(filename:join([Root, Dir])) end,
		  Dirs).

create_files(Root, NameContents) ->
    lists:foreach(
      fun({Name, Contents}) ->
	      file:write_file(filename:join([Root, Name]), Contents) end,
      NameContents).

create_rnd(FromDir, ToDir) ->
     From = filename:join([FromDir, "RAND"]),
     To = filename:join([ToDir, "RAND"]),
     file:copy(From, To).

remove_rnd(Dir) ->
    File = filename:join([Dir, "RAND"]),
    file:delete(File).

cmd(Cmd, Env) ->
    FCmd = lists:flatten(Cmd),
    Port = open_port({spawn, FCmd}, [stream, eof, exit_status, stderr_to_stdout, 
				    {env, Env}]),
    eval_cmd(Port, FCmd).

eval_cmd(Port, Cmd) ->
    receive 
	{Port, {data, _}} ->
	    eval_cmd(Port, Cmd);
	{Port, eof} ->
	    ok
    end,
    receive
	{Port, {exit_status, 0}}  ->
	    ok;
	{Port, {exit_status, Status}} ->
	    exit({eval_cmd, Cmd, Status})
    after 0 ->
	    ok
    end.

%%
%% Contents of configuration files 
%%

req_cnf(Root, C) ->
    ["# Purpose: Configuration for requests (end users and CAs)."
     "\n"
     "ROOTDIR	        = " ++ Root ++ "\n"
     "\n"

     "[req]\n"
     "input_password	= secret\n"
     "output_password	= secret\n"
     "default_bits	= ", integer_to_list(C#config.default_bits), "\n"
     "RANDFILE		= $ROOTDIR/RAND\n"
     "encrypt_key	= no\n"
     "default_md	= md5\n"
     "#string_mask	= pkix\n"
     "x509_extensions	= ca_ext\n"
     "prompt		= no\n"
     "distinguished_name= name\n"
     "\n"

     "[name]\n"
     "commonName		= ", C#config.commonName, "\n"
     "organizationalUnitName	= ", C#config.organizationalUnitName, "\n"
     "organizationName	        = ", C#config.organizationName, "\n" 
     "localityName		= ", C#config.localityName, "\n"
     "countryName		= ", C#config.countryName, "\n"
     "emailAddress		= ", C#config.emailAddress, "\n"
     "\n"

     "[ca_ext]\n"
     "basicConstraints 	= critical, CA:true\n"
     "keyUsage 		= cRLSign, keyCertSign\n"
     "subjectKeyIdentifier = hash\n"
     "subjectAltName	= email:copy\n"].

ca_cnf(Root, C = #config{issuing_distribution_point = true}) ->
    Hostname = net_adm:localhost(),		    
    ["# Purpose: Configuration for CAs.\n"
     "\n"
     "ROOTDIR	       = " ++ Root ++ "\n"
     "default_ca	= ca\n"
     "\n"

     "[ca]\n"
     "dir		= $ROOTDIR/", C#config.commonName, "\n"
     "certs		= $dir/certs\n"
     "crl_dir	        = $dir/crl\n"
     "database	        = $dir/index.txt\n"
     "new_certs_dir	= $dir/newcerts\n"
     "certificate	= $dir/cert.pem\n"
     "serial		= $dir/serial\n"
     "crl		= $dir/crl.pem\n",
     ["crlnumber		= $dir/crlnumber\n" || C#config.v2_crls],
     "private_key	= $dir/private/key.pem\n"
     "RANDFILE	        = $dir/private/RAND\n"
     "\n"
     "x509_extensions   = user_cert\n",
     ["crl_extensions = crl_ext\n" || C#config.v2_crls],
     "unique_subject  = no\n"
     "default_days	= 3600\n"
     "default_md	= md5\n"
     "preserve	        = no\n"
     "policy		= policy_match\n"
     "\n"

     "[policy_match]\n"
     "commonName		= supplied\n"
     "organizationalUnitName	= optional\n"
     "organizationName	        = match\n"
     "countryName		= match\n"
     "localityName		= match\n"
     "emailAddress		= supplied\n"
     "\n"

     "[crl_ext]\n"
     "authorityKeyIdentifier=keyid:always,issuer:always\n",
     ["issuingDistributionPoint=critical, @idpsec\n" || C#config.issuing_distribution_point],

     "[idpsec]\n"
     "fullname=URI:http://localhost:8000/",C#config.commonName,"/crl.pem\n"

     "[user_cert]\n"
     "basicConstraints	= CA:false\n"
     "keyUsage 		= nonRepudiation, digitalSignature, keyEncipherment\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid,issuer:always\n"
     "subjectAltName	= DNS.1:" ++ Hostname ++ "\n"
     "issuerAltName	= issuer:copy\n"
     "crlDistributionPoints=@crl_section\n"

     "[crl_section]\n"
     %% intentionally invalid
     "URI.1=http://localhost/",C#config.commonName,"/crl.pem\n"
     "URI.2=http://localhost:",integer_to_list(C#config.crl_port),"/",C#config.commonName,"/crl.pem\n"
     "\n"

     "[user_cert_digital_signature_only]\n"
     "basicConstraints	= CA:false\n"
     "keyUsage 		= digitalSignature\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid,issuer:always\n"
     "subjectAltName	= DNS.1:" ++ Hostname ++ "\n"
     "issuerAltName	= issuer:copy\n"
     "\n"

     "[ca_cert]\n"
     "basicConstraints 	= critical,CA:true\n"
     "keyUsage 		= cRLSign, keyCertSign\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid:always,issuer:always\n"
     "subjectAltName	= DNS.1:" ++ Hostname ++ "\n"
     "issuerAltName	= issuer:copy\n"
     "crlDistributionPoints=@crl_section\n"
    ];

ca_cnf(Root, C = #config{issuing_distribution_point = false}) ->
  Hostname = net_adm:localhost(),	     
    ["# Purpose: Configuration for CAs.\n"
     "\n"
     "ROOTDIR	          = " ++ Root ++ "\n"
     "default_ca	= ca\n"
     "\n"

     "[ca]\n"
     "dir		= $ROOTDIR/", C#config.commonName, "\n"
     "certs		= $dir/certs\n"
     "crl_dir	        = $dir/crl\n"
     "database	        = $dir/index.txt\n"
     "new_certs_dir	= $dir/newcerts\n"
     "certificate	= $dir/cert.pem\n"
     "serial		= $dir/serial\n"
     "crl		= $dir/crl.pem\n",
     ["crlnumber		= $dir/crlnumber\n" || C#config.v2_crls],
     "private_key	= $dir/private/key.pem\n"
     "RANDFILE	        = $dir/private/RAND\n"
     "\n"
     "x509_extensions   = user_cert\n",
     ["crl_extensions = crl_ext\n" || C#config.v2_crls],
     "unique_subject  = no\n"
     "default_days	= 3600\n"
     "default_md	= md5\n"
     "preserve	        = no\n"
     "policy		= policy_match\n"
     "\n"

     "[policy_match]\n"
     "commonName		= supplied\n"
     "organizationalUnitName	= optional\n"
     "organizationName	        = match\n"
     "countryName		= match\n"
     "localityName		= match\n"
     "emailAddress		= supplied\n"
     "\n"

     "[crl_ext]\n"
     "authorityKeyIdentifier=keyid:always,issuer:always\n",
     %["issuingDistributionPoint=critical, @idpsec\n" || C#config.issuing_distribution_point],

     %"[idpsec]\n"
     %"fullname=URI:http://localhost:8000/",C#config.commonName,"/crl.pem\n"

     "[user_cert]\n"
     "basicConstraints	= CA:false\n"
     "keyUsage 		= nonRepudiation, digitalSignature, keyEncipherment\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid,issuer:always\n"
     "subjectAltName	= DNS.1:" ++ Hostname ++ "\n"
     "issuerAltName	= issuer:copy\n"
     %"crlDistributionPoints=@crl_section\n"

     %%"[crl_section]\n"
     %% intentionally invalid
     %%"URI.1=http://localhost/",C#config.commonName,"/crl.pem\n"
     %%"URI.2=http://localhost:",integer_to_list(C#config.crl_port),"/",C#config.commonName,"/crl.pem\n"
     %%"\n"

     "[user_cert_digital_signature_only]\n"
     "basicConstraints	= CA:false\n"
     "keyUsage 		= digitalSignature\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid,issuer:always\n"
     "subjectAltName	= DNS.1:" ++ Hostname ++ "\n"
     "issuerAltName	= issuer:copy\n"
     "\n"

     "[ca_cert]\n"
     "basicConstraints 	= critical,CA:true\n"
     "keyUsage 		= cRLSign, keyCertSign\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid:always,issuer:always\n"
     "subjectAltName	= email:copy\n"
     "issuerAltName	= issuer:copy\n"
     %"crlDistributionPoints=@crl_section\n"
    ].
