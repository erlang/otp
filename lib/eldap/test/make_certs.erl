%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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

-module(make_certs).

-export([all/2]).

-record(dn, {commonName,
	     organizationalUnitName = "Erlang OTP",
	     organizationName = "Ericsson AB",
	     localityName = "Stockholm",
	     countryName = "SE",
	     emailAddress = "peter@erix.ericsson.se"}).

all(DataDir, PrivDir) ->
    OpenSSLCmd = "openssl",
    create_rnd(DataDir, PrivDir),			% For all requests
    rootCA(PrivDir, OpenSSLCmd, "erlangCA"),
    intermediateCA(PrivDir, OpenSSLCmd, "otpCA", "erlangCA"),
    endusers(PrivDir, OpenSSLCmd, "otpCA", ["client", "server"]),
    collect_certs(PrivDir, ["erlangCA", "otpCA"], ["client", "server"]),
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
    remove_rnd(PrivDir).

append_files(FileNames, ResultFileName) ->
    {ok, ResultFile} = file:open(ResultFileName, [write]),
    do_append_files(FileNames, ResultFile).

do_append_files([], RF) ->
    ok = file:close(RF);
do_append_files([F|Fs], RF) ->
    {ok, Data} = file:read_file(F),
    ok = file:write(RF, Data),
    do_append_files(Fs, RF).

rootCA(Root, OpenSSLCmd, Name) ->
    create_ca_dir(Root, Name, ca_cnf(Name)),
    DN = #dn{commonName = Name},
    create_self_signed_cert(Root, OpenSSLCmd, Name, req_cnf(DN)),
    ok.

intermediateCA(Root, OpenSSLCmd, CA, ParentCA) ->
    CA = "otpCA",
    create_ca_dir(Root, CA, ca_cnf(CA)),
    CARoot = filename:join([Root, CA]),
    DN = #dn{commonName = CA},
    CnfFile = filename:join([CARoot, "req.cnf"]),
    file:write_file(CnfFile, req_cnf(DN)),
    KeyFile = filename:join([CARoot, "private", "key.pem"]),
    ReqFile =  filename:join([CARoot, "req.pem"]),
    create_req(Root, OpenSSLCmd, CnfFile, KeyFile, ReqFile),
    CertFile = filename:join([CARoot, "cert.pem"]),
    sign_req(Root, OpenSSLCmd, ParentCA, "ca_cert", ReqFile, CertFile).

endusers(Root, OpenSSLCmd, CA, Users) ->
    lists:foreach(fun(User) -> enduser(Root, OpenSSLCmd, CA, User) end, Users).

enduser(Root, OpenSSLCmd, CA, User) ->
    UsrRoot = filename:join([Root, User]),
    file:make_dir(UsrRoot),
    CnfFile = filename:join([UsrRoot, "req.cnf"]),
    DN = #dn{commonName = User},
    file:write_file(CnfFile, req_cnf(DN)),
    KeyFile = filename:join([UsrRoot, "key.pem"]),
    ReqFile =  filename:join([UsrRoot, "req.pem"]),
    create_req(Root, OpenSSLCmd, CnfFile, KeyFile, ReqFile),
    CertFileAllUsage =  filename:join([UsrRoot, "cert.pem"]),
    sign_req(Root, OpenSSLCmd, CA, "user_cert", ReqFile, CertFileAllUsage),
    CertFileDigitalSigOnly =  filename:join([UsrRoot, "digital_signature_only_cert.pem"]),
    sign_req(Root, OpenSSLCmd, CA, "user_cert_digital_signature_only", ReqFile, CertFileDigitalSigOnly).

collect_certs(Root, CAs, Users) ->
    Bins = lists:foldr(
	     fun(CA, Acc) ->
		     File = filename:join([Root, CA, "cert.pem"]),
		     {ok, Bin} = file:read_file(File),
		     [Bin, "\n" | Acc]
	     end, [], CAs),
    lists:foreach(
      fun(User) ->
	      File = filename:join([Root, User, "cacerts.pem"]),
	      file:write_file(File, Bins)
      end, Users).

create_self_signed_cert(Root, OpenSSLCmd, CAName, Cnf) ->
    CARoot = filename:join([Root, CAName]),
    CnfFile = filename:join([CARoot, "req.cnf"]),
    file:write_file(CnfFile, Cnf),
    KeyFile = filename:join([CARoot, "private", "key.pem"]),
    CertFile = filename:join([CARoot, "cert.pem"]),
    Cmd = [OpenSSLCmd, " req"
	   " -new"
	   " -x509"
	   " -config ", CnfFile,
	   " -keyout ", KeyFile,
	   " -out ", CertFile],
    Env = [{"ROOTDIR", Root}],
    cmd(Cmd, Env),
    fix_key_file(OpenSSLCmd, KeyFile).

% openssl 1.0 generates key files in pkcs8 format by default and we don't handle this format
fix_key_file(OpenSSLCmd, KeyFile) ->
    KeyFileTmp = KeyFile ++ ".tmp",
    Cmd = [OpenSSLCmd, " rsa",
           " -in ",
           KeyFile,
           " -out ",
           KeyFileTmp],
    cmd(Cmd, []),
    ok = file:rename(KeyFileTmp, KeyFile).

create_ca_dir(Root, CAName, Cnf) ->
    CARoot = filename:join([Root, CAName]),
    file:make_dir(CARoot),
    create_dirs(CARoot, ["certs", "crl", "newcerts", "private"]),
    create_rnd(Root, filename:join([CAName, "private"])),
    create_files(CARoot, [{"serial", "01\n"},
			  {"index.txt", ""},
			  {"ca.cnf", Cnf}]).

create_req(Root, OpenSSLCmd, CnfFile, KeyFile, ReqFile) ->
    Cmd = [OpenSSLCmd, " req"
	   " -new"
	   " -config ", CnfFile,
	   " -keyout ", KeyFile,
	   " -out ", ReqFile],
    Env = [{"ROOTDIR", Root}],
    cmd(Cmd, Env),
    fix_key_file(OpenSSLCmd, KeyFile).

sign_req(Root, OpenSSLCmd, CA, CertType, ReqFile, CertFile) ->
    CACnfFile = filename:join([Root, CA, "ca.cnf"]),
    Cmd = [OpenSSLCmd, " ca"
	   " -batch"
	   " -notext"
	   " -config ", CACnfFile,
	   " -extensions ", CertType,
	   " -in ", ReqFile,
	   " -out ", CertFile],
    Env = [{"ROOTDIR", Root}],
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
    eval_cmd(Port).

eval_cmd(Port) ->
    receive
	{Port, {data, _}} ->
	    eval_cmd(Port);
	{Port, eof} ->
	    ok
    end,
    receive
	{Port, {exit_status, Status}} when Status /= 0 ->
	    %% io:fwrite("exit status: ~w~n", [Status]),
	    exit({eval_cmd, Status})
    after 0 ->
	    ok
    end.

%%
%% Contents of configuration files
%%

req_cnf(DN) ->
    ["# Purpose: Configuration for requests (end users and CAs)."
     "\n"
     "ROOTDIR	        = $ENV::ROOTDIR\n"
     "\n"

     "[req]\n"
     "input_password	= secret\n"
     "output_password	= secret\n"
     "default_bits	= 1024\n"
     "RANDFILE		= $ROOTDIR/RAND\n"
     "encrypt_key	= no\n"
     "default_md	= sha1\n"
     "#string_mask	= pkix\n"
     "x509_extensions	= ca_ext\n"
     "prompt		= no\n"
     "distinguished_name= name\n"
     "\n"

     "[name]\n"
     "commonName		= ", DN#dn.commonName, "\n"
     "organizationalUnitName	= ", DN#dn.organizationalUnitName, "\n"
     "organizationName	        = ", DN#dn.organizationName, "\n"
     "localityName		= ", DN#dn.localityName, "\n"
     "countryName		= ", DN#dn.countryName, "\n"
     "emailAddress		= ", DN#dn.emailAddress, "\n"
     "\n"

     "[ca_ext]\n"
     "basicConstraints 	= critical, CA:true\n"
     "keyUsage 		= cRLSign, keyCertSign\n"
     "subjectKeyIdentifier = hash\n"
     "subjectAltName	= email:copy\n"].


ca_cnf(CA) ->
    ["# Purpose: Configuration for CAs.\n"
     "\n"
     "ROOTDIR	        = $ENV::ROOTDIR\n"
     "default_ca	= ca\n"
     "\n"

     "[ca]\n"
     "dir		= $ROOTDIR/", CA, "\n"
     "certs		= $dir/certs\n"
     "crl_dir	        = $dir/crl\n"
     "database	        = $dir/index.txt\n"
     "new_certs_dir	= $dir/newcerts\n"
     "certificate	= $dir/cert.pem\n"
     "serial		= $dir/serial\n"
     "crl		= $dir/crl.pem\n"
     "private_key	= $dir/private/key.pem\n"
     "RANDFILE	        = $dir/private/RAND\n"
     "\n"
     "x509_extensions   = user_cert\n"
     "unique_subject  = no\n"
     "default_days	= 3600\n"
     "default_md	= sha1\n"
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

     "[user_cert]\n"
     "basicConstraints	= CA:false\n"
     "keyUsage 		= nonRepudiation, digitalSignature, keyEncipherment\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid,issuer:always\n"
     "subjectAltName	= email:copy\n"
     "issuerAltName	= issuer:copy\n"
     "\n"

     "[user_cert_digital_signature_only]\n"
     "basicConstraints	= CA:false\n"
     "keyUsage 		= digitalSignature\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid,issuer:always\n"
     "subjectAltName	= email:copy\n"
     "issuerAltName	= issuer:copy\n"
     "\n"

     "[ca_cert]\n"
     "basicConstraints 	= critical,CA:true\n"
     "keyUsage 		= cRLSign, keyCertSign\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid:always,issuer:always\n"
     "subjectAltName	= email:copy\n"
     "issuerAltName	= issuer:copy\n"].
