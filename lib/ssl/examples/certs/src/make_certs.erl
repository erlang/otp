%% The purpose of this module is to create example certificates for
%% testing. 
%% Run it as: 
%%
%% erl -noinput -run make_certs all "/path/to/openssl" -s erlang halt
%%

-module(make_certs).
-export([all/0, all/1]).

-record(dn, {commonName, 
	     organizationalUnitName = "Erlang OTP",
	     organizationName = "Ericsson AB",
	     localityName = "Stockholm",
	     countryName = "SE",
	     emailAddress = "peter@erix.ericsson.se"}).

all() ->
    all(["openssl"]).

all([OpenSSLCmd]) ->
    Root = filename:dirname(filename:dirname((code:which(?MODULE)))),
    %% io:fwrite("Root : ~s~n", [Root]),
    NRoot = filename:join([Root, "etc"]), 
    file:make_dir(NRoot), 
    create_rnd(Root, "etc"),			% For all requests
    rootCA(NRoot, OpenSSLCmd, "erlangCA"),
    intermediateCA(NRoot, OpenSSLCmd, "otpCA", "erlangCA"),
    endusers(NRoot, OpenSSLCmd, "otpCA", ["client", "server"]),
    collect_certs(NRoot, ["erlangCA", "otpCA"], ["client", "server"]),
    remove_rnd(Root, "etc").

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
    CertFile =  filename:join([UsrRoot, "cert.pem"]), 
    sign_req(Root, OpenSSLCmd, CA, "user_cert", ReqFile, CertFile).

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
    cmd(Cmd, Env).

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
    cmd(Cmd, Env).

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

create_rnd(Root, Dir) ->
    From = filename:join([Root, "rnd", "RAND"]),
    To = filename:join([Root, Dir, "RAND"]),
    file:copy(From, To).

remove_rnd(Root, Dir) ->
    File = filename:join([Root, Dir, "RAND"]),
    file:delete(File).

cmd(Cmd, Env) ->
    FCmd = lists:flatten(Cmd),
    Port = open_port({spawn, FCmd}, [stream, eof, exit_status, 
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
	    erlang:halt(Status)
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

     "[ca_cert]\n"
     "basicConstraints 	= critical,CA:true\n"
     "keyUsage 		= cRLSign, keyCertSign\n"
     "subjectKeyIdentifier = hash\n"
     "authorityKeyIdentifier = keyid:always,issuer:always\n"
     "subjectAltName	= email:copy\n"
     "issuerAltName	= issuer:copy\n"].


