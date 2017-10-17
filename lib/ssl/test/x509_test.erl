%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2017. All Rights Reserved.
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

%%

 -module(x509_test).

 -include_lib("public_key/include/public_key.hrl").

-export([extensions/1, gen_pem_config_files/3]).

gen_pem_config_files(#{server_config := ServerConf,
                       client_config := ClientConf}, ClientBase, ServerBase) ->
    
    ServerCaCertFile = ServerBase ++ "_server_cacerts.pem",
    ServerCertFile = ServerBase ++ "_server_cert.pem",
    ServerKeyFile = ServerBase ++ "_server_key.pem",
    
    ClientCaCertFile = ClientBase ++ "_client_cacerts.pem",
    ClientCertFile =  ClientBase ++ "_client_cert.pem",
    ClientKeyFile = ClientBase ++ "_client_key.pem",

    do_gen_pem_config_files(ServerConf,
                            ServerCertFile,
                            ServerKeyFile,
                            ServerCaCertFile),        
    do_gen_pem_config_files(ClientConf,
                            ClientCertFile,
                            ClientKeyFile,
                            ClientCaCertFile),
    [{server_config, [{certfile, ServerCertFile}, 
                      {keyfile, ServerKeyFile}, {cacertfile, ServerCaCertFile}]}, 
     {client_config, [{certfile, ClientCertFile}, 
                      {keyfile, ClientKeyFile}, {cacertfile, ClientCaCertFile}]}].
extensions(Exts) ->
    [extension(Ext) || Ext <- Exts].


do_gen_pem_config_files(Config, CertFile, KeyFile, CAFile) ->
    CAs = proplists:get_value(cacerts, Config),
    Cert = proplists:get_value(cert, Config),
    Key = proplists:get_value(key, Config),
    der_to_pem(CertFile, [cert_entry(Cert)]),
    der_to_pem(KeyFile, [key_entry(Key)]),
    der_to_pem(CAFile, ca_entries(CAs)).

cert_entry(Cert) ->
    {'Certificate', Cert, not_encrypted}.

key_entry({'RSAPrivateKey', DERKey}) ->
    {'RSAPrivateKey', DERKey, not_encrypted};
key_entry({'DSAPrivateKey', DERKey}) ->
    {'DSAPrivateKey', DERKey, not_encrypted};
key_entry({'ECPrivateKey', DERKey}) ->
    {'ECPrivateKey', DERKey, not_encrypted}.

ca_entries(CAs) ->
    [{'Certificate', CACert, not_encrypted} || CACert <- CAs].

extension({_, undefined}) ->
    [];
extension({basic_constraints, Data}) ->
    case Data of
	default ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = #'BasicConstraints'{cA=true},
			 critical=true};
	false -> 
	    [];
	Len when is_integer(Len) ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = #'BasicConstraints'{cA=true, pathLenConstraint = Len},
			 critical = true};
	_ ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = Data}
    end;
extension({key_usage, Value}) ->
    #'Extension'{extnID = ?'id-ce-keyUsage',
                 extnValue = Value,
                 critical = false};
extension({subject_alt, Hostname}) ->
    #'Extension'{extnID = ?'id-ce-subjectAltName',
                 extnValue = [{dNSName, Hostname}],
                 critical = false};
extension({Id, Data, Critical}) ->
    #'Extension'{extnID = Id, extnValue = Data, critical = Critical}.

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).
