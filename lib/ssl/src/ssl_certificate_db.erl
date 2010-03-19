%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Storage for trused certificats 
%%----------------------------------------------------------------------

-module(ssl_certificate_db).

-include_lib("public_key/include/public_key.hrl").

-export([create/0, remove/1, add_trusted_certs/3, 
	 remove_trusted_certs/2, lookup_trusted_cert/3, issuer_candidate/1,
	 lookup_cached_certs/1, cache_pem_file/3]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: create() -> Db
%% Db = term() - Reference to the crated database 
%% 
%% Description: Creates a new certificate db.
%% Note: lookup_trusted_cert/3 may be called from any process but only
%% the process that called create may call the other functions.
%%--------------------------------------------------------------------
create() ->
    [ets:new(certificate_db_name(), [named_table, set, protected]),
     ets:new(ssl_file_to_ref, [named_table, set, protected]),
     ets:new(ssl_pid_to_file, [bag, private])]. 

%%--------------------------------------------------------------------
%% Function: delete(Db) -> _
%% Db = Database refererence as returned by create/0
%%
%% Description: Removes database db  
%%--------------------------------------------------------------------
remove(Dbs) ->
    lists:foreach(fun(Db) -> true = ets:delete(Db) end, Dbs).

%%--------------------------------------------------------------------
%% Function: lookup_trusted_cert(Ref, SerialNumber, Issuer) -> {BinCert,DecodedCert}
%% Ref = ref()
%% SerialNumber = integer()
%% Issuer = {rdnSequence, IssuerAttrs}
%% BinCert = binary()
%%
%% Description: Retrives the trusted certificate identified by 
%% <SerialNumber, Issuer>. Ref is used as it is specified  
%% for each connection which certificates are trusted.
%%--------------------------------------------------------------------
lookup_trusted_cert(Ref, SerialNumber, Issuer) ->
    case lookup({Ref, SerialNumber, Issuer}, certificate_db_name()) of
	undefined ->
	    undefined;
	[Certs] ->
	    {ok, Certs}
    end.

lookup_cached_certs(File) ->
    ets:lookup(certificate_db_name(), {file, File}).

%%--------------------------------------------------------------------
%% Function: add_trusted_certs(Pid, File, Db) -> {ok, Ref}
%% Pid = pid() 
%% File = string()
%% Db = Database refererence as returned by create/0
%% Ref = ref()
%%
%% Description: Adds the trusted certificates from file <File> to the
%% runtime database. Returns Ref that should be handed to lookup_trusted_cert
%% together with the cert serialnumber and issuer.
%%--------------------------------------------------------------------
add_trusted_certs(Pid, File, [CertsDb, FileToRefDb, PidToFileDb]) ->
    Ref = case lookup(File, FileToRefDb) of
	      undefined ->
		  NewRef = make_ref(),
		  add_certs_from_file(File, NewRef, CertsDb),
		  insert(File, NewRef, 1, FileToRefDb),
		  NewRef;
	      [OldRef] ->
		  ref_count(File,FileToRefDb,1),
		  OldRef
	  end,
    insert(Pid, File, PidToFileDb),
    {ok, Ref}.

%%--------------------------------------------------------------------
%% Function: cache_pem_file(Pid, File, Db) -> FileContent
%%
%% Description: Cache file as binary in DB
%%--------------------------------------------------------------------
cache_pem_file(Pid, File, [CertsDb, _FileToRefDb, PidToFileDb]) ->
    Res = {ok, Content} = public_key:pem_to_der(File),
    insert({file, File}, Content, CertsDb),
    insert(Pid, File, PidToFileDb),
    Res.

%%--------------------------------------------------------------------
%% Function: remove_trusted_certs(Pid, Db) -> _ 
%%
%% Description: Removes trusted certs originating from 
%% the file associated to Pid from the runtime database.  
%%--------------------------------------------------------------------
remove_trusted_certs(Pid, [CertsDb, FileToRefDb, PidToFileDb]) ->
    Files = lookup(Pid, PidToFileDb),
    delete(Pid, PidToFileDb),
    Clear = fun(File) ->
		    delete({file,File}, CertsDb),
		    try
			0 = ref_count(File, FileToRefDb, -1),
			case lookup(File, FileToRefDb) of
			    [Ref] when is_reference(Ref) ->
				remove_certs(Ref, CertsDb);
			    _ -> ok
			end,
			delete(File, FileToRefDb)
		    catch _:_ ->
			    ok
		    end
	    end,
    case Files of 
	undefined -> ok;
	_ -> 
	    [Clear(File) || File <- Files],
	    ok
    end.

%%--------------------------------------------------------------------
%% Function: issuer_candidate() -> {Key, Candidate} | no_more_candidates   
%%
%%     Candidate
%%     
%%     
%% Description: If a certificat does not define its issuer through
%%              the extension 'ce-authorityKeyIdentifier' we can
%%              try to find the issuer in the database over known
%%              certificates.
%%--------------------------------------------------------------------
issuer_candidate(no_candidate) ->
    Db = certificate_db_name(),
    case ets:first(Db) of
 	'$end_of_table' ->
 	    no_more_candidates;
	{file, _} = Key ->
	    issuer_candidate(Key);
 	Key ->
	    [Cert] = lookup(Key, Db),
 	    {Key, Cert}
    end;

issuer_candidate(PrevCandidateKey) ->	    
    Db = certificate_db_name(),
    case ets:next(Db, PrevCandidateKey) of
 	'$end_of_table' ->
 	    no_more_candidates;
	{file, _} = Key ->
	    issuer_candidate(Key);
 	Key ->
	    [Cert] = lookup(Key, Db),
 	    {Key, Cert}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
certificate_db_name() ->
    ssl_otp_certificate_db.

insert(Key, Data, Db) ->
    true = ets:insert(Db, {Key, Data}).

insert(Key, Data, Count, Db) ->
    true = ets:insert(Db, {Key, Count, Data}).

ref_count(Key, Db,N) ->
    ets:update_counter(Db,Key,N).

delete(Key, Db) ->
    _ = ets:delete(Db, Key).

lookup(Key, Db) ->
    case ets:lookup(Db, Key) of
	[] ->
	    undefined;
	Contents  ->
	    Pick = fun({_, Data}) -> Data;
		      ({_,_,Data}) -> Data
		   end,
	    [Pick(Data) || Data <- Contents]
    end.

remove_certs(Ref, CertsDb) ->
    ets:match_delete(CertsDb, {{Ref, '_', '_'}, '_'}).

add_certs_from_file(File, Ref, CertsDb) ->   
    Decode = fun(Cert) ->
		     {ok, ErlCert} = public_key:pkix_decode_cert(Cert, otp),
		     TBSCertificate = ErlCert#'OTPCertificate'.tbsCertificate,
		     SerialNumber = TBSCertificate#'OTPTBSCertificate'.serialNumber,
		     Issuer = public_key:pkix_normalize_general_name(
				TBSCertificate#'OTPTBSCertificate'.issuer),
		     insert({Ref, SerialNumber, Issuer}, {Cert,ErlCert}, CertsDb)
	     end,
    {ok,Der} = public_key:pem_to_der(File),
    [Decode(Cert) || {cert, Cert, not_encrypted} <- Der].
    
