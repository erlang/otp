%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2015. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Storage for trusted certificates 
%%----------------------------------------------------------------------

-module(ssl_pkix_db).

-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

-export([create/0, add_crls/3, remove_crls/2, remove/1, add_trusted_certs/3, 
	 remove_trusted_certs/2, insert/3, remove/2, clear/1, db_size/1,
	 ref_count/3, lookup_trusted_cert/4, foldl/3, select_cert_by_issuer/2,
	 lookup_cached_pem/2, cache_pem_file/2, cache_pem_file/3,
	 lookup/2]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec create() -> [db_handle(),...].
%% 
%% Description: Creates a new certificate db.
%% Note: lookup_trusted_cert/4 may be called from any process but only
%% the process that called create may call the other functions.
%%--------------------------------------------------------------------
create() ->
    [%% Let connection process delete trusted certs
     %% that can only belong to one connection. (Supplied directly
     %% on DER format to ssl:connect/listen.)
     ets:new(ssl_otp_cacertificate_db, [set, public]),
     %% Let connection processes call ref_count/3 directly
     ets:new(ssl_otp_ca_file_ref, [set, public]),
     ets:new(ssl_otp_pem_cache, [set, protected]),
     %% Default cache
     {ets:new(ssl_otp_crl_cache, [set, protected]),
      ets:new(ssl_otp_crl_issuer_mapping, [bag, protected])}
    ].

%%--------------------------------------------------------------------
-spec remove([db_handle()]) -> ok. 
%%
%% Description: Removes database db  
%%--------------------------------------------------------------------
remove(Dbs) ->
    lists:foreach(fun({Db0, Db1})  ->
			  true = ets:delete(Db0),
			  true = ets:delete(Db1);
		     (undefined) -> 
			  ok;
		     (Db) ->
			  true = ets:delete(Db)
		  end, Dbs).

%%--------------------------------------------------------------------
-spec lookup_trusted_cert(db_handle(), certdb_ref(), serialnumber(), issuer()) ->
				 undefined | {ok, {der_cert(), #'OTPCertificate'{}}}.

%%
%% Description: Retrives the trusted certificate identified by 
%% <SerialNumber, Issuer>. Ref is used as it is specified  
%% for each connection which certificates are trusted.
%%--------------------------------------------------------------------
lookup_trusted_cert(DbHandle, Ref, SerialNumber, Issuer) ->
    case lookup({Ref, SerialNumber, Issuer}, DbHandle) of
	undefined ->
	    undefined;
	[Certs] ->
	    {ok, Certs}
    end.

lookup_cached_pem([_, _, PemChache | _], File) ->
    lookup_cached_pem(PemChache, File);
lookup_cached_pem(PemChache, File) ->
    lookup(File, PemChache).

%%--------------------------------------------------------------------
-spec add_trusted_certs(pid(), {erlang:timestamp(), string()} |
			{der, list()}, [db_handle()]) -> {ok, [db_handle()]}.
%%
%% Description: Adds the trusted certificates from file <File> to the
%% runtime database. Returns Ref that should be handed to lookup_trusted_cert
%% together with the cert serialnumber and issuer.
%%--------------------------------------------------------------------
add_trusted_certs(_Pid, {der, DerList}, [CertDb, _,_ | _]) ->
    NewRef = make_ref(),
    add_certs_from_der(DerList, NewRef, CertDb),
    {ok, NewRef};

add_trusted_certs(_Pid, File, [CertsDb, RefDb, PemChache | _] = Db) ->
    case lookup_cached_pem(Db, File) of
	[{_Content, Ref}] ->
	    ref_count(Ref, RefDb, 1),
	    {ok, Ref};
	[Content] ->
	    Ref = make_ref(),
	    update_counter(Ref, 1, RefDb),
	    insert(File, {Content, Ref}, PemChache),
	    add_certs_from_pem(Content, Ref, CertsDb),
	    {ok, Ref};
	undefined ->
	    new_trusted_cert_entry(File, Db)
    end.
%%--------------------------------------------------------------------
%%
%% Description: Cache file as binary in DB
%%--------------------------------------------------------------------
-spec cache_pem_file(binary(), [db_handle()]) -> {ok, term()}.
cache_pem_file(File, [_CertsDb, _RefDb, PemChache | _]) ->
    {ok, PemBin} = file:read_file(File),
    Content = public_key:pem_decode(PemBin),
    insert(File, Content, PemChache),
    {ok, Content}.


-spec cache_pem_file(reference(), binary(), [db_handle()]) -> {ok, term()}.
cache_pem_file(Ref, File, [_CertsDb, _RefDb, PemChache| _]) ->
    {ok, PemBin} = file:read_file(File),
    Content = public_key:pem_decode(PemBin),
    insert(File, {Content, Ref}, PemChache),
    {ok, Content}.

%%--------------------------------------------------------------------
-spec remove_trusted_certs(reference(), db_handle()) -> ok.
%%
%% Description: Removes all trusted certificates refernced by <Ref>.
%%--------------------------------------------------------------------
remove_trusted_certs(Ref, CertsDb) ->
    remove_certs(Ref, CertsDb).

%%--------------------------------------------------------------------
-spec remove(term(), db_handle()) -> ok.
%%
%% Description: Removes an element in a <Db>.
%%--------------------------------------------------------------------
remove(Key, Db) ->
    ets:delete(Db, Key),
    ok.

%%--------------------------------------------------------------------
-spec remove(term(), term(), db_handle()) -> ok.
%%
%% Description: Removes an element in a <Db>.
%%--------------------------------------------------------------------
remove(Key, Data, Db) ->
    ets:delete_object(Db, {Key, Data}),
    ok.

%%--------------------------------------------------------------------
-spec lookup(term(), db_handle()) -> [term()] | undefined.
%%
%% Description: Looks up an element in a <Db>.
%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------
-spec foldl(fun((_,_) -> term()), term(), db_handle()) -> term().
%%
%% Description: Calls Fun(Elem, AccIn) on successive elements of the
%% cache, starting with AccIn == Acc0. Fun/2 must return a new
%% accumulator which is passed to the next call. The function returns
%% the final value of the accumulator. Acc0 is returned if the certifate
%% db is empty.
%%--------------------------------------------------------------------
foldl(Fun, Acc0, Cache) ->
    ets:foldl(Fun, Acc0, Cache).


select_cert_by_issuer(Cache, Issuer) ->    
    ets:select(Cache, [{{{'_','_', Issuer},{'_', '$1'}},[],['$$']}]).

%%--------------------------------------------------------------------
-spec ref_count(term(), db_handle(), integer()) -> integer().
%%
%% Description: Updates a reference counter in a <Db>.
%%--------------------------------------------------------------------
ref_count(Key, Db, N) ->
    ets:update_counter(Db,Key,N).

%%--------------------------------------------------------------------
-spec clear(db_handle()) -> ok.
%%
%% Description: Clears the cache
%%--------------------------------------------------------------------
clear(Db) ->
    true = ets:delete_all_objects(Db),
    ok.

%%--------------------------------------------------------------------
-spec db_size(db_handle()) -> integer().
%%
%% Description: Returns the size of the db
%%--------------------------------------------------------------------
db_size(Db) ->
    ets:info(Db, size).

%%--------------------------------------------------------------------
-spec insert(Key::term(), Data::term(), Db::db_handle()) -> ok.
%%
%% Description: Inserts data into <Db>
%%--------------------------------------------------------------------
insert(Key, Data, Db) ->
    true = ets:insert(Db, {Key, Data}),
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
update_counter(Key, Count, Db) ->
    true = ets:insert(Db, {Key, Count}),
    ok.

remove_certs(Ref, CertsDb) ->
    true = ets:match_delete(CertsDb, {{Ref, '_', '_'}, '_'}),
    ok.

add_certs_from_der(DerList, Ref, CertsDb) ->
    Add = fun(Cert) -> add_certs(Cert, Ref, CertsDb) end,
    [Add(Cert) || Cert <- DerList],
    ok.

add_certs_from_pem(PemEntries, Ref, CertsDb) ->
    Add = fun(Cert) -> add_certs(Cert, Ref, CertsDb) end,
    [Add(Cert) || {'Certificate', Cert, not_encrypted} <- PemEntries],
    ok.

add_certs(Cert, Ref, CertsDb) ->
    try  ErlCert = public_key:pkix_decode_cert(Cert, otp),
	 TBSCertificate = ErlCert#'OTPCertificate'.tbsCertificate,
	 SerialNumber = TBSCertificate#'OTPTBSCertificate'.serialNumber,
	 Issuer = public_key:pkix_normalize_name(
		    TBSCertificate#'OTPTBSCertificate'.issuer),
	 insert({Ref, SerialNumber, Issuer}, {Cert,ErlCert}, CertsDb)
    catch
	error:_ ->
	    Report = io_lib:format("SSL WARNING: Ignoring a CA cert as "
				   "it could not be correctly decoded.~n", []),
	    error_logger:info_report(Report)
    end.

new_trusted_cert_entry(File, [CertsDb, RefDb, _ | _] = Db) ->
    Ref = make_ref(),
    update_counter(Ref, 1, RefDb),
    {ok, Content} = cache_pem_file(Ref, File, Db),
    add_certs_from_pem(Content, Ref, CertsDb),
    {ok, Ref}.

add_crls([_,_,_, {_, Mapping} | _], ?NO_DIST_POINT, CRLs) ->
    [add_crls(CRL, Mapping) || CRL <- CRLs];
add_crls([_,_,_, {Cache, Mapping} | _], Path, CRLs) ->
    insert(Path, CRLs, Cache), 
    [add_crls(CRL, Mapping) || CRL <- CRLs].

add_crls(CRL, Mapping) ->
    insert(crl_issuer(CRL), CRL, Mapping).

remove_crls([_,_,_, {_, Mapping} | _], {?NO_DIST_POINT, CRLs}) ->
    [rm_crls(CRL, Mapping) || CRL <- CRLs];
	
remove_crls([_,_,_, {Cache, Mapping} | _], Path) ->
    case lookup(Path, Cache) of
	undefined ->
	    ok;
	CRLs ->
	    remove(Path, Cache),
	    [rm_crls(CRL, Mapping) || CRL <- CRLs]
    end.

rm_crls(CRL, Mapping) ->
   remove(crl_issuer(CRL), CRL, Mapping). 

crl_issuer(DerCRL) ->
    CRL = public_key:der_decode('CertificateList', DerCRL),
    TBSCRL = CRL#'CertificateList'.tbsCertList,
    TBSCRL#'TBSCertList'.issuer.

