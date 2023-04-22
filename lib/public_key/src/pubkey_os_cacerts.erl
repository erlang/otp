%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

-module(pubkey_os_cacerts).

-include("public_key.hrl").
-export([load/0, load/1, get/0, clear/0]).

-on_load(on_load/0).
-nifs([os_cacerts/0]).

%% API

%% Return cacerts
-spec get() -> [public_key:combined_cert()].
get() ->
    case persistent_term:get(?MODULE, not_loaded) of
        not_loaded ->
            ok = load(),
            persistent_term:get(?MODULE);
        CaCerts ->
            CaCerts
    end.

%% (Re)Load default os cacerts and cache result.
-spec load() ->  ok | {error, Reason::term()}.
load() ->
    case os:type() of
        {unix, linux} ->
            load_from_file(linux_paths());
        {unix, openbsd} ->
            load_from_file(bsd_paths());
        {unix, freebsd} ->
            load_from_file(bsd_paths());
        {unix, netbsd} ->
            load_from_file(bsd_paths());
        {unix, sunos} ->
            load_from_files(sunos_path());
        {win32, _} ->
            load_win32();
	{unix, darwin} ->
            load_darwin();
        Os ->
            {error, {enotsup, Os}}
    end.

%% (Re)Load cacerts from file and cache result.
%% The file-paths will be tried in order.
%% Can be used when load/0 doesn't work for an unsupported os type.
-spec load([file:filename_all()]) -> ok | {error, Reason::term()}.
load(Paths) ->
    load_from_file(Paths).


%% cleanup persistent_key
-spec clear() -> boolean().
clear() ->
    persistent_term:erase(?MODULE).

%% Implementation
load_from_file([Path|Paths]) when is_list(Path); is_binary(Path) ->
    try
        {ok, Binary} = file:read_file(Path),
        ok = decode_result(Binary)
    catch _:_Reason ->
            load_from_file(Paths)
    end;
load_from_file([]) ->
    {error, enoent}.

decode_result(Binary) ->
    try
        MakeCert = fun({'Certificate', Der, not_encrypted}, Acc) ->
                           try
                               Decoded = public_key:pkix_decode_cert(Der, otp),
                               [#cert{der=Der, otp=Decoded}|Acc]
                           catch _:_ ->
                                   Acc
                           end
                   end,
        Certs = lists:foldl(MakeCert, [], pubkey_pem:decode(Binary)),
        store(Certs)
    catch _:Reason ->
            {error, Reason}
    end.


load_from_files(Path) ->
    MakeCert = fun(FileName, Acc) ->
                       try
                           {ok, Bin} = file:read_file(FileName),
                           [#cert{der=Der, otp=public_key:pkix_decode_cert(Der, otp)}
                            || {'Certificate', Der, not_encrypted} <- pubkey_pem:decode(Bin)]
                               ++ Acc
                       catch _:_ ->
                               Acc
                       end
               end,
    Certs = filelib:fold_files(Path, ".*\.pem", false, MakeCert, []),
    store(Certs).


load_win32() ->
    Dec = fun({_Enc, Der}, Acc) ->
                  try
                      Decoded = public_key:pkix_decode_cert(Der, otp),
                      [#cert{der=Der, otp=Decoded}|Acc]
                  catch _:_ ->
                          Acc
                  end
          end,
    store(lists:foldl(Dec, [], os_cacerts())).

load_darwin() ->
    %% Could/should probably be re-written to use Keychain Access API
    KeyChainFile = "/System/Library/Keychains/SystemRootCertificates.keychain",
    Args = ["export", "-t",  "certs", "-f", "pemseq", "-k", KeyChainFile],
    try run_cmd("/usr/bin/security", Args) of
        {ok, Bin} -> decode_result(Bin);
        Err -> Err
    catch error:Reason ->
            {error, {eopnotsupp, Reason}}
    end.

store([]) ->
    {error, no_cacerts_found};
store(CaCerts) ->
    persistent_term:put(?MODULE, CaCerts).

linux_paths() ->
    ["/etc/ssl/certs/ca-certificates.crt",                %% Debian, Ubuntu, Gentoo
     "/etc/pki/tls/certs/ca-bundle.crt",                  %% Fedora, RHEL 6, Amazon Linux
     "/etc/ssl/ca-bundle.pem",                            %% OpenSUSE
     "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem", %% CentOS, RHEL 7
     "/etc/ssl/cert.pem"                                  %% Alpine Linux
    ].

bsd_paths() ->
    ["/usr/local/share/certs/ca-root-nss.crt",
     "/etc/ssl/cert.pem",
     "/etc/openssl/certs/cacert.pem",   %% netbsd (if installed)
     "/etc/openssl/certs/ca-certificates.crt"
    ].

sunos_path() ->
    "/etc/certs/CA/".

run_cmd(Cmd, Args) ->
    Opts = [binary, exit_status, stderr_to_stdout],
    Port = open_port({spawn_executable, Cmd}, [{args, Args}|Opts]),
    unlink(Port),
    cmd_data(Port, <<>>).

cmd_data(Port, Acc) ->
    receive
        {Port, {data, Bin}} ->
            cmd_data(Port, <<Acc/binary, Bin/binary>>);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, Status}} ->
            {error, {eopnotsupp, Status, Acc}}
    end.

%%%
%%% NIF placeholders
%%%

-spec os_cacerts() -> [{Encoding::atom(), Cert::binary()}].

os_cacerts() ->
    erlang:nif_error(nif_not_loaded).

on_load() ->
    case os:type() of
        {win32, _} -> load_nif();
        _ -> ok
    end.

load_nif() ->
    PrivDir = code:priv_dir(public_key),
    LibName = "public_key",
    Lib = filename:join([PrivDir, "lib", LibName]),
    case erlang:load_nif(Lib, 0) of
        ok -> ok;
        {error, {load_failed, _}}=Error1 ->
            Arch = erlang:system_info(system_architecture),
            ArchLibDir = filename:join([PrivDir, "lib", Arch]),
            Candidate =
                filelib:wildcard(
                  filename:join([ArchLibDir,LibName ++ "*" ]),
                  erl_prim_loader),
            case Candidate of
                [] -> Error1;
                _ ->
                    ArchLib = filename:join([ArchLibDir, LibName]),
                    erlang:load_nif(ArchLib, 0)
            end;
        Error1 -> Error1
    end.
