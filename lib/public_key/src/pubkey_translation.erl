%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(pubkey_translation).
-moduledoc false.

-export([decode/1,encode/1]).

-include("public_key_internal.hrl").

decode(#'SubjectPublicKeyInfo'{algorithm=AlgId0,subjectPublicKey=Key}) ->
    #'SubjectPublicKeyInfo_algorithm'{algorithm=AlgId1,parameters=Params1} = AlgId0,
    AlgId = decode(AlgId1),
    Params = decode(Params1),
    %% Documented as AlgorithmIdentifier in plain
    #'SubjectPublicKeyInfo'{algorithm={'AlgorithmIdentifier', AlgId, Params},
                            subjectPublicKey=Key};
decode(#'DSA-Params'{p=P,q=Q,g=G}) ->
    {params, #'Dss-Parms'{p=P,q=Q,g=G}};
decode(#'DSA-Sig-Value'{r = R, s = S}) ->
    {'Dss-Sig-Value', R,S};
decode(#'OTPExtension'{}=E) ->
    setelement(1, E, 'Extension');
decode(#'SingleAttribute'{type=T,value=V}) ->
    #'AttributeTypeAndValue'{type=T,value=V};
decode({'OneAsymmetricKey', Vsn, KeyAlg, PrivKey, Attrs, PubKey} = Orig) ->   %% Defined In PKCS_FRAME
    case Vsn of
        v1 -> {'PrivateKeyInfo', Vsn, KeyAlg, PrivKey, Attrs, PubKey};
        _  -> Orig
    end;
decode({'EncryptionAlgorithmIdentifier', Algo, Params}) ->
    {'EncryptedPrivateKeyInfo_encryptionAlgorithm', Algo, Params};
decode(Tuple) when is_tuple(Tuple) ->
    case is_simple_tuple(Tuple) of
        true ->
            Tuple;
        false ->
            list_to_tuple(decode_list(tuple_to_list(Tuple)))
    end;
decode(List) when is_list(List) ->
    decode_list(List);
decode(Other) ->
    Other.

decode_list(List) ->
    [decode(E) || E <- List].

%% Documented as AlgorithmIdentifier in plain
encode(#'SubjectPublicKeyInfo'{algorithm={'AlgorithmIdentifier', AlgId0, Params},
                               subjectPublicKey=Key}) ->
    AlgId1 = encode(AlgId0),
    Params1 = encode(Params),
    Alg = #'SubjectPublicKeyInfo_algorithm'{algorithm=AlgId1,parameters=Params1},
    #'SubjectPublicKeyInfo'{algorithm=Alg,subjectPublicKey=Key};
encode(#'AttributeTypeAndValue'{type=T,value=V}) ->
    #'SingleAttribute'{type=T,value=V};
encode(#'SingleAttribute'{type=T,value={correct,V}}) ->
    #'SingleAttribute'{type=T,value=V};
encode({'PrivateKeyInfo', Vsn, KeyAlg, PrivKey, Attrs, PubKey}) ->
    {'OneAsymmetricKey', Vsn, KeyAlg, PrivKey, Attrs, PubKey};
encode({params, #'Dss-Parms'{p=P,q=Q,g=G}}) ->
    #'DSA-Params'{p=P,q=Q,g=G};
encode({'Dss-Sig-Value', R,S}) ->
    #'DSA-Sig-Value'{r = R, s = S};
encode({'EncryptedPrivateKeyInfo_encryptionAlgorithm', Algo, Params}) ->
    {'EncryptionAlgorithmIdentifier', Algo, Params};
encode(Tuple) when is_tuple(Tuple) ->
    case is_simple_tuple(Tuple) of
        true ->
            Tuple;
        false ->
            list_to_tuple(encode_list(tuple_to_list(Tuple)))
    end;
encode(List) when is_list(List) ->
    encode_list(List);
encode(Other) ->
    Other.

encode_list(List) ->
    [encode(E) || E <- List].

is_simple_tuple({'Extension',_,_,Bin}) when is_binary(Bin) ->
    true;
is_simple_tuple(Tuple) ->
    case element(1, Tuple) of
        Int when is_integer(Int) -> true;
        asn1_OPENTYPE -> true;
        'RSAPublicKey' -> true;
        utcTime -> true;
        _ -> false
    end.
