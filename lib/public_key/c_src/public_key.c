/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/*
 * Purpose:  CaCert fetcher
 */

#include <erl_nif.h>

#ifdef WINVER  /* Windows */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <Wincrypt.h>
#pragma comment(lib, "crypt32.lib")
#endif

ERL_NIF_TERM ATOM_PKCS_7_ASN_ENCODING;
ERL_NIF_TERM ATOM_X509_ASN_ENCODING;
ERL_NIF_TERM ATOM_UNKNOWN;

ERL_NIF_TERM ATOM_OPEN_ERROR;

static ERL_NIF_TERM os_cacerts(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ATOM_UNKNOWN      = enif_make_atom(env,"unknown");
    ATOM_OPEN_ERROR   = enif_make_atom(env, "internal_error");
    ATOM_PKCS_7_ASN_ENCODING  = enif_make_atom(env,"pkcs_7_asn_encoding");
    ATOM_X509_ASN_ENCODING  = enif_make_atom(env,"x509_asn_encoding");

    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
}

static ErlNifFunc nif_funcs[] =
  {
   {"os_cacerts", 0, os_cacerts, 0},
  };

ERL_NIF_INIT(pubkey_os_cacerts, nif_funcs, load, NULL, upgrade, unload)


#ifdef WINVER
ERL_NIF_TERM os_cacerts(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HANDLE          hStoreHandle = NULL;
    PCCERT_CONTEXT  pCertContext = NULL;
    ERL_NIF_TERM    head, tail, der, enc;
    unsigned char * data;

    hStoreHandle = CertOpenStore(CERT_STORE_PROV_SYSTEM_W, 0, 0,
                                 // CERT_SYSTEM_STORE_LOCAL_MACHINE,
                                 CERT_SYSTEM_STORE_CURRENT_USER,
                                 L"ROOT");
    if (!hStoreHandle) {
        return ATOM_OPEN_ERROR;
    }

    tail = enif_make_list(env, 0);
    while((pCertContext = CertEnumCertificatesInStore(hStoreHandle,pCertContext))) {
        switch (pCertContext->dwCertEncodingType) {
        case X509_ASN_ENCODING:
            enc = ATOM_X509_ASN_ENCODING;
            break;
        case PKCS_7_ASN_ENCODING:
            enc = ATOM_PKCS_7_ASN_ENCODING;
            break;
        default:
            enc = ATOM_UNKNOWN;
        }
        data = enif_make_new_binary(env, pCertContext->cbCertEncoded, &der);
        memcpy(data, pCertContext->pbCertEncoded, pCertContext->cbCertEncoded);
        head = enif_make_tuple2(env, enc, der);
        tail = enif_make_list_cell(env, head, tail);
    }

    CertCloseStore(hStoreHandle, 0);
    return tail;
}
#endif
