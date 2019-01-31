/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

#include "algorithms.h"

static unsigned int algo_hash_cnt, algo_hash_fips_cnt;
static ERL_NIF_TERM algo_hash[12];   /* increase when extending the list */
static unsigned int algo_pubkey_cnt, algo_pubkey_fips_cnt;
static ERL_NIF_TERM algo_pubkey[12]; /* increase when extending the list */
static unsigned int algo_cipher_cnt, algo_cipher_fips_cnt;
static ERL_NIF_TERM algo_cipher[29]; /* increase when extending the list */
static unsigned int algo_mac_cnt, algo_mac_fips_cnt;
static ERL_NIF_TERM algo_mac[3]; /* increase when extending the list */
static unsigned int algo_curve_cnt, algo_curve_fips_cnt;
static ERL_NIF_TERM algo_curve[89]; /* increase when extending the list */
static unsigned int algo_rsa_opts_cnt, algo_rsa_opts_fips_cnt;
static ERL_NIF_TERM algo_rsa_opts[11]; /* increase when extending the list */

void init_algorithms_types(ErlNifEnv* env)
{
    // Validated algorithms first
    algo_hash_cnt = 0;
    algo_hash[algo_hash_cnt++] = atom_sha;
#ifdef HAVE_SHA224
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha224");
#endif
#ifdef HAVE_SHA256
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha256");
#endif
#ifdef HAVE_SHA384
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha384");
#endif
#ifdef HAVE_SHA512
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha512");
#endif
#ifdef HAVE_SHA3_224
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_224");
#endif
#ifdef HAVE_SHA3_256
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_256");
#endif
#ifdef HAVE_SHA3_384
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_384");
#endif
#ifdef HAVE_SHA3_512
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_512");
#endif
    // Non-validated algorithms follow
    algo_hash_fips_cnt = algo_hash_cnt;
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "md4");
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "md5");
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "ripemd160");

    algo_pubkey_cnt = 0;
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "rsa");
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "dss");
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "dh");
#if defined(HAVE_EC)
#if !defined(OPENSSL_NO_EC2M)
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "ec_gf2m");
#endif
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "ecdsa");
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "ecdh");
#endif
    // Non-validated algorithms follow
    algo_pubkey_fips_cnt = algo_pubkey_cnt;
    // Don't know if Edward curves are fips validated
#if defined(HAVE_EDDSA)
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "eddsa");
#endif
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "srp");

    // Validated algorithms first
    algo_cipher_cnt = 0;
#ifndef OPENSSL_NO_DES
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des3_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des_ede3");
#ifdef HAVE_DES_ede3_cfb_encrypt
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des3_cbf");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des3_cfb");
#endif
#endif
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc128");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc192");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc256");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cfb8");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cfb128");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ctr");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ctr128");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ctr192");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ctr256");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ecb");
#if defined(HAVE_GCM)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"aes_gcm");
#endif
#if defined(HAVE_CCM)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"aes_ccm");
#endif
    // Non-validated algorithms follow
    algo_cipher_fips_cnt = algo_cipher_cnt;
#ifdef HAVE_AES_IGE
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"aes_ige256");
#endif
#ifndef OPENSSL_NO_DES
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"des_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"des_cfb");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"des_ecb");
#endif
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_cfb64");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_ofb64");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_ecb");
#ifndef OPENSSL_NO_RC2
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"rc2_cbc");
#endif
#ifndef OPENSSL_NO_RC4
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"rc4");
#endif
#if defined(HAVE_CHACHA20_POLY1305)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"chacha20_poly1305");
#endif
#if defined(HAVE_CHACHA20)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"chacha20");
#endif
 
    // Validated algorithms first
    algo_mac_cnt = 0;
    algo_mac[algo_mac_cnt++] = enif_make_atom(env,"hmac");
#ifdef HAVE_CMAC
    algo_mac[algo_mac_cnt++] = enif_make_atom(env,"cmac");
#endif
#ifdef HAVE_POLY1305
    algo_mac[algo_mac_cnt++] = enif_make_atom(env,"poly1305");
#endif
    // Non-validated algorithms follow
    algo_mac_fips_cnt = algo_mac_cnt;

    // Validated algorithms first
    algo_curve_cnt = 0;
#if defined(HAVE_EC)
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp160k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp160r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp160r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp192r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp192k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp224k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp224r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp256k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp256r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp384r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp521r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime192v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime192v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime192v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime239v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime239v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime239v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime256v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls7");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls9");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls12");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP160r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP160t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP192r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP192t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP224r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP224t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP256r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP256t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP320r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP320t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP384r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP384t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP512r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP512t1");
#if !defined(OPENSSL_NO_EC2M)
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect163k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect163r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect163r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect193r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect193r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect233k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect233r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect239k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect283k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect283r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect409k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect409r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect571k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect571r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb163v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb163v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb163v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb176v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb191v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb191v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb191v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb208w1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb239v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb239v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb239v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb272w1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb304w1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb359v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb368w1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb431r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls5");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls10");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls11");
#endif
#endif
    // Non-validated algorithms follow
    algo_curve_fips_cnt = algo_curve_cnt;
#if defined(HAVE_EC)
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp112r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp112r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp128r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp128r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls6");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls8");
#if !defined(OPENSSL_NO_EC2M)
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect113r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect113r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect131r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect131r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls4");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"ipsec3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"ipsec4");
#endif
#endif
    //--
#ifdef HAVE_EDDSA
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"ed25519");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"ed448");
#endif
#ifdef HAVE_ED_CURVE_DH
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"x25519");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"x448");
#endif

    // Validated algorithms first
    algo_rsa_opts_cnt = 0;
#ifdef HAS_EVP_PKEY_CTX
# ifdef HAVE_RSA_PKCS1_PSS_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_pss_padding");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pss_saltlen");
# endif
# ifdef HAVE_RSA_MGF1_MD
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_mgf1_md");
# endif
# ifdef HAVE_RSA_OAEP_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_oaep_padding");
# endif
# ifdef HAVE_RSA_OAEP_MD
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_oaep_label");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_oaep_md");
# endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"signature_md");
#endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_padding");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_x931_padding");
#ifdef HAVE_RSA_SSLV23_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_sslv23_padding");
#endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_no_padding");
    algo_rsa_opts_fips_cnt = algo_rsa_opts_cnt;


    // Check that the max number of algos is updated
    ASSERT(algo_hash_cnt <= sizeof(algo_hash)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_pubkey_cnt <= sizeof(algo_pubkey)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_cipher_cnt <= sizeof(algo_cipher)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_mac_cnt <= sizeof(algo_mac)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_curve_cnt <= sizeof(algo_curve)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_rsa_opts_cnt <= sizeof(algo_rsa_opts)/sizeof(ERL_NIF_TERM));
}

ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef FIPS_SUPPORT
    int fips_mode  = FIPS_mode();

    unsigned int hash_cnt   = fips_mode ? algo_hash_fips_cnt   : algo_hash_cnt;
    unsigned int pubkey_cnt = fips_mode ? algo_pubkey_fips_cnt : algo_pubkey_cnt;
    unsigned int cipher_cnt = fips_mode ? algo_cipher_fips_cnt : algo_cipher_cnt;
    unsigned int mac_cnt    = fips_mode ? algo_mac_fips_cnt    : algo_mac_cnt;
    unsigned int curve_cnt    = fips_mode ? algo_curve_fips_cnt    : algo_curve_cnt;
    unsigned int rsa_opts_cnt = fips_mode ? algo_rsa_opts_fips_cnt : algo_rsa_opts_cnt;
#else
    unsigned int hash_cnt   = algo_hash_cnt;
    unsigned int pubkey_cnt = algo_pubkey_cnt;
    unsigned int cipher_cnt = algo_cipher_cnt;
    unsigned int mac_cnt    = algo_mac_cnt;
    unsigned int curve_cnt    = algo_curve_cnt;
    unsigned int rsa_opts_cnt = algo_rsa_opts_cnt;
#endif
    return enif_make_tuple6(env,
                            enif_make_list_from_array(env, algo_hash,   hash_cnt),
			    enif_make_list_from_array(env, algo_pubkey, pubkey_cnt),
			    enif_make_list_from_array(env, algo_cipher, cipher_cnt),
                            enif_make_list_from_array(env, algo_mac,    mac_cnt),
			    enif_make_list_from_array(env, algo_curve,  curve_cnt),
			    enif_make_list_from_array(env, algo_rsa_opts, rsa_opts_cnt)
                            );
}
