/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2023. All Rights Reserved.
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

#include "pkey.h"
#include "bn.h"
#include "digest.h"
#include "dss.h"
#include "ec.h"
#include "eddsa.h"
#include "engine.h"
#include "rsa.h"

typedef struct PKeyCryptOptions {
    const EVP_MD *rsa_mgf1_md;
    ErlNifBinary rsa_oaep_label;
    const EVP_MD *rsa_oaep_md;
    int rsa_padding;
    const EVP_MD *signature_md;
} PKeyCryptOptions;

typedef struct PKeySignOptions {
    const EVP_MD *rsa_mgf1_md;
    int rsa_padding;
    int rsa_pss_saltlen;
} PKeySignOptions;


static int check_pkey_algorithm_type(ErlNifEnv *env,
                                     int alg_arg_num, ERL_NIF_TERM algorithm,
                                     ERL_NIF_TERM *err_return);
static int get_pkey_digest_type(ErlNifEnv *env, ERL_NIF_TERM algorithm,
                                int type_arg_num, ERL_NIF_TERM type,
				const EVP_MD **md,
                                ERL_NIF_TERM *err_return);
static int get_pkey_sign_digest(ErlNifEnv *env,
                                const ERL_NIF_TERM argv[],
                                int algorithm_arg_num, int type_arg_num, int data_arg_num,
				unsigned char *md_value, const EVP_MD **mdp,
				unsigned char **tbsp, size_t *tbslenp,
                                ERL_NIF_TERM *err_return);
static int get_pkey_sign_options(ErlNifEnv *env,
                                 const ERL_NIF_TERM argv[],
                                 int algorithm_arg_num, int options_arg_num,
                                 const EVP_MD *md, PKeySignOptions *opt,
                                 ERL_NIF_TERM *err_return);
static int get_pkey_private_key(ErlNifEnv *env,
                               const ERL_NIF_TERM argv[],
                               int algorithm_arg_num, int key_arg_num,
			       EVP_PKEY **pkey,
                               ERL_NIF_TERM *err_return);
static int get_pkey_public_key(ErlNifEnv *env,
                               const ERL_NIF_TERM argv[],
                               int algorithm_arg_num, int key_arg_num,
			       EVP_PKEY **pkey,
                               ERL_NIF_TERM *err_return);
static int get_pkey_crypt_options(ErlNifEnv *env,
                                  const ERL_NIF_TERM argv[],
                                  int algorithm_arg_num, int options_arg_num,
				  PKeyCryptOptions *opt,
                                  ERL_NIF_TERM *err_return);
#ifdef HAVE_RSA_SSLV23_PADDING
static size_t size_of_RSA(EVP_PKEY *pkey);
#endif

static int check_pkey_algorithm_type(ErlNifEnv *env,
                                     int alg_arg_num, ERL_NIF_TERM algorithm,
                                     ERL_NIF_TERM *err_return)
{
    if (
#ifndef HAVE_EDDSA
        (algorithm == atom_eddsa) ||
#endif

#ifndef HAVE_DSA
        (algorithm == atom_dss) ||
#endif

#ifndef HAVE_EC
        (algorithm == atom_ecdsa) ||
#endif
        0)
        assign_goto(*err_return, err,  EXCP_NOTSUP_N(env, alg_arg_num, "Unsupported algorithm"));
        

#ifdef HAVE_EDDSA
    if (FIPS_MODE() && algorithm == atom_eddsa)
        assign_goto(*err_return, err, EXCP_NOTSUP_N(env, alg_arg_num, "Unsupported algorithm in FIPS mode"));
#endif    

    if ((algorithm != atom_rsa) &&
        (algorithm != atom_dss) &&
        (algorithm != atom_ecdsa) &&
        (algorithm != atom_eddsa)
        )
        assign_goto(*err_return, err, EXCP_BADARG_N(env, alg_arg_num, "Bad algorithm"));

    return 1;

 err:
    return 0;
}


static int get_pkey_digest_type(ErlNifEnv *env, ERL_NIF_TERM algorithm,
                                int type_arg_num, ERL_NIF_TERM type,
				const EVP_MD **md,
                                ERL_NIF_TERM *err_return)
{
    struct digest_type_t *digp = NULL;
    *md = NULL;

    if (type == atom_none && algorithm == atom_rsa)
        return 1;

    if (algorithm == atom_eddsa) /* Type was skipped for eddsa in < OTP-25
                                    For eddsa the RFC 8032 mandates sha512 in
                                    the algorithm */
        return 1;
    
    if ((digp = get_digest_type(type)) == NULL)
        assign_goto(*err_return, notsup, EXCP_BADARG_N(env, type_arg_num, "Bad digest type"));

    if (DIGEST_FORBIDDEN_IN_FIPS(digp))
        assign_goto(*err_return, notsup, EXCP_BADARG_N(env, type_arg_num, "Digest type forbidden in FIPS"));

    if (digp->md.p == NULL)
        assign_goto(*err_return, notsup, EXCP_BADARG_N(env, type_arg_num, "Digest type not supported"));

    *md = digp->md.p;
    return 1;

 notsup:
    return 0;
}

static int get_pkey_sign_digest(ErlNifEnv *env,
                                const ERL_NIF_TERM argv[],
                                int algorithm_arg_num, int type_arg_num, int data_arg_num,
				unsigned char *md_value, const EVP_MD **mdp,
				unsigned char **tbsp, size_t *tbslenp,
                                ERL_NIF_TERM *err_return)
{
    int ret;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    ErlNifBinary tbs_bin;
    EVP_MD_CTX *mdctx = NULL;
    const EVP_MD *md;
    unsigned char *tbs;
    size_t tbslen;
    unsigned int tbsleni;

    md = *mdp;
    tbs = *tbsp;
    tbslen = *tbslenp;

    if (!check_pkey_algorithm_type(env, algorithm_arg_num, argv[algorithm_arg_num], err_return))
        goto err; /* An exception is present in ret */
    
    if (!get_pkey_digest_type(env, argv[algorithm_arg_num],
                              type_arg_num, argv[type_arg_num],
                              &md, err_return))
        goto err; /* An exception is present in ret */

    if (enif_get_tuple(env, argv[data_arg_num], &tpl_arity, &tpl_terms)) {
        if (tpl_arity != 2)
            assign_goto(*err_return, err, EXCP_BADARG_N(env, data_arg_num, "Bad list"));
        if (tpl_terms[0] != atom_digest)
            assign_goto(*err_return, err, EXCP_BADARG_N(env, data_arg_num, "Expected 'digest' as head"));
        if (!enif_inspect_iolist_as_binary(env, tpl_terms[1], &tbs_bin))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, data_arg_num, "Bad 2nd element in list"));
        if (tbs_bin.size > INT_MAX)
            assign_goto(*err_return, err, EXCP_BADARG_N(env, data_arg_num, "Too large binary"));
        if (md != NULL) {
            if ((int)tbs_bin.size != EVP_MD_size(md))
                assign_goto(*err_return, err, EXCP_BADARG_N(env, data_arg_num, "Bad binary size for the algorithm"));
        }

        /* We have a digest (= hashed text) in tbs_bin */
	tbs = tbs_bin.data;
	tbslen = tbs_bin.size;
    } else if (md == NULL) {
        if (!enif_inspect_iolist_as_binary(env, argv[data_arg_num], &tbs_bin))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, data_arg_num, "Expected a binary or a list"));

        /* md == NULL, that is no hashing because DigestType argument was atom_none */
	tbs = tbs_bin.data;
	tbslen = tbs_bin.size;
    } else {
        if (!enif_inspect_iolist_as_binary(env, argv[data_arg_num], &tbs_bin))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, data_arg_num, "Expected a binary or a list"));

        /* We have the cleartext in tbs_bin and the hash algo info in md */
	tbs = md_value;

        if ((mdctx = EVP_MD_CTX_create()) == NULL)
            assign_goto(*err_return, err, EXCP_ERROR(env, "Can't create MD_CTX"));

        /* Looks well, now hash the plain text into a digest according to md */
        if (EVP_DigestInit_ex(mdctx, md, NULL) != 1)
            assign_goto(*err_return, err, EXCP_ERROR(env, "Can't create EVP_DigestInit_ex"));
        if (EVP_DigestUpdate(mdctx, tbs_bin.data, tbs_bin.size) != 1)
            assign_goto(*err_return, err, EXCP_ERROR(env, "Can't create EVP_DigestUpdate"));
        if (EVP_DigestFinal_ex(mdctx, tbs, &tbsleni) != 1)
            assign_goto(*err_return, err, EXCP_ERROR(env, "Can't create EVP_DigestFinal_ex"));

        tbslen = (size_t)tbsleni;
    }

    *mdp = md;
    *tbsp = tbs;
    *tbslenp = tbslen;

    ret = 1;
    goto done;

 err:
    ret = 0;
 done:
    if (mdctx)
        EVP_MD_CTX_destroy(mdctx);
    return ret;
}

static int get_pkey_sign_options(ErlNifEnv *env,
                                 const ERL_NIF_TERM argv[],
                                 int algorithm_arg_num, int options_arg_num,
                                 const EVP_MD *md, PKeySignOptions *opt,
                                 ERL_NIF_TERM *err_return)
{
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    const EVP_MD *opt_md;

    if (!enif_is_list(env, argv[options_arg_num]))
        assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Expected a list"));

    /* defaults */
    if (argv[algorithm_arg_num] == atom_rsa) {
	opt->rsa_mgf1_md = NULL;
	opt->rsa_padding = RSA_PKCS1_PADDING;
	opt->rsa_pss_saltlen = -2;
    } else {
	opt->rsa_mgf1_md = NULL;
	opt->rsa_padding = 0;
	opt->rsa_pss_saltlen = 0;
    }

    if (enif_is_empty_list(env, argv[options_arg_num]))
	return 1;

    if (argv[algorithm_arg_num] != atom_rsa)
        assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Only RSA supports Options"));

    tail = argv[options_arg_num];
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        if (!enif_get_tuple(env, head, &tpl_arity, &tpl_terms) ||
            (tpl_arity != 2))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Expects only two-tuples in the list"));

        if (tpl_terms[0] == atom_rsa_mgf1_md) {
            if (!enif_is_atom(env, tpl_terms[1]))
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Atom expected as argument to option rsa_mgf1_md"));

            if (!get_pkey_digest_type(env, argv[algorithm_arg_num],
                                      options_arg_num, tpl_terms[1],
                                      &opt_md, err_return))
                goto err; /* An exception is present in ret */
            
            opt->rsa_mgf1_md = opt_md;

        } else if (tpl_terms[0] == atom_rsa_padding) {
            if (tpl_terms[1] == atom_rsa_pkcs1_padding) {
                opt->rsa_padding = RSA_PKCS1_PADDING;

            } else if (tpl_terms[1] == atom_rsa_pkcs1_pss_padding) {
#ifdef HAVE_RSA_PKCS1_PSS_PADDING
                opt->rsa_padding = RSA_PKCS1_PSS_PADDING;
                if (opt->rsa_mgf1_md == NULL)
                    opt->rsa_mgf1_md = md;
#else
                assign_goto(*err_return, err, EXCP_NOTSUP_N(env, options_arg_num, "rsa_pkcs1_pss_padding not supported"));
#endif

            } else if (tpl_terms[1] == atom_rsa_x931_padding) {
                opt->rsa_padding = RSA_X931_PADDING;

            } else if (tpl_terms[1] == atom_rsa_no_padding) {
                opt->rsa_padding = RSA_NO_PADDING;

            } else {
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Bad value in option rsa_padding"));
            }

        } else if (tpl_terms[0] == atom_rsa_pss_saltlen) {
            if (!enif_get_int(env, tpl_terms[1], &(opt->rsa_pss_saltlen)) ||
                (opt->rsa_pss_saltlen < -2) )
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Bad value in option rsa_pss_saltlen"));

        } else {
            assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Bad option"));
        }
    }

    return 1;

 err:
    return 0;
}

static int get_pkey_private_key(ErlNifEnv *env,
                               const ERL_NIF_TERM argv[],
                               int algorithm_arg_num, int key_arg_num,
			       EVP_PKEY **pkey,
                               ERL_NIF_TERM *err_return)
{
    char *id = NULL;
    char *password = NULL;
    int ret;

    if (enif_is_map(env, argv[key_arg_num])) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;

        if (!get_engine_and_key_id(env, argv[key_arg_num], &id, &e))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get engine and/or key id"));

        password = get_key_password(env, argv[key_arg_num]);
        *pkey = ENGINE_load_private_key(e, id, NULL, password);
        if (!*pkey)
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get private key from engine"));
#else
        assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "No engine support"));
#endif

    } else  if (argv[algorithm_arg_num] == atom_rsa) {
        if (!get_rsa_private_key(env, argv[key_arg_num], pkey))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get RSA private key"));

    } else if (argv[algorithm_arg_num] == atom_ecdsa) {
#if defined(HAVE_EC)
        if (!get_ec_private_key(env, argv[key_arg_num], pkey))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get ECDSA private key"));
#else
	assign_goto(*err_return, err, EXCP_NOTSUP_N(env, algorithm_arg_num, "ECDSA not supported"));
#endif

    } else if (argv[algorithm_arg_num] == atom_eddsa) {
#ifdef HAVE_EDDSA
        if (!FIPS_MODE()) {
            if (!get_eddsa_key(env, 0, argv[key_arg_num], pkey))
                assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get EDDSA private key"));
        } else
            assign_goto(*err_return, err, EXCP_NOTSUP_N(env, algorithm_arg_num, "EDDSA not supported in FIPS mode"));
#else
        assign_goto(*err_return, err, EXCP_NOTSUP_N(env, algorithm_arg_num, "EDDSA not supported"));
#endif        

    } else if (argv[algorithm_arg_num] == atom_dss) {
#ifdef HAVE_DSA
        if (!get_dss_private_key(env, argv[key_arg_num], pkey))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get DSA private key"));
#else
        assign_goto(*err_return, err, EXCP_NOTSUP_N(env, algorithm_arg_num, "DSA not supported"));
#endif

    } else
          assign_goto(*err_return, err, EXCP_BADARG_N(env, algorithm_arg_num, "Bad algorithm"));  

    ret = 1;
 done:
    if (password)
        enif_free(password);
    if (id)
        enif_free(id);

    return ret;

 err:
    if (*pkey)
        EVP_PKEY_free(*pkey);
    *pkey = NULL;
    ret = 0;
    goto done;
}


static int get_pkey_public_key(ErlNifEnv *env,
                               const ERL_NIF_TERM argv[],
                               int algorithm_arg_num, int key_arg_num,
			       EVP_PKEY **pkey,
                               ERL_NIF_TERM *err_return)
{
    char *id = NULL;
    char *password = NULL;
    int ret;

    if (enif_is_map(env, argv[key_arg_num])) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;

        if (!get_engine_and_key_id(env, argv[key_arg_num], &id, &e))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get engine and/or key id"));

        password = get_key_password(env, argv[key_arg_num]);
        *pkey = ENGINE_load_public_key(e, id, NULL, password);
        if (!pkey)
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get public key from engine"));
#else
        assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "No engine support"));
#endif

    } else  if (argv[algorithm_arg_num] == atom_rsa) {
        if (!get_rsa_public_key(env, argv[key_arg_num], pkey))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get RSA public key"));

    } else if (argv[algorithm_arg_num] == atom_ecdsa) {
#if defined(HAVE_EC)
        if (!get_ec_public_key(env, argv[key_arg_num], pkey))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get ECDSA public key"));
#else
        assign_goto(*err_return, err, EXCP_NOTSUP_N(env, algorithm_arg_num, "ECDSA not supported"));
#endif

    } else if (argv[algorithm_arg_num] == atom_eddsa) {
#ifdef HAVE_EDDSA
        if (!FIPS_MODE()) {
            if (!get_eddsa_key(env, 1, argv[key_arg_num], pkey))
                assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get EDDSA public key"));
        } else
            assign_goto(*err_return, err, EXCP_NOTSUP_N(env, algorithm_arg_num, "EDDSA not supported in FIPS mode"));
#else
        assign_goto(*err_return, err, EXCP_NOTSUP_N(env, algorithm_arg_num, "EDDSA not supported"));
#endif

    } else if (argv[algorithm_arg_num] == atom_dss) {
#ifdef HAVE_DSA
        if (!get_dss_public_key(env, argv[key_arg_num], pkey))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, key_arg_num, "Couldn't get DSA public key"));
#else
        assign_goto(*err_return, err, EXCP_NOTSUP_N(env, algorithm_arg_num, "DSA not supported"));
#endif
    } else
        assign_goto(*err_return, err, EXCP_BADARG_N(env, algorithm_arg_num, "Bad algorithm"));

    ret = 1;

 done:
    if (password)
        enif_free(password);
    if (id)
        enif_free(id);

    return ret;

 err:
    if (*pkey)
        EVP_PKEY_free(*pkey);
    *pkey = NULL;
    ret = 0;
    goto done;
}

ERL_NIF_TERM pkey_sign_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Type, Data|{digest,Digest}, Key|#{}, Options) */
    int sig_bin_alloc = 0;
    ERL_NIF_TERM ret = atom_undefined;
    const EVP_MD *md = NULL;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    EVP_PKEY *pkey = NULL;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx = NULL;
    size_t siglen;
#else
    int len;
    unsigned int siglen;
    RSA *rsa = NULL;
# ifdef HAVE_DSA
    DSA *dsa = NULL;
# endif
# if defined(HAVE_EC)
    EC_KEY *ec = NULL;
# endif
#endif
    PKeySignOptions sig_opt;
    ErlNifBinary sig_bin; /* signature */
    unsigned char *tbs = NULL; /* data to be signed */
    size_t tbslen = 0;

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[3]))
        assign_goto(ret, err, EXCP_BADARG_N(env, 3, "No engine support"));
#endif
    if (!get_pkey_sign_digest(env, argv, 0, 1, 2, md_value, &md, &tbs, &tbslen, &ret))
        goto err; /* An exception is present in ret */

    if (!get_pkey_sign_options(env, argv, 0, 4, md, &sig_opt, &ret))
        goto err; /* An exception is present in ret */

#ifdef HAS_EVP_PKEY_CTX
    { /* EVP_MD_CTX */
        if (!get_pkey_private_key(env, argv, 0, 3, &pkey, &ret))
            goto err; /* An exception is present in ret */

        if ((ctx = EVP_PKEY_CTX_new(pkey, NULL)) == NULL)
            assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate new EVP_PKEY_CTX"));

        if (argv[0] != atom_eddsa) {
            if (EVP_PKEY_sign_init(ctx) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_sign_init"));
            if (md != NULL) {
                if (EVP_PKEY_CTX_set_signature_md(ctx, md) != 1)
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_set_signature_md"));
            }
        }

        if (argv[0] == atom_rsa) {
            if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_set_rsa_padding"));
# ifdef HAVE_RSA_PKCS1_PSS_PADDING
            if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
                if (sig_opt.rsa_mgf1_md != NULL) {
#  ifdef HAVE_RSA_MGF1_MD
                    if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) != 1)
                        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_set_rsa_mgf1_md"));
#  else
                    assign_goto(ret, err, EXCP_NOTSUP_N(env, 4, "rsa_mgf1_md unavailable with this cryptolib"));
#  endif
                }
                if (sig_opt.rsa_pss_saltlen > -2) {
                    if (EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) != 1)
                        assign_goto(ret, err, EXCP_BADARG_N(env, 4, "Bad rsa_pss_saltlen"));
                }
            }
# endif
        }

        if (argv[0] == atom_eddsa) {
# ifdef HAVE_EDDSA
            if (!FIPS_MODE()) {
                EVP_MD_CTX *mdctx = NULL;
                if ((mdctx = EVP_MD_CTX_new()) == NULL)
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_MD_CTX_new"));

                if (EVP_DigestSignInit(mdctx, NULL, NULL, NULL, pkey) != 1)
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_DigestSignInit"));
                if (EVP_DigestSign(mdctx, NULL, &siglen, tbs, tbslen) != 1)
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_DigestSign"));
                if (!enif_alloc_binary(siglen, &sig_bin))
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));
                sig_bin_alloc = 1;

                if (EVP_DigestSign(mdctx, sig_bin.data, &siglen, tbs, tbslen) != 1) {
                    if (mdctx)
                        EVP_MD_CTX_free(mdctx);
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_DigestSign"));
                }
                if (mdctx)
                    EVP_MD_CTX_free(mdctx);
            }
            else
# endif
                assign_goto(ret, err, EXCP_NOTSUP_N(env, 0, "eddsa not supported"));
        } else {
            if (EVP_PKEY_sign(ctx, NULL, &siglen, tbs, tbslen) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_sign"));

            if (!enif_alloc_binary(siglen, &sig_bin))
                assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));
            sig_bin_alloc = 1;

            if (md != NULL) {
                ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
            }
            if (EVP_PKEY_sign(ctx, sig_bin.data, &siglen, tbs, tbslen) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_sign"));
        }

    ERL_VALGRIND_MAKE_MEM_DEFINED(sig_bin.data, siglen);
    if (siglen != sig_bin.size) {
        if (!enif_realloc_binary(&sig_bin, siglen))
            assign_goto(ret, err, EXCP_ERROR(env, "Can't reallocate binary"));
        ERL_VALGRIND_ASSERT_MEM_DEFINED(sig_bin.data, siglen);
    }

    ret = enif_make_binary(env, &sig_bin);
    sig_bin_alloc = 0;

    err:
    if (sig_bin_alloc)
        enif_release_binary(&sig_bin);
    if (ctx)
        EVP_PKEY_CTX_free(ctx);
    if (pkey)
        EVP_PKEY_free(pkey);
    return ret;
    }
    /* End of HAS_EVP_PKEY_CTX */
#else
    /* Old interface - before EVP_PKEY_CTX */
    {
        if (!get_pkey_private_key(env, argv, 0, 3, &pkey, &ret))
            goto err;  /* An exception is present in ret */

        if (argv[0] == atom_rsa) {
            if ((rsa = EVP_PKEY_get1_RSA(pkey)) == NULL)
                assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Not an RSA private key"));
            if ((len = RSA_size(rsa)) < 0)
                assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Bad RSA private key length"));
            if (!enif_alloc_binary((size_t)len, &sig_bin))
                assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));
            sig_bin_alloc = 1;

            if ((len = EVP_MD_size(md)) < 0)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't get md length"));
            ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);

            if (RSA_sign(md->type, tbs, (unsigned int)len, sig_bin.data, &siglen, rsa) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't sign"));
                            
        } else if (argv[0] == atom_dss) {
            if ((dsa = EVP_PKEY_get1_DSA(pkey)) == NULL)
                assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Not an DSA private key"));
            if ((len = DSA_size(dsa)) < 0)
                assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Bad DSA private key length"));
            if (!enif_alloc_binary((size_t)len, &sig_bin))
                assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));
            sig_bin_alloc = 1;

            if ((len = EVP_MD_size(md)) < 0)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't get md length"));
            ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);

            if (DSA_sign(md->type, tbs, len, sig_bin.data, &siglen, dsa) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't sign"));
        } else if (argv[0] == atom_ecdsa) {
# if defined(HAVE_EC)
            if ((ec = EVP_PKEY_get1_EC_KEY(pkey)) == NULL)
                assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Not an ECDSA private key"));
            if ((len = ECDSA_size(ec)) < 0)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't get size"));
            if (!enif_alloc_binary((size_t)len, &sig_bin))
                assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));
            sig_bin_alloc = 1;

            len = EVP_MD_size(md);
            ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);

            if (ECDSA_sign(md->type, tbs, len, sig_bin.data, &siglen, ec) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't sign"));
# else
            assign_goto(ret, notsup, EXCP_NOTSUP_N(env, 0, "ecdsa not supported"));
# endif /* HAVE_EC */
        } else {
            assign_goto(ret, notsup, EXCP_BADARG_N(env, 0, "Unknown algorithm"));
        }

        ERL_VALGRIND_MAKE_MEM_DEFINED(sig_bin.data, siglen);
        if (siglen != sig_bin.size) {
            if (!enif_realloc_binary(&sig_bin, siglen))
                 assign_goto(ret, err, EXCP_ERROR(env, "Can't re-allocate binary"));
            ERL_VALGRIND_ASSERT_MEM_DEFINED(sig_bin.data, siglen);
        }
        ret = enif_make_binary(env, &sig_bin);
        sig_bin_alloc = 0;

    notsup:
    err:
        if (sig_bin_alloc)
            enif_release_binary(&sig_bin);
        if (rsa)
            RSA_free(rsa);
# ifdef HAVE_DSA
        if (dsa)
            DSA_free(dsa);
# endif
# ifdef HAVE_EC
        if (ec)
            EC_KEY_free(ec);
# endif
# ifdef HAS_EVP_PKEY_CTX
        if (ctx)
            EVP_PKEY_CTX_free(ctx);
# endif
        if (pkey)
            EVP_PKEY_free(pkey);

        return ret;
    }
#endif

}



ERL_NIF_TERM pkey_verify_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Type, Data|{digest,Digest}, Signature, Key, Options) */
    int result;
    const EVP_MD *md = NULL;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    EVP_PKEY *pkey = NULL;
    PKeySignOptions sig_opt;
    ErlNifBinary sig_bin; /* signature */
    unsigned char *tbs = NULL; /* data to be signed */
    size_t tbslen = 0;
    ERL_NIF_TERM ret = atom_undefined;

#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx = NULL;
#else
    RSA *rsa = NULL;
# ifdef HAVE_DSA
        DSA *dsa = NULL;
# endif
# ifdef HAVE_EC
        EC_KEY *ec = NULL;
# endif
#endif  // HAS_EVP_PKEY_CTX


#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[3]))
        assign_goto(ret, err, EXCP_BADARG_N(env, 3, "No engine support"));
#endif

    if (!get_pkey_sign_digest(env, argv, 0, 1, 2, md_value, &md, &tbs, &tbslen, &ret))
        goto err; /* An exception is present in ret */

    if (!get_pkey_sign_options(env, argv, 0, 5, md, &sig_opt, &ret))
        goto err; /* An exception is present in ret */

    if (!enif_inspect_binary(env, argv[3], &sig_bin))
        assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Expected a binary"));

#ifdef HAS_EVP_PKEY_CTX
    /* EVP_PKEY_CTX */
    {
        if (!get_pkey_public_key(env, argv, 0, 4, &pkey, &ret))
            goto err; /* An exception is present in ret */

        if ((ctx = EVP_PKEY_CTX_new(pkey, NULL)) == NULL)
            assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate new EVP_PKEY_CTX"));

        if (argv[0] != atom_eddsa) {
            if (EVP_PKEY_verify_init(ctx) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_sign_init"));
            if (md != NULL) {
                if (EVP_PKEY_CTX_set_signature_md(ctx, md) != 1)
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_set_signature_md"));
            }
        }

        if (argv[0] == atom_rsa) {
            if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_set_rsa_padding"));
# ifdef HAVE_RSA_PKCS1_PSS_PADDING
            if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
                if (sig_opt.rsa_mgf1_md != NULL) {
#  ifdef HAVE_RSA_MGF1_MD
                    if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) != 1)
                        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_set_rsa_mgf1_md"));
#  else
                    assign_goto(ret, err, EXCP_NOTSUP_N(env, 5, "rsa_mgf1_md unavailable with this cryptolib"));
#  endif
                }
                if (sig_opt.rsa_pss_saltlen > -2) {
                    if (EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) != 1)
                        assign_goto(ret, err, EXCP_BADARG_N(env, 5, "Bad rsa_pss_saltlen"));
                }
            }
# endif
        }

        if (argv[0] == atom_eddsa) {
# ifdef HAVE_EDDSA
            EVP_MD_CTX *mdctx = NULL;
            if (!FIPS_MODE()) {
                if ((mdctx = EVP_MD_CTX_new()) == NULL)
                     assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_MD_CTX_new"));

                if (EVP_DigestVerifyInit(mdctx, NULL, NULL, NULL, pkey) != 1)
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_DigestVerifyInit"));

                result = EVP_DigestVerify(mdctx, sig_bin.data, sig_bin.size, tbs, tbslen);
                if (mdctx)
                    EVP_MD_CTX_free(mdctx);
            }
            else
# endif /* HAVE_EDDSA */
                assign_goto(ret, err, EXCP_NOTSUP_N(env, 0, "eddsa not supported"));
        } else {
            /* RSA or DSS */
            if (md != NULL) {
                ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
            }
            result = EVP_PKEY_verify(ctx, sig_bin.data, sig_bin.size, tbs, tbslen);
        }

        ret = (result == 1 ? atom_true : atom_false);

    err:
        if (ctx)
            EVP_PKEY_CTX_free(ctx);
        if (pkey)
            EVP_PKEY_free(pkey);

        return ret;
    }
    /* End of HAS_EVP_PKEY_CTX */
#else
    /* Old interface - before EVP_PKEY_CTX */
    {
        if (!get_pkey_public_key(env, argv, 0, 4, &pkey, &ret))
            goto err; /* An exception is present in ret */

        if (argv[0] == atom_rsa) {
            if ((rsa = EVP_PKEY_get1_RSA(pkey)) == NULL)
                assign_goto(ret, err, EXCP_BADARG_N(env, 4, "Not an RSA public key"));
            result = RSA_verify(md->type, tbs, (unsigned int)tbslen, sig_bin.data, (unsigned int)sig_bin.size, rsa);
        } else if (argv[0] == atom_dss) {
            if ((dsa = EVP_PKEY_get1_DSA(pkey)) == NULL)
                assign_goto(ret, err, EXCP_BADARG_N(env, 4, "Not an DSA public key"));
            result = DSA_verify(0, tbs, (int)tbslen, sig_bin.data, (int)sig_bin.size, dsa);
        } else if (argv[0] == atom_ecdsa) {
# if defined(HAVE_EC)
            if ((ec = EVP_PKEY_get1_EC_KEY(pkey)) == NULL)
                assign_goto(ret, err, EXCP_BADARG_N(env, 4, "Not an ECDSA private key"));
            result = ECDSA_verify(EVP_MD_type(md), tbs, (int)tbslen, sig_bin.data, (int)sig_bin.size, ec);
# else
            assign_goto(ret, err, EXCP_NOTSUP_N(env, 0, "ecdsa not supported"));
# endif /* HAVE_EC */
        } else {
            assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Unknown algorithm"));
        }
        ret = (result == 1 ? atom_true : atom_false);

    err:
        if (rsa)
            RSA_free(rsa);
# ifdef HAVE_DSA
        if (dsa)
            DSA_free(dsa);
# endif
# ifdef HAVE_EC
        if (ec)
            EC_KEY_free(ec);
# endif
        if (pkey)
            EVP_PKEY_free(pkey);

        return ret;
    }
#endif /* Pre  EVP_PKEY_CTX */

}


static int get_pkey_crypt_options(ErlNifEnv *env,
                                  const ERL_NIF_TERM argv[],
                                  int algorithm_arg_num, int options_arg_num,
				  PKeyCryptOptions *opt,
                                  ERL_NIF_TERM *err_return)
{
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    const EVP_MD *opt_md;

    if (!enif_is_list(env, argv[options_arg_num]))
        assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Expected a list"));

    /* defaults */
    if (argv[algorithm_arg_num] == atom_rsa) {
        opt->rsa_mgf1_md = NULL;
        opt->rsa_oaep_label.data = NULL;
        opt->rsa_oaep_label.size = 0;
        opt->rsa_oaep_md = NULL;
	opt->rsa_padding = RSA_PKCS1_PADDING;
        opt->signature_md = NULL;
    } else {
        opt->rsa_mgf1_md = NULL;
        opt->rsa_oaep_label.data = NULL;
        opt->rsa_oaep_label.size = 0;
        opt->rsa_oaep_md = NULL;
        opt->rsa_padding = 0;
        opt->signature_md = NULL;
    }

    if (enif_is_empty_list(env, argv[options_arg_num]))
        return 1; /* There are no options to fetch. Return OK */

    if (argv[algorithm_arg_num] != atom_rsa)
        assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Only RSA supports Options"));

    tail = argv[options_arg_num];
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        if (!enif_get_tuple(env, head, &tpl_arity, &tpl_terms) ||
            (tpl_arity != 2))
            assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Expect only two-tuples in the list"));

        if (tpl_terms[0] == atom_rsa_padding
            || tpl_terms[0] == atom_rsa_pad /* Compatibility */
            ) {
            if (tpl_terms[1] == atom_rsa_pkcs1_padding) {
                opt->rsa_padding = RSA_PKCS1_PADDING;

#ifdef HAVE_RSA_OAEP_PADDING
            } else if (tpl_terms[1] == atom_rsa_pkcs1_oaep_padding) {
                opt->rsa_padding = RSA_PKCS1_OAEP_PADDING;
#endif

#ifdef HAVE_RSA_SSLV23_PADDING
            } else if (tpl_terms[1] == atom_rsa_sslv23_padding) {
                opt->rsa_padding = RSA_SSLV23_PADDING;
#endif

            } else if (tpl_terms[1] == atom_rsa_x931_padding) {
                opt->rsa_padding = RSA_X931_PADDING;

            } else if (tpl_terms[1] == atom_rsa_no_padding) {
                opt->rsa_padding = RSA_NO_PADDING;

            } else
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Bad padding type in option rsa_padding"));

        } else if (tpl_terms[0] == atom_signature_md) {
            if (!enif_is_atom(env, tpl_terms[1]))
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Atom expected as argument to option signature_md"));

            if (!get_pkey_digest_type(env, argv[algorithm_arg_num],
                                      options_arg_num, tpl_terms[1],
                                      &opt_md, err_return))
                goto err; /* An exception is present in ret */

            opt->signature_md = opt_md;

        } else if (tpl_terms[0] == atom_rsa_mgf1_md) {
            if (!enif_is_atom(env, tpl_terms[1]))
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Atom expected as argument to option rsa_mgf1_md"));
#ifndef HAVE_RSA_MGF1_MD
            if (tpl_terms[1] != atom_sha)
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Only 'sha' is supported in option rsa_mgf1_md"));
#endif
            if (!get_pkey_digest_type(env, argv[algorithm_arg_num],
                                      options_arg_num, tpl_terms[1],
                                      &opt_md, err_return))
                goto err; /* An exception is present in ret */
            opt->rsa_mgf1_md = opt_md;

        } else if (tpl_terms[0] == atom_rsa_oaep_label) {
#ifdef HAVE_RSA_OAEP_MD
            if (!enif_inspect_binary(env, tpl_terms[1], &(opt->rsa_oaep_label)))
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Binary expected for option rsa_oaep_label"));
            continue;
#else
            assign_goto(*err_return, err, EXCP_NOTSUP_N(env, options_arg_num, "Option rsa_oaep_label is not supported"));
#endif

        } else if (tpl_terms[0] == atom_rsa_oaep_md) {
            if (!enif_is_atom(env, tpl_terms[1]))
                assign_goto(*err_return, err, EXCP_NOTSUP_N(env, options_arg_num, "Atom expected as argument to option rsa_oaep_md"));
#ifndef HAVE_RSA_OAEP_MD
            if (tpl_terms[1] != atom_sha)
                assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Only 'sha' is supported in option rsa_oaep_md"));
#endif
            if (!get_pkey_digest_type(env, argv[algorithm_arg_num],
                                      options_arg_num, tpl_terms[1],
                                      &opt_md, err_return))
                goto err; /* An exception is present in ret */
            opt->rsa_oaep_md = opt_md;
        } else
            assign_goto(*err_return, err, EXCP_BADARG_N(env, options_arg_num, "Unknown option"))
    }

    return 1;

 err:
    return 0;
}

#ifdef HAVE_RSA_SSLV23_PADDING
static size_t size_of_RSA(EVP_PKEY *pkey) {
    int ret = 0;
    RSA *rsa = NULL;

    if ((rsa = EVP_PKEY_get1_RSA(pkey)) == NULL)
        goto err;
    ret = RSA_size(rsa);

 err:
    if (rsa)
        RSA_free(rsa);

    return (ret < 0) ? 0 : (size_t)ret;
}
#endif

ERL_NIF_TERM pkey_crypt_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Data, PublKey=[E,N]|[E,N,D]|[E,N,D,P1,P2,E1,E2,C], Options, IsPrivate, IsEncrypt) */
    ERL_NIF_TERM ret = atom_undefined;
    int result = 0;
    int tmp_bin_alloc = 0;
    int out_bin_alloc = 0;
    EVP_PKEY *pkey = NULL;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx = NULL;
#else
    int len;
    RSA *rsa = NULL;
#endif
    PKeyCryptOptions crypt_opt;
    ErlNifBinary in_bin, out_bin, tmp_bin;
    size_t outlen;
#ifdef HAVE_RSA_SSLV23_PADDING
    size_t tmplen;
#endif
    int is_private, is_encrypt;
    unsigned char *label_copy = NULL;

    is_private = (argv[4] == atom_true);
    is_encrypt = (argv[5] == atom_true);

    if (!check_pkey_algorithm_type(env, 0, argv[0], &ret))
        goto err; /* An exception is present in ret */

    if (!enif_inspect_binary(env, argv[1], &in_bin))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Binary expected"));

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[2]))
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "No engine support"));
#endif

    if (is_private) {
        if (!get_pkey_private_key(env, argv, 0, 2, &pkey, &ret))
            goto err; /* An exception is present in ret */
    } else {
        if (!get_pkey_public_key(env, argv, 0, 2, &pkey, &ret))
            goto err;  /* An exception is present in ret */
    }

    if (!get_pkey_crypt_options(env, argv, 0, 3, &crypt_opt, &ret))
        goto err; /* An exception is present in ret */

#ifdef HAS_EVP_PKEY_CTX
    {
            int algo_init = 0;

            if ((ctx = EVP_PKEY_CTX_new(pkey, NULL)) == NULL)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate new EVP_PKEY_CTX"));

            if (is_private) {
                if (is_encrypt)
                    algo_init = EVP_PKEY_sign_init(ctx);
                else
                    algo_init = EVP_PKEY_decrypt_init(ctx);

            } else {
                if (is_encrypt)
                    algo_init = EVP_PKEY_encrypt_init(ctx);
                else
                    algo_init = EVP_PKEY_verify_recover_init(ctx);
            }

            if (algo_init != 1)
                assign_goto(ret, err, EXCP_NOTSUP(env, "Can't initiate encrypt/decrypt"));
    }

    if (argv[0] == atom_rsa) {
        if (crypt_opt.signature_md != NULL) {
            if (EVP_PKEY_CTX_set_signature_md(ctx, crypt_opt.signature_md) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_set_signature_md"));
        }

# ifdef HAVE_RSA_SSLV23_PADDING
        if (crypt_opt.rsa_padding == RSA_SSLV23_PADDING) {
            if (is_encrypt) {
                tmplen = size_of_RSA(pkey);
                if (tmplen < 1 || tmplen > INT_MAX)
                    assign_goto(ret, err, EXCP_BADARG_N(env, 2, "RSA key of wrong size"));
                if (!enif_alloc_binary(tmplen, &tmp_bin))
                    assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));
                tmp_bin_alloc = 1;
                if (in_bin.size > INT_MAX)
                    assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Binary too large"));
                if (!RSA_padding_add_SSLv23(tmp_bin.data, (int)tmplen, in_bin.data, (int)in_bin.size))
                    assign_goto(ret, err, EXCP_ERROR(env, "Couldn't RSA_padding_add_SSLv23"));
                in_bin = tmp_bin;
            }
            if (EVP_PKEY_CTX_set_rsa_padding(ctx, RSA_NO_PADDING) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Couldn't set RSA_NO_PADDING"));
        } else
# endif
            if (EVP_PKEY_CTX_set_rsa_padding(ctx, crypt_opt.rsa_padding) != 1)
                assign_goto(ret, err, EXCP_ERROR(env, "Couldn't set rsa padding"));

# ifdef HAVE_RSA_OAEP_MD
        if (crypt_opt.rsa_padding == RSA_PKCS1_OAEP_PADDING) {
            if (crypt_opt.rsa_oaep_md != NULL) {
                if (EVP_PKEY_CTX_set_rsa_oaep_md(ctx, crypt_opt.rsa_oaep_md) != 1)
                    assign_goto(ret, err, EXCP_ERROR(env, "Couldn't EVP_PKEY_CTX_set_rsa_oaep_md"));
            }

            if (crypt_opt.rsa_mgf1_md != NULL) {
                if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, crypt_opt.rsa_mgf1_md) != 1)
                    assign_goto(ret, err, EXCP_ERROR(env, "Couldn't EVP_PKEY_CTX_set_rsa_mgf1_md"));
            }

            if (crypt_opt.rsa_oaep_label.data != NULL && crypt_opt.rsa_oaep_label.size > 0) {
                if (crypt_opt.rsa_oaep_label.size > INT_MAX)
                    assign_goto(ret, err, EXCP_BADARG_N(env, 3, "RSA oep label too large"));
                if ((label_copy = OPENSSL_malloc(crypt_opt.rsa_oaep_label.size)) == NULL)
                    goto err;

                memcpy((void *)(label_copy), (const void *)(crypt_opt.rsa_oaep_label.data),
                       crypt_opt.rsa_oaep_label.size);

                if (EVP_PKEY_CTX_set0_rsa_oaep_label(ctx, label_copy,
                                                     (int)crypt_opt.rsa_oaep_label.size) != 1)
                    assign_goto(ret, err, EXCP_ERROR(env, "Couldn't set RSA oaep label"));
                /* On success, label_copy is owned by ctx */
                label_copy = NULL;
            }
        }
# endif
    }

    /* Get the size of the result */
    if (is_private)
        result =
            is_encrypt ? EVP_PKEY_sign(ctx, NULL, &outlen, in_bin.data, in_bin.size)
                       : EVP_PKEY_decrypt(ctx, NULL, &outlen, in_bin.data, in_bin.size);
    else
        result =
            is_encrypt ? EVP_PKEY_encrypt(ctx, NULL, &outlen, in_bin.data, in_bin.size)
                       : EVP_PKEY_verify_recover(ctx, NULL, &outlen, in_bin.data, in_bin.size);
    
    /* Check */
    if (result != 1)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't get size of result"));

    /* Allocate */
    if (!enif_alloc_binary(outlen, &out_bin))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));

    out_bin_alloc = 1; /* Flag de-allocation */

    /* Get the result into the newly allocated binary */
    if (is_private)
        result =
            is_encrypt ? EVP_PKEY_sign(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size)
                       : EVP_PKEY_decrypt(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
    else
        result=
            is_encrypt ? EVP_PKEY_encrypt(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size)
                       : EVP_PKEY_verify_recover(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);

    if (result != 1)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't get the result"));

#else
    /* Non-EVP cryptolib. Only supports RSA */

    if (argv[0] != atom_rsa)
        assign_goto(ret, err, EXCP_NOTSUP_N(env, 0, "Only RSA is supported"));

    if ((rsa = EVP_PKEY_get1_RSA(pkey)) == NULL)
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Not RSA key"));
    if ((len = RSA_size(rsa)) < 0)
        assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Bad RSA key length"));
    if (!enif_alloc_binary((size_t)len, &out_bin))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));
    out_bin_alloc = 1;

    if (in_bin.size > INT_MAX)
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Bad indata length"));
    
    if (is_encrypt) {
        ERL_VALGRIND_ASSERT_MEM_DEFINED(in_bin.data,in_bin.size);
    }

    if (is_private)
        result =
            is_encrypt ? RSA_private_encrypt((int)in_bin.size, in_bin.data,
                                             out_bin.data, rsa, crypt_opt.rsa_padding)
                       : RSA_private_decrypt((int)in_bin.size, in_bin.data,
                                             out_bin.data, rsa, crypt_opt.rsa_padding);
    else
        result =
            is_encrypt ? RSA_public_encrypt((int)in_bin.size, in_bin.data,
                                            out_bin.data, rsa, crypt_opt.rsa_padding)
                       : RSA_public_decrypt((int)in_bin.size, in_bin.data,
                                            out_bin.data, rsa, crypt_opt.rsa_padding);
    if (result > 0) {
        ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, result);
        if (!is_encrypt &&
            !enif_realloc_binary(&out_bin, (size_t)result))
            assign_goto(ret, err, EXCP_ERROR(env, "Can't re-allocate binary"));
    }

    outlen = (size_t)result;
#endif

    if ((result > 0) && argv[0] == atom_rsa && !is_encrypt) {
#ifdef HAVE_RSA_SSLV23_PADDING
        if (crypt_opt.rsa_padding == RSA_SSLV23_PADDING) {
            unsigned char *p;

            tmplen = size_of_RSA(pkey);
            if (tmplen < 1 || tmplen > INT_MAX)
                assign_goto(ret, err, EXCP_BADARG_N(env, 2, "RSA key of wrong size"));
            if (!enif_alloc_binary(tmplen, &tmp_bin))
                assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate binary"));
            tmp_bin_alloc = 1;
            if (out_bin.size > INT_MAX)
                assign_goto(ret, err, EXCP_ERROR(env, "Result too large"));

            p = out_bin.data;
            p++;

            result = RSA_padding_check_SSLv23(tmp_bin.data, (int)tmplen, p, (int)out_bin.size - 1, (int)tmplen);
            if (result >= 0) {
                outlen = (size_t)result;
                in_bin = out_bin;
                out_bin = tmp_bin;
                tmp_bin = in_bin;
                result = 1;
            }
        }
#endif
    }

    if (result > 0) {
        ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, outlen);
        if (outlen != out_bin.size) {
            if (!enif_realloc_binary(&out_bin, outlen))
                assign_goto(ret, err, EXCP_ERROR(env, "Can't re-allocate binary"));
            ERL_VALGRIND_ASSERT_MEM_DEFINED(out_bin.data, outlen);
        }
        ret = enif_make_binary(env, &out_bin);
        out_bin_alloc = 0;
    } else {
        assign_goto(ret, err, EXCP_ERROR(env, "RSA encrypt/decrypt failed"));
    }

 err:
    if (out_bin_alloc)
        enif_release_binary(&out_bin);
    if (tmp_bin_alloc)
        enif_release_binary(&tmp_bin);

#ifdef HAS_EVP_PKEY_CTX
    if (ctx)
        EVP_PKEY_CTX_free(ctx);
#else
    if (rsa)
        RSA_free(rsa);
#endif
    if (pkey)
        EVP_PKEY_free(pkey);

    if (label_copy)
        OPENSSL_free(label_copy);

    return ret;
    }

ERL_NIF_TERM privkey_to_pubkey_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ /* (Algorithm, PrivKey | KeyMap) */
    ERL_NIF_TERM ret;
    EVP_PKEY *pkey = NULL;

    ASSERT(argc == 2);

    if (!check_pkey_algorithm_type(env, 0, argv[0], &ret))
        goto err; /* An exception is present in ret */
    
    if (!get_pkey_private_key(env, argv, 0, 1, &pkey, &ret)) // handles engine
        goto err; /* An exception is present in ret */

    if (argv[0] == atom_rsa) {
        if (!rsa_privkey_to_pubkey(env, pkey, &ret))
            assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Couldn't get RSA public key from private key"));

#ifdef HAVE_DSA
    } else if (argv[0] == atom_dss) {
        if (!dss_privkey_to_pubkey(env, pkey, &ret))
            assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Couldn't get DSA public key from private key"));
#endif
    } else if (argv[0] == atom_ecdsa) {
#if defined(HAVE_EC)
        /* not yet implemented
          EC_KEY *ec = EVP_PKEY_get1_EC_KEY(pkey);
          if (ec) {
          / * Example of result:
               {
                 Curve =  {Field, Prime, Point, Order, CoFactor} =
                    {
                      Field =  {prime_field,<<255,...,255>>},
                      Prime = {<<255,...,252>>,
                               <<90,...,75>>,
                               <<196,...,144>>
                              },
                      Point =    <<4,...,245>>,
                      Order =    <<255,...,81>>,
                      CoFactor = <<1>>
                    },
                Key = <<151,...,62>>
                }
              or
              {
                Curve =
                    {characteristic_two_field,
                     M,
                     Basis = {tpbasis, _}
                           | {ppbasis, k1, k2, k3}
                    },
                Key
               }
        * /
            EVP_PKEY_free(pkey);
            return enif_make_list_from_array(env, ..., ...);
        */
#endif
        assign_goto(ret, err, EXCP_NOTSUP_N(env, 0, "ECDSA not implemented"));
    } else {
        ret = EXCP_BADARG_N(env, 0, "Bad algorithm");
    }

 err:
    if (pkey)
        EVP_PKEY_free(pkey);

    return ret;
}
