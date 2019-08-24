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

#include "pkey.h"
#include "bn.h"
#include "digest.h"
#include "dss.h"
#include "ec.h"
#include "eddsa.h"
#include "engine.h"
#include "rsa.h"

#define PKEY_BADARG -1
#define PKEY_NOTSUP 0
#define PKEY_OK 1

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


static int get_pkey_digest_type(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM type,
				const EVP_MD **md);
static int get_pkey_sign_digest(ErlNifEnv *env, ERL_NIF_TERM algorithm,
				ERL_NIF_TERM type, ERL_NIF_TERM data,
				unsigned char *md_value, const EVP_MD **mdp,
				unsigned char **tbsp, size_t *tbslenp);
static int get_pkey_sign_options(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM options,
                                 const EVP_MD *md, PKeySignOptions *opt);
static int get_pkey_private_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key, EVP_PKEY **pkey);
static int get_pkey_public_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key,
			       EVP_PKEY **pkey);
static int get_pkey_crypt_options(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM options,
				  PKeyCryptOptions *opt);
#ifdef HAVE_RSA_SSLV23_PADDING
static size_t size_of_RSA(EVP_PKEY *pkey);
#endif

static int get_pkey_digest_type(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM type,
				const EVP_MD **md)
{
    struct digest_type_t *digp = NULL;
    *md = NULL;

    if (type == atom_none && algorithm == atom_rsa)
        return PKEY_OK;
    if (algorithm == atom_eddsa) {
#ifdef HAVE_EDDSA
        if (!FIPS_mode()) return PKEY_OK;
#else
        return PKEY_NOTSUP;
#endif
    }
    if ((digp = get_digest_type(type)) == NULL)
        return PKEY_BADARG;
    if (digp->md.p == NULL)
        return PKEY_NOTSUP;

    *md = digp->md.p;
    return PKEY_OK;
}

static int get_pkey_sign_digest(ErlNifEnv *env, ERL_NIF_TERM algorithm,
				ERL_NIF_TERM type, ERL_NIF_TERM data,
				unsigned char *md_value, const EVP_MD **mdp,
				unsigned char **tbsp, size_t *tbslenp)
{
    int i, ret;
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

    if ((i = get_pkey_digest_type(env, algorithm, type, &md)) != PKEY_OK)
        return i;

    if (enif_get_tuple(env, data, &tpl_arity, &tpl_terms)) {
        if (tpl_arity != 2)
            goto bad_arg;
        if (tpl_terms[0] != atom_digest)
            goto bad_arg;
        if (!enif_inspect_iolist_as_binary(env, tpl_terms[1], &tbs_bin))
            goto bad_arg;
        if (tbs_bin.size > INT_MAX)
            goto bad_arg;
        if (md != NULL) {
            if ((int)tbs_bin.size != EVP_MD_size(md))
                goto bad_arg;
        }

        /* We have a digest (= hashed text) in tbs_bin */
	tbs = tbs_bin.data;
	tbslen = tbs_bin.size;
    } else if (md == NULL) {
        if (!enif_inspect_iolist_as_binary(env, data, &tbs_bin))
            goto bad_arg;

        /* md == NULL, that is no hashing because DigestType argument was atom_none */
	tbs = tbs_bin.data;
	tbslen = tbs_bin.size;
    } else {
        if (!enif_inspect_iolist_as_binary(env, data, &tbs_bin))
            goto bad_arg;

        /* We have the cleartext in tbs_bin and the hash algo info in md */
	tbs = md_value;

        if ((mdctx = EVP_MD_CTX_create()) == NULL)
            goto err;

        /* Looks well, now hash the plain text into a digest according to md */
        if (EVP_DigestInit_ex(mdctx, md, NULL) != 1)
            goto err;
        if (EVP_DigestUpdate(mdctx, tbs_bin.data, tbs_bin.size) != 1)
            goto err;
        if (EVP_DigestFinal_ex(mdctx, tbs, &tbsleni) != 1)
            goto err;

        tbslen = (size_t)tbsleni;
    }

    *mdp = md;
    *tbsp = tbs;
    *tbslenp = tbslen;

    ret = PKEY_OK;
    goto done;

 bad_arg:
 err:
    ret = PKEY_BADARG;

 done:
    if (mdctx)
        EVP_MD_CTX_destroy(mdctx);
    return ret;
}

static int get_pkey_sign_options(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM options,
                                 const EVP_MD *md, PKeySignOptions *opt)
{
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    const EVP_MD *opt_md;

    if (!enif_is_list(env, options))
        goto bad_arg;

    /* defaults */
    if (algorithm == atom_rsa) {
	opt->rsa_mgf1_md = NULL;
	opt->rsa_padding = RSA_PKCS1_PADDING;
	opt->rsa_pss_saltlen = -2;
    }

    if (enif_is_empty_list(env, options))
	return PKEY_OK;

    if (algorithm != atom_rsa)
        goto bad_arg;

    tail = options;
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        if (!enif_get_tuple(env, head, &tpl_arity, &tpl_terms))
            goto bad_arg;
        if (tpl_arity != 2)
            goto bad_arg;

        if (tpl_terms[0] == atom_rsa_mgf1_md && enif_is_atom(env, tpl_terms[1])) {
            int result;

            result = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
            if (result != PKEY_OK)
                return result;

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
                return PKEY_NOTSUP;
#endif

            } else if (tpl_terms[1] == atom_rsa_x931_padding) {
                opt->rsa_padding = RSA_X931_PADDING;

            } else if (tpl_terms[1] == atom_rsa_no_padding) {
                opt->rsa_padding = RSA_NO_PADDING;

            } else {
                goto bad_arg;
            }

        } else if (tpl_terms[0] == atom_rsa_pss_saltlen) {
            if (!enif_get_int(env, tpl_terms[1], &(opt->rsa_pss_saltlen)))
                goto bad_arg;
            if (opt->rsa_pss_saltlen < -2)
                goto bad_arg;

        } else {
            goto bad_arg;
        }
    }

    return PKEY_OK;

 bad_arg:
    return PKEY_BADARG;
}

static int get_pkey_private_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    EVP_PKEY *result = NULL;
    RSA *rsa = NULL;
#ifdef HAVE_DSA
    DSA *dsa = NULL;
#endif
#if defined(HAVE_EC)
    EC_KEY *ec = NULL;
#endif
    char *id = NULL;
    char *password = NULL;

    if (enif_is_map(env, key)) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;

        if (!get_engine_and_key_id(env, key, &id, &e))
            goto err;

        password = get_key_password(env, key);
        result = ENGINE_load_private_key(e, id, NULL, password);

#else
        return PKEY_BADARG;
#endif
    } else if (algorithm == atom_rsa) {
        if ((rsa = RSA_new()) == NULL)
            goto err;

        if (!get_rsa_private_key(env, key, rsa))
            goto err;
        if ((result = EVP_PKEY_new()) == NULL)
            goto err;
        if (EVP_PKEY_assign_RSA(result, rsa) != 1)
            goto err;
        /* On success, result owns rsa */
        rsa = NULL;

    } else if (algorithm == atom_ecdsa) {
#if defined(HAVE_EC)
	const ERL_NIF_TERM *tpl_terms;
	int tpl_arity;

        if (!enif_get_tuple(env, key, &tpl_arity, &tpl_terms))
            goto err;
        if (tpl_arity != 2)
            goto err;
        if (!enif_is_tuple(env, tpl_terms[0]))
            goto err;
        if (!enif_is_binary(env, tpl_terms[1]))
            goto err;
        if (!get_ec_key(env, tpl_terms[0], tpl_terms[1], atom_undefined, &ec))
            goto err;

        if ((result = EVP_PKEY_new()) == NULL)
            goto err;
        if (EVP_PKEY_assign_EC_KEY(result, ec) != 1)
            goto err;
        /* On success, result owns ec */
        ec = NULL;

#else
	return PKEY_NOTSUP;
#endif
    } else if (algorithm == atom_eddsa) {
#ifdef HAVE_EDDSA
        if (!FIPS_mode())
            {
                if (!get_eddsa_key(env, 0, key, &result))
                    goto err;
                else
                    goto done; // Not nice....
            }
#else
            return PKEY_NOTSUP;
#endif
    } else if (algorithm == atom_dss) {
#ifdef HAVE_DSA
        if ((dsa = DSA_new()) == NULL)
            goto err;
        if (!get_dss_private_key(env, key, dsa))
            goto err;

        if ((result = EVP_PKEY_new()) == NULL)
            goto err;
        if (EVP_PKEY_assign_DSA(result, dsa) != 1)
            goto err;
        /* On success, result owns dsa */
        dsa = NULL;

    } else {
#endif
	return PKEY_BADARG;
    }
    goto done;

 err:
    if (result)
        EVP_PKEY_free(result);
    result = NULL;

 done:
    if (password)
        enif_free(password);
    if (id)
        enif_free(id);
    if (rsa)
        RSA_free(rsa);
#ifdef HAVE_DSA
    if (dsa)
        DSA_free(dsa);
#endif
#ifdef HAVE_EC
    if (ec)
        EC_KEY_free(ec);
#endif

    if (result == NULL) {
        return PKEY_BADARG;
    } else {
        *pkey = result;
        return PKEY_OK;
    }
}

static int get_pkey_public_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key,
			       EVP_PKEY **pkey)
{
    EVP_PKEY *result = NULL;
    RSA *rsa = NULL;
#ifdef HAVE_DSA
    DSA *dsa = NULL;
#endif
#if defined(HAVE_EC)
    EC_KEY *ec = NULL;
#endif
    char *id = NULL;
    char *password = NULL;

    if (enif_is_map(env, key)) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;

        if (!get_engine_and_key_id(env, key, &id, &e))
            goto err;

        password = get_key_password(env, key);
        result = ENGINE_load_public_key(e, id, NULL, password);

#else
        return PKEY_BADARG;
#endif
    } else  if (algorithm == atom_rsa) {
        if ((rsa = RSA_new()) == NULL)
            goto err;

        if (!get_rsa_public_key(env, key, rsa))
            goto err;

        if ((result = EVP_PKEY_new()) == NULL)
            goto err;
        if (EVP_PKEY_assign_RSA(result, rsa) != 1)
            goto err;
        /* On success, result owns rsa */
        rsa = NULL;

    } else if (algorithm == atom_ecdsa) {
#if defined(HAVE_EC)
	const ERL_NIF_TERM *tpl_terms;
	int tpl_arity;

        if (!enif_get_tuple(env, key, &tpl_arity, &tpl_terms))
            goto err;
        if (tpl_arity != 2)
            goto err;
        if (!enif_is_tuple(env, tpl_terms[0]))
            goto err;
        if (!enif_is_binary(env, tpl_terms[1]))
            goto err;
        if (!get_ec_key(env, tpl_terms[0], atom_undefined, tpl_terms[1], &ec))
            goto err;

        if ((result = EVP_PKEY_new()) == NULL)
            goto err;

        if (EVP_PKEY_assign_EC_KEY(result, ec) != 1)
            goto err;
        /* On success, result owns ec */
        ec = NULL;

#else
	return PKEY_NOTSUP;
#endif
    } else if (algorithm == atom_eddsa) {
#ifdef HAVE_EDDSA
        if (!FIPS_mode()) {
            if (!get_eddsa_key(env, 1, key, &result))
                goto err;
        }
#else
	return PKEY_NOTSUP;
#endif
    } else if (algorithm == atom_dss) {
#ifdef HAVE_DSA
        if ((dsa = DSA_new()) == NULL)
            goto err;

        if (!get_dss_public_key(env, key, dsa))
            goto err;

        if ((result = EVP_PKEY_new()) == NULL)
            goto err;
        if (EVP_PKEY_assign_DSA(result, dsa) != 1)
            goto err;
        /* On success, result owns dsa */
        dsa = NULL;
#else
        return PKEY_NOTSUP;
#endif
    } else {
	return PKEY_BADARG;
    }

    goto done;

 err:
    if (result)
        EVP_PKEY_free(result);
    result = NULL;

 done:
    if (password)
        enif_free(password);
    if (id)
        enif_free(id);
    if (rsa)
        RSA_free(rsa);
#ifdef HAVE_DSA
    if (dsa)
        DSA_free(dsa);
#endif
#ifdef HAVE_EC
    if (ec)
        EC_KEY_free(ec);
#endif

    if (result == NULL) {
        return PKEY_BADARG;
    } else {
        *pkey = result;
        return PKEY_OK;
    }
}

ERL_NIF_TERM pkey_sign_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Type, Data|{digest,Digest}, Key|#{}, Options) */
    int i;
    int sig_bin_alloc = 0;
    ERL_NIF_TERM ret;
    const EVP_MD *md = NULL;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    EVP_PKEY *pkey = NULL;
#ifdef HAVE_EDDSA
    EVP_MD_CTX *mdctx = NULL;
#endif
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx = NULL;
    size_t siglen;
#else
    int len;
    unsigned int siglen;
#endif
    PKeySignOptions sig_opt;
    ErlNifBinary sig_bin; /* signature */
    unsigned char *tbs; /* data to be signed */
    size_t tbslen;
    RSA *rsa = NULL;
#ifdef HAVE_DSA
    DSA *dsa = NULL;
#endif
#if defined(HAVE_EC)
    EC_KEY *ec = NULL;
#endif
/*char buf[1024];
enif_get_atom(env,argv[0],buf,1024,ERL_NIF_LATIN1); printf("algo=%s ",buf);
enif_get_atom(env,argv[1],buf,1024,ERL_NIF_LATIN1); printf("hash=%s ",buf);
*/

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[3]))
        return atom_notsup;
#endif

    i = get_pkey_sign_digest(env, argv[0], argv[1], argv[2], md_value, &md, &tbs, &tbslen);
    switch (i) {
    case PKEY_OK:
        break;
    case PKEY_NOTSUP:
        goto notsup;
    default:
        goto bad_arg;
    }

    i = get_pkey_sign_options(env, argv[0], argv[4], md, &sig_opt);
    switch (i) {
    case PKEY_OK:
        break;
    case PKEY_NOTSUP:
        goto notsup;
    default:
        goto bad_arg;
    }

    if (get_pkey_private_key(env, argv[0], argv[3], &pkey) != PKEY_OK)
        goto bad_arg;

#ifdef HAS_EVP_PKEY_CTX
    if ((ctx = EVP_PKEY_CTX_new(pkey, NULL)) == NULL)
        goto err;

    if (argv[0] != atom_eddsa) {
        if (EVP_PKEY_sign_init(ctx) != 1)
            goto err;
        if (md != NULL) {
            if (EVP_PKEY_CTX_set_signature_md(ctx, md) != 1)
                goto err;
        }
    }

    if (argv[0] == atom_rsa) {
        if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) != 1)
            goto err;
# ifdef HAVE_RSA_PKCS1_PSS_PADDING
	if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
            if (sig_opt.rsa_mgf1_md != NULL) {
# ifdef HAVE_RSA_MGF1_MD
                if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) != 1)
                    goto err;
# else
                goto notsup;
# endif
            }
            if (sig_opt.rsa_pss_saltlen > -2) {
                if (EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) != 1)
                    goto err;
            }
        }
#endif
    }

    if (argv[0] == atom_eddsa) {
#ifdef HAVE_EDDSA
        if (!FIPS_mode()) {
            if ((mdctx = EVP_MD_CTX_new()) == NULL)
                goto err;

            if (EVP_DigestSignInit(mdctx, NULL, NULL, NULL, pkey) != 1)
                goto err;
            if (EVP_DigestSign(mdctx, NULL, &siglen, tbs, tbslen) != 1)
                goto err;
            if (!enif_alloc_binary(siglen, &sig_bin))
                goto err;
            sig_bin_alloc = 1;

            if (EVP_DigestSign(mdctx, sig_bin.data, &siglen, tbs, tbslen) != 1)
                goto bad_key;
        }
        else
#endif
            goto notsup;
    } else {
        if (EVP_PKEY_sign(ctx, NULL, &siglen, tbs, tbslen) != 1)
            goto err;
        if (!enif_alloc_binary(siglen, &sig_bin))
            goto err;
        sig_bin_alloc = 1;

        if (md != NULL) {
            ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
        }
        if (EVP_PKEY_sign(ctx, sig_bin.data, &siglen, tbs, tbslen) != 1)
            goto bad_key;
    }
#else
/*printf("Old interface\r\n");
 */
    if (argv[0] == atom_rsa) {
        if ((rsa = EVP_PKEY_get1_RSA(pkey)) == NULL)
            goto err;
        if ((len = RSA_size(rsa)) < 0)
            goto err;
        if (!enif_alloc_binary((size_t)len, &sig_bin))
            goto err;
        sig_bin_alloc = 1;

        if ((len = EVP_MD_size(md)) < 0)
            goto err;
        ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);

        if (RSA_sign(md->type, tbs, (unsigned int)len, sig_bin.data, &siglen, rsa) != 1)
            goto bad_key;
    } else if (argv[0] == atom_dss) {
        if ((dsa = EVP_PKEY_get1_DSA(pkey)) == NULL)
            goto err;
        if ((len = DSA_size(dsa)) < 0)
            goto err;
        if (!enif_alloc_binary((size_t)len, &sig_bin))
            goto err;
        sig_bin_alloc = 1;

        if ((len = EVP_MD_size(md)) < 0)
            goto err;
        ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);

        if (DSA_sign(md->type, tbs, len, sig_bin.data, &siglen, dsa) != 1)
            goto bad_key;
    } else if (argv[0] == atom_ecdsa) {
#if defined(HAVE_EC)
        if ((ec = EVP_PKEY_get1_EC_KEY(pkey)) == NULL)
            goto err;
        if ((len = ECDSA_size(ec)) < 0)
            goto err;
        if (!enif_alloc_binary((size_t)len, &sig_bin))
            goto err;
        sig_bin_alloc = 1;

        len = EVP_MD_size(md);
        ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);

        if (ECDSA_sign(md->type, tbs, len, sig_bin.data, &siglen, ec) != 1)
            goto bad_key;
#else
        goto notsup;
#endif
    } else {
        goto bad_arg;
    }
#endif

    ERL_VALGRIND_MAKE_MEM_DEFINED(sig_bin.data, siglen);
    if (siglen != sig_bin.size) {
        if (!enif_realloc_binary(&sig_bin, siglen))
            goto err;
        ERL_VALGRIND_ASSERT_MEM_DEFINED(sig_bin.data, siglen);
    }
    ret = enif_make_binary(env, &sig_bin);
    sig_bin_alloc = 0;
    goto done;

 bad_key:
    ret = atom_error;
    goto done;

 notsup:
    ret = atom_notsup;
    goto done;

 bad_arg:
 err:
    ret = enif_make_badarg(env);
    goto done;

 done:
    if (sig_bin_alloc)
        enif_release_binary(&sig_bin);
    if (rsa)
        RSA_free(rsa);
#ifdef HAVE_DSA
    if (dsa)
        DSA_free(dsa);
#endif
#ifdef HAVE_EC
    if (ec)
        EC_KEY_free(ec);
#endif
#ifdef HAS_EVP_PKEY_CTX
    if (ctx)
        EVP_PKEY_CTX_free(ctx);
#endif
    if (pkey)
        EVP_PKEY_free(pkey);

#ifdef HAVE_EDDSA
    if (mdctx)
        EVP_MD_CTX_free(mdctx);
#endif

    return ret;
}

ERL_NIF_TERM pkey_verify_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Type, Data|{digest,Digest}, Signature, Key, Options) */
    int i;
    int result;
    const EVP_MD *md = NULL;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    EVP_PKEY *pkey = NULL;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx = NULL;
#else
#endif
    PKeySignOptions sig_opt;
    ErlNifBinary sig_bin; /* signature */
    unsigned char *tbs; /* data to be signed */
    size_t tbslen;
    ERL_NIF_TERM ret;
    RSA *rsa = NULL;
#ifdef HAVE_DSA
    DSA *dsa = NULL;
#endif
#ifdef HAVE_EC
    EC_KEY *ec = NULL;
#endif
#ifdef HAVE_EDDSA
    EVP_MD_CTX *mdctx = NULL;
#endif

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[4]))
        return atom_notsup;
#endif

    if (!enif_inspect_binary(env, argv[3], &sig_bin))
	return enif_make_badarg(env);

    i = get_pkey_sign_digest(env, argv[0], argv[1], argv[2], md_value, &md, &tbs, &tbslen);
    switch (i) {
    case PKEY_OK:
        break;
    case PKEY_NOTSUP:
        goto notsup;
    default:
        goto bad_arg;
    }

    i = get_pkey_sign_options(env, argv[0], argv[5], md, &sig_opt);
    switch (i) {
    case PKEY_OK:
        break;
    case PKEY_NOTSUP:
        goto notsup;
    default:
        goto bad_arg;
    }

    if (get_pkey_public_key(env, argv[0], argv[4], &pkey) != PKEY_OK) {
        goto bad_arg;
    }

#ifdef HAS_EVP_PKEY_CTX
/* printf("EVP interface\r\n");
 */
    if ((ctx = EVP_PKEY_CTX_new(pkey, NULL)) == NULL)
        goto err;

    if (argv[0] != atom_eddsa) {
        if (EVP_PKEY_verify_init(ctx) != 1)
            goto err;
        if (md != NULL) {
            if (EVP_PKEY_CTX_set_signature_md(ctx, md) != 1)
                goto err;
        }
    }

    if (argv[0] == atom_rsa) {
        if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) != 1)
            goto err;
        if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
            if (sig_opt.rsa_mgf1_md != NULL) {
# ifdef HAVE_RSA_MGF1_MD
                if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) != 1)
                    goto err;
# else
                goto notsup;
# endif
            }
            if (sig_opt.rsa_pss_saltlen > -2) {
                if (EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) != 1)
                    goto err;
            }
        }
    }

    if (argv[0] == atom_eddsa) {
#ifdef HAVE_EDDSA
        if (!FIPS_mode()) {
            if ((mdctx = EVP_MD_CTX_new()) == NULL)
                goto err;

            if (EVP_DigestVerifyInit(mdctx, NULL, NULL, NULL, pkey) != 1)
                goto err;

            result = EVP_DigestVerify(mdctx, sig_bin.data, sig_bin.size, tbs, tbslen);
        }
        else
#endif
        goto notsup;
    } else {
        if (md != NULL) {
            ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
        }
        result = EVP_PKEY_verify(ctx, sig_bin.data, sig_bin.size, tbs, tbslen);
    }
#else
/*printf("Old interface\r\n");
*/
    if (tbslen > INT_MAX)
        goto bad_arg;
    if (sig_bin.size > INT_MAX)
        goto bad_arg;
    if (argv[0] == atom_rsa) {
        if ((rsa = EVP_PKEY_get1_RSA(pkey)) == NULL)
            goto err;
        result = RSA_verify(md->type, tbs, (unsigned int)tbslen, sig_bin.data, (unsigned int)sig_bin.size, rsa);
    } else if (argv[0] == atom_dss) {
        if ((dsa = EVP_PKEY_get1_DSA(pkey)) == NULL)
            goto err;
        result = DSA_verify(0, tbs, (int)tbslen, sig_bin.data, (int)sig_bin.size, dsa);
    } else if (argv[0] == atom_ecdsa) {
#if defined(HAVE_EC)
        if ((ec = EVP_PKEY_get1_EC_KEY(pkey)) == NULL)
            goto err;
        result = ECDSA_verify(EVP_MD_type(md), tbs, (int)tbslen, sig_bin.data, (int)sig_bin.size, ec);
#else
        goto notsup;
#endif
    } else {
        goto bad_arg;
    }
#endif

    ret = (result == 1 ? atom_true : atom_false);
    goto done;

 bad_arg:
 err:
    ret = enif_make_badarg(env);
    goto done;

 notsup:
    ret = atom_notsup;

 done:
#ifdef HAS_EVP_PKEY_CTX
    if (ctx)
        EVP_PKEY_CTX_free(ctx);
#endif
#ifdef HAVE_EDDSA
    if (mdctx)
        EVP_MD_CTX_free(mdctx);
#endif
    if (pkey)
        EVP_PKEY_free(pkey);
    if (rsa)
        RSA_free(rsa);
#ifdef HAVE_DSA
    if (dsa)
        DSA_free(dsa);
#endif
#ifdef HAVE_EC
    if (ec)
        EC_KEY_free(ec);
#endif

    return ret;
}

static int get_pkey_crypt_options(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM options,
				  PKeyCryptOptions *opt)
{
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    const EVP_MD *opt_md;

    if (!enif_is_list(env, options))
        goto bad_arg;

    /* defaults */
    if (algorithm == atom_rsa) {
	opt->rsa_mgf1_md = NULL;
	opt->rsa_oaep_label.data = NULL;
	opt->rsa_oaep_label.size = 0;
	opt->rsa_oaep_md = NULL;
	opt->rsa_padding = RSA_PKCS1_PADDING;
	opt->signature_md = NULL;
    }

    if (enif_is_empty_list(env, options))
        return PKEY_OK;

    if (algorithm != atom_rsa)
        goto bad_arg;

    tail = options;
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        if (!enif_get_tuple(env, head, &tpl_arity, &tpl_terms))
            goto bad_arg;
        if (tpl_arity != 2)
            goto bad_arg;

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

            } else {
                goto bad_arg;
            }

        } else if (tpl_terms[0] == atom_signature_md && enif_is_atom(env, tpl_terms[1])) {
            int i;
            i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
            if (i != PKEY_OK) {
                return i;
            }
            opt->signature_md = opt_md;

        } else if (tpl_terms[0] == atom_rsa_mgf1_md && enif_is_atom(env, tpl_terms[1])) {
            int i;
#ifndef HAVE_RSA_MGF1_MD
            if (tpl_terms[1] != atom_sha)
                return PKEY_NOTSUP;
#endif
            i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
            if (i != PKEY_OK) {
                return i;
            }
            opt->rsa_mgf1_md = opt_md;

        } else if (tpl_terms[0] == atom_rsa_oaep_label
                   && enif_inspect_binary(env, tpl_terms[1], &(opt->rsa_oaep_label))) {
#ifdef HAVE_RSA_OAEP_MD
            continue;
#else
            return PKEY_NOTSUP;
#endif

        } else if (tpl_terms[0] == atom_rsa_oaep_md && enif_is_atom(env, tpl_terms[1])) {
            int i;
#ifndef HAVE_RSA_OAEP_MD
            if (tpl_terms[1] != atom_sha)
                return PKEY_NOTSUP;
#endif
            i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
            if (i != PKEY_OK) {
                return i;
            }
            opt->rsa_oaep_md = opt_md;

        } else {
            goto bad_arg;
        }
    }

    return PKEY_OK;

 bad_arg:
    return PKEY_BADARG;
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
    ERL_NIF_TERM ret;
    int i;
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
    int algo_init = 0;
    unsigned char *label_copy = NULL;

    ASSERT(argc == 6);

    is_private = (argv[4] == atom_true);
    is_encrypt = (argv[5] == atom_true);

/* char algo[1024]; */

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[2]))
        return atom_notsup;
#endif

    if (!enif_inspect_binary(env, argv[1], &in_bin))
        goto bad_arg;

    i = get_pkey_crypt_options(env, argv[0], argv[3], &crypt_opt);
    switch (i) {
    case PKEY_OK:
        break;
    case PKEY_NOTSUP:
        goto notsup;
    default:
        goto bad_arg;
    }

    if (is_private) {
        if (get_pkey_private_key(env, argv[0], argv[2], &pkey) != PKEY_OK)
            goto bad_arg;
    } else {
        if (get_pkey_public_key(env, argv[0], argv[2], &pkey) != PKEY_OK)
            goto bad_arg;
    }

#ifdef HAS_EVP_PKEY_CTX
    if ((ctx = EVP_PKEY_CTX_new(pkey, NULL)) == NULL)
        goto err;

/* enif_get_atom(env,argv[0],algo,1024,ERL_NIF_LATIN1);  */

    if (is_private) {
        if (is_encrypt) {
            /* private encrypt */
            if ((algo_init = EVP_PKEY_sign_init(ctx)) != 1)
                goto bad_arg;
        } else {
            /* private decrypt */
            if ((algo_init = EVP_PKEY_decrypt_init(ctx)) != 1)
                goto bad_arg;
        }
    } else {
        if (is_encrypt) {
            /* public encrypt */
            if ((algo_init = EVP_PKEY_encrypt_init(ctx)) != 1)
                goto bad_arg;
        } else {
            /* public decrypt */
            if ((algo_init = EVP_PKEY_verify_recover_init(ctx)) != 1)
                goto bad_arg;
        }
    }

    if (argv[0] == atom_rsa) {
        if (crypt_opt.signature_md != NULL) {
            if (EVP_PKEY_CTX_set_signature_md(ctx, crypt_opt.signature_md) != 1)
                goto bad_arg;
        }

#ifdef HAVE_RSA_SSLV23_PADDING
        if (crypt_opt.rsa_padding == RSA_SSLV23_PADDING) {
            if (is_encrypt) {
                tmplen = size_of_RSA(pkey);
                if (tmplen < 1 || tmplen > INT_MAX)
                    goto err;
                if (!enif_alloc_binary(tmplen, &tmp_bin))
                    goto err;
                tmp_bin_alloc = 1;
                if (in_bin.size > INT_MAX)
                    goto err;
                if (!RSA_padding_add_SSLv23(tmp_bin.data, (int)tmplen, in_bin.data, (int)in_bin.size))
                    goto err;
                in_bin = tmp_bin;
            }
            if (EVP_PKEY_CTX_set_rsa_padding(ctx, RSA_NO_PADDING) != 1)
                goto err;
        } else
#endif
        {
            if (EVP_PKEY_CTX_set_rsa_padding(ctx, crypt_opt.rsa_padding) != 1)
                goto err;
        }

#ifdef HAVE_RSA_OAEP_MD
        if (crypt_opt.rsa_padding == RSA_PKCS1_OAEP_PADDING) {
            if (crypt_opt.rsa_oaep_md != NULL) {
                if (EVP_PKEY_CTX_set_rsa_oaep_md(ctx, crypt_opt.rsa_oaep_md) != 1)
                    goto err;
            }

            if (crypt_opt.rsa_mgf1_md != NULL) {
                if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, crypt_opt.rsa_mgf1_md) != 1)
                    goto err;
            }

            if (crypt_opt.rsa_oaep_label.data != NULL && crypt_opt.rsa_oaep_label.size > 0) {
                if (crypt_opt.rsa_oaep_label.size > INT_MAX)
                    goto err;
                if ((label_copy = OPENSSL_malloc(crypt_opt.rsa_oaep_label.size)) == NULL)
                    goto err;

                memcpy((void *)(label_copy), (const void *)(crypt_opt.rsa_oaep_label.data),
                       crypt_opt.rsa_oaep_label.size);

                if (EVP_PKEY_CTX_set0_rsa_oaep_label(ctx, label_copy,
                                                     (int)crypt_opt.rsa_oaep_label.size) != 1)
                    goto err;
                /* On success, label_copy is owned by ctx */
                label_copy = NULL;
            }
        }
#endif
    }

    if (is_private) {
        if (is_encrypt) {
            /* private_encrypt */
            result = EVP_PKEY_sign(ctx, NULL, &outlen, in_bin.data, in_bin.size);
        } else {
            /* private_decrypt */
            result = EVP_PKEY_decrypt(ctx, NULL, &outlen, in_bin.data, in_bin.size);
        }
    } else {
        if (is_encrypt) {
            /* public_encrypt */
            result = EVP_PKEY_encrypt(ctx, NULL, &outlen, in_bin.data, in_bin.size);
        } else {
            /* public_decrypt */
            result = EVP_PKEY_verify_recover(ctx, NULL, &outlen, in_bin.data, in_bin.size);
        }
    }
    /* fprintf(stderr,"i = %d %s:%d\r\n", i, __FILE__, __LINE__); */

    if (result != 1)
        goto err;

    if (!enif_alloc_binary(outlen, &out_bin))
        goto err;
    out_bin_alloc = 1;

    if (is_private) {
        if (is_encrypt) {
            /* private_encrypt */
            result = EVP_PKEY_sign(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
        } else {
            /* private_decrypt */
            result = EVP_PKEY_decrypt(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
        }
    } else {
        if (is_encrypt) {
            /* public_encrypt */
            result = EVP_PKEY_encrypt(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
        } else {
            /* public_decrypt */
            result = EVP_PKEY_verify_recover(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
        }
    }

#else
    /* Non-EVP cryptolib. Only support RSA */

    if (argv[0] != atom_rsa) {
        algo_init = -2;         /* exitcode: notsup */
        goto bad_arg;
    }

    if ((rsa = EVP_PKEY_get1_RSA(pkey)) == NULL)
        goto err;
    if ((len = RSA_size(rsa)) < 0)
        goto err;
    if (!enif_alloc_binary((size_t)len, &out_bin))
        goto err;
    out_bin_alloc = 1;

    if (in_bin.size > INT_MAX)
        goto err;
    if (is_private) {
        if (is_encrypt) {
            /* non-evp rsa private encrypt */
            ERL_VALGRIND_ASSERT_MEM_DEFINED(in_bin.data,in_bin.size);
            result = RSA_private_encrypt((int)in_bin.size, in_bin.data,
                                    out_bin.data, rsa, crypt_opt.rsa_padding);
            if (result > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, result);
            }
        } else {
            /* non-evp rsa private decrypt */
            result = RSA_private_decrypt((int)in_bin.size, in_bin.data,
                                    out_bin.data, rsa, crypt_opt.rsa_padding);
            if (result > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, result);
                if (!enif_realloc_binary(&out_bin, (size_t)result))
                    goto err;
            }
        }
    } else {
        if (is_encrypt) {
            /* non-evp rsa public encrypt */
            ERL_VALGRIND_ASSERT_MEM_DEFINED(in_bin.data,in_bin.size);
            result = RSA_public_encrypt((int)in_bin.size, in_bin.data,
                                   out_bin.data, rsa, crypt_opt.rsa_padding);
            if (result > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, result);
            }
        } else {
            /* non-evp rsa public decrypt */
            result = RSA_public_decrypt((int)in_bin.size, in_bin.data,
                                   out_bin.data, rsa, crypt_opt.rsa_padding);
            if (result > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, result);
                if (!enif_realloc_binary(&out_bin, (size_t)result))
                    goto err;
            }
        }
    }

    outlen = (size_t)result;
#endif

    if ((result > 0) && argv[0] == atom_rsa && !is_encrypt) {
#ifdef HAVE_RSA_SSLV23_PADDING
        if (crypt_opt.rsa_padding == RSA_SSLV23_PADDING) {
            unsigned char *p;

            tmplen = size_of_RSA(pkey);
            if (tmplen < 1 || tmplen > INT_MAX)
                goto err;
            if (!enif_alloc_binary(tmplen, &tmp_bin))
                goto err;
            tmp_bin_alloc = 1;
            if (out_bin.size > INT_MAX)
                goto err;

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
                goto err;
            ERL_VALGRIND_ASSERT_MEM_DEFINED(out_bin.data, outlen);
        }
        ret = enif_make_binary(env, &out_bin);
        out_bin_alloc = 0;
    } else {
        ret = atom_error;
    }
    goto done;

 notsup:
    ret = atom_notsup;
    goto done;

 bad_arg:
 err:
    if (algo_init == -2)
        ret = atom_notsup;
    else
        ret = enif_make_badarg(env);

 done:
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
    RSA *rsa = NULL;
#ifdef HAVE_DSA
    DSA *dsa = NULL;
#endif
    ERL_NIF_TERM result[8];

    ASSERT(argc == 2);

    if (get_pkey_private_key(env, argv[0], argv[1], &pkey) != PKEY_OK)
        goto bad_arg;

    if (argv[0] == atom_rsa) {
        const BIGNUM *n = NULL, *e = NULL, *d = NULL;

        if ((rsa = EVP_PKEY_get1_RSA(pkey)) == NULL)
            goto err;

        RSA_get0_key(rsa, &n, &e, &d);

        // Exponent E
        if ((result[0] = bin_from_bn(env, e)) == atom_error)
            goto err;
        // Modulus N = p*q
        if ((result[1] = bin_from_bn(env, n)) == atom_error)
            goto err;

        ret = enif_make_list_from_array(env, result, 2);

#ifdef HAVE_DSA
    } else if (argv[0] == atom_dss) {
        const BIGNUM *p = NULL, *q = NULL, *g = NULL, *pub_key = NULL;

        if ((dsa = EVP_PKEY_get1_DSA(pkey)) == NULL)
            goto err;

        DSA_get0_pqg(dsa, &p, &q, &g);
        DSA_get0_key(dsa, &pub_key, NULL);

        if ((result[0] = bin_from_bn(env, p)) == atom_error)
            goto err;
        if ((result[1] = bin_from_bn(env, q)) == atom_error)
            goto err;
        if ((result[2] = bin_from_bn(env, g)) == atom_error)
            goto err;
        if ((result[3] = bin_from_bn(env, pub_key)) == atom_error)
            goto err;

        ret = enif_make_list_from_array(env, result, 4);
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
        goto bad_arg;
    } else {
        goto bad_arg;
    }

    goto done;

 bad_arg:
 err:
    ret = enif_make_badarg(env);

 done:
    if (rsa)
        RSA_free(rsa);
#ifdef HAVE_DSA
    if (dsa)
        DSA_free(dsa);
#endif
    if (pkey)
        EVP_PKEY_free(pkey);

    return ret;
}
