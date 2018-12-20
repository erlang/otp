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
static size_t size_of_RSA(EVP_PKEY *pkey);


static int get_pkey_digest_type(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM type,
				const EVP_MD **md)
{
    struct digest_type_t *digp = NULL;
    *md = NULL;

    if (type == atom_none && algorithm == atom_rsa) return PKEY_OK;
#ifdef HAVE_EDDSA
    if (algorithm == atom_eddsa) return PKEY_OK;
#endif
    digp = get_digest_type(type);
    if (!digp) return PKEY_BADARG;
    if (!digp->md.p) return PKEY_NOTSUP;

    *md = digp->md.p;
    return PKEY_OK;
}

static int get_pkey_sign_digest(ErlNifEnv *env, ERL_NIF_TERM algorithm,
				ERL_NIF_TERM type, ERL_NIF_TERM data,
				unsigned char *md_value, const EVP_MD **mdp,
				unsigned char **tbsp, size_t *tbslenp)
{
    int i;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    ErlNifBinary tbs_bin;
    EVP_MD_CTX *mdctx;
    const EVP_MD *md = *mdp;
    unsigned char *tbs = *tbsp;
    size_t tbslen = *tbslenp;
    unsigned int tbsleni;

    if ((i = get_pkey_digest_type(env, algorithm, type, &md)) != PKEY_OK) {
	return i;
    }
    if (enif_get_tuple(env, data, &tpl_arity, &tpl_terms)) {
	if (tpl_arity != 2 || tpl_terms[0] != atom_digest
	    || !enif_inspect_binary(env, tpl_terms[1], &tbs_bin)
	    || (md != NULL && tbs_bin.size != EVP_MD_size(md))) {
	    return PKEY_BADARG;
	}
        /* We have a digest (= hashed text) in tbs_bin */
	tbs = tbs_bin.data;
	tbslen = tbs_bin.size;
    } else if (md == NULL) {
	if (!enif_inspect_binary(env, data, &tbs_bin)) {
	    return PKEY_BADARG;
	}
        /* md == NULL, that is no hashing because DigestType argument was atom_none */
	tbs = tbs_bin.data;
	tbslen = tbs_bin.size;
    } else {
	if (!enif_inspect_binary(env, data, &tbs_bin)) {
	    return PKEY_BADARG;
	}
        /* We have the cleartext in tbs_bin and the hash algo info in md */
	tbs = md_value;
	mdctx = EVP_MD_CTX_create();
	if (!mdctx) {
	    return PKEY_BADARG;
	}
        /* Looks well, now hash the plain text into a digest according to md */
	if (EVP_DigestInit_ex(mdctx, md, NULL) <= 0) {
	    EVP_MD_CTX_destroy(mdctx);
	    return PKEY_BADARG;
	}
	if (EVP_DigestUpdate(mdctx, tbs_bin.data, tbs_bin.size) <= 0) {
	    EVP_MD_CTX_destroy(mdctx);
	    return PKEY_BADARG;
	}
	if (EVP_DigestFinal_ex(mdctx, tbs, &tbsleni) <= 0) {
	    EVP_MD_CTX_destroy(mdctx);
	    return PKEY_BADARG;
	}
	tbslen = (size_t)(tbsleni);
	EVP_MD_CTX_destroy(mdctx);
    }

    *mdp = md;
    *tbsp = tbs;
    *tbslenp = tbslen;

    return PKEY_OK;
}

static int get_pkey_sign_options(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM options,
                                 const EVP_MD *md, PKeySignOptions *opt)
{
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    const EVP_MD *opt_md;
    int i;

    if (!enif_is_list(env, options)) {
	return PKEY_BADARG;
    }

    /* defaults */
    if (algorithm == atom_rsa) {
	opt->rsa_mgf1_md = NULL;
	opt->rsa_padding = RSA_PKCS1_PADDING;
	opt->rsa_pss_saltlen = -2;
    }

    if (enif_is_empty_list(env, options)) {
	return PKEY_OK;
    }

    if (algorithm == atom_rsa) {
	tail = options;
	while (enif_get_list_cell(env, tail, &head, &tail)) {
	    if (enif_get_tuple(env, head, &tpl_arity, &tpl_terms) && tpl_arity == 2) {
		if (tpl_terms[0] == atom_rsa_mgf1_md && enif_is_atom(env, tpl_terms[1])) {
		    i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
		    if (i != PKEY_OK) {
			return i;
		    }
		    opt->rsa_mgf1_md = opt_md;
		} else if (tpl_terms[0] == atom_rsa_padding) {
		    if (tpl_terms[1] == atom_rsa_pkcs1_padding) {
			opt->rsa_padding = RSA_PKCS1_PADDING;
                    } else if (tpl_terms[1] == atom_rsa_pkcs1_pss_padding) {
#ifdef HAVE_RSA_PKCS1_PSS_PADDING
                        opt->rsa_padding = RSA_PKCS1_PSS_PADDING;
                        if (opt->rsa_mgf1_md == NULL) {
                            opt->rsa_mgf1_md = md;
                        }
#else
                        return PKEY_NOTSUP;
#endif
		    } else if (tpl_terms[1] == atom_rsa_x931_padding) {
			opt->rsa_padding = RSA_X931_PADDING;
		    } else if (tpl_terms[1] == atom_rsa_no_padding) {
			opt->rsa_padding = RSA_NO_PADDING;
		    } else {
			return PKEY_BADARG;
		    }
		} else if (tpl_terms[0] == atom_rsa_pss_saltlen) {
		    if (!enif_get_int(env, tpl_terms[1], &(opt->rsa_pss_saltlen))
			|| opt->rsa_pss_saltlen < -2) {
			return PKEY_BADARG;
		    }
		} else {
		    return PKEY_BADARG;
		}
	    } else {
		return PKEY_BADARG;
	    }
	}
    } else {
	return PKEY_BADARG;
    }

    return PKEY_OK;
}

static int get_pkey_private_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    if (enif_is_map(env, key)) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;
        char *id = NULL;
        char *password;

        if (!get_engine_and_key_id(env, key, &id, &e))
            return PKEY_BADARG;
        password = get_key_password(env, key);
        *pkey = ENGINE_load_private_key(e, id, NULL, password);
        if (password) enif_free(password);
        enif_free(id);
        if (!*pkey)
            return PKEY_BADARG;
#else
        return PKEY_BADARG;
#endif
    }
    else if (algorithm == atom_rsa) {
	RSA *rsa = RSA_new();

	if (!get_rsa_private_key(env, key, rsa)) {
	    RSA_free(rsa);
	    return PKEY_BADARG;
	}

	*pkey = EVP_PKEY_new();
	if (!EVP_PKEY_assign_RSA(*pkey, rsa)) {
	    EVP_PKEY_free(*pkey);
	    RSA_free(rsa);
	    return PKEY_BADARG;
	}
    } else if (algorithm == atom_ecdsa) {
#if defined(HAVE_EC)
	EC_KEY *ec = NULL;
	const ERL_NIF_TERM *tpl_terms;
	int tpl_arity;

	if (enif_get_tuple(env, key, &tpl_arity, &tpl_terms) && tpl_arity == 2
	    && enif_is_tuple(env, tpl_terms[0]) && enif_is_binary(env, tpl_terms[1])
	    && get_ec_key(env, tpl_terms[0], tpl_terms[1], atom_undefined, &ec)) {

	    *pkey = EVP_PKEY_new();
	    if (!EVP_PKEY_assign_EC_KEY(*pkey, ec)) {
		EVP_PKEY_free(*pkey);
		EC_KEY_free(ec);
		return PKEY_BADARG;
	    }
	} else {
	    return PKEY_BADARG;
	}
#else
	return PKEY_NOTSUP;
#endif
    } else if (algorithm == atom_eddsa) {
#if defined(HAVE_EDDSA)
        if (!get_eddsa_key(env, 0, key, pkey)) {
            return PKEY_BADARG;
        }
#else
     return PKEY_NOTSUP;  
#endif
    } else if (algorithm == atom_dss) {
	DSA *dsa = DSA_new();

	if (!get_dss_private_key(env, key, dsa)) {
	    DSA_free(dsa);
            return PKEY_BADARG;
        }

	*pkey = EVP_PKEY_new();
	if (!EVP_PKEY_assign_DSA(*pkey, dsa)) {
	    EVP_PKEY_free(*pkey);
	    DSA_free(dsa);
	    return PKEY_BADARG;
	}
    } else {
	return PKEY_BADARG;
    }

    return PKEY_OK;
}

static int get_pkey_public_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key,
			       EVP_PKEY **pkey)
{
    if (enif_is_map(env, key)) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;
        char *id = NULL;
        char *password;

        if (!get_engine_and_key_id(env, key, &id, &e))
            return PKEY_BADARG;
        password = get_key_password(env, key);
        *pkey = ENGINE_load_public_key(e, id, NULL, password);
        if (password) enif_free(password);
        enif_free(id);
        if (!pkey)
            return PKEY_BADARG;
#else
        return PKEY_BADARG;
#endif
    } else  if (algorithm == atom_rsa) {
	RSA *rsa = RSA_new();

	if (!get_rsa_public_key(env, key, rsa)) {
	    RSA_free(rsa);
	    return PKEY_BADARG;
	}

	*pkey = EVP_PKEY_new();
	if (!EVP_PKEY_assign_RSA(*pkey, rsa)) {
	    EVP_PKEY_free(*pkey);
	    RSA_free(rsa);
	    return PKEY_BADARG;
	}
    } else if (algorithm == atom_ecdsa) {
#if defined(HAVE_EC)
	EC_KEY *ec = NULL;
	const ERL_NIF_TERM *tpl_terms;
	int tpl_arity;

	if (enif_get_tuple(env, key, &tpl_arity, &tpl_terms) && tpl_arity == 2
	    && enif_is_tuple(env, tpl_terms[0]) && enif_is_binary(env, tpl_terms[1])
	    && get_ec_key(env, tpl_terms[0], atom_undefined, tpl_terms[1], &ec)) {

	    *pkey = EVP_PKEY_new();
	    if (!EVP_PKEY_assign_EC_KEY(*pkey, ec)) {
		EVP_PKEY_free(*pkey);
		EC_KEY_free(ec);
		return PKEY_BADARG;
	    }
	} else {
	    return PKEY_BADARG;
	}
#else
	return PKEY_NOTSUP;
#endif
    } else if (algorithm == atom_eddsa) {
#if defined(HAVE_EDDSA)
        if (!get_eddsa_key(env, 1, key, pkey)) {
            return PKEY_BADARG;
        }
#else
     return PKEY_NOTSUP;  
#endif
    } else if (algorithm == atom_dss) {
	DSA *dsa = DSA_new();

	if (!get_dss_public_key(env, key, dsa)) {
	    DSA_free(dsa);
	    return PKEY_BADARG;
	}

	*pkey = EVP_PKEY_new();
	if (!EVP_PKEY_assign_DSA(*pkey, dsa)) {
	    EVP_PKEY_free(*pkey);
	    DSA_free(dsa);
	    return PKEY_BADARG;
	}
    } else {
	return PKEY_BADARG;
    }

    return PKEY_OK;
}

ERL_NIF_TERM pkey_sign_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Type, Data|{digest,Digest}, Key|#{}, Options) */
    int i;
    const EVP_MD *md = NULL;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    EVP_PKEY *pkey;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx;
    size_t siglen;
#else
    unsigned len, siglen;
#endif
    PKeySignOptions sig_opt;
    ErlNifBinary sig_bin; /* signature */
    unsigned char *tbs; /* data to be signed */
    size_t tbslen;
/*char buf[1024];
enif_get_atom(env,argv[0],buf,1024,ERL_NIF_LATIN1); printf("algo=%s ",buf);
enif_get_atom(env,argv[1],buf,1024,ERL_NIF_LATIN1); printf("hash=%s ",buf);
printf("\r\n");
*/

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[3])) {
        return atom_notsup;
    }
#endif

    i = get_pkey_sign_digest(env, argv[0], argv[1], argv[2], md_value, &md, &tbs, &tbslen);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    i = get_pkey_sign_options(env, argv[0], argv[4], md, &sig_opt);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    if (get_pkey_private_key(env, argv[0], argv[3], &pkey) != PKEY_OK) {
	return enif_make_badarg(env);
    }

#ifdef HAS_EVP_PKEY_CTX
    ctx = EVP_PKEY_CTX_new(pkey, NULL);
    if (!ctx) goto badarg;

    if (argv[0] != atom_eddsa) {
        if (EVP_PKEY_sign_init(ctx) <= 0) goto badarg;
        if (md != NULL && EVP_PKEY_CTX_set_signature_md(ctx, md) <= 0) goto badarg;
    }

    if (argv[0] == atom_rsa) {
	if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) <= 0) goto badarg;
# ifdef HAVE_RSA_PKCS1_PSS_PADDING
	if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
            if (sig_opt.rsa_mgf1_md != NULL) {
# ifdef HAVE_RSA_MGF1_MD
		if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) <= 0) goto badarg;
# else
                EVP_PKEY_CTX_free(ctx);
                EVP_PKEY_free(pkey);
                return atom_notsup;
# endif
            }
	    if (sig_opt.rsa_pss_saltlen > -2
		&& EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) <= 0)
		goto badarg;
	}
#endif
    }

    if (argv[0] == atom_eddsa) {
#ifdef HAVE_EDDSA
        EVP_MD_CTX* mdctx = EVP_MD_CTX_new();
        if (!EVP_DigestSignInit(mdctx, NULL, NULL, NULL, pkey)) {
            if (mdctx) EVP_MD_CTX_free(mdctx);
            goto badarg;
        }

        if (!EVP_DigestSign(mdctx, NULL, &siglen, tbs, tbslen)) {
            EVP_MD_CTX_free(mdctx);
            goto badarg;
        }
        enif_alloc_binary(siglen, &sig_bin);

        if (!EVP_DigestSign(mdctx, sig_bin.data, &siglen, tbs, tbslen)) {
            EVP_MD_CTX_free(mdctx);
            goto badarg;
        }
        EVP_MD_CTX_free(mdctx);
#else
        goto badarg;    
#endif
    }
    else
    {
        if (EVP_PKEY_sign(ctx, NULL, &siglen, tbs, tbslen) <= 0) goto badarg;
        enif_alloc_binary(siglen, &sig_bin);

        if (md != NULL) {
            ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
        }
        i = EVP_PKEY_sign(ctx, sig_bin.data, &siglen, tbs, tbslen);
    }
        
    EVP_PKEY_CTX_free(ctx);
#else
/*printf("Old interface\r\n");
 */
    if (argv[0] == atom_rsa) {
       RSA *rsa = EVP_PKEY_get1_RSA(pkey);
       enif_alloc_binary(RSA_size(rsa), &sig_bin);
       len = EVP_MD_size(md);
       ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);
       i = RSA_sign(md->type, tbs, len, sig_bin.data, &siglen, rsa);
       RSA_free(rsa);
    } else if (argv[0] == atom_dss) {
       DSA *dsa = EVP_PKEY_get1_DSA(pkey);
       enif_alloc_binary(DSA_size(dsa), &sig_bin);
       len = EVP_MD_size(md);
       ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);
       i = DSA_sign(md->type, tbs, len, sig_bin.data, &siglen, dsa);
       DSA_free(dsa);
    } else if (argv[0] == atom_ecdsa) {
#if defined(HAVE_EC)
       EC_KEY *ec = EVP_PKEY_get1_EC_KEY(pkey);
       enif_alloc_binary(ECDSA_size(ec), &sig_bin);
       len = EVP_MD_size(md);
       ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);
       i = ECDSA_sign(md->type, tbs, len, sig_bin.data, &siglen, ec);
       EC_KEY_free(ec);
#else
       EVP_PKEY_free(pkey);
       return atom_notsup;
#endif
    } else {
	goto badarg;
    }
#endif

    EVP_PKEY_free(pkey);
    if (i == 1) {
	ERL_VALGRIND_MAKE_MEM_DEFINED(sig_bin.data, siglen);
	if (siglen != sig_bin.size) {
	    enif_realloc_binary(&sig_bin, siglen);
	    ERL_VALGRIND_ASSERT_MEM_DEFINED(sig_bin.data, siglen);
	}
	return enif_make_binary(env, &sig_bin);
    } else {
	enif_release_binary(&sig_bin);
	return atom_error;
    }

 badarg:
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX_free(ctx);
#endif
    EVP_PKEY_free(pkey);
    return enif_make_badarg(env);
}

ERL_NIF_TERM pkey_verify_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Type, Data|{digest,Digest}, Signature, Key, Options) */
    int i;
    const EVP_MD *md = NULL;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    EVP_PKEY *pkey;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx;
#else
#endif
    PKeySignOptions sig_opt;
    ErlNifBinary sig_bin; /* signature */
    unsigned char *tbs; /* data to be signed */
    size_t tbslen;

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[4])) {
        return atom_notsup;
    }
#endif

    if (!enif_inspect_binary(env, argv[3], &sig_bin)) {
	return enif_make_badarg(env);
    }

    i = get_pkey_sign_digest(env, argv[0], argv[1], argv[2], md_value, &md, &tbs, &tbslen);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    i = get_pkey_sign_options(env, argv[0], argv[5], md, &sig_opt);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    if (get_pkey_public_key(env, argv[0], argv[4], &pkey) != PKEY_OK) {
	return enif_make_badarg(env);
    }

#ifdef HAS_EVP_PKEY_CTX
/* printf("EVP interface\r\n");
 */
    ctx = EVP_PKEY_CTX_new(pkey, NULL);
    if (!ctx) goto badarg;

    if (argv[0] != atom_eddsa) {
        if (EVP_PKEY_verify_init(ctx) <= 0) goto badarg;
        if (md != NULL && EVP_PKEY_CTX_set_signature_md(ctx, md) <= 0) goto badarg;
    }

    if (argv[0] == atom_rsa) {
	if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) <= 0) goto badarg;
	if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
            if (sig_opt.rsa_mgf1_md != NULL) {
# ifdef HAVE_RSA_MGF1_MD
		if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) <= 0) goto badarg;
# else
                EVP_PKEY_CTX_free(ctx);
                EVP_PKEY_free(pkey);
                return atom_notsup;
# endif
            }
	    if (sig_opt.rsa_pss_saltlen > -2
		&& EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) <= 0)
		goto badarg;
	}
    }

        if (argv[0] == atom_eddsa) {
#ifdef HAVE_EDDSA
        EVP_MD_CTX* mdctx = EVP_MD_CTX_create();
        
        if (!EVP_DigestVerifyInit(mdctx, NULL, NULL, NULL, pkey)) {
            if (mdctx) EVP_MD_CTX_destroy(mdctx);
            goto badarg;
        }

        i = EVP_DigestVerify(mdctx, sig_bin.data, sig_bin.size, tbs, tbslen);
        EVP_MD_CTX_destroy(mdctx);
#else
        goto badarg;    
#endif
        }
    else
        {
            if (md != NULL) {
                ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
            }
            i = EVP_PKEY_verify(ctx, sig_bin.data, sig_bin.size, tbs, tbslen);
        }

    EVP_PKEY_CTX_free(ctx);
#else
/*printf("Old interface\r\n");
*/
    if (argv[0] == atom_rsa) {
        RSA *rsa = EVP_PKEY_get1_RSA(pkey);
        i = RSA_verify(md->type, tbs, tbslen, sig_bin.data, sig_bin.size, rsa);
        RSA_free(rsa);
    } else if (argv[0] == atom_dss) {
        DSA *dsa = EVP_PKEY_get1_DSA(pkey);
        i = DSA_verify(0, tbs, tbslen, sig_bin.data, sig_bin.size, dsa);
        DSA_free(dsa);
    } else if (argv[0] == atom_ecdsa) {
#if defined(HAVE_EC)
        EC_KEY *ec = EVP_PKEY_get1_EC_KEY(pkey);
        i = ECDSA_verify(EVP_MD_type(md), tbs, tbslen, sig_bin.data, sig_bin.size, ec);
        EC_KEY_free(ec);
#else
        EVP_PKEY_free(pkey);
        return atom_notsup;
#endif
    } else {
	goto badarg;
    }
#endif

    EVP_PKEY_free(pkey);
    if (i == 1) {
	return atom_true;
    } else {
	return atom_false;
    }

 badarg:
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX_free(ctx);
#endif
    EVP_PKEY_free(pkey);
    return enif_make_badarg(env);
}

static int get_pkey_crypt_options(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM options,
				  PKeyCryptOptions *opt)
{
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    const EVP_MD *opt_md;
    int i;

    if (!enif_is_list(env, options)) {
	return PKEY_BADARG;
    }

    /* defaults */
    if (algorithm == atom_rsa) {
	opt->rsa_mgf1_md = NULL;
	opt->rsa_oaep_label.data = NULL;
	opt->rsa_oaep_label.size = 0;
	opt->rsa_oaep_md = NULL;
	opt->rsa_padding = RSA_PKCS1_PADDING;
	opt->signature_md = NULL;
    }

    if (enif_is_empty_list(env, options)) {
	return PKEY_OK;
    }

    if (algorithm == atom_rsa) {
	tail = options;
	while (enif_get_list_cell(env, tail, &head, &tail)) {
	    if (enif_get_tuple(env, head, &tpl_arity, &tpl_terms) && tpl_arity == 2) {
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
			return PKEY_BADARG;
		    }
		} else if (tpl_terms[0] == atom_signature_md && enif_is_atom(env, tpl_terms[1])) {
		    i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
		    if (i != PKEY_OK) {
			return i;
		    }
		    opt->signature_md = opt_md;
		} else if (tpl_terms[0] == atom_rsa_mgf1_md && enif_is_atom(env, tpl_terms[1])) {
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
		    return PKEY_BADARG;
		}
	    } else {
		return PKEY_BADARG;
	    }
	}
    } else {
	return PKEY_BADARG;
    }

    return PKEY_OK;
}

static size_t size_of_RSA(EVP_PKEY *pkey) {
    size_t tmplen;
    RSA *rsa = EVP_PKEY_get1_RSA(pkey);
    if (rsa == NULL) return 0;
    tmplen = RSA_size(rsa);
    RSA_free(rsa);
    return tmplen;
}

ERL_NIF_TERM pkey_crypt_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Data, PublKey=[E,N]|[E,N,D]|[E,N,D,P1,P2,E1,E2,C], Options, IsPrivate, IsEncrypt) */
    int i;
    EVP_PKEY *pkey;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx;
#else
    RSA *rsa;
#endif
    PKeyCryptOptions crypt_opt;
    ErlNifBinary in_bin, out_bin, tmp_bin;
    size_t outlen;
#ifdef HAVE_RSA_SSLV23_PADDING
    size_t tmplen;
#endif
    int is_private = (argv[4] == atom_true),
        is_encrypt = (argv[5] == atom_true);
    int algo_init = 0;

/* char algo[1024]; */

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[2])) {
        return atom_notsup;
    }
#endif

    if (!enif_inspect_binary(env, argv[1], &in_bin)) {
	return enif_make_badarg(env);
    }

    i = get_pkey_crypt_options(env, argv[0], argv[3], &crypt_opt);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    if (is_private) {
	if (get_pkey_private_key(env, argv[0], argv[2], &pkey) != PKEY_OK) {
	    return enif_make_badarg(env);
	}
    } else {
	if (get_pkey_public_key(env, argv[0], argv[2], &pkey) != PKEY_OK) {
	    return enif_make_badarg(env);
	}
    }

    out_bin.data = NULL;
    out_bin.size = 0;
    tmp_bin.data = NULL;
    tmp_bin.size = 0;

#ifdef HAS_EVP_PKEY_CTX
    ctx = EVP_PKEY_CTX_new(pkey, NULL);
    if (!ctx) goto badarg;

/* enif_get_atom(env,argv[0],algo,1024,ERL_NIF_LATIN1);  */

    if (is_private) {
        if (is_encrypt) {
            /* private encrypt */
            if ((algo_init=EVP_PKEY_sign_init(ctx)) <= 0) {
                /* fprintf(stderr,"BADARG %s private encrypt algo_init=%d %s:%d\r\n", algo, algo_init, __FILE__, __LINE__); */
                goto badarg;
            }
        } else {
            /* private decrypt */
            if ((algo_init=EVP_PKEY_decrypt_init(ctx)) <= 0) {
                /* fprintf(stderr,"BADARG %s private decrypt algo_init=%d %s:%d\r\n", algo, algo_init, __FILE__, __LINE__); */
                goto badarg;
            }
        }
    } else {
        if (is_encrypt) {
            /* public encrypt */
            if ((algo_init=EVP_PKEY_encrypt_init(ctx)) <= 0) {
                /* fprintf(stderr,"BADARG %s public encrypt algo_init=%d %s:%d\r\n", algo,algo_init,__FILE__, __LINE__); */
                goto badarg;
            }
        } else {
            /* public decrypt */
            if ((algo_init=EVP_PKEY_verify_recover_init(ctx)) <= 0) {
                /* fprintf(stderr,"BADARG %s public decrypt algo_init=%d %s:%d\r\n", algo,algo_init,__FILE__, __LINE__); */
                goto badarg;
            }
        }
    }

    if (argv[0] == atom_rsa) {
	if (crypt_opt.signature_md != NULL
	    && EVP_PKEY_CTX_set_signature_md(ctx, crypt_opt.signature_md) <= 0)
		goto badarg;
#ifdef HAVE_RSA_SSLV23_PADDING
	if (crypt_opt.rsa_padding == RSA_SSLV23_PADDING) {
	    if (is_encrypt) {
                tmplen = size_of_RSA(pkey);
                if (tmplen == 0) goto badarg;
		if (!enif_alloc_binary(tmplen, &tmp_bin)) goto badarg;
		if (RSA_padding_add_SSLv23(tmp_bin.data, tmplen, in_bin.data, in_bin.size) <= 0)
		    goto badarg;
		in_bin = tmp_bin;
	    }
	    if (EVP_PKEY_CTX_set_rsa_padding(ctx, RSA_NO_PADDING) <= 0) goto badarg;
	} else
#endif
                {
	    if (EVP_PKEY_CTX_set_rsa_padding(ctx, crypt_opt.rsa_padding) <= 0) goto badarg;
        }
#ifdef HAVE_RSA_OAEP_MD
	if (crypt_opt.rsa_padding == RSA_PKCS1_OAEP_PADDING) {
	    if (crypt_opt.rsa_oaep_md != NULL
		&& EVP_PKEY_CTX_set_rsa_oaep_md(ctx, crypt_opt.rsa_oaep_md) <= 0)
		goto badarg;
	    if (crypt_opt.rsa_mgf1_md != NULL
		&& EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, crypt_opt.rsa_mgf1_md) <= 0) goto badarg;
	    if (crypt_opt.rsa_oaep_label.data != NULL && crypt_opt.rsa_oaep_label.size > 0) {
		unsigned char *label_copy = NULL;
		label_copy = OPENSSL_malloc(crypt_opt.rsa_oaep_label.size);
		if (label_copy == NULL) goto badarg;
		memcpy((void *)(label_copy), (const void *)(crypt_opt.rsa_oaep_label.data),
		       crypt_opt.rsa_oaep_label.size);
		if (EVP_PKEY_CTX_set0_rsa_oaep_label(ctx, label_copy,
						     crypt_opt.rsa_oaep_label.size) <= 0) {
		    OPENSSL_free(label_copy);
		    label_copy = NULL;
		    goto badarg;
		}
	    }
	}
#endif
    }

    if (is_private) {
	if (is_encrypt) {
	    /* private_encrypt */
	    i = EVP_PKEY_sign(ctx, NULL, &outlen, in_bin.data, in_bin.size);
	} else {
	    /* private_decrypt */
	    i = EVP_PKEY_decrypt(ctx, NULL, &outlen, in_bin.data, in_bin.size);
	}
    } else {
	if (is_encrypt) {
	    /* public_encrypt */
	    i = EVP_PKEY_encrypt(ctx, NULL, &outlen, in_bin.data, in_bin.size);
	} else {
	    /* public_decrypt */
	    i = EVP_PKEY_verify_recover(ctx, NULL, &outlen, in_bin.data, in_bin.size);
	}
    }
    /* fprintf(stderr,"i = %d %s:%d\r\n", i, __FILE__, __LINE__); */

    if (i != 1) goto badarg;

    enif_alloc_binary(outlen, &out_bin);

    if (is_private) {
	if (is_encrypt) {
	    /* private_encrypt */
	    i = EVP_PKEY_sign(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
	} else {
	    /* private_decrypt */
	    i = EVP_PKEY_decrypt(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
	}
    } else {
	if (is_encrypt) {
	    /* public_encrypt */
	    i = EVP_PKEY_encrypt(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
	} else {
	    /* public_decrypt */
	    i = EVP_PKEY_verify_recover(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
	}
    }

#else
    /* Non-EVP cryptolib. Only support RSA */

    if (argv[0] != atom_rsa) {
        algo_init = -2;         /* exitcode: notsup */
        goto badarg;
    }
    rsa = EVP_PKEY_get1_RSA(pkey);
    enif_alloc_binary(RSA_size(rsa), &out_bin);

    if (is_private) {
        if (is_encrypt) {
            /* non-evp rsa private encrypt */
            ERL_VALGRIND_ASSERT_MEM_DEFINED(in_bin.data,in_bin.size);
            i = RSA_private_encrypt(in_bin.size, in_bin.data,
                                    out_bin.data, rsa, crypt_opt.rsa_padding);
            if (i > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, i);
            }
        } else {
            /* non-evp rsa private decrypt */
            i = RSA_private_decrypt(in_bin.size, in_bin.data,
                                    out_bin.data, rsa, crypt_opt.rsa_padding);
            if (i > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, i);
                enif_realloc_binary(&out_bin, i);
            }
        }
    } else {
        if (is_encrypt) {
            /* non-evp rsa public encrypt */
            ERL_VALGRIND_ASSERT_MEM_DEFINED(in_bin.data,in_bin.size);
            i = RSA_public_encrypt(in_bin.size, in_bin.data,
                                   out_bin.data, rsa, crypt_opt.rsa_padding);
            if (i > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, i);
	}
        } else {
            /* non-evp rsa public decrypt */
            i = RSA_public_decrypt(in_bin.size, in_bin.data,
                                   out_bin.data, rsa, crypt_opt.rsa_padding);
            if (i > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, i);
                enif_realloc_binary(&out_bin, i);
            }
        }
    }

    outlen = i;
    RSA_free(rsa);
#endif

    if ((i > 0) && argv[0] == atom_rsa && !is_encrypt) {
#ifdef HAVE_RSA_SSLV23_PADDING
	if (crypt_opt.rsa_padding == RSA_SSLV23_PADDING) {
	    unsigned char *p;
            tmplen = size_of_RSA(pkey);
	    if (tmplen == 0) goto badarg;
	    if (!enif_alloc_binary(tmplen, &tmp_bin))
                goto badarg;
	    p = out_bin.data;
	    p++;
	    i = RSA_padding_check_SSLv23(tmp_bin.data, tmplen, p, out_bin.size - 1, tmplen);
	    if (i >= 0) {
		outlen = i;
		in_bin = out_bin;
		out_bin = tmp_bin;
		tmp_bin = in_bin;
		i = 1;
	    }
	}
#endif
    }

    if (tmp_bin.data != NULL) {
	enif_release_binary(&tmp_bin);
    }

#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX_free(ctx);
#else
#endif
    EVP_PKEY_free(pkey);
    if (i > 0) {
	ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, outlen);
	if (outlen != out_bin.size) {
	    enif_realloc_binary(&out_bin, outlen);
	    ERL_VALGRIND_ASSERT_MEM_DEFINED(out_bin.data, outlen);
	}
	return enif_make_binary(env, &out_bin);
    } else {
	enif_release_binary(&out_bin);
	return atom_error;
    }

 badarg:
    if (out_bin.data != NULL) {
	enif_release_binary(&out_bin);
    }
    if (tmp_bin.data != NULL) {
	enif_release_binary(&tmp_bin);
    }
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX_free(ctx);
#else
#endif
    EVP_PKEY_free(pkey);
    if (algo_init == -2)
        return atom_notsup;
    else
        return enif_make_badarg(env);
}

ERL_NIF_TERM privkey_to_pubkey_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ /* (Algorithm, PrivKey | KeyMap) */
    EVP_PKEY *pkey;
    ERL_NIF_TERM alg = argv[0];
    ERL_NIF_TERM result[8];
    if (get_pkey_private_key(env, alg, argv[1], &pkey) != PKEY_OK) {
	return enif_make_badarg(env);
    }

    if (alg == atom_rsa) {
        const BIGNUM *n = NULL, *e = NULL, *d = NULL;
        RSA *rsa = EVP_PKEY_get1_RSA(pkey);
        if (rsa) {
            RSA_get0_key(rsa, &n, &e, &d);
            result[0] = bin_from_bn(env, e);  // Exponent E
            result[1] = bin_from_bn(env, n);  // Modulus N = p*q
            RSA_free(rsa);
            EVP_PKEY_free(pkey);
            return enif_make_list_from_array(env, result, 2);
        }

    } else if (argv[0] == atom_dss) {
        const BIGNUM *p = NULL, *q = NULL, *g = NULL, *pub_key = NULL;
        DSA *dsa = EVP_PKEY_get1_DSA(pkey);
        if (dsa) {
            DSA_get0_pqg(dsa, &p, &q, &g);
            DSA_get0_key(dsa, &pub_key, NULL);
            result[0] = bin_from_bn(env, p);
            result[1] = bin_from_bn(env, q);
            result[2] = bin_from_bn(env, g);
            result[3] = bin_from_bn(env, pub_key);
	    DSA_free(dsa);
            EVP_PKEY_free(pkey);
            return enif_make_list_from_array(env, result, 4);
        }

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
    }

    if (pkey) EVP_PKEY_free(pkey);
    return enif_make_badarg(env);
}
