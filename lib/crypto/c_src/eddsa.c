#include "eddsa.h"

#ifdef HAVE_EDDSA
int get_eddsa_key(ErlNifEnv* env, int public, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    /* key=[K] */
    ERL_NIF_TERM head, tail, tail2, algo;
    ErlNifBinary bin;
    int type;

    if (!enif_get_list_cell(env, key, &head, &tail)
	|| !enif_inspect_binary(env, head, &bin)
        || !enif_get_list_cell(env, tail, &algo, &tail2)
        || !enif_is_empty_list(env, tail2)) {
	return 0;
    }
    if (algo == atom_ed25519) type = EVP_PKEY_ED25519;
    else if (algo == atom_ed448) type = EVP_PKEY_ED448;
    else
        return 0;

    if (public)
        *pkey = EVP_PKEY_new_raw_public_key(type, NULL, bin.data, bin.size);
    else 
        *pkey = EVP_PKEY_new_raw_private_key(type, NULL, bin.data, bin.size);

    if (!pkey)
        return 0;
    return 1;
}
#endif
