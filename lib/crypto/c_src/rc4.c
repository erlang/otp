#include "rc4.h"

ERL_NIF_TERM rc4_set_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key) */
#ifndef OPENSSL_NO_RC4
    ErlNifBinary key;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!enif_inspect_iolist_as_binary(env,argv[0], &key)) {
	return enif_make_badarg(env);
    }
    RC4_set_key((RC4_KEY*)enif_make_new_binary(env, sizeof(RC4_KEY), &ret),
		key.size, key.data);
    return ret;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

ERL_NIF_TERM rc4_encrypt_with_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (State, Data) */
#ifndef OPENSSL_NO_RC4
    ErlNifBinary state, data;
    RC4_KEY* rc4_key;
    ERL_NIF_TERM new_state, new_data;

    CHECK_NO_FIPS_MODE();

    if (!enif_inspect_iolist_as_binary(env,argv[0], &state)
	|| state.size != sizeof(RC4_KEY)
	|| !enif_inspect_iolist_as_binary(env,argv[1], &data)) {
	return enif_make_badarg(env);
    }
    rc4_key = (RC4_KEY*)enif_make_new_binary(env, sizeof(RC4_KEY), &new_state);
    memcpy(rc4_key, state.data, sizeof(RC4_KEY));
    RC4(rc4_key, data.size, data.data,
	enif_make_new_binary(env, data.size, &new_data));
    CONSUME_REDS(env,data);
    return enif_make_tuple2(env,new_state,new_data);
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

