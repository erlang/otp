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

#include "fips.h"

ERL_NIF_TERM info_fips(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef FIPS_SUPPORT
    return FIPS_mode() ? atom_enabled : atom_not_enabled;
#else
    return atom_not_supported;
#endif
}

ERL_NIF_TERM enable_fips_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Boolean) */
    if (argv[0] == atom_true) {
#ifdef FIPS_SUPPORT
        if (FIPS_mode_set(1)) {
            return atom_true;
        }
#endif
        PRINTF_ERR0("CRYPTO: Could not setup FIPS mode");
        return atom_false;
    } else if (argv[0] == atom_false) {
#ifdef FIPS_SUPPORT
        if (!FIPS_mode_set(0)) {
            return atom_false;
        }
#endif
        return atom_true;
    } else {
        return enif_make_badarg(env);
    }
}
