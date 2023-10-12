/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2021. All Rights Reserved.
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
#include "digest.h"


ERL_NIF_TERM info_fips(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef FIPS_SUPPORT
    return FIPS_MODE() ? atom_enabled : atom_not_enabled;
#else
    return atom_not_supported;
#endif
}

ERL_NIF_TERM enable_fips_mode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Boolean) */
    return enable_fips_mode(env, argv[0]);
}


ERL_NIF_TERM enable_fips_mode(ErlNifEnv* env, ERL_NIF_TERM fips_mode_to_set)
#ifdef FIPS_SUPPORT
{
    if (fips_mode_to_set == atom_true) {
        if (FIPS_mode_set(1)) return atom_true;
                         else return atom_false;

    } else if (fips_mode_to_set == atom_false) {
        if (!FIPS_mode_set(0)) return atom_false;
                          else return atom_true;

    } else
        return enif_make_badarg(env);
}
#else

{
    if (fips_mode_to_set == atom_true) return atom_false;
    else if (fips_mode_to_set == atom_false) return atom_true;
    else return enif_make_badarg(env);
}
#endif    


