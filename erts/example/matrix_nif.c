/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */
/*
 * Purpose: Simple example of NIFs using resource objects to implement functions
 *          for matrix calculations.
 */

#include "erl_nif.h"

#include <stddef.h>
#include <assert.h>

typedef struct
{
    unsigned nrows;
    unsigned ncols;
    double* data;
}Matrix;

#define POS(MX, ROW, COL) ((MX)->data[(ROW)* (MX)->ncols + (COL)])

static int get_number(ErlNifEnv* env, ERL_NIF_TERM term, double* dp);
static Matrix* alloc_matrix(ErlNifEnv* env, unsigned nrows, unsigned ncols);
static void matrix_dtor(ErlNifEnv* env, void* obj);


static ErlNifResourceType* resource_type = NULL;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
						     "matrix_nif_example",
						     matrix_dtor,
						     ERL_NIF_RT_CREATE, NULL);
    if (rt == NULL) {
	return -1;
    }
    assert(resource_type == NULL);
    resource_type = rt;
    return 0;
}

static ERL_NIF_TERM create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* create(Nrows, Ncolumns, [[first row],[second row],...,[last row]]) -> Matrix */
    unsigned nrows, ncols;
    unsigned i, j;
    ERL_NIF_TERM list, row, ret;
    Matrix* mx = NULL;

    if (!enif_get_uint(env, argv[0], &nrows) || nrows < 1 ||
	!enif_get_uint(env, argv[1], &ncols) || ncols < 1) {

	goto badarg;
    }
    mx = alloc_matrix(env, nrows, ncols);
    list = argv[2];
    for (i = 0; i<nrows; i++) {
	if (!enif_get_list_cell(env, list, &row, &list)) {
	    goto badarg;
	}
	for (j = 0; j<ncols; j++) {
	    ERL_NIF_TERM v;
	    if (!enif_get_list_cell(env, row, &v, &row) ||
		!get_number(env, v, &POS(mx,i,j))) { 
		goto badarg;
	    }	    
	}
	if (!enif_is_empty_list(env, row)) {
	    goto badarg;
	}
    }
    if (!enif_is_empty_list(env, list)) {
	goto badarg;
    }

    ret = enif_make_resource(env, mx);
    enif_release_resource(mx);
    return ret;

badarg:
    if (mx != NULL) {
	enif_release_resource(mx);
    }
    return enif_make_badarg(env);
}


static ERL_NIF_TERM pos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* pos(Matrix, Row, Column) -> float() */
    Matrix* mx;
    unsigned i, j;
    if (!enif_get_resource(env, argv[0], resource_type, (void**)&mx) ||
	!enif_get_uint(env, argv[1], &i) || (--i >= mx->nrows) ||
	!enif_get_uint(env, argv[2], &j) || (--j >= mx->ncols)) {
	return enif_make_badarg(env);
    }
    return enif_make_double(env, POS(mx, i,j));
}

static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* add(Matrix_A, Matrix_B) -> Matrix_Sum */
    unsigned i, j;
    ERL_NIF_TERM ret;
    Matrix* mxA = NULL;
    Matrix* mxB = NULL;
    Matrix* mxS = NULL;

    if (!enif_get_resource(env, argv[0], resource_type, (void**)&mxA) ||
	!enif_get_resource(env, argv[1], resource_type, (void**)&mxB) ||
	mxA->nrows != mxB->nrows ||
	mxB->ncols != mxB->ncols) {

    	return enif_make_badarg(env);
    }
    mxS = alloc_matrix(env, mxA->nrows, mxA->ncols);
    for (i = 0; i < mxA->nrows; i++) {
	for (j = 0; j < mxA->ncols; j++) {
	    POS(mxS, i, j) = POS(mxA, i, j) + POS(mxB, i, j);
	}
    }
    ret = enif_make_resource(env, mxS);
    enif_release_resource(mxS);
    return ret;
}

static ERL_NIF_TERM size_of(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* size(Matrix) -> {Nrows, Ncols} */
    Matrix* mx;
    if (!enif_get_resource(env, argv[0], resource_type, (void**)&mx)) {
	return enif_make_badarg(env);
    }
    return enif_make_tuple2(env, enif_make_uint(env, mx->nrows),
			    enif_make_uint(env, mx->ncols));
}

static ERL_NIF_TERM to_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* to_term(Matrix) -> [[first row], [second row], ...,[last row]] */
    unsigned i, j;
    ERL_NIF_TERM res;
    Matrix* mx = NULL;

    if (!enif_get_resource(env, argv[0], resource_type, (void**)&mx)) {	
    	return enif_make_badarg(env);
    }
    res = enif_make_list(env, 0);
    for (i = mx->nrows; i-- > 0; ) {
	ERL_NIF_TERM row = enif_make_list(env, 0);
	for (j = mx->ncols; j-- > 0; ) {
	    row = enif_make_list_cell(env, enif_make_double(env, POS(mx,i,j)),
				      row);
	}
	res = enif_make_list_cell(env, row, res);
    }
    return res;
}

static int get_number(ErlNifEnv* env, ERL_NIF_TERM term, double* dp)
{
    long i;
    return enif_get_double(env, term, dp) || 
	(enif_get_long(env, term, &i) && (*dp=(double)i, 1));
}

static Matrix* alloc_matrix(ErlNifEnv* env, unsigned nrows, unsigned ncols)
{
    Matrix* mx = enif_alloc_resource(resource_type, sizeof(Matrix));
    mx->nrows = nrows;
    mx->ncols = ncols;
    mx->data = enif_alloc(nrows*ncols*sizeof(double));
    return mx;
}

static void matrix_dtor(ErlNifEnv* env, void* obj)
{
    Matrix* mx = (Matrix*) obj;
    enif_free(mx->data);
    mx->data = NULL;
}

static ErlNifFunc nif_funcs[] =
{
    {"create", 3, create},
    {"pos", 3, pos},
    {"add", 2, add},
    {"size_of", 1, size_of},
    {"to_term", 1, to_term}
};

ERL_NIF_INIT(matrix_nif,nif_funcs,load,NULL,NULL,NULL);

