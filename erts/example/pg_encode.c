/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
#include <erl_driver.h>

#include <libpq-fe.h>

#include <ei.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "pg_encode.h"

void encode_ok(ei_x_buff* x)
{
    const char* k_ok = "ok";
    ei_x_encode_atom(x, k_ok);
}

void encode_error(ei_x_buff* x, PGconn* conn)
{
    const char* k_error = "error";
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_atom(x, k_error);
    ei_x_encode_string(x, PQerrorMessage(conn));
}

void encode_result(ei_x_buff* x, PGresult* res, PGconn* conn)
{
    int row, n_rows, col, n_cols;
    switch (PQresultStatus(res)) {
    case PGRES_TUPLES_OK: 
	n_rows = PQntuples(res); 
	n_cols = PQnfields(res); 
	ei_x_encode_tuple_header(x, 2);
	encode_ok(x);
	ei_x_encode_list_header(x, n_rows+1);
 	ei_x_encode_list_header(x, n_cols);
	for (col = 0; col < n_cols; ++col) {
	    ei_x_encode_string(x, PQfname(res, col));
	}
	ei_x_encode_empty_list(x); 
	for (row = 0; row < n_rows; ++row) {
	    ei_x_encode_list_header(x, n_cols);
	    for (col = 0; col < n_cols; ++col) {
		ei_x_encode_string(x, PQgetvalue(res, row, col));
	    }
	    ei_x_encode_empty_list(x);
	}
	ei_x_encode_empty_list(x); 
	break; 
    case PGRES_COMMAND_OK:
	ei_x_encode_tuple_header(x, 2);
        encode_ok(x);
	ei_x_encode_string(x, PQcmdTuples(res));
        break;
    default:
	encode_error(x, conn);
	break;
    }
}

