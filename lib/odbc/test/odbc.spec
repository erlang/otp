{topcase, {dir, "../odbc_test"}}.
{skip, {odbc_data_type_SUITE, varchar_upper_limit, "Known bug in database"}}.
{skip, {odbc_data_type_SUITE, text_upper_limit, "Consumes too much resources"}}.
{skip, {odbc_data_type_SUITE, bit_true , "Not supported by driver"}}.
{skip, {odbc_data_type_SUITE, bit_false, "Not supported by driver"}}.
{skip, {odbc_query_SUITE, multiple_select_result_sets,"Not supported by driver"}}.
{skip, {odbc_query_SUITE, multiple_mix_result_sets, "Not supported by driver"}}.
{skip, {odbc_query_SUITE, multiple_result_sets_error, "Not supported by driver"}}.
{skip, {odbc_query_SUITE, param_insert_tiny_int, "Not supported by driver"}}.