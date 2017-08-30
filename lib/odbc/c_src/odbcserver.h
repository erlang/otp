/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
 *

 */

/* ----------------------------- CONSTANTS ------------------------------*/

#define MAXCOLSIZE 8001
#define MAX_ERR_MSG 1024
#define ERRMSG_HEADR_SIZE 20
#define MAX_CONN_STR_OUT 1024
#define MAX_NAME 255
#define TRUNCATED "01004"
#define INFO "00000"
#define SQL_STATE_SIZE 6
#define TRUE 1
#define FALSE 0
#define WAIT_FOR_NEW_MSG 0
#define NEW_MSG_ARRIVED  1
#define DEC_NUM_LENGTH 50

/* 0 in this case (SQL_ATTR_CONNECTION_TIMEOUT) corresponds to erlang
   infinity. Erlang will handle all timeouts so we do not want any in the
   portprogram. */
#define TIME_OUT 0  

/* Constats defining the command protocol between the Erlang control process
   and the port program. These constants must also be defined in the same
   way in Erlang. */ 
#define OPEN_CONNECTION		1
#define CLOSE_CONNECTION	2
#define COMMIT_TRANSACTION	3
#define COMMIT			4
#define ROLLBACK		5
#define QUERY			6
#define SELECT_COUNT		7
#define SELECT_FIRST		8
#define SELECT_LAST		9
#define SELECT_NEXT		10
#define SELECT_PREV		11
#define SELECT			12
#define SELECT_RELATIVE		13
#define SELECT_ABSOLUTE		14
#define SELECT_N_NEXT		15
#define PARAM_QUERY		16
#define DESCRIBE                17
#define SHUTDOWN		18
#define LENGTH_INDICATOR_SIZE	4
#define INT_VALUE		1
#define STR_VALUE		2
#define ON		        1
#define OFF		        2
#define DUMMY_OFFSET            0

/* EXIT CODES */
#define EXIT_ALLOC		 2
#define EXIT_ENV		 3
#define EXIT_CONNECTION		 4
#define EXIT_FREE		 5
#define EXIT_STDIN_HEADER        6 
#define EXIT_STDIN_BODY		 7
#define EXIT_BIN		 8
#define EXIT_THREAD		 9
#define EXIT_PARAM_ARRAY         10
#define EXIT_OLD_WINSOCK	 11
#define EXIT_SOCKET_CONNECT      12  
#define EXIT_SOCKET_SEND_HEADER	 13
#define EXIT_SOCKET_SEND_BODY	 14
#define EXIT_SOCKET_RECV_MSGSIZE 15
#define EXIT_SOCKET_SEND_MSGSIZE 16
#define EXIT_SOCKET_RECV_HEADER	 17
#define EXIT_SOCKET_RECV_BODY    18
#define EXIT_COLS		 19
#define EXIT_ROWS		 20
#define EXIT_DESC		 21
#define EXIT_BIND		 22
#define EXIT_DRIVER_INFO         23

/* COL_SIZE */
#define COL_SQL_SMALLINT 5
#define COL_SQL_INTEGER 10
#define COL_SQL_REAL 7
#define COL_SQL_DOUBLE 15
#define COL_SQL_TINYINT 4
#define COL_SQL_TIMESTAMP 19

/* Types of parameters given to param_query*/
#define USER_SMALL_INT 1
#define USER_INT 2
#define USER_DECIMAL 3
#define USER_NMERIC 4
#define USER_CHAR 5
#define USER_VARCHAR 6
#define USER_FLOAT 7
#define USER_REAL 8
#define USER_DOUBLE 9
#define USER_BOOLEAN 10
#define USER_TINY_INT 11
#define USER_WCHAR 12
#define USER_WVARCHAR 13
#define USER_TIMESTAMP 14
#define USER_WLONGVARCHAR 15

/*------------------------   TYPDEFS  ----------------------------------*/

typedef char byte;
typedef int Boolean;

typedef struct {
    SQLSMALLINT c;
    SQLSMALLINT sql;
    SQLUINTEGER col_size;
    SQLSMALLINT decimal_digits;
    SQLLEN len;
    SQLLEN  strlen_or_indptr;
    SQLLEN *strlen_or_indptr_array; 
} col_type;

typedef struct {
    char *buffer;
    col_type type;
} db_column;

typedef struct {
    int length;
    byte *buffer;
    Boolean dyn_alloc; 
} db_result_msg;

typedef struct {
    SQLCHAR sqlState[SQL_STATE_SIZE];
    SQLINTEGER nativeError;
    byte error_msg[MAX_ERR_MSG];
} diagnos;

typedef struct {
    col_type type;
    int offset;
    SQLUSMALLINT input_output_type;
    union {
	byte *string;
	SQLINTEGER *integer;
	double *floating;
	byte *bool;
    }values;
} param_array;

typedef struct {
    SQLUSMALLINT params_processed;
    SQLUSMALLINT *param_status_array;  
} param_status; 

typedef struct {
    SQLHDBC connection_handle;     
    SQLHENV environment_handle;    
    SQLHSTMT statement_handle;
    db_column *columns;
    int number_of_columns;
    ei_x_buff dynamic_buffer;
    Boolean associated_result_set;
    Boolean use_srollable_cursors;
    Boolean tuple_row;
    Boolean binary_strings;
    Boolean exists_more_result_sets;
    Boolean param_query;
    Boolean out_params;
    Boolean extended_errors;
} db_state;

typedef enum {
	ERL_ODBC_IN, ERL_ODBC_OUT, ERL_ODBC_INOUT
} in_or_out_type;

#define connection_handle(db_state) (db_state -> connection_handle)
#define environment_handle(db_state) (db_state -> environment_handle)
#define statement_handle(db_state) (db_state -> statement_handle)
#define columns(db_state) (db_state -> columns)
#define nr_of_columns(db_state) (db_state -> number_of_columns)
#define dynamic_buffer(db_state) (db_state -> dynamic_buffer)
#define associated_result_set(db_state) (db_state -> associated_result_set)
#define use_srollable_cursors(db_state) (db_state -> use_srollable_cursors)
#define tuple_row(db_state) (db_state -> tuple_row)
#define binary_strings(db_state) (db_state -> binary_strings)
#define exists_more_result_sets(db_state) (db_state -> exists_more_result_sets)
#define param_query(db_state) (db_state -> param_query)
#define out_params(db_state) (db_state -> out_params)
#define extended_errors(db_state) (db_state -> extended_errors)
#define extended_error(db_state, errorcode) ( extended_errors(state) ? ((char *)errorcode) : NULL )
