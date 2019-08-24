/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

/*
  DESCRIPTION: Erlang ODBC (Open Database Connectivity) application. An
  erlang control process sends request to the c-process that queries the
  database using the Microsoft ODBC API. The c-process is implemented
  using two threads the supervisor thread and the database handler thread.
  If the database thread should hang erlang can close the c-process down
  by sendig a shutdown request to the supervisor thread.

  Erlang will start this c-process as a port-program and send information
  regarding inet-port nummbers through the erlang-port.
  After that c-process will communicate via sockets with erlang. The
  reason for this is that some odbc-drivers do unexpected things with
  stdin/stdout messing up the erlang-port communication.
  
  
  Command protocol between Erlang and C
   -------------------------------------
   The requests from Erlang to C are byte lists composed as [CommandByte,
   Bytes, StringTerminator]
   
   CommandByte - constants between 0 and 255
   identifing the request defined in odbc_internal.hrl and odbcserver.h

   Bytes - How to interpret this sequence of bytes depends on the
   CommandByte.

   StringTerminator - 0

   When the C-program processed the request from erlang it will use the
   ei-interface to create an Erlang term. This term will be sent the
   erlang via a socket. The Erlang control process, will forward
   it to the client that does binary_to_term before returning the result
   to the client program.

   Here follows a list of [CommandByte, Bytes] that describes the possible
   values. Note the Bytes part may be empty as in the case
   of ?CLOSE_CONNECTION and if integer values may be larger than 255
   they are converted to string values.

   [?OPEN_CONNECTION, C_AutoCommitMode, C_TraceDriver, C_SrollableCursors,
   C_TupelRow, BinaryStrings, ConnectionStr]
   [?CLOSE_CONNECTION]		     
   [?COMMIT_TRANSACTION, CommitMode]
   [?QUERY, SQLQuery]
   [?SELECT_COUNT, SQLQuery]
   [?SELECT, ?SELECT_FIRST]
   [?SELECT, ?SELECT_LAST]	
   [?SELECT, ?SELECT_NEXT]
   [?SELECT, ?SELECT_PREV]
   [?SELECT, CursorRelation, integer_to_list(OffSet), ";",
   integer_to_list(N), ";"]
   [?PARAM_QUERY, Binary] 

   C_AutoCommitMode - ?ON | ?OFF
   C_TraceDriver - ?ON | ?OFF
   C_SrollableCursors - ?ON | ?OFF
   C_TupelRow -  - ?ON | ?OFF
   BinaryStrings - ?ON | ?OFF
   ConnectionStr -  String
   CommitMode -  ?COMMIT | ?ROLLBACK
   SQLQuery  - String
   CursorRelation - ?SELECT_RELATIVE | ?SELECT_ABSOLUTE | ?SELECT_N_NEXT
   OffSet - integer
   N - integer
   Binary - binary encodede tuple of {SQLQuery, NoRows, Parameters}
   NoRows - integer
   Parameters - [{Datatype, InOrOut, Value}]
   InOrOut = [ERL_ODBC_IN | ERL_ODBC_OUT | ERL_ODBC_INOUT]
   Datatype -  USER_INT | USER_SMALL_INT | {USER_DECIMAL, Precision, Scale} |
   {USER_NMERIC, Precision, Scale} | {USER_CHAR, Max} | {USER_VARCHAR, Max} |
   {USER_WVARCHAR, Max} | {USER_FLOAT, Precision} | USER_REAL | USER_DOUBLE |
   USER_TIMESTAMP | {USER_WLONGVARCHAR, Max}
   Scale - integer
   Precision - integer
   Max - integer
*/

/* ----------------------------- INCLUDES ------------------------------*/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef UNIX
#include <unistd.h>
#include <netinet/tcp.h>
#endif 

#if defined WIN32
#include <winsock2.h> 
#include <windows.h> 
#include <ws2tcpip.h >
#include <fcntl.h>
#include <sql.h>
#include <sqlext.h>
#else
#include "sql.h"
#include "sqlext.h"
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <netdb.h>
#include <netinet/in.h>
#endif

#include <limits.h>

#include "ei.h"
#include "odbcserver.h"

/* ---------------- Main functions ---------------------------------------*/
static void spawn_sup(const char *port);
#ifdef WIN32
DWORD WINAPI database_handler(const char *port);
#else
void database_handler(const char *port);
#endif
static db_result_msg handle_db_request(byte *reqstring, db_state *state);
static void supervise(const char *port);
/* ----------------- ODBC functions --------------------------------------*/

static db_result_msg db_connect(byte *connStrIn, db_state *state);
static db_result_msg db_close_connection(db_state *state);
static db_result_msg db_end_tran(byte compleationtype, db_state *state);
static db_result_msg db_query(byte *sql, db_state *state);
static db_result_msg db_select_count(byte *sql,db_state *state);
static db_result_msg db_select(byte *args, db_state *state);
static db_result_msg db_param_query(byte *buffer, db_state *state);
static db_result_msg db_describe_table(byte *sql, db_state *state);

/* ------------- Encode/decode functions -------- ------------------------*/

static db_result_msg encode_empty_message(void);
static db_result_msg encode_error_message(char *reason, char *errCode, SQLINTEGER nativeError);
static db_result_msg encode_atom_message(char *atom);
static db_result_msg encode_result(db_state *state);
static db_result_msg encode_result_set(SQLSMALLINT num_of_columns,
				       db_state *state);
static db_result_msg encode_out_params(db_state *state,
                                       int cols,
                                       param_array *params,
                                       int num_param_values);
static db_result_msg encode_column_name_list(SQLSMALLINT num_of_columns,
					     db_state *state);
static db_result_msg encode_value_list(SQLSMALLINT num_of_columns,
				       db_state *state);
static db_result_msg encode_value_list_scroll(SQLSMALLINT num_of_columns,
					      SQLSMALLINT Orientation,
					      SQLINTEGER OffSet, int N,
					      db_state *state);
static db_result_msg encode_row_count(SQLINTEGER num_of_rows,
				      db_state *state);
static void encode_column_dyn(db_column column, int column_nr,
			      db_state *state);
static void encode_data_type(SQLSMALLINT sql_type, SQLINTEGER size,
			     SQLSMALLINT decimal_digits, db_state *state);
static Boolean decode_params(db_state *state, byte *buffer, int *index, param_array **params,
			     int i, int j, int num_param_values);

/*------------- Erlang port communication functions ----------------------*/

static int read_exact(byte *buf, int len);
static byte * receive_erlang_port_msg(void);

/* ------------- Socket communication functions --------------------------*/

#ifdef WIN32
static SOCKET connect_to_erlang(const char *port); 
static void send_msg(db_result_msg *msg, SOCKET socket);
static byte *receive_msg(SOCKET socket);
static Boolean receive_msg_part(SOCKET socket,
				byte * buffer, size_t msg_len);
static Boolean send_msg_part(SOCKET socket, byte * buffer, size_t msg_len);
static void close_socket(SOCKET socket);
static void init_winsock(void);
#elif defined(UNIX)
static int connect_to_erlang(const char *port);
static void send_msg(db_result_msg *msg, int socket);
static byte *receive_msg(int socket);
static Boolean receive_msg_part(int socket, byte * buffer, size_t msg_len);
static Boolean send_msg_part(int socket, byte * buffer, size_t msg_len);
static void close_socket(int socket); 
static void tcp_nodelay(int sock);
#endif
static void clean_socket_lib(void);

/*------------- Memory handling funtions --------------------------------*/

static void * safe_malloc(int size);
static void * safe_realloc(void * ptr, int size);
static db_column * alloc_column_buffer(int n);
static void free_column_buffer(db_column **columns, int n);
static void free_params(param_array **params, int cols);
static void clean_state(db_state *state);
static SQLLEN* alloc_strlen_indptr(int n, int val);

/* ------------- Init/map/bind/retrive functions -------------------------*/

static void init_driver(int erl_auto_commit_mode, int erl_trace_driver,
			   db_state *state);
static void init_param_column(param_array *params, byte *buffer, int *index,
			      int num_param_values, db_state* state);

static void init_param_statement(int cols,
				 SQLLEN num_param_values,
				 db_state *state,
				 param_status *status);

static void map_dec_num_2_c_column(col_type *type, int precision,
				   int scale);
static db_result_msg map_sql_2_c_column(db_column* column, db_state *state);


static param_array * bind_parameter_arrays(byte *buffer, int *index,
					   int cols,
					   int num_param_values,
					   db_state *state);
static void * retrive_param_values(param_array *Param);

static db_column retrive_binary_data(db_column column, int column_nr,
				     db_state *state);
static db_result_msg retrive_scrollable_cursor_support_info(db_state
							    *state);
static int num_out_params(int num_of_params, param_array* params);
/* ------------- Error handling functions --------------------------------*/

static diagnos get_diagnos(SQLSMALLINT handleType, SQLHANDLE handle, Boolean extendedErrors);

/* ------------- Boolean functions ---------------------------------------*/

static db_result_msg more_result_sets(db_state *state);
static Boolean sql_success(SQLRETURN result);
static void str_tolower(char *str, int len);

/* ----------------------------- CODE ------------------------------------*/

#if defined(WIN32)
#  define DO_EXIT(code) do { ExitProcess((code)); exit((code));} while (0)
/* exit() called only to avoid a warning */
#else
#  define DO_EXIT(code) exit((code))
#endif

/* ----------------- Main functions --------------------------------------*/

int main(void)
{
    byte *msg = NULL;
    char *temp = NULL, *supervisor_port = NULL, *odbc_port = NULL;
    size_t length;
#ifdef WIN32
    _setmode(_fileno( stdin),  _O_BINARY);
#endif
    
    msg = receive_erlang_port_msg();

    temp = strtok(msg, ";");
    if (temp == NULL)
	DO_EXIT(EXIT_STDIN_BODY);
    length = strlen(temp);
    supervisor_port = safe_malloc(length + 1);
    strcpy(supervisor_port, temp);

    temp = strtok(NULL, ";");
    if (temp == NULL)
	DO_EXIT(EXIT_STDIN_BODY);
    length = strlen(temp);
    odbc_port = safe_malloc(length + 1);
    strcpy(odbc_port, temp);
    
    free(msg);

    spawn_sup(supervisor_port);
    database_handler(odbc_port);

    return 0;
}

#ifdef WIN32
static void spawn_sup(const char *port)
{
    DWORD threadId;
    (HANDLE)_beginthreadex(NULL, 0, supervise, port, 0, &threadId);
}
#elif defined(UNIX)
static void spawn_sup(const char *port)
{
    pthread_t thread;
    int result;
    
    result = pthread_create(&thread, NULL,
			    (void *(*)(void *))supervise,
			    (void *)port);
    if (result != 0)
	DO_EXIT(EXIT_THREAD);
}
#endif   

void supervise(const char *port) {
    byte *msg = NULL;
    int reason;
#ifdef WIN32
    SOCKET socket;
    init_winsock();
#elif defined(UNIX)
    int socket; 
#endif
    
    socket = connect_to_erlang(port);
    msg = receive_msg(socket);

    if(msg[0] == SHUTDOWN) {
	reason = EXIT_SUCCESS;
    } else {
	reason = EXIT_FAILURE; /* Should not happen */
    }

    free(msg);
    close_socket(socket);
    clean_socket_lib();
    DO_EXIT(reason);    
}

#ifdef WIN32
DWORD WINAPI database_handler(const char *port)
#else
    void database_handler(const char *port) 
#endif
{ 
    db_result_msg msg;
    byte *request_buffer = NULL;
    db_state state =
    {NULL, NULL, NULL, NULL, 0, {NULL, 0, 0},
     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE};
    byte request_id;
#ifdef WIN32
    SOCKET socket;
    init_winsock();
#elif defined(UNIX)
    int socket;
#endif

    socket = connect_to_erlang(port);
  
    do {      
	request_buffer = receive_msg(socket);

	request_id = request_buffer[0];
	msg = handle_db_request(request_buffer, &state);

	send_msg(&msg, socket); /* Send answer to erlang */
	    
	if (msg.dyn_alloc) {
	    ei_x_free(&(state.dynamic_buffer));
	} else {
	    free(msg.buffer);
	    msg.buffer = NULL;
	}   
	
	free(request_buffer);
	request_buffer = NULL;
    
    } while(request_id != CLOSE_CONNECTION);

    shutdown(socket, 2);
    close_socket(socket);
    clean_socket_lib();
    /* Exit will be done by suervisor thread */ 
#ifdef WIN32
    return (DWORD)0;
#endif
}
 
/* Description: Calls the appropriate function to handle the database
   request recived from the erlang-process. Returns a message to send back
   to erlang. */
static db_result_msg handle_db_request(byte *reqstring, db_state *state)
{
    byte *args;
    byte request_id;
  
    /* First byte is an index that identifies the requested command the
       rest is the argument string. */
    request_id = reqstring[0]; 
    args = reqstring + sizeof(byte);
  
    switch(request_id) {
    case OPEN_CONNECTION:
	return db_connect(args, state); 
    case CLOSE_CONNECTION:
	return db_close_connection(state);
    case COMMIT_TRANSACTION:
	if(args[0] == COMMIT) {
	    return db_end_tran((byte)SQL_COMMIT, state);
	} else { /* args[0] == ROLLBACK */
	    return db_end_tran((byte)SQL_ROLLBACK, state);
	}
    case QUERY:
	return db_query(args, state);
    case SELECT_COUNT:
	return db_select_count(args, state);
    case SELECT:
	return db_select(args, state);
    case PARAM_QUERY:
	return db_param_query(args, state);
    case DESCRIBE:
	return db_describe_table(args, state);
    default:
	DO_EXIT(EXIT_FAILURE); /* Should not happen */
    }  
}
 
/* ----------------- ODBC-functions  ----------------------------------*/
 
/* Description: Tries to open a connection to the database using
   <connStrIn>, returns a message indicating the outcome. */
static db_result_msg db_connect(byte *args, db_state *state)
{
    /*
     * Danil Onishchenko aka RubberCthulhu, alevandal@gmail.com. 2013.01.09.
     * It's a fix for Oracle ODBC driver for Linux.
     * The issue: Oracle ODBC driver for Linux ignores setup autocommit mode
     * during driver initialization before a connection to database has been
     * established.
     * Solution: set autocommit mode after a connection to database has been
     * established.
     *
     * BEGIN
     */
    SQLLEN auto_commit_mode;
    /* END */

    SQLCHAR connStrOut[MAX_CONN_STR_OUT + 1] = {0};
    SQLRETURN result;
    SQLSMALLINT stringlength2ptr = 0, connlen;
    db_result_msg msg;
    diagnos diagnos;
    byte *connStrIn; 
    int erl_auto_commit_mode, erl_trace_driver,
	    use_srollable_cursors, tuple_row_state, binary_strings,
	    extended_errors;
  
    erl_auto_commit_mode = args[0];
    erl_trace_driver = args[1];
    use_srollable_cursors = args[2];
    tuple_row_state = args[3];
    binary_strings = args[4];
    extended_errors = args[5];
    connStrIn = args + 6 * sizeof(byte);

    if(tuple_row_state == ON) {
	    tuple_row(state) = TRUE;  
    } else {
	    tuple_row(state) = FALSE;  
    }
    
    if(binary_strings == ON) {
	    binary_strings(state) = TRUE;  
    } else {
	    binary_strings(state) = FALSE;  
    }
    
    if(use_srollable_cursors == ON) {
	    use_srollable_cursors(state) = TRUE;
    } else {
	    use_srollable_cursors(state) = FALSE;
    }

    if(extended_errors == ON) {
	    extended_errors(state) = TRUE;
    } else {
	    extended_errors(state) = FALSE;
    }

    init_driver(erl_auto_commit_mode, erl_trace_driver, state); 
      
    connlen = (SQLSMALLINT)strlen((const char*)connStrIn);
    result = SQLDriverConnect(connection_handle(state), NULL,
			      (SQLCHAR *)connStrIn, 
			      connlen, 
			      connStrOut, (SQLSMALLINT)MAX_CONN_STR_OUT,
			      &stringlength2ptr, SQL_DRIVER_NOPROMPT);
  
    if (!sql_success(result)) {
	diagnos = get_diagnos(SQL_HANDLE_DBC, connection_handle(state), extended_errors(state));
	strcat((char *)diagnos.error_msg,
	       " Connection to database failed.");
	msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError );
    
	if(!sql_success(SQLFreeHandle(SQL_HANDLE_DBC,
				      connection_handle(state))))
	    DO_EXIT(EXIT_FREE);
	if(!sql_success(SQLFreeHandle(SQL_HANDLE_ENV,
				      environment_handle(state))))
	    DO_EXIT(EXIT_FREE);
    
	return msg;
    }

    /*
     * Danil Onishchenko aka RubberCthulhu, alevandal@gmail.com. 2013.01.09.
     * It's a fix for Oracle ODBC driver for Linux.
     * The issue: Oracle ODBC driver for Linux ignores setup autocommit mode
     * during driver initialization before a connection to database has been
     * established.
     * Solution: set autocommit mode after a connection to database has been
     * established.
     *
     * BEGIN
     */
    if(erl_auto_commit_mode == ON) {
	auto_commit_mode = SQL_AUTOCOMMIT_ON;
    } else {
	auto_commit_mode = SQL_AUTOCOMMIT_OFF;
    }

    if(!sql_success(SQLSetConnectAttr(connection_handle(state),
				      SQL_ATTR_AUTOCOMMIT,
				      (SQLPOINTER)auto_commit_mode, 0))) {
	diagnos = get_diagnos(SQL_HANDLE_DBC, connection_handle(state), extended_errors(state));
	strcat((char *)diagnos.error_msg, " Set autocommit mode failed.");

	msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);

	if(!sql_success(SQLFreeHandle(SQL_HANDLE_DBC,
				      connection_handle(state))))
	    DO_EXIT(EXIT_FREE);
	if(!sql_success(SQLFreeHandle(SQL_HANDLE_ENV,
				      environment_handle(state))))
	    DO_EXIT(EXIT_FREE);

	return msg;
    }
    /* END */

    msg = retrive_scrollable_cursor_support_info(state);
  
    return msg;
}

/* Close the connection to the database. Returns an ok or error message. */
static db_result_msg db_close_connection(db_state *state)
{
    SQLRETURN result;
    diagnos diagnos;

    if (associated_result_set(state)) {
	clean_state(state);
    }
  
    result = SQLDisconnect(connection_handle(state));
                       
    if (!sql_success(result)) {
	diagnos = get_diagnos(SQL_HANDLE_DBC, connection_handle(state), extended_errors(state));
	return encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
    }

    if(!sql_success(SQLFreeHandle(SQL_HANDLE_DBC,
				  connection_handle(state))))
	DO_EXIT(EXIT_FREE);
    if(!sql_success(SQLFreeHandle(SQL_HANDLE_ENV,
				  environment_handle(state))))
	DO_EXIT(EXIT_FREE);

    return encode_atom_message("ok");
}
 

/* Description: Requests a commit or rollback operation for all active
   operations on all statements associated with the connection
   handle <connection_handle(state)>. Returns an ok or error message. */
static db_result_msg db_end_tran(byte compleationtype, db_state *state)
{
    SQLRETURN result;
    diagnos diagnos;

    result = SQLEndTran(SQL_HANDLE_DBC, connection_handle(state),
			(SQLSMALLINT)compleationtype);

    if (!sql_success(result)) {
	diagnos = get_diagnos(SQL_HANDLE_DBC, connection_handle(state), extended_errors(state));
	return encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
    } else {
	return encode_atom_message("ok");
    }
}
 
/* Description: Executes an sql query and encodes the result set as an
   erlang term into the message buffer of the returned message-struct. */
static db_result_msg db_query(byte *sql, db_state *state)
{
    SQLRETURN result;
    db_result_msg msg;
    diagnos diagnos;
    byte is_error[6];
    
    if (associated_result_set(state)) {
	clean_state(state);
    }
    associated_result_set(state) = FALSE;

    msg = encode_empty_message();
    
    if(!sql_success(SQLAllocHandle(SQL_HANDLE_STMT,
				   connection_handle(state),
				   &statement_handle(state))))
	DO_EXIT(EXIT_ALLOC);

    result = SQLExecDirect(statement_handle(state), (SQLCHAR *)sql, SQL_NTS);
    
    /* SQL_SUCCESS_WITH_INFO at this point may indicate an error in user input. */
    if (result != SQL_SUCCESS && result != SQL_NO_DATA_FOUND) {
	    diagnos =  get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
	    if(strcmp((char *)diagnos.sqlState, INFO) == 0) { 
		    is_error[0] = 0;
		    strncat((char *)is_error, (char *)diagnos.error_msg, 
			    5);
		    str_tolower((char *)&is_error, 5);
		    /* The ODBC error handling could have been more
		       predictable but alas ... we try to make the best of
		       it as we want a nice and clean Erlang API  */
		    if((strcmp((char *)is_error, "error") == 0))
		    { 
			    msg = encode_error_message((char *)diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
 			    clean_state(state); 
			    return msg;
		    }  
	    } else {
		    msg = encode_error_message((char *)diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
		    clean_state(state); 
		    return msg;   
	    }	    
    }

    ei_x_new_with_version(&dynamic_buffer(state));

    /* OTP-5759, fails when 0 rows deleted */
    if (result == SQL_NO_DATA_FOUND) {
	msg = encode_result(state);
    } else {
	/* Handle multiple result sets */
	do {
	    ei_x_encode_list_header(&dynamic_buffer(state), 1);
	    msg = encode_result(state);
	    /* We don't want to continue if an error occured */
	    if (msg.length != 0) { 
		break;
	    }
	    msg = more_result_sets(state);
	    /* We don't want to continue if an error occured */
	    if (msg.length != 0) { 
		break;
	    }
	} while (exists_more_result_sets(state)); 
	
	ei_x_encode_empty_list(&dynamic_buffer(state));
    }
	
    clean_state(state);
    
    if (msg.length != 0) { /* An error has occurred */
	ei_x_free(&(dynamic_buffer(state))); 
	return msg;
    } else {
	msg.buffer = dynamic_buffer(state).buff;
	msg.length = dynamic_buffer(state).index;
	msg.dyn_alloc = TRUE;
	return msg;
    }
}
 
/* Description: Executes an sql query. Returns number of rows in the result
   set. */
static db_result_msg db_select_count(byte *sql, db_state *state)
{
    SQLSMALLINT num_of_columns;
    SQLLEN num_of_rows;
    diagnos diagnos;

    if (associated_result_set(state)) {
	clean_state(state);
    }
    associated_result_set(state) = TRUE;
  
    if(!sql_success(SQLAllocHandle(SQL_HANDLE_STMT,
				   connection_handle(state),
				   &statement_handle(state))))
	DO_EXIT(EXIT_ALLOC);

    if(use_srollable_cursors(state)) {
	/* This function will fail if the driver does not support scrollable
	   cursors, this is expected and will not cause any damage*/
	SQLSetStmtAttr(statement_handle(state),
		       (SQLINTEGER)SQL_ATTR_CURSOR_SCROLLABLE,
		       (SQLPOINTER)SQL_SCROLLABLE, (SQLINTEGER)0);
    }
    
    if(!sql_success(SQLExecDirect(statement_handle(state), (SQLCHAR *)sql, SQL_NTS))) {
	diagnos = get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
	clean_state(state);
	return encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
    }
  
    if(!sql_success(SQLNumResultCols(statement_handle(state),
				     &num_of_columns)))
	DO_EXIT(EXIT_COLS);

    nr_of_columns(state) = (int)num_of_columns;
    columns(state) = alloc_column_buffer(nr_of_columns(state));
  
    if(!sql_success(SQLRowCount(statement_handle(state), &num_of_rows)))
	DO_EXIT(EXIT_ROWS);
  
    return encode_row_count(num_of_rows, state);
}

/* Description: Fetches rows from the result set associated with the
   connection by db_select_count. The method of seletion will be according
   too <args> */
static db_result_msg db_select(byte *args, db_state *state)
{
    db_result_msg msg;
    SQLSMALLINT num_of_columns;
    int offset, n, orientation;
    byte erlOrientation;

    erlOrientation = args[0];

    switch(erlOrientation) {
    case SELECT_FIRST:
	orientation = SQL_FETCH_FIRST;
	offset = DUMMY_OFFSET;
	n = 1;
	break;
    case SELECT_LAST:
	orientation = SQL_FETCH_LAST;
	offset = DUMMY_OFFSET;
	n = 1;
	break;
    case SELECT_NEXT:
	orientation = SQL_FETCH_NEXT;
	offset = DUMMY_OFFSET;
	n = 1;
	break;
    case SELECT_PREV:
	orientation = SQL_FETCH_PRIOR;
	offset = DUMMY_OFFSET;
	n = 1;
	break;
    case SELECT_ABSOLUTE:
	orientation = SQL_FETCH_ABSOLUTE;
	offset = atoi(strtok((char *)(args + sizeof(byte)), ";"));
	n =  atoi(strtok(NULL, ";"));
	break;
    case SELECT_RELATIVE:
	orientation = SQL_FETCH_RELATIVE;
	offset = atoi(strtok((char *)(args + sizeof(byte)), ";"));
	n =  atoi(strtok(NULL, ";"));
	break;
    case SELECT_N_NEXT:
	orientation = SQL_FETCH_NEXT;
	offset = atoi(strtok((char *)(args + sizeof(byte)), ";"));
	n =  atoi(strtok(NULL, ";"));
	break;
    default:
	DO_EXIT(EXIT_PARAM_ARRAY);
    }

    msg = encode_empty_message();
    
    ei_x_new(&dynamic_buffer(state));
    ei_x_new_with_version(&dynamic_buffer(state));
    ei_x_encode_tuple_header(&dynamic_buffer(state), 3);
    ei_x_encode_atom(&dynamic_buffer(state), "selected");

    num_of_columns = nr_of_columns(state);
    msg = encode_column_name_list(num_of_columns, state);
    if (msg.length == 0) { /* If no error has occurred */   
	msg = encode_value_list_scroll(num_of_columns,
				       (SQLSMALLINT)orientation,
				       (SQLINTEGER)offset,
				       n, state);
    }
  
    if (msg.length != 0) { /* An error has occurred */
	ei_x_free(&(dynamic_buffer(state))); 
	return msg;
    } else {
	msg.buffer = dynamic_buffer(state).buff;
  	msg.length = dynamic_buffer(state).index; 
	msg.dyn_alloc = TRUE;
	return msg;
    }
}

/* Description: Handles parameterized queries ex:
   INSERT INTO FOO VALUES(?, ?) */
static db_result_msg db_param_query(byte *buffer, db_state *state)
{
    byte *sql; 
    db_result_msg msg;
    SQLLEN num_param_values;
    int i, ver = 0,
       erl_type = 0, index = 0, size = 0, cols = 0;
    long long_num_param_values;  
    param_status param_status;
    diagnos diagnos;
    param_array *params;
    SQLRETURN result;

    if (associated_result_set(state)) {
	clean_state(state);
    }
    associated_result_set(state) = FALSE;
    param_query(state) = TRUE;
    out_params(state) = FALSE;

    msg = encode_empty_message();

    ei_decode_version(buffer, &index, &ver);
    
    ei_decode_tuple_header(buffer, &index, &size);

    ei_get_type(buffer, &index, &erl_type, &size);

    sql = (byte*)safe_malloc((sizeof(byte) * (size + 1)));
    ei_decode_string(buffer, &index, sql); 

    ei_decode_long(buffer, &index, &long_num_param_values);

    num_param_values = (SQLLEN)long_num_param_values;
    ei_decode_list_header(buffer, &index, &cols);

    
    init_param_statement(cols, num_param_values, state, &param_status);

    params = bind_parameter_arrays(buffer, &index, cols,  
  				   num_param_values, state);  
    
    if(params != NULL) {

	result = SQLExecDirect(statement_handle(state), (SQLCHAR *)sql, SQL_NTS);
	if (!sql_success(result) || result == SQL_NO_DATA) {
		diagnos = get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
	}
	/* SQL_NO_DATA and SQLSTATE 00000 indicate success for
	   updates/deletes that affect no rows */
	if(!sql_success(result) &&
	   !(result == SQL_NO_DATA && !strcmp((char *)diagnos.sqlState, INFO))) {
		msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
	} else {
	    for (i = 0; i < param_status.params_processed; i++) {
		switch (param_status.param_status_array[i]) {
		case SQL_PARAM_SUCCESS:
		case SQL_PARAM_SUCCESS_WITH_INFO:
			/* SQL_PARAM_DIAG_UNAVAILABLE is entered when the
			 * driver treats arrays of parameters as a monolithic
			 * unit, so it does not generate this individual
			 * parameter level of error information. */	
		case SQL_PARAM_DIAG_UNAVAILABLE:
		break;
		default:
		    diagnos =
			get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
		    msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
		    i = param_status.params_processed;
		    break;
		}
	    }
	    if(msg.length == 0) {
		ei_x_new_with_version(&dynamic_buffer(state));
                if(out_params(state)){
                    msg = encode_out_params(state, cols, params, num_param_values);
                }else{
                    msg = encode_result(state);
                }
		if(msg.length == 0) {
		    msg.buffer = dynamic_buffer(state).buff;
  		    msg.length = dynamic_buffer(state).index; 
		    msg.dyn_alloc = TRUE;
		} else { /* Error occurred */
		    ei_x_free(&(dynamic_buffer(state)));
		}
	    }
	}
    
	if(!sql_success(SQLFreeStmt(statement_handle(state),
				    SQL_RESET_PARAMS))) {
	    DO_EXIT(EXIT_FREE);
	} 
    } else {
	msg = encode_atom_message("param_badarg");
    }
    
    free(sql); 

    free_params(&params, cols);
    
    free(param_status.param_status_array);     

    if(!sql_success(SQLFreeHandle(SQL_HANDLE_STMT,
				  statement_handle(state)))){
	DO_EXIT(EXIT_FREE);
    }
    statement_handle(state) = NULL;
    param_query(state) = FALSE;
    return msg;
}


static db_result_msg db_describe_table(byte *sql, db_state *state)
{
    db_result_msg msg; 
    SQLSMALLINT num_of_columns;
    SQLCHAR name[MAX_NAME];
    SQLSMALLINT name_len, sql_type, dec_digits, nullable;
    SQLULEN size;
    diagnos diagnos;
    int i;
    
    if (associated_result_set(state)) {
	clean_state(state);
    }
    associated_result_set(state) = FALSE;

    msg = encode_empty_message();
    
    if(!sql_success(SQLAllocHandle(SQL_HANDLE_STMT,
				   connection_handle(state),
				   &statement_handle(state))))
	DO_EXIT(EXIT_ALLOC);
    
    if (!sql_success(SQLPrepare(statement_handle(state), (SQLCHAR *)sql, SQL_NTS))){
	diagnos =  get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
	msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
	clean_state(state);
	return msg;
    }
    
    if(!sql_success(SQLNumResultCols(statement_handle(state),
				     &num_of_columns))) {
	diagnos =  get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
	msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
	clean_state(state);
	return msg;
    }
    
    ei_x_new_with_version(&dynamic_buffer(state));
    ei_x_encode_tuple_header(&dynamic_buffer(state), 2);
    ei_x_encode_atom(&dynamic_buffer(state), "ok");
    ei_x_encode_list_header(&dynamic_buffer(state), num_of_columns);
    
    for (i = 0; i < num_of_columns; ++i) {
	
	if(!sql_success(SQLDescribeCol(statement_handle(state),
				       (SQLUSMALLINT)(i+1),
				       name, sizeof(name), &name_len,
				       &sql_type, &size, &dec_digits,
				       &nullable)))
	    DO_EXIT(EXIT_DESC);

	ei_x_encode_tuple_header(&dynamic_buffer(state), 2);
	ei_x_encode_string_len(&dynamic_buffer(state),
			       (char *)name, name_len);
	encode_data_type(sql_type, size, dec_digits, state);
    }

    ei_x_encode_empty_list(&dynamic_buffer(state));
    
    clean_state(state);
    msg.buffer = dynamic_buffer(state).buff;
    msg.length = dynamic_buffer(state).index; 
    msg.dyn_alloc = TRUE;
    return msg;
}


/* ----------------- Encode/decode functions -----------------------------*/

static db_result_msg encode_empty_message(void)
{
    db_result_msg msg;

    msg.length = 0;
    msg.buffer = NULL;
    msg.dyn_alloc = FALSE;

    return msg;
}

/* Description: Encode an error-message to send back to erlang*/
static db_result_msg encode_error_message(char *reason, char *errCode, SQLINTEGER nativeError )
{
    int index;
    db_result_msg msg;
  
    index = 0;
    ei_encode_version(NULL, &index);
    ei_encode_tuple_header(NULL, &index, 2);
    ei_encode_atom(NULL, &index, "error");
    if (errCode)
    {
        ei_encode_tuple_header(NULL, &index, 3);
        ei_encode_string(NULL, &index, errCode);
        ei_encode_long(NULL, &index, nativeError);
    }
    ei_encode_string(NULL, &index, reason);
  
    msg.length = index;
    msg.buffer = (byte *)safe_malloc(index);
    msg.dyn_alloc = FALSE;
  
    index = 0;
    ei_encode_version((char *)msg.buffer, &index);
    ei_encode_tuple_header((char *)msg.buffer, &index, 2);
    ei_encode_atom((char *)msg.buffer, &index, "error");
    if (errCode)
    {
        ei_encode_tuple_header((char *)msg.buffer, &index, 3);
        ei_encode_string((char *)msg.buffer, &index, errCode);
        ei_encode_long((char *)msg.buffer, &index, nativeError);
    }
    ei_encode_string((char *)msg.buffer, &index, reason);
  
    return msg;
}

/* Description: Encode a messge that is a erlang atom */
static db_result_msg encode_atom_message(char* atom)
{
    int index;
    db_result_msg msg;
  
    index = 0;
    ei_encode_version(NULL, &index);
    ei_encode_atom(NULL, &index, atom);

    msg.length = index;
    msg.buffer = (byte *)safe_malloc(index);
    msg.dyn_alloc = FALSE;
  
    index = 0;
    ei_encode_version((char *)msg.buffer, &index);
    ei_encode_atom((char *)msg.buffer, &index, atom);
  
    return msg;
}


/* Top encode function for db_query that encodes the resulting erlang
   term to be returned to the erlang client. */
static db_result_msg encode_result(db_state *state)
{
    SQLSMALLINT num_of_columns = 0;
    SQLLEN RowCountPtr = 0;
    SQLINTEGER paramBatch = 0;
    db_result_msg msg;
    int elements, update, num_of_rows = 0;
    char *atom;
    diagnos diagnos;

    msg = encode_empty_message();
    
    if(!sql_success(SQLNumResultCols(statement_handle(state), 
  				     &num_of_columns))) { 
	    diagnos =  get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
	    msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
	    clean_state(state);
	    return msg;
    } 
    
    if (num_of_columns == 0) { 
        elements = 2;
        atom = "updated";
        update = TRUE;
    } else {
	elements = 3;
	atom = "selected";
	update = FALSE;
    }
    
    if(!sql_success(SQLRowCount(statement_handle(state), &RowCountPtr))) { 
	    diagnos =  get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
	    msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
	    clean_state(state);
	    return msg;
    }

    if(param_query(state) && update) {
	if(!sql_success(SQLGetInfo(connection_handle(state),
				   SQL_PARAM_ARRAY_ROW_COUNTS,
				   (SQLPOINTER)&paramBatch,
				   sizeof(paramBatch),
				   NULL))) {
	    DO_EXIT(EXIT_DRIVER_INFO);
	}
	
	if(paramBatch == SQL_PARC_BATCH ) {
	    /* Individual row counts (one for each parameter set)
	       are available, sum them up */
	    do {
		num_of_rows = num_of_rows + (int)RowCountPtr;
		msg = more_result_sets(state);
		/* We don't want to continue if an error occured */
		if (msg.length != 0) { 
		    return msg;
		}
		if(exists_more_result_sets(state)) {
		    if(!sql_success(SQLRowCount(statement_handle(state),
						&RowCountPtr))) { 
			DO_EXIT(EXIT_ROWS); 
		    }
		}
	    } while (exists_more_result_sets(state));
	} else {
	    /* Row counts are rolled up into one (SQL_PARC_NO_BATCH) */
	    num_of_rows = (int)RowCountPtr;
	}    
    } else { 
	num_of_rows = (int)RowCountPtr;
    }
    ei_x_encode_tuple_header(&dynamic_buffer(state), elements);
    ei_x_encode_atom(&dynamic_buffer(state), atom);
    if (update) {
	if(num_of_rows < 0 ) {
	    ei_x_encode_atom(&dynamic_buffer(state), "undefined");
	} else {
	    ei_x_encode_long(&dynamic_buffer(state), num_of_rows);
	}
    } else {
	msg = encode_result_set(num_of_columns, state);
    }
    return msg;
}
 
static db_result_msg encode_out_params(db_state *state,
                                       int num_of_params,
                                       param_array* params,
                                       int num_param_values)
{
    int num_of_columns = 0;
    int i = 0;
    int j = 0;
    param_array column;
    db_result_msg msg;
    TIMESTAMP_STRUCT* ts;
    msg = encode_empty_message();
    
    ei_x_encode_tuple_header(&dynamic_buffer(state), 3);
    ei_x_encode_atom(&dynamic_buffer(state), "executed");

    num_of_columns = num_out_params(num_of_params, params);
    ei_x_encode_long(&dynamic_buffer(state), num_of_columns);

    ei_x_encode_list_header(&dynamic_buffer(state), num_param_values);
    for(j =0; j < num_param_values; j ++){
    
        if(tuple_row(state)) {
            ei_x_encode_tuple_header(&dynamic_buffer(state), num_of_columns);

        } else {
            ei_x_encode_list_header(&dynamic_buffer(state), num_of_columns);
        }
    
        for (i = 0; i< num_of_params; i++) {
            if(params[i].input_output_type==SQL_PARAM_INPUT){
                continue;
            }
            column = params[i];
            if (column.type.len == 0 ||
                column.type.strlen_or_indptr == SQL_NULL_DATA) {
                ei_x_encode_atom(&dynamic_buffer(state), "null");
            } else {
                void* values = retrive_param_values(&column);
                switch(column.type.c) {
                case SQL_C_TYPE_TIMESTAMP:
                  ts = (TIMESTAMP_STRUCT*) values;
                  ei_x_encode_tuple_header(&dynamic_buffer(state), 2);
                  ei_x_encode_tuple_header(&dynamic_buffer(state), 3);
                  ei_x_encode_long(&dynamic_buffer(state), (long)(ts->year));
                  ei_x_encode_long(&dynamic_buffer(state), (long)(ts->month));
                  ei_x_encode_long(&dynamic_buffer(state), (long)(ts->day));
                  ei_x_encode_tuple_header(&dynamic_buffer(state), 3);
                  ei_x_encode_long(&dynamic_buffer(state), (long)(ts->hour));
                  ei_x_encode_long(&dynamic_buffer(state), (long)(ts->minute));
                  ei_x_encode_long(&dynamic_buffer(state), (long)(ts->second));
                  break;
                case SQL_C_CHAR:
			if binary_strings(state) {
				ei_x_encode_binary(&dynamic_buffer(state),
						   ((char*)values)+j*column.type.len,
						   (column.type.strlen_or_indptr_array[j]));
			}
			else {
				ei_x_encode_string(&dynamic_buffer(state),
						   ((char*)values)+j*column.type.len);
			}
			break;
                case SQL_C_WCHAR:
			ei_x_encode_binary(&dynamic_buffer(state),
					   ((char*)values)+j*column.type.len,
					   (column.type.strlen_or_indptr_array[j]));
			break;
                case SQL_C_SLONG:
                    ei_x_encode_long(&dynamic_buffer(state), ((SQLINTEGER*)values)[j]);
                    break;
                case SQL_C_DOUBLE:
                    ei_x_encode_double(&dynamic_buffer(state),
                                       ((double*)values)[j]);
                    break;
                case SQL_C_BIT:
                    ei_x_encode_atom(&dynamic_buffer(state),
                                     ((byte*)values)[j]==TRUE?"true":"false");
                    break;
                default:
                    ei_x_encode_atom(&dynamic_buffer(state), "error");
                    break;
                }
            } 
        } 
        if(!tuple_row(state)) {
            ei_x_encode_empty_list(&dynamic_buffer(state));
        }
    }
    ei_x_encode_empty_list(&dynamic_buffer(state));
    return msg;
}  

static int num_out_params(int num_of_params, param_array* params)
{
    int ret = 0;
    int i = 0;
    for(i=0; i < num_of_params; i++){
        if(params[i].input_output_type==SQL_PARAM_INPUT_OUTPUT ||
           params[i].input_output_type==SQL_PARAM_OUTPUT)
            ret++;
    }
    return ret;
}

/* Description: Encodes the result set into the "ei_x" - dynamic_buffer
   held by the state variable */
static db_result_msg encode_result_set(SQLSMALLINT num_of_columns,
				       db_state *state)
{
    db_result_msg msg;

    columns(state) = alloc_column_buffer(num_of_columns);
  
    msg = encode_column_name_list(num_of_columns, state);
    if (msg.length == 0) { /* If no error has occurred */   
	msg = encode_value_list(num_of_columns, state);
    }

    free_column_buffer(&(columns(state)), num_of_columns);
    
    return msg;
}  

/* Description: Encodes the list of column names into the "ei_x" -
   dynamic_buffer held by the state variable */
static db_result_msg encode_column_name_list(SQLSMALLINT num_of_columns,
					     db_state *state)
{
    int i;
    db_result_msg msg;
    SQLCHAR name[MAX_NAME];
    SQLSMALLINT name_len, sql_type, dec_digits, nullable;
    SQLULEN size;

    msg = encode_empty_message();
    
    ei_x_encode_list_header(&dynamic_buffer(state), num_of_columns);
  
    for (i = 0; i < num_of_columns; ++i) {

	if(!sql_success(SQLDescribeCol(statement_handle(state),
				       (SQLSMALLINT)(i+1),
				       name, sizeof(name), &name_len,
				       &sql_type, &size, &dec_digits,
				       &nullable)))
	    DO_EXIT(EXIT_DESC);

	if(sql_type == SQL_LONGVARCHAR || sql_type == SQL_LONGVARBINARY || sql_type == SQL_WLONGVARCHAR)
	    size = MAXCOLSIZE;
    
	(columns(state)[i]).type.decimal_digits = dec_digits;
	(columns(state)[i]).type.sql = sql_type;
	(columns(state)[i]).type.col_size = size;
      
	msg = map_sql_2_c_column(&columns(state)[i], state);
	if (msg.length > 0) {
	    return msg; /* An error has occurred */
	} else {
	    if (columns(state)[i].type.len > 0) {
		columns(state)[i].buffer =
		    (char *)safe_malloc(columns(state)[i].type.len);
	
		if (columns(state)[i].type.c == SQL_C_BINARY) {
		    /* retrived later by retrive_binary_data */
		} else {
		    if(!sql_success(
			SQLBindCol
			(statement_handle(state),
			 (SQLSMALLINT)(i+1),
			 columns(state)[i].type.c,
			 columns(state)[i].buffer,
			 columns(state)[i].type.len,
			 &columns(state)[i].type.strlen_or_indptr)))
			DO_EXIT(EXIT_BIND);
		}
		ei_x_encode_string_len(&dynamic_buffer(state),
				       (char *)name, name_len);
	    }
	    else {
		columns(state)[i].type.len = 0;
		columns(state)[i].buffer = NULL;
	    }
	}  
    }
    ei_x_encode_empty_list(&dynamic_buffer(state)); 

    return msg;
}

/* Description: Encodes the list(s) of row values fetched by SQLFetch into
   the "ei_x" - dynamic_buffer held by the state variable */
static db_result_msg encode_value_list(SQLSMALLINT num_of_columns,
				       db_state *state)
{
    int i;
    SQLRETURN result;
    db_result_msg msg;

    msg = encode_empty_message();
        
    for (;;) {
	/* fetch the next row */
	result = SQLFetch(statement_handle(state)); 
    
	if (result == SQL_NO_DATA) /* Reached end of result set */
	{
	    break;
	}

	ei_x_encode_list_header(&dynamic_buffer(state), 1);

	if(tuple_row(state)) {
	    ei_x_encode_tuple_header(&dynamic_buffer(state),
				     num_of_columns);
	} else {
	    ei_x_encode_list_header(&dynamic_buffer(state), num_of_columns);
	}
    
	for (i = 0; i < num_of_columns; i++) {
	    encode_column_dyn(columns(state)[i], i, state);
	}

	if(!tuple_row(state)) {
	    ei_x_encode_empty_list(&dynamic_buffer(state));
	}
    } 
    ei_x_encode_empty_list(&dynamic_buffer(state));
    return msg;
}

/* Description: Encodes the list(s) of row values fetched with
   SQLFetchScroll into the "ei_x" - dynamic_buffer held by the state
   variable */
static db_result_msg encode_value_list_scroll(SQLSMALLINT num_of_columns,
					      SQLSMALLINT Orientation,
					      SQLINTEGER OffSet, int N,
					      db_state *state)
{
    int i, j;
    SQLRETURN result;
    db_result_msg msg;

    msg = encode_empty_message();
    
    for (j = 0; j < N; j++) {
	if((j > 0) && (Orientation == SQL_FETCH_ABSOLUTE)) {
	    OffSet++;
	}
    
	if((j == 1) && (Orientation == SQL_FETCH_RELATIVE)) {
	    OffSet = 1;
	}

	result = SQLFetchScroll(statement_handle(state), Orientation,
				OffSet); 
    
	if (result == SQL_NO_DATA) /* Reached end of result set */
	{
	    break;
	}
	ei_x_encode_list_header(&dynamic_buffer(state), 1);

	if(tuple_row(state)) {
	    ei_x_encode_tuple_header(&dynamic_buffer(state),
				     num_of_columns);
	} else {
	    ei_x_encode_list_header(&dynamic_buffer(state), num_of_columns);
	}
	for (i = 0; i < num_of_columns; i++) {
	    encode_column_dyn(columns(state)[i], i, state);
	}
	if(!tuple_row(state)) {
	    ei_x_encode_empty_list(&dynamic_buffer(state));
	}
    } 
    ei_x_encode_empty_list(&dynamic_buffer(state));
    return msg;
}

/* Encodes row count result for erlang  */
static db_result_msg encode_row_count(SQLINTEGER num_of_rows,
				      db_state *state)
{
    db_result_msg msg;
    int index;
  
    index = 0;
    ei_encode_version(NULL, &index);
    ei_encode_tuple_header(NULL, &index, 2);
    ei_encode_atom(NULL, &index, "ok");
    if(num_of_rows == -1)
    {
	ei_encode_atom(NULL, &index, "undefined");
    } else {
	ei_encode_long(NULL, &index, num_of_rows);
    } 
    msg.length = index;
    msg.buffer = (byte *)safe_malloc(index);
    msg.dyn_alloc = FALSE;
  
    index = 0;
    ei_encode_version((char *)msg.buffer, &index);
    ei_encode_tuple_header((char *)msg.buffer, &index, 2);
    ei_encode_atom((char *)msg.buffer, &index, "ok");
  
    if(num_of_rows == -1)
    {
	ei_encode_atom((char *)msg.buffer, &index, "undefined");
    } else {
	ei_encode_long((char *)msg.buffer, &index, num_of_rows);
    }
    return msg;
}
 
/* Description: Encodes the a column value into the "ei_x" - dynamic_buffer
   held by the state variable */
static void encode_column_dyn(db_column column, int column_nr,
			      db_state *state)
{
    TIMESTAMP_STRUCT* ts;
    if (column.type.len == 0 ||
	column.type.strlen_or_indptr == SQL_NULL_DATA) {
	ei_x_encode_atom(&dynamic_buffer(state), "null");
    } else {
	switch(column.type.c) {
	case SQL_C_TYPE_TIMESTAMP:
            ts = (TIMESTAMP_STRUCT*)column.buffer;
            ei_x_encode_tuple_header(&dynamic_buffer(state), 2);
            ei_x_encode_tuple_header(&dynamic_buffer(state), 3);
            ei_x_encode_ulong(&dynamic_buffer(state), ts->year);
            ei_x_encode_ulong(&dynamic_buffer(state), ts->month);
            ei_x_encode_ulong(&dynamic_buffer(state), ts->day);
            ei_x_encode_tuple_header(&dynamic_buffer(state), 3);
            ei_x_encode_ulong(&dynamic_buffer(state), ts->hour);
            ei_x_encode_ulong(&dynamic_buffer(state), ts->minute);
            ei_x_encode_ulong(&dynamic_buffer(state), ts->second);
            break;
	case SQL_C_CHAR:
		if binary_strings(state) {
			 ei_x_encode_binary(&dynamic_buffer(state), 
					    column.buffer,column.type.strlen_or_indptr);
		} else {
			ei_x_encode_string(&dynamic_buffer(state), column.buffer);
		}
	    break;
	case SQL_C_WCHAR:
            ei_x_encode_binary(&dynamic_buffer(state), 
                               column.buffer,column.type.strlen_or_indptr);
	    break;
	case SQL_C_SLONG:
	    ei_x_encode_long(&dynamic_buffer(state),
	    	*(SQLINTEGER*)column.buffer);
	    break;
	case SQL_C_DOUBLE:
	    ei_x_encode_double(&dynamic_buffer(state),
			       *(double*)column.buffer);
	    break;
	case SQL_C_BIT:
	    ei_x_encode_atom(&dynamic_buffer(state),
			     column.buffer[0]?"true":"false");
	    break;
	case SQL_C_BINARY:		
	    column = retrive_binary_data(column, column_nr, state);
	    if binary_strings(state) {
		    ei_x_encode_binary(&dynamic_buffer(state), 
				       column.buffer,column.type.strlen_or_indptr);
	    } else {
		    ei_x_encode_string(&dynamic_buffer(state), (void *)column.buffer);
	    }
	    break;
	default:
	    ei_x_encode_atom(&dynamic_buffer(state), "error");
	    break;
	}
    } 
}

static void encode_data_type(SQLSMALLINT sql_type, SQLINTEGER size,
			     SQLSMALLINT decimal_digits, db_state *state)
{
    switch(sql_type) {
    case SQL_CHAR:
	ei_x_encode_tuple_header(&dynamic_buffer(state), 2);	
	ei_x_encode_atom(&dynamic_buffer(state), "sql_char");
	ei_x_encode_long(&dynamic_buffer(state), size);
	break;
    case SQL_VARCHAR:
	ei_x_encode_tuple_header(&dynamic_buffer(state), 2);	
	ei_x_encode_atom(&dynamic_buffer(state), "sql_varchar");
	ei_x_encode_long(&dynamic_buffer(state), size);
	break;
    case SQL_WCHAR:
	ei_x_encode_tuple_header(&dynamic_buffer(state), 2);	
	ei_x_encode_atom(&dynamic_buffer(state), "sql_wchar");
	ei_x_encode_long(&dynamic_buffer(state), size);
	break;
    case SQL_WVARCHAR:
	ei_x_encode_tuple_header(&dynamic_buffer(state), 2);	
	ei_x_encode_atom(&dynamic_buffer(state), "sql_wvarchar");
	ei_x_encode_long(&dynamic_buffer(state), size);
	break;
    case SQL_NUMERIC:
	ei_x_encode_tuple_header(&dynamic_buffer(state), 3);	
	ei_x_encode_atom(&dynamic_buffer(state), "sql_numeric");
	ei_x_encode_long(&dynamic_buffer(state), size);
	ei_x_encode_long(&dynamic_buffer(state), decimal_digits);
	break;
    case SQL_DECIMAL:
	ei_x_encode_tuple_header(&dynamic_buffer(state), 3);	
	ei_x_encode_atom(&dynamic_buffer(state), "sql_decimal");
	ei_x_encode_long(&dynamic_buffer(state), size);
	ei_x_encode_long(&dynamic_buffer(state), decimal_digits);
	break;
    case SQL_INTEGER:
	ei_x_encode_atom(&dynamic_buffer(state), "sql_integer");
	break;
    case SQL_TINYINT:
	ei_x_encode_atom(&dynamic_buffer(state), "sql_tinyint");
	break;
    case SQL_SMALLINT:
	ei_x_encode_atom(&dynamic_buffer(state), "sql_smallint");
	break;
    case SQL_REAL:
	ei_x_encode_atom(&dynamic_buffer(state), "sql_real");
	break;
    case SQL_FLOAT:
	ei_x_encode_tuple_header(&dynamic_buffer(state), 2);	
	ei_x_encode_atom(&dynamic_buffer(state), "sql_float");
	ei_x_encode_long(&dynamic_buffer(state), size);
	break;
    case SQL_DOUBLE:
	ei_x_encode_atom(&dynamic_buffer(state), "sql_double");
	break;
    case SQL_BIT:
	ei_x_encode_atom(&dynamic_buffer(state), "sql_bit");
	break;
    case SQL_TYPE_DATE:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_TYPE_DATE");
	break;
    case SQL_TYPE_TIME:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_TYPE_TIME");
	break;
    case SQL_TYPE_TIMESTAMP:
	ei_x_encode_atom(&dynamic_buffer(state), "sql_timestamp");
	break;
    case SQL_BIGINT:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_BIGINT");
	break;
    case SQL_BINARY:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_BINARY");
	break;
    case SQL_LONGVARCHAR:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_LONGVARCHAR");
	break;
    case SQL_WLONGVARCHAR:
	ei_x_encode_tuple_header(&dynamic_buffer(state), 2);
	ei_x_encode_atom(&dynamic_buffer(state), "sql_wlongvarchar");
	ei_x_encode_long(&dynamic_buffer(state), size);
	break;
    case SQL_VARBINARY:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_VARBINARY");
	break;
    case SQL_LONGVARBINARY:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_LONGVARBINARY");
	break;
    case SQL_INTERVAL_MONTH:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_INTERVAL_MONTH");
	break;
    case SQL_INTERVAL_YEAR:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_INTERVAL_YEAR");
	break;
    case SQL_INTERVAL_DAY:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_INTERVAL_DAY");
	break;
    case SQL_INTERVAL_MINUTE:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_INTERVAL_MINUTE");
	break;
    case SQL_INTERVAL_HOUR_TO_SECOND:
	ei_x_encode_atom(&dynamic_buffer(state),
			 "SQL_INTERVAL_HOUR_TO_SECOND");
	break;
    case SQL_INTERVAL_MINUTE_TO_SECOND:
	ei_x_encode_atom(&dynamic_buffer(state),
			 "SQL_INTERVAL_MINUTE_TO_SECOND");
	break;
    case SQL_UNKNOWN_TYPE:
	ei_x_encode_atom(&dynamic_buffer(state), "SQL_UNKNOWN_TYPE");
	break;
    default: /* Will probably never happen */
	ei_x_encode_atom(&dynamic_buffer(state), "ODBC_UNSUPPORTED_TYPE");
	break;
    }
}

static Boolean decode_params(db_state *state, byte *buffer, int *index, param_array **params,
			     int i, int j, int num_param_values)
{
    int erl_type, size;
    long bin_size, l64;
    long val;
    param_array* param;
    TIMESTAMP_STRUCT* ts;
    char atomarray[MAXATOMLEN+1];
    
    ei_get_type(buffer, index, &erl_type, &size);
    param = &(*params)[i];

    if(erl_type == ERL_ATOM_EXT) {
	ei_decode_atom(buffer, index, atomarray);
	if(strncmp(atomarray, "null", 4) == 0 ) {
	    param->offset += param->type.len;

	    if(!param->type.strlen_or_indptr_array)
		param->type.strlen_or_indptr_array = alloc_strlen_indptr(num_param_values, param->type.len);

	    param->type.strlen_or_indptr_array[j] = SQL_NULL_DATA;
	    return TRUE;
	}
    }

    switch (param->type.c) {
    case SQL_C_CHAR:
	    if (binary_strings(state)) {
		    ei_decode_binary(buffer, index,
				     &(param->values.string[param->offset]), &bin_size);
		    param->offset += param->type.len;
	    } else {
		    if(erl_type != ERL_STRING_EXT) {
			    return FALSE;
		    }
		    ei_decode_string(buffer, index, &(param->values.string[param->offset]));
		    param->offset += param->type.len;
	    }
	    break;
    case SQL_C_WCHAR:
	    ei_decode_binary(buffer, index, &(param->values.string[param->offset]), &bin_size);
	    param->offset += param->type.len;
	    break;
    case SQL_C_TYPE_TIMESTAMP:
	    ts = (TIMESTAMP_STRUCT*) param->values.string;
	    ei_decode_tuple_header(buffer, index, &size);
	    ei_decode_long(buffer, index, &val);
	    ts[j].year = (SQLUSMALLINT)val;
	    ei_decode_long(buffer, index, &val);
	    ts[j].month = (SQLUSMALLINT)val;
	    ei_decode_long(buffer, index, &val);
	    ts[j].day = (SQLUSMALLINT)val;
	    ei_decode_long(buffer, index, &val);
	    ts[j].hour = (SQLUSMALLINT)val;
	    ei_decode_long(buffer, index, &val);
	    ts[j].minute = (SQLUSMALLINT)val;
	    ei_decode_long(buffer, index, &val);
	    ts[j].second = (SQLUSMALLINT)val;
	    ts[j].fraction = (SQLINTEGER)0;
	    break;
    case SQL_C_SLONG:
	    if(!((erl_type == ERL_SMALL_INTEGER_EXT) ||
		 (erl_type == ERL_INTEGER_EXT) ||
		 (erl_type == ERL_SMALL_BIG_EXT) ||
		 (erl_type == ERL_LARGE_BIG_EXT))) {
		    return FALSE;
	    }
	    
	    if(ei_decode_long(buffer, index, &l64)) {
		    return FALSE;
	    }
	    
	    /* For 64-bit platforms we downcast 8-byte long
	     * to 4-byte SQLINTEGER, checking for overflow */
	    
	    if(l64>INT_MAX || l64<INT_MIN) {
		    return FALSE;
	    }

	param->values.integer[j]=(SQLINTEGER)l64;
	break;
	
    case SQL_C_DOUBLE: 
	    if((erl_type != ERL_FLOAT_EXT)) { 
		    return FALSE;
	    } 
	    ei_decode_double(buffer, index, &(param->values.floating[j])); 
	    break;
	    
    case SQL_C_BIT:
	if((erl_type != ERL_ATOM_EXT)) {
		return FALSE;
	}
	if (strncmp((char*)atomarray,"true",4) == 0)
	    param->values.bool[j] = TRUE;
	else if (strncmp((char*)atomarray,"false",5) == 0)
	    param->values.bool[j] = FALSE;
	else
	    return -1;
	break;
    default:
	    return FALSE;
    }
    
    return TRUE;
}  

/*------------- Erlang port communication functions ----------------------*/

/* read from stdin */ 
#ifdef WIN32
static int read_exact(byte *buffer, int len)
{
    HANDLE standard_input = GetStdHandle(STD_INPUT_HANDLE);
    
    unsigned read_result;
    unsigned sofar = 0;
    
    if (!len) { /* Happens for "empty packages */
	return 0;
    }
    for (;;) {
	if (!ReadFile(standard_input, buffer + sofar,
		      len - sofar, &read_result, NULL)) {
	    return -1; /* EOF */
	}
	if (!read_result) {
	    return -2; /* Interrupted while reading? */
	}
	sofar += read_result;
	if (sofar == len) {
	    return len;
	}
    }
} 
#elif defined(UNIX)
static int read_exact(byte *buffer, int len) {
    int i, got = 0;
    
    do {
	if ((i = read(0, buffer + got, len - got)) <= 0)
	    return(i);
	got += i;
    } while (got < len);
    return len;
   
}
#endif


static size_t length_buffer_to_size(byte length_buffer[LENGTH_INDICATOR_SIZE])
{
  size_t size = 0, i;

  for (i = 0; i < LENGTH_INDICATOR_SIZE; ++i) {
    size <<= 8;
    size |= (unsigned char)length_buffer[i];
  }

  return size;
}


/* Recieive (read) data from erlang on stdin */
static byte * receive_erlang_port_msg(void)
{
    size_t len;
    byte *buffer;
    byte lengthstr[LENGTH_INDICATOR_SIZE];

    if(read_exact(lengthstr, LENGTH_INDICATOR_SIZE) !=
       LENGTH_INDICATOR_SIZE)
    {
	DO_EXIT(EXIT_STDIN_HEADER);
    }

    len = length_buffer_to_size(lengthstr);
    
    if (len <= 0 || len > 1024) {
	DO_EXIT(EXIT_STDIN_HEADER);
    }

    buffer = (byte *)safe_malloc(len);
    
    if (read_exact(buffer, len) <= 0) {
	DO_EXIT(EXIT_STDIN_BODY);
    }

    if (buffer[len-1] != '\0') {
	DO_EXIT(EXIT_STDIN_BODY);
    }

    return buffer;
}
 
/* ------------- Socket communication functions --------------------------*/

#if defined(WIN32)
static SOCKET connect_to_erlang(const char *port)
#elif defined(UNIX)
static int connect_to_erlang(const char *port)
#endif
{
#if defined(WIN32)
	SOCKET sock;
#elif defined(UNIX)
	int sock;
#endif
	struct sockaddr_in sin;

#if defined(HAVE_STRUCT_SOCKADDR_IN6_SIN6_ADDR) && defined(AF_INET6)
	struct sockaddr_in6 sin6;

	sock = socket(AF_INET6, SOCK_STREAM, 0);

	memset(&sin6, 0, sizeof(sin6));
	sin6.sin6_port = htons ((unsigned short)atoi(port));
	sin6.sin6_family = AF_INET6;
	sin6.sin6_addr = in6addr_loopback;
    
	if (connect(sock, (struct sockaddr*)&sin6, sizeof(sin6)) == 0) {
		/* Enable TCP_NODELAY to disable Nagel's socket algorithm. (Removes ~40ms delay on Redhat ES 6). */
		#ifdef UNIX
		tcp_nodelay(sock);
		#endif
		return sock;
	}
	close_socket(sock);
#endif
	sock = socket(AF_INET, SOCK_STREAM, 0);

	memset(&sin, 0, sizeof(sin));
	sin.sin_port = htons ((unsigned short)atoi(port));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	
	if (connect(sock, (struct sockaddr*)&sin, sizeof(sin)) != 0) {
		close_socket(sock);
		DO_EXIT(EXIT_SOCKET_CONNECT);
	}

	/* Enable TCP_NODELAY to disable Nagel's socket algorithm. (Removes ~40ms delay on Redhat ES 6). */
	#ifdef UNIX
	tcp_nodelay(sock);
	#endif
	return sock;
}

#ifdef UNIX
static void tcp_nodelay(int sock)
{
	int flag = 1;
        int result = setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
	if (result < 0) {
		DO_EXIT(EXIT_SOCKET_CONNECT);
	}
}
#endif
#ifdef WIN32
static void close_socket(SOCKET socket)
{
    closesocket(socket);
}
#elif defined(UNIX)
static void close_socket(int socket)
{
    close(socket);
}
#endif

#ifdef WIN32
static byte * receive_msg(SOCKET socket) 
#elif defined(UNIX)
static byte * receive_msg(int socket) 
#endif
{
    byte lengthstr[LENGTH_INDICATOR_SIZE];
    size_t msg_len;
    byte *buffer = NULL;
    
    if(!receive_msg_part(socket, lengthstr, LENGTH_INDICATOR_SIZE)) {
	close_socket(socket);
	DO_EXIT(EXIT_SOCKET_RECV_HEADER);
    }
    
    msg_len = length_buffer_to_size(lengthstr);
    
    buffer = (byte *)safe_malloc(msg_len);

    if(!receive_msg_part(socket, buffer, msg_len)) {
	close_socket(socket);
	DO_EXIT(EXIT_SOCKET_RECV_BODY);
    }

    return buffer;
}

#ifdef WIN32
static Boolean receive_msg_part(SOCKET socket, byte * buffer, size_t msg_len)
#elif defined(UNIX)  
static Boolean receive_msg_part(int socket, byte * buffer, size_t msg_len)
#endif
{
    int nr_bytes_received = 0;
    
    nr_bytes_received = recv(socket, (void *)buffer, msg_len, 0);
    
    if(nr_bytes_received == msg_len) {
	return TRUE; 
    } else if(nr_bytes_received > 0 && nr_bytes_received < msg_len) {
	return receive_msg_part(socket, buffer + nr_bytes_received,
				msg_len - nr_bytes_received); 
    } else if(nr_bytes_received == -1) { 
	return FALSE; 
    } else {  /* nr_bytes_received > msg_len */
	close_socket(socket); 
	DO_EXIT(EXIT_SOCKET_RECV_MSGSIZE); 
    }
}

#ifdef WIN32
static void send_msg(db_result_msg *msg, SOCKET socket)
#elif defined(UNIX)   
static void send_msg(db_result_msg *msg, int socket)
#endif
{
    byte lengthstr[LENGTH_INDICATOR_SIZE];
    int len;
    len = msg ->length;
    
    lengthstr[0] = (len >> 24) & 0x000000FF;
    lengthstr[1] = (len >> 16) & 0x000000FF;
    lengthstr[2] = (len >> 8) & 0x000000FF;
    lengthstr[3] = len &  0x000000FF;

    if(!send_msg_part(socket, lengthstr, LENGTH_INDICATOR_SIZE)) {
	close_socket(socket);
	DO_EXIT(EXIT_SOCKET_SEND_HEADER);
    }
    
    if(!send_msg_part(socket, msg->buffer, len)) {
	close_socket(socket);
	DO_EXIT(EXIT_SOCKET_SEND_BODY);
    }
}

#ifdef WIN32
static Boolean send_msg_part(SOCKET socket, byte * buffer, size_t msg_len)
#elif defined(UNIX)  
static Boolean send_msg_part(int socket, byte * buffer, size_t msg_len)
#endif
{
    int nr_bytes_sent = 0;
    
    nr_bytes_sent = send(socket, (void *)buffer, msg_len, 0);
    
    if(nr_bytes_sent == msg_len) {
	return TRUE; 
    } else if(nr_bytes_sent > 0 && nr_bytes_sent < msg_len) {
	return send_msg_part(socket, buffer + nr_bytes_sent,
				msg_len - nr_bytes_sent); 
    } else if(nr_bytes_sent == -1) { 
	return FALSE; 
    } else {  /* nr_bytes_sent > msg_len */
	close_socket(socket); 
	DO_EXIT(EXIT_SOCKET_SEND_MSGSIZE); 
    }
}

#ifdef WIN32
static void init_winsock(void)
{
    WORD wVersionRequested;
    WSADATA wsaData;
    int err;
    
    wVersionRequested = MAKEWORD( 2, 0 );
 
    err = WSAStartup( wVersionRequested, &wsaData );
    if ( err != 0 ) {
	DO_EXIT(EXIT_OLD_WINSOCK);
    }

    if ( LOBYTE( wsaData.wVersion ) != 2 ||
	 HIBYTE( wsaData.wVersion ) != 0 ) {
	clean_socket_lib();
	DO_EXIT(EXIT_OLD_WINSOCK);
    }
}
#endif

static void clean_socket_lib(void)
{
#ifdef WIN32
    WSACleanup();
#endif
}
    

/*------------- Memmory handling funtions -------------------------------*/
static void *safe_malloc(int size)
{
    void *memory;
  
    memory = (void *)malloc(size);
    if (memory == NULL) 
	DO_EXIT(EXIT_ALLOC);

    return memory;
}

static void *safe_realloc(void *ptr, int size)
{
    void *memory;

    memory = (void *)realloc(ptr, size);

    if (memory == NULL)
    {
	free(ptr);
	DO_EXIT(EXIT_ALLOC);
    }
    return memory;
}
  
/* Description: Allocate memory for n columns */
static db_column * alloc_column_buffer(int n)
{
    int i;
    db_column *columns;
  
    columns = (db_column *)safe_malloc(n * sizeof(db_column));
    for(i = 0; i < n; i++)
	columns[i].buffer = NULL;
  
    return columns;
}
 
/* Description: Deallocate memory allocated by alloc_column_buffer */
static void free_column_buffer(db_column **columns, int n)
{
    int i;
    if(*columns != NULL) {
	for (i = 0; i < n; i++) {
	    if((*columns)[i].buffer != NULL) {
		free((*columns)[i].buffer);
	    }
	}
	free(*columns);
	*columns = NULL;
    }
}

static void free_params(param_array **params, int cols)
{
    int i;
    if(*params != NULL) {
	for (i = 0; i < cols; i++) {
	    if((*params)[i].type.strlen_or_indptr_array != NULL){
		free((*params)[i].type.strlen_or_indptr_array);
	    }    
	    free(retrive_param_values(&((*params)[i])));
	} 
	free(*params);
	*params = NULL;
    }
}   

/* Description: Frees resources associated with the current statement handle
   keeped in the state.*/
static void clean_state(db_state *state)
{
    if(statement_handle(state) != NULL) {
	if(!sql_success(SQLFreeHandle(SQL_HANDLE_STMT,
				      statement_handle(state)))) {
	    DO_EXIT(EXIT_FREE);
	}
	statement_handle(state) = NULL;
    }
    free_column_buffer(&(columns(state)), nr_of_columns(state));
    columns(state) = NULL;
    nr_of_columns(state) = 0;
}
 
/* Allocates and fill with default value StrLen_or_IndPtr array */
static SQLLEN* alloc_strlen_indptr(int n, int val)
{
    int i;
    SQLLEN* arr = (SQLLEN*)safe_malloc(n * sizeof(SQLLEN));

    for( i=0; i < n; ++i )
	arr[i] = val;

    return arr;
}

/* ------------- Init/map/bind/retrive functions  ------------------------*/

/* Prepare the state for a connection */
static void init_driver(int erl_auto_commit_mode, int erl_trace_driver,
			db_state *state)
{
  
    SQLLEN auto_commit_mode, trace_driver;
  
    if(erl_auto_commit_mode == ON) {
	auto_commit_mode = SQL_AUTOCOMMIT_ON;
    } else {
	auto_commit_mode = SQL_AUTOCOMMIT_OFF;
    }

    if(erl_trace_driver == ON) {
	trace_driver = SQL_OPT_TRACE_ON;
    } else {
	trace_driver = SQL_OPT_TRACE_OFF;
    }
  
    if(!sql_success(SQLAllocHandle(SQL_HANDLE_ENV,
				   SQL_NULL_HANDLE,
				   &environment_handle(state))))
	DO_EXIT(EXIT_ALLOC);
    if(!sql_success(SQLSetEnvAttr(environment_handle(state),
				  SQL_ATTR_ODBC_VERSION,
				  (SQLPOINTER)SQL_OV_ODBC3, 0)))
	DO_EXIT(EXIT_ENV);
    if(!sql_success(SQLAllocHandle(SQL_HANDLE_DBC,
				   environment_handle(state),
				   &connection_handle(state))))
	DO_EXIT(EXIT_ALLOC);
    /* By default Erlang handles all timeouts */
    if(!sql_success(SQLSetConnectAttr(connection_handle(state),
				      SQL_ATTR_CONNECTION_TIMEOUT,
				      (SQLPOINTER)TIME_OUT, 0)))
	    DO_EXIT(EXIT_CONNECTION);
    if(!sql_success(SQLSetConnectAttr(connection_handle(state),
				      SQL_ATTR_AUTOCOMMIT,
				      (SQLPOINTER)auto_commit_mode, 0)))
	    DO_EXIT(EXIT_CONNECTION);
    if(!sql_success(SQLSetConnectAttr(connection_handle(state),
				      SQL_ATTR_TRACE,
				      (SQLPOINTER)trace_driver, 0)))
	    DO_EXIT(EXIT_CONNECTION);
}

static void init_param_column(param_array *params, byte *buffer, int *index,
			      int num_param_values, db_state* state)
{
    long user_type, precision, scale, length;
    long in_or_out;
    
    ei_decode_long(buffer, index, &user_type);

    params->type.strlen_or_indptr = (SQLLEN)NULL;
    params->type.strlen_or_indptr_array = NULL;
    params->type.decimal_digits = (SQLINTEGER)0;
  
    switch (user_type) {
    case USER_SMALL_INT:
	params->type.sql = SQL_SMALLINT;
	params->type.c = SQL_C_SLONG;
	params->type.len = sizeof(SQLINTEGER);
	params->type.col_size = COL_SQL_SMALLINT;
	params->values.integer =
	    (SQLINTEGER*)safe_malloc(num_param_values * params->type.len);
	break;
    case USER_INT:
	params->type.sql = SQL_INTEGER;
	params->type.c = SQL_C_SLONG;
	params->type.len = sizeof(SQLINTEGER);
	params->type.col_size = COL_SQL_INTEGER;
	params->values.integer =
	    (SQLINTEGER*)safe_malloc(num_param_values * params->type.len);
	break;
    case USER_TINY_INT:
	params->type.sql = SQL_TINYINT;
	params->type.c = SQL_C_SLONG;
	params->type.len = sizeof(SQLINTEGER);
	params->type.col_size = COL_SQL_TINYINT;
	params->values.integer =
	    (SQLINTEGER*)safe_malloc(num_param_values * params->type.len);
	break;
    case USER_DECIMAL:
    case USER_NMERIC:
	if(user_type == USER_NMERIC) {
	   params->type.sql = SQL_NUMERIC;
	} else {
	    params->type.sql = SQL_DECIMAL;
	}
	ei_decode_long(buffer, index, &precision);
	ei_decode_long(buffer, index, &scale);
	map_dec_num_2_c_column(&params->type, (int)precision, (int)scale);
	if( params->type.c == SQL_C_SLONG) {
	    params->values.integer =
		(SQLINTEGER *)safe_malloc(num_param_values * params->type.len);
	} else if( params->type.c == SQL_C_DOUBLE) {
	    params->values.floating =
		(double *)safe_malloc(num_param_values * params->type.len);
	} else if(params->type.c == SQL_C_CHAR) {
	    params->type.strlen_or_indptr_array
		= alloc_strlen_indptr(num_param_values, SQL_NTS);
	    params->values.string =
		(byte *)safe_malloc(num_param_values *
				    sizeof(byte)* params->type.len);
	}    
	break;
    case USER_CHAR:
    case USER_VARCHAR:
	if(user_type == USER_CHAR) {
	     params->type.sql = SQL_CHAR;
	} else {
	     params->type.sql = SQL_VARCHAR;
	}
	ei_decode_long(buffer, index, &length);
	/* Max string length + string terminator */
	 params->type.len = length+1;
	 params->type.c = SQL_C_CHAR;
	 params->type.col_size = (SQLUINTEGER)length;
	 params->type.strlen_or_indptr_array
	     = alloc_strlen_indptr(num_param_values, SQL_NTS);
	 params->values.string =
	    (byte *)safe_malloc(num_param_values *
				sizeof(byte)* params->type.len);
	
	break;
    case USER_WCHAR:
    case USER_WVARCHAR:
    case USER_WLONGVARCHAR:
	switch (user_type) {
	    case USER_WCHAR:
		params->type.sql = SQL_WCHAR; break;
	    case USER_WVARCHAR:
		params->type.sql = SQL_WVARCHAR; break;
	    default:
		params->type.sql = SQL_WLONGVARCHAR; break;
	}
	ei_decode_long(buffer, index, &length);
	/* Max string length + string terminator */
	params->type.len = (length+1)*sizeof(SQLWCHAR);
        params->type.c = SQL_C_WCHAR;
        params->type.col_size = (SQLUINTEGER)length;
	params->type.strlen_or_indptr_array
	    = alloc_strlen_indptr(num_param_values, SQL_NTS);
        params->values.string =
          (byte *)safe_malloc(num_param_values * sizeof(byte) * params->type.len);
	
	break;
    case USER_TIMESTAMP:
      params->type.sql = SQL_TYPE_TIMESTAMP;
      params->type.len = sizeof(TIMESTAMP_STRUCT);
      params->type.c = SQL_C_TYPE_TIMESTAMP;
      params->type.col_size = (SQLUINTEGER)COL_SQL_TIMESTAMP;
      params->values.string =
        (byte *)safe_malloc(num_param_values * params->type.len);
      break;
    case USER_FLOAT:
	params->type.sql = SQL_FLOAT;
	params->type.c = SQL_C_DOUBLE;
	params->type.len = sizeof(double);
	ei_decode_long(buffer, index, &length);
	params->type.col_size = (SQLUINTEGER)length;
	params->values.floating =
	    (double *)safe_malloc(num_param_values * params->type.len);
	break;
    case USER_REAL:
	params->type.sql = SQL_REAL;
	params->type.c = SQL_C_DOUBLE;
	params->type.len = sizeof(double);
	params->type.col_size = COL_SQL_REAL;
	params->values.floating =
	    (double *)safe_malloc(num_param_values * params->type.len);
	break;
    case USER_DOUBLE:
	params->type.sql = SQL_DOUBLE;
	params->type.c = SQL_C_DOUBLE;
	params->type.len = sizeof(double);
	params->type.col_size = COL_SQL_DOUBLE;
	params->values.floating =
	    (double *)safe_malloc(num_param_values * params->type.len);
	break;
    case USER_BOOLEAN:
	params->type.sql = SQL_BIT;
	params->type.c = SQL_C_BIT;
	params->type.len = sizeof(byte);
	params->type.col_size = params->type.len;
	params->values.bool =
		(byte *)safe_malloc(num_param_values * params->type.len);
	break;
    }
    params->offset = 0;

    ei_decode_long(buffer, index, &in_or_out);
    switch((in_or_out_type)in_or_out){
    case(ERL_ODBC_OUT):
        out_params(state) = TRUE;
        params->input_output_type = SQL_PARAM_OUTPUT; break;
    case(ERL_ODBC_INOUT):
        out_params(state) = TRUE;
        params->input_output_type = SQL_PARAM_INPUT_OUTPUT; break;
    case(ERL_ODBC_IN):
    default:
        params->input_output_type = SQL_PARAM_INPUT; break;
    }

}

static void init_param_statement(int cols, SQLLEN num_param_values,
				 db_state *state, param_status *status)
{
    int i;

    status -> param_status_array =
	(SQLUSMALLINT *)safe_malloc(num_param_values * sizeof(SQLUSMALLINT));

    for(i=0; i<num_param_values; i++) {
    	status -> param_status_array[i] = SQL_PARAM_PROCEED;
    }

    status -> params_processed = 0;
    
    if(!sql_success(SQLAllocHandle(SQL_HANDLE_STMT,
				   connection_handle(state),
				   &statement_handle(state)))) {
	DO_EXIT(EXIT_ALLOC);
    }
    
    if(num_param_values <= 1) return;

    if(!sql_success(SQLSetStmtAttr(statement_handle(state),
				   SQL_ATTR_PARAM_BIND_TYPE,
				   SQL_PARAM_BIND_BY_COLUMN, 0))) {
	DO_EXIT(EXIT_PARAM_ARRAY);
    }

    /* Note the (SQLLEN *) cast is correct as the API function SQLSetStmtAttr
       takes either an interger or a pointer depending on the attribute */
    if(!sql_success(SQLSetStmtAttr(statement_handle(state),
				   SQL_ATTR_PARAMSET_SIZE,
				   (SQLLEN *)num_param_values,
				   0))) {
	DO_EXIT(EXIT_PARAM_ARRAY);
    }
    
    if(!sql_success(SQLSetStmtAttr(statement_handle(state),
				   SQL_ATTR_PARAM_STATUS_PTR,
				   (status -> param_status_array), 0))) {
	DO_EXIT(EXIT_PARAM_ARRAY);
    }
    
    if(!sql_success(SQLSetStmtAttr(statement_handle(state),
				   SQL_ATTR_PARAMS_PROCESSED_PTR,
				   &(status -> params_processed), 0))) {
	DO_EXIT(EXIT_PARAM_ARRAY);
    }
}

static void map_dec_num_2_c_column(col_type *type, int precision, int scale)
{
    type -> col_size = (SQLINTEGER)precision;
    type -> decimal_digits = (SQLSMALLINT)scale;

    if(precision >= 0 && precision <= 4 && scale == 0) {
	type->len = sizeof(SQLINTEGER);
	type->c = SQL_C_SLONG;
    } else if(precision >= 5 && precision <= 9 && scale == 0) {
	type->len = sizeof(SQLINTEGER);
	type->c = SQL_C_SLONG;
    }  else if((precision >= 10 && precision <= 15 && scale == 0)
	       || (precision <= 15 && scale > 0)) {
	type->len = sizeof(double);
	type->c = SQL_C_DOUBLE;
    } else {
	type->len = DEC_NUM_LENGTH;
	type->c = SQL_C_CHAR;
    }
}

/* Description: Transform SQL columntype to C columntype. Returns a dummy
 db_result_msg with length 0 on success and an errormessage otherwise.*/
static db_result_msg map_sql_2_c_column(db_column* column, db_state *state)
{
    db_result_msg msg;

    msg = encode_empty_message();
        
    switch(column -> type.sql) {
    case SQL_CHAR:
    case SQL_VARCHAR:
    case SQL_BINARY:
    case SQL_LONGVARCHAR:
    case SQL_VARBINARY:
    case SQL_LONGVARBINARY:
        column -> type.len = (column -> type.col_size) +
            /* Make place for NULL termination */
            sizeof(byte);
        column -> type.c = SQL_C_CHAR;
        column -> type.strlen_or_indptr = SQL_NTS;
        break;
    case SQL_WCHAR:
    case SQL_WVARCHAR:
    case SQL_WLONGVARCHAR:
        column -> type.len = (column -> type.col_size + 1)*sizeof(SQLWCHAR); 
        column -> type.c = SQL_C_WCHAR;
        column -> type.strlen_or_indptr = SQL_NTS;
	break;
    case SQL_NUMERIC:
    case SQL_DECIMAL:
	map_dec_num_2_c_column(&(column -> type), column -> type.col_size,
			       column -> type.decimal_digits);
	column -> type.strlen_or_indptr = (SQLLEN)NULL;
	break;
    case SQL_TINYINT:
    case SQL_INTEGER:
    case SQL_SMALLINT:
	column -> type.len = sizeof(SQLINTEGER);
	column -> type.c = SQL_C_SLONG;
	column -> type.strlen_or_indptr = (SQLLEN)NULL;
	break;
    case SQL_REAL:
    case SQL_FLOAT:
    case SQL_DOUBLE:
	column -> type.len = sizeof(double);
	column -> type.c = SQL_C_DOUBLE;
	column -> type.strlen_or_indptr = (SQLLEN)NULL;
	break;
    case SQL_TYPE_DATE:
    case SQL_TYPE_TIME:
	column -> type.len = (column -> type.col_size) +
	    sizeof(byte);
	column -> type.c = SQL_C_CHAR;
	column -> type.strlen_or_indptr = SQL_NTS;
	break;
    case SQL_TYPE_TIMESTAMP:
      column -> type.len = sizeof(TIMESTAMP_STRUCT);
      column -> type.c = SQL_C_TYPE_TIMESTAMP;
      column -> type.strlen_or_indptr = (SQLLEN)NULL;
      break;
    case SQL_BIGINT:
	column -> type.len = DEC_NUM_LENGTH;
	column -> type.c = SQL_C_CHAR;
	column -> type.strlen_or_indptr = (SQLLEN)NULL;
	break;
    case SQL_BIT:
	column -> type.len = sizeof(byte);
	column -> type.c = SQL_C_BIT;
	column -> type.strlen_or_indptr = (SQLLEN)NULL;
	break;
    case SQL_UNKNOWN_TYPE:
	msg = encode_error_message("Unknown column type", extended_error(state, ""), 0);
	break;
    default:
	msg = encode_error_message("Column type not supported", extended_error(state, ""), 0);
	break;
    }
    return msg;
}

static param_array * bind_parameter_arrays(byte *buffer, int *index,
					   int cols, int num_param_values,
					   db_state *state)
{
    int i, j, size, erl_type;
    long dummy;
    void *Values;
    param_array *params;
    
    params = (param_array *)safe_malloc(cols * sizeof(param_array)); 
    
    for (i = 0; i < cols; i++) {
    
	ei_get_type(buffer, index, &erl_type, &size);

	if(erl_type == ERL_NIL_EXT) {
	    /* End of previous list of column values when i > 0 */
	    ei_decode_list_header(buffer, index, &size);
	}

	ei_decode_tuple_header(buffer, index, &size);

	init_param_column(&params[i], buffer, index, num_param_values, state);

	ei_decode_list_header(buffer, index, &size);

	if(params[i].type.c == SQL_C_SLONG) {
         /* Get rid of the dummy value 256 that is added as the first value
	    of all integer parameter value lists. This is to avoid that the
	    list will be encoded as a string if all values are less
	    than 256 */
	    ei_decode_long(buffer, index, &dummy); 
	}
  
	for (j = 0; j < num_param_values; j++) {
	    if(!decode_params(state, buffer, index, &params, i, j, num_param_values)) {
		/* An input parameter was not of the expected type */  
		free_params(&params, i);
		return params;
	    }
	}

	Values = retrive_param_values(&params[i]); 

	if(!sql_success(
	    SQLBindParameter(statement_handle(state), i + 1,
			     params[i].input_output_type,
			     params[i].type.c,
			     params[i].type.sql,
			     params[i].type.col_size,
			     params[i].type.decimal_digits, Values,
			     params[i].type.len,
			     params[i].type.strlen_or_indptr_array))) {
	    DO_EXIT(EXIT_BIND);
	}
    }

    return params;
}
 
static void * retrive_param_values(param_array *Param)
{
    switch(Param->type.c) {
    case SQL_C_CHAR:
    case SQL_C_WCHAR:
    case SQL_C_TYPE_TIMESTAMP:
        return (void *)Param->values.string;
    case SQL_C_SLONG:
	return (void *)Param->values.integer;
    case SQL_C_DOUBLE: 
	return (void *)Param->values.floating;
    case SQL_C_BIT:
	return (void *)Param->values.bool;
    default:
	DO_EXIT(EXIT_FAILURE); /* Should not happen */
    }
}

/* Description: More than one call to SQLGetData may be required to retrieve
   data from a single column with  binary data. SQLGetData then returns
   SQL_SUCCESS_WITH_INFO nd the SQLSTATE will have the value 01004 (Data
   truncated). The application can then use the same column number to
   retrieve subsequent parts of the data until SQLGetData returns
   SQL_SUCCESS, indicating that all data for the column has been retrieved.
*/

static db_column retrive_binary_data(db_column column, int column_nr,
				     db_state *state)
{ 
    char *outputptr;
    int blocklen, outputlen, result;
    diagnos diagnos;
  
    blocklen = column.type.len;
    outputptr = column.buffer;
    result = SQLGetData(statement_handle(state), (SQLSMALLINT)(column_nr+1),
			SQL_C_CHAR, outputptr,
			blocklen, &column.type.strlen_or_indptr);

    while (result == SQL_SUCCESS_WITH_INFO) {

	diagnos = get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
    
	if(strcmp((char *)diagnos.sqlState, TRUNCATED) == 0) {
	    outputlen = column.type.len - 1;
	    column.type.len = outputlen + blocklen;
	    column.buffer =
		safe_realloc((void *)column.buffer, column.type.len);
	    outputptr = column.buffer + outputlen;
	    result = SQLGetData(statement_handle(state),
				(SQLSMALLINT)(column_nr+1), SQL_C_CHAR,
				outputptr, blocklen,
				&column.type.strlen_or_indptr);
	}
    }
  
    if (result == SQL_SUCCESS) {
	return column;
    } else {
	DO_EXIT(EXIT_BIN); 
    }
}

/* Description: Returns information about support for scrollable cursors */ 
static db_result_msg retrive_scrollable_cursor_support_info(db_state *state)
{
    db_result_msg msg;
    SQLUINTEGER supportMask;
  
    ei_x_new_with_version(&dynamic_buffer(state));
    ei_x_encode_tuple_header(&dynamic_buffer(state), 3);
    ei_x_encode_atom(&dynamic_buffer(state), "ok");

    if(use_srollable_cursors(state)) {
    
	if(!sql_success(SQLGetInfo(connection_handle(state),
				   SQL_DYNAMIC_CURSOR_ATTRIBUTES1,
				   (SQLPOINTER)&supportMask,
				   sizeof(supportMask),
				   NULL))) {
	    DO_EXIT(EXIT_DRIVER_INFO);
	}
    
	if ((supportMask & SQL_CA1_ABSOLUTE)) {
	    ei_x_encode_atom(&dynamic_buffer(state), "true");
	}
	else {
	    ei_x_encode_atom(&dynamic_buffer(state), "false");    
	}
    
	if ((supportMask & SQL_CA1_RELATIVE)) {
	    ei_x_encode_atom(&dynamic_buffer(state), "true");    
	}
	else {
	    ei_x_encode_atom(&dynamic_buffer(state), "false");
	}
    } else { /* Scrollable cursors disabled by the user */
	ei_x_encode_atom(&dynamic_buffer(state), "false");  
	ei_x_encode_atom(&dynamic_buffer(state), "false");
    } 
    msg.buffer = dynamic_buffer(state).buff;
    msg.length = dynamic_buffer(state).index; 
    msg.dyn_alloc = TRUE;
    return msg;
}
 
/* ------------- Boolean functions ---------------------------------------*/

/* Check if there are any more result sets */
static db_result_msg more_result_sets(db_state *state)
{
    SQLRETURN result;
    diagnos diagnos;
    db_result_msg msg;

    msg = encode_empty_message();
    result = SQLMoreResults(statement_handle(state));
  
    if(sql_success(result)){
	exists_more_result_sets(state) = TRUE;
	return msg;
    } else if(result == SQL_NO_DATA) {
	exists_more_result_sets(state) = FALSE;
	return msg;
    } else {
	/* As we found an error we do not care about any potential more result
	   sets */
	exists_more_result_sets(state) = FALSE;
	diagnos = get_diagnos(SQL_HANDLE_STMT, statement_handle(state), extended_errors(state));
	strcat((char *)diagnos.error_msg,
	       "Failed to create on of the result sets");
	msg = encode_error_message(diagnos.error_msg, extended_error(state, diagnos.sqlState), diagnos.nativeError);
	return msg;
    }
}
 
static Boolean sql_success(SQLRETURN result)
{
    return result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO;
}

/* ------------- Error handling functions --------------------------------*/

/* Description: An ODBC function can post zero or more diagnostic records
   each time it is called. This function loops through the current set of
   diagnostic records scaning for error messages and the sqlstate.
   If this function is called when no error has ocurred only the sqlState
   field may be referenced.*/
static diagnos get_diagnos(SQLSMALLINT handleType, SQLHANDLE handle, Boolean extendedErrors)
{
    diagnos diagnos;
    SQLINTEGER nativeError;
    SQLSMALLINT errmsg_buffer_size, record_nr, errmsg_size;
    int acc_errmsg_size;
    byte *current_errmsg_pos;
    SQLCHAR current_sql_state[SQL_STATE_SIZE];
    SQLRETURN result;

    diagnos.error_msg[0] = 0;
    
    current_errmsg_pos = (byte *)diagnos.error_msg;

    /* number bytes free in error message buffer */
    errmsg_buffer_size = MAX_ERR_MSG - ERRMSG_HEADR_SIZE;  
    acc_errmsg_size = 0; /* number bytes used in the error message buffer */

    /* Foreach diagnostic record in the current set of diagnostic records
       the error message is obtained */
    for(record_nr = 1; ;record_nr++) {    
        result = SQLGetDiagRec(handleType, handle, record_nr, current_sql_state,
			 &nativeError, (SQLCHAR *)current_errmsg_pos,
			 (SQLSMALLINT)errmsg_buffer_size, &errmsg_size);
	if(result == SQL_SUCCESS) {
	    /* update the sqlstate in the diagnos record, because the SQLGetDiagRec
	       call succeeded */
	    memcpy(diagnos.sqlState, current_sql_state, SQL_STATE_SIZE);
	    diagnos.nativeError = nativeError;
	    errmsg_buffer_size = errmsg_buffer_size - errmsg_size;
	    acc_errmsg_size = acc_errmsg_size + errmsg_size;
	    current_errmsg_pos = current_errmsg_pos + errmsg_size;
	} else if(result == SQL_SUCCESS_WITH_INFO && errmsg_size >= errmsg_buffer_size) {
	    memcpy(diagnos.sqlState, current_sql_state, SQL_STATE_SIZE);
	    diagnos.nativeError = nativeError;
	    acc_errmsg_size = errmsg_buffer_size;
	    break;
	} else {
	    break;
	}
    }
    
    if(acc_errmsg_size == 0) {
	strcat((char *)diagnos.error_msg,
	       "No SQL-driver information available.");
    }
    else if (!extendedErrors){
	strcat(strcat((char *)diagnos.error_msg, " SQLSTATE IS: "),
	       (char *)diagnos.sqlState);
    }
    return diagnos;
}

static void str_tolower(char *str, int len)
{
	int i;
	
	for(i = 0; i <= len; i++) {
		str[i] = tolower(str[i]);
	}
}
