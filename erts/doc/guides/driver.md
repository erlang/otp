<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# How to Implement a Driver

> #### Note {: .info }
>
> This section was written a long time ago. Most of it is still valid, as it
> explains important concepts, but this was written for an older driver
> interface so the examples do not work anymore. The reader is encouraged to
> read the [`erl_driver`](erl_driver.md) and [`driver_entry`](driver_entry.md)
> documentation also.

## Introduction

This section describes how to build your own driver for Erlang.

A driver in Erlang is a library written in C, which is linked to the Erlang
emulator and called from Erlang. Drivers can be used when C is more suitable
than Erlang, to speed up things, or to provide access to OS resources not
directly accessible from Erlang.

A driver can be dynamically loaded, as a shared library (known as a DLL on
Windows), or statically loaded, linked with the emulator when it is compiled and
linked. Only dynamically loaded drivers are described here, statically linked
drivers are beyond the scope of this section.

> #### Warning {: .warning }
>
> When a driver is loaded it is executed in the context of the emulator, shares
> the same memory and the same thread. This means that all operations in the
> driver must be non-blocking, and that any crash in the driver brings the whole
> emulator down. In short, be careful.

## Sample Driver

This section describes a simple driver for accessing a postgres database using
the libpq C client library. Postgres is used because it is free and open source.
For information on postgres, see [www.postgres.org](http://www.postgres.org).

The driver is synchronous, it uses the synchronous calls of the client library.
This is only for simplicity, but not good, as it halts the emulator while
waiting for the database. This is improved below with an asynchronous sample
driver.

The code is straightforward: all communication between Erlang and the driver is
done with [`port_control/3`](`port_control/3`), and the driver returns data back
using the `rbuf`.

An Erlang driver only exports one function: the driver entry function. This is
defined with a macro, `DRIVER_INIT`, which returns a pointer to a C `struct`
containing the entry points that are called from the emulator. The `struct`
defines the entries that the emulator calls to call the driver, with a `NULL`
pointer for entries that are not defined and used by the driver.

The `start` entry is called when the driver is opened as a port with
[`open_port/2`](`open_port/2`). Here we allocate memory for a user data
structure. This user data is passed every time the emulator calls us. First we
store the driver handle, as it is needed in later calls. We allocate memory for
the connection handle that is used by LibPQ. We also set the port to return
allocated driver binaries, by setting flag `PORT_CONTROL_FLAG_BINARY`, calling
`set_port_control_flags`. (This is because we do not know if our data will fit
in the result buffer of `control`, which has a default size, 64 bytes, set up by
the emulator.)

An entry `init` is called when the driver is loaded. However, we do not use
this, as it is executed only once, and we want to have the possibility of
several instances of the driver.

The `stop` entry is called when the port is closed.

The `control` entry is called from the emulator when the Erlang code calls
[`port_control/3`](`port_control/3`), to do the actual work. We have defined a
simple set of commands: `connect` to log in to the database, `disconnect` to log
out, and `select` to send a SQL-query and get the result. All results are
returned through `rbuf`. The library [`ei`] in [`erl_interface`](`e:erl_interface:index.html`)
is used to encode data in binary term format. The result is returned to the emulator as binary
terms, so `binary_to_term` is called in Erlang to convert the result to term
form.

The code is available in `pg_sync.c` in the `sample` directory of `erts`.

The driver entry contains the functions that will be called by the emulator. In
this example, only [`start`], [`stop`], and [`control`] are provided:

```c
/* Driver interface declarations */
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf,
                   int len, char **rbuf, int rlen);

static ErlDrvEntry pq_driver_entry = {
    NULL,                        /* init */
    start,
    stop,
    NULL,                        /* output */
    NULL,                        /* ready_input */
    NULL,                        /* ready_output */
    "pg_sync",                   /* the name of the driver */
    NULL,                        /* finish */
    NULL,                        /* handle */
    control,
    NULL,                        /* timeout */
    NULL,                        /* outputv */
    NULL,                        /* ready_async */
    NULL,                        /* flush */
    NULL,                        /* call */
    NULL                         /* event */
};
```

We have a structure to store state needed by the driver, in this case we only
need to keep the database connection:

```c
typedef struct our_data_s {
    PGconn* conn;
} our_data_t;
```

The control codes that we have defined are as follows:

```c
/* Keep the following definitions in alignment with the
 * defines in erl_pq_sync.erl
 */

#define DRV_CONNECT             'C'
#define DRV_DISCONNECT          'D'
#define DRV_SELECT              'S'
```

This returns the driver structure. The macro [`DRIVER_INIT`] defines the only
exported function. All the other functions are static, and will not be exported
from the library.

```c
/* INITIALIZATION AFTER LOADING */

/*
 * This is the init function called after this driver has been loaded.
 * It must *not* be declared static. Must return the address to
 * the driver entry.
 */

DRIVER_INIT(pq_drv)
{
    return &pq_driver_entry;
}
```

Here some initialization is done, [`start`] is called from `open_port/2`. The data
will be passed to [`control`] and [`stop`].

```c
/* DRIVER INTERFACE */
static ErlDrvData start(ErlDrvPort port, char *command)
{
    our_data_t* data;

    data = (our_data_t*)driver_alloc(sizeof(our_data_t));
    data->conn = NULL;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData)data;
}
```

We call disconnect to log out from the database. (This should have been done
from Erlang, but just in case.)

```c
static int do_disconnect(our_data_t* data, ei_x_buff* x);

static void stop(ErlDrvData drv_data)
{
    our_data_t* data = (our_data_t*)drv_data;

    do_disconnect(data, NULL);
    driver_free(data);
}
```

We use the binary format only to return data to the emulator; input data is a
string parameter for `connect` and `select`. The returned data consists of
Erlang terms.

The functions `get_s` and `ei_x_to_new_binary` are utilities that are used to
make the code shorter. `get_s` duplicates the string and zero-terminates it, as
the postgres client library wants that. `ei_x_to_new_binary` takes an
`ei_x_buff` buffer, allocates a binary, and copies the data there. This binary
is returned in `*rbuf`. (Notice that this binary is freed by the emulator, not
by us.)

```c
static char* get_s(const char* buf, int len);
static int do_connect(const char *s, our_data_t* data, ei_x_buff* x);
static int do_select(const char* s, our_data_t* data, ei_x_buff* x);

/* As we are operating in binary mode, the return value from control
 * is irrelevant, as long as it is not negative.
 */
static int control(ErlDrvData drv_data, unsigned int command, char *buf,
                   int len, char **rbuf, int rlen)
{
    int r;
    ei_x_buff x;
    our_data_t* data = (our_data_t*)drv_data;
    char* s = get_s(buf, len);
    ei_x_new_with_version(&x);
    switch (command) {
        case DRV_CONNECT:    r = do_connect(s, data, &x);  break;
        case DRV_DISCONNECT: r = do_disconnect(data, &x);  break;
        case DRV_SELECT:     r = do_select(s, data, &x);   break;
        default:             r = -1;        break;
    }
    *rbuf = (char*)ei_x_to_new_binary(&x);
    ei_x_free(&x);
    driver_free(s);
    return r;
}
```

`do_connect` is where we log in to the database. If the connection was
successful, we store the connection handle in the driver data, and return
`'ok'`. Otherwise, we return the error message from postgres and store `NULL` in
the driver data.

```c
static int do_connect(const char *s, our_data_t* data, ei_x_buff* x)
{
    PGconn* conn = PQconnectdb(s);
    if (PQstatus(conn) != CONNECTION_OK) {
        encode_error(x, conn);
        PQfinish(conn);
        conn = NULL;
    } else {
        encode_ok(x);
    }
    data->conn = conn;
    return 0;
}
```

If we are connected (and if the connection handle is not `NULL`), we log out
from the database. We need to check if we should encode an `'ok'`, as we can get
here from function [`stop`], which does not return data to the emulator:

```c
static int do_disconnect(our_data_t* data, ei_x_buff* x)
{
    if (data->conn == NULL)
        return 0;
    PQfinish(data->conn);
    data->conn = NULL;
    if (x != NULL)
        encode_ok(x);
    return 0;
}
```

We execute a query and encode the result. Encoding is done in another C module,
`pg_encode.c`, which is also provided as sample code.

```c
static int do_select(const char* s, our_data_t* data, ei_x_buff* x)
{
   PGresult* res = PQexec(data->conn, s);
    encode_result(x, res, data->conn);
    PQclear(res);
    return 0;
}
```

Here we check the result from postgres. If it is data, we encode it as lists of
lists with column data. Everything from postgres is C strings, so we use
[`ei_x_encode_string`] to send the result as strings to Erlang. (The head of the
list contains the column names.)

```c
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
```

## Compiling and Linking the Sample Driver

The driver is to be compiled and linked to a shared library (DLL on Windows).
With gcc, this is done with link flags `-shared` and `-fpic`. As we use the [`ei`]
library, we should include it too. There are several versions of [`ei`], compiled
for debug or non-debug and multi-threaded or single-threaded. In the makefile
for the samples, the `obj` directory is used for the [`ei`] library, meaning that
we use the non-debug, single-threaded version.

## Calling a Driver as a Port in Erlang

Before a driver can be called from Erlang, it must be loaded and opened. Loading
is done using the `m:erl_ddll` module (the `m:erl_ddll` driver that loads dynamic
driver is actually a driver itself). If loading is successful, the port can be
opened with [`open_port/2`](`open_port/2`). The port name must match the name of
the shared library and the name in the driver entry structure.

When the port has been opened, the driver can be called. In the `pg_sync`
example, we do not have any data from the port, only the return value from the
`port_control/3`.

The following code is the Erlang part of the synchronous postgres driver,
`pg_sync.erl`:

```erlang
-module(pg_sync).

-define(DRV_CONNECT, 1).
-define(DRV_DISCONNECT, 2).
-define(DRV_SELECT, 3).

-export([connect/1, disconnect/1, select/2]).

connect(ConnectStr) ->
    case erl_ddll:load_driver(".", "pg_sync") of
        ok -> ok;
        {error, already_loaded} -> ok;
        E -> exit({error, E})
    end,
    Port = open_port({spawn, ?MODULE}, []),
    case binary_to_term(port_control(Port, ?DRV_CONNECT, ConnectStr)) of
        ok -> {ok, Port};
        Error -> Error
    end.

disconnect(Port) ->
    R = binary_to_term(port_control(Port, ?DRV_DISCONNECT, "")),
    port_close(Port),
    R.

select(Port, Query) ->
    binary_to_term(port_control(Port, ?DRV_SELECT, Query)).
```

The API is simple:

- `connect/1` loads the driver, opens it, and logs on to the database, returning
  the Erlang port if successful.
- `select/2` sends a query to the driver and returns the result.
- `disconnect/1` closes the database connection and the driver. (However, it
  does not unload it.)

The connection string is to be a connection string for postgres.

The driver is loaded with `erl_ddll:load_driver/2`. If this is successful, or if
it is already loaded, it is opened. This will call the `start` function in the
driver.

We use the [`port_control/3`](`port_control/3`) function for all calls into the
driver. The result from the driver is returned immediately and converted to
terms by calling [`binary_to_term/1`](`binary_to_term/1`). (We trust that the
terms returned from the driver are well-formed, otherwise the `binary_to_term/1`
calls could be contained in a `catch`.)

## Sample Asynchronous Driver

Sometimes database queries can take a long time to complete, in our `pg_sync`
driver, the emulator halts while the driver is doing its job. This is often not
acceptable, as no other Erlang process gets a chance to do anything. To improve
on our postgres driver, we re-implement it using the asynchronous calls in
LibPQ.

The asynchronous version of the driver is in the sample files `pg_async.c` and
`pg_asyng.erl`.

```c
/* Driver interface declarations */
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf,
                   int len, char **rbuf, int rlen);
static void ready_io(ErlDrvData drv_data, ErlDrvEvent event);

static ErlDrvEntry pq_driver_entry = {
    NULL,                     /* init */
    start,
    stop,
    NULL,                     /* output */
    ready_io,                 /* ready_input */
    ready_io,                 /* ready_output */
    "pg_async",               /* the name of the driver */
    NULL,                     /* finish */
    NULL,                     /* handle */
    control,
    NULL,                     /* timeout */
    NULL,                     /* outputv */
    NULL,                     /* ready_async */
    NULL,                     /* flush */
    NULL,                     /* call */
    NULL                      /* event */
};

typedef struct our_data_t {
    PGconn* conn;
    ErlDrvPort port;
    int socket;
    int connecting;
} our_data_t;
```

Some things have changed from `pg_sync.c`: we use the entry `ready_io` for
[`ready_input`] and [`ready_output`], which is called from the emulator only when
there is input to be read from the socket. (Actually, the socket is used in a
`select` function inside the emulator, and when the socket is signaled,
indicating there is data to read, the [`ready_input`] entry is called. More about
this below.)

Our driver data is also extended, we keep track of the socket used for
communication with postgres, and also the port, which is needed when we send
data to the port with [`driver_output`]. We have a flag `connecting` to tell
whether the driver is waiting for a connection or waiting for the result of a
query. (This is needed, as the entry `ready_io` is called both when connecting
and when there is a query result.)

```c
static int do_connect(const char *s, our_data_t* data)
{
    PGconn* conn = PQconnectStart(s);
    if (PQstatus(conn) == CONNECTION_BAD) {
        ei_x_buff x;
        ei_x_new_with_version(&x);
        encode_error(&x, conn);
        PQfinish(conn);
        conn = NULL;
        driver_output(data->port, x.buff, x.index);
        ei_x_free(&x);
    }
    PQconnectPoll(conn);
    int socket = PQsocket(conn);
    data->socket = socket;
    driver_select(data->port, (ErlDrvEvent)socket, DO_READ, 1);
    driver_select(data->port, (ErlDrvEvent)socket, DO_WRITE, 1);
    data->conn = conn;
    data->connecting = 1;
    return 0;
}
```

The `connect` function looks a bit different too. We connect using the
asynchronous `PQconnectStart` function. After the connection is started, we
retrieve the socket for the connection with `PQsocket`. This socket is used with
the [`driver_select`] function to wait for connection. When the socket is ready
for input or for output, the `ready_io` function is called.

Notice that we only return data (with [`driver_output`]) if there is an error
here, otherwise we wait for the connection to be completed, in which case our
`ready_io` function is called.

```c
static int do_select(const char* s, our_data_t* data)
{
    data->connecting = 0;
    PGconn* conn = data->conn;
    /* if there's an error return it now */
    if (PQsendQuery(conn, s) == 0) {
        ei_x_buff x;
        ei_x_new_with_version(&x);
        encode_error(&x, conn);
        driver_output(data->port, x.buff, x.index);
        ei_x_free(&x);
    }
    /* else wait for ready_output to get results */
    return 0;
}
```

The `do_select` function initiates a select, and returns if there is no
immediate error. The result is returned when `ready_io` is called.

```c
static void ready_io(ErlDrvData drv_data, ErlDrvEvent event)
{
    PGresult* res = NULL;
    our_data_t* data = (our_data_t*)drv_data;
    PGconn* conn = data->conn;
    ei_x_buff x;
    ei_x_new_with_version(&x);
    if (data->connecting) {
        ConnStatusType status;
        PQconnectPoll(conn);
        status = PQstatus(conn);
        if (status == CONNECTION_OK)
            encode_ok(&x);
        else if (status == CONNECTION_BAD)
            encode_error(&x, conn);
    } else {
        PQconsumeInput(conn);
        if (PQisBusy(conn))
            return;
        res = PQgetResult(conn);
        encode_result(&x, res, conn);
        PQclear(res);
        for (;;) {
            res = PQgetResult(conn);
            if (res == NULL)
                break;
            PQclear(res);
        }
    }
    if (x.index > 1) {
        driver_output(data->port, x.buff, x.index);
        if (data->connecting)
            driver_select(data->port, (ErlDrvEvent)data->socket, DO_WRITE, 0);
    }
    ei_x_free(&x);
}
```

The `ready_io` function is called when the socket we got from postgres is ready
for input or output. Here we first check if we are connecting to the database.
In that case, we check connection status and return OK if the connection is
successful, or error if it is not. If the connection is not yet established, we
simply return; `ready_io` is called again.

If we have a result from a connect, indicated by having data in the `x` buffer,
we no longer need to select on output ([`ready_output`]), so we remove this by
calling [`driver_select`].

If we are not connecting, we wait for results from a `PQsendQuery`, so we get
the result and return it. The encoding is done with the same functions as in the
earlier example.

Error handling is to be added here, for example, checking that the socket is
still open, but this is only a simple example.

The Erlang part of the asynchronous driver consists of the sample file
`pg_async.erl`.

```erlang
-module(pg_async).

-define(DRV_CONNECT, $C).
-define(DRV_DISCONNECT, $D).
-define(DRV_SELECT, $S).

-export([connect/1, disconnect/1, select/2]).

connect(ConnectStr) ->
    case erl_ddll:load_driver(".", "pg_async") of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end,
    Port = open_port({spawn, ?MODULE}, [binary]),
    port_control(Port, ?DRV_CONNECT, ConnectStr),
    case return_port_data(Port) of
        ok ->
            {ok, Port};
        Error ->
            Error
    end.

disconnect(Port) ->
    port_control(Port, ?DRV_DISCONNECT, ""),
    R = return_port_data(Port),
    port_close(Port),
    R.

select(Port, Query) ->
    port_control(Port, ?DRV_SELECT, Query),
    return_port_data(Port).

return_port_data(Port) ->
    receive
        {Port, {data, Data}} ->
            binary_to_term(Data)
    end.
```

The Erlang code is slightly different, as we do not return the result
synchronously from `port_control/3`, instead we get it from [`driver_output`] as
data in the message queue. The function `return_port_data` above receives data
from the port. As the data is in binary format, we use
[`binary_to_term/1`](`binary_to_term/1`) to convert it to an Erlang term. Notice
that the driver is opened in binary mode ([`open_port/2`](`open_port/2`) is
called with option `[binary]`). This means that data sent from the driver to the
emulator is sent as binaries. Without option `binary`, they would have been
lists of integers.

## An Asynchronous Driver Using driver_async

As a final example we demonstrate the use of [`driver_async`]. We also use the
driver term interface. The driver is written in C++. This enables us to use an
algorithm from STL. We use the `next_permutation` algorithm to get the next
permutation of a list of integers. For large lists (> 100,000 elements), this
takes some time, so we perform this as an asynchronous task.

The asynchronous API for drivers is complicated. First, the work must be
prepared. In the example, this is done in [`output`]. We could have used
[`control`], but we want some variation in the examples. In our driver, we
allocate a structure that contains anything that is needed for the asynchronous
task to do the work. This is done in the main emulator thread. Then the
asynchronous function is called from a driver thread, separate from the main
emulator thread. Notice that the driver functions are not re-entrant, so they
are not to be used. Finally, after the function is completed, the driver
callback [`ready_async`] is called from the main emulator thread, this is where we
return the result to Erlang. (We cannot return the result from within the
asynchronous function, as we cannot call the driver functions.)

The following code is from the sample file `next_perm.cc`. The driver entry
looks like before, but also contains the callback [`ready_async`].

```c
static ErlDrvEntry next_perm_driver_entry = {
    NULL,                        /* init */
    start,
    NULL,                        /* stop */
    output,
    NULL,                        /* ready_input */
    NULL,                        /* ready_output */
    "next_perm",                 /* the name of the driver */
    NULL,                        /* finish */
    NULL,                        /* handle */
    NULL,                        /* control */
    NULL,                        /* timeout */
    NULL,                        /* outputv */
    ready_async,
    NULL,                        /* flush */
    NULL,                        /* call */
    NULL                         /* event */
};
```

The `output` function allocates the work area of the asynchronous function. As
we use C++, we use a struct, and stuff the data in it. We must copy the original
data, it is not valid after we have returned from the `output` function, and the
`do_perm` function is called later, and from another thread. We return no data
here, instead it is sent later from the [`ready_async`] callback.

The `async_data` is passed to the `do_perm` function. We do not use a
`async_free` function (the last argument to [`driver_async`]), it is only used if
the task is cancelled programmatically.

```c
struct our_async_data {
    bool prev;
    vector<int> data;
    our_async_data(ErlDrvPort p, int command, const char* buf, int len);
};

our_async_data::our_async_data(ErlDrvPort p, int command,
                               const char* buf, int len)
    : prev(command == 2),
      data((int*)buf, (int*)buf + len / sizeof(int))
{
}

static void do_perm(void* async_data);

static void output(ErlDrvData drv_data, char *buf, int len)
{
    if (*buf < 1 || *buf > 2) return;
    ErlDrvPort port = reinterpret_cast<ErlDrvPort>(drv_data);
    void* async_data = new our_async_data(port, *buf, buf+1, len);
    driver_async(port, NULL, do_perm, async_data, do_free);
}
```

In the `do_perm` we do the work, operating on the structure that was allocated
in `output`.

```c
static void do_perm(void* async_data)
{
    our_async_data* d = reinterpret_cast<our_async_data*>(async_data);
    if (d->prev)
        prev_permutation(d->data.begin(), d->data.end());
    else
        next_permutation(d->data.begin(), d->data.end());
}
```

In the `ready_async` function the output is sent back to the emulator. We use
the driver term format instead of [`ei`]. This is the only way to send Erlang
terms directly to a driver, without having the Erlang code to call
[`binary_to_term/1`](`binary_to_term/1`). In the simple example this works well,
and we do not need to use [`ei`] to handle the binary term format.

When the data is returned, we deallocate our data.

```c
static void ready_async(ErlDrvData drv_data, ErlDrvThreadData async_data)
{
    ErlDrvPort port = reinterpret_cast<ErlDrvPort>(drv_data);
    our_async_data* d = reinterpret_cast<our_async_data*>(async_data);
    int n = d->data.size(), result_n = n*2 + 3;
    ErlDrvTermData *result = new ErlDrvTermData[result_n], *rp = result;
    for (vector<int>::iterator i = d->data.begin();
         i != d->data.end(); ++i) {
        *rp++ = ERL_DRV_INT;
        *rp++ = *i;
    }
    *rp++ = ERL_DRV_NIL;
    *rp++ = ERL_DRV_LIST;
    *rp++ = n+1;
    driver_output_term(port, result, result_n);
    delete[] result;
    delete d;
}
```

This driver is called like the others from Erlang. However, as we use
[`driver_output_term`], there is no need to call `binary_to_term/1`. The Erlang code
is in the sample file `next_perm.erl`.

The input is changed into a list of integers and sent to the driver.

```erlang
-module(next_perm).

-export([next_perm/1, prev_perm/1, load/0, all_perm/1]).

load() ->
    case whereis(next_perm) of
        undefined ->
            case erl_ddll:load_driver(".", "next_perm") of
                ok -> ok;
                {error, already_loaded} -> ok;
                E -> exit(E)
            end,
            Port = open_port({spawn, "next_perm"}, []),
            register(next_perm, Port);
        _ ->
            ok
    end.

list_to_integer_binaries(L) ->
    [<<I:32/integer-native>> || I <- L].

next_perm(L) ->
    next_perm(L, 1).

prev_perm(L) ->
    next_perm(L, 2).

next_perm(L, Nxt) ->
    load(),
    B = list_to_integer_binaries(L),
    port_control(next_perm, Nxt, B),
    receive
        Result ->
            Result
    end.

all_perm(L) ->
    New = prev_perm(L),
    all_perm(New, L, [New]).

all_perm(L, L, Acc) ->
    Acc;
all_perm(L, Orig, Acc) ->
    New = prev_perm(L),
    all_perm(New, Orig, [New | Acc]).
```

[`ei`]: `e:erl_interface:ei.md`
[`start`]: `e:erts:driver_entry.md#start`
[`stop`]: `e:erts:driver_entry.md#stop`
[`control`]: `e:erts:driver_entry.md#control`
[`output`]: `e:erts:driver_entry.md#output`
[`DRIVER_INIT`]: `e:erts:driver_entry.md#DRIVER_INIT`
[`ready_input`]: `e:erts:driver_entry.md#ready_input`
[`ready_output`]: `e:erts:driver_entry.md#ready_output`
[`ready_async`]: `e:erts:driver_entry.md#ready_async`
[`driver_output`]: `e:erts:erl_driver.md#driver_output`
[`driver_output_term`]: `e:erts:erl_driver.md#driver_output_term`
[`driver_select`]: `e:erts:erl_driver.md#driver_select`
[`driver_async`]: `e:erts:erl_driver.md#driver_async`
[`ei_x_encode_string`]: `e:erl_interface:ei.md#ei_x_encode_string`
