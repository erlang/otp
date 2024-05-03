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
# The Erlang I/O Protocol

The I/O protocol in Erlang enables bi-directional communication between clients
and servers.

- The I/O server is a process that handles the requests and performs the
  requested task on, for example, an I/O device.
- The client is any Erlang process wishing to read or write data from/to the I/O
  device.

The common I/O protocol has been present in OTP since the beginning, but has
been undocumented and has also evolved over the years. In an addendum to Robert
Virding's rationale, the original I/O protocol is described. This section
describes the current I/O protocol.

The original I/O protocol was simple and flexible. Demands for memory efficiency
and execution time efficiency have triggered extensions to the protocol over the
years, making the protocol larger and somewhat less easy to implement than the
original. It can certainly be argued that the current protocol is too complex,
but this section describes how it looks today, not how it should have looked.

The basic ideas from the original protocol still hold. The I/O server and client
communicate with one single, rather simplistic protocol and no server state is
ever present in the client. Any I/O server can be used together with any client
code, and the client code does not need to be aware of the I/O device that the
I/O server communicates with.

## Protocol Basics

As described in Robert's paper, I/O servers and clients communicate using
`io_request`/`io_reply` tuples as follows:

```erlang
{io_request, From, ReplyAs, Request}
{io_reply, ReplyAs, Reply}
```

The client sends an `io_request` tuple to the I/O server and the server
eventually sends a corresponding `io_reply` tuple.

- `From` is the `t:pid/0` of the client, the process which the I/O server sends
  the I/O reply to.
- `ReplyAs` can be any datum and is returned in the corresponding `io_reply`.
  The `m:io` module monitors the I/O server and uses the monitor reference as
  the `ReplyAs` datum. A more complicated client can have many outstanding I/O
  requests to the same I/O server and can use different references (or something
  else) to differentiate among the incoming I/O replies. Element `ReplyAs` is to
  be considered opaque by the I/O server.

  Notice that the `t:pid/0` of the I/O server is not explicitly present in tuple
  `io_reply`. The reply can be sent from any process, not necessarily the actual
  I/O server.

- `Request` and `Reply` are described below.

When an I/O server receives an `io_request` tuple, it acts upon the `Request`
part and eventually sends an `io_reply` tuple with the corresponding `Reply`
part.

## Output Requests

To output characters on an I/O device, the following `Request`s exist:

```erlang
{put_chars, Encoding, Characters}
{put_chars, Encoding, Module, Function, Args}
```

- `Encoding` is `unicode` or `latin1`, meaning that the characters are (in case
  of binaries) encoded as UTF-8 or ISO Latin-1 (pure bytes). A well-behaved I/O
  server is also to return an error indication if list elements contain
  integers > 255 when `Encoding` is set to `latin1`.

  Notice that this does not in any way tell how characters are to be put on the
  I/O device or handled by the I/O server. Different I/O servers can handle the
  characters however they want, this only tells the I/O server which format the
  data is expected to have. In the `Module`/`Function`/`Args` case, `Encoding`
  tells which format the designated function produces.

  Notice also that byte-oriented data is simplest sent using the ISO Latin-1
  encoding.

- `Characters` are the data to be put on the I/O device. If `Encoding` is
  `latin1`, this is an `t:iolist/0`. If `Encoding` is `unicode`, this is an
  Erlang standard mixed Unicode list (one integer in a list per character,
  characters in binaries represented as UTF-8).
- `Module`, `Function`, and `Args` denote a function that is called to produce
  the data (like `io_lib:format/2`).

  `Args` is a list of arguments to the function. The function is to produce data
  in the specified `Encoding`. The I/O server is to call the function as
  [`apply(Mod, Func, Args)`](`apply/3`) and put the returned data on the I/O
  device as if it was sent in a `{put_chars, Encoding, Characters}` request. If
  the function returns anything else than a binary or list, or throws an
  exception, an error is to be sent back to the client.

The I/O server replies to the client with an `io_reply` tuple, where element
`Reply` is one of:

```text
ok
{error, Error}
```

- `Error` describes the error to the client, which can do whatever it wants with
  it. The `m:io` module typically returns it "as is".

## Input Requests

To read characters from an I/O device, the following `Request`s exist:

```erlang
{get_until, Encoding, Prompt, Module, Function, ExtraArgs}
```

- `Encoding` denotes how data is to be sent back to the client and what data is
  sent to the function denoted by `Module`/`Function`/`ExtraArgs`. If the
  function supplied returns data as a list, the data is converted to this
  encoding. If the function supplied returns data in some other format, no
  conversion can be done, and it is up to the client-supplied function to return
  data in a proper way.

  If `Encoding` is `latin1`, lists of integers `0..255` or binaries containing
  plain bytes are sent back to the client when possible. If `Encoding` is
  `unicode`, lists with integers in the whole Unicode range or binaries encoded
  in UTF-8 are sent to the client. The user-supplied function always sees lists
  of integers, never binaries, but the list can contain numbers > 255 if
  `Encoding` is `unicode`.

- `Prompt` is a list of characters (not mixed, no binaries) or an atom to be
  output as a prompt for input on the I/O device. `Prompt` is often ignored by
  the I/O server; if set to `''`, it is always to be ignored (and results in
  nothing being written to the I/O device).
- `Module`, `Function`, and `ExtraArgs` denote a function and arguments to
  determine when enough data is written. The function is to take two more
  arguments, the last state, and a list of characters. The function is to return
  one of:

  ```erlang
  {done, Result, RestChars}
  {more, Continuation}
  ```

  `Result` can be any Erlang term, but if it is a `t:list/0`, the I/O server can
  convert it to a `t:binary/0` of appropriate format before returning it to the
  client, if the I/O server is set in binary mode (see below).

  The function is called with the data the I/O server finds on its I/O device,
  returning one of:

  - `{done, Result, RestChars}` when enough data is read. In this case `Result`
    is sent to the client and `RestChars` is kept in the I/O server as a buffer
    for later input.
  - `{more, Continuation}`, which indicates that more characters are needed to
    complete the request.

  `Continuation` is sent as the state in later calls to the function when more
  characters are available. When no more characters are available, the function
  must return `{done, eof, Rest}`. The initial state is the empty list. The data
  when an end of file is reached on the IO device is the atom `eof`.

  An emulation of the `get_line` request can be (inefficiently) implemented
  using the following functions:

  ```erlang
  -module(demo).
  -export([until_newline/3, get_line/1]).

  until_newline(_ThisFar,eof,_MyStopCharacter) ->
      {done,eof,[]};
  until_newline(ThisFar,CharList,MyStopCharacter) ->
      case
          lists:splitwith(fun(X) -> X =/= MyStopCharacter end,  CharList)
      of
  	{L,[]} ->
              {more,ThisFar++L};
  	{L2,[MyStopCharacter|Rest]} ->
  	    {done,ThisFar++L2++[MyStopCharacter],Rest}
      end.

  get_line(IoServer) ->
      IoServer ! {io_request,
                  self(),
                  IoServer,
                  {get_until, unicode, '', ?MODULE, until_newline, [$\n]}},
      receive
          {io_reply, IoServer, Data} ->
  	    Data
      end.
  ```

  Notice that the last element in the `Request` tuple (`[$\n]`) is appended to
  the argument list when the function is called. The function is to be called
  like [`apply(Module, Function, [ State, Data | ExtraArgs ])`](`apply/3`) by
  the I/O server.

A fixed number of characters is requested using the following `Request`:

```text
{get_chars, Encoding, Prompt, N}
```

- `Encoding` and `Prompt` as for `get_until`.
- `N` is the number of characters to be read from the I/O device.

A single line (as in former example) is requested with the following `Request`:

```text
{get_line, Encoding, Prompt}
```

- `Encoding` and `Prompt` as for `get_until`.

Clearly, `get_chars` and `get_line` could be implemented with the `get_until`
request (and indeed they were originally), but demands for efficiency have made
these additions necessary.

The I/O server replies to the client with an `io_reply` tuple, where element
`Reply` is one of:

```text
Data
eof
{error, Error}
```

- `Data` is the characters read, in list or binary form (depending on the I/O
  server mode, see the next section).
- `eof` is returned when input end is reached and no more data is available to
  the client process.
- `Error` describes the error to the client, which can do whatever it wants with
  it. The `m:io` module typically returns it as is.

## I/O Server Modes

Demands for efficiency when reading data from an I/O server has not only lead to
the addition of the `get_line` and `get_chars` requests, but has also added the
concept of I/O server options. No options are mandatory to implement, but all
I/O servers in the Erlang standard libraries honor the `binary` option, which
allows element `Data` of the `io_reply` tuple to be a binary instead of a list
_when possible_. If the data is sent as a binary, Unicode data is sent in the
standard Erlang Unicode format, that is, UTF-8 (notice that the function of the
`get_until` request still gets list data regardless of the I/O server mode).

Notice that the `get_until` request allows for a function with the data
specified as always being a list. Also, the return value data from such a
function can be of any type (as is indeed the case when an
[`io:fread/2,3`](`io:fread/2`) request is sent to an I/O server). The client
must be prepared for data received as answers to those requests to be in various
forms. However, the I/O server is to convert the results to binaries whenever
possible (that is, when the function supplied to `get_until` returns a list).
This is done in the example in section
[An Annotated and Working Example I/O Server](io_protocol.md#example_io_server).

An I/O server in binary mode affects the data sent to the client, so that it
must be able to handle binary data. For convenience, the modes of an I/O server
can be set and retrieved using the following I/O requests:

```text
{setopts, Opts}
```

- `Opts` is a list of options in the format recognized by the `m:proplists`
  module (and by the I/O server).

As an example, the I/O server for the interactive shell (in `group.erl`)
understands the following options:

```erlang
{binary, boolean()} (or binary/list)
{echo, boolean()}
{expand_fun, fun()}
{encoding, unicode/latin1} (or unicode/latin1)
```

Options `binary` and `encoding` are common for all I/O servers in OTP, while
`echo` and `expand` are valid only for this I/O server. Option `unicode`
notifies how characters are put on the physical I/O device, that is, if the
terminal itself is Unicode-aware. It does not affect how characters are sent in
the I/O protocol, where each request contains encoding information for the
provided or returned data.

The I/O server is to send one of the following as `Reply`:

```text
ok
{error, Error}
```

An error (preferably `enotsup`) is to be expected if the option is not supported
by the I/O server (like if an `echo` option is sent in a `setopts` request to a
plain file).

To retrieve options, the following request is used:

```text
getopts
```

This request asks for a complete list of all options supported by the I/O server
as well as their current values.

The I/O server replies:

```text
OptList
{error, Error}
```

- `OptList` is a list of tuples `{Option, Value}`, where `Option` always is an
  atom.

## Multiple I/O Requests

The `Request` element can in itself contain many `Request`s by using the
following format:

```text
{requests, Requests}
```

- `Requests` is a list of valid `io_request` tuples for the protocol. They must
  be executed in the order that they appear in the list. The execution is to
  continue until one of the requests results in an error or the list is
  consumed. The result of the last request is sent back to the client.

The I/O server can, for a list of requests, send any of the following valid
results in the reply, depending on the requests in the list:

```erlang
ok
{ok, Data}
{ok, Options}
{error, Error}
```

## Optional I/O Request

The following I/O request is optional to implement and a client is to be
prepared for an error return:

```text
{get_geometry, Geometry}
```

- `Geometry` is the atom `rows` or the atom `columns`.

The I/O server is to send one of the following as `Reply`:

```text
N
{error, Error}
```

- `N` is the number of character rows or columns that the I/O device has, if
  applicable to the I/O device handled by the I/O server, otherwise
  `{error, enotsup}` is a good answer.

## Unimplemented Request Types

If an I/O server encounters a request that it does not recognize (that is, the
`io_request` tuple has the expected format, but the `Request` is unknown), the
I/O server is to send a valid reply with the error tuple:

```text
{error, request}
```

This makes it possible to extend the protocol with optional requests and for the
clients to be somewhat backward compatible.

## An Annotated and Working Example I/O Server

[](){: #example_io_server }

An I/O server is any process capable of handling the I/O protocol. There is no
generic I/O server behavior, but could well be. The framework is simple, a
process handling incoming requests, usually both I/O-requests and other I/O
device-specific requests (positioning, closing, and so on).

The example I/O server stores characters in an ETS table, making up a fairly
crude RAM file.

The module begins with the usual directives, a function to start the I/O server
and a main loop handling the requests:

```erlang
-module(ets_io_server).

-export([start_link/0, init/0, loop/1, until_newline/3, until_enough/3]).

-define(CHARS_PER_REC, 10).

-record(state, {
	  table,
	  position, % absolute
	  mode % binary | list
	 }).

start_link() ->
    spawn_link(?MODULE,init,[]).

init() ->
    Table = ets:new(noname,[ordered_set]),
    ?MODULE:loop(#state{table = Table, position = 0, mode=list}).

loop(State) ->
    receive
	{io_request, From, ReplyAs, Request} ->
	    case request(Request,State) of
		{Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
		    reply(From, ReplyAs, Reply),
		    ?MODULE:loop(NewState);
		{stop, Reply, _NewState} ->
		    reply(From, ReplyAs, Reply),
		    exit(Reply)
	    end;
	%% Private message
	{From, rewind} ->
	    From ! {self(), ok},
	    ?MODULE:loop(State#state{position = 0});
	_Unknown ->
	    ?MODULE:loop(State)
    end.
```

The main loop receives messages from the client (which can use the the `m:io`
module to send requests). For each request, the function `request/2` is called
and a reply is eventually sent using function `reply/3`.

The "private" message `{From, rewind}` results in the current position in the
pseudo-file to be reset to `0` (the beginning of the "file"). This is a typical
example of I/O device-specific messages not being part of the I/O protocol. It
is usually a bad idea to embed such private messages in `io_request` tuples, as
that can confuse the reader.

First, we examine the reply function:

```erlang
reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.
```

It sends the `io_reply` tuple back to the client, providing element `ReplyAs`
received in the request along with the result of the request, as described
earlier.

We need to handle some requests. First the requests for writing characters:

```erlang
request({put_chars, Encoding, Chars}, State) ->
    put_chars(unicode:characters_to_list(Chars,Encoding),State);
request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
	request({put_chars, Encoding, apply(Module, Function, Args)}, State)
    catch
	_:_ ->
	    {error, {error,Function}, State}
    end;
```

The `Encoding` says how the characters in the request are represented. We want
to store the characters as lists in the ETS table, so we convert them to lists
using function `unicode:characters_to_list/2`. The conversion function
conveniently accepts the encoding types `unicode` and `latin1`, so we can use
`Encoding` directly.

When `Module`, `Function`, and `Arguments` are provided, we apply it and do the
same with the result as if the data was provided directly.

We handle the requests for retrieving data:

```erlang
request({get_until, Encoding, _Prompt, M, F, As}, State) ->
    get_until(Encoding, M, F, As, State);
request({get_chars, Encoding, _Prompt, N}, State) ->
    %% To simplify the code, get_chars is implemented using get_until
    get_until(Encoding, ?MODULE, until_enough, [N], State);
request({get_line, Encoding, _Prompt}, State) ->
    %% To simplify the code, get_line is implemented using get_until
    get_until(Encoding, ?MODULE, until_newline, [$\n], State);
```

Here we have cheated a little by more or less only implementing `get_until` and
using internal helpers to implement `get_chars` and `get_line`. In production
code, this can be inefficient, but that depends on the frequency of the
different requests. Before we start implementing functions `put_chars/2` and
`get_until/5`, we examine the few remaining requests:

```erlang
request({get_geometry,_}, State) ->
    {error, {error,enotsup}, State};
request({setopts, Opts}, State) ->
    setopts(Opts, State);
request(getopts, State) ->
    getopts(State);
request({requests, Reqs}, State) ->
     multi_request(Reqs, {ok, ok, State});
```

Request `get_geometry` has no meaning for this I/O server, so the reply is
`{error, enotsup}`. The only option we handle is `binary`/`list`, which is done
in separate functions.

The multi-request tag (`requests`) is handled in a separate loop function
applying the requests in the list one after another, returning the last result.

`{error, request}` must be returned if the request is not recognized:

```erlang
request(_Other, State) ->
    {error, {error, request}, State}.
```

Next we handle the different requests, first the fairly generic multi-request
type:

```erlang
multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], Result) ->
    Result.
```

We loop through the requests one at the time, stopping when we either encounter
an error or the list is exhausted. The last return value is sent back to the
client (it is first returned to the main loop and then sent back by function
`io_reply`).

Requests `getopts` and `setopts` are also simple to handle. We only change or
read the state record:

```erlang
setopts(Opts0,State) ->
    Opts = proplists:unfold(
	     proplists:substitute_negations(
	       [{list,binary}],
	       Opts0)),
    case check_valid_opts(Opts) of
	true ->
	        case proplists:get_value(binary, Opts) of
		    true ->
			{ok,ok,State#state{mode=binary}};
		    false ->
			{ok,ok,State#state{mode=binary}};
		    _ ->
			{ok,ok,State}
		end;
	false ->
	    {error,{error,enotsup},State}
    end.
check_valid_opts([]) ->
    true;
check_valid_opts([{binary,Bool}|T]) when is_boolean(Bool) ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

getopts(#state{mode=M} = S) ->
    {ok,[{binary, case M of
		      binary ->
			  true;
		      _ ->
			  false
		  end}],S}.
```

As a convention, all I/O servers handle both `{setopts, [binary]}`,
`{setopts, [list]}`, and `{setopts,[{binary, boolean()}]}`, hence the trick with
`proplists:substitute_negations/2` and `proplists:unfold/1`. If invalid options
are sent to us, we send `{error, enotsup}` back to the client.

Request `getopts` is to return a list of `{Option, Value}` tuples. This has the
twofold function of providing both the current values and the available options
of this I/O server. We have only one option, and hence return that.

So far this I/O server is fairly generic (except for request `rewind` handled in
the main loop and the creation of an ETS table). Most I/O servers contain code
similar to this one.

To make the example runnable, we start implementing the reading and writing of
the data to/from the ETS table. First function `put_chars/3`:

```erlang
put_chars(Chars, #state{table = T, position = P} = State) ->
    R = P div ?CHARS_PER_REC,
    C = P rem ?CHARS_PER_REC,
    [ apply_update(T,U) || U <- split_data(Chars, R, C) ],
    {ok, ok, State#state{position = (P + length(Chars))}}.
```

We already have the data as (Unicode) lists and therefore only split the list in
runs of a predefined size and put each run in the table at the current position
(and forward). Functions `split_data/3` and `apply_update/2` are implemented
below.

Now we want to read data from the table. Function `get_until/5` reads data and
applies the function until it says that it is done. The result is sent back to
the client:

```erlang
get_until(Encoding, Mod, Func, As,
	  #state{position = P, mode = M, table = T} = State) ->
    case get_loop(Mod,Func,As,T,P,[]) of
	{done,Data,_,NewP} when is_binary(Data); is_list(Data) ->
	    if
		M =:= binary ->
		    {ok,
		     unicode:characters_to_binary(Data, unicode, Encoding),
		     State#state{position = NewP}};
		true ->
		    case check(Encoding,
		               unicode:characters_to_list(Data, unicode))
                    of
			{error, _} = E ->
			    {error, E, State};
			List ->
			    {ok, List,
			     State#state{position = NewP}}
		    end
	    end;
	{done,Data,_,NewP} ->
	    {ok, Data, State#state{position = NewP}};
	Error ->
	    {error, Error, State}
    end.

get_loop(M,F,A,T,P,C) ->
    {NewP,L} = get(P,T),
    case catch apply(M,F,[C,L|A]) of
	{done, List, Rest} ->
	    {done, List, [], NewP - length(Rest)};
	{more, NewC} ->
	    get_loop(M,F,A,T,NewP,NewC);
	_ ->
	    {error,F}
    end.
```

Here we also handle the mode (`binary` or `list`) that can be set by request
`setopts`. By default, all OTP I/O servers send data back to the client as
lists, but switching mode to `binary` can increase efficiency if the I/O server
handles it in an appropriate way. The implementation of `get_until` is difficult
to get efficient, as the supplied function is defined to take lists as
arguments, but `get_chars` and `get_line` can be optimized for binary mode.
However, this example does not optimize anything.

It is important though that the returned data is of the correct type depending
on the options set. We therefore convert the lists to binaries in the correct
encoding _if possible_ before returning. The function supplied in the
`get_until` request tuple can, as its final result return anything, so only
functions returning lists can get them converted to binaries. If the request
contains encoding tag `unicode`, the lists can contain all Unicode code points
and the binaries are to be in UTF-8. If the encoding tag is `latin1`, the client
is only to get characters in the range `0..255`. Function `check/2` takes care
of not returning arbitrary Unicode code points in lists if the encoding was
specified as `latin1`. If the function does not return a list, the check cannot
be performed and the result is that of the supplied function untouched.

To manipulate the table we implement the following utility functions:

```erlang
check(unicode, List) ->
    List;
check(latin1, List) ->
    try
	[ throw(not_unicode) || X <- List,
				X > 255 ],
	List
    catch
	throw:_ ->
	    {error,{cannot_convert, unicode, latin1}}
    end.
```

The function check provides an error tuple if Unicode code points > 255 are to
be returned if the client requested `latin1`.

The two functions `until_newline/3` and `until_enough/3` are helpers used
together with function `get_until/5` to implement `get_chars` and `get_line`
(inefficiently):

```erlang
until_newline([],eof,_MyStopCharacter) ->
    {done,eof,[]};
until_newline(ThisFar,eof,_MyStopCharacter) ->
    {done,ThisFar,[]};
until_newline(ThisFar,CharList,MyStopCharacter) ->
    case
        lists:splitwith(fun(X) -> X =/= MyStopCharacter end,  CharList)
    of
	{L,[]} ->
            {more,ThisFar++L};
	{L2,[MyStopCharacter|Rest]} ->
	    {done,ThisFar++L2++[MyStopCharacter],Rest}
    end.

until_enough([],eof,_N) ->
    {done,eof,[]};
until_enough(ThisFar,eof,_N) ->
    {done,ThisFar,[]};
until_enough(ThisFar,CharList,N)
  when length(ThisFar) + length(CharList) >= N ->
    {Res,Rest} = my_split(N,ThisFar ++ CharList, []),
    {done,Res,Rest};
until_enough(ThisFar,CharList,_N) ->
    {more,ThisFar++CharList}.
```

As can be seen, the functions above are just the type of functions that are to
be provided in `get_until` requests.

To complete the I/O server, we only need to read and write the table in an
appropriate way:

```erlang
get(P,Tab) ->
    R = P div ?CHARS_PER_REC,
    C = P rem ?CHARS_PER_REC,
    case ets:lookup(Tab,R) of
	[] ->
	    {P,eof};
	[{R,List}] ->
	    case my_split(C,List,[]) of
		{_,[]} ->
		    {P+length(List),eof};
		{_,Data} ->
		    {P+length(Data),Data}
	    end
    end.

my_split(0,Left,Acc) ->
    {lists:reverse(Acc),Left};
my_split(_,[],Acc) ->
    {lists:reverse(Acc),[]};
my_split(N,[H|T],Acc) ->
    my_split(N-1,T,[H|Acc]).

split_data([],_,_) ->
    [];
split_data(Chars, Row, Col) ->
    {This,Left} = my_split(?CHARS_PER_REC - Col, Chars, []),
    [ {Row, Col, This} | split_data(Left, Row + 1, 0) ].

apply_update(Table, {Row, Col, List}) ->
    case ets:lookup(Table,Row) of
	[] ->
	    ets:insert(Table,{Row, lists:duplicate(Col,0) ++ List});
	[{Row, OldData}] ->
	    {Part1,_} = my_split(Col,OldData,[]),
	    {_,Part2} = my_split(Col+length(List),OldData,[]),
	    ets:insert(Table,{Row, Part1 ++ List ++ Part2})
    end.
```

The table is read or written in chunks of `?CHARS_PER_REC`, overwriting when
necessary. The implementation is clearly not efficient, it is just working.

This concludes the example. It is fully runnable and you can read or write to
the I/O server by using, for example, the `m:io` module or even the `m:file`
module. It is as simple as that to implement a fully fledged I/O server in
Erlang.
