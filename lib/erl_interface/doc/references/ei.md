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
# ei

Routines for handling the Erlang binary term format.

## Description

The library `ei` contains macros and functions to encode and decode the Erlang
binary term format.

`ei` allows you to convert atoms, lists, numbers, and binaries to and from the
binary format. This is useful when writing port programs and drivers. `ei` uses
a given buffer, no dynamic memory (except `ei_decode_fun()`) and is often quite
fast.

`ei` also handles C-nodes, C-programs that talks Erlang distribution with Erlang
nodes (or other C-nodes) using the Erlang distribution format.The `ei` library
is thread safe, and using threads, one process can handle multiple C-nodes.

The decode and encode functions use a buffer and an index into the buffer, which
points at the point where to encode and decode. The index is updated to point
right after the term encoded/decoded. No checking is done whether the term fits
in the buffer or not. If encoding goes outside the buffer, the program can
crash.

All functions take two parameters:

- `buf` is a pointer to the buffer where the binary data is or will be.
- `index` is a pointer to an index into the buffer. This parameter is
  incremented with the size of the term decoded/encoded.

The data is thus at `buf[*index]` when an `ei` function is called.

All encode functions assume that the `buf` and `index` parameters point to a
buffer large enough for the data. Note that the binary term format uses variable-
length encoding so different values can require a different amount of space. For
example, smaller integer values can be more compact than larger ones. To get
the size of an encoded term, without encoding it, pass `NULL` instead of a
buffer pointer. Parameter `index` is incremented, but nothing will be encoded.
This is the way in `ei` to "preflight" term encoding.

There are also encode functions that use a dynamic buffer. It is often more
convenient to use these to encode data. All encode functions comes in two
versions; those starting with `ei_x_` use a dynamic buffer of type
[`ei_x_buff`](ei.md#ei_x_buff).

All functions return `0` if successful, otherwise `-1` (for example, if a term
is not of the expected type, or the data to decode is an invalid Erlang term).

Some of the decode functions need a pre-allocated buffer. This buffer must be
allocated large enough, and for non-compound types the
[`ei_get_type()`](ei.md#ei_get_type) function returns the size required (notice
that for strings an extra byte is needed for the `NULL`\-terminator).

## Data Types

- **`ei_term`{: #ei_term }**

  ```c
  typedef struct {
      char ei_type;
      int arity;
      int size;
      union {
  	long i_val;
  	double d_val;
  	char atom_name[MAXATOMLEN_UTF8];
  	erlang_pid pid;
  	erlang_port port;
  	erlang_ref ref;
      } value;
  } ei_term;
  ```

  Structure written by [`ei_decode_ei_term()`](ei.md#ei_decode_ei_term). The
  `ei_type` field is the type of the term which equals to what
  [`ei_get_type()`](ei.md#ei_get_type) sets `*type` to.

- **`ei_x_buff`{: #ei_x_buff }** - A dynamically resized buffer. It is a
  `struct` with two fields of interest for the user:

  - **`char *buff`** - Pointer to the dynamically allocated buffer.

  - **`int index`** - Offset to the next byte to write which also equals the
    amount of bytes currently written.

  An `ei_x_buff` is initialized by calling either [`ei_x_new()`](ei.md#ei_x_new)
  or [`ei_x_new_with_version()`](ei.md#ei_x_new_with_version). The memory used
  by an initialized `ei_x_buff` is released by calling
  [`ei_x_free()`](ei.md#ei_x_free).

- **`erlang_char_encoding`{: #erlang_char_encoding }**

  ```c
  typedef enum {
      ERLANG_ASCII = 1,
      ERLANG_LATIN1 = 2,
      ERLANG_UTF8 = 4
  } erlang_char_encoding;
  ```

  The character encodings used for atoms. `ERLANG_ASCII` represents 7-bit ASCII.
  Latin-1 and UTF-8 are different extensions of 7-bit ASCII. All 7-bit ASCII
  characters are valid Latin-1 and UTF-8 characters. ASCII and Latin-1 both
  represent each character by one byte. An UTF-8 character can consist of 1-4
  bytes. Notice that these constants are bit-flags and can be combined with
  bitwise OR.

- **`erlang_fun`{: #erlang_fun }** - Opaque data type representing an Erlang
  fun.

- **`erlang_pid`{: #erlang_pid }** - Opaque data type representing an Erlang
  process identifier.

- **`erlang_port`{: #erlang_port }** - Opaque data type representing an Erlang
  port identifier.

- **`erlang_ref`{: #erlang_ref }** - Opaque data type representing an Erlang
  reference.

- **`erlang_trace`{: #erlang_trace }** - Opaque data type representing an Erlang
  sequential trace token.

## ei_cmp_pids()

```c
int ei_cmp_pids(erlang_pid *a, erlang_pid *b);
```

Compare two process identifiers. The comparison is done the same way as Erlang
does.

Returns `0` if `a` and `b` are equal. Returns a value less than `0` if `a`
compares as less than `b`. Returns a value larger than `0` if `a` compares as
larger than `b`.

Available since OTP 23.0

## ei_cmp_ports()

```c
int ei_cmp_ports(erlang_port *a, erlang_port *b);
```

Compare two port identifiers. The comparison is done the same way as Erlang
does.

Returns `0` if `a` and `b` are equal. Returns a value less than `0` if `a`
compares as less than `b`. Returns a value larger than `0` if `a` compares as
larger than `b`.

Available since OTP 23.0

## ei_cmp_refs()

```c
int ei_cmp_refs(erlang_ref *a, erlang_ref *b);
```

Compare two references. The comparison is done the same way as Erlang does.

Returns `0` if `a` and `b` are equal. Returns a value less than `0` if `a`
compares as less than `b`. Returns a value larger than `0` if `a` compares as
larger than `b`.

Available since OTP 23.0

## ei_decode_atom()

```c
int ei_decode_atom(const char *buf, int *index, char *p);
```

Decodes an atom from the binary format. The `NULL`\-terminated name of the atom
is placed at `p`. At most `MAXATOMLEN` bytes can be placed in the buffer.

## ei_decode_atom_as()

```c
int ei_decode_atom_as(const char *buf, int *index, char *p, int plen,
  erlang_char_encoding want, erlang_char_encoding* was, erlang_char_encoding* result);
```

Decodes an atom from the binary format. The `NULL`\-terminated name of the atom
is placed in buffer at `p` of length `plen` bytes.

The wanted string encoding is specified by [`want`](ei.md#erlang_char_encoding).
The original encoding used in the binary format (Latin-1 or UTF-8) can be
obtained from `*was`. The encoding of the resulting string (7-bit ASCII,
Latin-1, or UTF-8) can be obtained from `*result`. Both `was` and `result` can
be `NULL`. `*result` can differ from `want` if `want` is a bitwise OR'd
combination like `ERLANG_LATIN1|ERLANG_UTF8` or if `*result` turns out to be
pure 7-bit ASCII (compatible with both Latin-1 and UTF-8).

This function fails if the atom is too long for the buffer or if it cannot be
represented with encoding `want`.

This function was introduced in Erlang/OTP R16 as part of a first step to
support UTF-8 atoms.

Available since OTP R16B

## ei_decode_bignum()

```c
int ei_decode_bignum(const char *buf, int *index, mpz_t obj);
```

Decodes an integer in the binary format to a GMP `mpz_t` integer. To use this
function, the `ei` library must be configured and compiled to use the GMP
library.

## ei_decode_binary()

```c
int ei_decode_binary(const char *buf, int *index, void *p, long *len);
```

Decodes a binary from the binary format. Parameter `len` is set to the actual
size of the binary. Notice that `ei_decode_binary()` assumes that there is
enough room for the binary. The size required can be fetched by
[`ei_get_type()`](ei.md#ei_get_type).

## ei_decode_bitstring()

```c
int ei_decode_bitstring(const char *buf, int *index, const char **pp,
  unsigned int *bitoffsp, size_t *nbitsp);
```

Decodes a bit string from the binary format.

- **`pp`** - Either `NULL` or `*pp` returns a pointer to the first byte of the
  bit string. The returned bit string is readable as long as the buffer pointed
  to by `buf` is readable and not written to.

- **`bitoffsp`** - Either `NULL` or `*bitoffsp` returns the number of unused
  bits in the first byte pointed to by `*pp`. The value of `*bitoffsp` is
  between 0 and 7. Unused bits in the first byte are the most significant bits.

- **`nbitsp`** - Either `NULL` or `*nbitsp` returns the length of the bit string
  in _bits_.

Returns `0` if it was a bit string term.

The number of _bytes_ pointed to by `*pp`, which are part of the bit string, is
`(*bitoffsp + *nbitsp + 7)/8`. If `(*bitoffsp + *bitsp)%8 > 0` then only
`(*bitoffsp + *bitsp)%8` bits of the last byte are used. Unused bits in the last
byte are the least significant bits.

The values of unused bits in the first and last byte are undefined and cannot be
relied on.

Number of bits may be divisible by 8, which means a binary decodable by
`ei_decode_binary` is also decodable by `ei_decode_bitstring`.

Available since OTP 22.0

## ei_decode_boolean()

```c
int ei_decode_boolean(const char *buf, int *index, int *p);
```

Decodes a boolean value from the binary format. A boolean is actually an atom,
`true` decodes 1 and `false` decodes 0.

## ei_decode_char()

```c
int ei_decode_char(const char *buf, int *index, char *p);
```

Decodes a char (8-bit) integer between 0-255 from the binary format. For
historical reasons the returned integer is of type `char`. Your C code is to
consider the returned value to be of type `unsigned char` even if the C
compilers and system can define `char` to be signed.

## ei_decode_double()

```c
int ei_decode_double(const char *buf, int *index, double *p);
```

Decodes a double-precision (64-bit) floating point number from the binary
format.

## ei_decode_ei_term()

```c
int ei_decode_ei_term(const char* buf, int* index, ei_term* term);
```

Decodes any term, or at least tries to. If the term pointed at by `*index` in
`buf` fits in the `term` union, it is decoded, and the appropriate field in
`term->value` is set, and `*index` is incremented by the term size.

The function returns `1` on successful decoding, `-1` on error, and `0` if the
term seems alright, but does not fit in the `term` structure. If `1` is
returned, the `index` is incremented, and `term` contains the decoded term.

The `term` structure contains the arity for a tuple or list, size for a binary,
string, or atom. It contains a term if it is any of the following: integer,
float, atom, pid, port, or ref.

## ei_decode_fun()

## free_fun()

```c
int ei_decode_fun(const char *buf, int *index, erlang_fun *p);
```

```c
void free_fun(erlang_fun* f);
```

Decodes a fun from the binary format. Parameter `p` is to be `NULL` or point to
an `erlang_fun` structure. This is the only decode function that allocates
memory. When the `erlang_fun` is no longer needed, it is to be freed with
`free_fun`. (This has to do with the arbitrary size of the environment for a
fun.)

## ei_decode_iodata()

```c
int ei_decode_iodata(const char *buf, int *index, int *size, char *outbuf);
```

Decodes a term of the type [`iodata()`](`e:system:typespec.md#builtin_types`).
The `t:iodata/0` term will be flattened an written into the buffer pointed to by
the `outbuf` argument. The byte size of the `iodata` is written into the integer
variable pointed to by the `size` argument. Both `size` and `outbuf` can be set
to `NULL`. The integer pointed to by the `index` argument is updated to refer to
the term following after the `t:iodata/0` term regardless of the the state of
the `size` and the `outbuf` arguments.

Note that the buffer pointed to by the `outbuf` argument must be large enough if
a non `NULL` value is passed as `outbuf`. You typically want to call
`ei_decode_iodata()` twice. First with a non `NULL` `size` argument and a `NULL`
`outbuf` argument in order to determine the size of the buffer needed, and then
once again in order to do the actual decoding. Note that the integer pointed to
by `index` will be updated by the call determining the size as well, so you need
to reset it before the second call doing the actual decoding.

Returns `0` on success and `-1` on failure. Failure might be either due to
invalid encoding of the term or due to the term not being of the type
`t:iodata/0`. On failure, the integer pointed to by the `index` argument will be
updated to refer to the sub term where the failure was detected.

Available since OTP 23.0

## ei_decode_list_header()

```c
int ei_decode_list_header(const char *buf, int *index, int *arity);
```

Decodes a list header from the binary format. The number of elements is returned
in `arity`. The `arity+1` elements follow (the last one is the tail of the list,
normally an empty list). If `arity` is `0`, it is an empty list.

Notice that lists are encoded as strings if they consist entirely of integers in
the range 0..255. This function do not decode such strings, use
`ei_decode_string()` instead.

## ei_decode_long()

```c
int ei_decode_long(const char *buf, int *index, long *p);
```

Decodes a long integer from the binary format. If the code is 64 bits, the
function `ei_decode_long()` is the same as `ei_decode_longlong()`.

## ei_decode_longlong()

```c
int ei_decode_longlong(const char *buf, int *index, long long *p);
```

Decodes a GCC `long long` or Visual C++ `__int64` (64-bit) integer from the
binary format.

## ei_decode_map_header()

```c
int ei_decode_map_header(const char *buf, int *index, int *arity);
```

Decodes a map header from the binary format. The number of key-value pairs is
returned in `*arity`. Keys and values follow in this order:
`K1, V1, K2, V2, ..., Kn, Vn`. This makes a total of `arity*2` terms. If `arity`
is zero, it is an empty map. A correctly encoded map does not have duplicate
keys.

Available since OTP 17.0

## ei_decode_pid()

```c
int ei_decode_pid(const char *buf, int *index, erlang_pid *p);
```

Decodes a process identifier (pid) from the binary format.

## ei_decode_port()

```c
int ei_decode_port(const char *buf, int *index, erlang_port *p);
```

Decodes a port identifier from the binary format.

## ei_decode_ref()

```c
int ei_decode_ref(const char *buf, int *index, erlang_ref *p);
```

Decodes a reference from the binary format.

## ei_decode_string()

```c
int ei_decode_string(const char *buf, int *index, char *p);
```

Decodes a string from the binary format. A string in Erlang is a list of
integers between 0 and 255. Notice that as the string is just a list, sometimes
lists are encoded as strings by [`term_to_binary/1`](`term_to_binary/1`), even
if it was not intended.

The string is copied to `p`, and enough space must be allocated. The returned
string is `NULL`\-terminated, so you must add an extra byte to the memory
requirement.

## ei_decode_trace()

```c
int ei_decode_trace(const char *buf, int *index, erlang_trace *p);
```

Decodes an Erlang trace token from the binary format.

## ei_decode_tuple_header()

```c
int ei_decode_tuple_header(const char *buf, int *index, int *arity);
```

Decodes a tuple header, the number of elements is returned in `arity`. The tuple
elements follow in order in the buffer.

## ei_decode_ulong()

```c
int ei_decode_ulong(const char *buf, int *index, unsigned long *p);
```

Decodes an unsigned long integer from the binary format. If the code is 64 bits,
the function `ei_decode_ulong()` is the same as `ei_decode_ulonglong()`.

## ei_decode_ulonglong()

```c
int ei_decode_ulonglong(const char *buf, int *index, unsigned long long *p);
```

Decodes a GCC `unsigned long long` or Visual C++ `unsigned __int64` (64-bit)
integer from the binary format.

## ei_decode_version()

```c
int ei_decode_version(const char *buf, int *index, int *version);
```

Decodes the version magic number for the Erlang binary term format. It must be
the first token in a binary term.

## ei_encode_atom()

## ei_encode_atom_len()

## ei_x_encode_atom()

## ei_x_encode_atom_len()

```c
int ei_encode_atom(char *buf, int *index, const char *p);
```

```c
int ei_encode_atom_len(char *buf, int *index, const char *p, int len);
```

```c
int ei_x_encode_atom(ei_x_buff* x, const char *p);
```

```c
int ei_x_encode_atom_len(ei_x_buff* x, const char *p, int len);
```

Encodes an atom in the binary format. Parameter `p` is the name of the atom in
Latin-1 encoding. Only up to `MAXATOMLEN-1` bytes are encoded. The name is to be
`NULL`\-terminated, except for the `ei_x_encode_atom_len()` function.

## ei_encode_atom_as()

Available since OTP R16B

## ei_encode_atom_len_as()

Available since OTP R16B

## ei_x_encode_atom_as()

Available since OTP R16B

## ei_x_encode_atom_len_as()

```c
int ei_encode_atom_as(char *buf, int *index, const char *p,
  erlang_char_encoding from_enc, erlang_char_encoding to_enc);
```

```c
int ei_encode_atom_len_as(char *buf, int *index, const char *p, int len,
  erlang_char_encoding from_enc, erlang_char_encoding to_enc);
```

```c
int ei_x_encode_atom_as(ei_x_buff* x, const char *p,
  erlang_char_encoding from_enc, erlang_char_encoding to_enc);
```

```c
int ei_x_encode_atom_len_as(ei_x_buff* x, const char *p, int len,
  erlang_char_encoding from_enc, erlang_char_encoding to_enc);
```

Encodes an atom in the binary format. Parameter `p` is the name of the atom with
character encoding [`from_enc`](ei.md#erlang_char_encoding) (ASCII, Latin-1, or
UTF-8). The name must either be `NULL`\-terminated or a function variant with a
`len` parameter must be used.

The encoding fails if `p` is not a valid string in encoding `from_enc`.

Argument `to_enc` is ignored. As from Erlang/OTP 20 the encoding is always done
in UTF-8 which is readable by nodes as old as Erlang/OTP R16.

Available since OTP R16B

## ei_encode_bignum()

## ei_x_encode_bignum()

```c
int ei_encode_bignum(char *buf, int *index, mpz_t obj);
```

```c
int ei_x_encode_bignum(ei_x_buff *x, mpz_t obj);
```

Encodes a GMP `mpz_t` integer to binary format. To use this function, the `ei`
library must be configured and compiled to use the GMP library.

## ei_encode_binary()

## ei_x_encode_binary()

```c
int ei_encode_binary(char *buf, int *index, const void *p, long len);
```

```c
int ei_x_encode_binary(ei_x_buff* x, const void *p, long len);
```

Encodes a binary in the binary format. The data is at `p`, of `len` bytes
length.

## ei_encode_bitstring()

Available since OTP 22.0

## ei_x_encode_bitstring()

```c
int ei_encode_bitstring(char *buf, int *index, const char *p, size_t bitoffs, size_t nbits);
```

```c
int ei_x_encode_bitstring(ei_x_buff* x, const char *p, size_t bitoffs, size_t nbits);
```

Encodes a bit string in the binary format.

The data is at `p`. The length of the bit string is `nbits` bits. The first
`bitoffs` bits of the data at `p` are unused. The first byte which is part of
the bit string is `p[bitoffs/8]`. The `bitoffs%8` most significant bits of the
first byte `p[bitoffs/8]` are unused.

The number of bytes which is part of the bit string is
`(bitoffs + nbits + 7)/8`. If `(bitoffs + nbits)%8 > 0` then only
`(bitoffs + nbits)%8` bits of the last byte are used. Unused bits in the last
byte are the least significant bits.

The values of unused bits are disregarded and does not need to be cleared.

Available since OTP 22.0

## ei_encode_boolean()

## ei_x_encode_boolean()

```c
int ei_encode_boolean(char *buf, int *index, int p);
```

```c
int ei_x_encode_boolean(ei_x_buff* x, int p);
```

Encodes a boolean value as the atom `true` if `p` is not zero, or `false` if `p`
is zero.

## ei_encode_char()

## ei_x_encode_char()

```c
int ei_encode_char(char *buf, int *index, char p);
```

```c
int ei_x_encode_char(ei_x_buff* x, char p);
```

Encodes a char (8-bit) as an integer between 0-255 in the binary format. For
historical reasons the integer argument is of type `char`. Your C code is to
consider the specified argument to be of type `unsigned char` even if the C
compilers and system may define `char` to be signed.

## ei_encode_double()

## ei_x_encode_double()

```c
int ei_encode_double(char *buf, int *index, double p);
```

```c
int ei_x_encode_double(ei_x_buff* x, double p);
```

Encodes a double-precision (64-bit) floating point number in the binary format.

Returns `-1` if the floating point number is not finite.

## ei_encode_empty_list()

## ei_x_encode_empty_list()

```c
int ei_encode_empty_list(char* buf, int* index);
```

```c
int ei_x_encode_empty_list(ei_x_buff* x);
```

Encodes an empty list. It is often used at the tail of a list.

## ei_encode_fun()

## ei_x_encode_fun()

```c
int ei_encode_fun(char *buf, int *index, const erlang_fun *p);
```

```c
int ei_x_encode_fun(ei_x_buff* x, const erlang_fun* fun);
```

Encodes a fun in the binary format. Parameter `p` points to an `erlang_fun`
structure. The `erlang_fun` is not freed automatically, the `free_fun` is to be
called if the fun is not needed after encoding.

## ei_encode_list_header()

## ei_x_encode_list_header()

```c
int ei_encode_list_header(char *buf, int *index, int arity);
```

```c
int ei_x_encode_list_header(ei_x_buff* x, int arity);
```

Encodes a list header, with a specified arity. The next `arity+1` terms are the
elements (actually its `arity` cons cells) and the tail of the list. Lists and
tuples are encoded recursively, so that a list can contain another list or
tuple.

For example, to encode the list `[c, d, [e | f]]`:

```c
ei_encode_list_header(buf, &i, 3);
ei_encode_atom(buf, &i, "c");
ei_encode_atom(buf, &i, "d");
ei_encode_list_header(buf, &i, 1);
ei_encode_atom(buf, &i, "e");
ei_encode_atom(buf, &i, "f");
ei_encode_empty_list(buf, &i);
```

> #### Note {: .info }
>
> It may seem that there is no way to create a list without knowing the number
> of elements in advance. But indeed there is a way. Notice that the list
> `[a, b, c]` can be written as `[a | [b | [c]]]`. Using this, a list can be
> written as conses.

To encode a list, without knowing the arity in advance:

```c
while (something()) {
    ei_x_encode_list_header(&x, 1);
    ei_x_encode_ulong(&x, i); /* just an example */
}
ei_x_encode_empty_list(&x);
```

## ei_encode_long()

## ei_x_encode_long()

```c
int ei_encode_long(char *buf, int *index, long p);
```

```c
int ei_x_encode_long(ei_x_buff* x, long p);
```

Encodes a long integer in the binary format. If the code is 64 bits, the
function `ei_encode_long()` is the same as `ei_encode_longlong()`.

## ei_encode_longlong()

## ei_x_encode_longlong()

```c
int ei_encode_longlong(char *buf, int *index, long long p);
```

```c
int ei_x_encode_longlong(ei_x_buff* x, long long p);
```

Encodes a GCC `long long` or Visual C++ `__int64` (64-bit) integer in the binary
format.

## ei_encode_map_header()

Available since OTP 17.0

## ei_x_encode_map_header()

```c
int ei_encode_map_header(char *buf, int *index, int arity);
```

```c
int ei_x_encode_map_header(ei_x_buff* x, int arity);
```

Encodes a map header, with a specified arity. The next `arity*2` terms encoded
will be the keys and values of the map encoded in the following order:
`K1, V1, K2, V2, ..., Kn, Vn`.

For example, to encode the map `#{a => "Apple", b => "Banana"}`:

```c
ei_x_encode_map_header(&x, 2);
ei_x_encode_atom(&x, "a");
ei_x_encode_string(&x, "Apple");
ei_x_encode_atom(&x, "b");
ei_x_encode_string(&x, "Banana");
```

A correctly encoded map cannot have duplicate keys.

Available since OTP 17.0

## ei_encode_pid()

## ei_x_encode_pid()

```c
int ei_encode_pid(char *buf, int *index, const erlang_pid *p);
```

```c
int ei_x_encode_pid(ei_x_buff* x, const erlang_pid *p);
```

Encodes an Erlang process identifier (pid) in the binary format. Parameter `p`
points to an `erlang_pid` structure which should either have been obtained
earlier with [`ei_decode_pid()`](ei.md#ei_decode_pid),
[`ei_self()`](ei_connect.md#ei_self) or created by
[`ei_make_pid()`](ei_connect.md#ei_make_pid).

## ei_encode_port()

## ei_x_encode_port()

```c
int ei_encode_port(char *buf, int *index, const erlang_port *p);
```

```c
int ei_x_encode_port(ei_x_buff* x, const erlang_port *p);
```

Encodes an Erlang port in the binary format. Parameter `p` points to an
`erlang_port` structure which should have been obtained earlier with
[`ei_decode_port()`](ei.md#ei_decode_port),

## ei_encode_ref()

## ei_x_encode_ref()

```c
int ei_encode_ref(char *buf, int *index, const erlang_ref *p);
```

```c
int ei_x_encode_ref(ei_x_buff* x, const erlang_ref *p);
```

Encodes an Erlang reference in the binary format. Parameter `p` points to an
`erlang_ref` structure which either should have been obtained earlier with
[`ei_decode_ref()`](ei.md#ei_decode_ref), or created by
[`ei_make_ref()`](ei_connect.md#ei_make_ref).

## ei_encode_string()

## ei_encode_string_len()

## ei_x_encode_string()

## ei_x_encode_string_len()

```c
int ei_encode_string(char *buf, int *index, const char *p);
```

```c
int ei_encode_string_len(char *buf, int *index, const char *p, int len);
```

```c
int ei_x_encode_string(ei_x_buff* x, const char *p);
```

```c
int ei_x_encode_string_len(ei_x_buff* x, const char* s, int len);
```

Encodes a string in the binary format. (A string in Erlang is a list, but is
encoded as a character array in the binary format.) The string is to be
`NULL`\-terminated, except for the `ei_x_encode_string_len()` function.

## ei_encode_trace()

## ei_x_encode_trace()

```c
int ei_encode_trace(char *buf, int *index, const erlang_trace *p);
```

```c
int ei_x_encode_trace(ei_x_buff* x, const erlang_trace *p);
```

Encodes an Erlang trace token in the binary format. Parameter `p` points to a
`erlang_trace` structure which should have been obtained earlier with
[`ei_decode_trace()`](ei.md#ei_decode_trace).

## ei_encode_tuple_header()

## ei_x_encode_tuple_header()

```c
int ei_encode_tuple_header(char *buf, int *index, int arity);
```

```c
int ei_x_encode_tuple_header(ei_x_buff* x, int arity);
```

Encodes a tuple header, with a specified arity. The next `arity` terms encoded
will be the elements of the tuple. Tuples and lists are encoded recursively, so
that a tuple can contain another tuple or list.

For example, to encode the tuple `{a, {b, {}}}`:

```c
ei_encode_tuple_header(buf, &i, 2);
ei_encode_atom(buf, &i, "a");
ei_encode_tuple_header(buf, &i, 2);
ei_encode_atom(buf, &i, "b");
ei_encode_tuple_header(buf, &i, 0);
```

## ei_encode_ulong()

## ei_x_encode_ulong()

```c
int ei_encode_ulong(char *buf, int *index, unsigned long p);
```

```c
int ei_x_encode_ulong(ei_x_buff* x, unsigned long p);
```

Encodes an unsigned long integer in the binary format. If the code is 64 bits,
the function `ei_encode_ulong()` is the same as `ei_encode_ulonglong()`.

## ei_encode_ulonglong()

## ei_x_encode_ulonglong()

```c
int ei_encode_ulonglong(char *buf, int *index, unsigned long long p);
```

```c
int ei_x_encode_ulonglong(ei_x_buff* x, unsigned long long p);
```

Encodes a GCC `unsigned long long` or Visual C++ `unsigned __int64` (64-bit)
integer in the binary format.

## ei_encode_version()

## ei_x_encode_version()

```c
int ei_encode_version(char *buf, int *index);
```

```c
int ei_x_encode_version(ei_x_buff* x);
```

Encodes a version magic number for the binary format. Must be the first token in
a binary term.

## ei_get_type()

```c
int ei_get_type(const char *buf, const int *index, int *type, int *size);
```

Returns the type in `*type` and size in `*size` of the encoded term. For strings
and atoms, size is the number of characters _not_ including the terminating
`NULL`. For binaries and bitstrings, `*size` is the number of bytes. For lists,
tuples and maps, `*size` is the arity of the object. For bignum integers,
`*size` is the number of bytes for the absolute value of the bignum. For other
types, `*size` is 0. In all cases, `index` is left unchanged.

Currently `*type` is one of:

- **ERL_ATOM_EXT** - Decode using either
  [`ei_decode_atom()`](ei.md#ei_decode_atom),
  [`ei_decode_atom_as()`](ei.md#ei_decode_atom_as), or
  [`ei_decode_boolean()`](ei.md#ei_decode_boolean).

- **ERL_BINARY_EXT** - Decode using either
  [`ei_decode_binary()`](ei.md#ei_decode_binary),
  [`ei_decode_bitstring()`](ei.md#ei_decode_bitstring), or
  [`ei_decode_iodata()`](ei.md#ei_decode_iodata).

- **ERL_BIT_BINARY_EXT** - Decode using
  [`ei_decode_bitstring()`](ei.md#ei_decode_bitstring).

- **ERL_FLOAT_EXT** - Decode using
  [`ei_decode_double()`](ei.md#ei_decode_double).

- **ERL_NEW_FUN_EXT, ERL_FUN_EXT, ERL_EXPORT_EXT** -
  Decode using [`ei_decode_fun()`](ei.md#ei_decode_fun).

- **ERL_SMALL_INTEGER_EXT, ERL_INTEGER_EXT, ERL_SMALL_BIG_EXT, ERL_LARGE_BIG_EXT** -
  Decode using either [`ei_decode_char()`](ei.md#ei_decode_char),
  [`ei_decode_long()`](ei.md#ei_decode_long),
  [`ei_decode_longlong()`](ei.md#ei_decode_longlong),
  [`ei_decode_ulong()`](ei.md#ei_decode_ulong),
  [`ei_decode_ulonglong()`](ei.md#ei_decode_ulonglong), or
  [`ei_decode_bignum()`](ei.md#ei_decode_bignum).

- **ERL_LIST_EXT, ERL_NIL_EXT** -
  Decode using either [`ei_decode_list_header()`](ei.md#ei_decode_list_header),
  or [`ei_decode_iodata()`](ei.md#ei_decode_iodata).

- **ERL_STRING_EXT** - Decode using either
  [`ei_decode_string()`](ei.md#ei_decode_string), or
  [`ei_decode_iodata()`](ei.md#ei_decode_iodata).

- **ERL_MAP_EXT** - Decode using
  [`ei_decode_map_header()`](ei.md#ei_decode_map_header).

- **ERL_PID_EXT** - Decode using [`ei_decode_pid()`](ei.md#ei_decode_pid).

- **ERL_PORT_EXT** - Decode using [`ei_decode_port()`](ei.md#ei_decode_port).

- **ERL_NEW_REFERENCE_EXT** - Decode using
  [`ei_decode_ref()`](ei.md#ei_decode_ref).

- **ERL_SMALL_TUPLE_EXT, ERL_LARGE_TUPLE_EXT**  
  Decode using [`ei_decode_tuple_header()`](ei.md#ei_decode_tuple_header).

Instead of decoding a term you can also skipped past it if you are not
interested in the data by usage of [`ei_skip_term()`](ei.md#ei_skip_term).

## ei_init()

```c
int ei_init(void);
```

Initialize the `ei` library. This function should be called once (and only once)
before calling any other functionality in the `ei` library.

On success zero is returned. On failure a posix error code is returned.

Available since OTP 21.3

## ei_print_term()

## ei_s_print_term()

```c
int ei_print_term(FILE* fp, const char* buf, int* index);
```

```c
int ei_s_print_term(char** s, const char* buf, int* index);
```

Prints a term, in clear text, to the file specified by `fp`, or the buffer
pointed to by `s`. It tries to resemble the term printing in the Erlang shell.

In `ei_s_print_term()`, parameter `s` is to point to a dynamically (malloc)
allocated string of `BUFSIZ` bytes or a `NULL` pointer. The string can be
reallocated (and `*s` can be updated) by this function if the result is more
than `BUFSIZ` characters. The string returned is `NULL`\-terminated.

The return value is the number of characters written to the file or string, or
`-1` if `buf[index]` does not contain a valid term. Unfortunately, I/O errors on
`fp` is not checked.

Argument `index` is updated, that is, this function can be viewed as a decode
function that decodes a term into a human-readable format.

## ei_set_compat_rel()

```c
void ei_set_compat_rel(unsigned release_number);
```

In general, the `ei` library is guaranteed to be compatible with other
Erlang/OTP components that are 2 major releases older or newer than the `ei`
library itself.

Sometimes an exception to the above rule has to be made to make new features (or
even bug fixes) possible. A call to `ei_set_compat_rel(release_number)` sets the
`ei` library in compatibility mode of OTP release `release_number`.

The only useful value for `release_number` is currently `21`. This will only be
useful and have an effect if _bit strings_ or _export funs_ are received from a
connected node. Before OTP 22, bit strings and export funs were not supported by
`ei`. They were instead encoded using an undocumented fallback tuple format when
sent from the emulator to `ei`:

- **`Bit string`** - The term `<<42, 1:1>>` was encoded as `{<<42, 128>>, 1}`.
  The first element of the tuple is a binary and the second element denotes how
  many bits of the last bytes are part of the bit string. In this example only
  the most significant bit of the last byte (128) is part of the bit string.

- **`Export fun`** - The term `fun lists:map/2` was encoded as `{lists,map}`. A
  tuple with the module, function and a missing arity.

If `ei_set_compat_rel(21)` is _not_ called then a connected emulator will send
bit strings and export funs correctly encoded. The functions
[`ei_decode_bitstring`](ei.md#ei_decode_bitstring) and
[`ei_decode_fun`](ei.md#ei_decode_fun) has to be used to decode such terms.
Calling `ei_set_compat_rel(21)` should only be done as a workaround to keep an
old implementation alive, which expects to receive the undocumented tuple
formats for bit strings and/or export funs.

> #### Note {: .info }
>
> If this function is called, it can only be called once and must be called
> before any other functions in the `ei` library are called.

## ei_skip_term()

```c
int ei_skip_term(const char* buf, int* index);
```

Skips a term in the specified buffer; recursively skips elements of lists and
tuples, so that a full term is skipped. This is a way to get the size of an
Erlang term.

`buf` is the buffer.

`index` is updated to point right after the term in the buffer.

> #### Note {: .info }
>
> This can be useful when you want to hold arbitrary terms: skip them and copy
> the binary term data to some buffer.

Returns `0` on success, otherwise `-1`.

## ei_x_append()

## ei_x_append_buf()

```c
int ei_x_append(ei_x_buff* x, const ei_x_buff* x2);
```

```c
int ei_x_append_buf(ei_x_buff* x, const char* buf, int len);
```

Appends data at the end of buffer `x`.

## ei_x_format()

## ei_x_format_wo_ver()

```c
int ei_x_format(ei_x_buff* x, const char* fmt, ...);
```

```c
int ei_x_format_wo_ver(ei_x_buff* x, const char *fmt, ... );
```

Formats a term, given as a string, to a buffer. Works like a sprintf for Erlang
terms. `fmt` contains a format string, with arguments like `~d`, to insert terms
from variables. The following formats are supported (with the C types given):

```text
~a  An atom, char*
~c  A character, char
~s  A string, char*
~i  An integer, int
~l  A long integer, long int
~u  A unsigned long integer, unsigned long int
~f  A float, float
~d  A double float, double float
~p  An Erlang pid, erlang_pid*
```

For example, to encode a tuple with some stuff:

```c
ei_x_format("{~a,~i,~d}", "numbers", 12, 3.14159)
encodes the tuple {numbers,12,3.14159}
```

`ei_x_format_wo_ver()` formats into a buffer, without the initial version byte.

> #### Change {: .info }
>
> Since OTP 26.2 maps can be encoded with syntax like `"#{k1 => v1, k2 => v2}"`.

## ei_x_free()

```c
int ei_x_free(ei_x_buff* x);
```

Deallocates the dynamically allocated content of the buffer referred by `x`.
After deallocation, the `buff` field is set to `NULL`.

## ei_x_new()

## ei_x_new_with_version()

```c
int ei_x_new(ei_x_buff* x);
```

```c
int ei_x_new_with_version(ei_x_buff* x);
```

Initialize the dynamically realizable buffer referred to by `x`. The fields of
the structure pointed to by parameter `x` is filled in, and a default buffer is
allocated. `ei_x_new_with_version()` also puts an initial version byte, which is
used in the binary format (so that `ei_x_encode_version()` will not be needed.)

## Debug Information

Some tips on what to check when the emulator does not seem to receive the terms
that you send:

- Be careful with the version header, use `ei_x_new_with_version()` when
  appropriate.
- Turn on distribution tracing on the Erlang node.
- Check the result codes from `ei_decode_-calls`.
