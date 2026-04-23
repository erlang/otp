# ERL-0005 - Illegal Base

## Example

```erlang
%% foo.erl
-module(foo).
t1() -> 1#000.
```

```
$ erlc foo.erl
foo.erl:2:9: illegal base '1'
%    2| t1() -> 1#000.
%     |         ^
% help: call `erlc -explain ERL-0005` to see a detailed explanation
```

## Explanation

This error message is emitted by the Erlang scanner when an illegal base
is used in a base-specified integer literal. The [Reference manual](`e:system:ref_man_data_types`)
has the following requirements:

> `base#digits`: Integer with the base `base`, which must be an integer
in the range 2 through 36. `digits` are `0-9` plus letters `A-Z` (upper
or lower case). This notation can also be found in the Ada programming
language. Erlang does not support prefixes such as `0x` for hexadecimal
or `077` for octal.


For example, the following integer literals are valid:

```erlang
16#1A3F  % Hexadecimal number
2#1011  % Binary number
10#1234 % Decimal number
```

This error occurs if:

- The base value is invalid (less than 2 or greater than 36).
- The digits in the number are invalid for the specified base (e.g., digit
2 in base 2).
- The base is malformed or includes underscores in invalid positions.

In the example above, `1#000` is invalid because base 1 is not allowed.

For many cases, the scanner also raises a [ERL-0007: syntax error](ERL-0007-syntax-error.md)
in the meantime.

To fix this error, ensure that the integer literal has a valid base.


