# ERL-1113 - Field already defined

## Example

```erlang
%% foo.erl
-module(foo).
-export([t/0]).
-record(r, {a,b}).
t() -> #r{a=foo,b=42,a=bar}.
```

```
$ erlc foo.erl
foo.erl:4:22: field a already defined in record r
%    4| t() -> #r{a=foo,b=42,a=bar}.
%     |                      ^
% help: call `erlc -explain ERL-1113` to see a detailed explanation
```

## Explanation

This error occurs when the Erlang linter sees a record creation or update
that tries to define a field more than once. Each field may appear at most
once in a record creation or update.

To fix this error, remove the duplicated field assignment so each field
appears at most once. Decide on which value should be set for each field.
Check typos to ensure that field names are not misspelled.

To learn more about records, please refer to the [Reference Manual](`e:system:ref_man_records`).
