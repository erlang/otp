# ERL-0021 - Unsupported constraint

## Example

```erlang
%% foo.erl
-module(foo).
-spec t(X) -> X when is_subtype(integer()).
```

```
$ erlc foo.erl
foo.erl:2:22: unsupported constraint is_subtype
%    2| -spec t(X) -> X when is_subtype(integer()).
%     |                      ^
% help: call `erlc -explain ERL-0021` to see a detailed explanation
```

## Explanation

Legacy error before OTP 19.0. Remove it?