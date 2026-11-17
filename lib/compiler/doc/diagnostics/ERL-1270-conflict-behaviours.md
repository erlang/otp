# ERL-1270 - Conflicting behaviours

## Example

```erlang
%% foo.erl
-module(foo).
-behaviour(gen_server).
-behaviour(supervisor).
-export([handle_call/3,handle_cast/2,handle_info/2]).
handle_call(_, _, _) -> ok.
handle_cast(_, _) -> ok.
handle_info(_, _) -> ok.
```

```
$ erlc foo.erl
foo.erl:3:5: a literal string in a binary pattern must not have a type or a size
%    3| t(<<"abc"/binary>>) -> ok.
%     |     ^
% help: call `erlc -explain ERL-1247` to see a detailed explanation
```

## Explanation