Pseudo function that by a `parse_transform` translates `LiteralFun` typed as
parameter in the function call to a [match specification](`m:ets#match_spec`).
With "literal" is meant that the fun must textually be written as the parameter
of the function, it cannot be held in a variable that in turn is passed to the
function.

The parse transform is provided in the `ms_transform` module and the source
_must_ include file `ms_transform.hrl` in STDLIB for this pseudo function to
work. Failing to include the hrl file in the source results in a runtime error,
not a compile time error. The include file is easiest included by adding line
`-include_lib("stdlib/include/ms_transform.hrl").` to the source file.

The fun is very restricted, it can take only a single parameter (the object to
match): a sole variable or a tuple. It must use the `is_` guard tests. Language
constructs that have no representation in a match specification (`if`, `case`,
`receive`, and so on) are not allowed.

The return value is the resulting match specification.

_Example:_

```erlang
1> ets:fun2ms(fun({M,N}) when N > 3 -> M end).
[{{'$1','$2'},[{'>','$2',3}],['$1']}]
```

Variables from the environment can be imported, so that the following works:

```erlang
2> X=3.
3
3> ets:fun2ms(fun({M,N}) when N > X -> M end).
[{{'$1','$2'},[{'>','$2',{const,3}}],['$1']}]
```

The imported variables are replaced by match specification `const` expressions,
which is consistent with the static scoping for Erlang funs. However, local or
global function calls cannot be in the guard or body of the fun. Calls to
built-in match specification functions is of course allowed:

```erlang
4> ets:fun2ms(fun({M,N}) when N > X, my_fun(M) -> M end).
Error: fun containing local Erlang function calls
('my_fun' called in guard) cannot be translated into match_spec
{error,transform_error}
5> ets:fun2ms(fun({M,N}) when N > X, is_atom(M) -> M end).
[{{'$1','$2'},[{'>','$2',{const,3}},{is_atom,'$1'}],['$1']}]
```

As shown by the example, the function can be called from the shell also. The fun
must be literally in the call when used from the shell as well.

> #### Warning {: .warning }
>
> If the `parse_transform` is not applied to a module that calls this pseudo
> function, the call fails in runtime (with a `badarg`). The `ets` module
> exports a function with this name, but it is never to be called except when
> using the function in the shell. If the `parse_transform` is properly applied
> by including header file `ms_transform.hrl`, compiled code never calls the
> function, but the function call is replaced by a literal match specification.

For more information, see [`ms_transform`](`m:ms_transform`).
