# ERL-1400 - Format error

## Example

```erlang
main() ->
    io:format("These are two arguments: ~p, ~p", [only_one]).
```

```

```

## Explanation

The warning occurs when the format string and the actual list of parameters in
a `io:format/2`, `io_lib:format/2` or equivalent function are inconsistent. In
the example above, this happens because the format string (the first of the
two arguments to the `io:format/2` function) contains **two** control
sequences for formatting (`~p`), but the list of arguments only contains one
element.

To learn more about formatting strings and control sequences, please refer to
the official documentation for the `io:fwrite/3` function.