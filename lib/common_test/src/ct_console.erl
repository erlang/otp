% Deals with output formatting for the terminal.

-module(ct_console).
-export([print_header/1, pluralize/3]).


-spec print_header(string()) -> ok.
print_header(Message) ->
    io:fwrite(user, "~s", [format_header(Message)]).

-spec format_header(string()) -> string().
format_header(Message) ->
    {ok, Columns} = terminal_width(user),
    MessageLength = iolist_size(Message),
    PaddingSizeLeft = trunc(Columns / 2) - trunc(MessageLength / 2) - 1,
    PaddingSizeRight = header_right_padding(Columns, PaddingSizeLeft, MessageLength),
    % shell_docs contains a lot of useful functions that we could maybe factor
    % out and use here.
    Start = "\033[;1m",
    Stop = "\033[0m",
    case PaddingSizeLeft of
        Amount when Amount < 0 ->
            % Not enough space to print the padding, proceed normally.
            io_lib:format("~s~s~s~n", [Start, Message, Stop]);
        _Amount ->
            PaddingLeft = lists:duplicate(PaddingSizeLeft, "="),
            PaddingRight = lists:duplicate(PaddingSizeRight, "="),
            io_lib:format("~s~s ~s ~s~s~n", [Start, PaddingLeft, Message, PaddingRight, Stop])
    end.


-spec pluralize(non_neg_integer(), string(), string()) -> string().
pluralize(1, Singular, _Plural) -> Singular;
pluralize(_, _Singular, Plural) -> Plural.


-spec terminal_width(atom()) -> {ok, pos_integer()}.
terminal_width(Driver) ->
    case io:columns(Driver) of
        {ok, _Columns} = Result ->
            Result;
        {error, enotsup} ->
            {ok, 80}
    end.

-spec header_right_padding(pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
header_right_padding(_Columns, LeftPadding, MessageLength) when MessageLength rem 2 == 1 ->
    LeftPadding - 1;
header_right_padding(Columns, LeftPadding, _MessageLength) when Columns rem 2 == 0 ->
    LeftPadding;
header_right_padding(_Columns, LeftPadding, _MessageLength) ->
    LeftPadding + 1.
