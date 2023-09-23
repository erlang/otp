% Deals with output formatting for the terminal.

-module(ct_console).
-export([print_header/1, print_results/1, pluralize/3]).

%% Colored output formatting characters
% If adding a new format here, make sure to add it to `size_on_terminal/1`.
-define(TERM_BOLD, "\033[;1m").
-define(TERM_BOLD_GREEN, "\033[;1;32m").
-define(TERM_BLACK, "\033[;30m").
-define(TERM_RED, "\033[;31m").
-define(TERM_GREEN, "\033[;32m").
-define(TERM_YELLOW, "\033[;33m").
-define(TERM_CLEAR, "\033[0m").

-spec print_results(map()) -> ok.
print_results(#{results := [_ | _]} = Results) ->
    {FailedTestResults, ResultsWithoutFailures} = maps:take(results, Results),
    print_header("failed tests follow", mc(?TERM_RED)),
    print_failed_test_results(FailedTestResults),
    print_results(ResultsWithoutFailures);

print_results(#{total := #{passed := OkN, failed := FailedN,
                           user_skipped := UserSkipN, auto_skipped := AutoSkipN},
                elapsed := Elapsed}) ->
    AllSkippedN = UserSkipN + AutoSkipN,
    PassedStr = format_if_nonzero(OkN, "~s~w passed~s", [mc(?TERM_BOLD_GREEN), OkN, ?TERM_CLEAR]),
    SkipStr = format_if_nonzero(AllSkippedN, "~s~w skipped~s", [mc(?TERM_YELLOW), AllSkippedN, ?TERM_CLEAR]),
    FailedStr = format_if_nonzero(FailedN, "~s~w failed~s", [mc(?TERM_RED), FailedN, ?TERM_CLEAR]),
    NonemptyStrs = lists:filter(fun(Item) -> Item =/= "" end, [PassedStr, SkipStr, FailedStr]),
    TimeDescription = format_time(Elapsed),
    PaddingColor = result_padding_color(OkN, FailedN),
    FormattedTimeDescription = io_lib:format("~s in ~s~s", [PaddingColor, TimeDescription, ?TERM_CLEAR]),
    ResultStr = lists:join(", ", NonemptyStrs) ++ FormattedTimeDescription,
    ResultSize = size_on_terminal(ResultStr),
    {PaddingSizeLeft, PaddingSizeRight} = centering_padding_size(ResultSize),
    {PaddingLeft, PaddingRight} = padding_characters(PaddingSizeLeft, PaddingSizeRight),
    io:fwrite(
        user,
        "~s~ts~s~ts~s~ts~s~n",
        [PaddingColor, PaddingLeft, ?TERM_CLEAR, ResultStr, PaddingColor, PaddingRight, ?TERM_CLEAR]
    ).


print_failed_test_results([#{reason := Reason, module := Module, function := Function} | Rest]) ->
    io:fwrite(user, "=> test case ~s:~s:~n", [Module, Function]),
    {CrashReason, Traceback} = format_failure_reason(Reason),
    io:fwrite(user, "~tp~n~tp~n~n", [CrashReason, elide_framework_code(Traceback)]),
    print_failed_test_results(Rest);

print_failed_test_results([]) ->
    ok.

format_failure_reason({'EXIT', Reason, Traceback}) -> {{'EXIT', Reason}, Traceback};
format_failure_reason({_Reason, _Traceback} = Result) -> Result.

-spec result_padding_color(non_neg_integer(), non_neg_integer()) -> string().
result_padding_color(_Ok, 0) -> mc(?TERM_GREEN);
result_padding_color(_Ok, _Failed) -> mc(?TERM_RED).

-spec format_if_nonzero(non_neg_integer(), io_lib:format(), [term()]) -> string().
format_if_nonzero(0, _Format, _Data) -> "";
format_if_nonzero(_, Format, Data) -> io_lib:format(Format, Data).

-spec print_header(string()) -> ok.
print_header(Message) ->
    print_header(Message, mc(?TERM_BOLD)).

-spec print_header(string(), string()) -> ok.
print_header(Message, StartingColor) ->
    io:fwrite(user, "~s", [format_header(Message, StartingColor)]).

-spec format_header(string(), string()) -> string().
format_header(Message, StartingColor) ->
    {PaddingSizeLeft, PaddingSizeRight} = centering_padding_size(iolist_size(Message)),
    % shell_docs contains a lot of useful functions that we could maybe factor
    % out and use here.
    Stop = ?TERM_CLEAR,
    {PaddingLeft, PaddingRight} = padding_characters(PaddingSizeLeft, PaddingSizeRight),
    io_lib:format("~s~ts~s~ts~s~n", [StartingColor, PaddingLeft, Message, PaddingRight, Stop]).


-spec padding_characters(integer(), integer()) -> {string(), string()}.
% Dialyzer said this can't happen
%padding_characters(SizeLeft, _SizeRight) when SizeLeft < 0 ->
%    % Not enough space to print the padding, proceed normally.
%    {"", ""};
padding_characters(SizeLeft, SizeRight) ->
    Left = lists:duplicate(SizeLeft, "="),
    Right = lists:duplicate(SizeRight, "="),
    {Left ++ " ", [" " | Right]}.

-spec centering_padding_size(integer()) -> {integer(), integer()}.
centering_padding_size(MessageSize) ->
    {ok, Columns} = terminal_width(user),
    PaddingSizeLeft = trunc(Columns / 2) - trunc(MessageSize / 2) - 1,
    PaddingSizeRight = header_right_padding(Columns, PaddingSizeLeft, MessageSize),
    {PaddingSizeLeft, PaddingSizeRight}.


-spec pluralize(non_neg_integer(), string()) -> string().
pluralize(Amount, Singular) ->
    pluralize(Amount, Singular, Singular ++ "s").

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
header_right_padding(_Columns, LeftPadding, MessageSize) when MessageSize rem 2 == 1 ->
    LeftPadding - 1;
header_right_padding(Columns, LeftPadding, _MessageSize) when Columns rem 2 == 0 ->
    LeftPadding;
header_right_padding(_Columns, LeftPadding, _MessageSize) ->
    LeftPadding + 1.

-spec size_on_terminal(iolist()) -> non_neg_integer().
size_on_terminal(?TERM_BOLD ++ Rest) -> size_on_terminal(Rest);
size_on_terminal(?TERM_BOLD_GREEN ++ Rest) -> size_on_terminal(Rest);
size_on_terminal(?TERM_BLACK ++ Rest) -> size_on_terminal(Rest);
size_on_terminal(?TERM_RED ++ Rest) -> size_on_terminal(Rest);
size_on_terminal(?TERM_GREEN ++ Rest) -> size_on_terminal(Rest);
size_on_terminal(?TERM_YELLOW ++ Rest) -> size_on_terminal(Rest);
size_on_terminal(?TERM_CLEAR ++ Rest) -> size_on_terminal(Rest);
size_on_terminal([Items | Rest]) when is_list(Items) -> size_on_terminal(Items) + size_on_terminal(Rest);
size_on_terminal([Char | Rest]) when is_integer(Char) -> 1 + size_on_terminal(Rest);
size_on_terminal([]) -> 0.

%% @doc Elide framework code from the given traceback.
-spec elide_framework_code(list()) -> list().
elide_framework_code([{test_server, _Function, _Location} | Rest]) ->
    elide_framework_code(Rest);
elide_framework_code([Frame | Rest]) ->
    [Frame | elide_framework_code(Rest)];
elide_framework_code([]) ->
    [].


%% @doc Turn the given seconds into a human-readable string.
-spec format_time(non_neg_integer()) -> string().
format_time(Seconds) ->
    Minutes = trunc(Seconds / 60),
    RemainingSeconds = Seconds rem 60,
    case Minutes of
        0 ->
            io_lib:format("~w ~s", [Seconds, pluralize(Seconds, "second")]);
        _ ->
            io_lib:format("~w ~s and ~w ~s", [Minutes, pluralize(Minutes, "minute"),
                                              RemainingSeconds, pluralize(RemainingSeconds, "second")])
    end.


%% @doc
%% Return the given string if colorized output is wanted, else an empty string.
%% `mc' => "maybe color".
%% @end
-spec mc(string()) -> string().
mc(Color) ->
    case os:getenv("NO_COLOR") of
        false ->
            Color;
        Value when Value =/= "" ->
            ""
    end.
