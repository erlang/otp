%%-----------------------------------------------------------------------
%% Program that gave erroneous warnings due to missing information about
%% the {encoding, latin1 | unicode | utf8 | ...} option of file:open/3.
%%-----------------------------------------------------------------------
-module(file_open_encoding).

-export([parse/1]).

-spec parse(string()) -> proplists:proplist().
parse(FileName) ->
  {ok, IoDevice} = file:open(FileName, [read, binary, {encoding, utf8}]),
  do_parse(IoDevice, []).

do_parse(IoDevice, ResultSoFar) ->
  case io:get_line(IoDevice, "") of
    eof ->
      file:close(IoDevice),
      ResultSoFar;
    <<"--"/utf8, _Comment/binary>> ->
      do_parse(IoDevice, ResultSoFar);
    Line ->
      [Key, Value] = binary:split(Line, [<<": ">>, <<"\n">>], [global, trim]),
      do_parse(IoDevice, [{binary_to_atom(Key, utf8), Value} | ResultSoFar])
 end.
