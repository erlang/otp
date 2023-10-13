-module(import_type).
-import_type(export_type, [my_binary/0]).
-export([use_my_binary/1]).

-spec use_my_binary(my_binary()) -> my_binary().
use_my_binary(MB) -> MB.
