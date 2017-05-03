-module(unused_unknown_type).

-export_type([unused/0]).

-type unused() :: unknown:type1().

-record(unused_rec, {a :: unknown:type2()}).

-record(rec, {a}).
-type unused_rec() :: #rec{a :: unknown:type3()}.
