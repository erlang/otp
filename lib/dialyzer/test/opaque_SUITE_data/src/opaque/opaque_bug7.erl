-module(opaque_bug7).

-export([loop/1]).
-export_type([unused_dummy/0]).

-type other_type() :: opaque_bug7_adt:adt().

%% Required for decorate/3 to be called, jarring the bug loose. Does not need
%% to be used anywhere.
-opaque unused_dummy() :: {pid(), binary()}.

-spec loop(other_type()) -> no_return().
loop(OtherType) ->
    receive
        _X ->
            loop(OtherType)
    after timer:minutes(30) ->
            opaque_bug7_adt:do(OtherType)
    end.
