-module(cover_inc).
-compile(export_all).
-include("cover_inc.hrl").

func() ->
    func1(),
    ok.

