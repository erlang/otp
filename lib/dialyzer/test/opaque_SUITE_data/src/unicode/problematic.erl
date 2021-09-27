-module(problematic).

-opaque '🐛'() :: bug.
-export_type(['🐛'/0]).

-export(['🔍'/1]).

-spec '🔍'('🐛'()) -> true.
'🔍'(bug) -> true.
