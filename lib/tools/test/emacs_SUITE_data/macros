%% -*- Mode: erlang; indent-tabs-mode: nil -*-
%% Copyright Ericsson AB 2017. All Rights Reserved.

%%% Macros should be indented as code

-define(M0, ok).

-define(M1,
        case X of
            undefined -> error;
            _ -> ok
        end).

-define(M2(M2A1,
           M2A2),
        func(M2A1,
             M2A2)
       ).

-define(
   M3,
   undefined
  ).

-ifdef(DEBUG).
-define(LOG,
        logger:log(?MODULE,?LINE)
       ).
-else().
-define(LOG, ok).
-endif().
