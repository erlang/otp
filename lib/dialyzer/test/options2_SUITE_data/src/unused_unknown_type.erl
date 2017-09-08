-module(unused_unknown_type).

-export([t/0]).

-export_type([unused/0]).

-export_type([wide/0, deep/0]).
-export_type([owide/0, odeep/0]).
-export_type([arg/0, rargs1/0, rargs2/0]).

-type unused() :: unknown:type1().

-record(unused_rec,
        {a :: unknown:type2(),
         b :: {{{{{{{{{{{{{{{{{{{{rfoo:rbar()}}}}}}}}}}}}}}}}}}}}}).

-record(rec, {a}).
-type unused_rec() :: #rec{a :: unknown:type3()}.

-type wide() :: {a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,xxx:y()}.
-type owide() :: {a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,owww:y()}.

%% Deeper than the hardcoded limit in erl_types.erl of 16.
-type deep() :: {{{{{{{{{{{{{{{{{{{{foo:bar()}}}}}}}}}}}}}}}}}}}}.
-type odeep() :: {{{{{{{{{{{{{{{{{{{{ofoo:obar()}}}}}}}}}}}}}}}}}}}}.

-type arg1(A) :: [A].
-type arg() :: arg1({a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,yyy:x()}).

%% No warning about www:x/0 because parameters are currently not
%% handled if the parameterized type cannot be found.
-type rargs1() :: zzz:arg({a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,www:x()}).

-type rargs2() :: dict:dict({a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,zzz:x()},
                            any()).

%% No warning. The check is commented out as it takes too long.
-spec t() -> 'a' | {{{{{{{{{{{{{{{{{{{{sfoo:sbar()}}}}}}}}}}}}}}}}}}}}.
t() ->
    a.
