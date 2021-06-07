-module(contracts_with_subtypes).

-compile(export_all).

%===============================================================================

-spec extract() -> 'ok'.

extract() ->
   case dz_extract() of
       {ok, Val} -> Val;
       error -> exit(boom)
   end.

-spec dz_extract() -> RetValue when
     FileList :: something,
     RetValue :: {ok, FileList} | error.

dz_extract() -> get(foo).

%-------------------------------------------------------------------------------

-spec extract2() -> 'ok'.

extract2() ->
   case dz_extract2() of
       {ok, Val} -> Val;
       error -> exit(boom)
   end.

-spec dz_extract2() -> RetValue when
     RetValue :: {ok, FileList} | error,
     FileList :: something.

dz_extract2() -> get(foo).

%===============================================================================

-spec foo1(Arg1) -> Res when
      Arg1 :: atom(),
      Res  :: atom().

foo1(X) -> X.

-spec foo2(Arg1) -> Res when
      Arg1 :: Arg2,
      Arg2 :: atom(),
      Res  :: atom().

foo2(X) -> X.

-spec foo3(Arg1) -> Res when
      Arg2 :: atom(),
      Arg1 :: Arg2,
      Res  :: atom().

foo3(X) -> X.

-spec foo4(Type) -> Type when is_subtype(Type, atom()).

foo4(X) -> X.

-spec foo5(Type :: atom()) -> Type :: atom().

foo5(X) -> X.

-spec foo6(Type) -> Type when Type :: atom().

foo6(X) -> X.

-spec foo7(Type) -> Type.

foo7(X) -> X.

%-------------------------------------------------------------------------------

bar(1) -> foo1(5);
bar(2) -> foo2(5);
bar(3) -> foo3(5);
bar(4) -> foo4(5);
bar(5) -> foo5(5);
bar(6) -> foo6(5);
bar(7) -> foo7(5).

wrong_foo6() ->
    b = foo6(a).

%===============================================================================

-spec rec_arg(Arg) -> ok when
      Arg :: {a, A} | {b, B},
      A   :: a | {b, B},
      B   :: b | {a, A}.

rec_arg(X) -> get(X).

c(aa) -> rec_arg({a, a});
c(bb) -> rec_arg({b, b});
c(abb) -> rec_arg({a, {b, b}});
c(baa) -> rec_arg({b, {a, a}});
c(abaa) -> rec_arg({a, {b, {a, a}}});
c(babb) -> rec_arg({b, {a, {b, b}}});
c(ababb) -> rec_arg({a, {b, {a, {b, b}}}});
c(babaa) -> rec_arg({b, {a, {b, {a, a}}}}).

w(ab) -> rec_arg({a, b}); % breaks the contract
w(ba) -> rec_arg({b, a}); % breaks the contract
w(aba) -> rec_arg({a, {b, a}}); % no longer breaks the contract
w(bab) -> rec_arg({b, {a, b}}); % breaks the contract
w(abab) -> rec_arg({a, {b, {a, b}}}); % no longer breaks the contract
w(baba) -> rec_arg({b, {a, {b, a}}}); % no longer breaks the contract
w(ababa) -> rec_arg({a, {b, {a, {b, a}}}});
w(babab) -> rec_arg({b, {a, {b, {a, b}}}}).

%% For comparison: the same thing with types

-type ab() :: {a, a()} | {b, b()}.
-type a() :: a | {b, b()}.
-type b() :: b | {a, a()}.

-spec rec2(Arg) -> ok when
      Arg :: ab().

rec2(X) -> get(X).

d(aa) -> rec2({a, a});
d(bb) -> rec2({b, b});
d(abb) -> rec2({a, {b, b}});
d(baa) -> rec2({b, {a, a}});
d(abaa) -> rec2({a, {b, {a, a}}});
d(babb) -> rec2({b, {a, {b, b}}});
d(ababb) -> rec2({a, {b, {a, {b, b}}}});
d(babaa) -> rec2({b, {a, {b, {a, a}}}}).

q(ab) -> rec2({a, b}); % breaks the contract
q(ba) -> rec2({b, a}); % breaks the contract
q(aba) -> rec2({a, {b, a}}); % breaks the contract
q(bab) -> rec2({b, {a, b}}); % breaks the contract
q(abab) -> rec2({a, {b, {a, b}}}); % breaks the contract
q(baba) -> rec2({b, {a, {b, a}}}); % breaks the contract
q(ababa) -> rec2({a, {b, {a, {b, a}}}}); % breaks the contract
q(babab) -> rec2({b, {a, {b, {a, b}}}}); % breaks the contract
q(ababab) -> rec2({a, {b, {a, {b, {a, b}}}}});
q(bababa) -> rec2({b, {a, {b, {a, {b, a}}}}});
q(abababa) -> rec2({a, {b, {a, {b, {a, {b, a}}}}}});
q(bababab) -> rec2({b, {a, {b, {a, {b, {a, b}}}}}}).

%===============================================================================

-type dublo(X) :: {X, X}.

-type weird(X,Y) :: {X, Y, X, X}.

-spec forfun(dublo(Var)) -> ok when Var :: atom().

forfun(_) -> ok.

-spec forfun2(weird(Var, Var)) -> ok when Var :: atom().

forfun2(_) -> ok.

%===============================================================================

-spec shallow(X) -> {ok, X} | {ok, X, file:filename()} | err1 | err2.

shallow(X) -> get(X).

st(X) when is_atom(X) ->
    case shallow(X) of
	err1 -> ok;
	err2 -> ok;
	{ok, X} -> ok;
	{ok, X, Res} ->
	    case Res of
		1      -> bad;
		_Other -> ok
	    end;
	alpha -> bad;
	{ok, 42} -> ok;
	42 -> bad
    end.

%-------------------------------------------------------------------------------

-spec deep(X) -> Ret when
      Ret :: {ok, X} | Err,
      Err :: err1 | err2.

deep(X) -> get(X).

dt(X) when is_atom(X) ->
    case deep(X) of
	err1 -> ok;
	err2 -> ok;
	{ok, X} -> ok;
	alpha -> bad;
	{ok, 42} -> ok;
	42 -> bad
    end.

%-------------------------------------------------------------------------------

-type local_errors() :: err1 | err2.

-spec deep2(X) -> Ret when
      Ret :: {ok, X} | Err,
      Err :: local_errors().

deep2(X) -> get(X).

dt2(X) when is_atom(X) ->
    case deep2(X) of
	err1 -> ok;
	err2 -> ok;
	{ok, X} -> ok;
	alpha -> bad;
	{ok, 42} -> ok;
	42 -> bad
    end.

%-------------------------------------------------------------------------------

-spec deep3(X) -> Ret when
      Ret :: {ok, X, file:filename()} | Err,
      Err :: local_errors().

deep3(X) -> get(X).

dt3(X) when is_atom(X) ->
    case deep3(X) of
	err1 -> ok;
	err2 -> ok;
	{ok, X, Res} ->
	    case Res of
		1      -> bad;
		_Other -> ok
	    end;
	{ok, X} -> bad;
	alpha -> bad;
	{ok, 42} -> bad;
	42 -> bad
    end.

%===============================================================================

-spec flat_ets_new(Name, Options) -> atom() when
      Name :: atom(),
      Options :: [Option],
      Option :: set
              | ordered_set
              | bag
              | duplicate_bag
              | public
              | protected
              | private
              | named_table
              | {keypos, integer()}
              | {heir, pid(), term()}
              | {heir, none}
              | {write_concurrency, boolean()}
              | {read_concurrency, boolean()}
              | compressed.

flat_ets_new(Name, Options) ->
    get({Name, Options}).

flat_ets_new_t() ->
    flat_ets_new(12,[]),
    flat_ets_new({a,b},[]),
    flat_ets_new(name,[foo]),
    flat_ets_new(name,{bag}),
    flat_ets_new(name,bag),
    ok.

-type access()     :: public | protected | private.
-type type()       :: set | ordered_set | bag | duplicate_bag.

-spec factored_ets_new(Name, Options) -> atom() when
      Name :: atom(),
      Options :: [Option],
      Option :: Type | Access | named_table | {keypos,Pos}
              | {heir, Pid :: pid(), HeirData} | {heir, none} | Tweaks,
      Type :: type(),
      Access :: access(),
      Tweaks :: {write_concurrency, boolean()}
              | {read_concurrency, boolean()}
              | compressed,
      Pos :: pos_integer(),
      HeirData :: term().

factored_ets_new(Name, Options) ->
    get({Name, Options}).

factored_ets_new_t() ->
    factored_ets_new(12,[]),
    factored_ets_new({a,b},[]),
    factored_ets_new(name,[foo]),
    factored_ets_new(name,{bag}),
    factored_ets_new(name,bag),
    ok.
