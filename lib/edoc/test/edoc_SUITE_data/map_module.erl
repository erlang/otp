-module(map_module).

-export([foo1/1,foo2/3,start_child/2]).

%% @type wazzup() = integer()
%% @type some_type() = map()
%% @type some_other_type() = {a, #{ list() => term()}}

-type some_type() :: map().
-type some_other_type() :: {'a', #{ list() => term()} }.
-type wazzup() :: integer().

-spec foo1(Map :: #{ 'a' => integer(), 'b' => term()}) -> term().

%% @doc Gets value from map.

foo1(#{ a:= 1, b := V}) -> V.

%% @spec foo2(some_type(), Type2 :: some_other_type(), map()) -> Value
%% @doc Gets value from map.

-spec foo2(
    Type1 :: some_type(),
    Type2 :: some_other_type(),
    Map :: #{ get => 'value', 'value' => binary()}) -> binary().

foo2(Type1, {a,#{ "a" := _}}, #{get := value, value := B}) when is_map(Type1) -> B.

%% from supervisor 18.0

-type child()    :: 'undefined' | pid().
-type child_id() :: term().
-type mfargs()   :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type modules()  :: [module()] | 'dynamic'.
-type restart()  :: 'permanent' | 'transient' | 'temporary'.
-type shutdown() :: 'brutal_kill' | timeout().
-type worker()   :: 'worker' | 'supervisor'.
-type sup_ref()  :: (Name :: atom())
                  | {Name :: atom(), Node :: node()}
                  | {'global', Name :: atom()}
                  | {'via', Module :: module(), Name :: any()}
                  | pid().
-type child_spec() :: #{name => child_id(),     % mandatory
			start => mfargs(),      % mandatory
			restart => restart(),   % optional
			shutdown => shutdown(), % optional
			type => worker(),       % optional
			modules => modules()}   % optional
                    | {Id :: child_id(),
                       StartFunc :: mfargs(),
                       Restart :: restart(),
                       Shutdown :: shutdown(),
                       Type :: worker(),
                       Modules :: modules()}.

-type startchild_err() :: 'already_present'
			| {'already_started', Child :: child()} | term().
-type startchild_ret() :: {'ok', Child :: child()}
                        | {'ok', Child :: child(), Info :: term()}
			| {'error', startchild_err()}.


-spec start_child(SupRef, ChildSpec) -> startchild_ret() when
      SupRef :: sup_ref(),
      ChildSpec :: child_spec() | (List :: [term()]).
start_child(Supervisor, ChildSpec) ->
    {Supervisor,ChildSpec}.
