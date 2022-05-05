%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2022. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(erl_features).

-export([all/0,
         info/1,
         short/1,
         long/1,
         enabled/0,
         load_allowed/1,
         keywords/0,
         keywords/1,
         keyword_fun/2,
         keyword_fun/4,
         used/1,
         format_error/1,
         format_error/2]).

-type type() :: 'extension' | 'backwards_incompatible_change'.
-type status() :: 'experimental'
                  | 'approved'
                  | 'permanent'
                  | 'rejected'.
-type release() :: non_neg_integer().
-type feature() :: atom().
-type error() :: {?MODULE, {'invalid_features', [atom()]}}.

-define(VALID_FEATURE(Feature),
        (case is_valid(Feature) of
             false ->
                 error(invalid_feature, [Feature],
                       [{error_info,
                         #{module => ?MODULE,
                           cause => #{1 => "unknown feature"}}}]);
             true -> ok
         end)).

%% Specification about currently known features.
feature_specs() ->
    #{maybe_expr =>
          #{short => "Value based error handling (EEP49)",
            description =>
                "Implementation of the maybe expression proposed in EEP49 -- "
            "Value based error handling.",
            status => experimental,
            experimental => 25,
            keywords => ['maybe', 'else'],
            type => extension}}.

%% Return all currently known features.
-spec all() -> [feature()].
all() ->
    Map = case persistent_term:get({?MODULE, feature_specs}, none) of
              none -> init_specs();
              M -> M
          end,
    maps:keys(Map).

is_valid(Ftr) ->
    lists:member(Ftr, all()).

-spec short(feature()) -> iolist() | no_return().
short(Feature) ->
    #{short := Short,
      status := Status} = Info = info(Feature),
    #{Status := Release} = Info,
    io_lib:format("~-40s ~-12s (~p)", [Short, Status, Release]).

-spec long(feature()) -> iolist() | no_return().
long(Feature) ->
    #{short := Short,
      description := Description,
      status := Status,
      keywords := Keywords,
      type := Type} = Info = info(Feature),
    StatusFmt = "  ~-10s ~-12s (~p)\n",
    History = [io_lib:format(StatusFmt, [T, S, R])
               || {T, S, R} <- history(Status, Info)],
    KeywordsStrs =
        if Keywords == [] -> "";
           true ->
                io_lib:format("  ~-10s ~p\n", ["Keywords", Keywords])
        end,
    Lines = [{"~s - ~s\n", [Feature, Short]},
             {"  ~-10s ~s\n", ["Type", Type]},
             {"~s", [History]},
             {"~s", [KeywordsStrs]},
             {"\n~s\n", [nqTeX(Description)]}],
    [io_lib:format(FStr, Args) || {FStr, Args} <- Lines].

history(Current, Info) ->
    G = fun(Key, S) ->
                case maps:find(Key, Info) of
                    error -> [];
                    {ok, R} -> [{S, Key, R}]
                end
        end,
    F = fun(Key) -> G(Key, "") end,
    History =
        case Current of
            experimental -> [];
            rejected -> F(experimental);
            approved -> F(experimental);
            permanent -> F(approved) ++ F(experimental)
        end,
    G(Current, "Status") ++ History.

%% Dead simple line breaking for better presentation.
nqTeX(String) ->
    Words = string:tokens(String, " "),
    WithLens = lists:map(fun(W) -> {W, length(W)} end, Words),
    adjust(WithLens).

adjust(WLs) ->
    adjust(0, WLs, []).

adjust(_, [], Ws) ->
    lists:reverse(tl(Ws));
adjust(Col, [{W, L}| WLs], Ws) ->
    case Col + L > 72 of
        true ->
            lists:reverse(["\n"| tl(Ws)])
                ++ adjust(L+1, WLs, [" ", W]);
        false ->
            adjust(Col + L + 1, WLs, [" ", W| Ws])
    end.


-spec info(feature()) -> FeatureInfoMap | no_return()
              when
      Description :: string(),
      FeatureInfoMap ::
        #{description := Description,
          short := Description,
          type := type(),
          keywords := [atom()],
          status := status(),
          experimental => release(),
          approved => release(),
          permanent => release(),
          rejected => release()
         }.
info(Feature) ->
    ?VALID_FEATURE(Feature),

    Map = persistent_term:get({?MODULE, feature_specs}),
    maps:get(Feature, Map).

%% New keywords introduced by a feature.
-spec keywords(feature()) -> [atom()] | no_return().
keywords(Ftr) ->
    ?VALID_FEATURE(Ftr),

    #{keywords := Keywords} = info(Ftr),
    Keywords.

%% Internal - Ftr is valid
keywords(Ftr, Map) ->
    maps:get(keywords, maps:get(Ftr, Map)).

%% Utilities
%% Returns list of enabled features and a new keywords function
-spec keyword_fun([term()], fun((atom()) -> boolean())) ->
          {'ok', {[feature()], fun((atom()) -> boolean())}}
              | {'error', error()}.
keyword_fun(Opts, KeywordFun) ->
    %% Get items enabling or disabling features, preserving order.
    IsFtr = fun({feature, _, enable}) -> true;
               ({feature, _, disable}) -> true;
               (_) -> false
            end,
    FeatureOps = lists:filter(IsFtr, Opts),
    {AddFeatures, DelFeatures} = collect_features(FeatureOps),
    %% FIXME check that all features are known at this stage so we
    %% don't miss out on reporting any unknown features.

    case add_features_fun(AddFeatures, KeywordFun) of
        {ok, Fun} ->
            case remove_features_fun(DelFeatures, Fun) of
                {ok, FunX} ->
                    {ok, {AddFeatures -- DelFeatures, FunX}};
                {error, _} = Error ->
                    %% FIXME We are missing potential incorrect
                    %% features being disabled
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec keyword_fun('enable' | 'disable', feature(), [feature()],
                  fun((atom()) -> boolean())) ->
          {'ok', {[feature()], fun((atom()) -> boolean())}}
              | {'error', error()}.
keyword_fun(Ind, Feature, Ftrs, KeywordFun) ->
    case is_valid(Feature) of
        true ->
            case Ind of
                enable ->
                    {ok, {[Feature | Ftrs],
                          add_feature_fun(Feature, KeywordFun)}};
                disable ->
                    {ok, {Ftrs -- [Feature],
                          remove_feature_fun(Feature, KeywordFun)}}
            end;
        false ->
            {error, {?MODULE, {invalid_features, [Feature]}}}
    end.

add_feature_fun(Feature, F) ->
    Words = keywords(Feature),
    fun(Word) ->
            lists:member(Word, Words)
                orelse F(Word)
    end.

remove_feature_fun(Feature, F) ->
    Words = keywords(Feature),
    fun(Word) ->
            case lists:member(Word, Words) of
                true -> false;
                false -> F(Word)
            end
    end.

-spec add_features_fun([feature()], fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}
              | {'error', error()}.
add_features_fun(Features, F) ->
    case lists:all(fun is_valid/1, Features) of
        true ->
            {ok, lists:foldl(fun add_feature_fun/2, F, Features)};
        false ->
            IsInvalid = fun(Ftr) -> not is_valid(Ftr) end,
            Invalid = lists:filter(IsInvalid, Features),
            {error, {?MODULE, {invalid_features, Invalid}}}
    end.

-spec remove_features_fun([feature()], fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}
              | {'error', error()}.
remove_features_fun(Features, F) ->
    case lists:all(fun is_valid/1, Features) of
        true ->
            {ok, lists:foldl(fun remove_feature_fun/2, F, Features)};
        false ->
            IsInvalid = fun(Ftr) -> not is_valid(Ftr) end,
            Invalid = lists:filter(IsInvalid, Features),
            {error, {?MODULE, {invalid_features, Invalid}}}
    end.

-spec format_error(Reason, StackTrace) -> ErrorDescription
              when Reason :: term(),
                   StackTrace :: erlang:stacktrace(),
                   ArgumentPosition :: pos_integer(),
                   ErrorDescription :: #{ArgumentPosition => unicode:chardata(),
                                         general => unicode:chardata(),
                                         reason => unicode:chardata()}.
format_error(Reason, [{_M, _F, _Args, Info}| _St]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorMap = maps:get(cause, ErrorInfo),
    ErrorMap#{reason => io_lib:format("~p: ~p", [?MODULE, Reason])}.

-spec format_error(Reason) -> iolist()
              when Reason :: term().
format_error({invalid_features, Features}) ->
    Fmt = fun F([Ftr]) -> io_lib:fwrite("'~p'", [Ftr]);
              F([Ftr1, Ftr2]) ->
                  io_lib:fwrite("'~p' and '~p'", [Ftr1, Ftr2]);
              F([Ftr| Ftrs]) ->
                  io_lib:fwrite("'~p', ~s", [Ftr, F(Ftrs)])
          end,
    case Features of
        [Ftr] ->
            io_lib:fwrite("the feature ~s does not exist.", [Fmt([Ftr])]);
        Ftrs ->
            io_lib:fwrite("the features ~s do not exist.", [Fmt(Ftrs)])
    end.

%% Hold the state of which features are currently enabled.
%% This is almost static, so we go for an almost permanent state,
%% i.e., use persistent_term.
init_features() ->
    Map = init_specs(),

    persistent_term:put({?MODULE, enabled_features}, []),
    persistent_term:put({?MODULE, keywords}, []),

    RawOps = lists:filter(fun({Tag, _}) ->
                                      Tag == 'enable-feature'
                                          orelse Tag == 'disable-feature';
                                 (_) -> false
                              end,
                              init:get_arguments()),

    Cnv = fun('enable-feature') -> enable;
             ('disable-feature') -> disable
          end,

    FeatureOps = lists:append(lists:map(fun({Tag, Strings}) ->
                                                lists:map(fun(S) ->
                                                                  {Tag, S} end,
                                                          Strings)
                                        end,
                                        RawOps)),

    %% Convert failure, e.g., too long string for atom, to not
    %% being a valid feature.
    F = fun({Tag, String}) ->
                try
                    Atom = list_to_atom(String),
                    case is_valid(Atom) of
                        true -> {true, {feature, Atom, Cnv(Tag)}};
                        false when Atom == all ->
                            {true, {feature, Atom, Cnv(Tag)}};
                        false -> false
                    end
                catch
                    _ -> false
                end
        end,
    FOps = lists:filtermap(F, FeatureOps),
    {Features, _} = collect_features(FOps),
    {Enabled, Keywords} =
        lists:foldl(fun(Ftr, {Ftrs, Keys}) ->
                            case lists:member(Ftr, Ftrs) of
                                true ->
                                    {Ftrs, Keys};
                                false ->
                                    {[Ftr| Ftrs],
                                     keywords(Ftr, Map) ++ Keys}
                            end
                    end,
                    {[], []},
                    Features),

    %% Save state
    enabled_features(Enabled),
    set_keywords(Keywords),
    persistent_term:put({?MODULE, init_done}, true),
    ok.

init_specs() ->
    Specs = case os:getenv("OTP_TEST_FEATURES") of
                "true" -> test_features();
                _ -> feature_specs()
            end,
    persistent_term:put({?MODULE, feature_specs}, Specs),
    Specs.

ensure_init() ->
    case persistent_term:get({?MODULE, init_done}, false) of
        true -> ok;
        false ->
            init_features()
    end.

%% Return list of currently enabled features
-spec enabled() -> [feature()].
enabled() ->
    ensure_init(),
    persistent_term:get({?MODULE, enabled_features}).

enabled_features(Ftrs) ->
    persistent_term:put({?MODULE, enabled_features}, Ftrs).

%% Return list of keywords activated by enabled features
-spec keywords() -> [atom()].
keywords() ->
    ensure_init(),
    persistent_term:get({?MODULE, keywords}).

set_keywords(Words) ->
    persistent_term:put({?MODULE, keywords}, Words).

%% Check that any features used in the module are enabled in the
%% runtime system.  If not, return
%%  {not_allowed, <list of not enabled features>}.
-spec load_allowed(binary()) -> ok | {not_allowed, [feature()]}.
load_allowed(Binary) ->
    case erts_internal:beamfile_chunk(Binary, "Meta") of
        undefined ->
            ok;
        Meta ->
            MetaData = erlang:binary_to_term(Meta),
            case proplists:get_value(enabled_features, MetaData) of
                undefined ->
                    ok;
                Used ->
                    Enabled = enabled(),
                    case lists:filter(fun(UFtr) ->
                                              not lists:member(UFtr, Enabled)
                                      end,
                                      Used) of
                        [] -> ok;
                        NotEnabled ->
                            {not_allowed, NotEnabled}
                    end
            end
    end.

%% Return features used by module or beam file
-spec used(module() | file:filename()) -> [feature()].
used(Module) when is_atom(Module) ->
    case code:get_object_code(Module) of
        error ->
            not_found;
        {_Mod, Bin, _Fname} ->
            features_in(Bin)
    end;
used(FName) when is_list(FName) ->
    features_in(FName).

features_in(NameOrBin) ->
    case beam_lib:chunks(NameOrBin, ["Meta"], [allow_missing_chunks]) of
        {ok, {_, [{_, missing_chunk}]}} ->
            [];
        {ok, {_, [{_, Meta}]}} ->
            MetaData = erlang:binary_to_term(Meta),
            proplists:get_value(enabled_features, MetaData, []);
        _ ->
            not_found
    end.

approved() ->
    [Ftr || Ftr <- all(),
            maps:get(status, info(Ftr)) == approved].

permanent() ->
    [Ftr || Ftr <- all(),
            maps:get(status, info(Ftr)) == permanent].

%% Interpret feature ops (enable or disable) to build the full set of
%% features.  The meta feature 'all' is expanded to all known
%% features.
collect_features(FOps) ->
    %% Features enabled by default
    Enabled = approved() ++ permanent(),
    collect_features(FOps, Enabled, []).

collect_features([], Add, Del) ->
    {Add, Del};
collect_features([{feature, all, enable}| FOps], Add, _Del) ->
    All = all(),
    Add1 = lists:foldl(fun add_ftr/2, Add, All),
    collect_features(FOps, Add1, []);
collect_features([{feature, Feature, enable}| FOps], Add, Del) ->
    collect_features(FOps, add_ftr(Feature, Add), Del -- [Feature]);
collect_features([{feature, all, disable}| FOps], _Add, Del) ->
    %% Start over
    All = all(),
    collect_features(FOps, [], Del -- All);
collect_features([{feature, Feature, disable}| FOps], Add, Del) ->
    collect_features(FOps, Add -- [Feature],
                     add_ftr(Feature, Del)).

add_ftr(F, []) ->
    [F];
add_ftr(F, [F| _] = Fs) ->
    Fs;
add_ftr(F, [F0| Fs]) ->
    [F0| add_ftr(F, Fs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test features - not present in a release
test_features() ->
    #{ifn_expr =>
          #{short => "New expression `ifn cond -> body end`",
            description =>
                "Inclusion of expression `ifn cond -> body end`, which "
            "evaluates `body` when cond is false.  This is a truly "
            "experimental feature, present only to show and use the "
            "support for experimental features.  Not extensively tested.  "
            "Implementated by a transformation in the parser.",
            status => experimental,
            experimental => 24,
            keywords => ['ifn'],
            type => extension},
      ifnot_expr =>
          #{short => "New expression `ifnot cond -> body end`",
            description =>
                "Inclusion of expression `ifnot cond -> body end`, which "
            "evaluates `body` when cond is false.  This is a truly "
            "experimental feature, present only to show and use the "
            "support for experimental features.  Not extensively tested.  "
            "Similar to ifn_expr, but with a deeper implementation.",
            status => experimental,
            experimental => 25,
            keywords => ['ifnot'],
            type => extension},
      unless_expr =>
          #{short => "`unless <cond> -> <body> end",
            description =>
                "Introduction of new expression `unless <cond> -> <body> end."
            " Truly experimental.",
            status => experimental,
            experimental => 25,
            keywords => ['unless'],
            type => extension},
      maps =>
          #{short => "Add maps as new data type",
            description => "Add new data type for maps with syntactic "
            "support in Erlang as well native support in the beam.",
            status => permanent,
            experimental => 17,
            approved => 18,
            permanent => 19,
            keywords => [],
            type => extension},
      cond_expr =>
          #{short => "Introduce general Lisp style conditional",
            description =>
                "Finally complement the painfully broken `if` "
            "with a general conditional as in Lisp from the days of old.",
            status => approved,
            experimental => 24,
            approved => 25,
            keywords => [],
            type => extension},
      while_expr =>
          #{short => "Introduce strange iterative expressions",
            description =>
                "Introduce looping constructs, with seemingly "
            "destructive assignment and vague semantics.",
            status => experimental,
            experimental => 25,
            keywords => ['while', 'until'],
            type => extension}}.
