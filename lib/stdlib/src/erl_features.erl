%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2023. All Rights Reserved.
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
-feature(maybe_expr, enable).

-export([all/0,
         configurable/0,
         info/1,
         short/1,
         long/1,
         enabled/0,
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
-type error() :: {?MODULE,
                  {'invalid_features', [atom()]}
                  | {'incorrect_features', [atom()]}
                  | {'not_configurable', [atom()]}}.

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
    lists:sort(maps:keys(Map)).

-spec configurable() -> [feature()].
configurable() ->
    [Ftr || Ftr <- all(),
            lists:member(maps:get(status, info(Ftr)),
                         [experimental, approved])].

is_valid(Ftr) ->
    lists:member(Ftr, all()).

is_configurable(Ftr) ->
    lists:member(Ftr, configurable()).

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
    {AddFeatures, DelFeatures, RawFtrs} = collect_features(FeatureOps),

    case configurable_features(RawFtrs) of
        ok ->
            {ok, Fun} = add_features_fun(AddFeatures, KeywordFun),
            {ok, FunX} = remove_features_fun(DelFeatures, Fun),
            {ok, {AddFeatures -- DelFeatures, FunX}};
        {error, _} = Error ->
            Error
    end.

-spec keyword_fun('enable' | 'disable', feature(), [feature()],
                  fun((atom()) -> boolean())) ->
          {'ok', {[feature()], fun((atom()) -> boolean())}}
              | {'error', error()}.
keyword_fun(Ind, Feature, Ftrs, KeywordFun) ->
    case is_configurable(Feature) of
        true ->
            case Ind of
                enable ->
                    NewFtrs = case lists:member(Feature, Ftrs) of
                                  true -> Ftrs;
                                  false -> [Feature | Ftrs]
                              end,
                    {ok, {NewFtrs,
                          add_feature_fun(Feature, KeywordFun)}};
                disable ->
                    {ok, {Ftrs -- [Feature],
                          remove_feature_fun(Feature, KeywordFun)}}
            end;
        false ->
            Error =
                case is_valid(Feature) of
                    true -> not_configurable;
                    false -> invalid_features
                end,
            {error, {?MODULE, {Error, [Feature]}}}
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
          {'ok', fun((atom()) -> boolean())}.
add_features_fun(Features, F) ->
    {ok, lists:foldl(fun add_feature_fun/2, F, Features)}.

-spec remove_features_fun([feature()], fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}.
remove_features_fun(Features, F) ->
    {ok, lists:foldl(fun remove_feature_fun/2, F, Features)}.

configurable_features(Features) ->
    case lists:all(fun is_configurable/1, Features) of
        true ->
            ok;
        false ->
            feature_error(Features)
    end.

feature_error(Features) ->
    IsInvalid = fun(Ftr) -> not is_valid(Ftr) end,
    IsNonConfig = fun(Ftr) ->
                          is_valid(Ftr)
                              andalso
                                (not is_configurable(Ftr))
                  end,
    Invalid = lists:filter(IsInvalid, Features),
    NonConfig = lists:filter(IsNonConfig, Features),
    {Error, Culprits} =
        case {Invalid, NonConfig} of
            {[], NC} -> {not_configurable, NC};
            {NV, []} -> {invalid_features, NV};
            {NV, NC} -> {incorrect_features, NV ++ NC}
        end,
    {error, {?MODULE, {Error, Culprits}}}.

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
format_error({Error, Features}) ->
    Fmt = fun F([Ftr]) -> io_lib:fwrite("'~p'", [Ftr]);
              F([Ftr1, Ftr2]) ->
                  io_lib:fwrite("'~p' and '~p'", [Ftr1, Ftr2]);
              F([Ftr| Ftrs]) ->
                  io_lib:fwrite("'~p', ~s", [Ftr, F(Ftrs)])
          end,
    FmtStr =
        case {Error, Features} of
            {invalid_features, [_]} ->
                "the feature ~s does not exist.";
            {invalid_features, _} ->
                "the features ~s do not exist.";
            {not_configurable, [_]} ->
                "the feature ~s is not configurable.";
            {not_configurable, _} ->
                "the features ~s are not configurable.";
            {incorrect_features, _} ->
                "the features ~s do not exist or are not configurable."
        end,

    io_lib:fwrite(FmtStr, [Fmt(Features)]).

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
                    case is_configurable(Atom) of
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
    {Features, _, _} = collect_features(FOps),
    {Enabled0, Keywords} =
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
    Enabled = lists:uniq(Enabled0),
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

%% Interpret feature ops (enable or disable) to build the full set of
%% features.  The meta feature 'all' is expanded to all known
%% features.
collect_features(FOps) ->
    %% Features enabled by default
    Enabled = [Ftr || Ftr <- all(),
            maps:get(status, info(Ftr)) == approved],
    collect_features(FOps, Enabled, [], []).

collect_features([], Add, Del, Raw) ->
    {Add, Del, Raw};
collect_features([{feature, all, enable}| FOps], Add, _Del, Raw) ->
    All = configurable(),
    Add1 = lists:foldl(fun add_ftr/2, Add, All),
    collect_features(FOps, Add1, [], Raw);
collect_features([{feature, Feature, enable}| FOps], Add, Del, Raw) ->
    collect_features(FOps, add_ftr(Feature, Add), Del -- [Feature],
                     Raw ++ [Feature]);
collect_features([{feature, all, disable}| FOps], _Add, Del, Raw) ->
    %% Start over
    All = configurable(),
    collect_features(FOps, [], Del -- All, Raw);
collect_features([{feature, Feature, disable}| FOps], Add, Del, Raw) ->
    collect_features(FOps, Add -- [Feature],
                     add_ftr(Feature, Del),
                    Raw ++ [Feature]).

add_ftr(F, []) ->
    [F];
add_ftr(F, [F| _] = Fs) ->
    Fs;
add_ftr(F, [F0| Fs]) ->
    [F0| add_ftr(F, Fs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test features - not present in a release
test_features() ->
    #{experimental_ftr_1 =>
          #{short => "Experimental test feature #1",
            description =>
                "Test feature in the experimental state. "
           "It is disabled by default, but can be enabled.",
            status => experimental,
            experimental => 24,
            keywords => ['ifn'],
            type => extension},
      experimental_ftr_2 =>
          #{short => "Experimental test features #2",
            description =>
                "Test feature in experimental state. "
            "It is disabled by default, but can be enabled.",
            status => experimental,
            experimental => 25,
            keywords => ['while', 'until'],
            type => extension},
      approved_ftr_1 =>
          #{short => "Approved test feature #1",
            description => "Test feature in the approved state.  "
            "It is on by default and can be disabled.",
            status => approved,
            experimental => 24,
            approved => 25,
            keywords => [],
            type => extension},
      approved_ftr_2 =>
          #{short => "Approved test feature #2",
            description =>
                "Test feature in the approved state. "
            "It is enabled by default, but can still be disabled.",
            status => approved,
            experimental => 24,
            approved => 25,
            keywords => ['unless'],
            type => extension},
      permanent_ftr =>
          #{short => "Permanent test feature",
            description => "Test feature in the permanent state.  "
            "This means it is on by default and cannot be disabled.  "
            "It is now a permanent part of Erlang/OTP.",
            status => permanent,
            experimental => 17,
            approved => 18,
            permanent => 19,
            keywords => [],
            type => extension},
      rejected_ftr =>
          #{short => "Rejected test feature.",
            description =>
                "Test feature existing only to end up as rejected. "
            "It is not available and cannot be enabled. "
            "This should be the only trace of it",
            status => rejected,
            experimental => 24,
            rejected => 25,
            keywords => ['inline', 'return', 'set'],
            type => extension}}.
