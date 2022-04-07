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

%% FIXME divide the exported functions in public and internal for the
%% sake of documentation.
-export([features/0,
         feature_info/1,
         collect_features/1,
         short/1,
         long/1,
         enabled_features/0,
         is_valid_feature/1,
         load_allowed/1,
         keywords/0,
         keywords/1,
         keyword_fun/2,
         keyword_fun/4,
         enable_feature/1,
         disable_feature/1,
         format_error/1,
         format_error/2]).

-export([features_used/1]).

-type type() :: 'extension' | 'backwards_incompatible_change'.
-type status() :: 'experimental'
                  | 'approved'
                  | 'permanent'
                  | 'rejected'.
-type release() :: non_neg_integer().
-type error() :: {?MODULE, {'invalid_features', [atom()]}}.

-define(VALID_FEATURE(Feature),
        (case is_valid_feature(Feature) of
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

%% Currently known features
-spec features() -> [atom()].
features() ->
    Map = case persistent_term:get({?MODULE, feature_specs}, none) of
              none -> init_specs();
              M -> M
          end,
    maps:keys(Map).

is_valid_feature(Ftr) ->
    lists:member(Ftr, features()).

-spec short(atom()) -> iolist().
short(Feature) ->
    #{short := Short,
      status := Status} = Info = feature_info(Feature),
    #{Status := Release} = Info,
    io_lib:format("~-40s ~-12s (~p)", [Short, Status, Release]).

long(Feature) ->
    #{short := Short,
      description := Description,
      status := Status,
      keywords := Keywords,
      type := Type} = Info = feature_info(Feature),
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


-spec feature_info(atom()) -> FeatureInfoMap | no_return()
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
feature_info(Feature) ->
    ?VALID_FEATURE(Feature),

    Map = persistent_term:get({?MODULE, feature_specs}),
    maps:get(Feature, Map).

%% New keywords for a feature.  The current set is just for
%% tests and development.
-spec keywords(atom()) -> [atom()].
keywords(Ftr) ->
    ?VALID_FEATURE(Ftr),

    #{keywords := Keywords} = feature_info(Ftr),
    Keywords.

%% Internal - Ftr is valid
keywords(Ftr, Map) ->
    maps:get(keywords, maps:get(Ftr, Map)).

%% Utilities
%% Returns list of enabled features and a new keywords function
%% -spec keyword_fun_add_feature(atom(), fun((atom()) -> boolean())) ->
%%           {'ok', fun((atom()) -> boolean())}
%%               | {'error', error()}.
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

    case keyword_fun_add_features(AddFeatures, KeywordFun) of
        {ok, Fun} ->
            case keyword_fun_remove_features(DelFeatures, Fun) of
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

%% -spec keyword_fun_add_feature(atom(), fun((atom()) -> boolean())) ->
%%           {'ok', fun((atom()) -> boolean())}
%%               | {'error', error()}.
keyword_fun(Ind, Feature, Ftrs, KeywordFun) ->
    case is_valid_feature(Feature) of
        true ->
            case Ind of
                enable ->
                    {ok,
                     add_feature(Feature, KeywordFun),
                     [Feature | Ftrs]};
                disable ->
                    {ok,
                     remove_feature(Feature, KeywordFun),
                     Ftrs -- [Feature]}
            end;
        false ->
            {error, {?MODULE, {invalid_features, [Feature]}}}
    end.

%% FIXME Rename this to reflect that it returns a function!
add_feature(Feature, F) ->
    Words = keywords(Feature),
    fun(Word) ->
            lists:member(Word, Words)
                orelse F(Word)
    end.

%% FIXME Rename this to reflect that it returns a function!
remove_feature(Feature, F) ->
    Words = keywords(Feature),
    fun(Word) ->
            case lists:member(Word, Words) of
                true -> false;
                false -> F(Word)
            end
    end.

-spec keyword_fun_add_features([atom()], fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}
              | {'error', error()}.
keyword_fun_add_features(Features, F) ->
    case lists:all(fun is_valid_feature/1, Features) of
        true ->
            {ok, lists:foldl(fun add_feature/2, F, Features)};
        false ->
            IsInvalid = fun(Ftr) -> not is_valid_feature(Ftr) end,
            Invalid = lists:filter(IsInvalid, Features),
            {error, {?MODULE, {invalid_features, Invalid}}}
    end.

-spec keyword_fun_remove_features([atom()], fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}
              | {'error', error()}.
keyword_fun_remove_features(Features, F) ->
    case lists:all(fun is_valid_feature/1, Features) of
        true ->
            {ok, lists:foldl(fun remove_feature/2, F, Features)};
        false ->
            IsInvalid = fun(Ftr) -> not is_valid_feature(Ftr) end,
            Invalid = lists:filter(IsInvalid, Features),
            {error, {?MODULE, {invalid_features, Invalid}}}
    end.

format_error(Reason, [{_M, _F, _Args, Info}| _St]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorMap = maps:get(cause, ErrorInfo),
    ErrorMap#{reason => io_lib:format("~p: ~p", [?MODULE, Reason])}.

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
                    case is_valid_feature(Atom) of
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

%% FIXME - remove this.  It should not be available at runtime.  This
%% is all done by the init code.
enable_feature(Feature) ->
    ?VALID_FEATURE(Feature),

    Features = enabled_features(),
    case lists:member(Feature, Features) of
        true ->
            %% already there, maybe raise an error
            Features;
        false ->
            NewFeatures = [Feature| Features],
            enabled_features(NewFeatures),
            Keywords = keywords(),
            New = keywords(Feature),
            set_keywords(New ++ Keywords),
            NewFeatures
    end.

disable_feature(Feature) ->
    ?VALID_FEATURE(Feature),

    Features = enabled_features(),
    case lists:member(Feature, Features) of
        true ->
            NewFeatures = Features -- [Feature],
            enabled_features(NewFeatures),
            Keywords = keywords(),
            Rem = keywords(Feature),
            set_keywords(Keywords -- Rem),
            NewFeatures;
        false ->
            %% Not there, possibly raise an error
            Features
    end.

enabled_features() ->
    ensure_init(),
    persistent_term:get({?MODULE, enabled_features}).

enabled_features(Ftrs) ->
    persistent_term:put({?MODULE, enabled_features}, Ftrs).

keywords() ->
    ensure_init(),
    persistent_term:get({?MODULE, keywords}).

set_keywords(Words) ->
    persistent_term:put({?MODULE, keywords}, Words).


-spec load_allowed(binary()) -> boolean().
load_allowed(Binary) ->
    case erts_internal:beamfile_chunk(Binary, "Meta") of
        undefined ->
            true;
        Meta ->
            MetaData = erlang:binary_to_term(Meta),
            case proplists:get_value(enabled_features, MetaData) of
                undefined ->
                    true;
                Used ->
                    Enabled = enabled_features(),
                    lists:all(fun(UFtr) ->
                                      lists:member(UFtr, Enabled)
                              end,
                              Used)
            end
    end.


%% Return features used by module or beam file
features_used(Module) when is_atom(Module) ->
    case code:get_object_code(Module) of
        error ->
            not_found;
        {_Mod, Bin, _Fname} ->
            features_in(Bin)
    end;
features_used(FName) when is_list(FName) ->
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

approved_features() ->
    [Ftr || Ftr <- features(),
            maps:get(status, feature_info(Ftr)) == approved].

permanent_features() ->
    [Ftr || Ftr <- features(),
            maps:get(status, feature_info(Ftr)) == permanent].

%% Interpret feature ops (enable or disable) to build the full set of
%% features.  The meta feature 'all' is expanded to all known
%% features.
collect_features(FOps) ->
    %% Features enabled by default
    Enabled = approved_features() ++ permanent_features(),
    collect_features(FOps, Enabled, []).

collect_features([], Add, Del) ->
    {Add, Del};
collect_features([{feature, all, enable}| FOps], Add, _Del) ->
    All = features(),
    Add1 = lists:foldl(fun add_ftr/2, Add, All),
    collect_features(FOps, Add1, []);
collect_features([{feature, Feature, enable}| FOps], Add, Del) ->
    collect_features(FOps, add_ftr(Feature, Add), Del -- [Feature]);
collect_features([{feature, all, disable}| FOps], _Add, Del) ->
    %% Start over
    All = features(),
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
          #{short => "`unless <cond> -> <bodby> end",
            description =>
                "Introduction of new expression `unless <cond> -> <body> end."
            " Truly experimental.",
            status => experimental,
            experimental => 25,
            keywords => ['unless'],
            type => extension},
      maps =>
          #{short => "Add maps as new data type",
            description => "Add new low data type maps with syntactic "
            "support in Erlang as well native support in the beam. "
            "Insert, lookup and delete are asymptotically constant.",
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
