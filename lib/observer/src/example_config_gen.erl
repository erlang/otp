%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @author  Bartlomiej Puzon <bartlomiej.puzon@erlang-solutions.com>
%%% @doc  The module provides an API to generate configurations.
%%%
%%% Version: 1.0

%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met: 
%%% * Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution. 
%%% * Neither the name of the Erlang Training & Consulting nor the names of its
%%% contributors may be used to endorse or promote products
%%% derived from this software without specific prior written permission.
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(example_config_gen).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Solutions Ltd.').
-include("example_server.hrl").

-export([create_trace_case/2, create_trace_case/1, create_trace_case/0,
         create_pattern/4, create_pattern/3, create_pattern/2,
         create_flags/2,
         create_merge_conf/2, create_merge_conf/0,
         add_pattern/2,
         add_flags/2,
         add_merge_conf/2,
         add_nodes/2,
         add_options/2,
         create_fun/1,
	 validate/5, validate/6]).

-define(TTB_FILENAME, "onviso_server").
-define(FETCH_DIR, undefined).
%% @doc Creates a default empty trace case
%% @end
-spec create_trace_case() -> #trace_case{}.     
create_trace_case() ->
    create_trace_case(?FETCH_DIR, "").

%% @doc Creates a default empty trace with appropriate comment
%% @end
-spec create_trace_case(string()) -> #trace_case{}.
create_trace_case(Comment) ->
    create_trace_case(?FETCH_DIR, Comment).

%% @doc Creates an empty trace
%% @end
-spec create_trace_case(string(), string()) -> #trace_case{}.
create_trace_case(FetchDir, Comment) ->
    #trace_case{case_id = undefined,
		all_traces = [],
                patterns = [],
                trace_flag = [],
                nodes = [],
                merge_confs = [],
                fetch_dir = FetchDir,
		trace_opts = [],
                comment = Comment
               }.

%% @doc Create a pattern record. Will perform some basic error validation
%% on the pattern. 
%% @end.
-spec create_pattern(atom(), extended_match_spec()) -> #pattern{}.
create_pattern(Module, MatchSpec) ->
    create_pattern(Module, '_', '_', MatchSpec).
    
-spec create_pattern(atom(), atom(), extended_match_spec()) -> #pattern{}.
create_pattern(Module, Function, MatchSpec) ->
    create_pattern(Module, Function, '_', MatchSpec).

-spec create_pattern(atom(), atom(), integer() | '_', extended_match_spec()) -> #pattern{}.
create_pattern(Module, Function, Arity, MatchSpec) ->
    case validate_ms(MatchSpec) of
        true ->
            do_create_pattern(Module, Function, Arity, MatchSpec);
        false ->
            {error, invalid_matchspec}
    end.

validate_ms(caller) -> true;
validate_ms(return) -> true;
validate_ms({fun2ms, FunStr}) when is_list(FunStr) -> true;
validate_ms(MS) when is_list(MS) -> true;
validate_ms(_) -> false.
    
do_create_pattern(Module, Function, Arity, MatchSpec) ->
    #pattern{module = Module, 
             function = Function, 
             arity = Arity,
             matchspec = MatchSpec}.

%% @doc Create flag record.
%% @end
-spec create_flags(proc_spec(), flag_spec()) -> #flags{}.
create_flags(Scope, Flags) ->
    #flags{scope = Scope, 
           flags = Flags}.

%% @doc <p>Create merge configuration record with default settings
%% @end
-spec create_merge_conf() -> #merge_conf{}.
create_merge_conf() ->
    create_merge_conf(undefined, "").

%% @doc <p>Create merge configuration record. </p>
%% @end
-spec create_merge_conf(merge_spec(), string()) -> #merge_conf{}.
create_merge_conf(MergeFun, Comment) ->
    #merge_conf{function = MergeFun,
		comment = Comment
	       }.

%% @doc Add a pattern to a trace case record. 
%% @end    
-spec add_pattern(#pattern{}, #trace_case{}) -> #pattern{}.
add_pattern(Pattern, TraceCase) ->
    NewPatterns = [Pattern|TraceCase#trace_case.patterns],
    TraceCase#trace_case{patterns=NewPatterns}.

%% @doc Add a flag specification to a trace case record. 
%% @end
-spec add_flags(#flags{}, #trace_case{}) -> #trace_case{}.
add_flags(Flags, TraceCase) ->    
    TraceCase#trace_case{trace_flag = Flags}.

%% @doc Add a merge configuration to a trace case record. 
%% @end
-spec add_merge_conf(#merge_conf{}, #trace_case{}) -> #trace_case{}.   
add_merge_conf(MergeConf, TraceCase) ->
    NewMergeConfs = [MergeConf|TraceCase#trace_case.merge_confs],
    TraceCase#trace_case{merge_confs = NewMergeConfs}.

%% @doc Append nodes to a trace case. 
%% @end
-spec add_nodes(list(atom()), #trace_case{}) -> #trace_case{}.
add_nodes(Nodes, TraceCase) ->
    AddNodes = lists:append(Nodes, TraceCase#trace_case.nodes),
    TraceCase#trace_case{nodes = AddNodes}.


%% @doc Append options to a trace case. 
%% @end
-spec add_options(list(), #trace_case{}) -> #trace_case{}.
add_options(Options, TraceCase) ->
    AddOptions = lists:append(Options, TraceCase#trace_case.trace_opts),
    TraceCase#trace_case{trace_opts = AddOptions}.

%% @doc Create a fun from a string. 
%% @end
-spec create_fun(string()) -> {ok, function()}.    
create_fun(StringFun) ->
    if length(StringFun) > 1 ->
	   {ok, WithEnd} = ensure_period_ending(StringFun),
	   FunToEval = string:join(["Fun = ", WithEnd], ""),
	   evaluate_fun(FunToEval);
       true ->
	   {ok, void}
    end.

%% @doc <p>Validate a configuration as inputted in a user interface. It is assumed
%% that all data is passed as strings in the following format:</p>
%% <pre>
%% Patterns = [{"module", "function", "arguments", "match specification"}]
%% Nodes = ["node@no-name-host"]
%% Flags = ["scope, [flags]"]
%% Merge = [{'fun', "WorkerFun"}, {comment, "Comment"}]
%% Options = "list()" (see ttbe for possible options list)
%% </pre>
%% <p>Call validate like this:</p>
%% <pre> validate(Patterns, Nodes, Flags, Merge, Options) </pre>
%% <p>The function will try to validate the input as is and in some cases try 
%% to correct minor mistakes. </p>
%% <p>When an error is present the return could look like this:</p>
%% <pre>
%%     {error, [{ok, [{ok, Pattern}, {ok, Pattern2}]},
%%              {ok, [{ok, Node}]},
%%              {ok, [{ok, Flag}]},
%%              {error, [{ok, {beginfun, BeginFun}},
%%                       {error, {workfun, syntax}},
%%                       {ok, {endfun, EndFun}}]},
%%              {ok, [{ok, Option}]}]}.
%% </pre>

-spec validate(list(), list(), list(), list(), list()) -> {ok, #trace_case{}}.
validate(Patterns, Nodes, Flags, Merge, Options) ->
    validate(Patterns, Nodes, Flags, Merge, Options, "No comment").

validate(Patterns, Nodes, Flags, Merge, Options, Comment) ->
    MatchValidators = pair_validators(Patterns, Nodes, Flags, 
				      Merge, Options),
    CheckedIndividual = lists:foldl(fun ({Validator, Validate}, Validated) ->
					    [Validator(Validate)| Validated]
				    end, [], MatchValidators),
    GeneralSummary = {summary(CheckedIndividual), CheckedIndividual},
    case GeneralSummary of
        {ok, Summary} ->
            {ok, create_complete_trace_case(Summary, Comment)};
        Error ->
            Error
    end.

%% Validators
pair_validators(Patterns, Nodes, Flags, Merge, Options) ->
    [{fun validate_patterns/1, Patterns},
     {fun validate_nodes/1, Nodes},
     {fun validate_flags/1, Flags},
     {fun validate_merge/1, Merge},
     {fun validate_options/1, Options}].

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------    

create_complete_trace_case(Summary, Name) ->
    % folds over each element in the list and passes them 
    % to a fun, the input accumulator is a trace case with generated ID.
    lists:foldl(fun add_to_trace_case_record/2,
                create_trace_case(Name), Summary).

-spec retrieve_merge_confs(list(list())) -> list(#merge_conf{}).
retrieve_merge_confs(Data) ->
    lists:map(fun({merge_conf, ok, List}) ->
		      {'fun', ok, {'fun', WorkFun}} = lists:keyfind('fun', 1, List),
		      {comment, ok, {comment, Comment}} = lists:keyfind(comment, 1, List),
		      create_merge_conf(WorkFun, Comment)
	      end, Data).

add_to_trace_case_record({Type, ok, Data}, Record) ->
    case Type of 
        patterns ->
            Patterns = [create_pattern(M, F, A, MS) || 
                           {pattern, ok, {M, F, A, MS}} <- Data],
            Record#trace_case{patterns = Patterns};
        nodes ->
            Nodes = [Node || {node, ok, Node} <- Data],
            Record#trace_case{nodes = Nodes};        
        flags ->
            [{flag, ok, {Scope, FlagList}}] = Data,
            Record#trace_case{trace_flag = create_flags(Scope, FlagList)};
        merge_confs ->
	    Record#trace_case{merge_confs = retrieve_merge_confs(Data)};
	trace_opts ->
	    {trace_opts, Opts} = Data,
	    Record#trace_case{trace_opts = Opts}
    end.

%%

validate_patterns([]) ->
    {patterns, error, no_data};

validate_patterns(Patterns) ->
    do_validate_patterns(Patterns, []).

do_validate_patterns([], Acc) ->
    {patterns, summary(Acc), Acc};
do_validate_patterns([{Module, Function, Arity, MatchSpec}
                      = OrgPattern|Rest], Acc) ->
    NewAcc = case validate_mfa(Module, Function, Arity) of
                 {Mod, Fun, Ari} ->
                     case evaluate_expression(MatchSpec) of
                         {ok, MatchExprs} ->
                             [{pattern, ok, {Mod, Fun, Ari, MatchExprs}}|Acc];
                         _ErrorExpr ->
                             [{pattern, error, {OrgPattern}}|Acc]
                     end;
                 _ ->
                     [{pattern, error, OrgPattern}|Acc]
             end,
    do_validate_patterns(Rest, NewAcc);
do_validate_patterns([Error|Rest], Acc) ->
    do_validate_patterns(Rest, [{pattern, error, Error}|Acc]).

%%
validate_mfa(M, F, "_") ->
    {list_to_atom(M), list_to_atom(F), '_'};
    
validate_mfa(M,F,A) ->
    Ari = case catch list_to_integer(A) of
	      Int when is_integer(Int),
		       Int >= 0 ->
		  Int;
	      _ ->
		  error
	  end,
    case Ari of
	error ->
	    error;
	Ari ->
	    {list_to_atom(M), list_to_atom(F), Ari}
    end.
		  
%% 
validate_nodes([]) ->
    {nodes, error, no_data};

validate_nodes(Nodes) ->
    AtomNodes = lists:foldl(fun(Node, Acc) ->
                                [{node, ok, list_to_atom(Node)}|Acc]
                        end, [], Nodes),

    {nodes, summary(AtomNodes), AtomNodes}.

%%
validate_flags([]) ->
    {flags, error, no_data};

validate_flags(Flags) ->
    F = lists:foldl(fun(Flag, Acc) ->
                            case evaluate_expression(Flag) of
                                {ok, Exprs} ->
                                    [{flag, ok, Exprs}|Acc];
                                {error, _Reason} ->
                                    [{flag, error, Flag}|Acc]
                            end
                    end, [], Flags),

    {flags, summary(F), F}.   

%% 

validate_merge([]) ->
    {merge_confs, error, no_data};

validate_merge(MergeConfs) ->
    do_validate_merges(MergeConfs, []).

do_validate_merges([], Acc) ->
    {merge_confs, summary(Acc), Acc};

do_validate_merges([MergeConf|Rest], Acc) ->
    M = lists:foldl(fun({comment, String}, Acc2) ->
			    [{comment, ok, {comment, String}} | Acc2];
		       ({'fun', "undefined"}, Acc2) ->
                            [{'fun', ok, {'fun', undefined}}|Acc2];
                       ({'fun', HandlerStr}, Acc2) ->
                            case evaluate_expression(HandlerStr) of
                                {ok, Handler} ->
                                    [{'fun', ok, {'fun', Handler}}|Acc2];
                                {error, Reason} ->
                                    [{'fun', error, {'fun', Reason}}|Acc2]
                            end
                    end, [], MergeConf),
    Result = {merge_conf, summary(M), M},
    do_validate_merges(Rest, [Result|Acc]).

%%
validate_options(Options) ->
    case evaluate_expression(Options) of
        {ok, Exprs} ->
            {trace_opts, ok, {trace_opts, Exprs}};
        {error, Reason} ->
            {trace_opts, error, {trace_opts, Reason}}
    end.

%% 
%% CheckList = [{type, validity, Data}]
summary(CheckList) -> 
    case lists:all(fun({_, Validity, _}) ->
                           Validity == ok
                   end, CheckList) of
        true ->
            ok;
        false ->
            error
    end.


%% 
ensure_period_ending(Fun) ->
    case string:right(Fun, 1) of
        "." ->
            {ok, Fun};
        _ ->
            {ok, string:join([Fun, "."], "")}
    end.

%%
evaluate_fun(StringFun) ->
    {ok, Tokenized, _} = erl_scan:string(StringFun),
    case erl_parse:parse_exprs(Tokenized) of
      {ok, Parsed} ->
	  case erl_eval:exprs(Parsed, []) of
	    {value, Fun, _} ->
		{ok, Fun};
	    _ ->
		{error, evaluation}
	  end;
      _ ->
	  {error, syntax}
    end.

%% 
evaluate_expression(StringData) ->
    VariableName = string:join(["Variable = ", StringData, "."], ""),
    evaluate_fun(VariableName).
