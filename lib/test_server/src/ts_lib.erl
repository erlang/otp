%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(ts_lib).

-include_lib("kernel/include/file.hrl").
-include("ts.hrl").

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([error/1, var/2, erlang_type/0,
	 initial_capital/1, interesting_logs/1, 
	 specs/1, suites/2, last_test/1,
	 force_write_file/2, force_delete/1,
	 subst_file/3, subst/2, print_data/1,
	 maybe_atom_to_list/1, progress/4
	]).

error(Reason) ->
    throw({error, Reason}).

%% Returns the value for a variable

var(Name, Vars) ->
    case lists:keysearch(Name, 1, Vars) of
	{value, {Name, Value}} ->
	    Value;
	false ->
	    error({bad_installation, {undefined_var, Name, Vars}})
    end.

%% Returns the level of verbosity (0-X)
verbosity(Vars) ->
    % Check for a single verbose option.
    case lists:member(verbose, Vars) of
	true ->
	    1;
	false ->
	    case lists:keysearch(verbose, 1, Vars) of
		{value, {verbose, Level}} ->
		    Level;
		_ ->
		    0
	    end
    end.

% Displays output to the console if verbosity is equal or more
% than Level.
progress(Vars, Level, Format, Args) ->
    V=verbosity(Vars),
    if
	V>=Level ->
	    io:format(Format, Args);
	true ->
	    ok
    end.

%% Returns: {Type, Version} where Type is otp|src

erlang_type() ->
    {_, Version} = init:script_id(),
    RelDir = filename:join(code:root_dir(), "releases"), % Only in installed
    case filelib:is_file(RelDir) of
	true -> {otp,Version};			% installed OTP
	false -> {srctree,Version}		% source code tree
    end.
	        
%% Upcases the first letter in a string.

initial_capital([C|Rest]) when $a =< C, C =< $z ->
    [C-$a+$A|Rest];
initial_capital(String) ->
    String.

%% Returns a list of the "interesting logs" in a directory,
%% i.e. those that correspond to spec files.

interesting_logs(Dir) ->
    Logs = filelib:wildcard(filename:join(Dir, [$*|?logdir_ext])),
    Interesting =
	case specs(Dir) of
	    [] ->
		Logs;
	    Specs0 ->
		Specs = ordsets:from_list(Specs0),
		[L || L <- Logs, ordsets:is_element(filename_to_atom(L), Specs)]
	end,
    sort_tests(Interesting).

specs(Dir) ->
    Specs = filelib:wildcard(filename:join([filename:dirname(Dir),
					    "*_test", "*.{dyn,}spec"])), 
    sort_tests([filename_to_atom(Name) || Name <- Specs]).

suites(Dir, Spec) ->
    Glob=filename:join([filename:dirname(Dir), Spec++"_test",
			"*_SUITE.erl"]),
    Suites=filelib:wildcard(Glob),
    [filename_to_atom(Name) || Name <- Suites].
    
filename_to_atom(Name) ->
    list_to_atom(filename:rootname(filename:basename(Name))).

%% Sorts a list of either log files directories or spec files.

sort_tests(Tests) ->
    Sorted = lists:usort([{suite_order(filename_to_atom(X)), X} ||
			     X <- Tests]),
    [X || {_, X} <- Sorted].

%% This defines the order in which tests should be run and be presented
%% in index files.

suite_order(emulator) -> 0;
suite_order(test_server) -> 1;
suite_order(kernel) -> 4;
suite_order(stdlib) -> 6;
suite_order(compiler) -> 8;
suite_order(hipe) -> 9;
suite_order(erl_interface) -> 12;
suite_order(jinterface) -> 14;
suite_order(sasl) -> 16;
suite_order(tools) -> 18;
suite_order(runtime_tools) -> 19;
suite_order(parsetools) -> 20;
suite_order(pman) -> 21;
suite_order(debugger) -> 22;
suite_order(toolbar) -> 23;
suite_order(ic) -> 24;
suite_order(orber) -> 26;
suite_order(inets) -> 28;
suite_order(asn1) -> 30;
suite_order(os_mon) -> 32;
suite_order(snmp) -> 38;
suite_order(mnemosyne) -> 40;
suite_order(mnesia_session) -> 42;
suite_order(mnesia) -> 44;
suite_order(system) -> 999; %% IMPORTANT: system SHOULD always be last!
suite_order(_) -> 200.

last_test(Dir) ->
    last_test(filelib:wildcard(filename:join(Dir, "run.[1-2]*")), false).

last_test([Run|Rest], false) ->
    last_test(Rest, Run);
last_test([Run|Rest], Latest) when Run > Latest ->
    last_test(Rest, Run);
last_test([_|Rest], Latest) ->
    last_test(Rest, Latest);
last_test([], Latest) ->
    Latest.

%% Do the utmost to ensure that the file is written, by deleting or
%% renaming an old file with the same name.

force_write_file(Name, Contents) ->
    force_delete(Name),
    file:write_file(Name, Contents).

force_delete(Name) ->
    case file:delete(Name) of
	{error, eacces} ->
	    force_rename(Name, Name ++ ".old.", 0);
	Other ->
	    Other
    end.

force_rename(From, To, Number) ->
    Dest = [To|integer_to_list(Number)],
    case file:read_file_info(Dest) of
	{ok, _} ->
	    force_rename(From, To, Number+1);
	{error, _} ->
	    file:rename(From, Dest)
    end.

%% Substitute all occurrences of @var@ in the In file, using
%% the list of variables in Vars, producing the output file Out.
%% Returns: ok | {error, Reason}

subst_file(In, Out, Vars) ->
    case file:read_file(In) of
	{ok, Bin} ->
	    Subst = subst(binary_to_list(Bin), Vars, []),
	    case file:write_file(Out, Subst) of
		ok ->
		    ok;
		{error, Reason} ->
		    {error, {file_write, Reason}}
	    end;
	Error ->
	    Error
    end.

subst(String, Vars) ->
    subst(String, Vars, []).

subst([$@, $_|Rest], Vars, Result) ->
    subst_var([$_|Rest], Vars, Result, []);
subst([$@, C|Rest], Vars, Result) when $A =< C, C =< $Z ->
    subst_var([C|Rest], Vars, Result, []);
subst([$@, C|Rest], Vars, Result) when $a =< C, C =< $z ->
    subst_var([C|Rest], Vars, Result, []);
subst([C|Rest], Vars, Result) ->
    subst(Rest, Vars, [C|Result]);
subst([], _Vars, Result) ->
    lists:reverse(Result).

subst_var([$@|Rest], Vars, Result, VarAcc) ->
    Key = list_to_atom(lists:reverse(VarAcc)),
    {Result1,Rest1} = do_subst_var(Key, Rest, Vars, Result, VarAcc),
    subst(Rest1, Vars, Result1);

subst_var([C|Rest], Vars, Result, VarAcc) ->
    subst_var(Rest, Vars, Result, [C|VarAcc]);
subst_var([], Vars, Result, VarAcc) ->
    subst([], Vars, [VarAcc++[$@|Result]]).

%% handle conditional
do_subst_var(Cond, Rest, Vars, Result, _VarAcc) when Cond == 'IFEQ' ;
						     Cond == 'IFNEQ' ->
    {Bool,Comment,Rest1} = do_test(Rest, Vars, Cond),
    Rest2 = extract_clause(Bool, Rest1),
    {lists:reverse(Comment, Result),Rest2};
%% variable substitution
do_subst_var(Key, Rest, Vars, Result, VarAcc) ->
    case lists:keysearch(Key, 1, Vars) of
	{value, {Key, Value}} ->
	    {lists:reverse(Value, Result),Rest};
	false ->
	    {[$@|VarAcc++[$@|Result]],Rest}
    end.

%% check arguments in "@IF[N]EQ@ (Arg1, Arg2)" for equality    
do_test(Rest, Vars, Test) ->
    {Arg1,Rest1} = get_arg(Rest, Vars, $,, []),
    {Arg2,Rest2} = get_arg(Rest1, Vars, 41, []), % $)
    Result = case Arg1 of
		 Arg2 when Test == 'IFEQ'  -> true;
		 Arg2 when Test == 'IFNEQ' -> false;
		 _    when Test == 'IFNEQ' -> true;
		 _                         -> false
	     end,
    Comment = io_lib:format("# Result of test: ~s (~s, ~s) -> ~w", 
			    [atom_to_list(Test),Arg1,Arg2,Result]), 
    {Result,Comment,Rest2}.

%% extract an argument
get_arg([$ |Rest], Vars, Stop, Acc) ->		
    get_arg(Rest, Vars, Stop, Acc);
get_arg([$(|Rest], Vars, Stop, _) ->		
    get_arg(Rest, Vars, Stop, []); 
get_arg([Stop|Rest], Vars, Stop, Acc) ->
    Arg = lists:reverse(Acc),
    Subst = subst(Arg, Vars),
    {Subst,Rest};
get_arg([C|Rest], Vars, Stop, Acc) ->
    get_arg(Rest, Vars, Stop, [C|Acc]).

%% keep only the true or false conditional clause
extract_clause(true, Rest) ->
    extract_clause(true, Rest, []);
extract_clause(false, Rest) ->
    Rest1 = discard_clause(Rest),		% discard true clause
    extract_clause(false, Rest1, []).

%% true clause buffered, done
extract_clause(true, [$@,$E,$L,$S,$E,$@|Rest], Acc) -> 
    Rest1 = discard_clause(Rest),		% discard false clause
    lists:reverse(Acc, Rest1);
%% buffering of false clause starts now
extract_clause(false, [$@,$E,$L,$S,$E,$@|Rest], _Acc) -> 
    extract_clause(false, Rest, []);
%% true clause buffered, done
extract_clause(true, [$@,$E,$N,$D,$I,$F,$@|Rest], Acc) -> 
    lists:reverse(Acc, Rest);
%% false clause buffered, done
extract_clause(false, [$@,$E,$N,$D,$I,$F,$@|Rest], Acc) -> 
    lists:reverse(Acc, Rest);
%% keep buffering
extract_clause(Bool, [C|Rest], Acc) ->
    extract_clause(Bool, Rest, [C|Acc]);
%% parse error
extract_clause(_, [], Acc) ->			
    lists:reverse(Acc).

discard_clause([$@,$E,$L,$S,$E,$@|Rest]) ->
    Rest;
discard_clause([$@,$E,$N,$D,$I,$F,$@|Rest]) ->
    Rest;
discard_clause([_C|Rest]) ->
    discard_clause(Rest);
discard_clause([]) ->				% parse error
    [].


print_data(Port) ->
    receive
	{Port, {data, Bytes}} ->
	    io:put_chars(Bytes),
	    print_data(Port);
	{Port, eof} ->
	    Port ! {self(), close}, 
	    receive
		{Port, closed} ->
		    true
	    end, 
	    receive
		{'EXIT',  Port,  _} -> 
		    ok
	    after 1 ->				% force context switch
		    ok
	    end
    end.

maybe_atom_to_list(To_list) when is_list(To_list) ->
    To_list;
maybe_atom_to_list(To_list) when is_atom(To_list)->
    atom_to_list(To_list).
    
