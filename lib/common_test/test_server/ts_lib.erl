%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(ts_lib).

-include_lib("kernel/include/file.hrl").
-include("ts.hrl").

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([error/1, var/2, erlang_type/0,
	 erlang_type/1,
	 initial_capital/1,
	 specs/1, suites/2,
	 test_categories/2, specialized_specs/2,
	 subst_file/3, subst/2, print_data/1,
	 make_non_erlang/2,
	 maybe_atom_to_list/1, progress/4,
	 b2s/1
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
    erlang_type(code:root_dir()).
erlang_type(RootDir) ->
    {_, Version} = init:script_id(),
    RelDir = filename:join(RootDir, "releases"), % Only in installed
    case filelib:is_file(RelDir) of
	true -> {otp,Version};			% installed OTP
	false -> {srctree,Version}		% source code tree
    end.
	        
%% Upcases the first letter in a string.

initial_capital([C|Rest]) when $a =< C, C =< $z ->
    [C-$a+$A|Rest];
initial_capital(String) ->
    String.

specialized_specs(Dir,PostFix) ->
    Specs = filelib:wildcard(filename:join([filename:dirname(Dir),
					    "*_test", "*_"++PostFix++".spec"])),
    sort_tests([begin
		    DirPart = filename:dirname(Name),
		    AppTest = hd(lists:reverse(filename:split(DirPart))),
		    list_to_atom(string:slice(AppTest, 0, string:length(AppTest)-5))
		end || Name <- Specs]).

specs(Dir) ->
    Specs = filelib:wildcard(filename:join([filename:dirname(Dir),
					    "*_test", "*.{dyn,}spec"])),
    %% Make sure only to include the main spec for each application
    MainSpecs =
	lists:flatmap(fun(FullName) ->
			      [Spec,TestDir|_] =
				  lists:reverse(filename:split(FullName)),
			      [_TestSuffix|TDParts] = 
				  lists:reverse(string:lexemes(TestDir,[$_,$.])),
			      [_SpecSuffix|SParts] = 
				  lists:reverse(string:lexemes(Spec,[$_,$.])),
			      if TDParts == SParts ->
				      [filename_to_atom(FullName)];	  
				 true ->
				      []
			      end
		      end, Specs),

    sort_tests(filter_tests(MainSpecs)).

test_categories(Dir, App) ->
    Specs = filelib:wildcard(filename:join([filename:dirname(Dir),
					    App++"_test", "*.spec"])),
    lists:flatmap(fun(FullName) ->
			  [Spec,_TestDir|_] =
			      lists:reverse(filename:split(FullName)),	 
			  case filename:rootname(Spec -- App) of
			      "" ->
				  [];
			      [_Sep | Cat] ->
				  [list_to_atom(Cat)]
			  end
		  end, Specs).

suites(Dir, App) ->
    Glob=filename:join([filename:dirname(Dir), App++"_test",
			"*_SUITE.erl"]),
    Suites=filelib:wildcard(Glob),
    [filename_to_atom(Name) || Name <- Suites].

filename_to_atom(Name) ->
    list_to_atom(filename:rootname(filename:basename(Name))).

%% Filter out tests of applications that are not accessible

filter_tests(Tests) ->
    lists:filter(
      fun(Special) when Special == epmd;
                        Special == emulator;
                        Special == system ->
              true;
         (Test) ->
              case application:load(filename_to_atom(Test)) of
                  {error, {already_loaded, _}} ->
                      true;
                  {error,_NoSuchApplication} ->
                      false;
                  _ ->
                      true
              end
      end, Tests).

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
suite_order(debugger) -> 22;
suite_order(ic) -> 24;
suite_order(orber) -> 26;
suite_order(inets) -> 28;
suite_order(asn1) -> 30;
suite_order(os_mon) -> 32;
suite_order(snmp) -> 38;
suite_order(mnesia) -> 44;
suite_order(system) -> 999; %% IMPORTANT: system SHOULD always be last!
suite_order(_) -> 200.

%% Substitute all occurrences of @var@ in the In file, using
%% the list of variables in Vars, producing the output file Out.
%% Returns: ok | {error, Reason}

subst_file(In, Out, Vars) ->
    case file:read_file(In) of
	{ok, Bin} ->
	    Subst = subst(b2s(Bin), Vars, []),
	    case file:write_file(Out, unicode:characters_to_binary(Subst)) of
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
get_arg([$(|Rest], Vars, Stop, _) ->		
    get_arg(Rest, Vars, Stop, []); 
get_arg([Stop|Rest], Vars, Stop, Acc) ->
    Arg = string:trim(lists:reverse(Acc),both,[$\s]),
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
    

%% Configure and run all the Makefiles in the data dir of the suite
%% in question
make_non_erlang(DataDir, Variables) ->
    %% Make the stuff in all_SUITE_data if it exists
    AllDir = filename:join(DataDir,"../all_SUITE_data"),
    case filelib:is_dir(AllDir) of
	true ->
	    make_non_erlang_do(AllDir,Variables);
	false ->
	    ok
    end,
    make_non_erlang_do(DataDir, Variables).

make_non_erlang_do(DataDir, Variables) ->
    try
	MakeCommand = proplists:get_value(make_command,Variables),

	FirstMakefile = filename:join(DataDir,"Makefile.first"),
	case filelib:is_regular(FirstMakefile) of
	    true ->
		io:format("Making ~p",[FirstMakefile]),
		ok = ts_make:make(
		       MakeCommand, DataDir, filename:basename(FirstMakefile));
	    false ->
		ok
	end,

	MakefileSrc = filename:join(DataDir,"Makefile.src"),
	MakefileDest = filename:join(DataDir,"Makefile"),
	case filelib:is_regular(MakefileSrc) of
	    true ->
		ok = ts_lib:subst_file(MakefileSrc,MakefileDest,Variables),
		io:format("Making ~p",[MakefileDest]),
		ok = ts_make:make([{makefile,"Makefile"},{data_dir,DataDir}
				   | Variables]);
	    false ->
		ok
	end
    after
	timer:sleep(100)  %% maybe unnecessary now when we don't do set_cwd anymore
    end.

b2s(Bin) ->
    unicode:characters_to_list(Bin,default_encoding()).

default_encoding() ->
    try epp:default_encoding()
    catch error:undef -> latin1
    end.
