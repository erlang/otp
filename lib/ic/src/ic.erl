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
%%
-module(ic).


-export([sgen/1, gen/1, gen/2, help/0, compile/3]).


%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------

-export([filter_params/2, handle_preproc/4, do_gen/4]).

-import(lists, [foldr/3]).


-include("icforms.hrl").
-include("ic.hrl").

-include_lib("stdlib/include/erl_compile.hrl").

-export([make_erl_options/1]).			% For erlc

-export([main/3, do_scan/1, do_parse/2, do_type/2]).


%%------------------------------------------------------------
%%
%% Entry point
%%
%%------------------------------------------------------------

%% compile(AbsFileName, Outfile, Options)
%%   Compile entry point for erl_compile.

compile(File, _OutFile, Options) ->
    case gen(File, make_erl_options(Options)) of
	ok -> ok;
	Other -> Other
    end.


%% Entry for the -s switch
sgen(ArgList) ->
%%%    io:format("sgen called w ~p~n", [ArgList]),
    apply(?MODULE, gen, ArgList).


gen(File) ->
    gen(File, []).

gen(File, Opts) ->
    G = ic_genobj:new(Opts),
    IdlFile = ic_file:add_dot_idl(File),
    case ic_options:get_opt(G, show_opts) of
	true ->
	    io:format("Opts: ~p~n", [ic_options:which_opts(G)]);
	_ -> ok
    end,
    ic_genobj:set_idlfile(G, IdlFile),
    case catch gen2(G, File, Opts) of
	{_, {'EXIT', R}} -> 
	    ic_genobj:free_table_space(G), %% Free space for all ETS tables
	    io:format("Fatal error : ~p~n",[R]),
	    error;
	{_, {'EXIT', _, R}} -> 
	    ic_genobj:free_table_space(G), %% Free space for all ETS tables
	    io:format("Fatal error : ~p~n",[R]),
	    error;
	{'EXIT', R} -> 
	    ic_genobj:free_table_space(G), %% Free space for all ETS tables
	    io:format("Fatal error : ~p~n",[R]),
	    error;
	{'EXIT', _, R} -> 
	    ic_genobj:free_table_space(G), %% Free space for all ETS tables
	    io:format("Fatal error : ~p~n",[R]),
	    error;	
	%% In this case, the pragma registration 
        %% found errors so this should return error.
	error ->
	    ic_genobj:free_table_space(G), %% Free space for all ETS tables
	    error;
	_ -> 
	    X = ic_error:return(G),
	    ic_genobj:free_table_space(G), %% Free space for all ETS tables
	    X
    end.


gen2(G, File, Opts) ->
    case ic_options:get_opt(G, time) of
	true -> 
	    time("TOTAL                ", ic, main, [G, File, Opts]);
	_ -> 
	    case main(G, File, Opts) of
		error ->
		    error;
		_ ->
		    ok
	    end
    end.



do_gen(erl_corba, G, File, T) ->
    ic_erlbe:do_gen(G, File, T);
do_gen(erl_template, G, File, T) ->
    ic_erl_template:do_gen(G, File, T);
do_gen(erl_genserv, G, File, T) ->
    ic_erlbe:do_gen(G, File, T);
do_gen(c_genserv, G, File, T) ->
    ic_cclient:do_gen(G, File, T);
do_gen(noc, G, File, T) ->
    ic_noc:do_gen(G, File, T);
do_gen(erl_plain, G, File, T) ->
    ic_plainbe:do_gen(G, File, T);
do_gen(c_server, G, File, T) ->
    ic_cserver:do_gen(G, File, T);
do_gen(c_client, G, File, T) ->
    ic_cclient:do_gen(G, File, T);
%% Java backend
do_gen(java, G, File, T) ->
    ic_jbe:do_gen(G, File, T);
%% No language choice
do_gen(_,_,_,_) -> 
    ok.

do_scan(G) ->
    icscan:scan(G, ic_genobj:idlfile(G)).
    

do_parse(G, Tokens) ->
    case icparse:parse(Tokens) of
	{ok, L} -> L;
	X when element(1, X) == error -> 
	    Err = element(2, X),
	    ic_error:fatal_error(G, {parse_error, element(1, Err), 
				  element(3, Err)});
	X -> exit(X)
    end.


do_type(G, Form) ->
    ictype:type_check(G, Form).
	
time(STR,M,F,A) ->
    case timer:tc(M, F, A) of
	{_, {'EXIT', R}} -> exit(R);
	{_, {'EXIT', _, R}} -> exit(R);
	{_, _X} when element(1, _X)==error -> throw(_X);
	{_T, _R} -> 
	    io:format("Time for ~s:  ~10.2f~n", [STR, _T/1000000]),
	    _R
    end.



%% Filters parameters so that only those with certain attributes are
%% seen. The filter parameter is a list of attributes that will be
%% seen, ex. [in] or [inout, out]
filter_params(Filter, Params) ->
    lists:filter(fun(P) ->
		    lists:member(get_param_attr(P#param.inout), Filter) end,
		 Params).


%% Access primitive to get the attribute name (and discard the line
%% number).
get_param_attr({A, _N}) -> A.


%%
%% Fixing the preproc directives
%%
handle_preproc(G, _N, line_nr, X) ->
    Id = ic_forms:get_id2(X),
    Flags = X#preproc.aux,
    case Flags of
	[] -> ic_genobj:push_file(G, Id);
	_ ->
	    foldr(fun({_, _, "1"}, Gprim) -> ic_genobj:push_file(Gprim, Id);
		     ({_, _, "2"}, Gprim) -> ic_genobj:pop_file(Gprim, Id);
		     ({_, _, "3"}, Gprim) -> ic_genobj:sys_file(Gprim, Id) end,
		  G, Flags)
    end;
handle_preproc(G, _N, _Other, _X) ->
    G.



%%------------------------------------------------------------
%%
%% The help department
%%
%% 
%%
%%------------------------------------------------------------

help() ->
    io:format("No help available at the moment~n", []),
    ok.

print_version_str(G) ->
    case {ic_options:get_opt(G, silent), ic_options:get_opt(G, silent2)} of
	{true, _} -> ok;
	{_, true} -> ok;
	_ -> 
	    io:format("Erlang IDL compiler version ~s~n", [?COMPILERVSN])
    end.



%%
%% Converts generic compiler options to specific options.
%% 
%% Used by erlc
%%

make_erl_options(Opts) ->

    %% This way of extracting will work even if the record passed
    %% has more fields than known during compilation.

    Includes1 = Opts#options.includes,
    Defines = Opts#options.defines,
    Outdir = Opts#options.outdir,
    Warning = Opts#options.warning,
    Verbose = Opts#options.verbose,
    Specific = Opts#options.specific,
    Optimize = Opts#options.optimize,
    PreProc = 
	lists:flatten(
	  lists:map(fun(D) -> io_lib:format("-I\"~ts\" ", [ic_util:to_list(D)]) end,
		    Includes1)++
	  lists:map(
	    fun ({Name, Value}) ->
		    io_lib:format("-D~s=~s ", [ic_util:to_list(Name), ic_util:to_list(Value)]);
		(Name) ->
		    io_lib:format("-D~s ", [ic_util:to_list(Name)])
	    end,
	    Defines)),
    Options =
	case Verbose of
	    true ->  [];
	    false -> []
	end ++
	case Warning of
	    0 -> [nowarn];
	    _ -> ['Wall']
	end ++
	case Optimize of
	    0 -> [];
	    _ -> []
	end,
    
    Options++[{outdir, Outdir}, {preproc_flags, PreProc}]++Specific.


%%%
%%% NEW main, avoids memory fragmentation
%%%
main(G, File, _Opts) ->
    print_version_str(G),
    ?ifopt(G, time, io:format("File ~p compilation started  :   ~p/~p/~p ~p:~2.2.0p~n", 
			      [ic_genobj:idlfile(G),
			       element(1,date()),
			       element(2, date()),
			       element(3, date()),
			       element(1, time()),
			       element(2, time())])),

    case ic_options:get_opt(G, help) of
	true -> help();

	_ ->
	    scanning(G, File)
    end.



scanning(G, File) ->
    S = ?ifopt2(G, time,
	       time("input file scanning  ", ic, do_scan, [G]),
	       ic:do_scan(G)),
    ?ifopt2(G, tokens, io:format("TOKENS: ~p~n", [S]), 
				 parsing(G, File, S)).

parsing(G, File, S) ->
    T = ?ifopt2(G, 
		time, 
		time("input file parsing   ", ic, do_parse, [G,S]),
		ic:do_parse(G,S)),
    ?ifopt2(G, form, io:format("PARSE FORM: ~p~n", [T]), 
	     pragma(G, File, T)).



pragma(G, File, T) ->
    case ?ifopt2(G, 
		 time,
		 time("pragma registration  ", ic_pragma, pragma_reg, [G,T]),
		 ic_pragma:pragma_reg(G,T)) of
	%% All pragmas were successfully applied
	{ok,Clean} ->
	    typing(G, File, Clean);
       
	error ->
	    error
    end.


typing(G, File, Clean) ->
    case catch ?ifopt2(G, 
		       time,
		       time("type code appliance  ", ic, do_type, [G,Clean]),
		       ic:do_type(G,Clean)) of
	{'EXIT',Reason} ->
	    io:format("Error under type appliance : ~p~n",[Reason]),
	    error;
       
	T2 ->	    
	    ?ifopt2(G, tform, io:format("TYPE FORM: ~p~n", [T2]),
		    generation(G, File, T2))
    end.



generation(G, File, T2) ->
    case ic_options:get_opt(G, multiple_be) of
	false ->
	    single_generation(G, File, T2);
	List ->
	    OutDir = 
		case ic_options:get_opt(G, outdir) of
		    false -> 
			[];
		    Dir ->
			Dir
		end,
	    
	    case ic_options:get_opt(G, be) of
		false ->
		    ok;
		Be ->
		    %% Generate this first
		    ic_options:add_opt(G,[{outdir,OutDir++atom_to_list(Be)}],true),
		    single_generation(G, File, T2)
	    end,
	    multiple_generation(G, File, T2, OutDir, List)
    end.

multiple_generation(_G, _File, _T2, _RootDir, []) ->
    ok;
multiple_generation(G, File, T2, RootDir, [Be|Bes]) ->
    ic_options:add_opt(G,[{outdir,RootDir++atom_to_list(Be)}],true),
    ic_options:add_opt(G,[{be,Be}],true),
    single_generation(G, File, T2),

    case ic_error:get_error_count(G) of
	0 ->
	     multiple_generation(G,File,T2,RootDir,Bes);
	 _ ->
	     %% Errors reported, abort
	     ok
     end.


single_generation(G, File, T2) ->
    case ic_error:get_error_count(G) of
	0 ->
	    %% Check if user has sett backend option
	    case ic_options:get_opt(G, be) of
		false ->
		    %% Use default backend option
		    DefaultBe = ic_options:defaultBe(),
		    ic_options:add_opt(G,[{be,DefaultBe}],true),
		    
		    ?ifopt2(G, 
			    time,
			    time("code generation      ", ic, do_gen, [DefaultBe, G, File, T2]),
			    ic:do_gen(DefaultBe, G, File, T2));
		Be ->
		    %% Use user defined backend		    
		    ?ifopt2(G, 
			    time,
			    time("code generation      ", ic, do_gen, [Be, G, File, T2]),
			    ic:do_gen(Be, G, File, T2))
	    end;
	_ ->
	    ok	    %% Does not matter
    end.
	


