%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-module(ic_options).

-include_lib("ic/src/ic.hrl").
-include_lib("kernel/include/file.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([defaultBe/0, float_to_version/1, get_opt/2, add_opt/3, 
	 read_cfg/2, which_opts/1, allowed_opt/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% Option handling
%%
%% Valid options are: (those with * is NotYetImpl)
%%
%% pedantic - makes the compiler really nitty-gritty about its input
%%
%% Wall - those warning options that we feel an IDL programmer should
%% care about. Not as picky as pedantic
%%
%% warn_multi_mod - warn if several modules are declared in the same
%% IDL file
%%
%% warn_nested_mod - warn if there are nested modules. This is not a
%% problem but it breakes the rule that modules are put into one file
%% each.
%%
%% warn_name_shadow - warn if identifiers are shadow through inherited
%% interfaces. Default is true.
%%
%% warn_quoted_atom - warn if atoms needs quote, this makes Erlang
%% code less nice but is certainly no error.
%%
%% nowarn - suppress all warning messages. Will still output warnings
%% if silent2 option is used
%%
%% always_outargs - force object server implementation return the
%% tuple {RetVal, OutArgs, NewState} even if there are no OutArgs. If
%% this option is not set then such an operation implementation is
%% assumed to return {RetVal, NewState}
%%
%% use_proc_dict - use the process dictionary in the client
%% stubs. This means that client stubs return RetVal instead of {ok,
%% RetVal, OutArgs} and that corba:get_outargs() returns OutArgs. The
%% out arguments are stored with the key '$corba_outargs'.
%%
%% module_group - use the top module as file name for both skeletons
%% and stubs. Default value is false which means that each interface
%% is put in a separate file.
%%
%% skel_module_group - group all interfaces in a module in one
%% skeleton file as opposed to one skeleton file for each
%% interface. Defaults to false.
%%
%% stub_module_group - group all interface stubs from a module in one
%% stub file as opposed to one stub file for each interface. Default
%% is false.
%%
%% *help - prints a small summary of the compiler usage
%%
%% silent - suppresses all messages from the compiler
%%
%% silent2 - suppresses all messages from the compiler and returns all
%% warnings or errors as lists. Returns {ok, WarnList} or {error,
%% WarnList, ErrList}
%%
%% *noexec - runs the compiler but does not open files or write to
%% files.
%%
%% {serv, <ModName>} - sets the name of the implementation skeleton
%% file. This defaults to ModName_skel.
%%
%% {impl, <ModName>} - sets the name of the interface server
%% implementation module name. This defaults to InterfaceName_impl
%%
%% {outdir, Dir} - use Dir as the directory to put all generated
%% files.
%%
%% {servdir, Dir} - put all generated skel files in the directory Dir.
%%
%% {stubdir, Dir} - put all generated stub files in the directory Dir.
%%
%% {this, InterfaceOrOpName} - puts the OE_THIS parameter into the
%% impl. call. This option can be used both on whole interfaces an on
%% distinct operations. Fullscoped names must be used (as in {this,
%% "M1::I1::Op"}). The option can be given in 3 ways: {this, Name}
%% means this will be added to all matching Name or as {{this, Name},
%% true} or this can explicitly be asked to be left out as in {{this,
%% Name}, false} which enables OE_THIS to be passed to all ops of an
%% interface except those set by the false flag.
%%
%% cfgfile - sets the name of the config file that is read at
%% startup. The order of the different ways to set options is: default
%% setting, configuration file, options given when generator is
%% called. Default name for this file is .ic_config
%%
%% serv_last_call - tells what the last handle_call clause should
%% do. It can have the values exception, which makes the last clause
%% return a CORBA exception and exit which does not generate a last clause
%% (which will make the server crash on an unknown call)
%%
%%
%% -- UNDOCUMENTED --
%%
%% debug - prints debug information
%%
%% tokens - prints the tokens from the tokenizer and then exit
%%
%% form - prints the form from the parser and then exit
%%
%% tform - form returned from type check
%%
%% time - if true then time is measured during compilation
%%
%% 
%%--------------------------------------------------------------------
allowed_opt(default_opts, _V)		-> true;
allowed_opt(debug, V)			-> is_bool(V);
allowed_opt(tokens, V)			-> is_bool(V);
allowed_opt(form, V)			-> is_bool(V);
allowed_opt(tform, V)			-> is_bool(V);
allowed_opt(time, V)			-> is_bool(V);
allowed_opt(maxerrs, V)			-> is_intorinfinity(V);
allowed_opt(maxwarns, V)		-> is_intorinfinity(V);
allowed_opt(nowarn, V)			-> is_bool(V);
allowed_opt(show_opts, V)		-> is_bool(V);

allowed_opt(help, V)			-> is_bool(V);
allowed_opt('Wall', V)			-> is_bool(V);
allowed_opt(warn_multi_mod, V)		-> is_bool(V);
allowed_opt(warn_quoted_atom, V)	-> is_bool(V);
allowed_opt(warn_nested_mod, V)		-> is_bool(V);
allowed_opt(warn_name_shadow, V)	-> is_bool(V);
allowed_opt(module_group, V)		-> is_bool(V);
allowed_opt(skel_module_group, V)	-> is_bool(V);
allowed_opt(stub_module_group, V)	-> is_bool(V);
allowed_opt(always_outargs, V)		-> is_bool(V);
allowed_opt(pedantic, V)		-> is_bool(V);
%%allowed_opt(gen_serv, V)		-> is_bool(V);
%%allowed_opt(gen_stub, V)		-> is_bool(V);
allowed_opt(gen_hrl, V)			-> is_bool(V);
allowed_opt(serv_last_call, exception)	-> true;
allowed_opt(serv_last_call, exit)	-> true;
allowed_opt(silent, V)			-> is_bool(V);
allowed_opt(silent2, V)			-> is_bool(V);
allowed_opt({serv, _}, _V)		-> true;
allowed_opt({impl, _}, _V)		-> true;
allowed_opt(outdir, _V)			-> true;
allowed_opt(servdir, _V)		-> true;
allowed_opt(stubdir, _V)		-> true;
allowed_opt(cfgfile, _V)		-> true;
allowed_opt(use_preproc, V)		-> is_bool(V);
allowed_opt(preproc_cmd, _V)		-> true;
allowed_opt(preproc_flags, _V)		-> true;
allowed_opt(this, _V)			-> true;
allowed_opt({this, _}, V)		-> is_bool(V);
allowed_opt(from, _V)			-> true;
allowed_opt({from, _}, V)		-> is_bool(V);
allowed_opt(handle_info, _V)		-> true;
allowed_opt({handle_info, _}, V)	-> is_bool(V);
allowed_opt(timeout, _V)	        -> true;
allowed_opt({timeout, _}, V)     	-> is_bool(V);
allowed_opt(c_timeout, {V1, V2})	-> is_int(V1) and is_int(V2);
allowed_opt(c_timeout, V)               -> is_int(V);
allowed_opt(c_report, V)             	-> is_bool(V);
allowed_opt(scoped_op_calls, V)         -> is_bool(V);
% Compatibility option (semantic check limitation)
allowed_opt(scl, V)                     -> is_bool(V);
% Added switches for non corba generation
allowed_opt(flags, V)                   -> is_int(V);
allowed_opt(be, erl_corba)	        -> true;
allowed_opt(be, erl_template)	        -> true;
allowed_opt(be, erl_genserv)	        -> true;
allowed_opt(be, c_genserv)		-> true;
allowed_opt(be, erl_plain)		-> true; 
allowed_opt(be, c_server)		-> true;
allowed_opt(be, c_client)		-> true;
allowed_opt(be, java)    		-> true;
% Noc backend
allowed_opt(be, noc)  	                -> true; 
allowed_opt({broker,_},{_,transparent}) -> true; 
allowed_opt({broker,_},{_,Term})  	-> is_term(Term);
allowed_opt({use_tk,_},V)               -> is_bool(V);
%
% Multiple be
allowed_opt(multiple_be, _List)    	-> true;
%
allowed_opt(precond, {_M, _F})          -> true;
allowed_opt({precond, _}, {_M, _F})     -> true;
allowed_opt(postcond, {_M, _F})         -> true;
allowed_opt({postcond, _}, {_M, _F})    -> true;
allowed_opt(no_codechange, V)           -> is_bool(V);
allowed_opt(user_protocol, _V)		-> true;
allowed_opt(light_ifr, V)               -> is_bool(V);
allowed_opt(_, _)			-> false.


-define(DEFAULTCFGFILE, ".ic_config").

which_opts(G) ->
    ets:match(G#genobj.options, {{option, '$1'}, '$2'}).

add_opt(G, KList, Val) when is_list(KList) ->
    lists:foreach(fun({K, V}) -> add_opt(G, K, V);
		  (K) -> add_opt(G, K, Val) end,
		  KList);

add_opt(G, servdir, V) ->
    do_add_opt(G, servdir, assure_directory(G, ic_util:to_list(V)));
add_opt(G, stubdir, V) ->
    do_add_opt(G, stubdir, assure_directory(G, ic_util:to_list(V)));
add_opt(G, K, V) ->
    do_add_opt(G, K, V).


assure_directory(_G, Dir) ->
    Dirs = filename:split(Dir),
    check_dirs(Dirs, [], filename:pathtype(Dir)).
    
check_dirs([X | Xs], SoFar, Type) ->
    New = if  SoFar == [], Type /= absolute ->
		  X;
	      true ->
		  filename:join(SoFar, X)
	  end,
    assert_dir(New),
    check_dirs(Xs, New, Type);
check_dirs([], SoFar, _Type) ->
    SoFar.

assert_dir(D) ->
    case file:read_file_info(D) of
	{ok, X} when X#file_info.type == directory -> ok;
	_ -> case file:make_dir(D) of
		 ok -> ok;
		 _ -> exit({could_not_create, D})
	     end
    end.

do_add_opt(G, handle_info, V) ->
    ?insert(G#genobj.options, {option, {handle_info, V}}, true);
do_add_opt(G, {handle_info, V}, false) ->
    ?insert(G#genobj.options, {option, {handle_info, V}}, force_false);
do_add_opt(G, timeout, V) ->
    ?insert(G#genobj.options, {option, {timeout, V}}, true);
do_add_opt(G, {timeout, V}, false) ->
    ?insert(G#genobj.options, {option, {timeout, V}}, force_false);
do_add_opt(G, this, V) ->
    ?insert(G#genobj.options, {option, {this, V}}, true);
do_add_opt(G, {this, V}, false) ->
    ?insert(G#genobj.options, {option, {this, V}}, force_false);
do_add_opt(G, from, V) ->
    ?insert(G#genobj.options, {option, {from, V}}, true);
do_add_opt(G, {from, V}, false) ->
    ?insert(G#genobj.options, {option, {from, V}}, force_false);
do_add_opt(G, scoped_op_calls, V) when V /= true, V /= false ->
    ?insert(G#genobj.options, {option, {scoped_op_calls, V}}, false);
do_add_opt(G, K, V) ->
    case allowed_opt(K, V) of
	true ->
	    case expand_opt(K) of
		L when is_list(L) ->
		    add_opt(G, L, V);
		_ ->
		    %%io:format("Add opt: ~p ~p~n", [K, V]),
		    ?insert(G#genobj.options, {option, K}, V)
	    end;
	_ ->
	    ic_error:warn(G, {illegal_opt, K})
    end.

get_opt(G, K) ->
    case ets:lookup(G#genobj.options, {option, K}) of
	[] -> false;
	[{{_, K}, V}] -> V
    end.

expand_opt(pedantic) -> [warn_multi_mod, warn_quoted_atom, always_outargs];
expand_opt(module_group) -> [skel_module_group, stub_module_group];
expand_opt('Wall') -> [warn_multi_mod, warn_nested_mod, warn_name_shadow];
expand_opt(outdir) -> [servdir, stubdir];
expand_opt(default_opts) -> 
    ['Wall', gen_hrl, {serv_last_call, exception},
     {outdir, []}, use_preproc, {preproc_cmd, "erl"}, 
     {preproc_flags, ""}, {maxerrs, 10}, {maxwarns, infinity}];
%% gcc preproc command {preproc_cmd, "gcc -x c++ -E"}
expand_opt(Opt) -> Opt.


%% Use this if user not provide 
%% a backend.
defaultBe() -> erl_corba.
    

%%
%% Read any config file
read_cfg(G, Opts) ->
    Name = case lists:keysearch(cfgfile, 1, Opts) of
	       {value, {_, N}} -> ic_util:to_list(N);
	       _ -> ?DEFAULTCFGFILE
	   end,
    case file:consult(Name) of
	{ok, OptList} ->
	    add_opt(G, OptList, true);
	_X when Name == ?DEFAULTCFGFILE -> ok;
%%	{error, X} ->
%%	    ic_error:warn(G, {cfg_open, X, Name});
	X -> ic_error:warn(G, {cfg_open, X, Name})
    end.


float_to_version({_,_,Str}) -> Str.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
is_bool(true) -> true;
is_bool(false) -> true;
is_bool(_) -> false.

is_int(V) when is_integer(V) -> true;
is_int(_) -> false.

is_intorinfinity(X) when is_integer(X) -> true;
is_intorinfinity(infinity) -> true;
is_intorinfinity(_X) -> false.


is_term(Term) when is_tuple(Term) -> true;
is_term(_NoTerm) -> false.
    
