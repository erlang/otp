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

-module(ic_genobj).


-include_lib("ic/src/ic.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([new/1, free_table_space/1, process_space/0]).
-export([skelfiled/1, stubfiled/1, hrlfiled/1, includefiled/1]).
-export([interfacefiled/1, helperfiled/1, holderfiled/1]).
-export([is_skelfile_open/1, is_stubfile_open/1, is_hrlfile_open/1]).
-export([include_file/1, include_file_stack/1]).
-export([push_file/2, pop_file/2, sys_file/2]).

-export([skelscope/1, stubscope/1, impl/1, do_gen/1]).
-export([symtab/1, auxtab/1, tktab/1, pragmatab/1, optiontab/1, typedeftab/1]).
-export([idlfile/1, module/1, set_idlfile/2, set_module/2]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% Initialisation stuff
%%
%% 
%%
%%--------------------------------------------------------------------


new(Opts) ->
    OptDB	= ets:new(options, [set, public]),
    Warns	= ets:new(warnings, [set, public]),
    Aux		= ets:new(aux, [set, public]),
    Tk		= ets:new(tktab, [set, public]),
    PragmaTab   = ets:new(pragmatab, [bag, public]),
    TypeDefTab  = ets:new(c_typedeftab, [set, public]),
    G = #genobj{options=OptDB, 
		warnings=Warns, 
		symtab=ic_symtab:new(), 
		auxtab=Aux, 
		tktab=Tk, 
		pragmatab=PragmaTab,
		c_typedeftab=TypeDefTab},
    ic_error:init_errors(G),
    ic_options:add_opt(G, default_opts, true),
    ic_options:read_cfg(G, Opts),	       % Read any config files
    ic_options:add_opt(G, Opts, true),
    ic_symtab:symtab_add_faked_included_types(G), % Add CORBA::<Types> that as if they
                                                  % were defined in an included file
    case ic_options:get_opt(G, be) of
	false ->
	    DefBE = ic_options:defaultBe(),
	    case ic_options:get_opt(G, multiple_be) of
		false ->
		    ic_options:add_opt(G, be, DefBE),
		    G;
		List ->
		    case lists:member(DefBE, List) of
			true ->
			    %% Delete the default be from the list to avoid
			    %% generating it twice.
			    NewList = lists:delete(DefBE, List),
			    ic_options:add_opt(G, multiple_be, NewList),
			    ic_options:add_opt(G, be, DefBE),
			    G;
			false ->
			    G
		    end
	    end;
	_ ->
	    G
    end.


%%--------------------------------------------------------------------
%%
%% Table removal
%%
%% 
%%
%%--------------------------------------------------------------------


free_table_space(G) ->
    %% Free ets tables
    ets:delete(G#genobj.options),
    ets:delete(G#genobj.symtab),
    ets:delete(G#genobj.warnings),
    ets:delete(G#genobj.auxtab),
    ets:delete(G#genobj.tktab),
    ets:delete(G#genobj.pragmatab),
    ets:delete(G#genobj.c_typedeftab),
    %% Close file descriptors
    close_fd(G#genobj.skelfiled),
    close_fd(G#genobj.stubfiled),
    close_fd(G#genobj.interfacefiled),
    close_fd(G#genobj.helperfiled),
    close_fd(G#genobj.holderfiled),
    close_fd(G#genobj.includefiled).

close_fd([]) ->
    ok;
close_fd([Fd|Fds]) ->
    file_close(Fd),
    close_fd(Fds).

file_close(empty) -> ok;
file_close(ignore) -> ok;
file_close(Fd) -> 
    file:close(Fd).


%%--------------------------------------------------------------------
%%
%% Process memory usage
%%
%% 
%%
%%--------------------------------------------------------------------

process_space() ->
    Pheap=4*element(2,element(2,lists:keysearch(heap_size,1,process_info(self())))),
    Pstack=4*element(2,element(2,lists:keysearch(stack_size,1,process_info(self())))),
    io:format("Process current heap = ~p bytes\n",[Pheap]),
    io:format("Symbol current stack = ~p bytes\n",[Pstack]),
    io:format("-----------------------------------------------\n"),
    io:format("Totally used  ~p bytes\n\n",[Pheap+Pstack]).






skelfiled(G) -> hd(G#genobj.skelfiled).
stubfiled(G) -> hd(G#genobj.stubfiled).
includefiled(G) -> hd(G#genobj.includefiled).
hrlfiled(G) ->  hd(G#genobj.includefiled).
interfacefiled(G) ->  hd(G#genobj.interfacefiled).
helperfiled(G) ->  hd(G#genobj.helperfiled).
holderfiled(G) ->  hd(G#genobj.holderfiled).

include_file(G) -> hd(G#genobj.includefile).
include_file_stack(G) -> G#genobj.includefile.

is_skelfile_open(G) ->
    if  hd(G#genobj.skelfiled) /= empty, hd(G#genobj.skelfiled) /= ignore
	-> true;
	true -> false
    end.
is_stubfile_open(G) ->
    if  hd(G#genobj.stubfiled) /= empty, hd(G#genobj.stubfiled) /= ignore
	-> true;
	true -> false
    end.

is_hrlfile_open(G) ->
    if  hd(G#genobj.includefiled) /= empty, hd(G#genobj.includefiled) /= ignore
	-> true;
	true -> false
    end.

%%--------------------------------------------------------------------
%%
%% Handling of pre processor file commands
%%
%%--------------------------------------------------------------------

push_file(G, Id) ->
    New = G#genobj.filestack+1,
    set_idlfile(G, Id),
    G#genobj{filestack=New, do_gen=true_or_not(New)}.
pop_file(G, Id) ->
    New = G#genobj.filestack-1,
    set_idlfile(G, Id),
    G#genobj{filestack=New, do_gen=true_or_not(New)}.
sys_file(G, _Id) -> G#genobj{sysfile=true}.


do_gen(G) -> G#genobj.do_gen.

%%--------------------------------------------------------------------
%%
%% Storage routines
%%i
%% The generator object G is used to store many usefull bits of
%% information so that the information doesn't need to get passed
%% around everywhere.
%%
%%--------------------------------------------------------------------


skelscope(G)	-> G#genobj.skelscope.
stubscope(G)	-> G#genobj.stubscope.
symtab(G)	-> G#genobj.symtab.
auxtab(G)	-> G#genobj.auxtab.
tktab(G)	-> G#genobj.tktab.
impl(G)		-> G#genobj.impl.
pragmatab(G)    -> G#genobj.pragmatab.
optiontab(G)    -> G#genobj.options.
typedeftab(G)    -> G#genobj.c_typedeftab.

idlfile(G)	-> ?lookup(G#genobj.options, idlfile).
module(G)	-> ?lookup(G#genobj.options, module).

set_idlfile(G, X)	-> ?insert(G#genobj.options, idlfile, X).
set_module(G, X)	-> ?insert(G#genobj.options, module, ic_forms:get_id(X)).


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
true_or_not(X) when X < 2 -> 
    true;
true_or_not(_) -> 
    false.
