%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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

-module(epp).

%% An Erlang code preprocessor.

-export([open/2,open/3,open/5,close/1,format_error/1]).
-export([scan_erl_form/1,parse_erl_form/1,macro_defs/1]).
-export([parse_file/1, parse_file/3]).
-export([interpret_file_attribute/1]).
-export([normalize_typed_record_fields/1,restore_typed_record_fields/1]).

%%------------------------------------------------------------------------

-type macros() :: [{atom(), term()}].

%% Epp state record.
-record(epp, {file,				%Current file
	      location,         		%Current location
	      name="",				%Current file name
	      istk=[],				%Ifdef stack
	      sstk=[],				%State stack
	      path=[],				%Include-path
	      macs = dict:new()  :: dict(),	%Macros (don't care locations)
	      uses = dict:new()  :: dict(),	%Macro use structure
	      pre_opened = false :: boolean()
	     }).

%%% Note on representation: as tokens, both {var, Location, Name} and
%%% {atom, Location, Name} can occur as macro identifiers. However, keeping
%%% this distinction here is done for historical reasons only: previously,
%%% ?FOO and ?'FOO' were not the same, but now they are. Removing the
%%% distinction in the internal representation would simplify the code
%%% a little.

%% open(FileName, IncludePath)
%% open(FileName, IncludePath, PreDefMacros)
%% open(FileName, IoDevice, StartLocation, IncludePath, PreDefMacros)
%% close(Epp)
%% scan_erl_form(Epp)
%% parse_erl_form(Epp)
%% parse_file(Epp)
%% parse_file(FileName, IncludePath, PreDefMacros)
%% macro_defs(Epp)

-spec open(file:name(), [file:name()]) ->
	{'ok', pid()} | {'error', term()}.

open(Name, Path) ->
    open(Name, Path, []).

-spec open(file:name(), [file:name()], macros()) ->
	{'ok', pid()} | {'error', term()}.

open(Name, Path, Pdm) ->
    Self = self(),
    Epp = spawn(fun() -> server(Self, Name, Path, Pdm) end),
    epp_request(Epp).

open(Name, File, StartLocation, Path, Pdm) ->
    Self = self(),
    Epp = spawn(fun() -> server(Self, Name, File, StartLocation,Path,Pdm) end),
    epp_request(Epp).

-spec close(pid()) -> 'ok'.

close(Epp) ->
    %% Make sure that close is synchronous as a courtesy to test
    %% cases that test for resource leaks.
    Ref = erlang:monitor(process, Epp),
    R = epp_request(Epp, close),
    receive {'DOWN',Ref,_,_,_} -> ok end,
    R.

scan_erl_form(Epp) ->
    epp_request(Epp, scan_erl_form).

parse_erl_form(Epp) ->
    case epp_request(Epp, scan_erl_form) of
	{ok,Toks} ->
	    erl_parse:parse_form(Toks);
	Other ->
	    Other
    end.

macro_defs(Epp) ->
    epp_request(Epp, macro_defs).

%% format_error(ErrorDescriptor) -> String
%%  Return a string describing the error.

format_error(cannot_parse) ->
    io_lib:format("cannot parse file, giving up", []);
format_error({bad,W}) ->
    io_lib:format("badly formed '~s'", [W]);
format_error({call,What}) ->
    io_lib:format("illegal macro call '~s'",[What]);
format_error({undefined,M}) ->
    io_lib:format("undefined macro '~w'", [M]);
format_error({depth,What}) ->
    io_lib:format("~s too deep",[What]);
format_error({mismatch,M}) ->
    io_lib:format("argument mismatch for macro '~w'", [M]);
format_error({arg_error,M}) ->
    io_lib:format("badly formed argument for macro '~w'", [M]);
format_error({redefine,M}) ->
    io_lib:format("redefining macro '~w'", [M]);
format_error({circular,M}) ->
    io_lib:format("circular macro '~w'", [M]);
format_error({include,W,F}) ->
    io_lib:format("can't find include ~s \"~s\"", [W,F]);
format_error({illegal,How,What}) ->
    io_lib:format("~s '-~s'", [How,What]);
format_error({'NYI',What}) ->
    io_lib:format("not yet implemented '~s'", [What]);
format_error(E) -> file:format_error(E).

%% parse_file(FileName, IncludePath, [PreDefMacro]) ->
%%	{ok,[Form]} | {error,OpenError}

parse_file(Ifile, Path, Predefs) ->
    case open(Ifile, Path, Predefs) of
	{ok,Epp} ->
	    Forms = parse_file(Epp),
	    close(Epp),
	    {ok,Forms};
	{error,E} ->
	    {error,E}
    end.

%% parse_file(Epp) ->
%%	[Form]

parse_file(Epp) ->
    case parse_erl_form(Epp) of
	{ok,Form} ->
	    case Form of
		{attribute,La,record,{Record, Fields}} ->
		    case normalize_typed_record_fields(Fields) of
			{typed, NewFields} ->
			    [{attribute, La, record, {Record, NewFields}},
			     {attribute, La, type, 
			      {{record, Record}, Fields, []}}
			     |parse_file(Epp)];
			not_typed ->
			    [Form|parse_file(Epp)]
		    end;
		_ ->
		    [Form|parse_file(Epp)]
	    end;
	{error,E} ->
	    [{error,E}|parse_file(Epp)];
	{eof,Location} ->
	    [{eof,Location}]
    end.

normalize_typed_record_fields(Fields) ->
    normalize_typed_record_fields(Fields, [], false).

normalize_typed_record_fields([], NewFields, Typed) ->
    case Typed of
	true -> {typed, lists:reverse(NewFields)};
	false -> not_typed
    end;
normalize_typed_record_fields([{typed_record_field,Field,_}|Rest], 
			      NewFields, _Typed) ->
    normalize_typed_record_fields(Rest, [Field|NewFields], true);
normalize_typed_record_fields([Field|Rest], NewFields, Typed) ->
    normalize_typed_record_fields(Rest, [Field|NewFields], Typed).

restore_typed_record_fields([]) ->
    [];
restore_typed_record_fields([{attribute,La,record,{Record,_NewFields}},
                             {attribute,La,type,{{record,Record},Fields,[]}}|
                             Forms]) ->
    [{attribute,La,record,{Record,Fields}}|
     restore_typed_record_fields(Forms)];
restore_typed_record_fields([{attribute,La,type,{{record,Record},Fields,[]}}|
                             Forms]) ->
    %% This clause is due to the compiler's 'E' option.
    %% Record information kept by erl_expand_records.
    [{attribute,La,record,{Record,Fields}}|
     restore_typed_record_fields(Forms)];
restore_typed_record_fields([Form|Forms]) ->
    [Form|restore_typed_record_fields(Forms)].

%% server(StarterPid, FileName, Path, PreDefMacros)

server(Pid, Name, Path, Pdm) ->
    process_flag(trap_exit, true),
    case file:open(Name, [read]) of
	{ok,File} ->
            Location = 1,
	    init_server(Pid, Name, File, Location, Path, Pdm, false);
	{error,E} ->
	    epp_reply(Pid, {error,E})
    end.

%% server(StarterPid, FileName, IoDevice, Location, Path, PreDefMacros)
server(Pid, Name, File, AtLocation, Path, Pdm) ->
    process_flag(trap_exit, true),
    init_server(Pid, Name, File, AtLocation, Path, Pdm, true).

init_server(Pid, Name, File, AtLocation, Path, Pdm, Pre) ->
    Ms0 = predef_macros(Name),
    case user_predef(Pdm, Ms0) of
	{ok,Ms1} ->
	    epp_reply(Pid, {ok,self()}),
	    St = #epp{file=File, location=AtLocation, name=Name,
                      path=Path, macs=Ms1, pre_opened = Pre},
	    From = wait_request(St),
	    enter_file_reply(From, Name, AtLocation, AtLocation),
	    wait_req_scan(St);
	{error,E} ->
	    epp_reply(Pid, {error,E})
    end.

%% predef_macros(FileName) -> Macrodict
%%  Initialise the macro dictionary with the default predefined macros,
%%  FILE, LINE, MODULE as undefined, MACHINE and MACHINE value.

predef_macros(File) ->
     Machine = list_to_atom(erlang:system_info(machine)),
     dict:from_list([
	{{atom,'FILE'}, 	      {none,[{string,1,File}]}},
	{{atom,'LINE'},		      {none,[{integer,1,1}]}},
	{{atom,'MODULE'},	      undefined},
	{{atom,'MODULE_STRING'},      undefined},
	{{atom,'BASE_MODULE'},	      undefined},
	{{atom,'BASE_MODULE_STRING'}, undefined},
	{{atom,'MACHINE'},	      {none,[{atom,1,Machine}]}},
	{{atom,Machine},	      {none,[{atom,1,true}]}}
     ]).

%% user_predef(PreDefMacros, Macros) ->
%%	{ok,MacroDict} | {error,E}
%%  Add the predefined macros to the macros dictionary. A macro without a
%%  value gets the value 'true'.

user_predef([{M,Val,redefine}|Pdm], Ms) when is_atom(M) ->
    Exp = erl_parse:tokens(erl_parse:abstract(Val)),
    user_predef(Pdm, dict:store({atom,M}, {none,Exp}, Ms));
user_predef([{M,Val}|Pdm], Ms) when is_atom(M) ->
    case dict:find({atom,M}, Ms) of
	{ok,_Def} ->
	    {error,{redefine,M}};
	error ->
	    Exp = erl_parse:tokens(erl_parse:abstract(Val)),
	    user_predef(Pdm, dict:store({atom,M}, {none,Exp}, Ms))
    end;
user_predef([M|Pdm], Ms) when is_atom(M) ->
    case dict:find({atom,M}, Ms) of
	{ok,_Def} ->
	    {error,{redefine,M}};
	error ->
	    user_predef(Pdm, dict:store({atom,M}, {none,[{atom,1,true}]}, Ms))
    end;
user_predef([Md|_Pdm], _Ms) -> {error,{bad,Md}};
user_predef([], Ms) -> {ok,Ms}.

%% wait_request(EppState) -> RequestFrom
%% wait_req_scan(EppState)
%% wait_req_skip(EppState, SkipIstack)
%%  Handle requests, processing trivial requests directly. Either return
%%  requestor or scan/skip tokens.

wait_request(St) ->
    receive
	{epp_request,From,scan_erl_form} -> From;
	{epp_request,From,macro_defs} ->
	    epp_reply(From, dict:to_list(St#epp.macs)),
	    wait_request(St);
	{epp_request,From,close} ->
	    close_file(St),
	    epp_reply(From, ok),
	    exit(normal);
	{'EXIT',_,R} ->
	    exit(R);
	Other ->
	    io:fwrite("Epp: unknown '~w'\n", [Other]),
	    wait_request(St)
    end.

close_file(#epp{pre_opened = true}) ->
    ok;
close_file(#epp{pre_opened = false, file = File}) ->
    ok = file:close(File).

wait_req_scan(St) ->
    From = wait_request(St),
    scan_toks(From, St).

wait_req_skip(St, Sis) ->
    From = wait_request(St),
    skip_toks(From, St, Sis).
	
%% enter_file(Path, FileName, IncludeToken, From, EppState)
%% leave_file(From, EppState)
%%  Handle entering and leaving included files. Notify caller when the
%%  current file is changed. Note it is an error to exit a file if we are
%%  in a conditional. These functions never return.

enter_file(_Path, _NewName, Inc, From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{abs_loc(Inc),epp,{depth,"include"}}}),
    wait_req_scan(St);
enter_file(Path, NewName, Inc, From, St) ->
    case file:path_open(Path, NewName, [read]) of
	{ok,NewF,Pname} ->
            Loc = start_loc(St#epp.location),
	    wait_req_scan(enter_file2(NewF, Pname, From, St, Loc));
	{error,_E} ->
	    epp_reply(From, {error,{abs_loc(Inc),epp,{include,file,NewName}}}),
	    wait_req_scan(St)
    end.

%% enter_file2(File, FullName, From, EppState, AtLocation) -> EppState.
%%  Set epp to use this file and "enter" it.

enter_file2(NewF, Pname, From, St, AtLocation) ->
    enter_file2(NewF, Pname, From, St, AtLocation, []).

enter_file2(NewF, Pname, From, St, AtLocation, ExtraPath) ->
    Loc = start_loc(AtLocation),
    enter_file_reply(From, Pname, Loc, AtLocation),
    Ms = dict:store({atom,'FILE'}, {none,[{string,Loc,Pname}]}, St#epp.macs),
    Path = St#epp.path ++ ExtraPath,
    #epp{location=Loc,file=NewF,
         name=Pname,sstk=[St|St#epp.sstk],path=Path,macs=Ms}.

enter_file_reply(From, Name, Location, AtLocation) ->
    Attr = loc_attr(AtLocation),
    Rep = {ok, [{'-',Attr},{atom,Attr,file},{'(',Attr},
		{string,Attr,file_name(Name)},{',',Attr},
		{integer,Attr,get_line(Location)},{')',Location},
                {dot,Attr}]},
    epp_reply(From, Rep).

%% Flatten filename to a string. Must be a valid filename.

file_name([C | T]) when is_integer(C), C > 0, C =< 255 ->
    [C | file_name(T)];
file_name([H|T]) ->
    file_name(H) ++ file_name(T);
file_name([]) ->
    [];
file_name(N) when is_atom(N) ->
    atom_to_list(N).

leave_file(From, St) ->
    case St#epp.istk of
	[I|Cis] -> 
	    epp_reply(From,
		      {error,{St#epp.location,epp, 
                              {illegal,"unterminated",I}}}),
	    leave_file(wait_request(St),St#epp{istk=Cis});
	[] ->
	    case St#epp.sstk of
		[OldSt|Sts] ->
		    close_file(St),
		    enter_file_reply(From, OldSt#epp.name, 
                                     OldSt#epp.location, OldSt#epp.location),
		    Ms = dict:store({atom,'FILE'},
				    {none,
				     [{string,OldSt#epp.location,
                                       OldSt#epp.name}]},
				    St#epp.macs),
		    wait_req_scan(OldSt#epp{sstk=Sts,macs=Ms});
		[] ->
		    epp_reply(From, {eof,St#epp.location}),
		    wait_req_scan(St)
	    end
    end.

%% scan_toks(From, EppState)
%% scan_toks(Tokens, From, EppState)

scan_toks(From, St) ->
    case io:scan_erl_form(St#epp.file, '', St#epp.location) of
	{ok,Toks,Cl} ->
	    scan_toks(Toks, From, St#epp{location=Cl});
	{error,E,Cl} ->
	    epp_reply(From, {error,E}),
	    wait_req_scan(St#epp{location=Cl});
	{eof,Cl} ->
	    leave_file(From, St#epp{location=Cl});
	{error,_E} ->
            epp_reply(From, {error,{St#epp.location,epp,cannot_parse}}),
	    leave_file(From, St)		%This serious, just exit!
    end.

scan_toks([{'-',_Lh},{atom,_Ld,define}=Define|Toks], From, St) ->
    scan_define(Toks, Define, From, St);
scan_toks([{'-',_Lh},{atom,_Ld,undef}=Undef|Toks], From, St) ->
    scan_undef(Toks, Undef, From, St);
scan_toks([{'-',_Lh},{atom,_Li,include}=Inc|Toks], From, St) ->
    scan_include(Toks, Inc, From, St);
scan_toks([{'-',_Lh},{atom,_Li,include_lib}=IncLib|Toks], From, St) ->
    scan_include_lib(Toks, IncLib, From, St);
scan_toks([{'-',_Lh},{atom,_Li,ifdef}=IfDef|Toks], From, St) ->
    scan_ifdef(Toks, IfDef, From, St);
scan_toks([{'-',_Lh},{atom,_Li,ifndef}=IfnDef|Toks], From, St) ->
    scan_ifndef(Toks, IfnDef, From, St);
scan_toks([{'-',_Lh},{atom,_Le,'else'}=Else|Toks], From, St) ->
    scan_else(Toks, Else, From, St);
scan_toks([{'-',_Lh},{'if',_Le}=If|Toks], From, St) ->
    scan_if(Toks, If, From, St);
scan_toks([{'-',_Lh},{atom,_Le,elif}=Elif|Toks], From, St) ->
    scan_elif(Toks, Elif, From, St);
scan_toks([{'-',_Lh},{atom,_Le,endif}=Endif|Toks], From, St) ->
    scan_endif(Toks, Endif, From, St);
scan_toks([{'-',_Lh},{atom,_Lf,file}=FileToken|Toks0], From, St) ->
    case catch expand_macros(Toks0, {St#epp.macs, St#epp.uses}) of
	Toks1 when is_list(Toks1) ->
            scan_file(Toks1, FileToken, From, St);
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end;
scan_toks(Toks0, From, St) ->
    case catch expand_macros(Toks0, {St#epp.macs, St#epp.uses}) of
	Toks1 when is_list(Toks1) ->
	    epp_reply(From, {ok,Toks1}),
	    wait_req_scan(St#epp{macs=scan_module(Toks1, St#epp.macs)});
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end.

scan_module([{'-',_Lh},{atom,_Lm,module},{'(',_Ll}|Ts], Ms) ->
    scan_module_1(Ts, [], Ms);
scan_module([{'-',_Lh},{atom,_Lm,extends},{'(',_Ll}|Ts], Ms) ->
    scan_extends(Ts, [], Ms);
scan_module(_Ts, Ms) -> Ms.

scan_module_1([{atom,_,_}=A,{',',L}|Ts], As, Ms) ->
    %% Parameterized modules.
    scan_module_1([A,{')',L}|Ts], As, Ms);
scan_module_1([{atom,Ln,A},{')',_Lr}|_Ts], As, Ms0) ->
    Mod = lists:concat(lists:reverse([A|As])),
    Ms = dict:store({atom,'MODULE'},
		     {none,[{atom,Ln,list_to_atom(Mod)}]}, Ms0),
    dict:store({atom,'MODULE_STRING'}, {none,[{string,Ln,Mod}]}, Ms);
scan_module_1([{atom,_Ln,A},{'.',_Lr}|Ts], As, Ms) ->
    scan_module_1(Ts, [".",A|As], Ms);
scan_module_1([{'.',_Lr}|Ts], As, Ms) ->
    scan_module_1(Ts, As, Ms);
scan_module_1(_Ts, _As, Ms) -> Ms.

scan_extends([{atom,Ln,A},{')',_Lr}|_Ts], As, Ms0) ->
    Mod = lists:concat(lists:reverse([A|As])),
    Ms = dict:store({atom,'BASE_MODULE'},
		     {none,[{atom,Ln,list_to_atom(Mod)}]}, Ms0),
    dict:store({atom,'BASE_MODULE_STRING'}, {none,[{string,Ln,Mod}]}, Ms);
scan_extends([{atom,_Ln,A},{'.',_Lr}|Ts], As, Ms) ->
    scan_extends(Ts, [".",A|As], Ms);
scan_extends([{'.',_Lr}|Ts], As, Ms) ->
    scan_extends(Ts, As, Ms);
scan_extends(_Ts, _As, Ms) -> Ms.

%% scan_define(Tokens, DefineToken, From, EppState)

scan_define([{'(',_Lp},{atom,_Lm,M}=Mac,{',',_Lc}|Toks], _Def, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_OldDef} ->
	    epp_reply(From, {error,{loc(Mac),epp,{redefine,M}}}),
	    wait_req_scan(St);
	error ->
	    scan_define_cont(From, St,
			     {atom, M},
			     {none,macro_expansion(Toks)})
    end;
scan_define([{'(',_Lp},{atom,_Lm,M}=Mac,{'(',_Lc}|Toks], Def, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_Def} ->
	    epp_reply(From, {error,{loc(Mac),epp,{redefine,M}}}),
	    wait_req_scan(St);
	error ->
	    case catch macro_pars(Toks, []) of
		{ok, {As, Me}} ->
		    scan_define_cont(From, St,
				    {atom, M},
				    {As, Me});
		_ ->
		    epp_reply(From, {error,{loc(Def),epp,{bad,define}}}),
		    wait_req_scan(St)
	    end
    end;
scan_define([{'(',_Lp},{var,_Lm,M}=Mac,{',',_Lc}|Toks], _Def, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_OldDef} ->
	    epp_reply(From, {error,{loc(Mac),epp,{redefine,M}}}),
	    wait_req_scan(St);
	error ->
	    scan_define_cont(From, St,
			     {atom, M},
			     {none,macro_expansion(Toks)})
    end;
scan_define([{'(',_Lp},{var,_Lm,M}=Mac,{'(',_Lc}|Toks], Def, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_Def} ->
	    epp_reply(From, {error,{loc(Mac),epp,{redefine,M}}}),
	    wait_req_scan(St);
	error ->
	    case catch macro_pars(Toks, []) of
		{ok, {As, Me}} ->
		    scan_define_cont(From, St,
				     {atom, M},
				     {As, Me});
		_ ->
		    epp_reply(From, {error,{loc(Def),epp,{bad,define}}}),
		    wait_req_scan(St)
	    end
    end;
scan_define(_Toks, Def, From, St) ->
    epp_reply(From, {error,{loc(Def),epp,{bad,define}}}),
    wait_req_scan(St).

%%% Detection of circular macro expansions (which would either keep
%%% the compiler looping forever, or run out of memory):
%%% When a macro is defined, we store the names of other macros it
%%% uses in St#epp.uses. If any macro is undef'ed, that information
%%% becomes invalid, so we redo it for all remaining macros.
%%% The circularity detection itself is done when a macro is expanded:
%%% the information from St#epp.uses is traversed, and if a circularity
%%% is detected, an error message is thrown.

scan_define_cont(F, St, M, Def) ->
    Ms = dict:store(M, Def, St#epp.macs),
    U = dict:store(M, macro_uses(Def), St#epp.uses),
    scan_toks(F, St#epp{uses=U, macs=Ms}).

macro_uses(undefined) ->
    undefined;
macro_uses({_Args, Tokens}) ->
    Uses0 = macro_ref(Tokens),
    lists:usort(Uses0).

macro_ref([]) ->
    [];
macro_ref([{'?', _}, {'?', _} | Rest]) ->
    macro_ref(Rest);
macro_ref([{'?', _}, {atom, _, A} | Rest]) ->
    [{atom, A} | macro_ref(Rest)];
macro_ref([{'?', _}, {var, _, A} | Rest]) ->
    [{atom, A} | macro_ref(Rest)];
macro_ref([_Token | Rest]) ->
    macro_ref(Rest).

all_macro_uses(D0) ->
    L = dict:to_list(D0),
    D = dict:new(),
    add_macro_uses(L, D).

add_macro_uses([], D) ->
    D;
add_macro_uses([{Key, Def} | Rest], D0) ->
    add_macro_uses(Rest, dict:store(Key, macro_uses(Def), D0)).

%% scan_undef(Tokens, UndefToken, From, EppState)

scan_undef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _Undef, From, St) ->
    scan_toks(From, St#epp{macs=dict:erase({atom,M}, St#epp.macs),
			   uses=all_macro_uses(St#epp.macs)});
scan_undef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _Undef, From,St) ->
    scan_toks(From, St#epp{macs=dict:erase({atom,M}, St#epp.macs),
			   uses=all_macro_uses(St#epp.macs)});
scan_undef(_Toks, Undef, From, St) ->
    epp_reply(From, {error,{loc(Undef),epp,{bad,undef}}}),
    wait_req_scan(St).

%% scan_include(Tokens, IncludeToken, From, St)

scan_include([{'(',_Llp},{string,_Lf,NewName0},{')',_Lrp},{dot,_Ld}], Inc, 
	     From, St) ->
    NewName = expand_var(NewName0),
    enter_file(St#epp.path, NewName, Inc, From, St);
scan_include(_Toks, Inc, From, St) ->
    epp_reply(From, {error,{abs_loc(Inc),epp,{bad,include}}}),
    wait_req_scan(St).

%% scan_include_lib(Tokens, IncludeToken, From, EppState)
%%  For include_lib we first test if we can find the file through the
%%  normal search path, if not we assume that the first directory name
%%  is a library name, find its true directory and try with that.

find_lib_dir(NewName) ->
    [Lib | Rest] = filename:split(NewName),
    {code:lib_dir(list_to_atom(Lib)), Rest}.

scan_include_lib([{'(',_Llp},{string,_Lf,_NewName0},{')',_Lrp},{dot,_Ld}],
                 Inc, From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{abs_loc(Inc),epp,{depth,"include_lib"}}}),
    wait_req_scan(St);
scan_include_lib([{'(',_Llp},{string,_Lf,NewName0},{')',_Lrp},{dot,_Ld}],
                 Inc, From, St) ->
    NewName = expand_var(NewName0),
    Loc = start_loc(St#epp.location),
    case file:path_open(St#epp.path, NewName, [read]) of
	{ok,NewF,Pname} ->
	    wait_req_scan(enter_file2(NewF, Pname, From, St, Loc));
	{error,_E1} ->
	    case catch find_lib_dir(NewName) of
		{LibDir, Rest} when is_list(LibDir) ->
		    LibName = filename:join([LibDir | Rest]),
		    case file:open(LibName, [read]) of
			{ok,NewF} ->
			    ExtraPath = [filename:dirname(LibName)],
			    wait_req_scan(enter_file2(NewF, LibName, From, 
                                                      St, Loc, ExtraPath));
			{error,_E2} ->
			    epp_reply(From,
				      {error,{abs_loc(Inc),epp,
                                              {include,lib,NewName}}}),
			    wait_req_scan(St)
		    end;
		_Error ->
		    epp_reply(From, {error,{abs_loc(Inc),epp,
                                            {include,lib,NewName}}}),
		    wait_req_scan(St)
	    end
    end;
scan_include_lib(_Toks, Inc, From, St) ->
    epp_reply(From, {error,{abs_loc(Inc),epp,{bad,include_lib}}}),
    wait_req_scan(St).

%% scan_ifdef(Tokens, IfdefToken, From, EppState)
%% scan_ifndef(Tokens, IfdefToken, From, EppSate)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if[n]def test and then treat as undefined macro.

scan_ifdef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfD, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_Def} ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
	error ->
	    skip_toks(From, St, [ifdef])
    end;
scan_ifdef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfD, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_Def} ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
	error ->
	    skip_toks(From, St, [ifdef])
    end;
scan_ifdef(_Toks, IfDef, From, St) ->
    epp_reply(From, {error,{loc(IfDef),epp,{bad,ifdef}}}),
    wait_req_skip(St, [ifdef]).

scan_ifndef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfnD, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_Def} ->
	    skip_toks(From, St, [ifndef]);
	error ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfnD, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_Def} ->
	    skip_toks(From, St, [ifndef]);
	error ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef(_Toks, IfnDef, From, St) ->
    epp_reply(From, {error,{loc(IfnDef),epp,{bad,ifndef}}}),
    wait_req_skip(St, [ifndef]).

%% scan_else(Tokens, ElseToken, From, EppState)
%%  If we are in an if body then convert to else and skip, if we are in an
%%  else or not in anything report an error.

scan_else([{dot,_Ld}], Else, From, St) ->
    case St#epp.istk of
	['else'|Cis] ->
	    epp_reply(From, {error,{loc(Else),
                                    epp,{illegal,"repeated",'else'}}}),
	    wait_req_skip(St#epp{istk=Cis}, ['else']);
	[_I|Cis] ->
	    skip_toks(From, St#epp{istk=Cis}, ['else']);
	[] ->
	    epp_reply(From, {error,{loc(Else),epp,
                                    {illegal,"unbalanced",'else'}}}),
	    wait_req_scan(St)
    end;
scan_else(_Toks, Else, From, St) ->
    epp_reply(From, {error,{loc(Else),epp,{bad,'else'}}}),
    wait_req_scan(St).

%% scan_if(Tokens, EndifToken, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_if(_Toks, If, From, St) ->
    epp_reply(From, {error,{loc(If),epp,{'NYI','if'}}}),
    wait_req_skip(St, ['if']).

%% scan_elif(Tokens, EndifToken, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_elif(_Toks, Elif, From, St) ->
    epp_reply(From, {error,{loc(Elif),epp,{'NYI','elif'}}}),
    wait_req_scan(St).

%% scan_endif(Tokens, EndifToken, From, EppState)
%%  If we are in an if body then exit it, else report an error.

scan_endif([{dot,_Ld}], Endif, From, St) ->
    case St#epp.istk of
	[_I|Cis] ->
	    scan_toks(From, St#epp{istk=Cis});
	[] ->
	    epp_reply(From, {error,{loc(Endif),epp,
                                    {illegal,"unbalanced",endif}}}),
	    wait_req_scan(St)
    end;
scan_endif(_Toks, Endif, From, St) ->
    epp_reply(From, {error,{loc(Endif),epp,{bad,endif}}}),
    wait_req_scan(St).

%% scan_file(Tokens, FileToken, From, EppState)
%%  Set the current file and line to the given file and line.
%%  Note that the line of the attribute itself is kept.

scan_file([{'(',_Llp},{string,_Ls,Name},{',',_Lc},{integer,_Li,Ln},{')',_Lrp},
           {dot,_Ld}], Tf, From, St) ->
    enter_file_reply(From, Name, Ln, neg_line(abs_loc(Tf))),
    Ms = dict:store({atom,'FILE'}, {none,[{string,1,Name}]}, St#epp.macs),
    Locf = loc(Tf),
    NewLoc = new_location(Ln, St#epp.location, Locf),
    scan_toks(From, St#epp{name=Name,location=NewLoc,macs=Ms});
scan_file(_Toks, Tf, From, St) ->
    epp_reply(From, {error,{loc(Tf),epp,{bad,file}}}),
    wait_req_scan(St).

new_location(Ln, Le, Lf) when is_integer(Lf) ->
    Ln+(Le-Lf);
new_location(Ln, {Le,_}, {Lf,_}) -> 
    {Ln+(Le-Lf),1}.

%% skip_toks(From, EppState, SkipIstack)
%%  Skip over forms until current conditional has been exited. Handle
%%  nested conditionals and repeated 'else's.

skip_toks(From, St, [I|Sis]) ->
    case io:scan_erl_form(St#epp.file, '', St#epp.location) of
	{ok,[{'-',_Lh},{atom,_Li,ifdef}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [ifdef,I|Sis]);
	{ok,[{'-',_Lh},{atom,_Li,ifndef}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [ifndef,I|Sis]);
	{ok,[{'-',_Lh},{'if',_Li}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, ['if',I|Sis]);
	{ok,[{'-',_Lh},{atom,_Le,'else'}=Else|_Toks],Cl}->
	    skip_else(Else, From, St#epp{location=Cl}, [I|Sis]);
	{ok,[{'-',_Lh},{atom,_Le,endif}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, Sis);
	{ok,_Toks,Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [I|Sis]);
	{error,_E,Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [I|Sis]);
	{eof,Cl} ->
	    leave_file(From, St#epp{location=Cl,istk=[I|Sis]});
	{error,_E} ->
            epp_reply(From, {error,{St#epp.location,epp,cannot_parse}}),
	    leave_file(From, St)		%This serious, just exit!
    end;
skip_toks(From, St, []) ->
    scan_toks(From, St).

skip_else(Else, From, St, ['else'|Sis]) ->
    epp_reply(From, {error,{loc(Else),epp,{illegal,"repeated",'else'}}}),
    wait_req_skip(St, ['else'|Sis]);
skip_else(_Else, From, St, [_I]) ->
    scan_toks(From, St#epp{istk=['else'|St#epp.istk]});
skip_else(_Else, From, St, Sis) ->
    skip_toks(From, St, Sis).

%% macro_pars(Tokens, ArgStack)
%% macro_expansion(Tokens)
%%  Extract the macro parameters and the expansion from a macro definition.

macro_pars([{')',_Lp}, {',',_Ld}|Ex], Args) ->
    {ok, {lists:reverse(Args), macro_expansion(Ex)}};
macro_pars([{var,_,Name}, {')',_Lp}, {',',_Ld}|Ex], Args) ->
    false = lists:member(Name, Args),		%Prolog is nice
    {ok, {lists:reverse([Name|Args]), macro_expansion(Ex)}};
macro_pars([{var,_L,Name}, {',',_}|Ts], Args) ->
    false = lists:member(Name, Args),           
    macro_pars(Ts, [Name|Args]).

macro_expansion([{')',_Lp},{dot,_Ld}]) -> [];
macro_expansion([{dot,_Ld}]) -> [];		%Be nice, allow no right paren!
macro_expansion([T|Ts]) ->
    [T|macro_expansion(Ts)].

%% expand_macros(Tokens, Macros)
%% expand_macro(Tokens, MacroToken, RestTokens)
%%  Expand the macros in a list of tokens, making sure that an expansion
%%  gets the same location as the macro call.

expand_macros(Type, MacT, M, Toks, Ms0) ->
    %% (Type will always be 'atom')
    {Ms, U} = Ms0,
    Lm = loc(MacT),
    check_uses([{Type,M}], [], U, Lm),
    Tinfo = element(2, MacT),
    case dict:find({Type,M}, Ms) of
	{ok,{none,Exp}} ->
	    expand_macros(expand_macro(Exp, Tinfo, Toks, dict:new()), Ms0);
	{ok,{As,Exp}} ->
	    {Bs,Toks1} = bind_args(Toks, Lm, M, As, dict:new()),
	    %%io:format("Bound arguments to macro ~w (~w)~n", [M,Bs]),
	    expand_macros(expand_macro(Exp, Tinfo, Toks1, Bs), Ms0);
	{ok,undefined} ->
	    throw({error,Lm,{undefined,M}});
	error ->
	    throw({error,Lm,{undefined,M}})
    end.

check_uses(undefined, _Anc, _U, _Lm) ->
    ok;
check_uses([], _Anc, _U, _Lm) ->
    ok;
check_uses([M|Rest], Anc, U, Lm) ->
    case lists:member(M, Anc) of
	true ->
	    {_, Name} = M,
	    throw({error,Lm,{circular,Name}});
	false ->
	    L = get_macro_uses(M, U),
	    check_uses(L, [M|Anc], U, Lm),
	    check_uses(Rest, Anc, U, Lm)
    end.
    
get_macro_uses(M, U) ->
    case dict:find(M, U) of
	error ->
	    [];
	{ok, L} ->
	    L
    end.

%% Macro expansion
%% Note: io:scan_erl_form() does not return comments or white spaces.
expand_macros([{'?',_Lq},{atom,_Lm,M}=MacT|Toks], Ms) ->
    expand_macros(atom, MacT, M, Toks, Ms);
%% Special macros
expand_macros([{'?',_Lq},{var,Lm,'LINE'}=Tok|Toks], Ms) ->
    {line,Line} = erl_scan:token_info(Tok, line),
    [{integer,Lm,Line}|expand_macros(Toks, Ms)];
expand_macros([{'?',_Lq},{var,_Lm,M}=MacT|Toks], Ms) ->
    expand_macros(atom, MacT, M, Toks, Ms);
%% Illegal macros
expand_macros([{'?',_Lq},Token|_Toks], _Ms) ->
    T = case erl_scan:token_info(Token, text) of
            {text,Text} ->
                Text;
            undefined ->
                {symbol,Symbol} = erl_scan:token_info(Token, symbol),
                io_lib:write(Symbol)
        end,
    throw({error,loc(Token),{call,[$?|T]}});
expand_macros([T|Ts], Ms) ->
    [T|expand_macros(Ts, Ms)];
expand_macros([], _Ms) -> [].

%% bind_args(Tokens, MacroLocation, MacroName, ArgumentVars, Bindings)
%%  Collect the arguments to a macro call and check for correct number.

bind_args([{'(',_Llp},{')',_Lrp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
bind_args([{'(',_Llp}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
bind_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}).

macro_args([{')',_Lrp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
macro_args([{',',_Lc}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
macro_args([], Lm, M, _As, _Bs) ->
    throw({error,Lm,{arg_error,M}});
macro_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}).

store_arg(L, M, _A, [], _Bs) ->
    throw({error,L,{mismatch,M}});
store_arg(_L, _M, A, Arg, Bs) ->
    dict:store(A, Arg, Bs).

%% macro_arg([Tok], [ClosePar], [ArgTok]) -> {[ArgTok],[RestTok]}.
%%  Collect argument tokens until we hit a ',' or a ')'. We know a
%%  enough about syntax to recognise "open parentheses" and keep
%%  scanning until matching "close parenthesis".

macro_arg([{',',Lc}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{',',Lc}|Toks]};
macro_arg([{')',Lrp}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{')',Lrp}|Toks]};
macro_arg([{'(',Llp}|Toks], E, Arg) ->
    macro_arg(Toks, [')'|E], [{'(',Llp}|Arg]);
macro_arg([{'<<',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, ['>>'|E], [{'<<',Lls}|Arg]);
macro_arg([{'[',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, [']'|E], [{'[',Lls}|Arg]);
macro_arg([{'{',Llc}|Toks], E, Arg) ->
    macro_arg(Toks, ['}'|E], [{'{',Llc}|Arg]);
macro_arg([{'begin',Lb}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'begin',Lb}|Arg]);
macro_arg([{'if',Li}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'if',Li}|Arg]);
macro_arg([{'case',Lc}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'case',Lc}|Arg]);
macro_arg([{'fun',Lc}|[{'(',_}|_]=Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'fun',Lc}|Arg]);
macro_arg([{'receive',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'receive',Lr}|Arg]);
macro_arg([{'try',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'try',Lr}|Arg]);
macro_arg([{'cond',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'cond',Lr}|Arg]);
macro_arg([{'query',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'query',Lr}|Arg]);
macro_arg([{Rb,Lrb}|Toks], [Rb|E], Arg) ->	%Found matching close
    macro_arg(Toks, E, [{Rb,Lrb}|Arg]);
macro_arg([T|Toks], E, Arg) ->
    macro_arg(Toks, E, [T|Arg]);
macro_arg([], _E, Arg) ->
    {lists:reverse(Arg),[]}.

%% expand_macro(MacroDef, MacroTokenInfo, RestTokens, Bindings)
%% expand_arg(Argtokens, MacroTokens, MacroLocation, RestTokens, Bindings)
%%  Insert the macro expansion replacing macro parameters with their
%%  argument values, inserting the location of first the macro call
%%  and then the macro arguments, i.e. simulate textual expansion.

expand_macro([{var,_Lv,V}|Ts], L, Rest, Bs) ->
    case dict:find(V, Bs) of
	{ok,Val} ->
	    %% lists:append(Val, expand_macro(Ts, L, Rest, Bs));
	    expand_arg(Val, Ts, L, Rest, Bs);
	error ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([{'?', _}, {'?', _}, {var,_Lv,V}|Ts], L, Rest, Bs) ->
    case dict:find(V, Bs) of
	{ok,Val} ->
	    %% lists:append(Val, expand_macro(Ts, L, Rest, Bs));
            expand_arg(stringify(Val, L), Ts, L, Rest, Bs);
	error ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([T|Ts], L, Rest, Bs) ->
    [setelement(2, T, L)|expand_macro(Ts, L, Rest, Bs)];
expand_macro([], _L, Rest, _Bs) -> Rest.

expand_arg([A|As], Ts, _L, Rest, Bs) ->
    %% It is not obvious that the location of arguments should replace L.
    NextL = element(2, A),
    [A|expand_arg(As, Ts, NextL, Rest, Bs)];
expand_arg([], Ts, L, Rest, Bs) ->
    expand_macro(Ts, L, Rest, Bs).

%%% stringify(Ts, L) returns a list of one token: a string which when
%%% tokenized would yield the token list Ts.

%% erl_scan:token_info(T, text) is not backward compatible with this.
token_src({dot, _}) ->
    ".";
token_src({X, _}) when is_atom(X) ->
    atom_to_list(X);
token_src({var, _, X}) ->
    atom_to_list(X);
token_src({char,_,C}) ->
    io_lib:write_char(C);
token_src({string, _, X}) ->
    lists:flatten(io_lib:format("~p", [X]));
token_src({_, _, X}) ->
    lists:flatten(io_lib:format("~w", [X])).

stringify1([]) ->
    [];
stringify1([T | Tokens]) ->
    [io_lib:format(" ~s", [token_src(T)]) | stringify1(Tokens)].

stringify(Ts, L) ->
    [$\s | S] = lists:flatten(stringify1(Ts)),
    [{string, L, S}].

%% epp_request(Epp)
%% epp_request(Epp, Request)
%% epp_reply(From, Reply)
%%  Handle communication with the epp.

epp_request(Epp) ->
    wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_request(Epp, Req) ->
    Epp ! {epp_request,self(),Req},
    wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_reply(From, Rep) ->
    From ! {epp_reply,self(),Rep},
    ok.

wait_epp_reply(Epp, Mref) ->
    receive
	{epp_reply,Epp,Rep} -> 
	    erlang:demonitor(Mref),
	    receive {'DOWN',Mref,_,_,_} -> ok after 0 -> ok end,
	    Rep;
	{'DOWN',Mref,_,_,E} -> 
	    receive {epp_reply,Epp,Rep} -> Rep
	    after 0 -> exit(E)
	    end
    end.

expand_var([$$ | _] = NewName) ->
    case catch expand_var1(NewName) of
	{ok, ExpName} ->
	    ExpName;
	_ ->
	    NewName
    end;
expand_var(NewName) ->
    NewName.

expand_var1(NewName) ->
    [[$$ | Var] | Rest] = filename:split(NewName),
    Value = os:getenv(Var),
    true = Value =/= false,
    {ok, filename:join([Value | Rest])}.

%% The line only. (Other tokens may have the column and text as well...)
loc_attr(Line) when is_integer(Line) ->
    Line;
loc_attr({Line,_Column}) ->
    Line.

loc(Token) ->
    {location,Location} = erl_scan:token_info(Token, location),
    Location.

abs_loc(Token) ->
    loc(setelement(2, Token, abs_line(element(2, Token)))).

neg_line(L) ->
    erl_scan:set_attribute(line, L, fun(Line) -> -abs(Line) end).

abs_line(L) ->
    erl_scan:set_attribute(line, L, fun(Line) -> abs(Line) end).

start_loc(Line) when is_integer(Line) ->
    1;
start_loc({_Line, _Column}) ->
    {1,1}.

get_line(Line) when is_integer(Line) ->
    Line;
get_line({Line,_Column}) ->
    Line.

%% epp has always output -file attributes when entering and leaving
%% included files (-include, -include_lib). Starting with R11B the
%% -file attribute is also recognized in the input file. This is
%% mainly aimed at yecc, the parser generator, which uses the -file
%% attribute to get correct lines in messages referring to code
%% supplied by the user (actions etc in .yrl files).
%% 
%% In a perfect world (read: perfectly implemented applications such
%% as Xref, Cover, Debugger, etc.) it would not be necessary to
%% distinguish -file attributes from epp and the input file. The
%% Debugger for example could have one window for each referred file,
%% each window with its own set of breakpoints etc. The line numbers
%% of the abstract code would then point into different windows
%% depending on the -file attribute. [Note that if, as is the case for
%% yecc, code has been copied into the file, then it is possible that
%% the copied code differ from the one referred to by the -file
%% attribute, which means that line numbers can mismatch.] In practice
%% however it is very rare with Erlang functions in included files, so
%% only one window is used per module. This means that the line
%% numbers of the abstract code have to be adjusted to refer to the
%% top-most source file. The function interpret_file_attributes/1
%% below interprets the -file attribute and returns forms where line
%% numbers refer to the top-most file. The -file attribute forms that
%% have been output by epp (corresponding to -include and
%% -include_lib) are kept, but the user's -file attributes are
%% removed. This seems sufficient for now.
%% 
%% It turns out to be difficult to distinguish -file attributes in the
%% input file from the ones added by epp unless some action is taken.
%% The (less than perfect) solution employed is to let epp assign
%% negative line numbers to user supplied -file attributes.

%% Note: it is assumed that the second element is a line or a key-list
%% where 'line' can be found.

interpret_file_attribute(Forms) ->
    interpret_file_attr(Forms, 0, []).

interpret_file_attr([{attribute,Loc,file,{File,Line}}=Form | Forms], 
                    Delta, Fs) ->
    {line, L} = erl_scan:attributes_info(Loc, line),
    if
        L < 0 ->
            %% -file attribute
            interpret_file_attr(Forms, (abs(L) + Delta) - Line, Fs);
        true ->
            %% -include or -include_lib
            % true = L =:= Line,
            case Fs of
                [_, Delta1, File | Fs1] -> % end of included file
                    [Form | interpret_file_attr(Forms, Delta1, [File | Fs1])];
                _ -> % start of included file
                    [Form | interpret_file_attr(Forms, 0, [File, Delta | Fs])]
            end
    end;
interpret_file_attr([Form0 | Forms], Delta, Fs) ->
    F = fun(Attrs) ->
                F2 = fun(L) -> abs(L) + Delta end,
                erl_scan:set_attribute(line, Attrs, F2)
        end,
    Form = erl_lint:modify_line(Form0, F),
    [Form | interpret_file_attr(Forms, Delta, Fs)];
interpret_file_attr([], _Delta, _Fs) ->
    [].

