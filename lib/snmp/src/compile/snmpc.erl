%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(snmpc).

%% API
-export([compile/1, compile/2, compile/3,
	 mib_to_hrl/1, mib_to_hrl/3, 
	 is_consistent/1]).

%% Debug
-export([look_at/1]).

%% Internal Exports
-export([init/3]).

-include_lib("stdlib/include/erl_compile.hrl").
-include("snmp_types.hrl").
-include("snmpc.hrl").
-include("snmpc_lib.hrl").

-record(dldata, {deprecated, relaxed_row_name_assign_check}).

look_at(Mib) ->
    io:format("~p ~n", [snmpc_lib:look_at(Mib)]).


%%-----------------------------------------------------------------
%% Misc compiler stuff
%%-----------------------------------------------------------------

is_consistent(Filenames) ->
    snmpc_lib:is_consistent(Filenames).

mib_to_hrl(MibName) ->
    snmpc_mib_to_hrl:convert(MibName).

mib_to_hrl(MibName, HrlFile, Opts) ->
    snmpc_mib_to_hrl:compile(MibName, HrlFile, Opts).


%%%-----------------------------------------------------------------
%%% Interface for erl_compile.
%%%-----------------------------------------------------------------

compile(Input, _Output, Options) ->
    case compile(Input, make_options(Options)) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format("~tp", [Reason]),
            error
    end.

%% Converts generic options to format expected by compile/2

make_options(#options{includes = Incs,
                      outdir   = Outdir,
                      warning  = Warning,
                      specific = Spec}) ->

    OutdirOpt = {outdir, Outdir},

    WarningOpt = 
        case Warning of
            0 -> {warnings, false};
            _ -> {warnings, true}
        end,

    IncludeOpt =
        {i, case Incs of
                [] ->
                    [""];
                _ ->
                    lists:map(fun(Dir) -> Dir++"/" end, Incs)
            end},

    [WarningOpt, OutdirOpt, IncludeOpt | Spec].

%% Returns: {ok, File}|{error, Reason}
compile([AtomFilename]) when is_atom(AtomFilename) ->
    compile(atom_to_list(AtomFilename), []), % from cmd line
    halt();
compile(FileName) -> 
    compile(FileName, []).


%%----------------------------------------------------------------------
%% Options:
%%          {deprecated,  bool()}                         true
%%          {group_check, bool()}                         true
%%          {db,          volatile|persistent|mnesia}     volatile
%%          {i,           [import_dir_string()]}          ["./"]
%%          {il,          [import_lib_dir_string()]}      []
%%          {warnings,    bool()}                         true
%%          warnings_as_errors
%%          {outdir,      string()}                       "./"
%%          description
%%          reference
%%          imports
%%          agent_capabilities
%%          module_compliance
%%          module_identity
%%          {module, string()}
%%          no_defs
%%          relaxed_row_name_assign_check
%% (hidden) {verbosity,   trace|debug|log|info|silence}   silence
%% (hidden) version 
%% (hidden) options 
%%----------------------------------------------------------------------

compile(FileName, Options) when is_list(FileName) ->
    case snmpc_misc:check_file(FileName) of
	true ->
	    compile_1(FileName, Options);
	false ->
	    {error, {invalid_file, FileName}}
    end.

compile_1(FileName, Options) ->
    DefOpts = [{deprecated,  true},
	       {group_check, true},
	       {i,           ["./"]},
	       {db,          volatile},
	       {warnings,    true},
	       {outdir,      "./"},
	       {il,          []}],
    Opts = update_options(DefOpts, Options),
    case check_options(Opts) of
	ok ->
	    maybe_display_version(Opts),
	    maybe_display_options(Opts),
	    Pid = spawn_link(?MODULE,init,[self(),FileName,Opts]),
	    receive
		{compile_result,R} -> R;
		{'EXIT',Pid, Reason} when Reason =/= normal ->
		    exit(Reason)
	    end;
	{error, Reason} -> 
	    {error, Reason}
    end.

maybe_display_version(Opts) ->
    case lists:member(version, Opts) of
	true ->
	    Vsn = (catch get_version()),
	    io:format("version: ~s~n", [Vsn]);
	false ->
	    ok
    end.

get_version() ->
    MI   = ?MODULE:module_info(),
    Attr = get_info(attributes, MI),
    Vsn  = get_info(app_vsn, Attr),
    Comp = get_info(compile, MI),
    Time = get_info(time, Comp),
    {Year, Month, Day, Hour, Min, Sec} = Time,
    io_lib:format("~s [~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w]", 
		  [Vsn, Year, Month, Day, Hour, Min, Sec]).

maybe_display_options(Opts) ->
    case lists:member(options, Opts) of
	true ->
	    {F, A} = get_options(Opts, [], []),
	    io:format("options: " ++ F ++ "~n", A);
	false ->
	    ok
    end.

get_options([], Formats, Args) ->
    {lists:concat(lists:reverse(Formats)), lists:reverse(Args)};
get_options([{deprecated, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   deprecated:  ~w"|Formats], [Val|Args]);
get_options([{group_check, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   group_check: ~w"|Formats], [Val|Args]);
get_options([{db, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   db:          ~w"|Formats], [Val|Args]);
get_options([{i, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   i:           ~p"|Formats], [Val|Args]);
get_options([{il, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   il:          ~p"|Formats], [Val|Args]);
get_options([{outdir, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   outdir:      ~s"|Formats], [Val|Args]);
get_options([{description, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   description: ~w"|Formats], [Val|Args]);
get_options([description|Opts], Formats, Args) ->
    get_options(Opts, ["~n   description"|Formats], Args);
get_options([{reference, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   reference: ~w"|Formats], [Val|Args]);
get_options([reference|Opts], Formats, Args) ->
    get_options(Opts, ["~n   reference"|Formats], Args);
get_options([{warnings, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   warnings:    ~w"|Formats], [Val|Args]);
get_options([warnings_as_errors|Opts], Formats, Args) ->
    get_options(Opts, ["~n   warnings_as_errors"|Formats], Args);
get_options([{verbosity, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   verbosity:   ~w"|Formats], [Val|Args]);
get_options([imports|Opts], Formats, Args) ->
    get_options(Opts, ["~n   imports"|Formats], Args);
get_options([module_identity|Opts], Formats, Args) ->
    get_options(Opts, ["~n   module_identity"|Formats], Args);
get_options([module_compliance|Opts], Formats, Args) ->
    get_options(Opts, ["~n   module_compliance"|Formats], Args);
get_options([agent_capabilities|Opts], Formats, Args) ->
    get_options(Opts, ["~n   agent_capabilities"|Formats], Args);
get_options([relaxed_row_name_assign_check|Opts], Formats, Args) ->
    get_options(Opts, ["~n   relaxed_row_name_assign_check"|Formats], Args);
get_options([_|Opts], Formats, Args) ->
    get_options(Opts, Formats, Args).
    

get_info(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    throw("undefined")
    end.

% p(F, A) ->
%     io:format("DBG: " ++ F ++ "~n", A).

update_options([], Options) -> 
    Options;
update_options([{Key,DefVal}|DefOpts], Options) ->
    case snmpc_misc:assq(Key, Options) of
	false ->
	    update_options(DefOpts, [{Key,DefVal}|Options]);
	{value, Val} when Key =:= i ->
	    Options1 = 
		lists:keyreplace(Key, 1, Options, {Key, Val++DefVal}),
	    update_options(DefOpts, Options1);
	{value, Val} when Key =:= il ->
	    Options1 = 
		lists:keyreplace(Key, 1, Options, {Key, Val++DefVal}),
	    update_options(DefOpts, Options1);
	{value, DefVal} -> %% Same value, no need to update
	    update_options(DefOpts, Options);
	{value, Val} ->    %% New value, so update
	    Options1 = 
		lists:keyreplace(Key, 1, Options, {Key, Val}),
	    update_options(DefOpts, Options1)
    end.

check_options([]) -> ok;
check_options([no_symbolic_info|T]) -> check_options(T);
check_options([{outdir, Str} | T]) when is_list(Str) ->
    check_options(T);
check_options([{debug, Atom} | T]) when is_atom(Atom) ->
    check_options(T);
check_options([{deprecated, Atom} | T]) when is_atom(Atom) ->
    check_options(T);		     
check_options([{group_check, Atom} | T]) when is_atom(Atom) ->
    check_options(T);
check_options([{warnings, Bool} | T]) ->
    check_bool(warnings, Bool),
    check_options(T);
check_options([warnings_as_errors | T]) ->
    check_options(T);
check_options([{db, volatile} | T]) ->
    check_options(T);
check_options([{db, persistent} | T]) ->
    check_options(T);
check_options([{db, mnesia} | T]) ->
    check_options(T);
check_options([{i, [Str|_]} | T]) when is_list(Str) ->
    check_options(T);
check_options([{il, []} | T]) ->
    check_options(T);
check_options([{il, [Str|_]} | T]) when is_list(Str) ->
    check_options(T);
check_options([{description, Bool}| T]) ->
    check_bool(description, Bool),
    check_options(T);
check_options([description| T]) -> %% same as {description, true}
    check_options(T);
check_options([{reference, Bool}| T]) ->
    check_bool(reference, Bool),
    check_options(T);
check_options([reference| T]) -> %% same as {reference, true}
    check_options(T);
check_options([{verbosity, V} | T]) when is_atom(V) ->
    snmpc_lib:vvalidate(V),
    check_options(T);
check_options([version| T]) ->
    check_options(T);
check_options([options| T]) ->
    check_options(T);
check_options([imports| T]) ->
    check_options(T);
check_options([module_identity| T]) ->
    check_options(T);
check_options([module_compliance| T]) ->
    check_options(T);
check_options([agent_capabilities| T]) ->
    check_options(T);
check_options([relaxed_row_name_assign_check| T]) ->
    check_options(T);
check_options([{module, M} | T]) when is_atom(M) ->
    check_options(T);
check_options([no_defs| T]) ->
    check_options(T);
check_options([Opt|_]) ->
    {error, {invalid_option, Opt}}.


check_bool(_Key, Bool) when (Bool =:= true) orelse (Bool =:= false) ->
    ok;
check_bool(Key, Val) ->
    {error, {invalid_option, {Key, Val}}}.
    
get_group_check(Options) ->
    snmpc_lib:key1search(group_check, Options, true).

get_deprecated(Options) ->
    snmpc_lib:key1search(deprecated, Options, true).

get_description(Options) ->
    get_bool_option(description, Options).

get_reference(Options) ->
    get_bool_option(reference, Options).

get_agent_capabilities(Options) ->
    get_bool_option(agent_capabilities, Options).

get_module_compliance(Options) ->
    get_bool_option(module_compliance, Options).

get_warnings_as_errors(Options) ->
    lists:member(warnings_as_errors, Options).

get_relaxed_row_name_assign_check(Options) ->
    lists:member(relaxed_row_name_assign_check, Options).

get_bool_option(Option, Options) ->
    case lists:member(Option, Options) of
	false ->
	    snmpc_lib:key1search(Option, Options, false);
	true ->
	    true
    end.

make_description(Message) ->
    case get(description) of
	true ->
	    Message;
	_ -> 
	    undefined
    end.

make_reference(undefined) ->
    [];
make_reference(Reference) ->
    case get(reference) of
	true ->
	    [{reference, Reference}];
	_ -> 
	    []
    end.

    
		
%%----------------------------------------------------------------------
%% verbosity stuff
%%----------------------------------------------------------------------

%% Verbosity level is selected from three (historical reasons)
%% options: warnings, debug and verbosity
%% - If warnings is true, then verbosity is _atleast_ warning
%%   (even if the verbosity flag is set to silence)
%% - If debug is true, the verbosity is _atleast_ log
%% - Otherwise, verbosity is used as is.
get_verbosity(Options) ->
    WarningsSeverity = 
	case snmpc_lib:key1search(warnings, Options) of
	    true ->
		warning;
	    _ ->
		silence
	end,
    case snmpc_lib:key1search(verbosity, Options) of
	undefined ->
	    %% Backward compatible: If not defined then try debug and convert
	    case snmpc_lib:key1search(debug, Options, false) of
		true ->
		    log;
		false ->
		    WarningsSeverity
	    end;
	silence ->
	    WarningsSeverity;
	Verbosity ->
	    Verbosity
    end.


%%----------------------------------------------------------------------
%% The compile process.
%%----------------------------------------------------------------------

init(From, MibFileName, Options) ->
    random:seed(erlang:phash2([node()]),
                erlang:monotonic_time(),
                erlang:unique_integer()),
    put(options,            Options),
    put(verbosity,          get_verbosity(Options)),
    put(description,        get_description(Options)),
    put(reference,          get_reference(Options)),
    put(agent_capabilities, get_agent_capabilities(Options)),
    put(module_compliance,  get_module_compliance(Options)),
    put(warnings_as_errors, get_warnings_as_errors(Options)),
    File = filename:rootname(MibFileName, ".mib"),
    put(filename, filename:basename(File ++ ".mib")),
    R = case catch c_impl(File) of
	    {ok, OutFile} -> {ok, OutFile};
	    {'EXIT',error} -> {error, compilation_failed};
	    Error -> exit(Error)
	end,
    From ! {compile_result, R}.


c_impl(File) ->
    {ok, PData} = parse(File),
    ?vtrace("Syntax analysis:"
	    "~n   PData: ~p", [PData]),
    MibName = compile_parsed_data(PData),
    ?vtrace("Compiler output:"
	    "~n   CDATA: ~p", [get(cdata)]),
    save(File, MibName, get(options)).

compile_parsed_data(#pdata{mib_name = MibName, 
			   imports  = Imports, 
			   defs     = Definitions}) ->
    snmpc_lib:import(Imports),
    update_imports(Imports),
    Opts = get(options),
    Deprecated = get_deprecated(Opts),
    RelChk = get_relaxed_row_name_assign_check(Opts),
    Data = #dldata{deprecated                    = Deprecated,
		   relaxed_row_name_assign_check = RelChk},
    put(augmentations, false),
    definitions_loop(Definitions, Data),
    MibName.

update_imports(Imports) ->
    case lists:member(imports, get(options)) of
	true ->
	    IMPs  = do_update_imports(Imports, []),
	    CDATA = get(cdata),
	    put(cdata, CDATA#cdata{imports = IMPs});
	false ->
	    ok
    end.

do_update_imports([], Acc) ->
    lists:reverse(Acc);
do_update_imports([{{Mib, ImportsFromMib0},_Line}|Imports], Acc) ->
    ImportsFromMib = [Name || {_, Name} <- ImportsFromMib0],
    Import = {Mib, ImportsFromMib},
    do_update_imports(Imports, [Import|Acc]).


update_status(Name, Status) ->
    #cdata{status_ets = Ets} = get(cdata),
    ets:insert(Ets, {Name, Status}).
    

%% A deprecated object
definitions_loop([{#mc_object_type{name = ObjName, status = deprecated}, 
		   Line}|T],
		 #dldata{deprecated = false} = Data) ->
    %% May be implemented but the compiler chooses not to.
    ?vinfo2("object_type ~w is deprecated => ignored", [ObjName], Line),    
    update_status(ObjName, deprecated), 
    definitions_loop(T, Data);

%% A obsolete object
definitions_loop([{#mc_object_type{name = ObjName, status = obsolete}, 
		   Line}|T], 
		 Data) ->
    ?vlog2("object_type ~w is obsolete => ignored", [ObjName], Line),
    %% No need to implement a obsolete object
    update_status(ObjName, obsolete),
    ensure_macro_imported('OBJECT-TYPE', Line),
    definitions_loop(T, Data);

%% Defining a table
definitions_loop([{#mc_object_type{name        = NameOfTable,
				   syntax      = {{sequence_of, SeqName}, _},
				   max_access  = Taccess,
				   kind        = Kind, 
				   status      = Tstatus,
				   description = Desc1,
				   units       = Tunits,
				   reference   = Ref, 
				   name_assign = Tindex},
		   Tline},
		  {#mc_object_type{name        = NameOfEntry,
				   syntax      = {{type, SeqName}, TEline},
				   max_access  = 'not-accessible',
				   kind        = {table_entry, IndexingInfo},
				   status      = Estatus,
				   description = Desc2,
				   units       = Eunits, 
				   name_assign = {NameOfTable,[1]}},
		   Eline},
		  {#mc_sequence{name   = SeqName,
				fields = FieldList},
		   Sline}|ColsEtc],
		 Data) ->
    ?vlog("defloop(~w) -> "
	  "[object_type(sequence_of),object_type(type,[1]),sequence]:"
	  "~n   NameOfTable:  ~p"
	  "~n   SeqName:      ~p"
	  "~n   Taccess:      ~p"
	  "~n   Kind:         ~p"
	  "~n   Tstatus:      ~p"
	  "~n   Tindex:       ~p"
	  "~n   Tunits:       ~p"
	  "~n   Tline:        ~p"
	  "~n   NameOfEntry:  ~p"
	  "~n   TEline:       ~p"
	  "~n   IndexingInfo: ~p"
	  "~n   Estatus:      ~p"
	  "~n   Eunits:       ~p"
	  "~n   Ref:          ~p"
	  "~n   Eline:        ~p"
	  "~n   FieldList:    ~p"
	  "~n   Sline:        ~p",
	  [?LINE, 
	   NameOfTable,SeqName,Taccess,Kind,Tstatus,
	   Tindex,Tunits,Tline,
	   NameOfEntry,TEline,IndexingInfo,Estatus,Eunits,Ref,Eline,
	   FieldList,Sline]),
    update_status(NameOfTable, Tstatus),
    update_status(NameOfEntry, Estatus),
    update_status(SeqName,     undefined),
    ensure_macro_imported('OBJECT-TYPE', Tline),
    test_table(NameOfTable,Taccess,Kind,Tindex,Tline),
    {Tfather,Tsubindex} = Tindex,
    snmpc_lib:register_oid(Tline,NameOfTable,Tfather,Tsubindex),
    Description1 = make_description(Desc1),
    TableME = #me{aliasname   = NameOfTable,
		  entrytype   = table, 
		  access      = 'not-accessible',
		  description = Description1,
		  units       = Tunits},
    snmpc_lib:register_oid(TEline,NameOfEntry,NameOfTable,[1]),
    Description2 = make_description(Desc2),
    TableEntryME = #me{aliasname   = NameOfEntry, 
		       entrytype   = table_entry,
		       assocList   = [{table_entry_with_sequence, SeqName}], 
		       access      = 'not-accessible',
		       description = Description2,
		       units       = Eunits},
    {ColMEs, RestObjs} = 
	define_cols(ColsEtc, 1, FieldList, NameOfEntry, NameOfTable, []),
    AfterIdxTypes = after_indexes_type(IndexingInfo, RestObjs), 
    TableInfo = snmpc_lib:make_table_info(Eline, NameOfTable,
					  IndexingInfo, AfterIdxTypes, ColMEs),
    snmpc_lib:add_cdata(#cdata.mes, 
			[TableEntryME,
			 TableME#me{assocList=[{table_info, 
						TableInfo} | make_reference(Ref)]} |
				ColMEs]),
    definitions_loop(RestObjs, Data);

definitions_loop([{#mc_object_type{name        = NameOfTable,
				   syntax      = {{sequence_of, SeqName}, _},
				   max_access  = Taccess,
				   kind        = Kind, 
				   status      = Tstatus,
				   description = Desc1,
				   units       = Tunits,
				   reference   = Ref, 
				   name_assign = Tindex},
		   Tline},
		  {#mc_object_type{name        = NameOfEntry,
				   syntax      = {{type, SeqName}, TEline},
				   max_access  = 'not-accessible',
				   kind        = {table_entry, IndexingInfo},
				   status      = Estatus,
				   description = Desc2,
				   units       = Eunits, 
				   name_assign = {NameOfTable,[Idx]} = BadOID},
		   Eline},
		  {#mc_sequence{name   = SeqName,
				fields = FieldList},
		   Sline}|ColsEtc],
		 #dldata{relaxed_row_name_assign_check = true} = Data) 
  when is_integer(Idx) andalso (Idx > 1) ->
    ?vlog("defloop(~w) -> "
	  "[object_type(sequence_of),object_type(type,[~w]),sequence]:"
	  "~n   NameOfTable:  ~p"
	  "~n   SeqName:      ~p"
	  "~n   Taccess:      ~p"
	  "~n   Kind:         ~p"
	  "~n   Tstatus:      ~p"
	  "~n   Tindex:       ~p"
	  "~n   Tunits:       ~p"
	  "~n   Tline:        ~p"
	  "~n   NameOfEntry:  ~p"
	  "~n   TEline:       ~p"
	  "~n   IndexingInfo: ~p"
	  "~n   Estatus:      ~p"
	  "~n   Eunits:       ~p"
	  "~n   Ref:          ~p"
	  "~n   Eline:        ~p"
	  "~n   FieldList:    ~p"
	  "~n   Sline:        ~p",
	  [?LINE, 
	   Idx, 
	   NameOfTable,SeqName,Taccess,Kind,Tstatus,
	   Tindex,Tunits,Tline,
	   NameOfEntry,TEline,IndexingInfo,Estatus,Eunits,Ref,Eline,
	   FieldList,Sline]),
    update_status(NameOfTable, Tstatus),
    update_status(NameOfEntry, Estatus),
    update_status(SeqName,     undefined),
    ensure_macro_imported('OBJECT-TYPE', Tline),
    ?vwarning2("Bad TableEntry OID definition (~w)",
	       [BadOID], Eline),
    test_table(NameOfTable,Taccess,Kind,Tindex,Tline),
    {Tfather,Tsubindex} = Tindex,
    snmpc_lib:register_oid(Tline,NameOfTable,Tfather,Tsubindex),
    Description1 = make_description(Desc1),
    TableME = #me{aliasname   = NameOfTable,
		  entrytype   = table, 
		  access      = 'not-accessible',
		  description = Description1,
		  units       = Tunits},
    snmpc_lib:register_oid(TEline,NameOfEntry,NameOfTable,[Idx]),
    Description2 = make_description(Desc2),
    TableEntryME = #me{aliasname   = NameOfEntry, 
		       entrytype   = table_entry,
		       assocList   = [{table_entry_with_sequence, SeqName}], 
		       access      = 'not-accessible',
		       description = Description2,
		       units       = Eunits},
    {ColMEs, RestObjs} = 
	define_cols(ColsEtc, 1, FieldList, NameOfEntry, NameOfTable, []),
    AfterIdxTypes = after_indexes_type(IndexingInfo, RestObjs), 
    TableInfo = snmpc_lib:make_table_info(Eline, NameOfTable,
					  IndexingInfo, AfterIdxTypes, ColMEs),
    snmpc_lib:add_cdata(#cdata.mes, 
			[TableEntryME,
			 TableME#me{assocList=[{table_info, 
						TableInfo} | make_reference(Ref)]} |
				ColMEs]),
    definitions_loop(RestObjs, Data);

definitions_loop([{#mc_object_type{name        = NameOfTable,
				   syntax      = {{sequence_of, SeqName},_},
				   max_access  = Taccess,
				   kind        = Kind, 
				   status      = Tstatus,
				   description = Desc1,
				   units       = Tunits,
				   reference   = Ref, 
				   name_assign = Tindex}, Tline},
		  {#mc_object_type{name        = NameOfEntry,
				   syntax      = {{type, SeqName},_},
				   max_access  = 'not-accessible',
				   kind        = {table_entry,IndexingInfo}, 
				   status      = Estatus, 
				   description = Desc2,
				   units       = Eunits,
				   name_assign = BadOID}, Eline},
		  {#mc_sequence{name   = SeqName,
				fields = FieldList}, Sline}|ColsEtc],
		 Data) ->
    ?vlog("defloop(~w) -> "
	  "[object_type(sequence_of),object_type(type),sequence(fieldList)]:"
	  "~n   NameOfTable:  ~p"
	  "~n   SeqName:      ~p"
	  "~n   Taccess:      ~p"
	  "~n   Kind:         ~p"
	  "~n   Tstatus:      ~p"
	  "~n   Tindex:       ~p"
	  "~n   Tunits:       ~p"
	  "~n   Tline:        ~p"
	  "~n   NameOfEntry:  ~p"
	  "~n   IndexingInfo: ~p"
	  "~n   Estatus:      ~p"
	  "~n   BadOID:       ~p"
	  "~n   Eunits:       ~p"
	  "~n   Ref:          ~p"
	  "~n   Eline:        ~p"
	  "~n   FieldList:    ~p"
	  "~n   Sline:        ~p",
	  [?LINE, 
	   NameOfTable,SeqName,Taccess,Kind,Tstatus,
	   Tindex,Tunits,Tline,
	   NameOfEntry,IndexingInfo,Estatus,BadOID,Eunits,Ref,Eline,
	   FieldList,Sline]),
    update_status(NameOfTable, Tstatus),
    update_status(NameOfEntry, Estatus),
    update_status(SeqName,     undefined),
    ensure_macro_imported('OBJECT-TYPE', Tline),
    snmpc_lib:print_error("Bad TableEntry OID definition (~w)",
			  [BadOID],Eline),
    test_table(NameOfTable,Taccess,Kind,Tindex,Tline),
    {Tfather,Tsubindex} = Tindex,
    snmpc_lib:register_oid(Tline,NameOfTable,Tfather,Tsubindex),
    Description1 = make_description(Desc1),
    TableME = #me{aliasname   = NameOfTable,
		  entrytype   = table, 
		  access      = 'not-accessible',
		  description = Description1,
		  units       = Tunits},
    Description2 = make_description(Desc2),
    TableEntryME = #me{aliasname   = NameOfEntry, 
		       entrytype   = table_entry,
		       access      = 'not-accessible',
		       assocList   = [{table_entry_with_sequence,SeqName}],
		       description = Description2,
		       units       = Eunits},
    {ColMEs, RestObjs} = 
	define_cols(ColsEtc, 1, FieldList, NameOfEntry, NameOfTable, []),
    AfterIdxTypes = after_indexes_type(IndexingInfo, RestObjs), 
    TableInfo = snmpc_lib:make_table_info(Eline, NameOfTable,
					  IndexingInfo, AfterIdxTypes, ColMEs),
    snmpc_lib:add_cdata(#cdata.mes, 
			       [TableEntryME,
				TableME#me{assocList=[{table_info, 
						       TableInfo} | make_reference(Ref)]} |
				ColMEs]),
    definitions_loop(RestObjs, Data);

definitions_loop([{#mc_new_type{name         = NewTypeName,
				macro        = Macro,
				syntax       = OldType,
				display_hint = DisplayHint},Line}|T],
		 Data) ->
    ?vlog2("defloop -> new_type:"
	   "~n   Macro:       ~p"
	   "~n   NewTypeName: ~p"
	   "~n   OldType:     ~p"
	   "~n   DisplayHint: ~p", 
	   [Macro, NewTypeName, OldType, DisplayHint], Line),
    ensure_macro_imported(Macro,Line),
    Types = (get(cdata))#cdata.asn1_types,
    case lists:keysearch(NewTypeName, #asn1_type.aliasname, Types) of
	{value,_} ->
	    snmpc_lib:print_error("Type ~w already defined.",
				  [NewTypeName],Line);
	false ->
	    %% NameOfOldType = element(2,OldType), 
	    ASN1 = snmpc_lib:make_ASN1type(OldType),
	    snmpc_lib:add_cdata(#cdata.asn1_types,
				[ASN1#asn1_type{aliasname    = NewTypeName,
						imported     = false,
						display_hint = DisplayHint}])
    end,
    definitions_loop(T,	Data);

%% Plain variable
definitions_loop([{#mc_object_type{name        = NewVarName,
				   syntax      = Type, 
				   max_access  = Access,
				   kind        = {variable, DefVal}, 
				   status      = Status,
				   description = Desc1, 
				   units       = Units,
				   name_assign = {Parent,SubIndex}},Line} |T],
		 Data) ->
    ?vlog2("defloop -> object_type (variable):"
	   "~n   NewVarName: ~p"
	   "~n   Type:       ~p"
	   "~n   Access:     ~p"
	   "~n   DefVal:     ~p"
	   "~n   Status:     ~p"
	   "~n   Units:      ~p"
	   "~n   Parent:     ~p"
	   "~n   SubIndex:   ~p",
	   [NewVarName, Type, Access, DefVal, 
	    Status, Units, Parent, SubIndex], Line),
    update_status(NewVarName, Status),
    snmpc_lib:test_father(Parent, NewVarName, SubIndex, Line),
    ASN1type = snmpc_lib:make_ASN1type(Type),
    snmpc_lib:register_oid(Line, NewVarName, Parent, SubIndex),
    Description1 = make_description(Desc1),
    NewME = #me{aliasname   = NewVarName, 
		asn1_type   = ASN1type,
		entrytype   = variable,
		access      = Access,
		description = Description1, 
		units       = Units,
		assocList   = DefVal},  
    NewME2 = snmpc_lib:resolve_defval(NewME),
    %% hmm, should this be done in resolve_defval?
    VI = snmpc_lib:make_variable_info(NewME2), 
    snmpc_lib:add_cdata(#cdata.mes,
			[NewME2#me{assocList = [{variable_info, VI}]}]),
    definitions_loop(T, Data);

definitions_loop([{#mc_module_identity{name         = NewVarName,
				       last_updated = LU,
				       organization = Org,
				       contact_info = CI,
				       description  = Desc,
				       revisions    = Revs0, 
				       name_assign  = {Parent, SubIndex}},
		   Line}|T],
		 Data) ->
    ?vlog2("defloop -> module-identity: "
	   "~n   NewVarName: ~p"
	   "~n   LU:         ~p"
	   "~n   Org:        ~p"
	   "~n   CI:         ~p"
	   "~n   Desc:       ~p"
	   "~n   Revs0:      ~p"
	   "~n   Parent:     ~p"
	   "~n   SubIndex:   ~w",
	   [NewVarName, LU, Org, CI, Desc, Revs0, Parent, SubIndex], Line),
    ensure_macro_imported('MODULE-IDENTITY', Line),
    snmpc_lib:register_oid(Line, NewVarName, Parent, SubIndex),
    Revs = [{R,D}||#mc_revision{revision = R,description = D} <- Revs0],
    MI = #module_identity{last_updated = LU,
			  organization = Org,
			  contact_info = CI,
			  description  = Desc,
			  revisions    = Revs},
    CDATA = get(cdata),
    put(cdata, CDATA#cdata{module_identity = MI}),
    snmpc_lib:add_cdata(
      #cdata.mes,
      [snmpc_lib:makeInternalNode2(false, NewVarName)]),
    definitions_loop(T, Data);    

definitions_loop([{#mc_internal{name      = NewVarName,
				macro     = Macro,
				parent    = Parent,
				sub_index = SubIndex},Line}|T],
		 Data) ->
    ?vlog2("defloop -> internal:"
	   "~n   NewVarName: ~p"
	   "~n   Macro:      ~p"
	   "~n   Parent:     ~p"
	   "~n   SubIndex:   ~w", 
	   [NewVarName, Macro, Parent, SubIndex], Line),
    ensure_macro_imported(Macro, Line),
    snmpc_lib:register_oid(Line, NewVarName, Parent, SubIndex),
    snmpc_lib:add_cdata(
      #cdata.mes,
      [snmpc_lib:makeInternalNode2(false, NewVarName)]),
    definitions_loop(T, Data);    

%% A trap message
definitions_loop([{#mc_trap{name        = TrapName,
			    enterprise  = EnterPrise, 
			    vars        = Variables, 
			    description = Desc1,
			    num         = SpecificCode}, Line}|T],
		 Data) ->
    ?vlog2("defloop -> trap:"
	   "~n   TrapName:     ~p"
	   "~n   EnterPrise:   ~p"
	   "~n   Variables:    ~p"
	   "~n   SpecificCode: ~p",
	   [TrapName, EnterPrise, Variables, SpecificCode], Line),
    update_status(TrapName, undefined),
    CDATA = get(cdata),
    snmpc_lib:check_trap_name(EnterPrise, Line, CDATA#cdata.mes),
    Descriptions = make_description(Desc1),
    Trap = #trap{trapname      = TrapName, 
		 enterpriseoid = EnterPrise,
		 specificcode  = SpecificCode,
		 %% oidobjects: Store Variables temporary here.
		 %%             This will be replaced later in the 
		 %%             get_final_mib function by a call to
		 %%             the update_trap_objects function.
		 oidobjects    = Variables,  
		 description   = Descriptions},
    lists:foreach(fun(Trap2) -> snmpc_lib:check_trap(Trap2, Trap, Line) end, 
		  CDATA#cdata.traps), 
    snmpc_lib:add_cdata(#cdata.traps, [Trap]),
    definitions_loop(T, Data);    

definitions_loop([{#mc_object_type{name        = NameOfEntry, 
				   syntax      = Type, 
				   max_access  = Eaccess,
				   kind        = {table_entry, Index},
				   status      = Estatus,
				   name_assign = SubIndex},Eline}|T], 
		 Data) ->
    ?vlog("defloop -> object_type (table_entry):"
	  "~n   NameOfEntry: ~p"
	  "~n   Type:        ~p"
	  "~n   Eaccess:     ~p"
	  "~n   Index:       ~p"
	  "~n   Estatus:     ~p"
	  "~n   SubIndex:    ~p"
	  "~n   SubIndex:    ~p"
	  "~n   Eline:       ~p",
	  [NameOfEntry, Type, Eaccess, Index, Estatus, SubIndex, Eline]),
    update_status(NameOfEntry, Estatus),
    snmpc_lib:print_error("Misplaced TableEntry definition (~w)",
			  [NameOfEntry], Eline),
    definitions_loop(T, Data);

definitions_loop([{#mc_notification{name   = TrapName,
				    status = deprecated}, Line}|T],
		 #dldata{deprecated = false} = Data) ->
    ?vinfo2("defloop -> notification ~w is deprecated => ignored",
	    [TrapName], Line),    
    update_status(TrapName, deprecated),
    ensure_macro_imported('NOTIFICATION-TYPE', Line),
    definitions_loop(T, Data);    

definitions_loop([{#mc_notification{name   = TrapName,
				    status = obsolete}, Line}|T],
		 Data) ->
    ?vlog2("defloop -> notification ~w is obsolete => ignored", 
	   [TrapName], Line),
    update_status(TrapName, obsolete),
    ensure_macro_imported('NOTIFICATION-TYPE', Line),
    definitions_loop(T, Data);    

definitions_loop([{#mc_notification{name        = TrapName,
				    vars        = Variables,
				    status      = Status,
				    description = Desc,
				    name_assign = {Parent, SubIndex}},Line}|T],
		 Data) ->
    ?vlog2("defloop -> notification:"
	   "~n   TrapName:  ~p"
	   "~n   Variables: ~p"
	   "~n   Status:    ~p"
	   "~n   Parent:    ~p"
	   "~n   SubIndex:  ~p",
	   [TrapName, Variables, Status, Parent, SubIndex], Line),
    update_status(TrapName, Status),
    ensure_macro_imported('NOTIFICATION-TYPE', Line),
    CDATA = get(cdata),
    snmpc_lib:register_oid(Line, TrapName, Parent, SubIndex),
    Descriptions = make_description(Desc),
    Notif = #notification{trapname    = TrapName,
			  description = Descriptions,
			  %% oidobjects: Store Variables temporary here.
			  %%             This will be replaced later in the 
			  %%             get_final_mib function by a call to
			  %%             the update_trap_objects function.
			  oidobjects  = Variables}, 
    snmpc_lib:check_notification(Notif, Line, CDATA#cdata.traps),
    snmpc_lib:add_cdata(#cdata.traps, [Notif]),
    definitions_loop(T, Data);    

definitions_loop([{#mc_agent_capabilities{name        = Name,
					  status      = Status,
					  description = Desc,
					  reference   = Ref,
					  modules     = Mods, 
					  name_assign = {Parent, SubIdx}},Line}|T], Data) ->
    ?vlog2("defloop -> agent_capabilities ~p:"
	   "~n   Status:       ~p"
	   "~n   Desc:         ~p"
	   "~n   Ref:          ~p"
	   "~n   Mods:         ~p"
	   "~n   Parent:       ~p"
	   "~n   SubIndex:     ~p", 
	   [Name, Status, Desc, Ref, Mods, Parent, SubIdx], Line),
    ensure_macro_imported('AGENT-CAPABILITIES', Line),
    case get(agent_capabilities) of
	true ->
	    update_status(Name, Status),
	    snmpc_lib:register_oid(Line, Name, Parent, SubIdx),
	    NewME       = snmpc_lib:makeInternalNode2(false, Name),
	    Description = make_description(Desc), 
	    Reference   = 
		case Ref of
		    undefined ->
			[];
		    _ ->
			[{reference, Ref}]
		end,
	    Modules     = 
		case Mods of
		    undefined ->
			[];
		    [] ->
			[];
		    _ ->
			[{modules, Mods}]
		end,
	    AssocList = Reference ++ Modules,
	    NewME2 = NewME#me{description = Description,
			      assocList   = AssocList}, 
	    snmpc_lib:add_cdata(#cdata.mes, [NewME2]);
	_ ->
	    ok
    end,
    definitions_loop(T, Data);

definitions_loop([{#mc_module_compliance{name        = Name,
					 status      = Status,
					 description = Desc,
					 reference   = Ref,
					 modules     = Mods, 
					 name_assign = {Parent, SubIdx}},Line}|T], Data) ->
    ?vlog2("defloop -> module_compliance: ~p"
	   "~n   Status:       ~p"
	   "~n   Desc:         ~p"
	   "~n   Ref:          ~p"
	   "~n   Mods:         ~p"
	   "~n   Parent:       ~p"
	   "~n   SubIndex:     ~p", 
	   [Name, Status, Desc, Ref, Mods, Parent, SubIdx], Line),
    ensure_macro_imported('MODULE-COMPLIANCE', Line),
    case get(module_compliance) of
	true ->
	    update_status(Name, Status),
	    snmpc_lib:register_oid(Line, Name, Parent, SubIdx),
	    NewME       = snmpc_lib:makeInternalNode2(false, Name),
	    Description = make_description(Desc), 
	    Reference   = 
		case Ref of
		    undefined ->
			[];
		    _ ->
			[{reference, Ref}]
		end,
	    Modules    = 
		case Mods of
		    undefined ->
			[];
		    [] ->
			[];
		    _ ->
			[{modules, Mods}]
		end,
	    AssocList = Reference ++ Modules,
	    NewME2 = NewME#me{description = Description,
			      assocList   = AssocList}, 
	    snmpc_lib:add_cdata(#cdata.mes, [NewME2]);
	_ ->
	    ok
    end,
    definitions_loop(T, Data);

definitions_loop([{#mc_object_group{name        = Name,
				    objects     = GroupObjects,
				    status      = Status,
				    description = Desc,
				    reference   = Ref,
				    name_assign = {Parent,SubIndex}}, Line}|T],
		 Data) ->
    ?vlog2("defloop -> object_group ~p:"
	   "~n   GroupObjects: ~p"
	   "~n   Status:       ~p"
	   "~n   Desc:         ~p"
	   "~n   Ref:          ~p"
	   "~n   Parent:       ~p"
	   "~n   SubIndex:     ~p", 
	   [Name, GroupObjects, Status, Desc, Ref, Parent, SubIndex], Line),
    ensure_macro_imported('OBJECT-GROUP', Line),
    GroupBool = get_group_check(get(options)),
    case GroupBool of
	true ->
	    snmpc_lib:add_cdata(#cdata.objectgroups,
				       [{Name,GroupObjects,Line}]),
	    %% Check that the group members has been defined 
	    %% and that they have the correct status
	    snmpc_lib:check_object_group(Name, GroupObjects,
					 Line, Status);
	_ ->
	    ok
    end,

    update_status(Name, Status),
    snmpc_lib:test_father(Parent, Name, SubIndex, Line),
    snmpc_lib:register_oid(Line, Name, Parent, SubIndex),
    Description = make_description(Desc),
    NewME = #me{aliasname   = Name, 
		entrytype   = group,
		access      = 'not-accessible', 
		description = Description, 
		assocList   = [{kind,    object}, 
			       {objects, GroupObjects}]},  
    snmpc_lib:add_cdata(#cdata.mes, [NewME]),

    definitions_loop(T, Data);

definitions_loop([{#mc_notification_group{name        = Name,
					  objects     = GroupObjects,
					  status      = Status,
					  description = Desc,
					  reference   = Ref,
					  name_assign = {Parent,SubIndex}}, 
		   Line}
		  |T], Data) ->
    ?vlog2("defloop -> notification_group ~p:"
	   "~n   GroupObjects: ~p"
	   "~n   Status:       ~p"
	   "~n   Desc:         ~p"
	   "~n   Ref:          ~p"
	   "~n   Parent:       ~p"
	   "~n   SubIndex:     ~p",
	   [Name, GroupObjects, Status, Desc, Ref, Parent, SubIndex], Line),
    ensure_macro_imported('NOTIFICATION-GROUP', Line),
    GroupBool = get_group_check(get(options)),
    case GroupBool of
	true ->
	    snmpc_lib:add_cdata(#cdata.notificationgroups,
				       [{Name,GroupObjects,Line}]),

	    %% Check that the group members has been defined 
	    %% and that they have the correct status
	    snmpc_lib:check_notification_group(Name, GroupObjects,
					       Line, Status);
	_ ->
	    ok
    end,

    update_status(Name, Status),
    snmpc_lib:test_father(Parent, Name, SubIndex, Line),
    snmpc_lib:register_oid(Line, Name, Parent, SubIndex),
    Description = make_description(Desc),
    NewME = #me{aliasname   = Name, 
		entrytype   = group,
		access      = 'not-accessible', 
		description = Description, 
		assocList   = [{kind,    notification}, 
			       {objects, GroupObjects}]},  
    snmpc_lib:add_cdata(#cdata.mes, [NewME]),

    definitions_loop(T, Data);

definitions_loop([{#mc_object_type{name   = NameOfTable,
				   syntax = {{sequence_of, SeqName},_},
				   status = Tstatus},Tline}, 
		  Entry, Seq|T],
		Data) ->
    ?vlog("defloop -> object_type (sequence_of): "
	  "~n   NameOfTable: ~p"
	  "~n   SeqName:     ~p"
	  "~n   Tline:       ~p"
	  "~n   Entry:       ~p"
	  "~n   Seq:         ~p",
	  [NameOfTable, SeqName, Tline, Entry, Seq]),
    update_status(NameOfTable, Tstatus),
    case Entry of
	{#mc_object_type{syntax      = {{type, SeqName},_line},
			 max_access  = 'not-accessible',
			 kind        = {table_entry, _IndexingInfo},
			 name_assign = {_NameOfTable,[1]}}, _Eline} ->
	    case Seq of
		{#mc_sequence{name = SeqName}, Sline} ->
		    snmpc_lib:error("Internal error. Correct incorrect "
					   "table (~p,~w).",[SeqName,Sline],
				    Tline);
		_ ->
		    ?vinfo("defloop -> Invalid sequence: ~p", [Seq]),
		    snmpc_lib:print_error(
		      "Invalid SEQUENCE OF '~p'.",
		      [safe_elem(1,safe_elem(2,Seq))],Tline)
	    end;
	Else ->
	    ?vinfo("defloop -> Invalid table entry: "
		   "~n   ~p", [Else]),
	    snmpc_lib:print_error(
	      "Invalid TableEntry '~p' (check STATUS, Sequence name, Oid)",
	      [safe_elem(1,safe_elem(2,Entry))],Tline)
    end,
    definitions_loop(T, Data);

definitions_loop([{#mc_object_type{name   = NameOfTable,
				   syntax = {{sequence_of, SeqName},_},
				   status = Tstatus},Tline}|T],
		 Data) ->
    ?vlog("defloop -> object_type (sequence_of):"
	  "~n   object_type: ~p"
	  "~n   sequence_of: ~p"
	  "~n   Tline:       ~p", [NameOfTable, SeqName, Tline]),
    update_status(NameOfTable, Tstatus),
    snmpc_lib:print_error("Invalid statements following table ~p.",
			  [NameOfTable],Tline),
    definitions_loop(T, Data);

definitions_loop([{#mc_sequence{name   = SeqName,
				fields = _FieldList},Line}|T],
		 Data) ->
    ?vwarning2("Unexpected SEQUENCE ~w => ignoring", [SeqName], Line),
    definitions_loop(T, Data);

definitions_loop([{Obj,Line}|T], Data) ->
    ?vinfo2("defloop -> unknown error"
	    "~n   Obj:  ~p", [Obj], Line),
    snmpc_lib:print_error("Unknown Error in MIB. "
	 "Can't describe the error better than this: ~999p ignored."
	 " Please send a trouble report to support@erlang.ericsson.se.",
				 [Obj], Line),
    definitions_loop(T, Data);

definitions_loop([], _Data) ->
    ?vlog("defloop -> done", []),
    case get(augmentations) of
        true ->
            CData = get(cdata),
            put(cdata, CData#cdata{mes = augmentations(CData#cdata.mes)}),
            ok;
        false ->
            ok
    end.

augmentations(
  [#me{
      aliasname = AliasName,
      assocList =
          [{table_info,
            #table_info{
               index_types =
                   {augments, SrcTableEntry, Line}} = TableInfo}|Ref]} = Me
   |Mes]) ->
    ?vlog("augmentations(~w) ->"
          "~n   NameOfTable:  ~p"
          "~n   IndexingInfo: ~p"
          "~n   Sline:        ~p",
          [?LINE, AliasName, {augments, SrcTableEntry}, Line]),
    NewTableInfo = snmpc_lib:fix_table_info_augmentation(TableInfo),
    [Me#me{assocList = [{table_info,NewTableInfo}|Ref]}
     |augmentations(Mes)];
augmentations([Me | Mes]) ->
     [Me|augmentations(Mes)];
augmentations([]) ->
    ?vlog("augmentations -> done", []),
    [].



safe_elem(N,T) ->
    case catch(element(N,T)) of
	{'EXIT',_} ->
	    "no more information available";
	X -> X
    end.


%% An table index is either: 
%%   a) part of the table
%%   b) not part of the table and defined *before* the table
%%   c) not part of the table and defined *after* the table

%% A correct column
define_cols([{#mc_object_type{name        = NameOfCol,
			      syntax      = Type1,
			      max_access  = Access,
			      kind        = {variable,Defval},
			      status      = Status,
			      description = Desc,
			      units       = Units,
			      name_assign = {NameOfEntry,[SubIndex]}},
	      Oline}|Rest],
	    SubIndex, 
	    [{NameOfCol,Type2}|Fields], NameOfEntry, TableName, ColMEs) ->
    ?vlog("defcols -> object_type (variable):"
	  "~n   NameOfCol:  ~p"
	  "~n   Type1:      ~p"
	  "~n   Access:     ~p"
	  "~n   Defval:     ~p"
	  "~n   Status      ~p"
	  "~n   Units       ~p"
	  "~n   NameOfEntry ~p"
	  "~n   Oline:      ~p",
	  [NameOfCol, Type1, Access, Defval, Status, Units, 
	   NameOfEntry, Oline]),
    update_status(NameOfCol, Status),
    Deprecated = get_deprecated(get(options)),
    ASN1type = snmpc_lib:make_ASN1type(Type1),
    case (snmpc_lib:make_ASN1type(Type2))#asn1_type.bertype of
	T2 when T2 == ASN1type#asn1_type.bertype -> ok;
	_Else ->
	    snmpc_lib:error(
	      "Types for ~p differs from the SEQUENCE definition. ",
	      [NameOfCol],Oline)
    end,
    NewAccess = % a simple way to get the obsolete behaviour
	if
	    Status =:= obsolete ->
		%% Be quiet and don't implement
		'not-accessible';
	    (Status =:= deprecated) andalso (Deprecated =:= false) ->
		%% The compiler chooses not to implement the column.
		?vinfo2("object_type ~w is deprecated => ignored",
			[NameOfCol], Oline),
		'not-accessible';
	    true -> Access
	end,
    snmpc_lib:register_oid(Oline,NameOfCol,NameOfEntry,[SubIndex]),
    Description = make_description(Desc),
    ColumnME = snmpc_lib:resolve_defval(
		 #me{oid         = SubIndex,
		     aliasname   = NameOfCol, 
		     asn1_type   = ASN1type,
		     entrytype   = table_column, 
		     access      = NewAccess,
		     description = Description,
		     units       = Units,   %% Propably not usefull
		     assocList   = [{table_name,TableName} | Defval]}),
    define_cols(Rest,SubIndex+1,Fields,NameOfEntry,TableName,
		[ColumnME|ColMEs]);

%% A "hole" (non-consecutive columns) in the table.
%% Implemented as a not-accessible column so Col always is index in
%% row tuple.
define_cols([{#mc_object_type{name        = NameOfCol,
			      syntax      = Type1,
			      max_access  = Access,
			      kind        = Kind,
			      status      = Status,
			      name_assign = {NameOfEntry,[SubIndex]}},
	      Oline}|Rest],
	    ExpectedSubIndex, Fields, NameOfEntry, TableName, ColMEs) 
  when SubIndex > ExpectedSubIndex ->
    ?vlog("defcols -> object_type (non consecutive cols):"
	  "~n   NameOfCol:  ~p"
	  "~n   Type1:      ~p"
	  "~n   Access:     ~p"
	  "~n   Status      ~p"
	  "~n   NameOfEntry ~p"
	  "~n   Oline:      ~p",
	  [NameOfCol, Type1, Access, Status, NameOfEntry, Oline]),
    update_status(NameOfCol, Status),
    Int = {{type, 'INTEGER'},Oline},
    GeneratedColumn =  
	%% be sure to use an invalid column name here!
	{#mc_object_type{name        = '$no_name$', 
                         syntax      = Int, 
                         max_access  = 'not-accessible',
                         kind        = {variable, [{defval,0}]}, 
                         status      = current, 
                         description = undefined,
                         name_assign = {NameOfEntry, [ExpectedSubIndex]}}, 
         Oline},
    define_cols([GeneratedColumn, 
                 {#mc_object_type{name        = NameOfCol, 
                                  syntax      = Type1, 
                                  max_access  = Access, 
                                  kind        = Kind, 
                                  status      = Status,
                                  description = undefined,
		                  name_assign = {NameOfEntry,[SubIndex]}},
                  Oline}|Rest], ExpectedSubIndex,
		 [{'$no_name$', Int}|Fields], NameOfEntry, TableName,ColMEs) ;

%% Ok. done. All fields are eaten.
define_cols(Rest, _SubIndex, [], _NameOfEntry, _TableName, ColMEs) ->
    {ColMEs, Rest};


%% Error Handling

%% The name of the field and object is the same
define_cols([{#mc_object_type{name        = NameOfCol,
			      kind        = Kind,
			      name_assign = SubIndex}, Oline}|Rest],
	    SubIndex2, [{NameOfCol, _Type2}|Fields], 
	    NameOfEntry, TableName, ColMEs) ->
    ?vlog("defcols -> object_type (name of field and object is the same):"
	  "~n   NameOfCol:   ~p"
	  "~n   Kind:        ~p"
	  "~n   SubIndex:    ~p"
	  "~n   Oline:       ~p"
	  "~n   SubIndex2:   ~p"
	  "~n   NameOfEntry  ~p"
	  "~n   TableName    ~p",
      [NameOfCol,Kind,SubIndex,Oline,SubIndex2,NameOfEntry,TableName]),    
    SIok = case SubIndex of
	       {Parent,[_SI]} when Parent =/= NameOfEntry ->
		   snmpc_lib:print_error(
		     "Invalid parent ~p for table column ~p (should be ~p).",
		     [Parent,NameOfCol,NameOfEntry],Oline),
		   error;
	       {NameOfEntry,[SubIndex2]} ->
		   ok;
	       {NameOfEntry,[SI]} ->
		   snmpc_lib:print_error(
		     "Invalid column number ~p for column ~p.",
		     [SI, NameOfCol], Oline),
		   error;
	       _Q ->
		   snmpc_lib:print_error(
		     "Invalid parent for column ~p.",[NameOfCol],Oline),
		   error
	   end,
    Kok = case Kind of
	      {variable,_} ->
		  ok;
	      _Q2 ->
		  snmpc_lib:print_error(
		    "Expected a table column.",[],Oline),
		  error
	  end,
    case {SIok, Kok} of
	{ok, ok} ->
	    snmpc_lib:print_error("Invalid table column definition for"
				  " ~p.",[NameOfCol],Oline);
	_Q4 ->
	    done % already reported
    end,
    define_cols(Rest,SubIndex2+1,Fields,NameOfEntry,TableName,ColMEs);

%% It's an object-type but everything else is wrong
define_cols([{#mc_object_type{name = NameOfCol},Oline}|Rest],SubIndex2,Fields,
	    NameOfEntry,TableName,ColMEs) ->
    snmpc_lib:print_error(
      "Number of columns differs from SEQUENCE definition (object:~p).",
      [NameOfCol],Oline),
    define_cols(Rest,SubIndex2+1,Fields,NameOfEntry,TableName,ColMEs);

define_cols([{Obj,Line}|Tl], _SubIndex,_,_,_,ColMEs) ->
    snmpc_lib:print_error("Corrupt table definition.",[],Line),
    {ColMEs,[{Obj,Line}|Tl]};
define_cols(Rest, _SubIndex,_,_,_,ColMEs) ->
    snmpc_lib:print_error("Corrupt table definition.",[]),
    {ColMEs,Rest}.


%% Table indexes can either be: 
%%   a) part of the table (a column)
%%   b) not part of the table and defined *before* the table
%%   c) not part of the table and defined *after* the table

after_indexes_type({indexes, Indexes}, Objs) ->
    after_indexes_type2(Indexes, Objs);
after_indexes_type(_, _) ->
    [].

after_indexes_type2(Indexes, Objs) ->
    after_indexes_type2(Indexes, Objs, []).

after_indexes_type2([], _Objs, IndexesASN1types) ->
    IndexesASN1types;
after_indexes_type2([Index|Indexes], Objs, Acc) ->
    Acc2 = after_indexes_type3(Index, Objs, Acc),
    after_indexes_type2(Indexes, Objs, Acc2).

after_indexes_type3(_Index, [], Acc) ->
    Acc;
after_indexes_type3(Index, 
		    [{#mc_object_type{name   = Index, 
				      syntax = Syntax},_}|_], Acc) ->
    ASN1 = snmpc_lib:make_ASN1type(Syntax), 
    [{Index, ASN1}|Acc];
after_indexes_type3(Index, [_|Objs], Acc) ->
    after_indexes_type3(Index, Objs, Acc). 



ensure_macro_imported(dummy, _Line) -> ok;
ensure_macro_imported(Macro, Line) ->
    Macros = (get(cdata))#cdata.imported_macros,
    case lists:member(Macro, Macros) of
	true -> ok;
	false ->
	    snmpc_lib:print_error("Macro ~p not imported.", [Macro], Line)
    end.

test_table(NameOfTable, Taccess, Kind, _Tindex, Tline) ->
    if
	Taccess =/= 'not-accessible' ->
	    snmpc_lib:print_error(
	      "Table ~w must have STATUS not-accessible",
	      [NameOfTable],Tline),
	    error;
	Kind =/= {variable,[]} ->
	    snmpc_lib:print_error(
	      "Bad table definition (~w).",
	      [NameOfTable],Tline),
	    error;
	true ->
	    ok
    end.

save(Filename, MibName, Options) ->
    R     = filename:rootname(Filename),
    File1 = filename:basename(R),
    File3 = snmpc_misc:to_upper(File1),
    case snmpc_misc:to_upper(atom_to_list(MibName)) of
	File3 ->
	    {value, OutDirr} = snmpc_misc:assq(outdir, Options),
	    OutDir = snmpc_misc:ensure_trailing_dir_delimiter(OutDirr),
	    File2 = (OutDir ++ File1) ++ ".bin",
	    {ok, MIB} = snmpc_lib:get_final_mib(File1, Options),
	    case get(errors) of
		undefined ->
		    case file:write_file(File2, term_to_binary(MIB)) of
			ok ->
			    {ok, File2};
			_Err ->
			    snmpc_lib:error(
			      "Couldn't write file \"~s\".",[File2])
		    end;
		E ->
		    ?vlog("save failed: "
			  "~n   ~p", [E]),
		    {'EXIT',error}
	    end;
	MibNameL ->
	    snmpc_lib:error("Mibname (~s) differs from filename (~s).",
				   [MibNameL, File1])
    end.
    
%% parse takes a text file as a input and the output is a list of tokens. 
%% Input: FileName (file of mibs)
%% Output: {ok, Mib} where MIB is a tuple of Tokens.
%%         {error, {LineNbr, Mod, Msg} an error on line number LineNb.


parse(FileName) ->
%%     ?vtrace("parse -> start tokenizer for ~p", [FileName]),
    case snmpc_tok:start_link(reserved_words(),
			      [{file, FileName ++ ".mib"},
			       {forget_stringdata, true}]) of
	{error,ReasonStr} ->
	    snmpc_lib:error(lists:flatten(ReasonStr),[]);
	{ok, TokPid} ->
%% 	    ?vtrace("parse ->  tokenizer start, now get tokens", []),
	    Toks = snmpc_tok:get_all_tokens(TokPid),
%% 	    ?vtrace("parse ->  tokens: ~p", [Toks]),
	    set_version(Toks),
	    %% ?vtrace("parse -> lexical analysis: ~n~p", [Toks]),
            CDataArg =
                case lists:keysearch(module, 1, get(options)) of
                    {value, {module, M}} -> {module, M};
                    _ -> {file, FileName ++ ".funcs"}
                end,
            put(cdata,snmpc_lib:make_cdata(CDataArg)),
%% 	    ?vtrace("parse ->  stop tokenizer and then do the actual parse", 
%% 		    []),
	    snmpc_tok:stop(TokPid),
	    Res = if 
		      is_list(Toks) ->
			  snmpc_mib_gram:parse(Toks);
		      true ->
			  Toks
		  end,
%% 	    ?vtrace("parse -> parsed result: ~n~p", [Res]),
	    case Res of
		{ok, PData} ->
		    {ok, PData};
		{error, {LineNbr, Mod, Msg}} ->
		    case catch format_yecc_error(LineNbr, Msg) of
			{Line, Format, Data} -> 
			    snmpc_lib:error(Format,Data,Line);
			_Q -> % sorry, have to use ugly yecc printouts
			    Str = apply(Mod, format_error, [Msg]),
			    snmpc_lib:error("~s",[Str],LineNbr)
		    end
	    end
    end.

set_version(Toks) when is_list(Toks) ->
%% MODULE-IDENTITY _must_ be invoked in SNMPv2 according to RFC1908
    case lists:keymember('MODULE-IDENTITY',1,Toks) of
	true ->
	    put(snmp_version,2);
	false ->
	    put(snmp_version,1)
    end;
set_version(_) ->
    put(snmp_version,1).


%% YeccGeneratedFile:format_error/1 is bad.
format_yecc_error(Line, [ErrMsg, [${,Category, $,, _LineStr,$,, Value, $}]]) ->
    {Line, "~s \"~s\" (~s).", [ErrMsg, Value, Category]}.

%% The same as the (quoted) Terminals in the snmpc_mib_gram.yrl
reserved_words() -> 
    [ 
      'ACCESS', 
      'BEGIN', 
      'BIT', 
      'CONTACT-INFO',
      'Counter', 
      'DEFINITIONS', 
      'DEFVAL', 
      'DESCRIPTION', 
      'DISPLAY-HINT',
      'END', 
      'ENTERPRISE', 
      'FROM', 
      'Gauge', 
      'IDENTIFIER', 
      'IDENTIFIER',
      'IMPORTS', 
      'INDEX', 
      'INTEGER', 
      'IpAddress', 
      'LAST-UPDATED',
      'NetworkAddress', 
      'OBJECT', 
      'OBJECT', 
      'OBJECT-TYPE', 
      'OCTET', 
      'OF',
      'Opaque', 
      'REFERENCE', 
      'SEQUENCE', 
      'SIZE', 
      'STATUS', 
      'STRING',
      'SYNTAX', 
      'TRAP-TYPE', 
      'TimeTicks', 
      'VARIABLES', 

      %% v2
      'LAST-UPDATED',
      'ORGANIZATION',
      'CONTACT-INFO',
      'MODULE-IDENTITY',
      'NOTIFICATION-TYPE',
      'MODULE-COMPLIANCE',
      'OBJECT-GROUP',
      'NOTIFICATION-GROUP',
      'REVISION',
      'OBJECT-IDENTITY',
      'MAX-ACCESS',
      'UNITS',
      'AUGMENTS',
      'IMPLIED',
      'OBJECTS',
      'TEXTUAL-CONVENTION',
      'OBJECT-GROUP',
      'NOTIFICATION-GROUP',
      'NOTIFICATIONS',
      'MODULE-COMPLIANCE',
      'AGENT-CAPABILITIES',
      'PRODUCT-RELEASE',
      'SUPPORTS',
      'INCLUDES',
      'MODULE',
      'MANDATORY-GROUPS',
      'GROUP',
      'WRITE-SYNTAX',
      'MIN-ACCESS',
      'BITS'
     ]
.
