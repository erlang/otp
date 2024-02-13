%% vim: tabstop=8:shiftwidth=4
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
-module(asn1ct).
-moduledoc """
ASN.1 compiler and compile-time support functions

The ASN.1 compiler takes an ASN.1 module as input and generates a corresponding
Erlang module, which can encode and decode the specified data types.
Alternatively, the compiler takes a specification module specifying all input
modules, and generates a module with encode/decode functions. In addition, some
generic functions can be used during development of applications that handles
ASN.1 data (encoded as `BER` or `PER`).

> #### Note {: .info }
>
> By default in OTP 17, the representation of the `BIT STRING` and
> `OCTET STRING` types as Erlang terms were changed. `BIT STRING` values are now
> Erlang bit strings and `OCTET STRING` values are binaries. Also, an undecoded
> open type is now wrapped in an `asn1_OPENTYPE` tuple. For details, see
> [BIT STRING](asn1_getting_started.md#bit-string),
> [OCTET STRING](asn1_getting_started.md#octet-string), and
> [ASN.1 Information Objects](asn1_getting_started.md#Information-Object) in the
> User's Guide.
>
> To revert to the old representation of the types, use option
> `legacy_erlang_types`.

> #### Note {: .info }
>
> In OTP R16, the options were simplified. The back end is chosen using one of
> the options `ber`, `per`, `uper` or `jer`. Options `optimize`, `nif`, and
> `driver` options are no longer necessary (and the ASN.1 compiler generates a
> warning if they are used). Options `ber_bin`, `per_bin`, and `uper_bin`
> options still work, but generates a warning.
>
> Another change in OTP R16 is that the generated function `encode/2` always
> returns a binary. Function `encode/2` for the `BER` back end used to return an
> iolist.
""".

%% Compile Time functions for ASN.1 (e.g ASN.1 compiler).

%% Public exports
-export([compile/1, compile/2]).
-export([test/1, test/2, test/3, value/2, value/3]).

%% Application internal exports
-export([compile_asn/3,compile_asn1/3,compile_py/3,compile/3,
	 vsn/0,
	 get_name_of_def/1,get_pos_of_def/1,
	 unset_pos_mod/1]).
-export([read_config_data/1,get_gen_state_field/1,
	 partial_inc_dec_toptype/1,update_gen_state/2,
	 get_tobe_refed_func/1,reset_gen_state/0,is_function_generated/1,
	 generated_refed_func/1,next_refed_func/0,
	 update_namelist/1,step_in_constructed/0,
	 add_tobe_refed_func/1,add_generated_refed_func/1,
	 maybe_rename_function/3,current_sindex/0,
	 set_current_sindex/1,maybe_saved_sindex/2,
	 parse_and_save/2,verbose/3,warning/3,warning/4,error/3,format_error/1]).
-export([save_config/2,save_gen_state/2,save_gen_state/3]).
-export([get_bit_string_format/0,use_legacy_types/0]).

-include("asn1_records.hrl").
-include_lib("stdlib/include/erl_compile.hrl").
-include_lib("kernel/include/file.hrl").

-ifndef(vsn).
-define(vsn,"0.0.1").
-endif.

-define(unique_names,0).
-define(dupl_uniquedefs,1).
-define(dupl_equaldefs,2).
-define(dupl_eqdefs_uniquedefs,?dupl_equaldefs bor ?dupl_uniquedefs).

%% Removed functions

-removed({decode,'_',"use Mod:decode/2 instead"}).
-removed({encode,'_',"use Mod:encode/2 instead"}).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the interface to the compiler

-doc(#{equiv => compile/2}).
-spec compile(Asn1module) -> ok | {error, Reason} when
      Asn1module :: atom() | string(),
      Reason :: term().
compile(File) ->
    compile(File,[]).

-doc """
Compiles the `ASN.1` module `Asn1module` and generates an Erlang module
`Asn1module.erl` with encode and decode functions for the types defined in
`Asn1module`. For each ASN.1 value defined in the module, an Erlang function
that returns the value in Erlang representation is generated.

If `Asn1module` is a filename without extension, first `".asn1"` is assumed,
then `".asn"`, and finally `".py"` (to be compatible with the old ASN.1
compiler). `Asn1module` can be a full pathname (relative or absolute) including
filename with (or without) extension.[](){: #asn1set }

If it is needed to compile a set of `ASN.1` modules into an Erlang file with
encode/decode functions, ensure to list all involved files in a configuration
file. This configuration file must have a double extension `".set.asn"`
(`".asn"` can alternatively be `".asn1"` or `".py"`). List the input file names
within quotation marks (""), one at each row in the file. If the input files are
`File1.asn`, `File2.asn`, and `File3.asn`, the configuration file must look as
follows:

```text
File1.asn
File2.asn
File3.asn
```

The output files in this case get their names from the configuration file. If
the configuration file is named `SetOfFiles.set.asn`, the names of the output
files are `SetOfFiles.hrl, SetOfFiles.erl, and SetOfFiles.asn1db`.

Sometimes in a system of `ASN.1` modules, different default tag modes, for
example, `AUTOMATIC`, `IMPLICIT`, or `EXPLICIT`. The multi-file compilation
resolves the default tagging as if the modules were compiled separately.

Name collisions is another unwanted effect that can occur in multi
file-compilation. The compiler solves this problem in one of two ways:

- If the definitions are identical, the output module keeps only one definition
  with the original name.
- If the definitions have the same name and differs in the definition, they are
  renamed. The new names are the definition name and the original module name
  concatenated.

If a name collision occurs, the compiler reports a `"NOTICE: ..."` message that
tells if a definition was renamed, and the new name that must be used to
encode/decode data.

`Options` is a list with options specific for the `ASN.1` compiler and options
that are applied to the Erlang compiler. The latter are not recognized as
`ASN.1` specific. The available options are as follows:

- **`ber | per | uper | jer`** - The encoding rule to be used. The supported
  encoding rules are Basic Encoding Rules (`ber`), Packed Encoding Rules (`per`)
  aligned, PER unaligned (`uper`) and JSON Encoding Rules (`jer`). The `jer`
  option can be used by itself to generate a module that only supports
  encoding/decoding to JER or it can be used as a supplementary option to ber,
  per and uper. In the latter case a module with for both the main encoding
  rules and JER will be generated. The exported functions for JER will then be
  `jer_encode(Type, Value)` and `jer_decode(Type, Bytes)`.

  The `jer` encoding rules (ITU-T X.697) are experimental in OTP 22. There is
  support for a subset of the X.697 standard, for example there is no support
  for:

  - JER encoding instructions
  - the REAL type

  Also note that when using the `jer` encoding rules the generated module will
  get a dependency to an external json component. The generated code is
  currently tested together with:

  - `jsx` which currently is the default.
  - `jsone` can be chosen instead of `jsx` by providing the option `{d,jsone}`.

  If the encoding rule option is omitted, `ber` is the default.

  The generated Erlang module always gets the same name as the `ASN.1` module.
  Therefore, only one encoding rule per `ASN.1` module can be used at runtime.

- **`der`** - With this option the Distinguished Encoding Rules (`der`) is
  chosen. DER is regarded as a specialized variant of the BER encoding rule.
  Therefore, this option only makes sense together with option `ber`. This
  option sometimes adds sorting and value checks when encoding, which implies a
  slower encoding. The decoding routines are the same as for `ber`.

- **`maps`** - This option changes the representation of the types `SEQUENCE`
  and `SET` to use maps (instead of records). This option also suppresses the
  generation of `.hrl` files.

  For details, see Section
  [Map representation for SEQUENCE and SET](asn1_getting_started.md#MAP_SEQ_SET)
  in the User's Guide.

- **`compact_bit_string`** - The `BIT STRING` type is decoded to "compact
  notation". _This option is not recommended for new code._ This option cannot
  be combined with the option `maps`.

  For details, see Section [BIT STRING](asn1_getting_started.md#bit-string) in
  the User's Guide.

  This option implies option `legacy_erlang_types`.

- **`legacy_bit_string`** - The `BIT STRING` type is decoded to the legacy
  format, that is, a list of zeroes and ones. _This option is not recommended
  for new code._ This option cannot be combined with the option `maps`.

  For details, see Section [BIT STRING](asn1_getting_started.md#bit-string) in
  the User's Guide

  This option implies option `legacy_erlang_types`.

- **`legacy_erlang_types`** - Use the same Erlang types to represent
  `BIT STRING` and `OCTET STRING` as in OTP R16.

  For details, see Section [BIT STRING](asn1_getting_started.md#bit-string) and
  Section [OCTET STRING](asn1_getting_started.md#octet-string) in the User's
  Guide.

  _This option is not recommended for new code._ This option cannot be combined
  with the option `maps`.

- **`{n2n, EnumTypeName}`** - Tells the compiler to generate functions for
  conversion between names (as atoms) and numbers and conversely for the
  specified `EnumTypeName`. There can be multiple occurrences of this option to
  specify several type names. The type names must be declared as `ENUMERATIONS`
  in the ASN.1 specification.

  If `EnumTypeName` does not exist in the ASN.1 specification, the compilation
  stops with an error code.

  The generated conversion functions are named `name2num_EnumTypeName/1` and
  `num2name_EnumTypeName/1`.

- **`noobj`** - Do not compile (that is, do not produce object code) the
  generated `.erl` file. If this option is omitted, the generated Erlang module
  is compiled.

- **`{i, IncludeDir}`** - Adds `IncludeDir` to the search-path for `.asn1db` and
  `ASN.1` source files. The compiler tries to open an `.asn1db` file when a
  module imports definitions from another `ASN.1` module. If no `.asn1db` file
  is found, the `ASN.1` source file is parsed. Several `{i, IncludeDir}` can be
  given.

- **`{outdir, Dir}`** - Specifies directory `Dir` where all generated files are
  to be placed. If this option is omitted, the files are placed in the current
  directory.

- **`asn1config`** - When using one of the specialized decodes, exclusive or
  selective decode, instructions must be given in a configuration file. Option
  `asn1config` enables specialized decodes and takes the configuration file in
  concern. The configuration file has the same name as the ASN.1 specification,
  but with extension `.asn1config`.

  For instructions for exclusive decode, see Section
  [Exclusive Decode](asn1_spec.md#Exclusive-Instruction) in the User's Guide.

  For instructions for selective decode, see Section
  [Selective Decode](asn1_spec.md#Selective-Instruction) in the User's Guide.

- **`undec_rest`** - A buffer that holds a message, being decoded it can also
  have some following bytes. Those following bytes can now be returned together
  with the decoded value. If an ASN.1 specification is compiled with this
  option, a tuple `{ok, Value, Rest}` is returned. `Rest` can be a list or a
  binary. Earlier versions of the compiler ignored those following bytes.

- **`no_ok_wrapper`** - With this option, the generated `encode/2` and
  `decode/2` functions do not wrap a successful return value in an `{ok,...}`
  tuple. If any error occurs, an exception will be raised.

- **`{macro_name_prefix, Prefix}`** - All macro names generated by the compiler
  are prefixed with `Prefix`. This is useful when multiple protocols that
  contain macros with identical names are included in a single module.

- **`{record_name_prefix, Prefix}`** - All record names generated by the
  compiler are prefixed with `Prefix`. This is useful when multiple protocols
  that contain records with identical names are included in a single module.

- **`verbose`** - Causes more verbose information from the compiler describing
  what it is doing.

- **`warnings_as_errors`** - Causes warnings to be treated as errors.

- **`deterministic`** - Causes all non-deterministic options to be stripped from
  the -asn1_info() attribute.

Any more option that is applied is passed to the final step when the generated
`.erl` file is compiled.

The compiler generates the following files:

- `Asn1module.hrl` (if any `SET` or `SEQUENCE` is defined)
- `Asn1module.erl` \- Erlang module with encode, decode, and value functions
- `Asn1module.asn1db` \- Intermediate format used by the compiler when modules
  `IMPORT` definitions from each other.
""".
-spec compile(Asn1module, Options) -> ok | {error, Reason} when
      Asn1module :: atom() | string(),
      Options :: [Option | OldOption],
      Option ::
        ber | per | uper | jer | der |
        compact_bit_string | legacy_bit_string |
        legacy_erlang_types | noobj |
        {n2n, EnumTypeName :: term()} |
        {outdir, Dir :: term()} |
        {i, IncludeDir :: term()} |
        asn1config | undec_rest | no_ok_wrapper |
        {macro_name_prefix, Prefix} |
        {record_name_prefix, Prefix} |
        verbose | warnings_as_errors | deterministic,
      OldOption :: ber | per,
      Reason :: term(),
      Prefix :: string().
compile(File, Options0) when is_list(Options0) ->
    try translate_options(Options0) of
	Options1 ->
	    Options2 = includes(File,Options1),
	    Includes = strip_includes(Options2),
	    in_process(fun() -> compile_proc(File, Includes, Options2) end)
    catch throw:Error ->
	    Error
    end.

-record(st,
	{file=[],
	 files=[],
	 inputmodules=[],
	 code,
	 opts=[],
	 outfile,
	 dbfile,
	 includes=[],
	 erule,
	 error=none,
	 run
	}).

compile_proc(File, Includes, Options) ->
    Erule = get_rule(Options),
    St = #st{opts=Options,includes=Includes,erule=Erule},
    case input_file_type(File, Includes) of
        {single_file, SuffixedFile} -> %% "e.g. "/tmp/File.asn"
            compile1(SuffixedFile, St);
        {multiple_files_file, SetBase, FileName} ->
            case get_file_list(FileName, Includes) of
                FileList when is_list(FileList) ->
                    compile_set(SetBase, FileList, St);
                Err ->
                    Err
            end;
        Err = {input_file_error, _Reason} ->
            {error, Err}
    end.

set_passes() ->
    [{pass,scan_parse,fun set_scan_parse_pass/1},
     {pass,merge,fun merge_pass/1}|common_passes()].

single_passes() ->
    [{pass,scan,fun scan_pass/1},
     {pass,parse,fun parse_pass/1}|common_passes()].

parse_and_save_passes() ->
    [{pass,scan,fun scan_pass/1},
     {pass,parse,fun parse_pass/1},
     {pass,save,fun save_pass/1}].

common_passes() ->
    [{iff,parse,{pass,parse_listing,fun parse_listing/1}},
     {pass,check,fun check_pass/1},
     {iff,abs,{pass,abs_listing,fun abs_listing/1}},
     {pass,generate,fun generate_pass/1},
     {unless,noobj,{pass,compile,fun compile_pass/1}}].

scan_pass(#st{file=File}=St) ->
    case asn1ct_tok:file(File) of
	{error,Reason} ->
	    {error,St#st{error=Reason}};
	Tokens when is_list(Tokens) ->
	    {ok,St#st{code=Tokens}}
    end.

set_scan_parse_pass(#st{files=Files}=St) ->
    try
	L = set_scan_parse_pass_1(Files, St),
	{ok,St#st{code=L}}
    catch
	throw:Error ->
	    {error,St#st{error=Error}}
    end.

set_scan_parse_pass_1([F|Fs], #st{file=File}=St) ->
    case asn1ct_tok:file(F) of
	{error,Error} ->
	    throw(Error);
	Tokens when is_list(Tokens) ->
	    case asn1ct_parser2:parse(File, Tokens) of
		{ok,M} ->
		    [M|set_scan_parse_pass_1(Fs, St)];
		{error,Errors} ->
		    throw(Errors)
	    end
    end;
set_scan_parse_pass_1([], _) -> [].

parse_pass(#st{file=File,code=Tokens}=St) ->
    case asn1ct_parser2:parse(File, Tokens) of
	{ok,M} ->
	    {ok,St#st{code=M}};
	{error,Errors} ->
	    {error,St#st{error=Errors}}
    end.

merge_pass(#st{file=Base,code=Code}=St) ->
    M = merge_modules(Code, Base),
    {ok,St#st{code=M}}.

check_pass(#st{code=M,file=File,includes=Includes,
	       erule=Erule,dbfile=DbFile,opts=Opts,
	       inputmodules=InputModules}=St) ->
    start(Includes),
    case asn1ct_check:storeindb(#state{erule=Erule,options=Opts}, M) of
	ok ->
	    Module = asn1_db:dbget(M#module.name, 'MODULE'),
	    State = #state{mname=Module#module.name,
			   module=Module#module{typeorval=[]},
			   erule=Erule,
			   inputmodules=InputModules,
			   options=Opts,
			   sourcedir=filename:dirname(File)},
	    case asn1ct_check:check(State, Module#module.typeorval) of
		{error,Reason} ->
		    {error,St#st{error=Reason}};
		{ok,NewTypeOrVal,GenTypeOrVal} ->
		    NewM = Module#module{typeorval=NewTypeOrVal},
		    asn1_db:dbput(NewM#module.name, 'MODULE', NewM),
		    asn1_db:dbsave(DbFile, M#module.name),
		    verbose("--~p--~n", [{generated,DbFile}], Opts),
		    {ok,St#st{code={M,GenTypeOrVal}}}
	    end;
	{error,Reason} ->
	    {error,St#st{error=Reason}}
    end.

save_pass(#st{code=M,erule=Erule,opts=Opts}=St) ->
    ok = asn1ct_check:storeindb(#state{erule=Erule,options=Opts}, M),
    {ok,St}.

parse_listing(#st{code=Code,outfile=OutFile0}=St) ->
    OutFile = OutFile0 ++ ".parse",
    case file:write_file(OutFile, io_lib:format("~p\n", [Code])) of
	ok ->
	    done;
	{error,Reason} ->
	    Error = {write_error,OutFile,Reason},
	    {error,St#st{error=[{structured_error,{OutFile0,none},?MODULE,Error}]}}
    end.

abs_listing(#st{code={M,_},outfile=OutFile}) ->
    pretty2(M#module.name, OutFile++".abs"),
    done.

generate_pass(#st{code=Code,outfile=OutFile,erule=Erule,opts=Opts}=St0) ->
    St = St0#st{code=undefined},		%Reclaim heap space
    case generate(Code, OutFile, Erule, Opts) of
        ok ->
            {ok,St};
        {error,Errors} ->
            {error,St#st{error=Errors}}
    end.

compile_pass(#st{outfile=OutFile,opts=Opts0}=St) ->
    asn1_db:dbstop(),				%Reclaim memory.
    asn1ct_table:delete([renamed_defs,original_imports,automatic_tags]),
    Opts = remove_asn_flags(Opts0),
    case c:c(OutFile, Opts) of
	{ok,_Module} ->
	    {ok,St};
	_ ->
	    {error,St}
    end.

run_passes(Passes, #st{opts=Opts}=St) ->
    Run = case lists:member(time, Opts) of
	      false ->
		  fun(_, Pass, S) -> Pass(S) end;
	      true ->
		  fun run_tc/3
	  end,
    run_passes_1(Passes, St#st{run=Run}).

run_tc(Name, Fun, St) ->
    Before0 = statistics(runtime),
    Val = (catch Fun(St)),
    After0 = statistics(runtime),
    {Before_c, _} = Before0,
    {After_c, _} = After0,
    io:format("~-31s: ~10.2f s\n",
	      [Name,(After_c-Before_c) / 1000]),
    Val.

run_passes_1([{unless,Opt,Pass}|Passes], #st{opts=Opts}=St) ->
    case proplists:get_bool(Opt, Opts) of
	false ->
	    run_passes_1([Pass|Passes], St);
	true ->
	    run_passes_1(Passes, St)
    end;
run_passes_1([{iff,Opt,Pass}|Passes], #st{opts=Opts}=St) ->
    case proplists:get_bool(Opt, Opts) of
	true ->
	    run_passes_1([Pass|Passes], St);
	false ->
	    run_passes_1(Passes, St)
    end;
run_passes_1([{pass,Name,Pass}|Passes], #st{run=Run}=St0)
  when is_function(Pass, 1) ->
    try Run(Name, Pass, St0) of
	{ok,St} ->
	    run_passes_1(Passes, St);
	{error,#st{error=Errors}} ->
	    {Structured,AllErrors} = clean_errors(Errors),
	    print_structured_errors(Structured),
	    {error,AllErrors};
	done ->
	    ok
    catch
	Class:Error:Stk ->
	    io:format("Internal error: ~p:~p\n~p\n",
		      [Class,Error,Stk]),
	    {error,{internal_error,{Class,Error}}}
    end;
run_passes_1([], _St) ->
    ok.

clean_errors(Errors) when is_list(Errors) ->
    F = fun({structured_error,_,_,_}) -> true;
	   (_) -> false
	end,
    {Structured0,AdHoc} = lists:partition(F, Errors),
    Structured = lists:sort(Structured0),
    {Structured,Structured ++ AdHoc};
clean_errors(AdHoc) -> {[],AdHoc}.

print_structured_errors(Errors) when is_list(Errors) ->
    _ = [print_structured_error(F, M, E) ||
            {structured_error,F,M,E} <- Errors],
    ok.

print_structured_error(F, M, Error) ->
    Formatted = M:format_error(Error),
    case F of
        none ->
            io:format("~ts\n", [Formatted]);
        {File,none} ->
            io:format("~ts: ~ts\n", [File,Formatted]);
        {File,Line} when is_integer(Line) ->
            io:format("~ts:~p: ~ts\n", [File,Line,Formatted])
    end.

compile1(File, #st{opts=Opts}=St0) ->
    compiler_verbose(File, Opts),
    Passes = single_passes(),
    Base = filename:rootname(filename:basename(File)),
    OutFile = outfile(Base, "", Opts),
    DbFile = outfile(Base, "asn1db", Opts),
    St1 = St0#st{file=File,outfile=OutFile,dbfile=DbFile},
    run_passes(Passes, St1).
			  
%%****************************************************************************%%
%% functions dealing with compiling of several input files to one output file %%
%%****************************************************************************%%

%% compile_set/3 merges and compiles a number of asn1 modules
%% specified in a .set.asn file to one .erl file.
compile_set(SetBase, Files, #st{opts=Opts}=St0) ->
    compiler_verbose(Files, Opts),
    OutFile = outfile(SetBase, "", Opts),
    DbFile = outfile(SetBase, "asn1db", Opts),
    InputModules = [begin
			F1 = filename:basename(F0),
			F = filename:rootname(F1),
			list_to_atom(F)
		    end || F0 <- Files],
    St = St0#st{file=SetBase,files=Files,outfile=OutFile,
		dbfile=DbFile,inputmodules=InputModules},
    Passes = set_passes(),
    run_passes(Passes, St).

compiler_verbose(What, Opts) ->
    verbose("Erlang ASN.1 compiler ~s\n", [?vsn], Opts),
    verbose("Compiling: ~p\n", [What], Opts),
    verbose("Options: ~p\n", [Opts], Opts).

%% merge_modules/2 -> returns a module record where the typeorval lists are merged,
%% the exports lists are merged, the imports lists are merged when the 
%% elements come from other modules than the merge set, the tagdefault 
%% field gets the shared value if all modules have same tagging scheme,
%% otherwise a tagging_error exception is thrown, 
%% the extensiondefault ...(not handled yet).
merge_modules(ModuleList, CommonName) ->
    NewModuleList = remove_name_collisions(ModuleList),
    case asn1ct_table:size(renamed_defs) of
        0 -> asn1ct_table:delete(renamed_defs);
        _ -> ok
    end,
    save_imports(NewModuleList),
    TypeOrVal = lists:append(lists:map(fun(X)->X#module.typeorval end,
				       NewModuleList)),
    InputMNameList = lists:map(fun(X)->X#module.name end,
			       NewModuleList),
    CExports = common_exports(NewModuleList),
   
    ImportsModuleNameList = lists:map(fun(X)->
					      {X#module.imports,
					       X#module.name} end,
				      NewModuleList),
    %% ImportsModuleNameList: [{Imports,ModuleName},...]
    %% Imports is a tuple {imports,[#'SymbolsFromModule'{},...]}
    CImports = common_imports(ImportsModuleNameList,InputMNameList),
    TagDefault = check_tagdefault(NewModuleList),
    #module{name=CommonName,tagdefault=TagDefault,exports=CExports,
	    imports=CImports,typeorval=TypeOrVal}.

%% causes an exit if duplicate definition names exist in a module
remove_name_collisions(Modules) ->
    asn1ct_table:new(renamed_defs),
    %% Name duplicates in the same module is not allowed.
    lists:foreach(fun exit_if_nameduplicate/1,Modules),
    %% Then remove duplicates in different modules and return the
    %% new list of modules.
    remove_name_collisions2(Modules,[]).

%% For each definition in the first module in module list, find
%% all definitions with same name and rename both definitions in
%% the first module and in rest of modules
remove_name_collisions2([M|Ms],Acc) ->
    TypeOrVal = M#module.typeorval,
    MName = M#module.name,
    %% Test each name in TypeOrVal on all modules in Ms
    {NewM,NewMs} = remove_name_collisions2(MName,TypeOrVal,Ms,[]),
    remove_name_collisions2(NewMs,[M#module{typeorval=NewM}|Acc]);
remove_name_collisions2([],Acc) ->
    finished_warn_prints(),
    Acc.

%% For each definition in list of defs find definitions in (rest of)
%% modules that have same name. If duplicate was found rename def.
%% Test each name in [T|Ts] on all modules in Ms
remove_name_collisions2(ModName,[T|Ts],Ms,Acc) ->
    Name = get_name_of_def(T),
    case discover_dupl_in_mods(Name,T,Ms,[],?unique_names) of
	{_,?unique_names} -> % there was no name collision
	    remove_name_collisions2(ModName,Ts,Ms,[T|Acc]);
	{NewMs,?dupl_uniquedefs} -> % renamed defs in NewMs
	    %% rename T
	    NewT = set_name_of_def(ModName,Name,T), %rename def
	    warn_renamed_def(ModName,get_name_of_def(NewT),Name),
	    asn1ct_table:insert(renamed_defs,
	                        {get_name_of_def(NewT), Name, ModName}),
	    remove_name_collisions2(ModName,Ts,NewMs,[NewT|Acc]);
	{NewMs,?dupl_equaldefs} -> % name duplicates, but identical defs
	    %% keep name of T
	    warn_kept_def(ModName,Name),
	    remove_name_collisions2(ModName,Ts,NewMs,[T|Acc]);
	{NewMs,?dupl_eqdefs_uniquedefs} ->
	    %% keep name of T, renamed defs in NewMs
	    warn_kept_def(ModName,Name),
	    remove_name_collisions2(ModName,Ts,NewMs,[T|Acc])
    end;
remove_name_collisions2(_,[],Ms,Acc) ->
    {Acc,Ms}.

%% Name is the name of a definition. If a definition with the same name
%% is found in the modules Ms the definition will be renamed and returned.
discover_dupl_in_mods(Name,Def,[M=#module{name=N,typeorval=TorV}|Ms],
			      Acc,AnyRenamed) ->
    Fun = fun(T,RenamedOrDupl)->
		  case {get_name_of_def(T),compare_defs(Def,T)} of
		      {Name,not_equal} ->
			  %% rename def
			  NewT=set_name_of_def(N,Name,T),
			  warn_renamed_def(N,get_name_of_def(NewT),Name),
			  asn1ct_table:insert(renamed_defs,
						          {get_name_of_def(NewT), Name, N}),
			  {NewT,?dupl_uniquedefs bor RenamedOrDupl};
		      {Name,equal} ->
			  %% delete def
			  warn_deleted_def(N,Name),
			  {[],?dupl_equaldefs bor RenamedOrDupl};
		      _ ->
			  {T,RenamedOrDupl}
		  end
	  end,
    {NewTorV,NewAnyRenamed} = lists:mapfoldl(Fun,AnyRenamed,TorV),
    %% have to flatten the NewTorV to remove any empty list elements
    discover_dupl_in_mods(Name,Def,Ms,
			  [M#module{typeorval=lists:flatten(NewTorV)}|Acc],
			  NewAnyRenamed);
discover_dupl_in_mods(_,_,[],Acc,AnyRenamed) ->
    {Acc,AnyRenamed}.

warn_renamed_def(ModName,NewName,OldName) ->
    maybe_first_warn_print(),
    io:format("NOTICE: The ASN.1 definition in module ~p with name ~p has been renamed in generated module. New name is ~p.~n",[ModName,OldName,NewName]).

warn_deleted_def(ModName,DefName) ->
    maybe_first_warn_print(),
    io:format("NOTICE: The ASN.1 definition in module ~p with name ~p has been deleted in generated module.~n",[ModName,DefName]).

warn_kept_def(ModName,DefName) ->
    maybe_first_warn_print(),
    io:format("NOTICE: The ASN.1 definition in module ~p with name ~p has kept its name due to equal definition as duplicate.~n",[ModName,DefName]).

maybe_first_warn_print() ->
    case get(warn_duplicate_defs) of
	undefined ->
	    put(warn_duplicate_defs,true),
	    io:format("~nDue to multiple occurrences of a definition name in "
		      "multi-file compiled files:~n");
	_ ->
	    ok
    end.
finished_warn_prints() ->
    put(warn_duplicate_defs,undefined).


exit_if_nameduplicate(#module{typeorval=TorV}) ->
    exit_if_nameduplicate(TorV);
exit_if_nameduplicate([]) ->
    ok;
exit_if_nameduplicate([Def|Rest]) ->
    Name=get_name_of_def(Def),
    exit_if_nameduplicate2(Name,Rest),
    exit_if_nameduplicate(Rest).

exit_if_nameduplicate2(Name,Rest) ->
    Pred=fun(Def)->
		 case get_name_of_def(Def) of
		     Name -> true;
		     _ -> false
		 end
	 end,
        case lists:any(Pred,Rest) of
	true ->
	    throw({error,{"more than one definition with same name",Name}});
	_ ->
	    ok
    end.

compare_defs(D1,D2) ->
    compare_defs2(unset_pos_mod(D1),unset_pos_mod(D2)).
compare_defs2(D,D) ->
    equal;
compare_defs2(_,_) ->
    not_equal.

-doc false.
unset_pos_mod(Def) when is_record(Def,typedef) ->
    Def#typedef{pos=undefined};
unset_pos_mod(Def) when is_record(Def,classdef) ->
    Def#classdef{pos=undefined};
unset_pos_mod(Def) when is_record(Def,valuedef) ->
    Def#valuedef{pos=undefined,module=undefined};
unset_pos_mod(Def) when is_record(Def,ptypedef) ->
    Def#ptypedef{pos=undefined};
unset_pos_mod(Def) when is_record(Def,pvaluedef) ->
    Def#pvaluedef{pos=undefined};
unset_pos_mod(Def) when is_record(Def,pvaluesetdef) ->
    Def#pvaluesetdef{pos=undefined};
unset_pos_mod(Def) when is_record(Def,pobjectdef) ->
    Def#pobjectdef{pos=undefined};
unset_pos_mod(Def) when is_record(Def,pobjectsetdef) ->
    Def#pobjectsetdef{pos=undefined};
unset_pos_mod(#'ComponentType'{} = Def) ->
    Def#'ComponentType'{pos=undefined};
unset_pos_mod(Def) -> Def.

-doc false.
get_pos_of_def(#typedef{pos=Pos}) ->
    Pos;
get_pos_of_def(#classdef{pos=Pos}) ->
    Pos;
get_pos_of_def(#valuedef{pos=Pos}) ->
    Pos;
get_pos_of_def(#ptypedef{pos=Pos}) ->
    Pos;
get_pos_of_def(#pvaluedef{pos=Pos}) ->
    Pos;
get_pos_of_def(#pvaluesetdef{pos=Pos}) ->
    Pos;
get_pos_of_def(#pobjectdef{pos=Pos}) ->
    Pos;
get_pos_of_def(#pobjectsetdef{pos=Pos}) ->
    Pos;
get_pos_of_def(#'Externaltypereference'{pos=Pos}) ->
    Pos;
get_pos_of_def(#'Externalvaluereference'{pos=Pos}) ->
    Pos;
get_pos_of_def(_) -> 
    undefined.
    
    
-doc false.
get_name_of_def(#typedef{name=Name}) ->
    Name;
get_name_of_def(#classdef{name=Name}) ->
    Name;
get_name_of_def(#valuedef{name=Name}) ->
    Name;
get_name_of_def(#ptypedef{name=Name}) ->
    Name;
get_name_of_def(#pvaluedef{name=Name}) ->
    Name;
get_name_of_def(#pvaluesetdef{name=Name}) ->
    Name;
get_name_of_def(#pobjectdef{name=Name}) ->
    Name;
get_name_of_def(#pobjectsetdef{name=Name}) ->
    Name;
get_name_of_def(_) ->
    undefined.

set_name_of_def(ModName,Name,OldDef) ->
    NewName = list_to_atom(lists:concat([Name,ModName])),
    case OldDef of
	#typedef{} -> OldDef#typedef{name=NewName};
	#classdef{} -> OldDef#classdef{name=NewName};
	#valuedef{} -> OldDef#valuedef{name=NewName};
	#ptypedef{} -> OldDef#ptypedef{name=NewName};
	#pvaluedef{} -> OldDef#pvaluedef{name=NewName};
	#pvaluesetdef{} -> OldDef#pvaluesetdef{name=NewName};
	#pobjectdef{} -> OldDef#pobjectdef{name=NewName};
	#pobjectsetdef{} -> OldDef#pobjectsetdef{name=NewName}
    end.

save_imports(ModuleList)->
    Fun = fun(M) ->
		  case M#module.imports of
		      {_,[]} -> [];
		      {_,I} -> 
			  {M#module.name,I}
		  end
	  end,
    ImportsList = lists:map(Fun,ModuleList),
    case lists:flatten(ImportsList) of
	[] ->
	    ok;
	ImportsList2 ->
	    asn1ct_table:new(original_imports),
	    lists:foreach(fun(X) -> asn1ct_table:insert(original_imports, X) end,
                      ImportsList2)
    end.
				    
	    
common_exports(ModuleList) ->
    %% if all modules exports 'all' then export 'all', 
    %% otherwise export each typeorval name
    case lists:filter(fun(X)->
			      element(2,X#module.exports) /= all
		      end,
		      ModuleList) of
	[]->
	    {exports,all};
	ModsWithExpList ->
	    CExports1 = 
		lists:append(lists:map(fun(X)->element(2,X#module.exports) end,
				       ModsWithExpList)),
	    CExports2 = export_all(lists:subtract(ModuleList,ModsWithExpList)),
	    {exports,CExports1++CExports2}
    end.

export_all([])->[];
export_all(ModuleList) ->
    ExpList =
	lists:map(
	  fun(M)->
		  TorVL=M#module.typeorval,
		  MName = M#module.name,
		  lists:map(
		    fun(Def)->
			    case Def of
				T when is_record(T,typedef)->
				    #'Externaltypereference'{pos=0,
							     module=MName,
							     type=T#typedef.name};
				V when is_record(V,valuedef) ->
				    #'Externalvaluereference'{pos=0,
							      module=MName,
							      value=V#valuedef.name};
				C when is_record(C,classdef) ->
				    #'Externaltypereference'{pos=0,
							     module=MName,
							     type=C#classdef.name};
				P when is_record(P,ptypedef) ->
				    #'Externaltypereference'{pos=0,
							     module=MName,
							     type=P#ptypedef.name};
				PV when is_record(PV,pvaluesetdef) ->
				    #'Externaltypereference'{pos=0,
							     module=MName,
							     type=PV#pvaluesetdef.name};
				PO when is_record(PO,pobjectdef) ->
				    #'Externalvaluereference'{pos=0,
							      module=MName,
							      value=PO#pobjectdef.name}
			    end
		    end,
		    TorVL)
	  end,
	  ModuleList),
    lists:append(ExpList).

%% common_imports/2
%% IList is a list of tuples, {Imports,MName}, where Imports is the imports of
%% the module with name MName.
%% InputMNameL holds the names of all merged modules.
%% Returns an import tuple with a list of imports that are external the merged
%% set of modules.
common_imports(IList,InputMNameL) ->
    SetExternalImportsList = remove_in_set_imports(IList,InputMNameL,[]),
    {imports,remove_import_doubles(SetExternalImportsList)}.

check_tagdefault(ModList) ->
    case have_same_tagdefault(ModList) of
	{true,TagDefault}  -> TagDefault;
	{false,TagDefault} ->
        asn1ct_table:new(automatic_tags),
	    save_automatic_tagged_types(ModList),
	    TagDefault
    end.

have_same_tagdefault([#module{tagdefault=T}|Ms]) ->
    have_same_tagdefault(Ms,{true,T}).

have_same_tagdefault([],TagDefault) ->
    TagDefault;
have_same_tagdefault([#module{tagdefault=T}|Ms],TDefault={_,T}) ->
    have_same_tagdefault(Ms,TDefault);
have_same_tagdefault([#module{tagdefault=T1}|Ms],{_,T2}) ->
    have_same_tagdefault(Ms,{false,rank_tagdef([T1,T2])}).

rank_tagdef(L) ->
    case lists:member('EXPLICIT',L) of
	true -> 'EXPLICIT';
	_ -> 'IMPLICIT'
    end.

save_automatic_tagged_types([])->
    done;
save_automatic_tagged_types([#module{tagdefault='AUTOMATIC',
				     typeorval=TorV}|Ms]) ->
    Fun =
	fun(T) ->
		asn1ct_table:insert(automatic_tags, {get_name_of_def(T)})
	end,
    lists:foreach(Fun,TorV),
    save_automatic_tagged_types(Ms);
save_automatic_tagged_types([_M|Ms]) ->
    save_automatic_tagged_types(Ms).

%% remove_in_set_imports/3 :
%% input: list with tuples of each module's imports and module name 
%% respectively.
%% output: one list with same format but each occurred import from a
%% module in the input set (IMNameL) is removed.
remove_in_set_imports([{{imports,ImpL},_ModName}|Rest],InputMNameL,Acc) ->
    NewImpL = remove_in_set_imports1(ImpL,InputMNameL,[]),
    remove_in_set_imports(Rest,InputMNameL,NewImpL++Acc);
remove_in_set_imports([],_,Acc) ->
    lists:reverse(Acc).

remove_in_set_imports1([I|Is],InputMNameL,Acc) ->
    case I#'SymbolsFromModule'.module of
	#'Externaltypereference'{type=MName} ->
	    case lists:member(MName,InputMNameL) of
		true ->
		    remove_in_set_imports1(Is,InputMNameL,Acc);
		false ->
		    remove_in_set_imports1(Is,InputMNameL,[I|Acc])
	    end;
	_ ->
	    remove_in_set_imports1(Is,InputMNameL,[I|Acc])
    end;
remove_in_set_imports1([],_,Acc) ->
    lists:reverse(Acc).

remove_import_doubles([]) ->
    [];
%% If several modules in the merge set imports symbols from
%% the same external module it might be doubled.
%% ImportList has #'SymbolsFromModule' elements
remove_import_doubles(ImportList) ->
    MergedImportList = 
	merge_symbols_from_module(ImportList,[]),
    delete_double_of_symbol(MergedImportList,[]).

merge_symbols_from_module([Imp|Imps],Acc) ->
    #'Externaltypereference'{type=ModName} = Imp#'SymbolsFromModule'.module,
    IfromModName = 
	lists:filter(
	  fun(I)->
		  case I#'SymbolsFromModule'.module of
		      #'Externaltypereference'{type=ModName} ->
			  true;
		      #'Externalvaluereference'{value=ModName} ->
			  true;
		      _ -> false
		  end
	  end,
	  Imps),
    NewImps = lists:subtract(Imps,IfromModName),
    NewImp =
	Imp#'SymbolsFromModule'{
	  symbols = lists:append(
		      lists:map(fun(SL)->
					SL#'SymbolsFromModule'.symbols 
				end,[Imp|IfromModName]))},
    merge_symbols_from_module(NewImps,[NewImp|Acc]);
merge_symbols_from_module([],Acc) ->
    lists:reverse(Acc).

delete_double_of_symbol([I|Is],Acc) ->
    SymL=I#'SymbolsFromModule'.symbols,
    NewSymL = delete_double_of_symbol1(SymL,[]),
    delete_double_of_symbol(Is,[I#'SymbolsFromModule'{symbols=NewSymL}|Acc]);
delete_double_of_symbol([],Acc) ->
    Acc.

delete_double_of_symbol1([TRef=#'Externaltypereference'{type=TrefName}|Rest],Acc)->
    NewRest = 
	lists:filter(fun(S)->
			     case S of
				 #'Externaltypereference'{type=TrefName}->
				     false;
				 _ -> true
			     end
		     end,
		     Rest),
    delete_double_of_symbol1(NewRest,[TRef|Acc]);
delete_double_of_symbol1([VRef=#'Externalvaluereference'{value=VName}|Rest],Acc) ->
    NewRest = 
	lists:filter(fun(S)->
			     case S of
				 #'Externalvaluereference'{value=VName}->
				     false;
				 _ -> true
			     end
		     end,
		     Rest),
    delete_double_of_symbol1(NewRest,[VRef|Acc]);
delete_double_of_symbol1([TRef={#'Externaltypereference'{type=MRef},
				#'Externaltypereference'{type=TRef}}|Rest],
			 Acc)->
    NewRest = 
	lists:filter(
	  fun(S)->
		  case S of
		      {#'Externaltypereference'{type=MRef},
		       #'Externaltypereference'{type=TRef}}->
			  false;
		      _ -> true
		  end
	  end,
	  Rest),
    delete_double_of_symbol1(NewRest,[TRef|Acc]);
delete_double_of_symbol1([],Acc) ->
    Acc.


%%***********************************

generate({M,CodeTuple}, OutFile, EncodingRule, Options) ->
    {Types,Values,Ptypes,Classes,Objects,ObjectSets} = CodeTuple,
    Code = #abst{name=M#module.name,
                 types=Types,values=Values,ptypes=Ptypes,
                 classes=Classes,objects=Objects,objsets=ObjectSets},
    setup_bit_string_format(Options),
    setup_legacy_erlang_types(Options),
    asn1ct_table:new(check_functions),

    Gen = init_gen_record(EncodingRule, Options),

    check_maps_option(Gen),

    %% create decoding function names and taglists for partial decode
    case specialized_decode_prepare(Gen, M) of
        {error,_}=Error ->
            Error;
        ok ->
            asn1ct_gen:pgen(OutFile, Gen, Code),
            cleanup_bit_string_format(),
            erase(tlv_format),                  % used in ber
            erase(class_default_type),          % used in ber
            asn1ct_table:delete(check_functions),
            ok
    end.

init_gen_record(EncodingRule, Options) ->
    Erule = case EncodingRule of
                uper -> per;
                _ -> EncodingRule
            end,
    Der = proplists:get_bool(der, Options),
    Jer = proplists:get_bool(jer, Options) andalso (EncodingRule =/= jer),
    Aligned = EncodingRule =:= per,
    RecPrefix = proplists:get_value(record_name_prefix, Options, ""),
    MacroPrefix = proplists:get_value(macro_name_prefix, Options, ""),
    Pack = case proplists:get_value(maps, Options, false) of
               true -> map;
               false -> record
           end,
    #gen{erule=Erule,der=Der,jer=Jer,aligned=Aligned,
         rec_prefix=RecPrefix,macro_prefix=MacroPrefix,
         pack=Pack,options=Options}.


setup_legacy_erlang_types(Opts) ->
    F = case lists:member(legacy_erlang_types, Opts) of
	    false ->
		case get_bit_string_format() of
		    bitstring ->
			false;
		    compact ->
			legacy_forced_info(compact_bit_string),
			true;
		    legacy ->
			legacy_forced_info(legacy_bit_string),
			true
		end;
	    true ->
		true
	end,
    put(use_legacy_erlang_types, F).

legacy_forced_info(Opt) ->
    io:format("Info: The option 'legacy_erlang_types' "
	      "is implied by the '~s' option.\n", [Opt]).

-doc false.
use_legacy_types() ->
    get(use_legacy_erlang_types).

setup_bit_string_format(Opts) ->
    Format = case {lists:member(compact_bit_string, Opts),
		   lists:member(legacy_bit_string, Opts)} of
		 {false,false} -> bitstring;
		 {true,false} -> compact;
		 {false,true} -> legacy;
		 {true,true} ->
		     Message = "Contradicting options given: "
			 "compact_bit_string and legacy_bit_string",
		     exit({error,{asn1,Message}})
	     end,
    put(bit_string_format, Format).

cleanup_bit_string_format() ->
    erase(bit_string_format).

-doc false.
get_bit_string_format() ->
    get(bit_string_format).

check_maps_option(#gen{pack=map}) ->
    case get_bit_string_format() of
        bitstring ->
            ok;
        _ ->
            Message1 = "The 'maps' option must not be combined with "
                "'compact_bit_string' or 'legacy_bit_string'",
            exit({error,{asn1,Message1}})
    end,
    case use_legacy_types() of
        false ->
            ok;
        true ->
            Message2 = "The 'maps' option must not be combined with "
                "'legacy_erlang_types'",
            exit({error,{asn1,Message2}})
    end;
check_maps_option(#gen{}) ->
    ok.


%% parse_and_save parses an asn1 spec and saves the unchecked parse
%% tree in a data base file.
%% Does not support multifile compilation files
-doc false.
parse_and_save(Module,S) ->
    Options = S#state.options,
    SourceDir = S#state.sourcedir,
    Includes = [I || {i,I} <- Options],
    Erule = S#state.erule,
    Maps = lists:member(maps, Options),
    case get_input_file(Module, [SourceDir|Includes]) of
	%% search for asn1 source
	{file,SuffixedASN1source} ->
	    Mtime = filelib:last_modified(SuffixedASN1source),
	    case asn1_db:dbload(Module, Erule, Maps, Mtime) of
		ok -> ok;
		error -> parse_and_save1(S, SuffixedASN1source, Options)
	    end;
	Err when not Maps ->
	    case asn1_db:dbload(Module) of
		ok ->
                    %% FIXME: This should be an error.
		    warning("could not do a consistency check of the ~p file: no asn1 source file was found.~n",
			    [lists:concat([Module,".asn1db"])],Options);
		error ->
		    ok
	    end,
	    {error,{asn1,input_file_error,Err}};
        Err ->
            %% Always fail directly when the 'maps' option is used.
	    {error,{asn1,input_file_error,Err}}
    end.

parse_and_save1(#state{erule=Erule}, File, Options) ->
    Ext = filename:extension(File),
    Base = filename:basename(File, Ext),
    DbFile = outfile(Base, "asn1db", Options),
    St = #st{file=File,dbfile=DbFile,erule=Erule},
    Passes = parse_and_save_passes(),
    run_passes(Passes, St).

get_input_file(Module,[]) ->
    Module;
get_input_file(Module,[I|Includes]) ->
    case (catch input_file_type(filename:join([I,Module]))) of
	{single_file,FileName} ->
		    {file,FileName};
	_ ->
	    get_input_file(Module,Includes)
    end.

input_file_type(Name,I) ->
   case input_file_type(Name) of
       {error,_} -> input_file_type2(filename:basename(Name),I);
       Err={input_file_error,_} -> Err;
       Res -> Res
   end.
input_file_type2(Name,[I|Is]) ->
    case input_file_type(filename:join([I,Name])) of
	{error,_} -> input_file_type2(Name,Is);
	Err={input_file_error,_} -> Err;
	Res -> Res
    end;
input_file_type2(Name,[]) ->
    input_file_type(Name).
	   
input_file_type([]) ->
    {empty_name,[]};
input_file_type(File) ->
    case filename:extension(File) of
	[] ->
	    case file:read_file_info(lists:concat([File,".asn1"])) of
		{ok,_FileInfo} ->
		    {single_file, lists:concat([File,".asn1"])};
		_ ->
		    case file:read_file_info(lists:concat([File,".asn"])) of
			{ok,_FileInfo} ->
			    {single_file, lists:concat([File,".asn"])};
			_ ->
			    case file:read_file_info(lists:concat([File,".py"])) of
				{ok,_FileInfo} ->
				    {single_file, lists:concat([File,".py"])};
				Error ->
				    Error
			    end
		    end
	    end;
	".asn1config" ->
	    case read_config_file_info(File, asn1_module) of
		{ok,Asn1Module} -> 
		    input_file_type(Asn1Module);
		Error ->
		    Error
	    end;
	Asn1SFix ->
	    Base = filename:basename(File,Asn1SFix),
	    Ret =
		case filename:extension(Base) of
		    [] ->
			{single_file,File};
		    SetSFix when (SetSFix == ".set") ->
			{multiple_files_file,
			 list_to_atom(filename:basename(Base,SetSFix)),
			 File};
		    _Error ->
			throw({input_file_error,{'Bad input file',File}})
		end,
	    %% check that the file exists
	    case file:read_file_info(File) of
		{ok,_} -> Ret;
		Err -> Err
	    end
    end.

get_file_list(File,Includes) ->
    case file:open(File,[read]) of
	{error,Reason} ->
	    {error,{File,file:format_error(Reason)}};
	{ok,Stream} ->
	    get_file_list1(Stream,filename:dirname(File),Includes,[])
    end.

get_file_list1(Stream,Dir,Includes,Acc) ->
    Ret = io:get_line(Stream,''),
    case Ret of
	eof ->
	    ok = file:close(Stream),
	    lists:reverse(Acc);
	FileName ->
	    SuffixedNameList =
		case (catch input_file_type(filename:join([Dir,lists:delete($\n,FileName)]),Includes)) of
		    {empty_name,[]} -> [];
		    {single_file,Name} -> [Name];
		    {multiple_files_file,_,Name} ->
			get_file_list(Name,Includes);
		    _Err ->
			[]
		end,
	    get_file_list1(Stream,Dir,Includes,SuffixedNameList++Acc)
    end.

get_rule(Options) ->
    case [Rule || Rule <- [ber,per,uper,jer],
		  Opt <- Options,
		  Rule =:= Opt] of
	[Rule] ->
	    Rule;
	[Rule|_] ->
	    Rule;
	[] ->
	    ber
    end.

%% translate_options(NewOptions) -> OldOptions
%%  Translate the new option names to the old option name.

translate_options([ber_bin|T]) ->
    io:format("Warning: The option 'ber_bin' is now called 'ber'.\n"),
    [ber|translate_options(T)];
translate_options([per_bin|T]) ->
    io:format("Warning: The option 'per_bin' is now called 'per'.\n"),
    [per|translate_options(T)];
translate_options([uper_bin|T]) ->
    io:format("Warning: The option 'uper_bin' is now called 'uper'.\n"),
    translate_options([uper|T]);
translate_options([nif|T]) ->
    io:format("Warning: The option 'nif' is no longer needed.\n"),
    translate_options(T);
translate_options([optimize|T]) ->
    io:format("Warning: The option 'optimize' is no longer needed.\n"),
    translate_options(T);
translate_options([inline|T]) ->
    io:format("Warning: The option 'inline' is no longer needed.\n"),
    translate_options(T);
translate_options([{inline,_}|_]) ->
    io:format("ERROR: The option {inline,OutputFilename} is no longer supported.\n"),
    throw({error,{unsupported_option,inline}});
translate_options([H|T]) ->
    [H|translate_options(T)];
translate_options([]) -> [].

remove_asn_flags(Options) ->
    [X || X <- Options, not is_asn1_flag(X)].

is_asn1_flag(asn1config) -> true;
is_asn1_flag(ber) -> true;
is_asn1_flag(compact_bit_string) -> true;
is_asn1_flag(debug) -> true;
is_asn1_flag(der) -> true;
is_asn1_flag(legacy_bit_string) -> true;
is_asn1_flag({macro_name_prefix,_}) -> true;
is_asn1_flag({n2n,_}) -> true;
is_asn1_flag(noobj) -> true;
is_asn1_flag(no_ok_wrapper) -> true;
is_asn1_flag(optimize) -> true;
is_asn1_flag(per) -> true;
is_asn1_flag({record_name_prefix,_}) -> true;
is_asn1_flag(undec_rec) -> true;
is_asn1_flag(uper) -> true;
is_asn1_flag(verbose) -> true;
%% 'warnings_as_errors' is intentionally passed through to the compiler.
is_asn1_flag(_) -> false.


outfile(Base, Ext, Opts) ->
    Obase = case lists:keysearch(outdir, 1, Opts) of
		{value, {outdir, Odir}} -> filename:join(Odir, Base);
		_NotFound -> Base % Not found or bad format
	    end,
    case Ext of
	[] ->
	    Obase;
	_ ->
	    lists:concat([Obase,".",Ext])
    end.

includes(File,Options) -> 
    Options2 = include_append(".", Options),
    Options3 = include_append(filename:dirname(File), Options2),
    case proplists:get_value(outdir, Options) of
        undefined -> Options3;
        OutDir    -> include_prepend(OutDir, Options3)
    end.

include_append(Dir, Options) ->
    option_add({i, Dir}, Options, fun(Opts) -> Opts ++ [{i, Dir}] end).

include_prepend(Dir, Options) ->
    option_add({i, Dir}, Options, fun(Opts) -> [{i, Dir}|Opts] end).

option_add(Option, Options, Fun) ->
    case lists:member(Option, Options) of
        true  -> Options;
        false -> Fun(Options)
    end.

strip_includes(Includes) ->
    [I || {i, I} <- Includes].

		    
%% compile(AbsFileName, Options)
%%   Compile entry point for erl_compile.

-doc false.
compile_asn(File,OutFile,Options) ->
    compile(lists:concat([File,".asn"]),OutFile,Options).

-doc false.
compile_asn1(File,OutFile,Options) ->
    compile(lists:concat([File,".asn1"]),OutFile,Options).

-doc false.
compile_py(File,OutFile,Options) ->
    compile(lists:concat([File,".py"]),OutFile,Options).

-doc false.
compile(File, _OutFile, Options) ->
    case compile(File, make_erl_options(Options)) of
	{error,_Reason} ->
	    error;
	ok -> 
	    ok
    end.

%% Converts generic compiler options to specific options.

make_erl_options(Opts) ->

    %% This way of extracting will work even if the record passed
    %% has more fields than known during compilation.

    Includes = Opts#options.includes,
    Defines = Opts#options.defines,
    Outdir = Opts#options.outdir,
    Warning = Opts#options.warning,
    Verbose = Opts#options.verbose,
    Specific = Opts#options.specific,
    Optimize = Opts#options.optimize,
    OutputType = Opts#options.output_type,
    Cwd = Opts#options.cwd,

    Options =
	case Verbose of
	    true ->  [verbose];
	    false -> []
	end ++
	case Warning of
	    0 -> [];
	    _ -> [warnings]
	end ++
	[] ++
	case Optimize of
	    1 -> [optimize];
	    999 -> [];
	    _ -> [{optimize,Optimize}]
	end ++
	lists:map(
	  fun ({Name, Value}) ->
		  {d, Name, Value};
	      (Name) ->
		  {d, Name}
	  end,
	  Defines) ++
	case OutputType of
	    undefined -> [ber]; % temporary default (ber when it's ready)
	    _ -> [OutputType]	% pass through
	end,

    Options++[errors, {cwd, Cwd}, {outdir, Outdir}|
	      lists:map(fun(Dir) -> {i, Dir} end, Includes)]++Specific.

pretty2(Module,AbsFile) ->
    {ok,F} = file:open(AbsFile,[write]),
    M = asn1_db:dbget(Module,'MODULE'),
    io:format(F,"%%%%%%%%%%%%%%%%%%%   ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    io:format(F,"~s.\n",[asn1ct_pretty_format:term(M#module.defid)]),
    io:format(F,"~s.\n",[asn1ct_pretty_format:term(M#module.tagdefault)]),
    io:format(F,"~s.\n",[asn1ct_pretty_format:term(M#module.exports)]),
    io:format(F,"~s.\n",[asn1ct_pretty_format:term(M#module.imports)]),
    io:format(F,"~s.\n\n",[asn1ct_pretty_format:term(M#module.extensiondefault)]),

    {Types,Values,ParameterizedTypes,Classes,Objects,ObjectSets} = M#module.typeorval,
    io:format(F,"%%%%%%%%%%%%%%%%%%% TYPES in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s.\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])
		  end,Types),
    io:format(F,"%%%%%%%%%%%%%%%%%%% VALUES in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s.\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])
		  end,Values),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Parameterized Types in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s.\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])			   
		  end,ParameterizedTypes),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Classes in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s.\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])			   
		  end,Classes),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Objects in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s.\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])			   
		  end,Objects),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Object Sets in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s.\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])			   
		  end,ObjectSets).

start(Includes) when is_list(Includes) ->
    asn1_db:dbstart(Includes).

-doc(#{equiv => test/3}).
-spec test(Module) -> ok | {error, Reason} when
      Module :: module(),
      Reason :: term().
test(Module)                             -> test_module(Module, []).

-doc(#{equiv => test/3}).
-spec test(Module, Type | Options) -> ok | {error, Reason} when
      Module :: module(),
      Type :: atom(),
      Options :: [{i, IncludeDir :: term()}],
      Reason :: term().
test(Module, [] = Options)               -> test_module(Module, Options);
test(Module, [{i, _}|_] = Options)       -> test_module(Module, Options);
test(Module, Type)                       -> test_type(Module, Type, []).

-doc """
Performs a test of encode and decode of types in `Module`. The generated
functions are called by this function. This function is useful during test to
secure that the generated encode and decode functions as well as the general
runtime support work as expected.

> #### Note {: .info }
>
> Currently, the `test` functions have many limitations. Essentially, they will
> mostly work for old specifications based on the 1997 standard for ASN.1, but
> not for most modern-style applications. Another limitation is that the `test`
> functions may not work if options that change code generations strategies such
> as the options `macro_name_prefix` and `record_name_prefix` have been used.

- [`test/1`](`test/1`) iterates over all types in `Module`.
- [`test/2`](`test/2`) tests type `Type` with a random value.
- [`test/3`](`test/3`) tests type `Type` with `Value`.

Schematically, the following occurs for each type in the module:

```erlang
{ok, Value} = asn1ct:value(Module, Type),
{ok, Bytes} = Module:encode(Type, Value),
{ok, Value} = Module:decode(Type, Bytes).
```

The `test` functions use the `*.asn1db` files for all included modules. If they
are located in a different directory than the current working directory, use the
`include` option to add paths. This is only needed when automatically generating
values. For static values using `Value` no options are needed.
""".
-spec test(Module, Type, Value | Options) -> ok | {error, Reason} when
      Module :: module(),
      Type :: atom(),
      Value :: term(),
      Options :: [{i, IncludeDir :: term()}],
      Reason :: term().
test(Module, Type, [] = Options)         -> test_type(Module, Type, Options);
test(Module, Type, [{i, _}|_] = Options) -> test_type(Module, Type, Options);
test(Module, Type, Value)                -> test_value(Module, Type, Value).

test_module(Module, Includes) ->
    in_process(fun() ->
                   start(strip_includes(Includes)),
                   case check(Module, Includes) of
                       {ok, NewTypes} -> test_each(Module, NewTypes);
                       Error          -> Error
                   end
               end).

test_each(Module, [Type|Rest]) ->
    case test_type(Module, Type) of
        {ok, _Result} -> test_each(Module, Rest);
        Error         -> Error
    end;
test_each(_,[]) ->
    ok.

test_type(Module, Type, Includes) ->
    in_process(fun() ->
                   start(strip_includes(Includes)),
                   case check(Module, Includes) of
                       {ok, _NewTypes} -> test_type(Module, Type);
                       Error           -> Error
                   end
               end).

test_type(Module, Type) ->
    case get_value(Module, Type) of
        {ok, Val}       -> test_value(Module, Type, Val);
        {error, Reason} -> {error, {asn1, {value, Reason}}}
    end.

test_value(Module, Type, Value) ->
    in_process(fun() ->
                   case catch Module:encode(Type, Value) of
                       {ok, Bytes} ->
                           test_value_decode(Module, Type, Value, Bytes);
                       Bytes when is_binary(Bytes) ->
                           test_value_decode(Module, Type, Value, Bytes);
                       Error ->
                           {error, {asn1,
                                    {encode, {{Module, Type, Value}, Error}}}}
                   end
               end).


test_value_decode(Module, Type, Value, Bytes) ->
    NewBytes = prepare_bytes(Bytes),
    case Module:decode(Type, NewBytes) of
        {ok,Value}      -> {ok, {Module,Type,Value}};
        {ok,Value,<<>>} -> {ok, {Module,Type,Value}};
        Value           -> {ok, {Module,Type,Value}};
        {Value,<<>>}    -> {ok, {Module,Type,Value}};

        %% Errors:
        {ok, Res}   ->
            {error, {asn1,
                     {encode_decode_mismatch,
                      {{Module, Type, Value}, Res}}}};
        {ok, Res, Rest} ->
            {error, {asn1,
                     {encode_decode_mismatch,
                      {{Module, Type, Value}, {Res,Rest}}}}};
        Error       ->
            {error, {asn1,
                     {{decode,
                       {Module, Type, Value}, Error}}}}
    end.

-doc """
Returns an Erlang term that is an example of a valid Erlang representation of a
value of the `ASN.1` type `Type`. The value is a random value and subsequent
calls to this function will for most types return different values.

> #### Note {: .info }
>
> Currently, the `value` function has many limitations. Essentially, it will
> mostly work for old specifications based on the 1997 standard for ASN.1, but
> not for most modern-style applications. Another limitation is that the `value`
> function may not work if options that change code generations strategies such
> as the options `macro_name_prefix` and `record_name_prefix` have been used.
""".
-spec value(Module, Type) -> {ok, Value} | {error, Reason} when
      Module :: module(),
      Type :: atom(),
      Value :: term(),
      Reason :: term().
value(Module, Type) -> value(Module, Type, []).

-doc false.
value(Module, Type, Includes) ->
    in_process(fun() ->
                   start(strip_includes(Includes)),
                   case check(Module, Includes) of
                       {ok, _NewTypes} -> get_value(Module, Type);
                       Error           -> Error
                   end
               end).

get_value(Module, Type) ->
    case asn1ct_value:from_type(Module, Type) of
        {error, Reason} -> {error, Reason};
        Result          -> {ok, Result}
    end.

check(Module, Includes) ->
    case asn1_db:dbload(Module) of
	error ->
            {error,asn1db_missing_or_out_of_date};
	ok ->
	    M = asn1_db:dbget(Module, 'MODULE'),
            TypeOrVal =  M#module.typeorval,
            State = #state{mname = M#module.name,
                           module = M#module{typeorval=[]},
                           options = Includes},
            case asn1ct_check:check(State, TypeOrVal) of
                {ok, {NewTypes, _, _, _, _, _}, _} -> {ok, NewTypes};
                {error, Reason}                    -> {error, Reason}
            end
    end.

prepare_bytes(Bytes) when is_binary(Bytes) -> Bytes;
prepare_bytes(Bytes) -> list_to_binary(Bytes).

-doc false.
vsn() ->
    ?vsn.

specialized_decode_prepare(#gen{erule=ber,options=Options}=Gen, #module{name=Mod}) ->
    case lists:member(asn1config, Options) of
        true ->
            case read_config_file(Gen, Mod) of
                {ok,ConfigName,ConfigItems} ->
                    try
                        asn1ct_partial_decode:prepare(ConfigItems, Mod)
                    catch
                        throw:{structured_error,Error} ->
                            {error,[{structured_error,{ConfigName,none},
                                     asn1ct_partial_decode,Error}]}
                    end;
                no_config_file ->
                    ok
            end;
        false ->
            ok
    end;
specialized_decode_prepare(_, _) ->
    ok.

-doc false.
partial_inc_dec_toptype([T|_]) when is_atom(T) ->
    T;
partial_inc_dec_toptype([{T,_}|_]) when is_atom(T) ->
    T;
partial_inc_dec_toptype([L|_]) when is_list(L) ->
    partial_inc_dec_toptype(L);
partial_inc_dec_toptype(_) ->
    throw({error,{"no top type found for partial incomplete decode"}}).

%% Reads the content from the configuration file and returns the
%% selected part chosen by InfoType. Assumes that the config file
%% content is an Erlang term.
read_config_file_info(ModuleName, InfoType) when is_atom(InfoType) ->
    Name = ensure_ext(ModuleName, ".asn1config"),
    {ok,_,CfgList} = read_config_file0(Name, []),
    get_config_info(CfgList, InfoType).

read_config_file(#gen{options=Options}, ModuleName) ->
    Name = ensure_ext(ModuleName, ".asn1config"),
    Includes = [I || {i,I} <- Options],
    read_config_file0(Name, ["."|Includes]).

read_config_file0(Name0, [Dir|Dirs]) ->
    Name = filename:join(Dir, Name0),
    case file:consult(Name) of
	{ok,CfgList} ->
	    {ok,Name,CfgList};
	{error,enoent} ->
            read_config_file0(Name0, Dirs);
	{error,Reason} ->
	    Error = "error reading asn1 config file: " ++
		file:format_error(Reason),
	    throw({error,Error})
    end;
read_config_file0(_, []) ->
    no_config_file.

ensure_ext(ModuleName, Ext) ->
    Name = filename:join([ModuleName]),
    case filename:extension(Name) of
        Ext -> Name;
        _ -> Name ++ Ext
    end.
    
get_config_info(CfgList,InfoType) ->
    case lists:keysearch(InfoType,1,CfgList) of
	{value,{InfoType,Value}} ->
	    Value;
	false ->
	    []
    end.

%% save_config/2 saves the Info with the key Key
%% Before saving anything check if a table exists
%% The record gen_state is saved with the key {asn1_config,gen_state}
-doc false.
save_config(Key,Info) ->
    asn1ct_table:new_reuse(asn1_general),
    asn1ct_table:insert(asn1_general, {{asn1_config, Key}, Info}).

-doc false.
read_config_data(Key) ->
    case asn1ct_table:exists(asn1_general) of
	false -> undefined;
	true ->
	    case asn1ct_table:lookup(asn1_general,{asn1_config,Key}) of
		[{_,Data}] -> Data;
		Err ->
                    %% Err is [] when nothing was saved in the ets table
		    Err
	    end
    end.


%%
%% Functions to manipulate the gen_state record saved in the
%% asn1_general ets table.
%%

%% saves input data in a new gen_state record
-doc false.
save_gen_state(exclusive_decode,{_,ConfList},PartIncTlvTagList) ->
    State =
	case get_gen_state() of
	    S when is_record(S,gen_state) -> S;
	    _ -> #gen_state{}
	end,
    StateRec = State#gen_state{inc_tag_pattern=PartIncTlvTagList,
			       inc_type_pattern=ConfList},
    save_config(gen_state,StateRec);
save_gen_state(_,_,_) ->
    case get_gen_state() of
	S when is_record(S,gen_state) -> ok;
	_ -> save_config(gen_state,#gen_state{})
    end.

-doc false.
save_gen_state(selective_decode,{_,Type_component_name_list}) ->
    State =
	case get_gen_state() of
	    S when is_record(S,gen_state) -> S;
	    _ -> #gen_state{}
	end,
    StateRec = State#gen_state{type_pattern=Type_component_name_list},
    save_config(gen_state,StateRec);
save_gen_state(selective_decode,_) ->
    ok.

save_gen_state(GenState) when is_record(GenState,gen_state) ->
    save_config(gen_state,GenState).


%% get_gen_state_field returns undefined if no gen_state exists or if
%% Field is undefined or the data at the field.
-doc false.
get_gen_state_field(Field) ->
    case read_config_data(gen_state) of
	undefined ->
	    undefined;
	GenState when is_record(GenState,gen_state) -> 
	    get_gen_state_field(GenState,Field);
	Err ->
	    exit({error,{asn1,{"false configuration file info",Err}}})
    end.
get_gen_state_field(#gen_state{active=Active},active) ->
    Active;
get_gen_state_field(_,active) ->
    false;
get_gen_state_field(GS,prefix) ->
    GS#gen_state.prefix;
get_gen_state_field(GS,inc_tag_pattern) ->
    GS#gen_state.inc_tag_pattern;
get_gen_state_field(GS,tag_pattern) ->
    GS#gen_state.tag_pattern;
get_gen_state_field(GS,inc_type_pattern) ->
    GS#gen_state.inc_type_pattern;
get_gen_state_field(GS,type_pattern) ->
    GS#gen_state.type_pattern;
get_gen_state_field(GS,func_name) ->
    GS#gen_state.func_name;
get_gen_state_field(GS,namelist) ->
    GS#gen_state.namelist;
get_gen_state_field(GS,tobe_refed_funcs) ->
    GS#gen_state.tobe_refed_funcs;
get_gen_state_field(GS,gen_refed_funcs) ->
    GS#gen_state.gen_refed_funcs;
get_gen_state_field(GS,generated_functions) ->
    GS#gen_state.generated_functions;
get_gen_state_field(GS,suffix_index) ->
    GS#gen_state.suffix_index;
get_gen_state_field(GS,current_suffix_index) ->
    GS#gen_state.current_suffix_index.

get_gen_state() ->
    read_config_data(gen_state).


-doc false.
update_gen_state(Field,Data) ->
    case get_gen_state() of
	State when is_record(State,gen_state) ->
	    update_gen_state(Field,State,Data);
	_ ->
	    exit({error,{asn1,{internal,
			       "tried to update nonexistent gen_state",Field,Data}}})
    end.
update_gen_state(active,State,Data) ->
    save_gen_state(State#gen_state{active=Data});
update_gen_state(prefix,State,Data) ->
    save_gen_state(State#gen_state{prefix=Data});
update_gen_state(inc_tag_pattern,State,Data) ->
    save_gen_state(State#gen_state{inc_tag_pattern=Data});
update_gen_state(tag_pattern,State,Data) ->
    save_gen_state(State#gen_state{tag_pattern=Data});
update_gen_state(inc_type_pattern,State,Data) ->
    save_gen_state(State#gen_state{inc_type_pattern=Data});
update_gen_state(type_pattern,State,Data) ->
    save_gen_state(State#gen_state{type_pattern=Data});
update_gen_state(func_name,State,Data) ->
    save_gen_state(State#gen_state{func_name=Data});
update_gen_state(namelist,State,Data) ->
    save_gen_state(State#gen_state{namelist=Data});
update_gen_state(tobe_refed_funcs,State,Data) ->
    save_gen_state(State#gen_state{tobe_refed_funcs=Data});
update_gen_state(gen_refed_funcs,State,Data) ->
    save_gen_state(State#gen_state{gen_refed_funcs=Data});
update_gen_state(generated_functions,State,Data) ->
    save_gen_state(State#gen_state{generated_functions=Data});
update_gen_state(suffix_index,State,Data) ->
    save_gen_state(State#gen_state{suffix_index=Data});
update_gen_state(current_suffix_index,State,Data) ->
    save_gen_state(State#gen_state{current_suffix_index=Data}).

-doc false.
update_namelist(Name) ->
    case get_gen_state_field(namelist) of
	[Name,Rest] -> update_gen_state(namelist,Rest);
	[Name|Rest] -> update_gen_state(namelist,Rest);
	[{Name,List}] when is_list(List) -> update_gen_state(namelist,List);
	[{Name,Atom}|Rest] when is_atom(Atom) -> update_gen_state(namelist,Rest);
	Other -> Other
    end.

%% removes a bracket from the namelist
-doc false.
step_in_constructed() ->
    case get_gen_state_field(namelist) of
	[L] when is_list(L) ->
	    update_gen_state(namelist,L);
	_ -> ok
    end.
			
-doc false.
is_function_generated(Name) ->
    case get_gen_state_field(gen_refed_funcs) of
	L when is_list(L) ->
	    lists:member(Name,L);
	_ ->
	    false
    end.
			       
-doc false.
get_tobe_refed_func(Name) ->
    case get_gen_state_field(tobe_refed_funcs) of
	L when is_list(L) ->
	    case lists:keysearch(Name,1,L) of
		{_,Element} ->
		    Element;
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.

%% add_tobe_refed_func saves Data that is a three or four element
%% tuple.  Do not save if it exists in generated_functions, because
%% then it will be or already is generated.
-doc false.
add_tobe_refed_func(Data) ->
    {Name,SI,Pattern} = 
	fun({N,Si,P,_}) -> {N,Si,P};
	    (D) -> D end (Data),
    NewData =
	case SI of
	    I when is_integer(I) ->
		fun(D) -> D end(Data);
	    _ ->
		fun({N,_,P}) -> {N,0,P};
		   ({N,_,P,T}) -> {N,0,P,T} end (Data)
	end,
    
    L = get_gen_state_field(generated_functions),
    case generated_functions_member(get(currmod),Name,L,Pattern) of
	true ->
            %% it exists in generated_functions, it has already
            %% been generated or saved in tobe_refed_func
	    ok;
	_ ->
	    add_once_tobe_refed_func(NewData),
	    %% only to get it saved in generated_functions
	    maybe_rename_function(tobe_refed,Name,Pattern)
    end.



%% Adds only one element with same Name and Index where Data =
%% {Name,Index,Pattern}.
add_once_tobe_refed_func(Data) ->
    TRFL = get_gen_state_field(tobe_refed_funcs),
    {Name,Index} = {element(1,Data),element(2,Data)},
    case lists:filter(fun({N,I,_}) when N==Name,I==Index ->true;
			 ({N,I,_,_}) when N==Name,I==Index -> true;
			 (_) -> false end,TRFL) of
	[] ->
	    update_gen_state(tobe_refed_funcs,[Data|TRFL]);
	_ ->
	    ok
    end.


%% Moves Name from the to be list to the generated list.
-doc false.
generated_refed_func(Name) ->
    L = get_gen_state_field(tobe_refed_funcs),
    NewL = lists:keydelete(Name,1,L),
    update_gen_state(tobe_refed_funcs,NewL),
    L2 = get_gen_state_field(gen_refed_funcs),
    update_gen_state(gen_refed_funcs,[Name|L2]).

%% Adds Data to gen_refed_funcs field in gen_state.
-doc false.
add_generated_refed_func(Data) ->
    case is_function_generated(Data) of
	true ->
	    ok;
	_ ->
	    L = get_gen_state_field(gen_refed_funcs),
	    update_gen_state(gen_refed_funcs,[Data|L])
    end.

-doc false.
next_refed_func() ->
    case get_gen_state_field(tobe_refed_funcs) of
	[] ->
	    [];
	[H|T] ->
	    update_gen_state(tobe_refed_funcs,T),
	    H
    end.

-doc false.
reset_gen_state() ->
    save_gen_state(#gen_state{}).

%% Adds Data to generated_functions field in gen_state.
add_generated_function(Data) ->
    L = get_gen_state_field(generated_functions),
    update_gen_state(generated_functions,[Data|L]).


%% Each type has its own index starting from 0. If index is 0 there is
%% no renaming.
-doc false.
maybe_rename_function(Mode,Name,Pattern) ->
    case get_gen_state_field(generated_functions) of
	[] when Mode==inc_disp -> add_generated_function({Name,0,Pattern}),
	      Name;
	[] ->
	    exit({error,{asn1,internal_error_exclusive_decode}});
	L ->
	    case {Mode,generated_functions_member(get(currmod),Name,L)} of
		{_,true} ->
		    L2 = generated_functions_filter(get(currmod),Name,L),
		    case lists:keysearch(Pattern,3,L2) of
			false ->
                            %% name existed, but not pattern
			    NextIndex = length(L2),
			    %% rename function
			    Suffix = lists:concat(["_",NextIndex]),
			    NewName = 
				maybe_rename_function2(type_check(Name),Name,
						       Suffix),
			    add_generated_function({Name,NextIndex,Pattern}),
			    NewName;
			Value ->
                            %% name and pattern existed
			    %% do not save any new index
			    Suffix = make_suffix(Value),
			    Name2 =
				case Name of
				    #'Externaltypereference'{type=T} -> T;
				    _ -> Name
				end,
			    lists:concat([Name2,Suffix])
		    end;
		{inc_disp,_} ->
                    %% this is when decode_partial_inc_disp/2 is
                    %% generated
		    add_generated_function({Name,0,Pattern}),
		    Name;
		_ -> % this if call from add_tobe_refed_func
		    add_generated_function({Name,0,Pattern}),
		    Name
	    end
    end.

    
maybe_rename_function2(record,#'Externaltypereference'{type=Name},Suffix) ->
    lists:concat([Name,Suffix]);
maybe_rename_function2(list,List,Suffix) ->
    lists:concat([asn1ct_gen:list2name(List),Suffix]);
maybe_rename_function2(Thing,Name,Suffix)
  when Thing==atom;Thing==integer;Thing==string ->
    lists:concat([Name,Suffix]).
	       
%% generated_functions_member/4 checks on both Name and Pattern if
%%  the element exists in L
generated_functions_member(M,Name,L,Pattern) ->
    maybe
        true ?= generated_functions_member(M,Name,L),
        L2 = generated_functions_filter(M,Name,L),
        {value,_} ?= lists:keysearch(Pattern,3,L2),
        true
    else
        false -> false
    end.

generated_functions_member(_M,Name,[{Name,_,_}|_]) ->
    true;
generated_functions_member(M,#'Externaltypereference'{module=M,type=T},
			   [{#'Externaltypereference'{module=M,type=T}
			     ,_,_}|_]) ->
    true;
generated_functions_member(M,#'Externaltypereference'{module=M,type=Name},
			  [{Name,_,_}|_]) ->
    true;
generated_functions_member(M,Name,[_|T]) ->
    generated_functions_member(M,Name,T);
generated_functions_member(_,_,[]) ->
    false.

generated_functions_filter(_,Name,L) when is_atom(Name);is_list(Name) ->
    lists:filter(fun({N,_,_}) when N==Name -> true;
		    (_) -> false
		 end, L);
generated_functions_filter(M,#'Externaltypereference'{module=M,type=Name},L)->
    %% remove top typename from patterns
    RemoveTType = 
	fun({N,I,[N,P]}) when N == Name ->
		{N,I,P};
	   ({#'Externaltypereference'{module=M1,type=N},I,P}) when M1==M ->
		{N,I,P};
	   (P) -> P
	end,
    L2 = lists:map(RemoveTType,L),
    generated_functions_filter(M,Name,L2).


-doc false.
maybe_saved_sindex(Name,Pattern) ->
    case get_gen_state_field(generated_functions) of
	[] -> false;
	L ->
	    case generated_functions_member(get(currmod),Name,L) of
		true ->
		    L2 = generated_functions_filter(get(currmod),Name,L),
		    case lists:keysearch(Pattern,3,L2) of
			{value,{_,I,_}} ->
			    I;
			_ -> length(L2) % this should be length(L2)!
		    end;
		_ -> false
	    end
    end.
    
-doc false.
current_sindex() ->
    get_gen_state_field(current_suffix_index).

-doc false.
set_current_sindex(Index) ->
    update_gen_state(current_suffix_index,Index).


type_check(A) when is_atom(A) ->
    atom;
type_check(L) when is_list(L) ->
    Pred = fun(X) when X=<255 ->
		   false;
	      (_) -> true
	   end,
    case lists:filter(Pred,L) of
	[] ->
	    string;
	_ ->
	    list
    end;
type_check(#'Externaltypereference'{}) ->
    record.

 make_suffix({_,{_,0,_}}) ->
     "";
 make_suffix({_,{_,I,_}}) ->
     lists:concat(["_",I]);
 make_suffix(_) ->
     "".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Report functions.
%%
%% Error messages are controlled with the 'errors' compiler option
%% Warning messages are controlled with the 'warnings' compiler option
%% Verbose messages are controlled with the 'verbose' compiler option

-doc false.
error(Format, Args, S) ->
    case is_error(S) of
	true ->
	    io:format(Format, Args);
	false ->
	    ok
    end.

-doc false.
warning(Format, Args, S) ->
    case is_warning(S) of
	true ->
	    io:format("Warning: " ++ Format, Args);
	false ->
	    ok
    end.

-doc false.
warning(Format, Args, S, Reason) ->
    case {is_werr(S), is_error(S), is_warning(S)} of
	{true, true, _} ->
	    io:format(Format, Args),
	    throw({error, Reason});
	{false, _, true} ->
	    io:format(Format, Args);
	_ ->
	    ok
    end.

-doc false.
verbose(Format, Args, S) ->
    case is_verbose(S) of
	true ->
	    io:format(Format, Args);
	false ->
	    ok
    end.

-doc false.
format_error({write_error,File,Reason}) ->
    io_lib:format(<<"writing output file ~ts failed: ~s">>,
		  [File,file:format_error(Reason)]).

is_error(#state{options=Opts}) ->
    is_error(Opts);
is_error(#gen{options=Opts}) ->
    is_error(Opts);
is_error(O) ->
    lists:member(errors, O) orelse is_verbose(O).

is_warning(S) when is_record(S, state) ->
    is_warning(S#state.options);
is_warning(O) ->
    lists:member(warnings, O) orelse is_verbose(O).

is_verbose(#state{options=Opts}) ->
    is_verbose(Opts);
is_verbose(#gen{options=Opts}) ->
    is_verbose(Opts);
is_verbose(O) ->
    lists:member(verbose, O).

is_werr(S) when is_record(S, state) ->
    is_werr(S#state.options);
is_werr(O) ->
    lists:member(warnings_as_errors, O).


in_process(Fun) ->
    Parent = self(),
    Pid = spawn_link(fun() -> process(Parent, Fun) end),
    receive
        {Pid, Result}               -> Result;
        {Pid, Class, Reason, Stack} ->
            ST = try throw(x) catch throw:x:Stk -> Stk end,
            erlang:raise(Class, Reason, Stack ++ ST)
    end.

process(Parent, Fun) ->
    try
        Parent ! {self(), Fun()}
    catch Class:Reason:Stack ->
        Parent ! {self(), Class, Reason, Stack}
    end.
