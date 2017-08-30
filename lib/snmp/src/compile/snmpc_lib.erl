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

-module(snmpc_lib).

%% API
%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([test_father/4, make_ASN1type/1, import/1, makeInternalNode2/2,
	 is_consistent/1, resolve_defval/1, make_variable_info/1,
	 check_trap_name/3, make_table_info/5, get_final_mib/2, set_dir/2,
	 look_at/1, add_cdata/2,
	 check_object_group/4, check_notification_group/4, 
	 check_notification/3, 
	 register_oid/4,
	 error/2, error/3,
	 %% warning/2, warning/3,
	 print_error/2, print_error/3,
	 make_cdata/1,
	 key1search/2, key1search/3]).

%% internal exports
-export([check_of/1, check_trap/2, check_trap/3, get_elem/2]).

%% debug exports
-export([vvalidate/1, vprint/6]).


-include("snmp_types.hrl").
-include("snmpc.hrl").
-include("snmpc_lib.hrl").


%%----------------------------------------------------------------------------
%% Some basic types 
%%----------------------------------------------------------------------------

-type severity() :: 'silence' | 'warning' | 'info' | 'log' | 'debug' | 'trace'.


test_father(FatherName, NewVarName, SubIndex, Line) ->
    CDATA = get(cdata),
    case lists:keysearch(FatherName, #me.aliasname, CDATA#cdata.mes) of
	{value, #me{entrytype = table, aliasname = TableName}} ->
	    print_error("Variable '~w' (sub-index '~w') cannot "
			"be defined under table '~w'.",
			[NewVarName, SubIndex, TableName],Line);
	{value, #me{entrytype = table_entry, aliasname = TableName}} ->
	    print_error("Variable '~w' (sub-index '~w') cannot "
			"be defined under table entry '~w'.",
			[NewVarName, SubIndex, TableName], Line);
	
	_X -> %% internal or variable
	    case lists:last(SubIndex) of
		0 ->
		    print_error("'~w'. A zero-valued final subidentifier is reserved for future use. (RFC1902, 7.10)",[NewVarName],Line);
		_ -> ok
	    end
    end.

make_ASN1type({{type,Type},Line}) ->
    case lookup_vartype(Type) of
        {value, ASN1type} ->  
	    ASN1type;
	false ->
	    print_error("Undefined type '~w'",[Type],Line),
	    guess_integer_type()
    end;
make_ASN1type({{type_with_size,Type,{range,Lo,Hi}},Line}) ->
    case lookup_vartype(Type) of
        {value,ASN1type} ->  
	    case allow_size_rfc1902(BaseType = ASN1type#asn1_type.bertype) of
		true ->
		    ok;
		false ->
		    print_error(
		      "Size refinement is not allowed for subclass from ~w.",
		      [BaseType],Line)
	    end,
	    ASN1type#asn1_type{lo = Lo, hi = Hi};
	false ->
	    print_error("Undefined type '~w'",[Type],Line),
	    guess_string_type()
    end;
make_ASN1type({{integer_with_enum,Type,Enums},Line}) ->
    case lookup_vartype(Type) of
        {value,ASN1type} ->  ASN1type#asn1_type{assocList = [{enums, Enums}]};
	false ->
	    print_error("Undefined type '~w'",[Type],Line),
    	    guess_integer_type()
    end;
make_ASN1type({{bits,Kibbles},Line}) ->
    case get(snmp_version) of
	2 ->
	    {value,Bits} = lookup_vartype('BITS'),
	    Kibbles2 = test_kibbles(Kibbles, Line),
	    Bits#asn1_type{assocList = [{kibbles, Kibbles2}]};
	_ ->
	    guess_integer_type()
    end;
make_ASN1type({{sequence_of, _Type},Line}) ->
    print_error("Use of SEQUENCE OF in non-table context.",[],Line),
    guess_integer_type().

test_kibbles([], Line) ->
    print_error("No kibbles found.",[],Line),
    [];
test_kibbles(Kibbles,Line) ->
    test_kibbles2(R = lists:keysort(2,Kibbles),0,Line),
    R.

test_kibbles2([],_,_) ->
    ok;
test_kibbles2([{_KibbleName,BitNo}|Ks],ExpectBitNo,Line)
  when BitNo >= ExpectBitNo ->
    test_kibbles2(Ks,BitNo+1,Line);
test_kibbles2([{_KibbleName,BitNo}|_Ks],ExpectBitNo,Line) ->
    print_error("Expected kibble no ~p but got ~p.",[ExpectBitNo,BitNo],Line).
    
    
allow_size_rfc1902('INTEGER') -> true;
allow_size_rfc1902('Integer32') -> true;
allow_size_rfc1902('Unsigned32') -> true;
allow_size_rfc1902('OCTET STRING') -> true;
allow_size_rfc1902('Gauge32') -> true;
allow_size_rfc1902('Opaque') -> true;
allow_size_rfc1902(_) -> false.

guess_integer_type() ->
    {value,ASN1int} = lookup_vartype('INTEGER'),
    ASN1int.

guess_string_type() ->
    {value,ASN1str} = lookup_vartype('OCTET STRING'),
    ASN1str.

lookup_vartype(Type) ->
    CDATA = get(cdata),
    lists:keysearch(Type, #asn1_type.aliasname, CDATA#cdata.asn1_types).




%%--------------------------------------------------
%% Reads the oid-function files.
%% Out: A list of {oid, entry}.
%% oid is here either a Oid with integers, or
%% with symbolic names.
%% entry is {M,F,A}.
%%--------------------------------------------------
read_funcs(FileName) ->
    case snmpc_misc:read_noexit(FileName, fun check_of/1) of
	{ok, Res} -> Res;
	{error, LineNo, Reason} ->
	    print_error("~p: ~w: Syntax error: ~p", 
			[FileName, LineNo, Reason]),
	    [];
        {error, open_file} -> []
    end.

check_of({module, M}) when is_atom(M) ->
    {ok, {module, M}};
check_of({Oid, {M, F, A}}) when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    {ok, {Oid, {M, F, A}}};
check_of({_Oid, {M, F, A}})  ->
    {invalid_argument, {M, F, A}};
check_of(X) ->
    {invalid_func, X}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for IMPORT implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import(ImportList) ->
    %% Register some well known top-nodes (directly under the root)
    WellKnownNodes = [ makeInternalNode(ccitt,             [0]),
		       makeInternalNode(iso,               [1]),
		       makeInternalNode('joint-iso-ccitt', [2]) ],
    lists:foreach(
      fun(#me{aliasname = AliasName, oid = Oid}) ->
	      register_oid(undef, AliasName, root, Oid)
      end,
      WellKnownNodes),
    lists:foreach(fun import_mib/1, ImportList).


%%----------------------------------------------------------------------
%% Returns: <nothing> only side effect stuff.
%%----------------------------------------------------------------------
import_mib({{'SNMPv2-SMI', ImportsFromMib},Line}) ->
    Nodes = [makeInternalNode(internet,     [1,3,6,1]),
	     makeInternalNode(directory,    [1,3,6,1,1]),
	     makeInternalNode(mgmt,         [1,3,6,1,2]),
	     makeInternalNode('mib-2',      [1,3,6,1,2,1]),
	     makeInternalNode(transmission, [1,3,6,1,2,1,10]),
	     makeInternalNode(experimental, [1,3,6,1,3]),
	     makeInternalNode(private,      [1,3,6,1,4]),
	     makeInternalNode(enterprises,  [1,3,6,1,4,1]),
	     makeInternalNode(zeroDotZero,  [0,0]),
	     makeInternalNode(security,     [1,3,6,1,5]),
	     makeInternalNode(snmpV2,       [1,3,6,1,6]),
	     makeInternalNode(snmpDomains,  [1,3,6,1,6,1]),
	     makeInternalNode(snmpProxys,   [1,3,6,1,6,2]),
	     makeInternalNode(snmpModules,  [1,3,6,1,6,3])],
    Types = [#asn1_type{bertype   = 'Integer32',
			aliasname = 'Integer32',
			lo = -2147483648, hi = 2147483647},
	     #asn1_type{bertype   = 'IpAddress',
			aliasname = 'IpAddress',
			lo = 4, hi = 4},
	     #asn1_type{bertype   = 'Counter32', 
			aliasname = 'Counter32',
			lo = 0, hi = 4294967295},
	     #asn1_type{bertype   = 'Gauge32', 
			aliasname = 'Gauge32',
			lo = 0, hi = 4294967295},
	     #asn1_type{bertype   = 'Unsigned32', 
			aliasname = 'Unsigned32',
			lo = 0, hi = 4294967295},
	     #asn1_type{bertype   = 'TimeTicks', 
			aliasname = 'TimeTicks',
			lo = 0, hi=4294967295},
	     #asn1_type{bertype   = 'Opaque', 
			aliasname = 'Opaque'},
	     #asn1_type{bertype   = 'Counter64', 
			aliasname = 'Counter64',
			lo = 0, hi = 18446744073709551615},
	     #asn1_type{bertype   = 'BITS', 
			aliasname = 'BITS'}
	    ],
    Macros = ['MODULE-IDENTITY','OBJECT-IDENTITY','OBJECT-TYPE',
	      'NOTIFICATION-TYPE'],
    import_built_in_loop(ImportsFromMib,Nodes,Types,Macros,'SNMPv2-SMI',Line);
import_mib({{'RFC-1215', ImportsFromMib},Line}) ->
    Macros = ['TRAP-TYPE'],
    import_built_in_loop(ImportsFromMib, [],[],Macros,'RFC-1215', Line);
import_mib({{'RFC-1212', ImportsFromMib},Line}) ->
    Macros = ['OBJECT-TYPE'],
    import_built_in_loop(ImportsFromMib, [],[],Macros,'RFC-1212', Line);
import_mib({{'SNMPv2-TC', ImportsFromMib},Line}) ->
    Nodes  = [],
    Types  = [#asn1_type{aliasname = 'DisplayString',
			 bertype   = 'OCTET STRING',
			 lo = 0, hi = 255},
	      #asn1_type{aliasname = 'PhysAddress',
			 bertype   = 'OCTET STRING'},
	      #asn1_type{aliasname = 'MacAddress',
			 bertype   = 'OCTET STRING',
			 lo = 6, hi = 6},
	      #asn1_type{aliasname = 'TruthValue',
			 bertype   = 'INTEGER',
			 assocList = [{enums,[{false,2},{true,1}]}]},
	      #asn1_type{aliasname = 'TestAndIncr',
			 bertype   = 'INTEGER',
			 lo = 0, hi = 2147483647},
	      #asn1_type{aliasname = 'AutonomousType',
			 bertype   = 'OBJECT IDENTIFIER'},
	      #asn1_type{aliasname = 'InstancePointer',
			 bertype   = 'OBJECT IDENTIFIER'},
	      #asn1_type{aliasname = 'VariablePointer',
			 bertype   = 'OBJECT IDENTIFIER'},
	      #asn1_type{aliasname = 'RowPointer',
			 bertype   = 'OBJECT IDENTIFIER'},
	      #asn1_type{aliasname = 'RowStatus',
			 bertype   = 'INTEGER',
			 assocList = [{enums,[{destroy,       6},
					      {createAndWait, 5},
					      {createAndGo,   4},
					      {notReady,      3},
					      {notInService,  2},
					      {active,        1}]}]},
	      #asn1_type{aliasname = 'TimeStamp',
			 bertype   = 'TimeTicks'},
	      #asn1_type{aliasname = 'TimeInterval',
			 bertype   = 'INTEGER',
			 lo = 0, hi = 2147483647},
	      #asn1_type{aliasname = 'DateAndTime',
			 bertype   = 'OCTET STRING',
			 lo = 8, hi = 11}, %% Actually 8 | 11
	      #asn1_type{aliasname = 'StorageType',
			 bertype   = 'INTEGER',
			 assocList = [{enums,[{readOnly,    5},
					      {permanent,   4},
					      {nonVolatile, 3},
					      {volatile,    2},
					      {other,       1}]}]},
	      #asn1_type{aliasname = 'TDomain',
			 bertype   = 'OBJECT IDENTIFIER'},
	      #asn1_type{aliasname = 'TAddress',
			 bertype   = 'OCTET STRING',
			 lo = 1, hi = 255}
	     ],
    Macros = ['TEXTUAL-CONVENTION'],
    import_built_in_loop(ImportsFromMib,Nodes,Types,Macros,'SNMPv2-TC',Line);
import_mib({{'SNMPv2-CONF', ImportsFromMib},Line}) ->
    Macros = ['OBJECT-GROUP',
	      'NOTIFICATION-GROUP',
	      'MODULE-COMPLIANCE', 
	      'AGENT-CAPABILITIES'],
    import_built_in_loop(ImportsFromMib,[],[],Macros,'SNMPv2-CONF',Line);
import_mib({{'RFC1155-SMI', ImportsFromMib},Line}) ->
    Nodes = [makeInternalNode(internet, [1,3,6,1]),
	     makeInternalNode(directory, [1,3,6,1,1]),
	     makeInternalNode(mgmt, [1,3,6,1,2]),
	     makeInternalNode(experimental, [1,3,6,1,3]),
	     makeInternalNode(private, [1,3,6,1,4]),
	     makeInternalNode(enterprises, [1,3,6,1,4,1])],
    Types = [#asn1_type{bertype = 'NetworkAddress',
			aliasname = 'NetworkAddress', lo = 4, hi = 4},
	     #asn1_type{bertype='Counter',aliasname='Counter',
			lo=0,hi=4294967295},
	     #asn1_type{bertype='Gauge',aliasname='Gauge',
			lo = 0, hi = 4294967295},
	     #asn1_type{bertype='IpAddress',aliasname='IpAddress',lo=4,hi=4},
	     #asn1_type{bertype = 'TimeTicks', aliasname = 'TimeTicks',
			lo = 0, hi=4294967295},
	     #asn1_type{bertype = 'Opaque', aliasname = 'Opaque'}],
    Macros = ['OBJECT-TYPE'],
    import_built_in_loop(ImportsFromMib,Nodes,Types,Macros,'RFC1155-SMI',Line);
import_mib({{MibName, ImportsFromMib},Line}) ->
    import_from_file({{MibName, ImportsFromMib},Line}).

import_built_in_loop(Objs, Nodes, Types, Macros, MibName, Line) ->
    lists:foreach(fun (Obj) ->
			 import_built_in(Obj,Nodes,Types,Macros,MibName,Line)
		 end, Objs).

import_from_file({{_MibName, []}, _Line}) -> 
    done;
import_from_file({{MibName, ImportsFromMib},Line}) ->
    Filename         = atom_to_list(MibName) ++ ".bin",
    {value, Path}    = snmpc_misc:assq(i, get(options)),
    {value, LibPath} = snmpc_misc:assq(il,get(options)),
    LibPath2         = include_lib(LibPath),
    Path2 = Path++LibPath2++[filename:join(code:priv_dir(snmp),"mibs"),
			     "./"],
    ImportedMib = case read_mib(Line,Filename, Path2) of
		      error ->
			  error("Could not import ~p from mib ~s. "
				"File not found. "
				"Check that the MIB to be IMPORTED "
				"is compiled and present in the import path.",
				[ImportsFromMib, Filename], Line);
		      Mib -> 
			  Mib
		  end,
    lists:foreach(fun (ImpObj) -> import(ImpObj,ImportedMib) end,
		  ImportsFromMib).

import_built_in({_tag,Obj}, Nodes, Types, Macros, MibName, Line) ->
    case lookup(Obj, Nodes) of
	{value, ME} ->
	    register_oid(undef, ME#me.aliasname, root, ME#me.oid),
	    add_cdata(#cdata.mes, [ME#me{imported = true, oid = undefined}]);
	false ->
	    case lists:keysearch(Obj, #asn1_type.aliasname, Types) of
		{value, ASN1Type} ->
		    add_cdata(#cdata.asn1_types,
			      [ASN1Type#asn1_type{imported=true}]);
		false ->
		    case lists:member(Obj, Macros) of
			true ->
			    add_cdata(#cdata.imported_macros,[Obj]);
			false ->
			    print_error("Cannot find '~w' in mib '~s'.",
					[Obj, MibName], Line)
		    end
	    end
    end.

include_lib([]) -> [];
include_lib([Dir|Dirs]) ->
    [Appl|Path] = filename:split(Dir),
    case code:lib_dir(list_to_atom(Appl)) of
	{error, _Reason} ->
	    include_lib(Dirs);
	DirPath ->
	    [filename:join(DirPath,filename:join(Path))|include_lib(Dirs)]
    end.


%%----------------------------------------------------------------------
%% Returns: #mib
%%----------------------------------------------------------------------
read_mib(_Line, _Filename, []) ->
    error;
read_mib(Line, Filename, [Dir|Path]) ->
    Dir2 = snmpc_misc:ensure_trailing_dir_delimiter(Dir),
    case snmpc_misc:read_mib(AbsFile=lists:append(Dir2, Filename)) of
	{ok, MIB} -> MIB;
	{error, enoent} ->
	    read_mib(Line, Filename, Path);
	{error, Reason} ->
	    ?vwarning("~s found but not imported: "
		      "~n   Reason: ~p", [AbsFile,Reason]),
	    read_mib(Line, Filename, Path)
    end.


%%----------------------------------------------------------------------
%% imports ME or Type from other Mib into current compilation data.
%%----------------------------------------------------------------------
import({node, NodeName}, #mib{mes = IMES, name = MibName}) ->
    case lookup(NodeName, IMES) of
	{value, ME} when ME#me.imported == false ->
	    register_oid(undef, ME#me.aliasname, root, ME#me.oid),
	    add_cdata(#cdata.mes, [ME#me{imported = true}]);
	_ ->
	    print_error("Cannot find '~w' among the objects in the mib '~s'.",
			[NodeName, MibName])
    end;
import({type, TypeName}, #mib{asn1_types = Types, name = MibName}) ->
    case lists:keysearch(TypeName, #asn1_type.aliasname, Types) of
	{value, ASN1Type} when is_record(ASN1Type, asn1_type) andalso 
                          (ASN1Type#asn1_type.imported =:= false) ->
	    add_cdata(#cdata.asn1_types, [ASN1Type#asn1_type{imported=true,
							aliasname=TypeName}]);
	_X ->
	    print_error("Cannot find '~w' among the types in the mib '~s'.",
			[TypeName, MibName])
    end;
import({builtin, Obj}, #mib{}) ->
    print_error("~p should be imported from a standard mib.",[Obj]).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for initialisation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Types defined in rfc1155 (SMI) are hard coded.
init_types() ->
    VerDep = case get(snmp_version) of
	     1 -> [];
	     2 ->
		  [#asn1_type{imported=true,bertype='BITS',aliasname='BITS'}]
	     end,
    [#asn1_type{imported = true, bertype = 'INTEGER', aliasname = 'INTEGER'},
     #asn1_type{imported=true,bertype='OCTET STRING',aliasname='OCTET STRING'},
     #asn1_type{imported=true,bertype='BIT STRING',aliasname='BIT STRING'},
     #asn1_type{imported = true, bertype = 'OBJECT IDENTIFIER',
		aliasname = 'OBJECT IDENTIFIER'} | VerDep].
     
makeInternalNode(Name, Oid) -> 
    makeInternalNode3(false, Name, Oid).

makeInternalNode2(Imported, Name) ->
    #me{imported = Imported, aliasname = Name, entrytype = internal}.

makeInternalNode3(Imported, Name, Oid) ->
    #me{imported = Imported, oid = Oid, aliasname = Name, entrytype = internal}.

make_cdata(CDataArg) ->
    MibFuncs =
        case CDataArg of
            {module, _Mod} -> [CDataArg];
            {file, MibFuncsFile} -> read_funcs(MibFuncsFile)
        end,
    #cdata{mibfuncs   = MibFuncs, 
	   asn1_types = init_types(),
	   oid_ets    = ets:new(oid_ets,    [set, private]),
	   status_ets = ets:new(status_ets, [set, private])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for Intermib consistency checking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_consistent(Filenames) ->
    case catch check_all_consistency(Filenames) of
	ok -> 
	    ok;
	{undef, Format, Data} ->
	    ok = io:format(Format, Data),
	    io:format("~n"),
	    {error, inconsistent}
    end.
    
check_all_consistency(Filenames) ->
    MIBs = [load_mib(Filename) || Filename <- Filenames],
    check_oid_conflicts(MIBs),
    check_trap_conflicts(MIBs),
    ok.

check_oid_conflicts(MIBs) ->
    MEs = lists:append( [get_elem(MIB, #mib.mes) || MIB <- MIBs] ), 
    SortedMEs = lists:keysort(#me.oid, MEs),
    search_for_dublettes2(#me{aliasname=dummy_init}, SortedMEs).

check_trap_conflicts(MIBs) ->
    Traps = lists:append( [get_elem(MIB, #mib.traps) || MIB <- MIBs] ),
    [check_trap(Trap, Traps) || Trap <- Traps].

check_trap(Trap, Traps) ->
    %% check_trap/3 -> error | ok
    Checked = [check_trap(T, Trap, undef) || T <- lists:delete(Trap, Traps)],
    case lists:member(error, Checked) of
	true ->
	    throw({undef,"",[]});
	false ->
	    ok
    end.


%%----------------------------------------------------------------------
%% Returns: {Oid, ASN1Type}
%%----------------------------------------------------------------------
trap_variable_info(Variable, Type, MEs) ->
    case lookup(Variable, MEs) of
	false ->
	    error("Error in ~s definition. Cannot find object '~w'.",
		  [Type, Variable]);
	{value, ME} when ME#me.entrytype == variable ->
	    {{variable, ME#me.aliasname}, ME#me.asn1_type};
	{value, ME} ->
	    {{column, ME#me.aliasname}, ME#me.asn1_type}
    end.

get_elem(MIB, Idx) ->
    element(Idx, MIB).

load_mib(Filename) ->
    F1 = snmpc_misc:strip_extension_from_filename(Filename, ".mib"),
    F2 = lists:append(F1, ".bin"),
    case snmpc_misc:read_mib(F2) of
	{error, Reason} ->
	    throw({undef, "Error reading file: ~w. Reason:~w", [F1, Reason]});
	{ok, Mib} ->
	    Mib
    end.

search_for_dublettes2(_PrevME, []) -> ok;
search_for_dublettes2(PrevME, [ME|MEs])
            when ME#me.imported==true ->
    search_for_dublettes2(PrevME, MEs);
search_for_dublettes2(PrevME, [ME|MEs]) 
    when PrevME#me.oid == ME#me.oid ->
    if PrevME#me.entrytype == internal, ME#me.entrytype == internal,
       PrevME#me.aliasname == ME#me.aliasname ->
	    search_for_dublettes2(ME, MEs);
       true ->
	    throw({undef,"Multiple used object with OBJECT IDENTIFIER '~w'"
			  " Used by '~w' and '~w' ", [PrevME#me.oid,
						      PrevME#me.aliasname, 
						      ME#me.aliasname]})
    end;
search_for_dublettes2(_PrevME, [ME|MEs]) ->
    search_for_dublettes2(ME, MEs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for handling of default value resolving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolve_defval(ME) ->
    case has_complex_defval(ME) of
	true ->
	    CDATA = get(cdata),
	    resolve_complex_defval(ME, CDATA#cdata.mes);
	false -> ME
    end.

has_complex_defval(#me{aliasname = N,
		       assocList = AssocList, 
		       asn1_type = #asn1_type{bertype = BT}}) 
           when is_list(AssocList) ->
    case snmpc_misc:assq(defval, AssocList) of
	{value, Int} when is_integer(Int) ->
	    false;
	{value, Val} when is_atom(Val) andalso (BT =:= 'OBJECT IDENTIFIER') ->
	    false; % resolved in update_me_oids
	{value, Val} when is_atom(Val) andalso (BT =:= 'INTEGER') ->
	    true;
	{value, Bits} when is_list(Bits) andalso (BT =:= 'BITS') ->
	    true;
	{value, Str} when is_list(Str) andalso (BT =:= 'OCTET STRING') ->
	    false; % but ok
	{value, Str} when is_list(Str) andalso (BT =:= 'Opaque') ->
	    false; % but ok
	{value, Str} when is_list(Str) andalso 
			  (length(Str) =:= 4) andalso 
			  (BT =:= 'IpAddress') ->
	    false; % but ok
	{value, Shit} ->
	    print_error("Bad default value for ~p: ~p [~p]",[N,Shit,BT]),
	    false;
	false -> %% no defval (or strings nyi)
	    false
    end;
has_complex_defval(_) -> false.

resolve_complex_defval(ME, _AllMEs) 
     when (ME#me.asn1_type)#asn1_type.bertype == 'INTEGER' ->
    #me{aliasname = MEName, assocList = AssocList} = ME,
    {value, DefVal} = snmpc_misc:assq(defval, AssocList),
    #asn1_type{bertype = TypeName,
	       assocList = AssocListForASN1Type} = ME#me.asn1_type,
    case snmpc_misc:assq(enums, AssocListForASN1Type) of
	false ->
	    print_error("Type '~w' has no defined enums. "
			"Used in DEFVAL for '~w'.", [TypeName, MEName]),
	    ME;
	{value, Enums} ->
	    case snmpc_misc:assq(DefVal, Enums) of
		false ->
		    print_error("Enum '~w' not found. "
				"Used in DEFVAL for '~w'.", [DefVal, MEName]),
		    ME;
		{value, IntVal} when is_integer(IntVal) ->
		    ME#me{assocList = lists:keyreplace(defval, 1, AssocList,
						       {defval, IntVal})}
	    end
    end;

resolve_complex_defval(ME, _AllMEs) 
     when (ME#me.asn1_type)#asn1_type.bertype =:= 'BITS' ->
    #me{aliasname = MEName, assocList = AssocList} = ME,
    {value, DefVal} = snmpc_misc:assq(defval, AssocList),
    #asn1_type{assocList = AssocListForASN1Type} = ME#me.asn1_type,
    {value, Kibbles} = snmpc_misc:assq(kibbles, AssocListForASN1Type),
    case snmpc_misc:bits_to_int(DefVal,Kibbles) of
	error->
	    print_error("Invalid default value ~w for ~w.",[DefVal, MEName]),
	    ME;
	IntVal when is_integer(IntVal) ->
	    ME#me{assocList = lists:keyreplace(defval, 1, AssocList,
					       {defval, IntVal})}
    end.


make_variable_info(#me{asn1_type = Asn1Type, assocList = Alist}) ->
    Defval = 
	case snmpc_misc:assq(defval, Alist) of
	    {value, Val} -> 
		Val;
	    _ -> 
		get_def(Asn1Type)
	end,
    #variable_info{defval = Defval}.

get_def(#asn1_type{bertype = BT, lo = LO, assocList = AL}) ->
    ?vtrace("get_def -> entry with"
	    "~n   BT: ~p"
	    "~n   LO: ~p"
	    "~n   AL: ~p", [BT, LO, AL]),
    get_def(BT, LO, AL).

get_def('INTEGER', Lo, _) when is_integer(Lo) -> Lo;
get_def('INTEGER', _, AL) -> 
    case snmpc_misc:assq(enums, AL) of
	{value, Enums} ->
	    case lists:keysort(2, Enums) of
		[{_, Val}|_] ->
		    Val;
		_ ->
		    0
	    end;
	_ ->
	    0
    end;
get_def('Counter', _, _) -> 0;
get_def('Gauge', _, _) -> 0;
get_def('TimeTicks', _, _) -> 0;
get_def('OCTET STRING', _, _) -> "";
get_def('IpAddress', _, _) -> [0,0,0,0];
get_def('NetworkAddress', _, _) -> [0,0,0,0];
get_def('OBJECT IDENTIFIER', _, _) -> [0, 0];
get_def('Opaque', _, _) -> "";
%v2
get_def('Integer32',Lo, _) when is_integer(Lo) -> Lo;
get_def('Integer32',_, _) -> 0;
get_def('Counter32',_, _) -> 0;
get_def('Gauge32',_, _) -> 0;
get_def('Unsigned32',_, _) -> 0;
get_def('BITS',_, _) -> 0;
get_def('Counter64',_, _) -> 0.

check_trap_name(EnterpriseName, Line, MEs) ->
    case lists:keysearch(EnterpriseName, #me.aliasname, MEs) of
	false -> 
	    error("Error in trap definition. Cannot find object '~w'.",
		  [EnterpriseName],Line);
	{value, _} -> 
	    true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for table functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%% This information is needed to be able to create default instrumentation
%% functions for tables.
%%----------------------------------------------------------------------

make_table_info(Line, TableName, {augments, SrcTableEntry}, _, ColumnMEs) ->
    ColMEs = lists:keysort(#me.oid, ColumnMEs),
    Nbr_of_Cols = length(ColMEs), 
    MEs = ColMEs ++ (get(cdata))#cdata.mes,
    Aug = case lookup(SrcTableEntry, MEs) of
	      false ->
		  print_error("Cannot AUGMENT the non-existing table entry ~p",
			      [SrcTableEntry], Line),
		  {augments, error};
	      {value, ME} ->
		  {augments, {SrcTableEntry, translate_type(ME#me.asn1_type)}}
	  end,
    FirstNonIdxCol = augments_first_non_index_column(ColMEs), 
    NoAccs         = list_not_accessible(FirstNonIdxCol, ColMEs),
    FirstAcc       = first_accessible(TableName, ColMEs),
    #table_info{nbr_of_cols      = Nbr_of_Cols,
		first_accessible = FirstAcc, 
		not_accessible   = NoAccs, 
		index_types      = Aug}; 
make_table_info(Line, TableName, {indexes, []}, _, _ColumnMEs) ->
    print_error("Table ~w lacks indexes.", [TableName],Line),
    #table_info{};
make_table_info(Line, TableName, {indexes, Indexes}, AfterIdxTypes, 
		ColumnMEs) ->
    ColMEs = lists:keysort(#me.oid, ColumnMEs),
    NonImpliedIndexes = lists:map(fun non_implied_name/1, Indexes),
    test_read_create_access(ColMEs, Line, dummy),
    NonIndexCol = test_index_positions(Line, NonImpliedIndexes, ColMEs),
    Nbr_of_Cols = length(ColMEs),
    ASN1Indexes = find_asn1_types_for_indexes(Indexes, 
					      AfterIdxTypes, ColMEs, Line),
    FA = first_accessible(TableName, ColMEs),
    StatCol = find_status_col(Line, TableName, ColMEs),
    NoAccs = list_not_accessible(NonIndexCol,ColMEs),
    case lists:member(StatCol,NoAccs) of
	true ->
	    print_error("Status column cannot be not-accessible. In table ~p.",
			[TableName],Line);
	false -> ok
    end,
    #table_info{nbr_of_cols      = Nbr_of_Cols,
		first_own_index  = find_first_own_index(NonImpliedIndexes,
							ColMEs, 1),
		status_col       = StatCol,
		first_accessible = FA,
		not_accessible   = NoAccs,
		index_types      = ASN1Indexes}.

%% Perkins p110
test_read_create_access([#me{aliasname = N, access = 'read-create'}|_ColMEs],
			Line, 'read-write') ->
    print_error("Column ~p cannot be read-create when another is read-write.",
		[N], Line);
test_read_create_access([#me{aliasname = N, access = 'read-write'}|_ColMEs],
		       Line, 'read-create') ->
    print_error("Column ~p cannot be read-write when another is read-create.",
		[N], Line);
test_read_create_access([#me{access = 'read-write'}|ColMEs],
			Line, _OtherStat) ->
    test_read_create_access(ColMEs,Line,'read-write');
test_read_create_access([#me{access = 'read-create'}|ColMEs],
			Line, _OtherStat) ->
    test_read_create_access(ColMEs,Line,'read-create');
test_read_create_access([_ME|ColMEs],Line,OtherStat) ->
    test_read_create_access(ColMEs,Line,OtherStat);
test_read_create_access([], _Line, _) -> 
    ok.    

find_status_col(_Line, _TableName, []) ->
    undefined;
find_status_col(_Line, _TableName,
		[#me{asn1_type=#asn1_type{aliasname='RowStatus'}}|_]) ->
    1;
find_status_col(Line, TableName, [_ShitME | MEs]) ->
    case find_status_col(Line, TableName, MEs) of
	undefined -> undefined;
	N -> 1+N
    end.

list_not_accessible(none,_) -> [];
list_not_accessible(NonIndexCol, ColMEs) when is_integer(NonIndexCol) ->
    list_not_accessible(lists:nthtail(NonIndexCol - 1,ColMEs)).

list_not_accessible([#me{access='not-accessible', oid=Col}|ColMEs]) ->
    [Col | list_not_accessible(ColMEs)];
list_not_accessible([_ColME|ColMEs]) ->
    list_not_accessible(ColMEs);
list_not_accessible([]) ->
    [].

%%----------------------------------------------------------------------
%% See definition of first_own_index in the table_info record definition.
%%----------------------------------------------------------------------
find_first_own_index([], _ColMEs, _FOI) -> 0;
find_first_own_index([NameOfIndex | Indexes], ColMEs, FOI) ->
    case lists:keysearch(NameOfIndex, #me.aliasname, ColMEs) of
	{value, _ME} ->
	    FOI;
	false ->
	    find_first_own_index(Indexes, ColMEs, FOI + 1)
    end.

first_accessible(TableName, []) ->
    error("Table '~w' must have at least one accessible column.",[TableName]);
first_accessible(TableName, [#me{access = 'not-accessible'} | T]) ->
    first_accessible(TableName, T);
first_accessible(_TableName, [#me{oid = Col} | _]) ->
    Col.

get_defvals(ColMEs) ->
    lists:keysort(1, 
        lists:filter(fun drop_undefined/1,
		     lists:map(fun column_and_defval/1, ColMEs))).

find_asn1_types_for_indexes(Indexes, AfterIdxTypes, ColMEs, Line) ->
    ?vtrace("find_asn1_types_for_indexes -> "
	    "~n   Indexes: ~p"
	    "~n   ColMEs:  ~p"
	    "~n   Line:    ~p", [Indexes, ColMEs, Line]),
    MEs = ColMEs ++ (get(cdata))#cdata.mes ++ 
	[#me{aliasname = Idx, asn1_type = Type} || {Idx, Type} <- 
						       AfterIdxTypes],
    test_implied(Indexes, Line),
    lists:map(fun (ColumnName) ->
		      translate_type(get_asn1_type(ColumnName, MEs, Line))
	      end, 
	      Indexes).

test_implied([],_) -> ok;
test_implied([{implied, _Type}, _OtherIndexElem|_], Line) ->
    print_error("Implied must be last.", [], Line);
test_implied([{implied, _Type}], _Line) -> 
    ok;
test_implied([_H|T], Line) -> 
    test_implied(T, Line).

drop_undefined({_X, undefined}) -> false;
drop_undefined({_X, _Y}) -> true;
drop_undefined(undefined) -> false;
drop_undefined(_X) -> true.

%% returns: {ColumnNo, Defval}
column_and_defval(#me{oid = Oid, assocList = AssocList}) ->
    ColumnNo = lists:last(Oid),
    case snmpc_misc:assq(defval, AssocList) of
	false -> {ColumnNo, undefined};
	{value, DefVal} -> {ColumnNo, DefVal}
    end.

%% returns: an asn1_type if ColME is an indexfield, otherwise undefined.
get_asn1_type({implied, ColumnName}, MEs, Line) ->
    ?vtrace("get_asn1_type(implied) -> "
	    "~n   ColumnName: ~p"
	    "~n   MEs:        ~p"
	    "~n   Line:       ~p", [ColumnName, MEs, Line]),
    case lookup(ColumnName, MEs) of
	{value,#me{asn1_type=A}} when A#asn1_type.bertype =:=
				      'OCTET STRING' ->
	    A#asn1_type{implied = true};
	{value,#me{asn1_type=A}} when A#asn1_type.bertype =:= 
				      'OBJECT IDENTIFIER' ->
	    A#asn1_type{implied = true};
	Shit ->
	    print_error("Only OCTET STRINGs and OIDs can be IMPLIED.(~w)", 
			[Shit], Line)
    end;
get_asn1_type(ColumnName, MEs, Line) ->
    ?vtrace("get_asn1_type -> "
	    "~n   ColumnName: ~p"
	    "~n   MEs:        ~p"
	    "~n   Line:       ~p", [ColumnName, MEs, Line]),
    case lookup(ColumnName, MEs) of
	{value,ME} -> ME#me.asn1_type;
	false -> error("Can't find object ~p. Used as INDEX in table.",
		       [ColumnName],Line)
    end.

test_index_positions(Line, Indexes, ColMEs) ->
    IsTLI = fun(IndexName) -> is_table_local_index(IndexName, ColMEs) end, 
    TLI   = lists:filter(IsTLI, Indexes),
    test_index_positions_impl(Line, TLI, ColMEs).

%% An table that augments another cannot conatin any index, 
%% so the first non-index column is always the first column.
augments_first_non_index_column([]) ->
    none;
augments_first_non_index_column([#me{oid=Col}|_ColMEs]) ->
    Col.

%% Returns the first non-index column | none
test_index_positions_impl(_Line, [], []) -> none;
test_index_positions_impl(_Line, [], [#me{oid=Col}|_ColMEs]) ->
    Col;
test_index_positions_impl(Line, Indexes,
			  [#me{aliasname = Name, 
			       asn1_type = Asn1} | ColMEs]) ->
    case lists:member(Name, Indexes) of
	true ->
	    if 
		Asn1#asn1_type.bertype =:= 'BITS' ->
		    print_error("Invalid data type 'BITS' for index '~w'.",
				[Name],Line);
	       true -> true
	    end,
	    test_index_positions_impl(Line,
					  lists:delete(Name, Indexes), ColMEs);
	false -> 
	    ?vwarning2("Index columns must be first for "
		       "the default functions to work properly. "
		       "~w is no index column.", [Name], Line),
	    none
    end.

is_table_local_index(IndexName, ColMEs) ->
    case lists:keysearch(IndexName, #me.aliasname, ColMEs) of
	false -> false;
	_Q -> true
    end.

non_implied_name({implied, IndexColumnName}) -> IndexColumnName;
non_implied_name(IndexColumnName) -> IndexColumnName.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for generationg the final mib
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% returns: {ok, 
%  {snmp_mib, MEs, traps, list of {TrapOid, list of oids (objects)}}}
get_final_mib(Name, Options) ->
    ?vdebug("get_final_mib -> entry", []),
    CDATA = get(cdata),
    #cdata{mes        = MEs,
	   mibfuncs   = MibFuncs,
	   asn1_types = Types,
	   traps      = Traps0,
	   oid_ets    = OidEts} = CDATA,

    ?vdebug("get_final_mib -> resolve oids", []),
    resolve_oids(OidEts),
    %% Reverse so that we get report on objects earlier in the file
    %% before later objects.
    UMEs = update_me_oids(lists:reverse(MEs), OidEts, []),
    ?vtrace("get_final_mib -> "
	    "~n   UMEs: ~p", [UMEs]),

    Traps1 = update_trap_objects(Traps0, MEs, []), 
    Traps2 = update_trap_oids(Traps1, OidEts, []),
    ?vtrace("get_final_mib -> "
	    "~n   Traps2: ~p", [Traps2]),

    SortedMEs = lists:keysort(#me.oid,UMEs),
    ?vdebug("get_final_mib -> search for dublettes", []),
    search_for_dublettes(#me{aliasname=dummy_init}, SortedMEs),

    ?vdebug("get_final_mib -> search for oid conflicts", []),
    search_for_oid_conflicts(Traps2, SortedMEs),

    ?vdebug("get_final_mib -> resolve oid", []),
    %% FIXME: use list comprehension instead
    MibFs = lists:keysort(1,
      lists:zf(fun({module, _Mod}) -> false;
                  (MF) -> {true, resolve_oid(MF,SortedMEs)}
               end, MibFuncs)),
    ?vtrace("get_final_mib -> "
	    "~n   MibFs: ~p", [MibFs]),
    {value, DBName} = snmpc_misc:assq(db, Options),
    Module = key1search(module, MibFuncs, undefined),
    MEsWithMFA = insert_mfa(MibFs, SortedMEs, DBName, Module),
    Misc = [{snmp_version,get(snmp_version)}
	    | case lists:member(no_symbolic_info,Options) of
		  true -> [no_symbolic_info];
		  false -> []
	      end],
    {value, GroupBool} = snmpc_misc:assq(group_check, Options),
    case GroupBool of
	true ->
	    case get(snmp_version) =:= 2 of
		true ->
		    ?vdebug("get_final_mib -> check object groups:"
			    "~n   ~p", [CDATA#cdata.objectgroups]),
		    check_group(CDATA#cdata.mes, 
				CDATA#cdata.objectgroups),
		    ?vdebug("get_final_mib -> check notifications group:"
			    "~n   ~p", [CDATA#cdata.notificationgroups]),
		    check_notification(Traps2,
				       CDATA#cdata.notificationgroups);
		false ->
		    ok
	    end;
	false ->
	    ok
    end,
    MI = module_identity(CDATA),
    Mib = #mib{name            = Name, 
	       misc            = Misc,
	       module_identity = MI,
	       mes            = lists:map(fun(ME) -> translate_me_type(ME) end,
					  MEsWithMFA), 
	       variable_infos = extract_variable_infos(MEsWithMFA),
	       table_infos    = extract_table_infos(MEsWithMFA),
	       traps          = lists:map(fun(T) -> translate_trap_type(T) end,
					  Traps2), 
	       asn1_types     = lists:map(fun(T) -> translate_type(T) end,
					  Types),
	       imports        = CDATA#cdata.imports},
    ?vdebug("get_final_mib -> done", []),
    {ok, Mib}.
	      

module_identity(#cdata{module_identity = MI}) ->
    case lists:member(module_identity, get(options)) of
	true ->
	    MI;
	false ->
	    undefined
    end.


update_trap_objects([], _MEs, Acc) ->
    ?vtrace("update_trap_objects -> done", []),
    lists:reverse(Acc);
update_trap_objects([#trap{trapname   = Name, 
			   oidobjects = Variables} = Trap|Traps], MEs, Acc) ->
    ?vtrace("update_trap_objects -> update objects for trap ~p:"
	    "~n   ~p", [Name, Variables]),
    OidObjects = 
	[trap_variable_info(Var, "trap", MEs) || Var <- Variables],
    UpdTrap = Trap#trap{oidobjects = OidObjects},
    update_trap_objects(Traps, MEs, [UpdTrap|Acc]);
update_trap_objects([#notification{trapname   = Name, 
				   oidobjects = Variables} = Notif|Traps], 
		    MEs, Acc) ->
    ?vtrace("update_trap_objects -> update objects for notification ~p:"
	    "~n   ~p", [Name, Variables]),
    OidObjects = 
	[trap_variable_info(Var, "notification", MEs) || Var <- Variables],
    UpdNotif = Notif#notification{oidobjects = OidObjects},
    update_trap_objects(Traps, MEs, [UpdNotif|Acc]);
update_trap_objects([_|Traps], MEs, Acc) ->
    update_trap_objects(Traps, MEs, Acc).


%% We don't want a zillion aliases for INTEGER (etc),
%% and since they are encoded with the same tag we can treat them as
%% equivalent.
%% The reason for having them at compile time is for example that
%% Unsigned32 is allowed as INDEX but not Gauge.
%% The compiler might want to ensure this and more...
translate_me_type(ME) ->
    ME#me{asn1_type = translate_type(ME#me.asn1_type)}.

translate_trap_type(Trap) when is_record(Trap, notification) ->
    translate_trap_type_notif(Trap);
translate_trap_type(Trap) when is_record(Trap, trap) ->
    translate_trap_type_trap(Trap).

translate_trap_type_notif(Trap)->
    NewOidobjects = 
	lists:map(fun({Oid,ASN1type}) ->{Oid,translate_type(ASN1type)} end,
		  Trap#notification.oidobjects),
    Trap#notification{oidobjects=NewOidobjects}.

translate_trap_type_trap(Trap)->
    NewOidobjects = 
	lists:map(fun({Oid,ASN1type}) ->
			  {Oid, translate_type(ASN1type)} 
		  end,
		  Trap#trap.oidobjects),
    Trap#trap{oidobjects = NewOidobjects}.
    
translate_type(ASN1type) when ASN1type#asn1_type.bertype =:= 'NetworkAddress' ->
    ASN1type#asn1_type{bertype = 'IpAddress'};
translate_type(ASN1type) when ASN1type#asn1_type.bertype =:= 'Integer32' ->
    ASN1type#asn1_type{bertype = 'INTEGER'};
translate_type(ASN1type) when ASN1type#asn1_type.bertype =:= 'Counter' ->
    ASN1type#asn1_type{bertype = 'Counter32'};
translate_type(ASN1type) when ASN1type#asn1_type.bertype =:= 'Gauge' ->
    ASN1type#asn1_type{bertype = 'Unsigned32'};
translate_type(ASN1type) when ASN1type#asn1_type.bertype =:= 'Gauge32' ->
    ASN1type#asn1_type{bertype = 'Unsigned32'};
translate_type(ASN1type) -> ASN1type.

%% Check for both NOTIFICATION-GROUP and OBJECT-GROUP

check_notification_group(Name, GroupObjects, Line, Status) ->
    #cdata{traps = Traps, status_ets = Ets} = get(cdata),
    Objects = get_notification_names(Traps),
    check_def(notification, Name, Line, Status, GroupObjects, Objects, Ets).

get_notification_names(Traps) when is_list(Traps) ->
    [Name || #notification{trapname = Name} <- Traps].

check_object_group(Name, GroupObjects, Line, Status) ->
    #cdata{mes = MEs, status_ets = Ets} = get(cdata),
    Objects = get_object_names(MEs),
    check_def(object, Name, Line, Status, GroupObjects, Objects, Ets).

get_object_names([])->[];
get_object_names([#me{access=A, entrytype=T, aliasname=N}|MEs]) 
  when (A =/= 'not-accessible') andalso (T =/= 'internal') ->
    [N|get_object_names(MEs)];
get_object_names([_ME|Rest]) ->
    get_object_names(Rest).

%% Strictly we should not need to check more then the status 
%% table, but since error do happen...
check_def(Type, Name, Line, Status, [GroupObject|GroupObjects], Objects, Ets) ->
    ?vdebug2("check definition of ~p [~p]: presumed member of ~p [~p]",
	     [GroupObject, Type, Name, Status], Line),
    case lists:member(GroupObject, Objects) of
	true ->
	    ?vtrace("~p is a member of ~p", [GroupObject, Name]),
	    %% Lucky so far, now lets check that the status is valid
	    case ets:lookup(Ets, GroupObject) of
		[{GroupObject, ObjectStatus}] ->
		    ?vtrace("check that the object status (~p) is valid", 
			    [ObjectStatus]),
		    check_group_member_status(Name, Status,
					      GroupObject, ObjectStatus);
		_ ->
		    print_error("group (~w) member ~w not found"
				" in status table - status check failed", 
				[Name, GroupObject])
	    end;
	false ->
	    %% Ok, this could be because the status is obsolete or
	    %% deprecated (with the deprecated flag = false)
	    ?vtrace("~p is not a member of ~p "
		    "[object status could be obsolete]", 
		    [GroupObject, Name]),
	    case ets:lookup(Ets, GroupObject) of
		[{GroupObject, ObjectStatus}] ->
		    ?vtrace("check that the object status (~p) is valid", 
			    [ObjectStatus]),
		    check_group_member_status(Name, Status,
					      GroupObject, ObjectStatus);
		_ ->
		    group_member_error(Type, GroupObject, Line)
	    end
    end,
    check_def(Type, Name, Line, Status, GroupObjects, Objects, Ets);
check_def(_, _, _, _, [], _, _) ->
    ok.

group_member_error(object, Name, Line) ->
    print_error("OBJECT-TYPE definition missing or "
		"'not-accessible' for '~w'", [Name],Line);
group_member_error(notification, Name, Line) ->
    print_error("NOTIFICATION-TYPE definition missing for '~w'", 
		[Name], Line).
		

check_group_member_status(_GroupName, _GroupStatus, _Member, undefined) ->
    ok;
check_group_member_status(_GroupName, current, _Member, current) ->
    ok;
check_group_member_status(GroupName, current, Member, MemberStatus) ->
    group_member_status_error(GroupName, current, Member, MemberStatus,
			      "current");
check_group_member_status(_GroupName, deprecated, _Member, MemberStatus) 
  when (MemberStatus =:= deprecated) orelse (MemberStatus =:= current) ->
    ok;
check_group_member_status(GroupName, deprecated, Member, MemberStatus) ->
    group_member_status_error(GroupName, deprecated, Member, MemberStatus,
			      "deprecated or current");
check_group_member_status(_GroupName, obsolete, _Member, MemberStatus) 
  when (MemberStatus =:= obsolete) orelse
       (MemberStatus =:= deprecated) orelse
       (MemberStatus =:= current) ->
    ok;
check_group_member_status(GroupName, obsolete, Member, MemberStatus) ->
    group_member_status_error(GroupName, obsolete, Member, MemberStatus,
			      "obsolete, deprecated or current");
check_group_member_status(_GroupName, _GroupStatus, _Member, _MemberStatus) ->
    ok.

group_member_status_error(Name, Status, Member, MemberStatus, Expected) ->
    snmpc_lib:print_error("Invalid status of group member ~p "
			  "in group ~p. "
			  "Group status is ~p "
			  "and member status is ~p "
			  "(should be ~s)", 
			  [Member, Name, Status, MemberStatus, Expected]).



% check_def(Objects,[GroupObject|GroupObjects],Line)->
%     case lists:member(GroupObject,Objects) of
% 	false ->
% 	    print_error("OBJECT-TYPE definition missing or "
% 			"'not-accessible' for '~w'", [GroupObject],Line),
% 	    check_def(Objects,GroupObjects,Line);
% 	true ->
% 	    check_def(Objects,GroupObjects,Line)
%     end;
% check_def(_Objects,[],_Line) ->
%     ok.

%% Checking the definition of OBJECT-GROUP

%%-----------------------------
check_group([#me{imported = true} | T],GroupObjects)->
    ?vtrace("check_group(imported) -> skip", []),
    check_group(T,GroupObjects);
check_group([],_GroupObjects) ->
    ?vtrace("check_group -> done", []),
    ok;
check_group([#me{access    = A, 
		 entrytype = T, 
		 aliasname = N}|MEs], GroupObjects) 
  when ((A =/= 'not-accessible') andalso 
	(T =/= 'internal') andalso 
	(T =/= group)) ->
    ?vtrace("check_group -> "
	    "~n   access:    ~p"
	    "~n   entrytype: ~p"
	    "~n   aliasname: ~p", [A, T, N]),
    check_member_group(N, GroupObjects),
    check_group(MEs,GroupObjects);
check_group([_|MEs],GroupObjects) ->
    check_group(MEs,GroupObjects).

check_member_group(Aliasname, [])->
    print_error("'~w' missing in OBJECT-GROUP",[Aliasname]);
check_member_group(Aliasname, [{Name,GroupObject,Line}|Tl])->
    ?vtrace2("check_member_group -> entry with"
	     "~n   Aliasname:   ~p"
	     "~n   Name:        ~p"
	     "~n   GroupObject: ~p", [Aliasname, Name, GroupObject], Line),
    case lists:member(Aliasname,GroupObject) of
	true ->
	    ok;
	false ->
	    check_member_group(Aliasname,Tl)
    end.                                     


%% Checking definition in NOTIFICATION-GROUP

%%--------------------------
check_notification([],_NotificationObjects) ->
    ok;
check_notification([#notification{trapname=Aliasname}|Traps],
		   NotificationObjects) ->
    check_member_notification(Aliasname, NotificationObjects),
    check_notification(Traps,NotificationObjects);
check_notification([_|Traps],NotificationObjects) ->
    check_notification(Traps,NotificationObjects).

check_member_notification(Aliasname,[])->
    print_error("'~w' missing in NOTIFICATION-GROUP",[Aliasname]);
check_member_notification(Aliasname,[{_Name,NotificationObject,_Line}|Tl]) ->
    case lists:member(Aliasname, NotificationObject) of
	true ->
	    ok;
	false ->
	    check_member_notification(Aliasname,Tl)
    end.                               


%%----------------------------------------------------------------------
%% Purpose: Resolves oids for aliasnames used in .funcs file.
%% Returns: {Oid, X}
%%----------------------------------------------------------------------
resolve_oid({NameOrOid, X}, MEs) ->
    case lookup(NameOrOid, MEs) of
	{value, #me{oid=Oid,entrytype=variable}} -> {Oid, X};
	{value, #me{oid=Oid,entrytype=table}} -> {Oid, X};
	{value, #me{entrytype=table_entry}} ->
		error("Cannot associate an instrumentation function with a "
		      "Table Entry: ~w (must be table or variable)",
		      [NameOrOid]);
	{value, #me{entrytype=table_column}} ->
		error("Cannot associate an instrumentation function with a "
		      "Table Column: ~w (must be table or variable)",
		      [NameOrOid]);
	_Q ->
	    error("Cannot find OBJECT-TYPE definition for '~w'.",
		  [NameOrOid])
    end.

%%----------------------------------------------------------------------
%% Fs is list of {Oid, {M,F,A}}
%% returns: MEs with access-functions.
%% Pre: Fs, MEs are sorted (on Oid) (then we can traverse mib efficiently)
%%----------------------------------------------------------------------
insert_mfa(Fs, [ME | MEs], DBName, Mod) 
  when ME#me.imported =:= true ->
    [ME | insert_mfa(Fs, MEs, DBName, Mod)];

insert_mfa(Fs, [ME | MEs], DBName, Mod) 
  when ME#me.entrytype =:= internal ->
    [ME | insert_mfa(Fs, MEs, DBName, Mod)];

insert_mfa(Fs, [ME|MEs], DBName, Mod) 
  when ME#me.entrytype =:= group ->
    [ME | insert_mfa(Fs, MEs, DBName, Mod)];

insert_mfa([X | Fs], [ME | MEs], DBName, Mod) 
  when ME#me.entrytype =:= variable ->
    {Oid, {M,F,A}} = X,
    case ME#me.oid of
	Oid ->
	    [ME#me{mfa = {M,F,A}} | insert_mfa(Fs, MEs, DBName, Mod)];
	_Q -> 
	    [insert_default_mfa(ME, DBName, Mod) |
	     insert_mfa([X | Fs], MEs, DBName, Mod)]
    end;

insert_mfa([X | Fs], [TableME | MEs], DBName, Mod) 
  when TableME#me.entrytype =:= table ->
    {Oid, {M,F,A}} = X,
    {TableMEs, RestMEs} = collect_mes_for_table(TableME, [TableME | MEs]),
    [TableEntryME | ColMEs] = tl(TableMEs),
    DefVals = get_defvals(ColMEs),
    {value,TableInfo} = snmpc_misc:assq(table_info,TableME#me.assocList),
    NAssocList = [{table_info, TableInfo#table_info{defvals = DefVals}} |
		  lists:keydelete(table_info, 1, TableME#me.assocList)],
    NTableME = TableME#me{assocList = NAssocList},
    case is_same_table(Oid, NTableME#me.oid) of
	true ->  % use mfa from .funcs
	    lists:append([NTableME,
			  TableEntryME#me{mfa = {M, F, A}}
			  | ColMEs],
			 insert_mfa(Fs, RestMEs, DBName, Mod));
	false ->
	    lists:append(insert_default_mfa([NTableME | tl(TableMEs)], 
					    DBName, Mod),
			 insert_mfa([X|Fs], RestMEs, DBName, Mod))
    end;

insert_mfa([], [ME|MEs], DBName, Mod) 
  when ME#me.entrytype =:= variable ->
    [insert_default_mfa(ME, DBName, Mod) | insert_mfa([], MEs, DBName, Mod)];

insert_mfa([], [ME|MEs], DBName, Mod) 
  when ME#me.entrytype =:= table ->
    {TableMEs, RestMEs} = collect_mes_for_table(ME, [ME|MEs]),
    [TableME, _TableEntryME | ColMEs] = TableMEs,
    DefVals = get_defvals(ColMEs),
    {value,TableInfo} = snmpc_misc:assq(table_info,TableME#me.assocList),
    NAssocList = [{table_info, TableInfo#table_info{defvals = DefVals}} |
		  lists:keydelete(table_info, 1, TableME#me.assocList)],
    NTableME = TableME#me{assocList = NAssocList},
    NewTableMEs = insert_default_mfa([NTableME | tl(TableMEs)], DBName, Mod),
    lists:append(NewTableMEs, insert_mfa([], RestMEs, DBName, Mod));

insert_mfa([], [], _DBName, _Mod) -> 
    [];
insert_mfa([], [ME|_MEs], _DBName, _Mod) ->
    error("Missing access-functions for '~w'.",[ME#me.aliasname]).

%%----------------------------------------------------------------------
%% Returns: {[TableME, TableEntryME | ColumnMEs], RestMEs}
%%----------------------------------------------------------------------
collect_mes_for_table(_TableME, []) -> 
    {[], []};

collect_mes_for_table(TableME, [ME|MEs]) ->
    case is_same_table(TableME#me.oid, ME#me.oid) of
	true ->
	    {TableMEs, RestMEs} = collect_mes_for_table(TableME, MEs),
	    {[ME | TableMEs], RestMEs};
	false ->
	    {[], [ME | MEs]}
    end.

%% returns: MibEntry with access-functions.
insert_default_mfa(ME, DBName, undefined) when is_record(ME, me)->
    case lists:member(no_defs, get(options)) of
        true ->
            error("Missing access function for ~s", [ME#me.aliasname]);
        false ->
	    ?vinfo("No accessfunction for '~w' => using default", 
		   [ME#me.aliasname]),
	    set_default_function(ME, DBName)
    end;

insert_default_mfa(ME, _DBName, Mod) when is_record(ME, me)->
    ME#me{mfa = {Mod, ME#me.aliasname, []}};

insert_default_mfa([TableME, EntryME | Columns], DBName, undefined) ->
    case lists:member(no_defs, get(options)) of
        true ->
            error("Missing access function for ~s", [TableME#me.aliasname]);
        false ->
	    ?vinfo("No accessfunction for '~w' => using default", 
		   [TableME#me.aliasname]),
	    set_default_function([TableME, EntryME | Columns], DBName)
    end;

insert_default_mfa([TableME, EntryME | Columns], _DBName, Mod) ->
    [TableME,
     EntryME#me{mfa = {Mod, TableME#me.aliasname, []}} |
     Columns].


%% returns boolean.
is_same_table(Oid, TableOid) ->
    lists:prefix(Oid, TableOid).

%% returns false | {value, ME}
lookup(UniqName, MEs) when is_atom(UniqName) ->
    lists:keysearch(UniqName, #me.aliasname, MEs);
lookup(Oid, MEs) when is_list(Oid) ->
    lists:keysearch(Oid, #me.oid, MEs).

search_for_dublettes(PrevME, [ME|_MEs])
            when PrevME#me.oid =:= ME#me.oid ->
    error("Multiple used object with OBJECT IDENTIFIER '~w'. "
	  "Used in '~w' and '~w'.", [PrevME#me.oid,
				     PrevME#me.aliasname,
				     ME#me.aliasname]);
search_for_dublettes(PrevME, [ME|MEs]) 
       when ((PrevME#me.entrytype =:= variable) andalso 
	     (ME#me.entrytype =:= variable)) ->
    case lists:prefix(PrevME#me.oid, ME#me.oid) of
	true ->
	    error("Variable '~w' (~w) defined below other "
		  "variable '~w' (~w). ",
		  [ME#me.aliasname, ME#me.oid,
		   PrevME#me.aliasname, PrevME#me.oid]);
	false ->
	    search_for_dublettes(ME, MEs)
    end;
search_for_dublettes(_PrevME, [ME|MEs]) ->
    search_for_dublettes(ME, MEs);
search_for_dublettes(_PrevME, []) -> 
    ok.


search_for_oid_conflicts([Rec|Traps],MEs) when is_record(Rec,notification) ->
    #notification{oid = Oid, trapname = Name} = Rec,
    case search_for_oid_conflicts1(Oid,MEs) of
	{error,ME} ->
	    error("Notification with OBJECT IDENTIFIER '~w'. "
		  "Used in '~w' and '~w'.", [Oid,Name,ME#me.aliasname]);
	ok ->
	    search_for_oid_conflicts(Traps,MEs)
    end;
search_for_oid_conflicts([_Trap|Traps],MEs) ->
    search_for_oid_conflicts(Traps,MEs);
search_for_oid_conflicts([],_MEs) ->
    ok.

search_for_oid_conflicts1(_Oid,[]) ->
    ok;
search_for_oid_conflicts1(Oid,[ME|_MEs]) when Oid == ME#me.oid ->
    {error,ME};
search_for_oid_conflicts1(Oid,[_ME|MEs]) ->
    search_for_oid_conflicts1(Oid,MEs).

set_default_function([TableMe, EntryMe | ColMes], DBName) ->
    #me{aliasname = Name} = TableMe,
    check_rowstatus(TableMe),
    [TableMe,
     EntryMe#me{mfa = {snmp_generic, table_func, [{Name, DBName}]}} |
     ColMes];

set_default_function(MibEntry,DBName) when MibEntry#me.entrytype == variable ->
    #me{aliasname = Aname} = MibEntry,
    MibEntry#me{mfa = {snmp_generic, variable_func, [{Aname, DBName}]}}.

check_rowstatus(TableME) ->
    {value,TableInfo} = snmpc_misc:assq(table_info,TableME#me.assocList),
    case TableInfo#table_info.status_col of
	undefined -> 
        ?vwarning("No RowStatus column in table ~w => "
		  "The default functions won't work properly",
		  [TableME#me.aliasname]);
	_Q -> ok
    end.

check_trap(#trap{trapname=N1, specificcode=C, enterpriseoid=E},
	   #trap{trapname=N2, specificcode=C, enterpriseoid=E},Line) ->
    print_error("Trap code collision. Enterprise: ~w. Trapcode: ~w, "
		"Name of traps: ~w, ~w.", [E, C, N1, N2],Line),
    error;
check_trap(#trap{trapname=N, specificcode=C1, enterpriseoid=E1},
	   #trap{trapname=N, specificcode=C2, enterpriseoid=E2},Line) ->
    print_error("Trap name collision. Name: ~w Enterprises: ~w, ~w. "
		"Trapcodes: ~w, ~w", [N, E1, E2, C1, C2],Line),
    error;
check_trap(_OldTrap, _ThisTrap, _Line) ->
    ok.

check_notification(Notif, Line, Notifs) ->
    lists:map(fun (OtherNotif) ->
		      check_notification1(Notifs,OtherNotif,Line) 
	      end, lists:delete(Notif,Notifs)).

check_notification1(#notification{trapname=N},#notification{trapname=N},Line)->
    print_error("Trap name collision for '~w.",[N],Line);
check_notification1(#notification{oid=Oid},#notification{oid=Oid},Line)->
    print_error("Trap oid collision for '~w.",[Oid],Line);
check_notification1(_T1, _T2, _L) ->
    ok.

%%----------------------------------------------------------------------
%% Returns: list of {VariableName, variable_info-record}
%%----------------------------------------------------------------------
extract_variable_infos([]) -> [];
extract_variable_infos([#me{entrytype = variable, 
			    assocList = AL,
			    aliasname = Name} | T]) ->
    {value, VI} = snmpc_misc:assq(variable_info, AL),
    [{Name, VI} | extract_variable_infos(T)];
extract_variable_infos([_ME | T]) ->
    extract_variable_infos(T).

%%----------------------------------------------------------------------
%% Returns: list of {TableName, table_info-record}
%%----------------------------------------------------------------------
extract_table_infos([]) -> [];
extract_table_infos([#me{entrytype = table, 
			 assocList = AL,
			 aliasname = Name} | T]) ->
    {value, VI} = snmpc_misc:assq(table_info, AL),
    [{Name, VI} | extract_table_infos(T)];
extract_table_infos([_ME | T]) ->
    extract_table_infos(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for misc useful functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% m(M) -> {?MODULE, M}.

set_dir(File, NewDir) ->
    case string:chr(File, $/) of
	0 -> lists:append(NewDir, File);
	N -> set_dir(lists:nthtail(N,File), NewDir)
    end.

%% Look up a key in a list, and if found returns the value
%% or if not found returns the default value
key1search(Key, List) ->
    key1search(Key, List, undefined).

key1search(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        {value, {Key, Val}} -> Val;
        _ -> Default
    end.


%% print the compiled mib
look_at(FileName) ->
    case file:read_file(FileName) of
        {ok,Bin} -> 
	    binary_to_term(Bin);     
        {error,Reason} ->
            {error,Reason}
    end.


%% Data is appended to compiler information
add_cdata(OffsetInRecord, ListOfData) ->
    CDATA = get(cdata),
    OldData = element(OffsetInRecord, CDATA),
    put(cdata, setelement(OffsetInRecord, CDATA, lists:append(ListOfData,
							      OldData))),
    undefined.

check_sub_ids([H | _T], Name, Line) when H < 0 ->
    error("OBJECT IDENTIFIER must have all sub "
	  "indexes > 0. Name: '~w'. Illegal sub index: ~w.",
	  [Name, H], Line);
check_sub_ids([H | _T], Name, Line) when H > 4294967295 ->
    error("OBJECT IDENTIFIER must have all sub "
	  "indexes < 4294967295. Name: '~w'. Illegal sub index: ~w.",
	  [Name, H], Line);
check_sub_ids([_H | T], Name, Line) ->
    check_sub_ids(T, Name, Line);
check_sub_ids([], _Name, _Line) ->
    ok.


%%-----------------------------------------------------------------
%% Handle forward references:
%% This code handles OIDs that are defined in terms of a
%% parent OID that is defined later in the file.  Ex:
%%   x OBJECT IDENTIFIER ::= {y 1}
%%   y OBJECT IDENTIFIER ::= {enterprises 1}
%% The following alg is used to handle this:
%% Datastructure:
%%    An ets table, with one entry for each object in the mib:
%%    {Name, FatherName, Line, SubIndex, Children}
%%       Name : aliasname
%%       FatherName : aliasname of parent object
%%       SubIndex : list of subindexes from parent object
%%       Children : list of aliasnames for all objects registered
%%                  under this one
%%    FatherName == 'root' => top-level object
%% 1) When an OID is found in the mib, it is registered using
%%    register_oid/4.  This function updates the parent entry,
%%    by adding the new name to its Children list.  It also
%%    updates the entry for the object defined.
%% 2) When all objects are registered, the ets table contains
%%    a directed graph of all objects.
%% 3) resolve_oids/1 is called.  This function traverses the
%%    graph, starting at 'root', and changes each entry to
%%    {Name, Line, Oid}, where Oid is a list of integers.
%% 4) The list of MibEntries is traversed.  Each object is
%%    looked up in the ets table, and the correspsonding oid
%%    is updated.  The functions for this is update_me_oids/2
%%    and update_trap_oids/2.
%%-----------------------------------------------------------------
register_oid(Line, Name, ParentName, SubIndex) when Name =/= '$no_name$' ->
    ?vtrace2("register_oid -> entry with"
      "~n   Name:       ~p"
      "~n   ParentName: ~p"
      "~n   SubIndex:   ~p", [Name, ParentName, SubIndex], Line),
    check_sub_ids(SubIndex, Name, Line),
    OidEts = (get(cdata))#cdata.oid_ets,
    %% Lookup Parent - if it doesn't already exist, create it
    {_ParentName, ItsParentName, ItsLine, ItsSubIndex, Children} = 
	case ets:lookup(OidEts, ParentName) of
	    [Found] -> 
		?vtrace("register_oid -> parent found: "
			"~n   ~p", [Found]),
		Found;
	    [] -> 
		?vtrace("register_oid -> father not found: "
			"~n   ~p", [ets:tab2list(OidEts)]),
		{ParentName, undef, undef, [], []}
	end,
    %% Update Father with a pointer to us
    NChildren = case lists:member(Name, Children) of
		    true -> Children;
		    false -> [Name | Children]
		end,
    NParent = {ParentName, ItsParentName, ItsLine, ItsSubIndex, NChildren},
    ?vtrace("register_oid -> NParent: ~n~p", [NParent]),
    ets:insert(OidEts, NParent),
    %% Lookup ourselves - if we don't exist, create us
    MyChildren =
	case ets:lookup(OidEts, Name) of
	    [Found2] ->
		?vtrace("register_oid -> children found: "
			"~n   ~p", [Found2]),
		element(5, Found2);
	    [] -> 
		?vtrace("register_oid -> no children found", []),
		[]
	end,
    %% Update ourselves
    NSelf = {Name, ParentName, Line, SubIndex, MyChildren},
    ?vtrace("register_oid -> NSelf: "
	    "~n   ~p", [NSelf]),
    ets:insert(OidEts, NSelf);
register_oid(_Line, _Name, _ParentName, _SubIndex) ->
    ok.


resolve_oids(OidEts) ->
    case ets:lookup(OidEts, root) of
	[{_, _, _, _, RootChildren}] ->
	    resolve_oids(RootChildren, [], OidEts);
	[] ->
	    ok
    end.

resolve_oids([Name | T], FatherOid, OidEts) ->
    {MyOid, MyChildren, MyLine} =
	case ets:lookup(OidEts, Name) of
	    [{Name, Oid, Line}] ->
		print_error("Circular OBJECT IDENTIFIER definitions "
			    "involving ~w\n", [Name], Line),
		{Oid, [], Line};
	    [{Name, _Father, Line, SubIndex, Children}] ->
		{FatherOid ++ SubIndex, Children, Line}
	end,
    ets:insert(OidEts, {Name, MyOid, MyLine}),
    resolve_oids(T, FatherOid, OidEts),
    resolve_oids(MyChildren, MyOid, OidEts);
resolve_oids([], _, _) ->
    ok.
    
		 

update_me_oids([], _OidEts, Acc) ->
    lists:reverse(Acc);
update_me_oids([#me{aliasname = '$no_name$'} | Mes], OidEts, Acc) ->
    update_me_oids(Mes, OidEts, Acc);
update_me_oids([Me | Mes], OidEts, Acc) ->
    ?vtrace("update_me_oids -> entry with"
	    "~n   Me: ~p", [Me]),
    Oid = tr_oid(Me#me.aliasname, OidEts),
    NMe = resolve_oid_defval(Me, OidEts),
    update_me_oids(Mes, OidEts, [NMe#me{oid = Oid} | Acc]).

update_trap_oids([], _OidEts, Acc) ->
    lists:reverse(Acc);
update_trap_oids([#trap{enterpriseoid = EOid, 
			oidobjects    = OidObjs} = Trap | Traps], 
		 OidEts, Acc) ->
    ?vtrace("update_trap_oids -> entry with"
	    "~n   EOid: ~p", [EOid]),
    NEnter   = tr_oid(EOid, OidEts),
    NOidObjs = tr_oid_objs(OidObjs, OidEts),
    NTrap = Trap#trap{enterpriseoid = NEnter,
		      oidobjects    = NOidObjs},
    update_trap_oids(Traps, OidEts, [NTrap|Acc]);
update_trap_oids([#notification{trapname   = Name,
				oidobjects = OidObjs} = Notif | Traps], 
		 OidEts, Acc) ->
    ?vtrace("update_trap_oids -> entry with"
	    "~n   Name: ~p", [Name]),
    Oid      = tr_oid(Name, OidEts),
    NOidObjs = tr_oid_objs(OidObjs, OidEts),
    NNotif   = Notif#notification{oid = Oid, oidobjects = NOidObjs},
    update_trap_oids(Traps, OidEts, [NNotif|Acc]).

tr_oid(Name, OidEts) ->
    ?vtrace("tr_oid -> entry with"
	    "~n   Name: ~p", [Name]),
    case ets:lookup(OidEts, Name) of
	[{Name, MyOid, _MyLine}] ->
	    MyOid;
	[{_Natrap, Parent, Line, SubIndex, _Children}] ->
	    print_error("OBJECT IDENTIFIER [~w] defined in terms "
			"of undefined parent object. Parent: '~w'."
			"(Sub-indexes: ~w.)",
			[Name, Parent, SubIndex], Line),
	    ?vtrace("tr_oid -> ets:tab2list(~w): "
		    "~n   ~p", [OidEts, ets:tab2list(OidEts)]),
	    rnd_oid()
    end.

tr_oid_objs([{{variable, Name}, Type} | T], OidEts) ->
    ?vtrace("tr_oid_objs(variable) -> entry with"
	    "~n   Name: ~p", [Name]),
    Oid = tr_oid(Name, OidEts) ++ [0],
    [{Oid, Type} | tr_oid_objs(T, OidEts)];
tr_oid_objs([{{column, Name}, Type} | T], OidEts) ->
    ?vtrace("tr_oid_objs(column) -> entry with"
	    "~n   Name: ~p", [Name]),
    Oid = tr_oid(Name, OidEts),
    [{Oid, Type} | tr_oid_objs(T, OidEts)];
tr_oid_objs([], _OidEts) ->
    [].
    

resolve_oid_defval(ME, OidEts) 
  when (ME#me.asn1_type)#asn1_type.bertype == 'OBJECT IDENTIFIER' ->
    #me{aliasname = MEName, assocList = AssocList} = ME,
    case snmpc_misc:assq(defval, AssocList) of
	{value, DefVal} when is_atom(DefVal) ->
	    case ets:lookup(OidEts, DefVal) of
		[{_, Oid, _}] ->
		    ME#me{assocList = lists:keyreplace(defval, 1, AssocList,
						       {defval, Oid})};
		_ ->
		    print_error("Can not find OBJECT-TYPE definition for '~w' "
				"Used in DEFVAL for '~w'.", [DefVal, MEName]),
		    ME
	    end;
	_ ->
	    ME
    end;
resolve_oid_defval(ME, _OidEts) ->
    ME.

rnd_oid() ->
    [99,99].  %% '99' means "stop computer" in Y2Kish...

error(FormatStr, Data) when is_list(FormatStr) ->
    print_error(FormatStr,Data),
    exit(error).

error(FormatStr, Data, Line) when is_list(FormatStr) ->
    print_error(FormatStr,Data,Line),
    exit(error).

print_error(FormatStr, Data) when is_list(FormatStr) ->
    ok = io:format("~s: " ++ FormatStr,[get(filename)|Data]),
    put(errors,yes),
    io:format("~n").
    
print_error(FormatStr, Data,Line) when is_list(FormatStr) ->
    ok = io:format("~s: ~w: " ++ FormatStr,[get(filename), Line |Data]),
    put(errors,yes),
    io:format("~n").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for debug functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vprint(Severity, Mod, Line, MibLine, F, A) ->
    case printable(Severity) of
	standard when is_integer(MibLine) ->
	    io:format("[~s:~w][~s]: " ++ F ++ "~n",
		      [get(filename), MibLine, image_of_severity(Severity)|A]);
	standard ->
	    io:format("[~s][~s]: " ++ F ++ "~n",
		      [get(filename), image_of_severity(Severity)|A]);
	extended when is_integer(MibLine) ->
	    io:format("[~s:~w][~w:~w][~s]: " ++ F ++ "~n",
		      [get(filename), MibLine, Mod, Line, 
		       image_of_severity(Severity)|A]);
	extended ->
	    io:format("[~s][~w:~w][~s]: " ++ F ++ "~n",
		      [get(filename), Mod, Line,
		       image_of_severity(Severity)|A]);
	_ ->
	    ok
    end.

printable(Severity) ->	    
    printable(get(verbosity), Severity).

printable(silence, _)       -> none;
printable(warning, warning) -> standard; 
printable(info,    info)    -> standard;
printable(info,    warning) -> standard;
printable(log,     warning) -> standard;
printable(log,     info)    -> standard;
printable(log,     log)     -> standard;
printable(debug,   warning) -> standard;
printable(debug,   info)    -> standard;
printable(debug,   log)     -> standard;
printable(debug,   debug)   -> standard;
printable(trace,   _Sev)    -> extended;
printable(_Ver,    _Sev)    -> none.

-spec image_of_severity(Sev :: severity()) -> string().
image_of_severity(warning)  -> "WAR";
image_of_severity(info)     -> "INF";
image_of_severity(log)      -> "LOG";
image_of_severity(debug)    -> "DBG";
image_of_severity(trace)    -> "TRC";
image_of_severity(_)        -> " - ".


vvalidate(silence) -> ok;
vvalidate(warning) -> ok;
vvalidate(info)    -> ok;
vvalidate(log)     -> ok;
vvalidate(debug)   -> ok;
vvalidate(trace)   -> ok;
vvalidate(V)       -> exit({invalid_verbosity,V}).


