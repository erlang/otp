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
-module(ic_pragma).


-export([pragma_reg/2,pragma_cover/3]).
-export([pragma_prefix/3,pragma_version/3,pragma_id/3]).
-export([mk_alias/3,get_alias/2,scope2id/2,id2scope/2,mk_scope/1]).
-export([mk_ref/3,get_incl_refs/1,get_local_refs/1]).
-export([get_dependencies/1, add_inh_data/3, preproc/3]).
-export([getBrokerData/3,defaultBrokerData/1,list_to_term/1]).
-export([get_local_c_headers/2,get_included_c_headers/1,is_inherited_by/3]).
-export([no_doubles/1,fetchRandomLocalType/1,fetchLocalOperationNames/2]).
-export([is_local/2]).

%% Debug
-export([print_tab/1,slashify/1,is_short/1]).

-import(lists,[suffix/2,delete/2,reverse/1,keysearch/3,member/2,last/1,flatten/1]).
-import(string,[tokens/2]).
-import(ets,[insert/2,lookup/2]).

-import(ic_forms,   [get_id2/1, get_body/1, get_line/1]).
-import(ic_util,    [to_atom/1]).
-import(ic_genobj,  [idlfile/1]).
-import(ic_options, [get_opt/2]).

-include("icforms.hrl").
-include("ic.hrl").




%% Initialization of the pragma table and
%% start of pragma registration. 
%% NOTE : this pragma registration is build
%% as a separate stage under compilation.
%% If it is to be optimised, it should be 
%% embodied in one of other compiling stages. 
pragma_reg(G,X) ->
    S = ic_genobj:pragmatab(G),
    init_idlfile(G,S),
    init_pragma_status(S),
    registerOptions(G,S),
    pragma_reg_all(G, S, [], X),
    denote_specific_code_opts(G),
    case get_pragma_compilation_status(S) of
	true ->
	    %% Remove ugly pragmas from form
	    PragmaCleanForm = cleanup(X),
	    {ok,PragmaCleanForm};
	false ->
	    ErrorNr = get_pragma_error_nr(S),
	    %% Just print the number of errors found
	    case ErrorNr > 1 of
		true ->
		    io:format("There were ~p errors found on file ~p~n",
			      [ErrorNr,get_idlfile(S)]),
		    error;
		false ->
		    io:format("There were ~p error found on file ~p~n",
			      [ErrorNr,get_idlfile(S)]),
		    error
	    end 
    end.



registerOptions(G,S) ->
    OptList = ets:tab2list(ic_genobj:optiontab(G)),
    registerOptions(G,S,OptList).


registerOptions(_G,_S,[]) ->    
    true;
registerOptions(G,S,[{{option,{broker,Scope}},{Mod,Type}}|Rest]) ->
    insert(S,
	   {codeopt,
	    reverse(tokens(Scope,":")),
	    {broker,{Mod,Type}},
	    -1,
	    nil,
	    nil}),
    registerOptions(G,S,Rest);
registerOptions(G,S,[_|Rest]) ->
    registerOptions(G,S,Rest).


%% Decide if to apply pragmas
%% by checking backend switch
applyPragmasInBe(G) ->
    case get_opt(G, be) of
	erl_plain ->
	    false;
	_ ->
	    true
    end.


%% Decide if the code option directive
%% is allowed to change backend
applyCodeOpt(G) ->
    case get_opt(G, be) of
	erl_corba -> %% Does not support codeopt
	    false;
	erl_plain -> %% Does not support codeopt
	    false;
	c_native ->  %% Does not support codeopt
	    false;
	_ ->
	    true
    end.



%% This removes all pragma records from the form.
%% When debugged, it can be enbodied in pragma_reg_all.
cleanup(undefined,C) -> C;
cleanup([],C) -> C;
cleanup([X|Xs],CSF) ->
    cleanup(Xs, CSF++cleanup(X)).

cleanup(X) when is_list(X) -> cleanup(X,[]);
cleanup(X) when is_record(X, preproc) -> [X];
cleanup(X) when is_record(X, pragma) -> [];
cleanup(X) when is_record(X, op) -> % Clean inside operation parameters
    [ X#op{params = cleanup(X#op.params,[])}];

cleanup(X) when is_record(X, module) ->  % Clean inside module body
    [ X#module{body = cleanup(X#module.body,[])}];

cleanup(X) when is_record(X, interface) ->  % Clean inside interface body
    [ X#interface{body = cleanup(X#interface.body,[])}];

cleanup(X) when is_record(X, except) ->  % Clean inside exception body
    [ X#except{body = cleanup(X#except.body,[])}];

cleanup(X) when is_record(X, struct) ->  % Clean inside struct body
    [ X#struct{body = cleanup(X#struct.body,[])}];

cleanup(X) when is_record(X, case_dcl) ->  % Clean inside union body
    [ X#case_dcl{label = cleanup(X#case_dcl.label,[])}];

cleanup(X) when is_record(X, union) ->  % Clean inside union body
    [ X#union{body = cleanup(X#union.body,[])}];

cleanup(X) when is_record(X, enum) ->  % Clean inside enum body
    [ X#enum{body = cleanup(X#enum.body,[])}];

cleanup(X) -> [X].




%% pragma_reg_all is top level registration for pragmas 
pragma_reg_all(_G, _S, _N, []) -> ok;
pragma_reg_all(G, S, N, [X|Xs]) ->
    pragma_reg(G, S, N, X), 
    pragma_reg_all(G, S, N, Xs).


%% pragma_reg is top level registration for pragmas 
pragma_reg(G, S, N, X)  when is_list(X) -> pragma_reg_list(G, S, N, X);
pragma_reg(_G, S, _N, X)  when element(1, X) == preproc ->
    case X#preproc.aux of
	[{_, _, "1"}] ->
	    IncludeEntryLNr = get_line(X#preproc.id),
	    IncludeFileName = element(3,element(3,X)),
	    insert(S,{includes,get_idlfile(S),IncludeFileName,IncludeEntryLNr});
	_Other ->
	    ok
    end,
    set_idlfile(S,element(3,element(3,X)));
pragma_reg(G, S, N, X)  when element(1, X) == pragma ->
    case applyPragmasInBe(G) of

	%% Pragmas are allowed to be
	%% applied in this this backend.
	true ->

	    File = get_idlfile(S), % The current file or an included one.
	    Type = case idlfile(G) of % Local/Included flag
		       File ->
			   local;
		       _ ->
			   included
		   end,
	    
	    %% Register pragmas into pragmatab.
	    case X of
		{pragma,{_,LineNr,"prefix"}, _To, _Apply} ->
		    insert(S,{prefix,X,LineNr,N,File,Type});

		{pragma,{_,_,"ID"},_,_} ->
		    pragma_reg_ID(G, S, N, X);

		{pragma,{_,_,"version"},_,_} ->
		    pragma_reg_version(G, S, N, X );
		
		{pragma,{_,_,"CODEOPT"},_,_} ->
		    pragma_reg_codeOpt(G,S,N,X);
		
		{pragma,{_,LineNr,BadPragma}, _To, _Apply} ->
		    io:format("Warning : on file ~p :~n",[get_idlfile(S)]),
		    io:format("  Unknown pragma directive ~p on line ~p, ignored.~n",
			      [BadPragma,LineNr])
	    end;

	%% Pragmas are not to be applied in 
	%% this backend, ignore all pragmas.
	false ->
	    true
    end,
    ok;

pragma_reg(G, S, N, X) when is_record(X, module) ->
    mk_ref(G,[get_id2(X) | N],mod_ref),
    mk_file_data(G,X,N,module),
    pragma_reg_all(G, S, [get_id2(X) | N], get_body(X));

pragma_reg(G, S, N, X) when is_record(X, interface) ->
    mk_ref(G,[get_id2(X) | N],ifc_ref),
    mk_file_data(G,X,N,interface),
    pragma_reg_all(G, S, [get_id2(X) | N], get_body(X));

pragma_reg(G, S, N, X) when is_record(X, op) ->  
    %% Add operation in table
    insert(S,{op,
	      get_id2(X),
	      N,
	      get_idlfile(S),
	      get_filepath(S)}),
    mk_file_data(G,X,N,op),
    pragma_reg_all(G, S, N, X#op.params);

pragma_reg(G, S, N, X) when is_record(X, except) -> 
    mk_ref(G,[get_id2(X) | N],except_ref),
    mk_file_data(G,X,N,except),
    pragma_reg_all(G, S, N, X#except.body);

pragma_reg(G, _S, N, X) when is_record(X, const) ->  
    mk_ref(G,[get_id2(X) | N],const_ref),
    mk_file_data(G,X,N,const);

pragma_reg(G, _S, N, X) when is_record(X, typedef) ->  
    XX = #id_of{type=X},
    lists:foreach(fun(Id) ->
			  mk_ref(G,[get_id2(Id) | N],typedef_ref),
			  mk_file_data(G,XX#id_of{id=Id},N,typedef)
		  end,
		  ic_forms:get_idlist(X));

pragma_reg(G, S, N, X) when is_record(X, enum) ->  
    mk_ref(G,[get_id2(X) | N],enum_ref),
    mk_file_data(G,X,N,enum),
    pragma_reg_all(G, S, N, X#enum.body);

pragma_reg(G, S, N, X) when is_record(X, union) ->  
    mk_ref(G,[get_id2(X) | N],union_ref),
    mk_file_data(G,X,N,union),
    pragma_reg_all(G, S, N, X#union.body);

pragma_reg(G, S, N, X) when is_record(X, struct) -> 
    mk_ref(G,[get_id2(X) | N],struct_ref),
    mk_file_data(G,X,N,struct),
    case X#struct.body of
	undefined ->
	    ok;
	_ ->
	    pragma_reg_all(G, S, N, X#struct.body)
    end;

pragma_reg(G, _S, N, X) when is_record(X, attr) -> 
    XX = #id_of{type=X},
    lists:foreach(fun(Id) ->
			  mk_ref(G,[get_id2(Id) | N],attr_ref),
			  mk_file_data(G,XX#id_of{id=Id},N,attr)
		  end,
		  ic_forms:get_idlist(X));
    
pragma_reg(_G, _S, _N, _X) ->  ok.




pragma_reg_list(_G, _S, _N, []) -> ok;
pragma_reg_list(G, S, N, List ) ->
    CurrentFileName = get_idlfile(S), 
    pragma_reg_list(G, S, N, CurrentFileName, List).

pragma_reg_list(_G, _S, _N, _CFN, []) -> ok;
pragma_reg_list(G, S, N, CFN, [X | Xs]) ->
    case X of
	{preproc,_,{_,_,FileName},_} ->
	    set_idlfile(S,FileName),
	    pragma_reg(G, S, N, X),
	    pragma_reg_list(G, S, N, FileName, Xs);
	_ ->
	    pragma_reg(G, S, N, X),
	    pragma_reg_list(G, S, N, CFN, Xs)
    end.





pragma_reg_ID(G, S, N, X) ->
    {pragma,{_,LineNr,"ID"}, _To, Apply} = X,

    
    File = get_idlfile(S), % The current file or an included one.
    Type = case idlfile(G) of % Local/Included flag
	       File ->
		   local;
	       _ ->
		   included
	   end,
    
    %% Check if ID is one of the allowed types :
    %%    * OMG IDL
    %%    * DCE UUID
    %%    * LOCAL
    case tokens(element(3,Apply),":") of
	["IDL",_,_] ->
	    insert(S,{id,X,LineNr,N,File,Type});
	["DCE",_,VSN] ->
	    case is_short(VSN) of
		true ->
		    insert(S,{id,X,LineNr,N,File,Type});
		false ->
		    set_compilation_failure(S),
		    io:format("Error on file ~p :~n",[get_idlfile(S)]),
		    io:format("  Bad pragma ID ~p on line ~p,~n",
			      [element(3,Apply),LineNr]),
		    io:format("  the version part of ID is not a short integer.~n")
	    end;
	["LOCAL"|_] ->
	    insert(S,{id,X,LineNr,N,File,Type});
	_ ->
	    set_compilation_failure(S),
	    io:format("Error on file ~p :~n",[get_idlfile(S)]),
	    io:format("  Bad pragma ID ~p on line ~p.~n",
		      [element(3,Apply),LineNr])
    end.



pragma_reg_version(G, S, N, X) ->
    {pragma,{_,LineNr,"version"}, _To, Apply} = X,

    File = get_idlfile(S), % The current file or an included one.
    Type = case idlfile(G) of % Local/Included flag
	       File ->
		   local;
	       _ ->
		   included
	   end,

    case tokens(Apply,".") of
	[Major,Minor] ->
	    case is_short(Major) and is_short(Minor) of
		true ->
		    insert(S,{version,X,LineNr,N,File,Type});
		false ->
		    set_compilation_failure(S),
		    io:format("Error on file ~p :~n",[get_idlfile(S)]),
		    io:format("  Bad pragma version ~p on line ~p,~n",
			      [Apply,LineNr]),
		    io:format("  the version is not valid.~n")
	    end;
	_ ->
	    set_compilation_failure(S),
	    io:format("Error on file ~p :~n",[get_idlfile(S)]),
	    io:format("  Bad pragma version ~p on line ~p,~n",
		      [Apply,LineNr]),
	    io:format("  the version is not valid.~n")
    end.


pragma_reg_codeOpt(G, S, _N, {pragma,{_,LineNr,"CODEOPT"},_,Apply} )->
    case applyCodeOpt(G) of
	true ->
	    {_,_,OptionList_str} = Apply,
	    case  list_to_term(OptionList_str) of
		error ->
		    ic_error:error(G,{pragma_code_opt_bad_option_list,LineNr});
		OptionList ->
		    case lists:keysearch(be,1,OptionList) of
			false ->
			    %% Add the terms of the option list 
			    %% to the compiler option list 
			    applyCodeOpts(G,S,LineNr,OptionList);
			{value, {be,Type}} ->
			    %% If backend is set from user,
			    %% let the same backend be otherwize 
			    %% set backend by codeOpt directive
			    case get_opt(G, be) of
				false ->
				    %% Add the terms of the option list 
				    %% to the compiler option list 
				    applyCodeOpts(G,S,LineNr,OptionList);
				_ ->
				    %% Add all the terms of the option list 
				    %% to the compiler option list but the
				    %% backend option
				    applyCodeOpts(G,
						  S,
						  LineNr,
						  lists:delete({be,Type},OptionList))
			    end
		    end
	    end;
	false ->
	    true
    end.



applyCodeOpts(_,_,_,[]) ->
    true;
applyCodeOpts(G,S,LNr,[{{broker,Scope},{M,T}}|Xs]) ->
    ScopedId = reverse(tokens(Scope,":")),
    case ets:match(S,
		   {codeopt,ScopedId,
		    '$1','$2','_','_'}) of
	[] ->    
	    %% Add pragma in table
	    insert(S,
		   {codeopt,
		    ScopedId,
		    {broker,{M,T}},
		    LNr,
		    get_idlfile(S),
		    get_filepath(S)}),
	    %% Continue
	    applyCodeOpts(G,S,LNr,Xs);
	_ ->
	    %% Use the code option
	    %% from user and continue
	    applyCodeOpts(G,S,LNr,Xs)
    end;
applyCodeOpts(G,S,LNr,[X|Xs]) ->
    case is_allowed_opt(X) of
	true ->
	    %% Add that term of the option list 
	    %% to the compiler option list      
	    ic_options:add_opt(G, [X], true),
	    %% Continue
	    applyCodeOpts(G,S,LNr,Xs);
	false ->
	    %% Print warning and continue
	    io:format("Warning on file ~p :~n",[get_idlfile(S)]),
	    io:format("  Bad option in pragma : ~p, ignored !~n",[X]),
	    applyCodeOpts(G,S,LNr,Xs)
    end.


is_allowed_opt({X,Y}) ->
    ic_options:allowed_opt(X,Y);
is_allowed_opt(_X) ->
    false.
    
	 

%% Returns a tuple { PFX, VSN, ID }, that is the  
%% pragma prefix, version and id coverages of
%% the scope SCOPE. This is done by use of the 
%% function pragma_cover/4.
pragma_cover(G,Scope,Object) ->
    pragma_cover(ic_genobj:pragmatab(G),get_id2(Object),Scope,get_line(Object)).

%% Returns a tuple { PFX, VSN, ID }, that is the  
%% pragma prefix, version and id coverages of
%% the scope SCOPE
pragma_cover(PragmaTab,Name,Scope,LineNr) ->
    PFX = pragma_prefix_cover(PragmaTab,Name,Scope,LineNr), 
    VSN = pragma_version_cover(PragmaTab,Name,Scope,LineNr),
    ID = pragma_id_cover(PragmaTab,Name,Scope,LineNr),
    { PFX, VSN, ID }.



%% Finds out which pragma PREFIX that affects 
%% the scope Scope
pragma_prefix(G,Scope,Object) ->
    pragma_prefix_cover(ic_genobj:pragmatab(G),get_id2(Object),Scope,get_line(Object)).


%% Finds out which pragma PREFIX that affects 
%% the scope Scope
pragma_prefix_cover(PragmaTab,Name,Scope,LineNr) ->
    case lookup(PragmaTab,prefix) of
	[] ->
	    none;
	PragmaPrefixList ->
	    FilteredPragmaPrefixList = 
		filter_pragma_prefix_list(PragmaTab,Name,Scope,PragmaPrefixList),
	    case most_local(FilteredPragmaPrefixList,Scope) of
		[] ->
		    none;
		MostLocalList ->	    
		    case dominant_prefix(MostLocalList,LineNr) of
			none ->
			    none;

			%% Just filter empty pragma prefix
			{prefix,{pragma,{_,_,_},_,{'<string_literal>',_,[]}},_,_,_,_} ->
			    none;

			DP ->
			    %% Return the scoped id (reversed list of
                            %% path elements, but remember to remove 
                            %% '[]' that represents the top level   
			    slashify(lists:sublist(Scope, 1,
						   length(Scope) - length(element(4,DP))) ++
				     [ element(3,element(4,element(2,DP)))])
		    end
	    end
    end.


%% Returns a slashified name, [I1, M1] becomes "M1/I1"
slashify(List) -> lists:foldl(fun(X, Acc) -> X++"/"++Acc end, 
			      hd(List), tl(List)).


%% Finds out which pragma VERSION that affects 
%% the scope Scope
pragma_version(G,Scope,Object) ->
    pragma_version_cover(ic_genobj:pragmatab(G),get_id2(Object),Scope,get_line(Object)).

%% Finds out which pragma VERSION that affects 
%% the scope Scope
pragma_version_cover(PragmaTab,Name,Scope,LineNr) ->
    case lookup(PragmaTab,version) of
	[] ->
	    default_version();
	PragmaVersionList ->
	    case all_actual_for_version_or_id( PragmaVersionList, Name ) of
		[] ->
		    default_version();
		ActualVersionList ->
		    case most_local(ActualVersionList,Scope) of
			[] ->
			    default_version();
			MostLocalList ->
			    case dominant_version(MostLocalList,LineNr) of
				DV ->
				    element(4,element(2,DV))
			    end
		    end
	    end
    end.


default_version() -> "1.0".
    


%% Finds out which pragma ID that affects 
%% the scope Scope
pragma_id(G,Scope,Object) ->
    pragma_id_cover(ic_genobj:pragmatab(G),get_id2(Object),Scope,get_line(Object)).

%% Finds out which pragma ID that affects 
%% the scope Scope
pragma_id_cover(PragmaTab,Name,Scope,LineNr) ->
    case lookup(PragmaTab,id) of
	[] ->
	    none;
	PragmaIdList ->
	    case all_actual_for_version_or_id( PragmaIdList, Name ) of
		[] ->
		    none;
		ActualIdList ->
		    case most_local(ActualIdList,Scope) of
			[] ->
			    none;
			MostLocalList ->	    
			    case dominant_id(MostLocalList,LineNr) of
				PI ->
				    element(3,element(4,element(2,PI)))
			    end
		    end
	    end
    end.



    
%% Finds out which pragma VERSION ( or ID ) that 
%% that affects the scope object with name NAME
all_actual_for_version_or_id(NList, Name) ->
    all_actual_for_version_or_id( NList, [], Name ).

all_actual_for_version_or_id([], Actual, _) ->
    Actual;
all_actual_for_version_or_id([First|Rest], Found, Name) ->
    case is_actual_for_version_or_id(First,Name) of
	true ->
	    all_actual_for_version_or_id(Rest, [First|Found], Name);
	false ->
	    all_actual_for_version_or_id(Rest, Found, Name)
    end.

is_actual_for_version_or_id( Current, Name ) ->
    case element(3,element(3,element(2,Current))) of
	Name ->
	    true;
	OtherName ->
	    suffix([Name],tokens(OtherName,"::"))
    end.




%% Find the most locally defind pragmas
%% to the scope SCOPE
most_local( SList, Scope ) ->
    case SList of
	[] ->
	    [];
	[First|Rest] ->
	    case suffix( element(4,First), Scope ) of
		true ->
		    most_local( Rest, First, Scope, [First] );
		false ->
		    most_local( Rest, Scope )
	    end
    end.

%% Returns a list of all pragmas found in the 
%% same scope. Should choose the right one by looking 
%% att the  position of the pragma in relation to
%% the current object..... ( For hairy cases ).  
most_local( SList, Current, Scope, AllFound ) ->
    case SList of
	[] ->
	    AllFound;
	[First|Rest] ->
	    FirstScope = element(4,First),
	    case suffix( FirstScope, Scope ) of
		true ->
		    CurrentScope = element(4,Current),
		    case suffix( CurrentScope, FirstScope ) of
			true -> 
			    case length( CurrentScope ) == length( FirstScope ) of 
				true -> %% SAME SCOPE ! KEEP BOTH
				    most_local( Rest, Current, Scope, [First|AllFound] );
				false -> 
				    most_local( Rest, First, Scope, [First] )
			    end;
			false ->
			    most_local( Rest, Current, Scope, AllFound )
		    end;
		false -> 
		    most_local( Rest, Current, Scope, AllFound )
	    end
    end.




%% Find the most dominant prefix pragmas
%% located onto the SAME scope. Now
%% we look att the line number, the position
%% on the file. 
dominant_prefix(SList,LineNr) ->
    case SList of 
	[First|Rest] ->
	    dominant_prefix(Rest,First,LineNr)
    end.


dominant_prefix([],{prefix,X,PLNr,N,F,T},LineNr) ->
    case LineNr > PLNr of
	true ->
	    {prefix,X,PLNr,N,F,T};
	false ->
	    none
    end;
dominant_prefix([{prefix,FX,FPLNr,FN,F1,T1}|Rest],{prefix,CX,CPLNr,CN,F2,T2},LineNr) ->
    case LineNr > FPLNr of % Check if FIRST before the object 
	true -> 
	    case FPLNr > CPLNr of % Check if FIRST after CURRENT
		true ->
		    dominant_prefix(Rest,{prefix,FX,FPLNr,FN,F1,T1},LineNr);
		false ->
		    dominant_prefix(Rest,{prefix,CX,CPLNr,CN,F2,T2},LineNr)
	    end;
	false -> % FIRST does not affect the object
	    dominant_prefix(Rest,{prefix,CX,CPLNr,CN,F2,T2},LineNr)
    end.




%% Find the most dominant version pragmas
%% located onto the SAME scope. Now
%% we look att the line number, the position
%% on the file. 
dominant_version(SList,LineNr) ->
    case SList of 
	[First|Rest] ->
	    dominant_version(Rest,First,LineNr)
    end.


dominant_version([],Current,_) -> Current;
dominant_version([{version,FX,FPLNr,FN,F1,T1}|Rest],{version,CX,CPLNr,CN,F2,T2},LineNr) ->
    case FPLNr > CPLNr of % Check if FIRST after CURRENT
	true ->
	    dominant_version(Rest,{prefix,FX,FPLNr,FN,F1,T1},LineNr);
	false ->
	    dominant_version(Rest,{prefix,CX,CPLNr,CN,F2,T2},LineNr)
    end.




%% Find the most dominant id pragmas
%% located onto the SAME scope. Now
%% we look att the line number, the position
%% on the file. 
dominant_id(SList,LineNr) ->
    case SList of 
	[First|Rest] ->
	    dominant_id(Rest,First,LineNr)
    end.


dominant_id([],Current,_) -> Current;
dominant_id([{id,FX,FPLNr,FN,F1,T1}|Rest],{id,CX,CPLNr,CN,F2,T2},LineNr) ->
    case FPLNr > CPLNr of % Check if FIRST after CURRENT
	true ->
	    dominant_id(Rest,{id,FX,FPLNr,FN,F1,T1},LineNr);
	false ->
	    dominant_id(Rest,{id,CX,CPLNr,CN,F2,T2},LineNr)
    end.





%% This registers a module defined inside the file or
%% an included file. A tuple that describes the module
%% is added to the table. 
%% Observe that the modules registered are ONLY those
%% who are in the top level, not definedd inside others !
mk_ref(G,Name,Type) ->
    case length(Name) > 1 of
	true -> %% The interface is NOT defined att top level
	    true;
	false ->
	    S = ic_genobj:pragmatab(G),
	    File = get_idlfile(S), % The current file or an included one.
	    case idlfile(G) of % The current file to be compiled.
		File ->
		    insert(S,{Type,Name,File,local});
		_ ->
		    insert(S,{Type,Name,File,included})
	    end
    end.


%% The same as mk_ref/3 but this registers everything with 
%% all vital information available inside files.
%% Registers ESSENTIAL data for included files
mk_file_data(G,X,Scope,Type) ->
    S = ic_genobj:pragmatab(G),
    Name = get_id2(X),
    PreprocFile = get_idlfile(S), % The current file or an included one.
    CompFile = idlfile(G), % The current file compiled
    Depth = length(Scope), % The depth of the scope
    ScopedName = ic_util:to_undersc([Name|Scope]),
    Line = ic_forms:get_line(X),
    case PreprocFile of 
	CompFile ->
	    insert(S,{file_data_local,CompFile,CompFile,Type,Scope,Name,ScopedName,Depth,Line});
	PreprocFile ->
	    insert(S,{file_data_included,PreprocFile,CompFile,Type,Scope,Name,ScopedName,Depth,Line})
    end.



%% Return a list with all the headers from
%% the local file that represent the module
%% or interface that is preciding the current
get_local_c_headers(G,X) ->
    S = ic_genobj:pragmatab(G),
    Local = lookup(S,file_data_local),
    FoundLocal = get_local_c_headers(X,Local,Local),
    no_doubles(FoundLocal).

get_local_c_headers(X,Local,Local) -> 
    get_local_c_headers(X,Local,Local,[]).

get_local_c_headers(_X,[],_All,Found) ->
    Found;
get_local_c_headers(X,[{file_data_local,_PF_idl,_,module,_,_,SN,_,Line}|Hs],All,Found)->
    case ic_forms:get_line(X) > Line of
	true ->
	    get_local_c_headers(X,Hs,All,[SN|Found]);
	false ->
	    get_local_c_headers(X,Hs,All,Found)
    end;
get_local_c_headers(X,[{file_data_local,_PF_idl,_,interface,_,_,SN,_,Line}|Hs],All,Found)->
    case ic_forms:get_line(X) > Line of
	true ->
	    get_local_c_headers(X,Hs,All,[SN|Found]);
	false ->
	    get_local_c_headers(X,Hs,All,Found)
    end;
get_local_c_headers(X,[_|Hs],All,Found) ->
    get_local_c_headers(X,Hs,All,Found).



%% Return a list with all the headers from
%% the included file that represent the module
%% or interface that have to be included
get_included_c_headers(G) ->
    S = ic_genobj:pragmatab(G),
    Included = lookup(S,file_data_included),
    FoundIncluded = get_included_c_headers(Included,Included),
    no_doubles(FoundIncluded).

get_included_c_headers(Included,Included) -> 
    get_included_c_headers(Included,Included,[]).

get_included_c_headers([],_All,Found) ->
    Found;
get_included_c_headers([{file_data_included,PF_idl,_CF_idl,T,_S,_N,SN,0,_}|Hs],All,Found) ->
    Len = length(PF_idl),
    FN = string:sub_string(PF_idl,1,Len-4),
    case only_top_level(PF_idl,All) of
	true ->
	    %%
	    L = string:tokens(FN,"/"),
	    FN2 = lists:last(L),
	    %%
	    get_included_c_headers(Hs,All,["oe_"++FN2|Found]);
	false ->
	    case T of
		module ->
		    case contains_interface(PF_idl,All) of
			true ->
			    %%
			    L = string:tokens(FN,"/"),
			    FN2 = lists:last(L),
			    %%
			    get_included_c_headers(Hs,All,["oe_"++FN2|Found]);
			false ->
			    get_included_c_headers(Hs,All,[SN|Found])
		    end;
	        interface ->
		    case contains_interface(PF_idl,All) of
			true ->
			    %%
			    L = string:tokens(FN,"/"),
			    FN2 = lists:last(L),
			    %%
			    get_included_c_headers(Hs,All,["oe_"++FN2|Found]);
			false ->
			    get_included_c_headers(Hs,All,[SN|Found])
		    end;
		_ ->
		    get_included_c_headers(Hs,All,["oe_"++FN|Found])
	    end
    end;
get_included_c_headers([{file_data_included,_PF_idl,_,module,_,_,SN,_,_}|Hs],All,Found)->
    get_included_c_headers(Hs,All,[SN|Found]);
get_included_c_headers([{file_data_included,_PF_idl,_,interface,_,_,SN,_,_}|Hs],All,Found)->
    get_included_c_headers(Hs,All,[SN|Found]);
get_included_c_headers([_|Hs],All,Found) ->
    get_included_c_headers(Hs,All,Found).

%% Help functions for the above

only_top_level(_PF_idl,[]) ->
    true;
only_top_level(PF_idl,[H|Hs]) ->
    case element(2,H) of
	PF_idl ->
	    case element(8,H) > 0 of
		true ->
		    false;
		false ->
		    only_top_level(PF_idl,Hs)
	    end;
	_ ->
	    only_top_level(PF_idl,Hs)
    end.
	
contains_interface(_PF_idl,[]) ->
    false;
contains_interface(PF_idl,[H|Hs]) ->
    case element(2,H) of
	PF_idl ->
	    case element(4,H) of
		interface ->
		    case element(8,H) > 0 of
			true ->
			    true;
			false ->
			    contains_interface(PF_idl,Hs)
		    end;
		_ ->
		    contains_interface(PF_idl,Hs)
	    end;
	_ ->
	    contains_interface(PF_idl,Hs)
    end.
    


%% This returns a list of everything defined in an included file.
get_incl_refs(G) ->
    S = ic_genobj:pragmatab(G),
    
    RefList = 
	ets:match(S,{mod_ref,'$0','_',included}) ++
	ets:match(S,{ifc_ref,'$0','_',included}) ++
	ets:match(S,{const_ref,'$0','_',included}) ++
	ets:match(S,{typedef_ref,'$0','_',included}) ++
	ets:match(S,{except_ref,'$0','_',included}) ++
	ets:match(S,{struct_ref,'$0','_',included}) ++
	ets:match(S,{union_ref,'$0','_',included}) ++
	ets:match(S,{enum_ref,'$0','_',included}) ++
	ets:match(S,{attr_ref,'$0','_',included}),

    case RefList of
	[] ->
	    none;
	_ ->
	    RefList
    end.



%% This returns a list of everything locally defined.
get_local_refs(G) ->
    S = ic_genobj:pragmatab(G),

    RefList = 
	ets:match(S,{mod_ref,'$0','_',local}) ++
	ets:match(S,{ifc_ref,'$0','_',local}) ++
	ets:match(S,{const_ref,'$0','_',local}) ++
	ets:match(S,{typedef_ref,'$0','_',local}) ++
	ets:match(S,{except_ref,'$0','_',local}) ++
	ets:match(S,{struct_ref,'$0','_',local}) ++
	ets:match(S,{union_ref,'$0','_',local}) ++
	ets:match(S,{enum_ref,'$0','_',local}) ++
	ets:match(S,{attr_ref,'$0','_',local}),

    case RefList of
	[] ->
	    none;
	_ ->
	    RefList
    end.





%% This is intented to be used for solving the identification
%% problem introduced by pragmas. It creates aliases between
%% scoped and "final" identities.
mk_alias(G,PragmaId,ScopedId) ->
    %io:format("~nMaking alias -> ~p~n",[PragmaId]),
    S = ic_genobj:pragmatab(G),
    insert(S,{alias,ScopedId,PragmaId}).


%% This is used to find out if the object described with
%% the scoped id is created. If this is the case, it should
%% be registered as an alias and the identity of the object 
%% is returned. Otherwize "none" is returned.
get_alias(G,ScopedId) ->
    S = ic_genobj:pragmatab(G),
    case ets:match(S,{alias,ScopedId,'$1'}) of
	[] ->
	    none;
	[[IfrId]] ->
	    %io:format("~nFound alias -> ~p~n",[IfrId]),
	    IfrId
    end.



%% Returns the alias id or constructs an id
scope2id(G,ScopedId) ->
    case get_alias(G,ScopedId) of
	none ->
	    case is_included(G,ScopedId) of
		true -> %% File included
		    get_included_IR_ID(G,ScopedId);
		false -> %% File local
		    NewIfrId = mk_id(ScopedId),    % Create a "standard" id
		    mk_alias(G,NewIfrId,ScopedId), % Create an alias
		    NewIfrId
	    end;
	IfrId ->
	    IfrId
    end.




is_included(G,ScopedId) ->
    S = ic_genobj:pragmatab(G),
    Name = ic_util:to_undersc(ScopedId),
    case ets:match(S,{file_data_included,'_','_','_','_','_',Name,'_','_'}) of
	[[]] ->
	    true;
	_ ->
	    false
    end.



get_included_IR_ID(G,ScopedId) ->
    S = ic_genobj:pragmatab(G),
    ScopedName = ic_util:to_undersc(ScopedId),
    [[Scope,Name,LNr]] = ets:match(S,{file_data_included,'_','_','_','$3','$4',ScopedName,'_','$7'}),
    {Prefix,Vsn,Id} = pragma_cover(S,Name,Scope,LNr),
    case Id of
	none ->
	    case Prefix of
		none ->
		    IR_ID = 
			lists:flatten(io_lib:format("IDL:~s:~s",[ScopedName, Vsn])),
		    ic_pragma:mk_alias(G,IR_ID,ScopedId),
		    IR_ID;
		_ ->
		    IR_ID = 
			lists:flatten(io_lib:format("IDL:~s:~s",[Prefix ++ "/" ++ ScopedName, Vsn])),
		    ic_pragma:mk_alias(G,IR_ID,ScopedId),
		    IR_ID
	    end;
	_ ->
	    ic_pragma:mk_alias(G,Id,ScopedId),
	    Id
    end.
	    




%% Returns the scope for object
id2scope(G,IfrId) ->
    S = ic_genobj:pragmatab(G),
    case lookup(S,alias) of
	[] ->
	    mk_scope(IfrId);
	AliasList ->
	    case keysearch(IfrId,3,AliasList) of
		false ->
		    mk_scope(IfrId);
		{value,{alias,ScopedId,_}} ->
		    ScopedId
	    end
    end.

%% Returns a "standard" IDL ID by getting the scope list
mk_id(ScopedId) ->
    "IDL:" ++ ic_pragma:slashify(ScopedId) ++ ":" ++ default_version().

%% Returns the scope of an object when getting a "standard" IDL ID
mk_scope(IfrId) ->
    [_,Body,_] = tokens(IfrId,":"),
    reverse(tokens(Body,"/")).



%% This is used to note the exact compiled file  
%% under pragma creation. There are two options, the 
%% main file or files included by the main file. This
%% just denotes the CURRENT file, the main file or
%% the included ones. A very usual field is the file
%% path that shows the include path of the file 

init_idlfile(G,S) ->
    IdlFile = idlfile(G),
    insert(S,{file,IdlFile,[]}).

set_idlfile(S,FileName) ->
    FilePath = get_filepath(S),
    case FilePath of
	[] ->
	    ets:delete(S,file),
	    insert(S,{file,FileName,[FileName|FilePath]});
	_ ->
	    case hd(FilePath) of
		[] ->
		    ets:delete(S,file),
		    insert(S,{file,FileName,[FileName|FilePath]});
		_ ->
		    case tl(FilePath) of
			[] ->
			    ets:delete(S,file),
			    insert(S,{file,FileName,[FileName|FilePath]});
			_ ->
			    case hd(tl(FilePath)) of
				[] ->
				    ets:delete(S,file),
				    insert(S,{file,FileName,[FileName|FilePath]});
				FileName ->
				    ets:delete(S,file),
				    insert(S,{dependency,FilePath}), % Add dependency branch
				    insert(S,{file,FileName,tl(FilePath)});
				_ ->
				    ets:delete(S,file),
				    insert(S,{file,FileName,[FileName|FilePath]})
			    end
		    end
	    end
    end.

get_idlfile(S) ->
    [FT] = lookup(S,file),
    element(2,FT).

get_filepath(S) ->
    [FT] = lookup(S,file),
    element(3,FT).


%% This returns a list of file names
%% that direct or indirect the current
%% compiled file is depended on.
get_dependencies(G) ->
    S = ic_genobj:pragmatab(G),
    case lookup(S,dependency) of
	[] ->
	    [];
	Dependencies ->
	    {get_idlfile(S),get_dependencies(Dependencies,[])}
    end.

get_dependencies([],Dependencies) ->
    no_doubles(Dependencies);
get_dependencies([{dependency,Path}|Tail],Current) ->
    get_dependencies(Tail,[hd(Path)|Current]).


no_doubles(List) ->
    no_doubles(List,[]).

no_doubles([],NoDoubles) ->
    NoDoubles;
no_doubles([X|Xs],Current) ->
    case member(X,Xs) of
	true ->
	    no_doubles(Xs,Current);
	false ->
	    no_doubles(Xs,[X|Current])
    end.




%% Pragma compilation status initialization
init_pragma_status(S) ->    
    insert(S,{status,true,0}).

%% Pragma compilation status set to failure
%% and count up the number of errors
set_compilation_failure(S) ->
    [{status,_,ErrorNr}] = lookup(S,status),
    ets:delete(S,status),
    insert(S,{status,false,ErrorNr+1}).

%% Pragma compilation status set to lookup
get_pragma_compilation_status(S) ->
    [Status] = lookup(S,status),
    element(2,Status).

%% Pragma error number
get_pragma_error_nr(S) ->
    [Status] = lookup(S,status),
    element(3,Status).


%% Short check 
is_short(N_str) when is_list(N_str) ->
    case is_short_decimal_str(N_str) of
	true ->
	    true;
	false ->
	    false
    end;
is_short(N) when is_integer(N)->
    (N < 65535) and (N > -65536);
is_short(_) -> false.


%% Check if the string is a
%% list of characters representing
%% a short. Avoid crash !.
is_short_decimal_str(N_str) ->
    case is_decimal_str(N_str) of
	true ->
	    N = list_to_integer(N_str),
	    (N < 65535) and (N > -65536); 
	false ->
	    false
    end.

%% Check if the string is a
%% list of characters representing
%% decimals.
is_decimal_str([]) ->
    true;
is_decimal_str([First|Rest]) ->
    case is_decimal_char(First) of
        true ->
            is_decimal_str(Rest);
        false ->
            false
    end.

%% True if D is a character 
%% representing a decimal (0-9).
is_decimal_char(D) ->
    case (48=<D) and (D=<57) of
	true ->
	    true;
	false ->
	    false
    end.


%% Prints out all the table
print_tab(G) ->
    io:format("~nPragmaTab = ~p~n",[ets:tab2list(ic_genobj:pragmatab(G))]).


list_to_term(List) ->
    case catch erl_scan:string(List) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
		{ok,Term} ->
		    Term;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.



%% Cleanup all other code options for a specified scope
%% in the same file, but the most dominant.
cleanup_codeOptions(G,S,ScopedId) ->
    case ets:match(S,{codeopt,ScopedId,'$1','$2',idlfile(G),'$4'}) of
	[] ->
	    %% No codeOpt directive is placed inside the
	    %% currently compiled file. Try to find other
            %% directives located in included files. 
	    true;
	List -> 
	    %% A codeOpt directive is placed inside the
	    %% currently compiled file. This dominates
            %% all other directives. 
	    CodeOption = best_positioned_codeOpt(List),
	    %% Remove code options that do not affect 
	    %% the code production (redundant) 
	    remove_redundant_codeOpt(S,[ScopedId|CodeOption])
    end.


%% Best positioned is the codeopt located 
%% "highest" on the SAME file, the one with 
%% lowest line number. 
best_positioned_codeOpt([X|Xs]) -> 
    best_positioned_codeOpt(Xs,X).

best_positioned_codeOpt([],Found) ->
    Found;
best_positioned_codeOpt([X|Xs],Current) ->
    case hd(tl(X)) > hd(tl(Current)) of
	true ->
	    best_positioned_codeOpt(Xs,Current);
	false ->
	    best_positioned_codeOpt(Xs,X)
    end.


remove_redundant_codeOpt(S,[ScopedId,CodeOption,LNr,FilePath]) ->
    ets:match_delete(S,{codeopt,ScopedId,'$1','$2','$3','$4'}),
    ets:insert(S,{codeopt,ScopedId,CodeOption,LNr,last(FilePath),FilePath}).




add_inh_data(G,InclScope,X) ->
    S = ic_genobj:pragmatab(G),
    case X#interface.inherit of
	[] ->
	    true;
	[InhBody] -> 
	    Scope = [get_id2(X)|InclScope],
	    insert(S,{inherits,Scope,InhBody});
	InhList ->
	    add_inh_data(G, S, InclScope, X, InhList)
    end.

add_inh_data(_,_,_,_,[]) ->
    true;
add_inh_data(G, S, InclScope, X, [InhBody|InhBodies]) ->	
    Scope = [get_id2(X)|InclScope],
    insert(S, {inherits,Scope,InhBody}),
    add_inh_data(G, S, InclScope, X, InhBodies).


%% Returns a default broker data
defaultBrokerData(G) ->
    {to_atom(ic_genobj:impl(G)),transparent}.


%% Loops through the form and sdds inheritence data 
preproc(G, N, [X|Xs]) when is_record(X, interface) ->
    %% Add inheritence data to pragmatab
    ic_pragma:add_inh_data(G,N,X),
    N2 = [get_id2(X) | N],
    preproc(G, N2, get_body(X)),
    lists:foreach(fun({_Name, Body}) -> preproc(G, N2, Body) end, 
		  X#interface.inherit_body), 
    preproc(G, N, Xs);

preproc(G,N,[X|Xs]) when is_record(X, module) ->
    N2 = [get_id2(X) | N],
    preproc(G, N2, get_body(X)),
    preproc(G,N,Xs);

preproc(G,N,[_X|Xs]) ->
    preproc(G,N,Xs);

preproc(_G, _N, []) ->
    ok.


%% Returns a tuple / list of tuples { Mod, Type } 
%% Does not check overridence because it is the 
%% top scope for the module to be produced and
%% cannot be overriden.
getBrokerData(G,X,Scope) ->
    S = ic_genobj:pragmatab(G),
    cleanup_codeOptions(G,S,Scope),

    %% Check if it is an operation denoted
    case isOperation(S,Scope) of
	%% Yes, check options
	true ->
	    %% Look if there is a specific code option on top file
	    case hasSpecificCodeoptionOnTopFile(S,ic_genobj:idlfile(G),Scope) of
		true ->
		    %% Yes, let it work
		    getBrokerData(G,S,X,Scope,[Scope],[]);
		false ->
		    %% No, try to see if there is codeoption on top file
		    case hasNonSpecificCodeoptionOnTopFile(S,ic_genobj:idlfile(G)) of
			true ->
			    %% Yes, override every other specific code option
			    [_H|T] = Scope,
			    getBrokerData(G,S,X,Scope,[T],[]);
			false ->
			    %% No, let inherited specific code options work
			    getBrokerData(G,S,X,Scope,[Scope],[])
		    end
	    end;
	%% No, continue
	false ->
	    getBrokerData(G,S,X,Scope,[Scope],[])
    end.

%% Returns a tuple / list of tuples { Mod, Type }
%% Inside loop, uses overridence. 
getBrokerData(G,X,RS,Scope,CSF) ->
    S = ic_genobj:pragmatab(G),
    cleanup_codeOptions(G,S,Scope),
    OvScope = overridedFrom(S,RS,Scope),
    getBrokerData(G,S,X,RS,[OvScope],[OvScope|CSF]).



getBrokerData(G,S,X,RS,[[[First]|Rest]],CSF) when is_integer(First) ->
    Scope = [[First]|Rest],
    case ets:match(S,{codeopt,Scope,'$1','_','_','_'}) of
	[] ->
	    case ets:match(S,{inherits,Scope,'$1'}) of
		[] -> %% No inheritence, no pragma codeopt
		    defaultBrokerData(G); %% Default
		[InhScope] ->
		    getBrokerData(G,S,X,RS,InhScope,CSF);
		InhList ->
		    getBrokerDataInh(G,S,X,RS,Scope,CSF,InhList)
	    end;
	[[{broker,{Module,Type}}]] -> %% A branch only, with pragma codeopt
	    {Module,Type};
	List -> %% Multiple branches with pragma codeopt
	    flatten(List)
    end;

getBrokerData(G,S,X,RS,[[[First]|Rest]],CSF) ->
    getBrokerDataLoop(G,S,X,RS,[[First]|Rest],CSF);

getBrokerData(G,S,X,RS,[Scope],CSF) ->
   %io:format(" 1"),
    case ets:match(S,{codeopt,Scope,'$1','_','_','_'}) of
	[] ->
	   %io:format(" 2"),
	    case ets:match(S,{inherits,Scope,'$1'}) of
		[] -> %% No inheritence, no pragma codeopt
		   %io:format(" 5"),
		    defaultBrokerData(G); %% Default
		[InhScope] ->
		   %io:format(" 6"),
		    getBrokerData(G,S,X,RS,InhScope,CSF);
		InhList ->
		   %io:format(" 7"),
		    getBrokerDataInh(G,S,X,RS,Scope,CSF,InhList)
	    end;
	[[{broker,{Module,Type}}]] -> %% A branch only, with pragma codeopt
	   %io:format(" 3"),
	    {Module,Type};
	List -> %% Multiple branches with pragma codeopt
	   %io:format(" 4"),
	    flatten(List)
    end.


%% Special treatment when X is an operation
getBrokerDataInh(G,S,X,RS,Scope,CSF,InhList) when is_record(X,op)->
   %io:format(" 8"),
    case ets:match(S,{op,get_id2(X),'$1','_','_'}) of
	[] ->
	   %io:format(" 10"),
	    CleanList = remove_inherited(S,InhList),
	    getBrokerDataLoop(G,S,X,RS,CleanList,CSF);
	
	[[Scope]] ->
	   %io:format(" 11"),
	    CleanList = remove_inherited(S,InhList),
	    getBrokerDataLoop(G,S,X,RS,CleanList,CSF);
   
	[[OpScope]] ->
	   %io:format(" 12"),
	    case member([OpScope],InhList) of 
		true ->
		   %io:format(" 14"),
		    %% No inherited scopes
		    getBrokerData(G,X,RS,OpScope,CSF);	
		false ->
		   %io:format(" 15"),
		    %% Inherited scopes
		    CleanList = remove_inherited(S,InhList),
		    getBrokerDataLoop(G,S,X,RS,CleanList,CSF)
	    end;
	
	ListOfOpScopes ->
	   %io:format(" 13"),
            case get_inherited(S,Scope,ListOfOpScopes) of
		[[OpScope]] ->
		    case member([OpScope],InhList) of 
			true ->
			    getBrokerData(G,X,RS,OpScope,CSF);
			false ->
			    CleanList = remove_inherited(S,InhList),
			    getBrokerDataLoop(G,S,X,RS,CleanList,CSF)
		    end;
		_ ->
		    CleanList = remove_inherited(S,InhList), 
		    getBrokerDataLoop(G,S,X,RS,CleanList,CSF)
	    end
    end;
%% Just add InhList after removing all inherited
getBrokerDataInh(G,S,X,RS,_Scope,CSF,InhList) ->
   %io:format(" 9"),
    CleanList = remove_inherited(S,InhList),
    getBrokerDataLoop(G,S,X,RS,CleanList,CSF).




%% Loops over a list of scopes
getBrokerDataLoop(G,S,X,RS,List,CSF) ->
    getBrokerDataLoop(G,S,X,RS,List,[],CSF).

getBrokerDataLoop(G,_,_X,_RS,[],BrokerDataList,_CSF) ->
    case no_doubles(BrokerDataList) of
	[BrokerData] -> %% No pragma codeopt / Multiple branches with pragma codeopt
	    BrokerData;
	List ->
	    DefaultBD = defaultBrokerData(G),
	    case member(DefaultBD,List) of
		true ->
		    %% Remove default, choose codeoption 
		    NewList = delete(DefaultBD,List),
		    case NewList of
			[BData] -> %% A branch only, with pragma codeopt
			    BData;
			_Other -> %% Multiple branches with pragma codeopt
			    %%io:format("Multiple branches ~p~n",[Other]),
			    NewList
		    end;
		false -> %% Multiple branches with pragma codeopt
		    flatten(List)
	    end
    end;

getBrokerDataLoop(G,S,X,RS,[[Scope]|Scopes],_Found,CSF) when is_integer(Scope) ->
   getBrokerData(G,S,X,RS,[[Scope]|Scopes],CSF); 

getBrokerDataLoop(G,S,X,RS,[[Scope]|Scopes],Found,CSF) ->
    %% Start from the beginning, check for overridings
    case member(overridedFrom(S,RS,Scope),CSF) of %% Avoid infinite loops
	true ->
	    getBrokerDataLoop(G,S,X,RS,Scopes,Found,CSF);
	false ->
	    BrokerData = getBrokerData(G,X,RS,Scope,CSF),
	    getBrokerDataLoop(G,S,X,RS,Scopes,[BrokerData|Found],[Scope|CSF])
    end.




%%%--------------------------------------
%%% Finds out the overrider of a scope
%%%--------------------------------------
overridedFrom(S,RS,Scope) ->
    overridedFrom(S,RS,Scope,Scope).	    

overridedFrom(S,RS,Last,Scope) ->
    case ets:match(S,{inherits,'$0',Scope}) of
	[] -> 
	    %% No inheritence, no pragma codeopt,
	    %% choose the last scope.
	    Last;

	[[RS]] ->
	    %% Garbage, unused interface with pragma
            %% code option ! Danger !
	    Last;
	
	[[InhScope]] ->
	    case ets:match(S,{codeopt,InhScope,'$1','_','_','_'}) of
		[] -> 
		    %% InhScope has no code options, keep Last.
		    overridedFrom(S,RS,Scope,InhScope);
		_ ->
		    %% InhScope has code option, Last = InhScope.
		    overridedFrom(S,RS,InhScope,InhScope)
	    end;
	List -> 
	    %% Several inherit from Scope, choose the one feeseble,
	    %% the one DIRECTLY inherited by Scope and not through
	    %% other interface.  
	    case remove_inheriters(S,RS,List) of
		[] ->
		    Scope;
		Removed ->
		    Removed
	    end
    end.

%%%------------------------------------------------------
%%% Removes all the scopes that inherit from others 
%%%------------------------------------------------------
remove_inheriters(S,RS,InheriterList) ->
    DominantList =
	dominantList(S,InheriterList),
    ReducedInhList = 
	[X || X <- InheriterList, 
	      member(X,DominantList)],

    case ReducedInhList of
        [] ->
	    [];
	[_OneOnly] ->
	    ReducedInhList;
	_Other ->
	    CleanList = 
                ets:match_object(S, {inherits,'_','_'}),
%	    CodeOptList = 
%		[X || X <- EtsList, element(1,X) == codeopt],
	    NoInheriters =remove_inheriters2(S,ReducedInhList,CleanList),

	    [ [X] || [X] <- NoInheriters,
		     inherits(RS,X,CleanList)]
    end.

remove_inheriters2(_,[A],_) ->
    [A];
remove_inheriters2(_S,[A,B],EtsList) ->
    case remove_inh(A,B,[A,B],EtsList) of
	[[X]] ->
	    X;
	List ->
	    List
    end;
remove_inheriters2(S,[A,B|Rest],EtsList) ->
    case remove_inh(A,B,[A,B|Rest],EtsList) of
	[A,B|Rest] ->
	    [A,B|Rest];
	NewList ->
	    remove_inheriters2(S,NewList,EtsList)
    end.

remove_inh([X],[Y],List,EtsList) ->
    case inherits(X,Y,EtsList) of
	true ->
	    delete([X],List);
	false ->
	    case inherits(Y,X,EtsList) of
		true ->
		    delete([Y],List);
		false ->
		    List
	    end
    end.



%%%----------------------------------------------
%%% Should remove all scope links that inherit 
%%% from others in the list 
%%%----------------------------------------------
remove_inherited(S,InheriterList) ->
    CleanList = 
        ets:match_object(S, {inherits, '_', '_'}),
    remove_inherited(S,InheriterList,CleanList).


remove_inherited(_S,[A,B],EtsList) ->
    case remove_inhed(A,B,[A,B],EtsList) of
	[[X]] ->
	    [[X]];
	List ->
	    List
    end;
remove_inherited(S,[A,B|Rest],EtsList) ->
    case remove_inhed(A,B,[A,B|Rest],EtsList) of
	[A,B|Rest] ->
	    [A,B|Rest];
	NewList ->
	    remove_inherited(S,NewList,EtsList)
    end.


remove_inhed([X],[Y],List,EtsList) ->
    case inherits(X,Y,EtsList) of
	true ->
	    delete([Y],List);
	false ->
	    case inherits(Y,X,EtsList) of
		true ->
		    delete([X],List);
		false ->
		    List
	    end
    end.







%%%----------------------------------------------
%%% Should return all scope links that is 
%%  are inherited from scope in the list 
%%%----------------------------------------------
get_inherited(S,Scope,OpScopeList) ->
    EtsList1 = ets:match(S, {inherits, Scope, '$1'}),
    [X || X <- EtsList1, member(X, OpScopeList)].







%%%---------------------------------------------------
%%% Returns a the list of scopes that have codeoption
%%% from a list of scopes
%%%---------------------------------------------------
dominantList(S,IL) ->
    dominantList(S,IL,[]).

dominantList(_S,[],Found) ->
    Found;
dominantList(S,[[X]|Xs],Found) ->
    case ets:match(S,{codeopt,X,'$1','_','_','_'}) of
	[] ->
	    dominantList(S,Xs,Found);
	_ ->
	    dominantList(S,Xs,[[X]|Found]) 
    end.




%%%---------------------------------------------------
%%% Returns true if X direct or indirect inherits Y
%%%---------------------------------------------------
inherits(X,Y,EtsList) ->
    case member({inherits,X,Y},EtsList) of
	true ->
	    %% Direct inherited
	    true;
	false ->
	    %% Indirectly inherited
	    AllInh = [ B || {inherits,A,B} <- EtsList, A == X ], 
	    inherits(X,Y,AllInh,EtsList)
    end.

inherits(_X,_Y,[],_EtsList) ->
    false;
inherits(X,Y,[Z|Zs],EtsList) ->
    case inherits2(X,Y,Z,EtsList) of
	true ->
	    true;
	false ->
	    inherits(X,Y,Zs,EtsList)
    end.

inherits2(_X,Y,Z,EtsList) ->
    case  member({inherits,Z,Y},EtsList) of
	true ->
	    true;
	false ->
	    inherits(Z,Y,EtsList)
    end.



%%
%% is_inherited_by/3
%%
%% Returns :
%%
%%     true if the first parameter is
%%          inherited by the second one
%%
%%     false otherwise   
%%
is_inherited_by(Interface1,Interface2,PragmaTab) ->
    InheritsList = ets:match_object(PragmaTab, {inherits, '_', '_'}),
    inherits(Interface2,Interface1,InheritsList).




%% Filters all pragma prefix from list not in same file 
%% the object

filter_pragma_prefix_list(PragmaTab, Name, Scope, List) ->
    IdlFile = scoped_names_idl_file(PragmaTab, Name, Scope),
    filter_pragma_prefix_list2(PragmaTab,IdlFile,List,[]).


filter_pragma_prefix_list2(_,_,[],Found) ->
    Found;
filter_pragma_prefix_list2(PT, IdlFile, [PP|PPs], Found) ->
    case PP of 
	{prefix,_,_,_,IdlFile,_} -> %% Same file as the Object, keep
	    filter_pragma_prefix_list2(PT, IdlFile, PPs, [PP|Found]);
	
	_Other -> %% NOT in same file as the Object, throw away
	    filter_pragma_prefix_list2(PT, IdlFile, PPs, Found)
    end.

scoped_names_idl_file(PragmaTab, Name, Scope) ->
    case ets:match(PragmaTab,{'_','$0','_','$2',Scope,Name,'_','_','_'}) of
	[[IdlFile, _Type]] -> %% Usual case 
	    IdlFile;
	[[_File,module]|_Files] -> %% Multiple modules, get LOCAL file
	    case ets:match(PragmaTab,{file_data_local,'$0','_',module,Scope,Name,'_','_','_'}) of
		[[LocalIdlFile]] -> 
		    LocalIdlFile;
		_ -> %% Should  NEVER occur
		    error
	    end;

	_ ->
	    error %% Should  NEVER occur
    end.






%%-------------------------------------------------
%%
%% Register specific pragma code options
%%
%% If there is an operation with that
%% scope, denote this as {codeopt_specific,Scope}
%%
%%-------------------------------------------------
denote_specific_code_opts(G) ->
    case ic_options:get_opt(G, be) of
	noc ->
	    S = ic_genobj:pragmatab(G),
	    COList = ets:match(S,{codeopt,'$0','_','_','_','_'}),
	    OPList = ets:match(S,{op,'$0','$1','_','_'}),
	    denote_specific_code_opts(S,COList,OPList);
	_ ->
	    ok
    end.

denote_specific_code_opts(_,_,[]) ->
    ok;
denote_specific_code_opts(S,COList,[[OpN,OpS]|OPSs]) ->
    case lists:member([[OpN|OpS]],COList) of
	true ->
	    insert(S, {codeopt_specific,[OpN|OpS]});
	false ->
	    ok
    end,
    denote_specific_code_opts(S,COList,OPSs).



%%---------------------------------------------
%%
%% Returns true/false if it denotes an operation
%%
%%---------------------------------------------
isOperation(_S,[]) ->
    false;
isOperation(_S,[_]) ->
    false;
isOperation(S,[H|T]) ->
    case ets:match(S,{op,H,T,'$2','$3'}) of
	[] ->
	    false;
	_ ->
	    true
    end.


hasSpecificCodeoptionOnTopFile(S,File,Scope) ->
    case ets:match(S,{codeopt,Scope,'_','$2',File,[File]}) of
	[] ->
	    false;
	_ ->
	    true
    end.


hasNonSpecificCodeoptionOnTopFile(S,File) ->
    case ets:match(S,{codeopt,'_','_','$2',File,[File]}) of
	[] ->
	    false;
	_ ->
	    true
    end.



%%---------------------------------------------
%%
%% Returns {ok,IfrId}/error when searching a random local type
%%
%%---------------------------------------------


fetchRandomLocalType(G) ->
    
    S = ic_genobj:pragmatab(G),

    case ets:match(S,{file_data_local,'_','_','$2','$3','$4','_','_','_'}) of		
	[] ->
	    false;
	
	List ->
	    fetchRandomLocalType(S,List)
    end.


fetchRandomLocalType(_,[]) ->
    false;
fetchRandomLocalType(S,[[module|_]|Tail]) ->
    fetchRandomLocalType(S,Tail);
fetchRandomLocalType(S,[[_,Scope,Name]|Tail]) ->
    case ets:match(S,{alias,[Name|Scope],'$1'}) of
	[] ->
	    fetchRandomLocalType(S,Tail);
	[[IfrId]] ->
	    {ok,IfrId}
    end.



%%---------------------------------------------
%%
%% Returns A list of local operation mapping 
%% for a given scope
%%
%%---------------------------------------------


fetchLocalOperationNames(G,I) ->
    S = ic_genobj:pragmatab(G),
    case ets:match(S,{file_data_local,'_','_',op,I,'$4','_','_','_'}) of
	[] ->
	    [];
	List ->
	    fetchLocalOperationNames2(List,[])
    end.

fetchLocalOperationNames2([],Found) ->
    lists:reverse(Found);
fetchLocalOperationNames2([[Name]|Names],Found) ->
    fetchLocalOperationNames2(Names,[Name|Found]).



%%------------------------------------------------
%%
%%  Returns a true if this scoped id is a local
%%  one, false otherwise
%%
%%------------------------------------------------
is_local(G,ScopedId) ->
    S = ic_genobj:pragmatab(G),
    Name = ic_util:to_undersc(ScopedId),
    case ets:match(S,{file_data_local,'_','_','_',tl(ScopedId),'_',Name,'_','_'}) of
	[[]] ->
	    true;
	_ ->
	    false
    end.
