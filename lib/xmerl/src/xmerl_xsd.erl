%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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

%% @doc Interface module for XML Schema validation. 
%% It handles the W3.org 
%% <a href="http://www.w3.org/XML/Schema#dev">specifications</a>
%% of XML Schema second edition 28 october 2004. For an introduction to
%% XML Schema study <a href="http://www.w3.org/TR/xmlschema-0/">part 0.</a>
%% An XML structure is validated by xmerl_xsd:validate/[2,3].
%% @type global_state(). <p>The global state of the validator. It is 
%% representated by the <code>#xsd_state{}</code> record.
%% </p>
%% @type option_list(). <p>Options allow to customize the behaviour of the 
%% validation.
%% </p>
%% <p>
%% Possible options are :
%% </p>
%% <dl>
%%   <dt><code>{tab2file,boolean()}</code></dt>
%%      <dd>Enables saving of abstract structure on file for debugging
%%         purpose.</dd>
%%   <dt><code>{xsdbase,filename()}</code></dt>
%%      <dd>XSD Base directory.</dd>
%%   <dt><code>{fetch_fun,FetchFun}</code></dt>
%%      <dd>Call back function to fetch an external resource.</dd>
%%   <dt><code>{fetch_path,PathList}</code></dt>
%%      <dd>PathList is a list of directories to search when fetching files.
%%          If the file in question is not in the fetch_path, the URI will
%%          be used as a file name.</dd>
%%   <dt><code>{state,State}</code></dt>
%%      <dd>It is possible by this option to provide a state with process
%%          information from an earlier validation.</dd> 
%% </dl>
%% @type filename() = string()
%% @end
%%%-------------------------------------------------------------------
-module(xmerl_xsd).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("xmerl.hrl").
-include("xmerl_internal.hrl").
-include("xmerl_xsd.hrl").
-include_lib("kernel/include/file.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([validate/2,validate/3,process_validate/2,process_validate/3,
	 process_schema/1,process_schema/2,
	 process_schemas/1,process_schemas/2,
	 state2file/1,state2file/2,file2state/1,format_error/1]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([print_table/1]).
%%-export([whitespace/1]).

%%----------------------------------------------------------------------
%% Imports
%%----------------------------------------------------------------------
-import(xmerl_lib,[is_facet/1, is_builtin_simple_type/1, is_xsd_string/1]).
-import(xmerl_xsd_type,[facet_fun/2]).
-import(lists,[reverse/1,reverse/2,foldl/3,member/2,filter/2,flatten/1,map/2,
	       splitwith/2,mapfoldl/3,keysearch/3,keymember/3,
	       keyreplace/4,keydelete/3]).



%%======================================================================
%% Functions
%%======================================================================

%% @spec validate(Element,State) -> Result
%% @equiv validate(Element,State,[])
validate(Xml,State) ->
    validate(Xml,State,[]).

%% @spec validate(Element,State,Options) -> Result
%%       Element      = XmlElement
%%       Options      = option_list() 
%%       Result       = {ValidElement,global_state()} | {error,Reasons}
%%       ValidElement = XmlElement
%%       State        = global_state()
%%       Reasons      = [ErrorReason] | ErrorReason
%% @doc Validates a parsed well-formed XML element (Element).
%% <p>A call to validate/2 or validate/3 must provide a well formed 
%% parsed XML element <code>#xmlElement{}</code> and a State,
%% <code>global_state()</code>, which holds necessary information from
%% an already processed schema.
%% Thus validate enables reuse of the schema information and
%% therefore if one shall validate several times towards the same
%% schema it reduces time consumption.</p>
%% <p>The result, ValidElement, is the valid element that conforms to the 
%% post-schema-validation infoset. When the validator finds an error it
%% tries to continue and reports a list of all errors found. In those cases
%% an unexpected error is found it may cause a single error reason.
%% </p>
%% <p> Usage example:</p>
%% <p>
%% <code>1>{E,_} = xmerl_scan:file("my_XML_document.xml").</code><br/>
%% <code>2>{ok,S} = xmerl_xsd:process_schema("my_XML_Schema.xsd").</code><br/>
%% <code>3>{E2,_} = xmerl_xsd:validate(E,S).</code>
%% </p>
%% <p> Observe that E2 may differ from E if for instance there are default
%% values defined in <code>my_XML_Schema.xsd</code>.</p>
validate(Xml,State,Opts) when is_record(State,xsd_state) ->
    S2 = initiate_state2(State,Opts),
    S3 = validation_options(S2,Opts),
    validate3(S3#xsd_state.schema_name,Xml,S3).

%% @spec state2file(State) -> ok | {error,Reason}
%% @doc Same as state2file(State,SchemaName)
%%
%% The name of the saved file is the same as the name of the
%% schema, but with <code>.xss</code> extension.
state2file(S=#xsd_state{schema_name=SN}) ->
    state2file(S,filename:rootname(SN)).

%% @spec state2file(State,FileName) -> ok | {error,Reason}
%%       State = global_state()
%%       FileName = string()
%% @doc Saves the schema state with all information of the processed
%% schema in a file. You can provide the file name for the saved
%% state. FileName is saved with the <code>.xss</code> extension
%% added.
state2file(S,FileName) when is_record(S,xsd_state) ->
    save_xsd_state(S),
    case catch ets:tab2file(S#xsd_state.table,lists:append(FileName,".xss")) of
	{'EXIT',Reason} ->
	    {error,{[],?MODULE,Reason}};
	Ret -> Ret
    end.

%% @spec file2state(FileName) -> {ok,State} | {error,Reason}
%%       State = global_state()
%%       FileName = string()
%% @doc Reads the schema state with all information of the processed
%% schema from a file created with <code>state2file/[1,2]</code>.  The
%% format of this file is internal. The state can then be used
%% validating an XML document.
file2state(FileName) ->
    case catch ets:file2tab(FileName) of
	{ok,Tab} ->
	    case load_xsd_state(Tab) of
		[{state,S}] when is_record(S,xsd_state) ->
		    xmerl_xsd_vsn_check(S);
%%		    {ok,S};
		Other ->
		    {error,{[],?MODULE,{incomplete_file,FileName,Other}}}
	    end;
	{error,Reason} ->
	    {error,{[],?MODULE,Reason}};
	Other ->
	    {error,{[],?MODULE,Other}}
    end.

save_xsd_state(S) ->
    catch ets:insert(S#xsd_state.table,{state,S}).
load_xsd_state(Table) ->
    catch ets:lookup(Table,state).
   
xmerl_xsd_vsn() ->
    case lists:keysearch(vsn,1,xmerl_xsd:module_info(attributes)) of
	{value,{_,MD5_VSN}} ->
	    MD5_VSN;
	_ ->
	    undefined
    end.
xmerl_xsd_vsn_check(S=#xsd_state{vsn=MD5_VSN}) ->
    case [V||{vsn,V}<-xmerl_xsd:module_info(attributes)] of
	[MD5_VSN] ->
	    {ok,S};
	_ ->
	    {error,{[],?MODULE,{different_version_of_xmerl_xsd_module_used,
		    state_not_reliable}}}
    end.
    
	

%% @spec process_validate(Schema,Element) -> Result
%% @equiv process_validate(Schema,Xml,[])
process_validate(Schema,Xml) ->
    process_validate(Schema,Xml,[]).
%% @spec process_validate(Schema,Element,Options) -> Result
%%       Schema   = string()
%%       Element  = XmlElement
%%       Options  = option_list()
%%       Result   = {ValidXmlElement,State} | {error,Reason}
%%       Reason   = [ErrorReason] | ErrorReason
%% @doc Validates a parsed well-formed XML element towards an XML
%% schema.  <p> Validates in two steps. First it processes the schema,
%% saves the type and structure info in an ets table and then
%% validates the element towards the schema.</p>
%% <p> Usage example:</p>
%% <p>
%% <code>1>{E,_} = xmerl_scan:file("my_XML_document.xml").</code><br/>
%% <code>2>{E2,_} = xmerl_xsd:validate("my_XML_Schema.xsd",E).</code>
%% </p>
%% <p> Observe that E2 may differ from E if for instance there are default
%% values defined in <code>my_XML_Schema.xsd</code>.</p>
process_validate(Schema,Xml,Opts) ->
    TargetNamespace = target_namespace(Xml),
    case Schema of
	[H|_] when is_list(H); is_tuple(H) ->
	    case process_schemas(Schema,
				 [{target_namespace,TargetNamespace}|Opts]) of
		{ok,S} ->
		    S2 = validation_options(S,Opts),
		    validate3(S2#xsd_state.schema_name,Xml,S2);
		Err ->
		    Err
	    end;
	_ ->
	    process_validate2(xmerl_scan:file(Schema),Schema,Xml,Opts)
    end.

process_validate2(Err={error,_},_,_,_) ->
    Err;
process_validate2({SE,_},Schema,Xml,Opts) ->
    S = initiate_state(Opts,Schema),
    S1 = validate_schema(SE,S),
    S2 = validate_schema_ph2(S1),
    S3 = schema_concistence_checks(S2),
    S4 = validation_options(S3,Opts),
    validate3(Schema,Xml,S4).

validate3(Schema, Xml,S =#xsd_state{errors=[]}) -> 
    Ret = {_, S2} = 
	case catch validate_xml(Xml, S) of
	    _Err = {error, Reason} ->
		{Xml, acc_errs(S, Reason)};
	    {'EXIT', Reason} ->
		{Xml, acc_errs(S, {error_path(Xml, Xml#xmlElement.name), ?MODULE,
				 {undefined, {internal_error, Reason}}})};
	    {XML2, Rest, Sx} ->
		case lists:dropwhile(fun(X) when is_record(X, xmlComment) -> true; (_) -> false end, Rest) of
		    [] ->
			case XML2 of
			    [XML3] ->
				{XML3,Sx};
			    XML3 ->
				{XML3,Sx}
			end;
		    UnValidated ->
			{Xml,acc_errs(Sx,{error_path(UnValidated,Xml#xmlElement.name),?MODULE,
					  {unvalidated_rest,UnValidated}})}
		end
	end,
    save_to_file(S2,filename:rootname(Schema)++".tab2"),
    case S2#xsd_state.errors of
	[] ->
	    Ret;
	L ->
	    %%delete_table(S2),
	    return_error(L)
    end;
validate3(_,_,S) ->
    return_schema_error(S#xsd_state.errors).

%% @spec process_schema(Schema) -> Result
%% @equiv process_schema(Schema,[])
process_schema(Schema) ->
    process_schema(Schema,[]).
%% @spec process_schema(Schema,Options) -> Result
%%       Schema  = string()
%%       Result  = {ok,State} | {error,Reason}
%%       State   = global_state()
%%       Reason  = [ErrorReason] | ErrorReason
%%       Options = option_list()
%% @doc Reads the referenced XML schema and checks that it is valid.
%% Returns the <code>global_state()</code> with schema info or an 
%% error reason. The error reason may be a list of several errors
%% or a single error encountered during the processing.
process_schema(Schema,Options) when is_list(Options) ->
    State = initiate_state(Options,Schema),
    process_schema(Schema, State);
process_schema(Schema, State=#xsd_state{fetch_fun=Fetch})->
    case Fetch(Schema, State) of
	{ok,{file,File},_} ->
	    process_schema2(xmerl_scan:file(File), State, Schema);
	{ok,{string,Str},_} ->
	    process_schema2(xmerl_scan:string(Str), State, Schema);
	{ok,[],_} ->
	    {error,enoent};
	Err ->
	    Err
    end.

process_schema2(Err={error,_},_,_) ->
    Err;
process_schema2({SE,_},State,_Schema) ->
    S1 = validate_schema(SE,State),
    S2 = validate_schema_ph2(S1),
    case schema_concistence_checks(S2) of
	S3 = #xsd_state{errors=[]} ->
	    {ok,S3};
	S3 ->
	    delete_table(S3),
	    return_error(S3#xsd_state.errors)
    end.

%% @spec process_schemas(Schemas) -> Result
%% @equiv process_schema(Schemas,[])
process_schemas(Schemas) ->
    process_schemas(Schemas,[]).
%% @spec process_schemas(Schemas,Options) -> Result
%%       Schemas  = [{NameSpace,string()}|Schemas] | []
%%       Result   = {ok,State} | {error,Reason}
%%       Reason   = [ErrorReason] | ErrorReason
%%       Options  = option_list()
%% @doc Reads the referenced XML schemas and controls they are valid.
%% Returns the <code>global_state()</code> with schema info or an 
%% error reason. The error reason may be a list of several errors
%% or a single error encountered during the processing.
process_schemas(Schemas=[{_,Schema}|_],Options) when is_list(Options) ->
    State = initiate_state(Options,Schema),
    process_schemas(Schemas, State);
process_schemas([{_NS,Schema}|Rest],State=#xsd_state{fetch_fun=Fetch}) ->
    Res=
    case Fetch(Schema,State) of
	{ok,{file,File},_} ->
	    process_schema2(xmerl_scan:file(File),State,Schema);
	{ok,{string,Str},_} ->
	    process_schema2(xmerl_scan:string(Str),State,Schema);
	{ok,[],_} ->
	    {ok,State};
	Err ->
	    Err
    end,
    case Res of
	{ok,S2} ->
	    process_schemas(Rest,S2);
	_ ->
	    Res
    end;
process_schemas([],S) when is_record(S,xsd_state) ->
    {ok,S}.

initiate_state(Opts,Schema) ->
    XSDBase = filename:dirname(Schema),
    {{state,S},RestOpts}=new_state(Opts),
    S2 = create_tables(S),
    initiate_state2(S2#xsd_state{schema_name = Schema, xsd_base=XSDBase,
				 fetch_fun = fun fetch/2}, 
		    RestOpts).

initiate_state2(S,[]) ->
    S;
initiate_state2(S,[{tab2file,Bool}|T]) ->
    initiate_state2(S#xsd_state{tab2file=Bool},T);
initiate_state2(S,[{xsdbase, XSDBase}|T]) ->
    initiate_state2(S#xsd_state{xsd_base=XSDBase, external_xsd_base=true},T);
initiate_state2(S,[{fetch_fun,FetchFun}|T]) ->
    initiate_state2(S#xsd_state{fetch_fun=FetchFun},T);
initiate_state2(S,[{fetch_path,FetchPath}|T]) ->
    initiate_state2(S#xsd_state{fetch_path=FetchPath},T);
initiate_state2(S,[{schema_preprocessed,Bool}|T]) ->
    initiate_state2(S#xsd_state{schema_preprocessed=Bool},T);
initiate_state2(S,[{target_namespace,_NS}|T]) ->
%%    initiate_state2(S#xsd_state{targetNamespace=if_list_to_atom(NS)},T);
    initiate_state2(S,T); %% used in validation phase
initiate_state2(S,[H|T]) ->
    error_msg("~w: invalid option: ~p~n",[?MODULE, H]),
    initiate_state2(S,T).

validation_options(S,[{target_namespace,NS}|T]) ->
    validation_options(S#xsd_state{targetNamespace=if_list_to_atom(NS)},T);
validation_options(S,[_H|T]) ->
    validation_options(S,T);
validation_options(S,[]) ->
    S.

new_state(Opts) ->
    XSD_VSN = xmerl_xsd_vsn(),
    keysearch_delete(state,1,Opts,{state,#xsd_state{vsn=XSD_VSN}}).


%% validate_schema/2 traverses the shema element to save necessary
%% information as defined elements and types.
validate_schema(E=#xmlElement{},
		    S) ->
    %% namespace is always a xmlNamespace record, attributs a list of
    %% #xmlAttributes and content a list of #xmlElements|#xmlText|...

    %% Have to save namespace nodes. Use of namespace in paths for
    %% unique,key and keyref are used after the schema is processed.

    S1 = S#xsd_state{targetNamespace=target_namespace(E)},
    case is_already_processed(S1#xsd_state.targetNamespace,S1) of
	true ->
	    save_namespace_definition(S1#xsd_state.targetNamespace,S1);
	_ ->
	    S2 = S1,%save_namespace_definition(S1#xsd_state.targetNamespace,S1),
	    {CM,S3} = traverse_content(E,S2),
	    save_schema_element(CM,S3),
	    S3
    end.

validate_schema_ph2(S=#xsd_state{derived_types=[]}) ->
    S;
validate_schema_ph2(S=#xsd_state{derived_types=DT}) ->
    deduce_derived_types(DT,S).
    
%% traverse_content/2 creates the content model of the schema.
%% content model depends on (1) the type: 
%% complex type:
%%       sequence, choice, all
%% simple type: no content other than characters
%% (2) minOccurs/maxOccurs attributes.
%% The schema for schemas content model is:
%% schema:	  ((include | import | redefine | annotation)*, 
%%		  (((simpleType | complexType | group | attributeGroup)
%%		   | element | attribute | notation), annotation*)*)
%% attribute:	  (annotation?, simpleType?)
%% element:	  (annotation?, ((simpleType | complexType)?, (unique |
%%		   key | keyref)*))
%% complexType:	  (annotation?, (simpleContent | complexContent |
%%		  ((group | all | choice | sequence)?,
%%		  ((attribute | attributeGroup)*,anyAttribute?))))
%% attributeGroup:(annotation?,
%%                 ((attribute | attributeGroup)*, anyAttribute?))
%% group:	  (annotation?, (all | choice | sequence)?)
%% all:		  (annotation?, element*)
%% sequence:	  (annotation?,
%%                 (element | group | choice | sequence | any)*)
%% choice:	  (annotation?, (element | group | choice | sequence |
%%		   any)*)
%% any:		  (annotation?) any wellformed xml inside "any"
%% unique:	  (annotation?, (selector, field+))
%% key:		  (annotation?, (selector, field+))
%% keyref:	  (annotation?, (selector, field+))
%% selector:	  (annotation?)
%% field:	  (annotation?)
%% notation:	  (annotation?)
%% annotation:	  (appinfo | documentation)*
%% appinfo:	  ({any})*
%% documentation: ({any})*
%% simpleType:	  (annotation?, (restriction | list | union))
%% restriction:	  (annotation?, (simpleType?, (minExclusive | 
%%		   minInclusive | maxExclusive | maxInclusive | 
%%		   totalDigits | fractionDigits | length | minLength | 
%%		   maxLength | enumeration | whiteSpace | pattern)*))
%% list:	  (annotation?, simpleType?)
%% union:	  (annotation?, simpleType*)
%% include:	  (annotation?)
%% import:	  (annotation?)
%% redefine:	  (annotation | (simpleType | complexType | group |
%%		   attributeGroup))*
traverse_content(E=#xmlElement{name=Name},S) ->
    case local_name(Name) of
	schema ->
	    Content = E#xmlElement.content,
%%	    S1 = S#xsd_state{targetNamespace=target_namespace(E)},
	    ThisNS = {"#this#",S#xsd_state.schema_name,
		      S#xsd_state.targetNamespace},
	    S2 = S#xsd_state{checked_namespace_nodes=
			      add_once(ThisNS,S#xsd_state.checked_namespace_nodes)},
	    S3 = namespace_nodes(E,S2),
	    S4 = element_form_default(E,S3),
	    S5 = attribute_form_default(E,S4),
	    S6 = substitution_default(finalDefault,E,S5),
	    S7 = substitution_default(blockDefault,E,S6),
	    traverse_content2(Content,S7,[]);
	Err -> 
	    exit({error,{[],?MODULE,{schema_error,Err}}})
    end.
    

traverse_content2([],S,Acc) ->
    {reverse(remove_annotation(Acc)),reset_scope(S)};
traverse_content2([El|Els],S,Acc) when is_record(El,xmlElement) ->
    %% element declaration: save name, type, scope.
    {Object,S2} = element_content(kind(El,S),El,S#xsd_state.scope),%% Object={Kind,Obj}
    traverse_content2(Els,S2,[Object|Acc]);
traverse_content2([_T|Els],S,Acc) -> %% xmlText,xmlPI ...
    traverse_content2(Els,S,Acc).

target_namespace(E) ->
    case get_attribute_value(targetNamespace,E,undefined) of
	URI when is_list(URI) ->
	    list_to_atom(URI);
	URI ->
	    URI
    end.

%% namespace_nodes/2 ->
%%     NS.
namespace_nodes(#xmlElement{namespace=#xmlNamespace{nodes=NS}},
		S=#xsd_state{namespace_nodes=NSN,
			     global_namespace_nodes=GNSN}) ->
    S2 =S#xsd_state{namespace_nodes=foldl(fun add_once/2,NSN,NS)},
    S2#xsd_state{global_namespace_nodes=
		 add_key_once(S#xsd_state.targetNamespace,1,
			      {S#xsd_state.targetNamespace,NS},
			      GNSN)}.

attribute_form_default(#xmlElement{attributes=Atts},S)->
    Def=form_default(attributeFormDefault,Atts,S),
    S#xsd_state{attributeFormDefault=Def}.
element_form_default(#xmlElement{attributes=Atts},S) ->
    Def=form_default(elementFormDefault,Atts,S),
    S#xsd_state{elementFormDefault=Def}.
form_default(Key,Atts,_S) ->
    case keyNsearch(Key,#xmlAttribute.name,Atts,unqualified) of
	#xmlAttribute{value=V} when is_list(V) -> list_to_atom(V);
	#xmlAttribute{value=V} ->V;
	 _-> unqualified
    end.

substitution_default(Subst = finalDefault,El,S) ->
    S#xsd_state{finalDefault = substitution(Subst,El,S)};
substitution_default(Subst = blockDefault,El,S) ->
    S#xsd_state{blockDefault = substitution(Subst,El,S)}.
substitution(Subst,El,_S) ->
    split_by_whitespace(get_attribute_value(Subst,El,[]),[]).
    
	 
%% element_content may be one of: annotation, type def(simple or
%% complex), import, unique, key, keyref, attribute def, attribute
%% group, all, group, complexContent, simpleContent, choice, sequence
element_content({attribute,S=#xsd_state{scope=Scope}},Att,Env) ->
    case qualify_NCName(Att,S) of
	no_name ->
	    Ref = attribute_ref(Att),
	    AttRef = 
		{attribute,get_QName(Ref,Att#xmlElement.namespace, %%QQQ
				     reset_scope(S))},
	    {AttRef,add_ref(S,AttRef)};
	Name ->
	    {AttrType,S2} = attribute_type(Att,[Name|Env],S),
	    S3 = check_cm(attribute,allowed_content(attribute,Env),AttrType,S2),
	    {Attr,S4} = attribute_properties(Att#xmlElement.attributes,
					#schema_attribute{type=AttrType},S3),
	    Object = {attribute,
		      Attr#schema_attribute{name=Name,scope=Scope}},
	    S5 = save_object(Object,S4),
	    {{attribute,Name},S5}
    end;
element_content({element,S},El,Env) ->
    %% The type of an element may be a simple or complex type (named
    %% or anonymous), a referenced name or member of a substitution group.
    case qualify_NCName(El,S) of
	no_name ->
	    Ref = particle_ref(El),
	    {Occ,S2} = occurance(El,{1,1},S),
	    %% 3.3.3 bullet 2.2
	    S3 = element_forbidden_properties(El,S2),
	    S4 = element_forbidden_content(El#xmlElement.content,S3),
	    ElRef =
		{element,
		 {get_QName(Ref,El#xmlElement.namespace,reset_scope(S)),
		  Occ}},
	    {ElRef,add_ref(S4,ElRef)};
	Name ->
	    {Type,S2} = element_type(El,[Name|Env],S),
	    S3 = check_cm(element,allowed_content(element,Env),Type,S2),
	    Type2 = remove_annotation(Type),
	    Unique = [X||X={unique,_} <- Type2],
	    Key = [X||X={K,_} <- Type2,K == key orelse K==keyref],
	    {Occur,S4} = occurance(El,{1,1},S3),
	    {SE,S5} = element_properties(El#xmlElement.attributes,
					 #schema_element{},El,S4),
	    CM = remove_attributes([X||X={Y,_}<-Type2,
				       unique=/=Y,key=/=Y,
				       keyref=/=Y,annotation=/=Y]),
	    %% take care of key/keyref later
	    SE2 = SE#schema_element{name=Name,type=CM,uniqueness=Unique,
				    key=Key, occurance=Occur,
				    scope=S5#xsd_state.scope},
	    S6 = insert_substitutionGroup(SE2,S5),
	    S7 = save_object({element,SE2},S6),
	    {{element,{Name,Occur}},S7}
    end;
element_content({complexType,S},CT,Env) ->
    %% complex type definition without a name is returnd and added to
    %% the content model at this level. A complex type may also contain
    %% attributes or attribute group references in the end of its content.
    %%?debug("complexType content: ~p~nenv: ~p~n",[CT,Env]),
    {SCT,S1} = c_t_properties(CT,#schema_complex_type{},S),
    {Mixed,S2} = mixed(CT,S1),
    Complexity = complexity(CT#xmlElement.content),
    {Object,Name,S7} =
    case qualify_NCName(CT,S2) of
	no_name ->
	    {CM,S3} = type(CT#xmlElement.content,
			   in_scope(anonymous,S2),[complexType|Env]),
	    S4 = check_cm(complexType,allowed_content(complexType,Env),CM,S3),
	    Name1 = get_QName('_xmerl_no_name_',CT#xmlElement.namespace,S4),
	    S5 = set_scope(S#xsd_state.scope,S4),	    
	    {Content,Attributes}=split_content(remove_annotation(CM)),
	    SCT2 = base_type(Content,SCT),
	    CTObj =
		{complexType,
		 SCT2#schema_complex_type{name=Name1,
					  scope=S5#xsd_state.scope,
					  attributes=Attributes,
					  complexity=Complexity,
					  content=mixify(Mixed,Content)}},
	    {CTObj,Name1,S5};
        Name2 ->
	    S3 = in_scope(Name2,S2),
	    S3a = push_circularity_mark({typeDef,Name2},S3),
	    {CM,S4} = type(CT#xmlElement.content,S3a,
				[complexType|Env]),
	    S4a = pop_circularity_mark({typeDef,Name2},S4),
	    S5 = check_cm(complexType,allowed_content(complexType,Env),
			  CM,S4a),
	    S6 = set_scope(S#xsd_state.scope,S5),
	    {Content,Attributes}=split_content(remove_annotation(CM)),
	    SCT2 = base_type(Content,SCT),
	    {{complexType,
	      SCT2#schema_complex_type{name=Name2,
				      scope=S6#xsd_state.scope,
				      attributes=Attributes,
				       complexity=Complexity,
				      content=mixify(Mixed,Content)}},
	     Name2,S6}
    end,
    S8 = save_object(Object,S7),
    S9 = derived_type(Object,S8),
    {{complexType,Name},S9};
element_content({attributeGroup,S},AG,Env) ->
    %% an attribute group always have a name or a ref, the content is
    %% (annotation?,(attribute | attributGroup)*, anyAttribute?).
    case qualify_NCName(AG,S) of
	no_name ->
	    %% an attribute group ref inside complex type def or attr
	    %% group def ( XSD1:3.6.2).
	    Ref = attributeGroup_ref(AG),
	    AGRef =
		{attributeGroup,get_QName(Ref,AG#xmlElement.namespace,%%QQQ
					  reset_scope(S))},
	    {AGRef,add_ref(S,AGRef)};
	Name ->
	    %% must occur on top level of schema( XSD1:3.6.2). The
	    %% only thing needed in content are the names of all
	    %% attributes or referenced attribute groups.
	    {CM,S2} = type(AG#xmlElement.content,in_scope(Name,S),
			       [attributeGroup|Env]),
	    S2_1 = out_scope(Name,S2),
	    S3 = check_cm(attributeGroup,allowed_content(attributeGroup,Env),CM,S2_1),
	    S4 = save_object({attributeGroup,
			 #schema_attribute_group{name=Name,
						 content=keep_attributes(CM)}},S3),
	    {{attributeGroup,Name},S4}
    end;
element_content({group,S},G,Env) ->
    %% a model group associates a name with a content model. It can be
    %% a reference or a definition.
    %% content is one of all, choice or sequence.
    case qualify_NCName(G,S) of
	no_name -> % reference.
	    %% If reference is a recursive ref to a group with the
	    %% same name as this group points at the redefined valid
	    %% schema group. See XMLSchema part 1, section 4.2.2
	    %% "Schema Representation Constraint: Individual Component
	    %% Redefinition"
	    Ref = particle_ref(G),
	    {Occur,S2} = occurance(G,{1,1},S),
	    GRef =
		{group,
	      {get_QName(Ref,G#xmlElement.namespace,reset_scope(S2)),%%QQQ
	       Occur}},
	    {GRef,add_ref(S2,GRef)};
	Name -> % definition, always schema or redefine as parent
	    {CM,S2} = type(G#xmlElement.content,in_scope(Name,S),[group|Env]),
	    CM2 = recursive_redefine(Name,CM,S2),
	    S2_1 = out_scope(Name,S2),
	    S3 = check_cm(group,allowed_content(group,Env),CM2,S2_1),
	    S4 = save_object({group,#schema_group{name=Name,
					     content=remove_annotation(CM2)}},S3),
	    {{group,Name},S4}
    end;
element_content({all,S},All,Env) ->
    %% each element occurs 0 or 1 times in any order
    %% {all,[{element_name,occurance}]}
%%    CM = content_model(Seq#xmlElement.content,S,[all|Env]),
    {Occur,S1} = occurance(All,{1,1},S),
    {CM,S2} = type(All#xmlElement.content,S1,[all|Env]),
    S3 = check_cm(all,allowed_content(all,Env),CM,S2),
    {{all,{[X||X = {element,_} <- CM],Occur}},S3};
element_content({sequence,S},Seq,Env) ->
    %% {sequence,[{element_name,occurance}]}
%%    CM = content_model(Seq#xmlElement.content,S,[sequence|Env]),
    {Occur,S1} = occurance(Seq,{1,1},S),
    {CM,S2} = type(Seq#xmlElement.content,S1,[sequence|Env]),
    S3 = check_cm(sequence,allowed_content(sequence,Env),CM,S2),
    {{sequence,{remove_annotation(CM),Occur}},S3};
element_content({choice,S},Choice,Env) ->
    %% allowed content: (annotation?,
    %%                   (element | group | choice | sequence | any)*)
    %% returns: {choice,[element_name]}
%%    CM = content_model(Choice#xmlElement.content,S,[choice|Env]),
    {Occur,S1} = occurance(Choice,{1,1},S),
    {CM,S2} = type(Choice#xmlElement.content,S1,[choice|Env]),
    S3 = check_cm(choice,allowed_content(choice,Env),CM,S2),
    {{choice,{remove_annotation(CM),Occur}},S3};
element_content({any,S},Any,_Env) ->
    {Occur,S1} = occurance(Any,{1,1},S),
    NameSpace = wildcard_namespace(Any,S1),
    PC = processor_contents(Any),
    ?debug("element_content, any: Any content:~p~n",[Any#xmlElement.content]),
    Pred = fun(E=#xmlElement{}) -> case kind(E) of
				       annotation -> false;
				       _ -> true
				   end;
	      (_) ->
		   false
	   end,
    S2 = case filter(Pred,Any#xmlElement.content) of
	     [] -> S1;
	     Err -> %% report error
		 acc_errs(S1,{error_path(Any,Any#xmlElement.name),?MODULE,
			      {unexpected_content_in_any,Err}})
	 end,
    {{any,{NameSpace,Occur,PC}},S2};
element_content({IDC,S},El,Env) 
  when IDC==unique;IDC==key;IDC==keyref->
    QName = qualify_NCName(El,reset_scope(S)),
    Ref = keyrefer(IDC,El,S),
    {SelField,S2} = type(El#xmlElement.content,S,[IDC|Env]),
    case {[X||X={selector,_} <- SelField],[X||X={field,_} <- SelField]} of
	{[Sel],Fields=[_H|_T]} ->
	    IDConstr = #id_constraint{category=IDC,name=QName,refer=Ref,
				      selector=Sel,fields=Fields},
	    S3=save_idc(IDC,IDConstr,S2),
	    {{IDC,IDConstr},S3};
	Err ->
	    S3 = acc_errs(S2,{error_path(El,El#xmlElement.name),?MODULE,
			      {erroneous_content_in_identity_constraint,IDC,Err}}),
	    {{IDC,[]},S3}
    end;
element_content({selector,S},Sel,_Env) ->
    case get_attribute_value(xpath,Sel,error) of
	error -> 
	    S2 = acc_errs(S,{error_path(Sel,Sel#xmlElement.name),?MODULE,
			     {missing_xpath_attribute,selector}}),
	    {{selector,[]},S2};
	XPath ->
	    {{selector,XPath},S}
    end;
element_content({field,S},F,_Env) ->
    case get_attribute_value(xpath,F,error) of
	error -> 
	    S2 = acc_errs(S,{error_path(F,F#xmlElement.name),?MODULE,
			     {missing_xpath_attribute,field}}),
	    {{field,[]},S2};
	XPath ->
	    {{field,XPath},S}
    end;
element_content({notation,S},_N,_Env) ->
    {{notation,[]},S};
element_content({annotation,S},_Ann,_Env) ->
    {{annotation,[]},S};
element_content({appinfo,S},_AI,_Env) ->
    {{appinfo,[]},S};
element_content({documentation,S},_D,_Env) ->
    {{documentation,[]},S};
element_content({simpleType,S},ST,Env) ->
    Name = case qualify_NCName(ST,S) of
	       no_name ->
		   get_QName('_xmerl_no_name_',ST#xmlElement.namespace,
			     in_scope('_xmerl_no_name_',S));%%---
	       QName ->
		   QName
	   end,
    {Type,S2} = type(ST#xmlElement.content,
		     push_circularity_mark({typeDef,Name},in_scope(Name,S)),
		      [simpleType|Env]),
    S2_1 = pop_circularity_mark({typeDef,Name},S2),
    S3 = set_scope(S#xsd_state.scope,S2_1),
    S4 = check_cm(simpleType,allowed_content(simpleType,Env),Type,S3),
    {BaseType,Facets} = facets(Type,S4),
    Variety = variety(Type),
    Final = simpleType_final(ST,S4),
    Object = {simpleType,#schema_simple_type{name=Name,
					     base_type=BaseType,
					     final=Final,
					     facets=Facets,
					     variety=Variety,
					     content=remove_annotation(Type),
					     scope=S4#xsd_state.scope}},
    S5 = save_object(Object,S4),
    S6 = derived_type(Object,S5),
    {{simpleType,Name},S6};
element_content({restriction,S},R,Env) ->
    %% If complexContent, all element definitions of base type must be
    %% repeated. However, attributes are inherited.
    %% possible parents are simpleType or complexType (grand parent)
    %% If parent is simpleType the base type is either the attribute
    %% base (resolved by base_type/1) or the type defined in content. 
    {CM,S2} = type(R#xmlElement.content,S,[restriction|Env]),
    S3 = check_cm(restriction,allowed_content(restriction,Env),CM,S2),
    {BaseTypeName,CM2,S4} = restriction_base_type(R,CM,S3), %% a QName
%%    S5 = add_circularity_mark(BaseTypeName,S4),
    BaseTypeType = base_type_type(Env),
    {{restriction,{BaseTypeName,remove_annotation(CM2)}},
     add_ref(S4,{BaseTypeType,BaseTypeName})}; %% Does not return name but content model
element_content({list,S=#xsd_state{scope=Scope}},L,Env) ->
    {Type,S2} = list_type(L,S,[list|Env]),
    S3 = check_cm(list,allowed_content(list,Scope),Type,S2),
    {{list,remove_annotation(Type)},S3};
element_content({union,S=#xsd_state{scope=Scope}},U,Env) ->
    {Types,S2} = union_types(U,S,[union|Env]),
    S3 = check_cm(union,allowed_content(union,Scope),Types,S2),
    {{union,Types},S3};
element_content({include,S=#xsd_state{schema_name=ThisSchema,
				      targetNamespace=TNS}},I,_Env) ->
    S2 = process_external_schema_once(I,S#xsd_state.targetNamespace,S),
    {{include,[]},S2#xsd_state{schema_name=ThisSchema,targetNamespace=TNS}};
element_content({import,S=#xsd_state{schema_name=ThisSchema,
				     targetNamespace=ThisNameS}},I,_Env) ->
    %% import unlike include and redefine may include definitions from
    %% other namespaces than the target namespace of the including
    %% schema.

    %% namespace and schemaLocation
    Namespace = 
	case get_attribute_value(namespace,I,undefined) of
	    L when is_list(L) ->
		list_to_atom(L);
	    A -> A
	end,
    %% If Namespace is absent, then the import allows unqualified
    %% reference to components with no target namespace.

    SchemaLocation = get_attribute_value(schemaLocation,I,absent),
    %% If SchemaLocation is absent, the identification of that schema
    %% is leaved to the instance, application or user, via the
    %% mechanisms described §4.3 in XML Schema Part 1.
    
    S2 = process_external_schema_once(SchemaLocation,Namespace,S),
    {{import,[]},S2#xsd_state{schema_name=ThisSchema,
			      targetNamespace=ThisNameS}};
element_content({redefine,S=#xsd_state{schema_name=ThisSchema}},RD,Env) ->
    %% Must be a child of "schema" element
    %% redefine of simple and complex types, groups and attribute
    %% groups obtained from external files.
    %% Brings in all definitions of external schema and redefines one.
    %% External schema must be in same namespace as current schema or
    %% no namespace.
    S2 = process_external_schema_once(RD,S#xsd_state.targetNamespace,
				      S#xsd_state{errors=[]}),
    case S2#xsd_state.errors of
	[] ->
%%	    RedefSource = S2#xsd_state.schema_name,
	    S3 = S2#xsd_state{schema_name=ThisSchema,
%%			      global_element_source=add_once({ThisSchema,RedefSource},GES),
			      errors=S#xsd_state.errors},
	    {CM,S4} = type(RD#xmlElement.content,
				 S3#xsd_state{redefine=true},[redefine|Env]),
	    S5 = S4#xsd_state{redefine=false},
	    S6 = check_cm(redefine,allowed_content(redefine,Env),CM,S5),
	    S7 = redefine(CM,S6),	    
	    {{redefine,[]},S7};
	Errs ->
	    S3 = S2#xsd_state{schema_name=ThisSchema,
			      errors=Errs++S#xsd_state.errors},
	    {{redefine,[]},S3}
    end;
element_content({anyAttribute,S},AA,_Env) ->
    %% has attributes processContents = (lax | skip | strict) : strict
    %% namespace = ((##any | ##other) | 
    %%               List of (anyURI | (##targetNamespace | ##local)) )  : ##any
    
    NameSpace = wildcard_namespace(AA,S),
    PC = processor_contents(AA),
    Pred = fun(E=#xmlElement{}) -> case kind(E) of
			 annotation -> false;
			 _ -> true
		     end;
	      (_) -> false
	   end,
    S2 =
	case filter(Pred,AA#xmlElement.content) of
	    [] -> S;
	    Err -> %% report error
		acc_errs(S,{error_path(AA,AA#xmlElement.name),?MODULE,
			    {content_in_anyAttribute,Err}})
	end,
    {{anyAttribute,{NameSpace,PC}},S2};
element_content({simpleContent,S},SC,Env) ->
    %% only as child of complexType.
    %% allowed content: (annotation?, (restriction | extension))
    S2 = pre_check_cm(simpleContent,SC#xmlElement.content,mk_name(S#xsd_state.scope),S),
    case filter(fun(X=#xmlElement{}) -> 
			case kind(X) of
			    restriction -> true;
			    extension -> true;
			    _ -> false
			end;
		   (_) -> false
		end,
		SC#xmlElement.content) of
	[E] ->
	    element_content(kind(E,S2),E,[simpleContent|Env]);
	Err ->
	    {[],acc_errs(S2,{error_path(SC,SC#xmlElement.name),?MODULE,
			     {content_in_simpleContent,Err}})}
    end;
element_content({complexContent,S},CC,Env) ->
    S2 = pre_check_cm(complexContent,CC#xmlElement.content,
		      mk_name(S#xsd_state.scope),S),
    %% the mixed attribute was fetched in the complexType element that
    %% held this complexContent
    case filter(fun(X=#xmlElement{}) -> case kind(X) of
					    restriction -> true;
					    extension -> true;
					    _ -> false
					end;
		   (_) -> false
		end,CC#xmlElement.content) of
	[E] -> 
	    element_content(kind(E,S2),E,[complexContent|Env]);
	Err ->
	    {[],acc_errs(S2,{error_path(CC,CC#xmlElement.name),?MODULE,
			     {complexContent_content_failure,Err}})}
    end;
element_content({extension,S},Ext,Env) ->
    %% may be used in both simple and complex content with different
    %% content allowed.
    %% this should be returned and checked for allowed content in
    %% parent, but we don't know if base type is a forward reference.
    BaseType = base_type(Ext),
    {CM,S2} = type(Ext#xmlElement.content,S,[extension|Env]),
    S3 = check_cm(extension,allowed_content(extension,S#xsd_state.scope),CM,S2),
    BaseTypeName = get_QName(BaseType,Ext#xmlElement.namespace,reset_scope(S)),%%QQQ
    BaseTypeType = base_type_type(Env),
    {{extension,{BaseTypeName,CM}},add_ref(S3,{BaseTypeType,BaseTypeName})};
%% The following are facets
element_content({minExclusive,S},CF,_Env) ->
    Value = get_value(CF),
    {{minExclusive,Value},S};
element_content({minInclusive,S},CF,_Env) ->
    Value = get_value(CF),
    {{minInclusive,Value},S};
element_content({maxExclusive,S},CF,_Env) ->
    Value = get_value(CF),
    {{maxExclusive,Value},S};
element_content({maxInclusive,S},CF,_Env) ->
    Value = get_value(CF),
    {{maxInclusive,Value},S};
element_content({totalDigits,S},CF,_Env) ->
    Value = get_value(CF),
    {{totalDigits,Value},S};
element_content({fractionDigits,S},CF,_Env) ->
    Value = get_value(CF),
    {{fractionDigits,Value},S};
element_content({length,S},CF,_Env) ->
    Value = get_value(CF),
    {{length,Value},S};
element_content({minLength,S},CF,_Env) ->
    Value = get_value(CF),
    {{minLength,Value},S};
element_content({maxLength,S},CF,_Env) ->
    Value = get_value(CF),
    {{maxLength,Value},S};
element_content({enumeration,S},CF,_Env) ->
    Value = get_value(CF),
    {{enumeration,Value},S};
element_content({whiteSpace,S},CF,_Env) ->
    Value = get_value(CF),
    {{whiteSpace,Value},S};
element_content({pattern,S},CF,_Env) ->
    Value = get_value(CF),
    {{pattern,Value},S};
element_content({Other,S=#xsd_state{errors=Errs}},C,_Env) ->
    case Errs of
	[] ->
	    {[],acc_errs(S,{error_path(C,C#xmlElement.name),?MODULE,
			    {unknown_content,Other}})};
	_ ->
	    {[],S}
    end.


type(C,S,Env) -> 
    type(C,S,Env,[]).
type([E=#xmlElement{}|Els],S,Env,Acc) -> 
    {CM,S2} = element_content(kind(E,S),E,Env),
    type(Els,set_scope(S#xsd_state.scope,S2),
	 Env,[CM|Acc]);
type([_H|Els],S,Env,Acc) ->
    type(Els,S,Env,Acc);
type([],S,_Env,Acc) ->
    {flatten(reverse(Acc)),S}.
    
simpleUrType() ->
    {anySimpleType,[]}.
%% simpleUrTypeRef() ->
%%     {anySimpleType,[],'http://www.w3.org/2001/XMLSchema'}.
urType() ->
    {anyType,[]}.


attribute_type(Att,Env=[Name|_],S) ->
    %% The attribute type may be referenced by the type attribute or
    %% explicitly defined as a simpleType inside the attribute
    %% element. In the latter case the type must be saved with the
    %% unique name of the scope and name attribute combined.
    {CM,S2} = type(Att#xmlElement.content,in_scope(Name,S),Env),
    case remove_annotation(CM) of
	[] ->
	    case keyNsearch(type,#xmlAttribute.name,
			    Att#xmlElement.attributes,[]) of
		#xmlAttribute{value=SimpleTypeName} -> %% a QName as string
		    %% This name may be a forward reference to a simple type.
		    TypeRef = {simpleType,get_QName(SimpleTypeName, %%QQQ
						    Att#xmlElement.namespace,
						    reset_scope(S))},
		    {[TypeRef],
		     set_scope(S#xsd_state.scope,add_ref(S2,TypeRef))};
		_ -> {[{simpleType,simpleUrType()}],
		      set_scope(S#xsd_state.scope,S2)}
	    end;
	Type  ->
	    {Type,set_scope(S#xsd_state.scope,S2)}
    end.

element_type(El,Env=[Name|_],S) ->
    %% In the top environment of the schema there may exist: global
    %% element declarations, substitution group members.
    %% Other element declarations are local
    {CM,S2} = type(El#xmlElement.content,in_scope(Name,S),Env),
    case remove_annotation(CM) of
	[] -> %% no simple or complex type definition
	    case {get_attribute_value(type,El,no_name),
		  get_attribute_value(substitutionGroup,El,undefined)} of
		{no_name,SGName} when is_list(SGName) ->
		    QN = get_QName(SGName,El#xmlElement.namespace,reset_scope(S)),%%QQQ
		    case is_simple_type(QN,S2) of
			true ->
			    exit(this_can_never_happen),
			    %% A substitutionGroup is an element, and
			    %% the type of this element is the
			    %% resolved type of the referenced
			    %% element.
			    TRef = {simpleType,QN},
			    {[TRef],
			     add_ref(set_scope(S#xsd_state.scope,S2),TRef)};
			_ ->
			    {[{substitutionGroup,QN}],
			     set_scope(S#xsd_state.scope,S2)}
		    end;
		{TName,_} when is_list(TName) ->
		    QN = get_QName(TName,El#xmlElement.namespace,reset_scope(S2)),%%QQQ
		    case is_simple_type(QN,S2) of
			true ->
			    TRef={simpleType,QN},
			    {[TRef],
			     add_ref(set_scope(S#xsd_state.scope,S2),TRef)};
			_ ->
			    TRef = {simple_or_complex_Type,QN},
			    {[TRef],
			     add_ref(set_scope(S#xsd_state.scope,S2),TRef)}
		    end;
		_ ->
		    case {get_attribute_value(ref,El,no_name),
			  is_global_env(Env)} of
			{Ref,false} when is_list(Ref) ->
			    %% a ref attribute references an element
			    {[{element,
			      get_QName(Ref,El#xmlElement.namespace,%%QQQ
					reset_scope(S))}],
			     set_scope(S#xsd_state.scope,S2)};
			_ ->
			    {[urType()],
			     set_scope(S#xsd_state.scope,S2)}
		    end
	    end;
%% 	Type ->
%% 	    {Type,set_scope(S#xsd_state.scope,S2)}
	_Type ->
	    {CM,set_scope(S#xsd_state.scope,S2)}
    end.

%% list_type/3 -> list() | name()
list_type(L,S,Env) ->
    case keyNsearch(itemType,#xmlAttribute.name,L#xmlElement.attributes,[]) of
	[] ->
%%	    {element(1,type(L#xmlElement.content,S,Env)),S};
	    type(L#xmlElement.content,S,Env);
	#xmlAttribute{value=V} ->
	    %% this type should be preliminary saved and checked after
	    %% the parsing of the schema.
	    TypeRef ={simpleType,
		      get_QName(V,L#xmlElement.namespace,reset_scope(S))}, 
	    {[TypeRef],add_ref(S,TypeRef)}
    end.
union_types(U,S,Env) ->
    {MemberTypes,S2} =
	case keyNsearch(memberTypes,#xmlAttribute.name,U#xmlElement.attributes,[]) of
	    [] ->
		{[],S};
	    #xmlAttribute{value = NameString} ->
		Names = namestring2namelist(NameString),
		UTypeRefs =
		    [{simpleType,get_QName(X,U#xmlElement.namespace,
					   reset_scope(S))}||X<-Names],
		{UTypeRefs,foldl(fun(X,S_in) -> add_ref(S_in,X) end,S,UTypeRefs)} 
	end,
    {DefinedTypes,S3} = union_types1(U#xmlElement.content,S2,Env),
    {MemberTypes++DefinedTypes,S3}.

union_types1(C,S,Env) ->
    union_types1(C,S,Env,[]).
union_types1([],S,_Env,Acc) ->
    {Acc,S};
union_types1([C=#xmlElement{}|Cs],S,Env,Acc) ->
    case element_content(kind(C,S),C,Env) of
	{ST={simpleType,_},S2} ->
	    union_types1(Cs,S2,Env,[ST|Acc]);
	{{annotation,_},S2} ->
	    union_types1(Cs,S2,Env,Acc);
	{IllegalType,S2} ->
	    Err = {error_path(C,C#xmlElement.name),?MODULE,
		   {union_member_type_not_simpleType,IllegalType}},
	    union_types1(Cs,acc_errs(S2,Err),Env,Acc)
    end;
union_types1([_H|T],S,Env,Acc) ->
    union_types1(T,S,Env,Acc).

%% If a group in a redefine refer to itself the reference is to the
%% "old" definition of the group. See XMLSchema part 1, section 4.2.2
%% "Schema Representation Constraint: Individual Component
%% Redefinition"
recursive_redefine(Name,CM,S=#xsd_state{redefine=true}) ->
    case remove_annotation(CM) of
	[{MG,{C,Occ}}] ->
	    [{MG,{recursive_redefine2(Name,C,S),Occ}}];
	_ ->
	    CM
    end;
recursive_redefine(_,CM,_) ->
    CM.
recursive_redefine2(Name,[{group,{Name,Occ}}|T],S) ->
    %% Rename old group definition
    case rename_redef_group(Name,S) of
	failed ->
	    [{group,{Name,Occ}}|T];
	NewName ->
	    [{group,{NewName,Occ}}|T]
    end;
recursive_redefine2(Name,[{MG,{C,Occ}}|T],S) 
  when MG =:= sequence; MG =:= choice; MG=:= all; MG=:= group ->
    C2 = recursive_redefine2(Name,C,S),
    [{MG,{C2,Occ}}|recursive_redefine2(Name,T,S)];
recursive_redefine2(Name,[H|T],S) ->
    [H|recursive_redefine2(Name,T,S)];
recursive_redefine2(_,[],_) ->
    [].
    
rename_redef_group(Name={LN,Scope,NS},S) ->
    %% Scope must be []
    NewName = {LN,['#redefine'|Scope],NS},
    case resolve({group,NewName},S) of
	{SG=#schema_group{name=Name},_} ->
	    _ = save_object({group,SG#schema_group{name=NewName}},S),
	    NewName;
	_ ->
	    failed
    end.
	    
    
add_ref(S=#xsd_state{unchecked_references=UR},STRef={simpleType,Ref}) ->
    case {is_builtin_simple_type(Ref),Ref} of
	{true,_} ->
	    S;
	{_,{'',_,_}} ->
	    S;
	_ ->
	    S2 = S#xsd_state{unchecked_references=add_once(STRef,UR)},
	    add_circularity_ref(STRef,S2)
    end;
add_ref(S=#xsd_state{unchecked_references=UR},STRef={simple_or_complex_Type,Ref}) ->
    case {is_builtin_simple_type(Ref),Ref} of
	{true,_} ->
	    S;
	{_,{'',_,_}} ->
	    S;
	{_,{anyType,_,?XSD_NAMESPACE}} ->
	    S;
	{_,{anySimpleType,_,?XSD_NAMESPACE}} ->
	    S;
	_ ->
	    S2 = S#xsd_state{unchecked_references=add_once(STRef,UR)},
	    add_circularity_ref(STRef,S2)
    end;
add_ref(S,{complexType,{anyType,_,?XSD_NAMESPACE}}) ->
    S;
add_ref(S=#xsd_state{unchecked_references=UR},Ref) ->
    S2 = S#xsd_state{unchecked_references=add_once(Ref,UR)},
    add_circularity_ref(Ref,S2).
%% add_ref(S=#xsd_state{unchecked_references=UR},Ref) ->
%%     S#xsd_state{unchecked_references=add_once(Ref,UR)}.

%% Name of simpleType/complexType is unique within the whole
%% environment, which is checked elsewhere, so ignore the kind of type
%% for simplicity.
add_circularity_ref(Ref={Kind,To},S=#xsd_state{circularity_disallowed=CD,
					       redefine=false})
  when Kind==simpleType;Kind==simple_or_complex_Type;Kind==complexType ->
    case get_circularity_mark(Ref,S) of
	[] ->
	    S;
	From -> %% This is the node from which the graph reaches Ref
	    S#xsd_state{circularity_disallowed=add_once({From,{typeDef,To}},CD)}
    end;
add_circularity_ref(_,S) ->
    S.
get_circularity_mark({TD,_},S) 
  when TD==simpleType;TD==complexType;TD==simple_or_complex_Type ->
    case S#xsd_state.circularity_stack of
	[From={typeDef,_}|_] ->
	    From;
	_ -> []
    end;
get_circularity_mark(_,_S) ->
    [].

push_circularity_mark(Mark,S=#xsd_state{circularity_stack=CS,
					redefine=false}) ->
    S#xsd_state{circularity_stack=[Mark|CS]};
push_circularity_mark(_,S) ->
    S.
pop_circularity_mark(Mark,S=#xsd_state{redefine=false}) ->
    case S#xsd_state.circularity_stack of
	[Mark|Rest] ->
	    S#xsd_state{circularity_stack=Rest};
	_ ->
	    S
    end;
pop_circularity_mark(_,S) ->
    S.

derived_type({complexType,#schema_complex_type{name=Name,content=C}},
	     S=#xsd_state{derived_types=DT}) ->
    case {keymember(restriction,1,C),keymember(extension,1,C)} of
	{false,false} ->
	    S;
	_ ->
	    S#xsd_state{derived_types=[{complexType,Name}|DT]}
    end;
derived_type({simpleType,#schema_simple_type{name=Name,content=C}},
	     S=#xsd_state{derived_types=DT}) ->
    case keymember(restriction,1,C) of
	true ->
	    S#xsd_state{derived_types=[{simpleType,Name}|DT]};
	_ ->
	    S
    end.

facets([{annotation,_}|Rest],S) ->
    facets(Rest,S);
facets([{restriction,{BaseType,CM}}],_S) ->
    Facets = [X||X={F,_} <- CM,is_facet(F)],
    GroupFacets = group_facets(Facets),
    {BaseType,GroupFacets};
facets(_,_S) ->
    {undefined,[]}.

group_facets(Facets) ->    
    group_facets(Facets,[]).
group_facets(L=[{enumeration,_}|_Rest],Acc) ->
    {Enums,Rest} = splitwith(fun({enumeration,_}) -> true;
				       (_) -> false
				    end,
				    L),
    group_facets(Rest,[{enumeration,[X||{enumeration,X}<-Enums]}|Acc]);
group_facets([H|T],Acc) ->
    group_facets(T,[H|Acc]);
group_facets([],Acc) ->
    reverse(Acc).

simpleType_final(ST,_S) ->
    Final = get_attribute_value(final,ST,[]),
    split_by_whitespace(Final,[]).

%% A redefine may contain (simpleType | complexType | group |
%% attributeGroup)*
%%{simpleType,Name},{complexType,Name},{group,Name},{attributeGroup,Name}
redefine([CM|Rest],S) ->
    S2=redefine(CM,S),
    redefine(Rest,S2);
redefine(ST={Type,_Name},S)
  when Type==simpleType ; Type==complexType ->
    %% Get the original definition
    {OriginalType,S2} = resolve(ST,S),
    %% unnecessary to delete saved object, it will be overwritten.
    {RedefinedType,S3} = load_redefine_object(ST,S2),
    {_MergedType,S4} = merge_derived_types(OriginalType,RedefinedType,redefine,S3),
    S4;
redefine(_,S) ->
    %% attributeGroup and group redefines are already redefined
    S.

keyrefer(keyref,El,S) ->
    Ref=get_attribute_value(refer,El,undefined),
    get_QName(Ref,El#xmlElement.namespace,reset_scope(S));
keyrefer(_,_,_) ->
    undefined.

remove_annotation(CM) when is_list(CM) ->
    [X||X = {K,_} <- CM, K=/=annotation].
remove_attributes(CM) when is_list(CM) ->
    [X||X = {K,_} <- CM, K=/=attribute,K=/=anyAttribute,K=/=attributeGroup].
keep_attributes(CM) when is_list(CM) ->
    [X||X = {K,_} <- CM, K==attribute orelse K==anyAttribute orelse K==attributeGroup].
split_content([{restriction,{BaseT,CM}}]) ->
    {[{restriction,{BaseT,remove_attributes(CM)}}],keep_attributes(CM)};
split_content([{extension,{BaseT,CM}}]) ->
    {[{extension,{BaseT,remove_attributes(remove_annotation(CM))}}],
     keep_attributes(CM)};
split_content(CM) ->
    {remove_attributes(CM),keep_attributes(CM)}.

restriction_base_type(R,CM,S) ->
    case base_type(R) of
	[] ->
	    case [X||X={simpleType,_}<-CM] of
		[{simpleType,TypeName}] ->
		    {TypeName,keydelete(simpleType,1,CM),S};
		Other ->
		    Err = {error_path(R,R#xmlElement.name),?MODULE,
			   {missing_base_type,restriction,Other}},
		    {{[],[],[]},CM,acc_errs(S,Err)}
	    end;
	BT ->
	    {get_QName(BT,R#xmlElement.namespace,reset_scope(S)),CM,S}
    end.
		    
base_type([{restriction,{BaseT,_}}],SCT) -> 
    SCT#schema_complex_type{base_type=BaseT};
base_type([{extension,{BaseT,_}}],SCT) ->
    SCT#schema_complex_type{base_type=BaseT};
base_type(_,SCT) ->
    SCT.

variety([{list,_ItemType}]) ->
    list;
variety([{union,_ItemType}]) ->
    union;
variety(_) ->
    atomic.

%% pre_check_cm/2 is for now only for simpleContent | complexContent
%% which allow content: (annotation?, (restriction | extension))
pre_check_cm(Kind,Cs=[C=#xmlElement{}|RestC],Name,S) ->
    case kind(C,S) of
	{annotation,_} ->
	    pre_check_cm2(Kind,RestC,Name,C,S,0);
	{_,S2} ->
	    pre_check_cm2(Kind,Cs,Name,C,S2,0)
    end;
pre_check_cm(Kind,[_C|Cs],Name,S) ->
    pre_check_cm(Kind,Cs,Name,S);
pre_check_cm(Kind,[],Name,S) ->
    Err = {[],?MODULE,{content_failure,Kind,[],Name}},
    acc_errs(S,Err).

pre_check_cm2(Kind,[C=#xmlElement{}|Cs],Name,_El,S,N) ->
    S2 =
    case kind(C,S) of
	{restriction,_} ->
	    S;
	{extension,_} ->
	    S;
	{Other,S1} ->
	    Err = {error_path(C,C#xmlElement.name),?MODULE,
		   {illegal_element,Kind,Other,Name}},
	    acc_errs(S1,Err)
    end,
    pre_check_cm2(Kind,Cs,Name,C,S2,N+1);
pre_check_cm2(Kind,[_H|T],Name,El,S,N) ->
    pre_check_cm2(Kind,T,Name,El,S,N);
pre_check_cm2(_,[],_,_,S,N) when N==1 ->
    S;
pre_check_cm2(Kind,[],Name,El,S,N) ->
    Err =
	case N of
	    0 ->
		{error_path(El,El#xmlElement.name),?MODULE,
		 {content_failure_expected_restriction_or_extension,
		  Kind,Name}};
	    _ ->
		{error_path(El,El#xmlElement.name),?MODULE,
		 {content_failure_only_one_restriction_or_extension_allowed,
		  Kind,Name}}
	end,
    acc_errs(S,Err).


%% check_cm(Arg1,Arg2,Arg3)
%% Arg1 - The allowed content for this type according to schema for schemas
%% Arg2 - The content model of this particular schema
check_cm(Kind,S4SCM,ContentModel,S) ->
    case check_cm2(Kind,S4SCM,ContentModel,S) of
	{[],_S} ->
	    S;
	{[_,[]|_],_S} ->
	    S;
	{_CM,S2} ->
	    S2;
	Err ->
	    exit({error,{[],?MODULE,{internal_error,Err}}})
    end.

check_cm2(Kind,#chain{content=S4SCM,occurance=Occ},
	 ContentModel,S) ->
    case occurance_loop(Occ,fun check_chain/1,
			[S4SCM,ContentModel,Kind,S],0) of
  	{ok,[]} ->
 	    {[],S};
	{ok,[S4SCMRest,CMRest|_]} ->
	    case all_optional(S4SCMRest) of
		true ->
		    {CMRest,S};
		_ ->
		    Err = {[],?MODULE,
			   {mandatory_component_missing,S4SCMRest,Kind}},
		    acc_errs(S,Err)
	    end;
	{error,{_,_,Reason}} ->
	    Err = {[],?MODULE,{illegal_content,Reason,Kind}},
	    {ContentModel,acc_errs(S,Err)}
    end;
check_cm2(Kind,#alternative{content=S4SCM,occurance=Occ},
	 ContentModel,S) ->
    case occurance_loop(Occ,fun check_alternative/1,
			[S4SCM,ContentModel,Kind,S],0) of
	{ok,[]} ->
	    {[],S};
	{ok,[_,CMRest|_]} ->
	    {CMRest,S};
	{error,Reason} ->
	    {ContentModel,acc_errs(S,Reason)}
    end;
check_cm2(_,{Kind,Occ},CM,S) ->
    case occurance_loop(Occ,fun check_simple_cm/1,[Kind,CM],0) of
 	{ok,[]} ->
	    {[],S};
	{ok,[_,CMRest|_]} ->
	    {CMRest,S};
	{error,Reason} ->
	    {CM,acc_errs(S,Reason)};
	Err ->
	    {CM,acc_errs(S,Err)}
    end.

%% check_simple_cm
check_simple_cm([Kind,CM]) ->
    check_simple_cm(Kind,CM).
    

check_simple_cm(Kind,[]) ->
    {error,{[],?MODULE,{no_match,{Kind,[]}}}};
check_simple_cm(Kind,[{Kind,_}|Rest]) ->
    {ok,[Kind,Rest]};
check_simple_cm(Kind,[{Other,_}|Rest]) 
  when Kind==simpleType;Kind==complexType ->
    case Other of
	simple_or_complex_Type -> {ok,[Kind,Rest]};
	_ -> {error,{[],?MODULE,{no_match,Other}}}
    end;
check_simple_cm(_Kind,[{Other,_}|_]) ->
    {error,{[],?MODULE,{no_match,Other}}}.


check_chain([S4SCM,ContentModel,Kind,S]) ->
    check_chain(Kind,S4SCM,ContentModel,S).
check_chain(Kind,[S4SC|S4SCs],ChainCM=[_H|_T],
	    S=#xsd_state{errors=Errs}) ->
    NewKind =
	case S4SC of
	    {NK,_} -> NK;
	    _ -> Kind
	end,
    case check_cm2(NewKind,S4SC,ChainCM,S) of
	{ChainCMRest,#xsd_state{errors=Errs}} ->
	    check_chain(Kind,S4SCs,ChainCMRest,S);
	{_ChainCMRest,_S2} ->
	    case optional(S4SC) of
		true ->
		    check_chain(Kind,S4SCs,ChainCM,S);
		_ ->
		    {error,{[],?MODULE,{unmatched_mandatory_object,Kind,S4SC}}}
	    end
    end;
check_chain(Kind,[],CM,S) ->
    {ok,[[],CM,Kind,S]};
check_chain(Kind,Rest,CM,S) ->
    case all_optional(Rest) of
	true ->
	    {ok,[Rest,CM,Kind,S]}; %% or {ok,[[],CM,Kind,S]}
	_ ->
	    {error,{[],?MODULE,{bad_match,Rest,CM}}}

    end.


check_alternative([S4SC,CM,Kind,S]) ->
    check_alternative(Kind,S4SC,CM,S).
check_alternative(Kind,[S4SC|S4SCs],AltCM = [_H|_T],
		  S=#xsd_state{errors=Err})  ->
    NewKind =
	case S4SC of
	    {NK,_} -> NK;
	    _ -> Kind
	end,
    case check_cm2(NewKind,S4SC,AltCM,S) of
	{AltCMRest,#xsd_state{errors=Err}} ->
	    {ok,[[S4SC],AltCMRest,Kind,S]};
	{AltCMRest,_S2} ->
	    check_alternative(Kind,S4SCs,AltCMRest,S)
    end;
check_alternative(Kind,[],_AltCM,_S) ->
    {error,{[],?MODULE,{no_match,Kind}}}.
    

%% occurance_loop keeps track of the right number of elements
%% Third argument is a list: [S4SContent,ContentModel]
%% returns {ok,Rest} where Rest is the next unmatched abstract
%% structure.
occurance_loop({Min,Max},_CheckFun,[_,[]|_Rest],N)
  when Min =< N, Max >= N ->
    {ok,[]};
occurance_loop(Occ={Min,Max},CheckFun,Args,N) ->
    Nplus1 = N+1,
    case CheckFun(Args) of
	{error,{_,_,{no_match,_}}} when Min =< N, Max >= N  ->
	    {ok,Args};
	Err = {error,_} ->
	    Err;
	{ok,Args} ->
	    {error,{[],?MODULE,{no_match,occurance_kind(Args)}}};
	{ok,NewArgs} when Nplus1 < Max ->
	    occurance_loop(Occ,CheckFun,NewArgs,Nplus1);
	Ret = {ok,_NewArgs} ->
	    Ret
    end.

occurance_kind([Kind,_]) ->
    Kind;
occurance_kind([_,_,Kind,_]) ->
    Kind;
occurance_kind(_) ->
    [].
%% if_simple_hd(S4SCM,ConstrCM) 
%%   when is_record(S4SCM,chain);is_record(S4SCM,alternative);is_list(S4SCM) ->
%%     ConstrCM;
%% if_simple_hd(_,[H|_Tl]) ->
%%     H.

%% if_simple_tl(S4SCM,_ConstrCM)
%%   when is_record(S4SCM,chain);is_record(S4SCM,alternative);is_list(S4SCM) ->
%%     [];
%% if_simple_tl(_,[_|Tl]) ->
%%     Tl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count_occur({Min,Max}) ->
%    {decrease(Min),decrease(Max)};
    {decrease(Min),Max};
count_occur(Other) ->
    Other.

decrease(I) when is_integer(I), I > 0 ->
    I-1;
decrease(I) ->
    I.

decrease_occurance({K,{ID,Occ}}) ->
    {K,{ID,count_occur(Occ)}};
decrease_occurance(Other) ->
    Other.

get_occur({_,{_,Occ={Min,_}}}) when is_integer(Min) ->
    Occ;
get_occur({_,{_,Occ={Min,_},_}}) when is_integer(Min) ->
    Occ;
get_occur(Other) ->
    Other.
    
%% remove_whitespace(L=[T=#xmlText{}|Rest]) ->
%%     case is_whitespace(T) of
%% 	true ->
%% 	    remove_whitespace(Rest);
%% 	_ -> L
%%     end;
%% remove_whitespace(L) ->
%%     L.
    
optional(optional_text) ->
    true;
optional({_,{0,_}}) ->
    true;
optional({_,{_,{0,_}}}) ->
    true; %% sequence, all or choice
optional({any,{_,{0,_},_}}) ->
    true;
optional(#chain{occurance={0,_}}) ->
    true;
optional(#alternative{occurance={0,_}}) ->
    true;
optional(#chain{content=Content}) ->
    catch is_optional_content(Content);
optional(#alternative{content=Content}) ->
    catch is_optional_content(Content);
optional({all,{Content,_}}) ->
    catch is_optional_content(Content);
optional(_) ->
    false.

is_optional_content([H|T]) ->
    case optional(H) of
	true ->
	    is_optional_content(T);
	false ->
	    throw(false)
    end;
is_optional_content([]) ->
    true.

not_optional(X) ->
    case optional(X) of
	true ->
	    false;
	_ ->
	    true
    end.

all_optional([]) ->
    true;
all_optional(L) ->
    case filter(fun not_optional/1,L) of
	[] ->
	    true;
	_ ->
	    false
    end.


%% allowed_content/2 returns a representation of the allowed content
%% model for that object
allowed_content(element,_Parents) ->
    #chain{content=
	      [{annotation,{0,1}},
	       #chain{content=
			 [#alternative{content=
				  [{simpleType,{1,1}},{complexType,{1,1}}],
				  occurance={0,1}},
			  #alternative{content=
				  [{unique,{1,1}},{key,{1,1}},{keyref,{1,1}}],
				  occurance={0,unbounded}}]
			}]
	     };
allowed_content(attribute,_Parents) ->
    #chain{content=[{annotation,{0,1}},{simpleType,{0,1}}]};
allowed_content(complexType,Parents) ->
    #chain{content=
	[{annotation,{0,1}},
	 #alternative{content=
	    [set_occurance(allowed_content(simpleContent,Parents),{1,1}),
	     set_occurance(allowed_content(complexContent,Parents),{1,1}),
	     #chain{content=
		    [#alternative{content=
				  [{group,{1,1}},{all,{1,1}},
				   {choice,{1,1}},{sequence,{1,1}}],
				  occurance={0,1}},
		     #chain{content=
			    [#alternative{content=
					  [{attribute,{1,1}},
					   {attributeGroup,{1,1}}],
					  occurance={0,unbounded}},
			     {anyAttribute,{0,1}}]
			   }
		    ]
		   }
	    ]
		     }
	]
	  };
allowed_content(attributeGroup,Parents) ->
    case member(simpleContent,Parents) of
	true ->
	    {annotation,{0,1}};
	_ ->
	    #chain{content=
		      [{annotation,{0,1}},
		       #chain{content=
				 [#alternative{content=
					  [{attribute,{1,1}},
					   {attributeGroup,{1,1}}],
					  occurance={0,unbounded}},
				  {anyAttribute,{0,1}}]}]}
    end;
allowed_content(group,_Parents) ->
    #chain{content=
	      [{annotation,{0,1}},
	       #alternative{content=
		       [{all,{1,1}},{choice,{1,1}},{sequence,{1,1}}],
		       occurance={0,1}}]};
allowed_content(all,_Parents) ->
    #chain{content=[{annotation,{0,1}},{element,{0,unbounded}}]};
allowed_content(SorC,_Parents) when SorC==sequence;SorC==choice ->
    #chain{content=
	      [{annotation,{0,1}},
	       #alternative{content=
		       [{element,{1,1}},{group,{1,1}},
			{choice,{1,1}},{sequence,{1,1}},
			{any,{1,1}}],
		       occurance={0,unbounded}}]};
%% allowed_content(E,_Parents)
%%   when E==any;E==selector;E==field;E==notation;E==include;E==import;
%%        E==anyAttribute ->
%%     {annotation,{0,1}};
%% allowed_content(UKK,_Parents) when UKK==unique;UKK==key;UKK==keyref->
%%     #chain{content=
%% 	      [{annotation,{0,1}},
%% 	       #chain{content=
%% 			 [{selector,{1,1}},{selector,{1,unbounded}}]}]};
%% allowed_content(annotation,_Parents) ->
%%     #alternative{content=[{appinfo,{1,1}},{documentation,{1,1}}],
%% 	    occurance={0,unbounded}};
%% allowed_content(E,_Parents) when E==appinfo;E==documentation ->
%%     {any,{0,unbounded}};
allowed_content(simpleType,_Parents) ->
    #chain{content=
	      [{annotation,{0,1}},
	       #alternative{content=[{restriction,{1,1}},{list,{1,1}},
				{union,{1,1}}]}]};
allowed_content(restriction,Parents) ->
    case member(simpleType,Parents) of
	true ->
	    allowed_content2(restriction,simpleType);
	_ ->
	    case member(simpleContent,Parents) of
		true ->
		    allowed_content2(restriction,simpleContent);
		_ ->
		    allowed_content2(restriction,complexContent)
	    end
    end;
allowed_content(LU,_Parent) when LU==list;LU==union ->
    #chain{content=[{annotation,{0,1}},{simpleType,{0,1}}]};
%% allowed_content(schema,_) ->
%%     #chain{content=
%% 	      [#alternative{content=
%% 		       [{include,{1,1}},{import,{1,1}},
%% 			{redefine,{1,1}},{annotation,{1,1}}],
%% 		       occurance={0,1}},
%% 	       #chain{content=
%% 			 [#alternative{content=
%% 				  [#alternative{content=
%% 					   [{simpleType,{1,1}},{complexType,{1,1}},
%% 					    {group,{1,1}},{attributeGroup,{1,1}}]},
%% 				   {element,{1,1}},
%% 				   {attribute,{1,1}},
%% 				   {notation,{1,1}}]},
%% 			  {annotation,{0,unbounded}}],
%% 			 occurance={0,unbounded}}]};
allowed_content(redefine,_Parents) ->
    #alternative{content=
	    [{annotation,{1,1}},
	     #alternative{content=
		     [{simpleType,{1,1}},{complexType,{1,1}},
		      {group,{1,1}},{attributeGroup,{1,1}}]}],
	    occurance={0,unbounded}};
allowed_content(E,_Parents) when E==simpleContent;
				 E==complexContent ->
    #chain{content=
	      [{annotation,{0,1}},
	       #alternative{content=
		       [{restriction,{1,1}},{extension,{1,1}}]}]};
allowed_content(extension,Parents) ->
    case member(simpleContent,Parents) of
	true ->
	    allowed_content2(extension,simpleContent);
	_ ->
	    allowed_content2(extension,complexContent)
    end.
%% allowed_content(minExclusive,_Parents) ->
%%     [];
%% allowed_content(minInclusive,_Parents) ->
%%     [];
%% allowed_content(maxExclusive,_Parents) ->
%%     [];
%% allowed_content(maxInclusive,_Parents) ->
%%     [];
%% allowed_content(totalDigits,_Parents) ->
%%     [];
%% allowed_content(fractionDigits,_Parents) ->
%%     [];
%% allowed_content(length,_Parents) ->
%%     [];
%% allowed_content(minLength,_Parents) ->
%%     [];
%% allowed_content(maxLength,_Parents) ->
%%     [];
%% allowed_content(enumeration,_Parents) ->
%%     [];
%% allowed_content(whiteSpace,_Parents) ->
%%     [];
%% allowed_content(pattern,_Parents) ->
%%     [].
	       



allowed_content2(restriction,simpleType) ->
    #chain{content=
	      [{annotation,{0,1}},
	       #chain{content=
			 [{simpleType,{0,1}},
			  #alternative{content=
				  [{minExclusive,{1,1}},{minInclusive,{1,1}},
				   {maxExclusive,{1,1}},{maxInclusive,{1,1}},
				   {totalDigits,{1,1}},{fractionDigits,{1,1}},
				   {length,{1,1}},{minLength,{1,1}},
				   {maxLength,{1,1}},{enumeration,{1,1}},
				   {whiteSpace,{1,1}},{pattern,{1,1}}],
				  occurance={0,unbounded}}]}]};
allowed_content2(restriction,simpleContent) ->
    #chain{content=
	      [{annotation,{0,1}},
	       #chain{content=
			[{simpleType,{0,1}},
			 #alternative{content=
				 [{minExclusive,{1,1}},{minInclusive,{1,1}},
				  {maxExclusive,{1,1}},{maxInclusive,{1,1}},
				  {totalDigits,{1,1}},{fractionDigits,{1,1}},
				  {length,{1,1}},{minLength,{1,1}},
				  {maxLength,{1,1}},{enumeration,{1,1}},
				  {whiteSpace,{1,1}},{pattern,{1,1}}],
				 occurance={0,unbounded}}],
			 occurance={0,1}},
	       #chain{content=
			 [#alternative{content=
				 [{attribute,{1,1}},{attributeGroup,{1,1}}],
				  occurance={0,unbounded}},
			  {anyAttribute,{0,1}}]}]};
allowed_content2(restriction,complexContent) ->
    #chain{content=
	      [{annotation,{0,1}},
	       #alternative{content=
		       [{group,{1,1}},{all,{1,1}},{choice,{1,1}},
			{sequence,{1,1}}],
		       occurance={0,1}},
	       #chain{content=
			 [#alternative{content=
				  [{attribute,{1,1}},{attributeGroup,{1,1}}],
				  occurance={0,unbounded}},
			  {anyAttribute,{0,1}}]}]};
allowed_content2(extension,simpleContent) ->
    #chain{content=
	      [{annotation,{0,1}},
	       #chain{content=
			 [#alternative{content=
				  [{attribute,{1,1}},{attributeGroup,{1,1}}],
				  occurance={0,unbounded}},
			  {anyAttribute,{0,1}}]}]};
allowed_content2(extension,complexContent) ->
    #chain{content=
	      [{annotation,{0,1}},
	       #chain{content=
			 [#alternative{content=
				  [{group,{1,1}},{all,{1,1}},{choice,{1,1}},
				   {sequence,{1,1}}],
				  occurance={0,1}},
			  #chain{content=
				    [#alternative{content=
					     [{attribute,{1,1}},
					      {attributeGroup,{1,1}}],
					     occurance={0,1}},
				     {anyAttribute,{0,1}}]}]}]}.
						  

set_occurance(Ch = #chain{},Occ) ->
    Ch#chain{occurance=Occ};
set_occurance(Alt = #alternative{},Occ) ->
    Alt#alternative{occurance=Occ};
set_occurance({Name,_},Occ) when is_atom(Name) ->
    {Name,Occ}.
%% set_occurance(CM,_) ->
%%     CM.


process_external_schema_once(E,Namespace,S) when is_record(E,xmlElement) ->
    case get_attribute_value(schemaLocation,E,[]) of
	[] ->
	    Err = {missing_schemalocation_attribute,E#xmlElement.name},
	    acc_errs(S,Err);
	Path ->
	    process_external_schema_once(Path,Namespace,S)
    end;
process_external_schema_once(SchemaLocation,Namespace,S) ->
    case fetch_external_schema(SchemaLocation,S) of
	{E=#xmlElement{},S2} ->
	    case is_already_processed(Namespace,S2) of
		true ->
		    save_namespace_definition(Namespace,S2);
		_ ->
		    S3 = save_namespace_definition(Namespace,S2),
		    traverse_ext_schema(E,S3#xsd_state{targetNamespace=Namespace})
	    end;
	{_,S2} ->
	    S2
    end.

%% process_external_schema/2 returns:
%% {ok,some_result()} | {error,reason()}
process_external_schema(Path,S) when is_list(Path) ->
    case fetch_external_schema(Path,S) of
	{E=#xmlElement{},S2} ->
	    traverse_ext_schema(E,S2);
	{_,S2} ->
	    S2
    end;
process_external_schema(absent,S) ->
    S.

fetch_external_schema(Path,S) when is_list(Path) ->
    FetchFun = S#xsd_state.fetch_fun,
    %%    {ExtXSD,S2} = 
	case FetchFun(Path,S) of
	    {ok,{file,File},_} ->
		?debug("scanning file: ~p~n",[File]),
		case xmerl_scan:file(File,S#xsd_state.xml_options) of
		    {error,Reason} ->
			{error,acc_errs(S,{[],?MODULE,{parsing_external_schema_failed,File,Reason}})};
		    {EXSD,_} ->
			{EXSD,S#xsd_state{schema_name=File}}
		end;
	    {_,{string,String},_} -> %% this is for a user defined fetch fun that returns an xml document on string format.
		?debug("scanning string: ~p~n",[String]),
		case xmerl_scan:string(String,S#xsd_state.xml_options) of
		    {error,Reason} ->
			{error,acc_errs(S,{[],?MODULE,{parsing_external_schema_failed,Path,Reason}})};
		    {EXSD,_} ->
			{EXSD,S#xsd_state{schema_name=Path}}
		end;
	    {ok,[],_} ->
		{ok,S};
	    {_,Other,_} ->
		{error,acc_errs(S,{[],?MODULE,{fetch_fun_failed,Other}})}
	end;
fetch_external_schema(absent,S) ->
    {ok,S}.


%% The schema name is also important here because a schema may import
%% and must include from the same namespace as the target namespace of
%% the including schema.
is_already_processed(NameSpace,#xsd_state{schema_name=SchemaName,
					  checked_namespace_nodes=CNS}) ->
%%     case {keymember(SchemaName,2,CNS),keymember(NameSpace,3,CNS)} of
%% 	{true,true} ->
    case keysearch(SchemaName,2,CNS) of
	{_,{_,_,NameSpace}} ->
	    true;
	_ ->
	    false
    end.

%% 
save_namespace_definition(NameSpace,
			  S=#xsd_state{targetNamespace=TNS,
				       global_namespace_nodes=GNS,
				       checked_namespace_nodes=CNS}) ->
    %% 1) Have to find a matching namespace in the global list for
    %% this schema, and get the associated prefix. 2) Then check
    %% whether a schema with this prefix - namespace combinaton
    %% already is checked, if so do nothing. 3a) If this namespace is
    %% checked but with another prefix only add the prefix - namespace
    %% pair to the checked namespace list. 3b) Otherwise add the
    %% prefix - namespace pair.
    {Prefix,S2} = 
	case keysearch(TNS,1,GNS) of
	    {value,{_,ImportedNodes}} ->
		case keysearch(NameSpace,2,ImportedNodes) of
		    {value,{_P,_}} -> {_P,S};
		    _ -> {none,S}
		end;
	    _ ->
		Err = {[],?MODULE,{imported_namespace_wo_namespace_definition,NameSpace}},
		{none,acc_errs(S,Err)}
	end,
    %% Instead of 2, 3a and 3b just add_once
    case Prefix of
	none ->
	    S2;
	_ ->
	    S#xsd_state{checked_namespace_nodes=
			add_once({Prefix,S#xsd_state.schema_name,NameSpace},CNS)}
    end.

%% prefix_namespace_2global

%% adds {Prefix,Namespace} to the global namespace nodes list for the
%% targetnamespace. Prefix is the right one found in Nodes.
prefix_namespace_2global(Namespace,
			 #xmlNamespace{nodes=Nodes},
			 S=#xsd_state{targetNamespace=TNS,
				     global_namespace_nodes=GNS}) ->
    case keysearch(Namespace,2,Nodes) of
	{value,{Prefix,_}} ->
	    case keysearch(TNS,1,GNS) of
		{value,{_,DefinedNamespaces}} ->
		    S#xsd_state{global_namespace_nodes=
				keyreplace(TNS,1,GNS,
					   {TNS,add_once({Prefix,Namespace},
							 DefinedNamespaces)})};
		_ ->
		    S#xsd_state{global_namespace_nodes=
				[{TNS,[{Prefix,Namespace}|default_namespace_by_convention()]}]}
	    end;
	_ ->
	    S
    end;
prefix_namespace_2global(_,_,S) ->
    S.


traverse_ext_schema(E,S) ->
    TargetNS = target_namespace(E),
    case {TargetNS,S#xsd_state.targetNamespace} of
	{undefined,_} ->
	    traverse_ext_schema2(E,S);
	{TNS,TNS} ->
	    traverse_ext_schema2(E,S);
	_ ->
	    Err = {error_path(E,schema),?MODULE,{illegal_target_namespace_external_schema,E#xmlElement.name}},
	    acc_errs(S,Err)
    end.
traverse_ext_schema2(E,S) ->

    S1 = namespace_nodes(E,S),
    S2 = element_form_default(E,S1),
    S3 = attribute_form_default(E,S2),
    S4 = substitution_default(finalDefault,E,S3),
    S5 = substitution_default(blockDefault,E,S4),
    {CM,S6} = traverse_content2(E#xmlElement.content,S5,[]),
%%    ?debug("External schema S6:~n~p~n",[S6]),
    save_schema_element(CM,S6),
    S6.


attribute_properties([#xmlAttribute{name=default,value=Default}|Rest],
		     Attr,S) ->
    attribute_properties(Rest,Attr#schema_attribute{default=Default},S);
attribute_properties([#xmlAttribute{name=fixed,value=Fixed}|Rest],Attr,S) ->
    attribute_properties(Rest,Attr#schema_attribute{fixed=Fixed},S);
attribute_properties([#xmlAttribute{name=use,value=Use}|Rest],Attr,S) ->
    {Use2,S2} = attribute_use(Use,S),
    attribute_properties(Rest,Attr#schema_attribute{use=Use2},S2);
attribute_properties([#xmlAttribute{name=form,value=Form}|Rest],Attr,S) ->
    {Form2,S2} = attribute_form(Form,S),
    attribute_properties(Rest,Attr#schema_attribute{form=Form2},S2);
attribute_properties([#xmlAttribute{name=id,value=ID}|Rest],Attr,S) ->
    S2 = check_and_save_ID(ID,S),
    attribute_properties(Rest,Attr#schema_attribute{id=ID},S2);
attribute_properties([_H|Rest],Attr,S) ->
    attribute_properties(Rest,Attr,S);
attribute_properties([],Attr,S) ->
    {Attr,S}.
attribute_use(Use,S) when Use=="optional";Use=="prohibited";Use=="required" ->
    {list_to_atom(Use),S};
attribute_use(Use,S) ->
    {Use,acc_errs(S,{[],?MODULE,{illegal_use_value,Use}})}.
attribute_form(Form,S) when Form=="qualified";Form=="unqualified" ->
    {list_to_atom(Form),S};
attribute_form(Form,S) ->
    {Form,acc_errs(S,{[],?MODULE,{illegal_form_value,Form}})}.

element_properties([#xmlAttribute{name=default,value=Default}|Rest],SE,El,S) ->
    case SE#schema_element.value_constraint of
	{fixed,_} -> 
	    Err = {error_path(El,schema),?MODULE,{"only one of final/default attributes allowed",
		   El#xmlElement.name}},
	    element_properties(Rest,SE,El,acc_errs(S,Err));
	_ ->
	    element_properties(Rest,SE#schema_element{value_constraint=
						      {default,Default}},El,S)
    end;
element_properties([#xmlAttribute{name=fixed,value=Fixed}|Rest],SE,El,S) ->
    case SE#schema_element.value_constraint of
	{default,_} -> 
	    Err = {error_path(El,schema),?MODULE,
		   {"only one of final/default attributes allowed",
		    El#xmlElement.name}},
	    element_properties(Rest,SE,El,acc_errs(S,Err));
	_ ->
	    element_properties(Rest,SE#schema_element{value_constraint=
						      {fixed,Fixed}},El,S)
    end;
element_properties([#xmlAttribute{name=substitutionGroup,value=SG}|Rest],
		   SE,El,S) ->
    SGName = get_QName(SG,El#xmlElement.namespace,reset_scope(S)),
    element_properties(Rest,SE#schema_element{substitutionGroup=SGName},El,
		       add_ref(S,{element,SGName}));
element_properties([#xmlAttribute{name=form,value=F}|Rest],SE,El,S) ->
    {Form,S2} = attribute_form(F,S),
    element_properties(Rest,SE#schema_element{form=Form},El,S2);
element_properties([#xmlAttribute{name=id,value=ID}|Rest],SE,El,S) ->
    S2 = check_and_save_ID(ID,S),
    element_properties(Rest,SE#schema_element{id=ID},El,S2);
element_properties([#xmlAttribute{name=nillable,value=N}|Rest],SE,El,S) ->
    case boolean_to_atom(N) of
	error ->
	    element_properties(Rest,SE,El,
			       acc_errs(S,{error_path(El,schema),?MODULE,{illegal_nillable_value,N}}));
	N_atom ->
	    element_properties(Rest,SE#schema_element{nillable=N_atom},El,S)
    end;
element_properties([#xmlAttribute{name=abstract,value=A}|Rest],SE,El,S) ->
    case boolean_to_atom(A) of
	error ->
	    element_properties(Rest,SE,El,
			       acc_errs(S,{error_path(El,schema),?MODULE,{illegal_abstract_value,A}}));
	A_atom ->
	    element_properties(Rest,SE#schema_element{abstract=A_atom},El,S)
    end;
element_properties([#xmlAttribute{name=block,value=B}|Rest],SE,El,S) ->
    BlockValues = split_by_whitespace(B,[]),
    case legal_block_values(element,BlockValues) of
	{error,Reason} ->
	    element_properties(Rest,SE,El,
			       acc_errs(S,{error_path(El,schema),?MODULE,{illegal_block_values,Reason}}));
	_ ->
	    element_properties(Rest,SE#schema_element{block=BlockValues},El,S)
    end;
element_properties([#xmlAttribute{name=final,value=F}|Rest],SE,El,S) ->
    FinalValues = split_by_whitespace(F,[]),
    case legal_final_values(element,FinalValues) of
	{error,Reason} ->
	    element_properties(Rest,SE,El,
			       acc_errs(S,{error_path(El,schema),?MODULE,{illegal_final_values,Reason}}));
	_ ->
	    element_properties(Rest,SE#schema_element{final=FinalValues},El,S)
    end;
element_properties([_H|T],SE,El,S) ->
    element_properties(T,SE,El,S);
element_properties([],SE,_El,S) ->
    {SE,S}.

%% 3.3.3 bullet 2.2
%% nillable, default, fixed, form, block and type properties must be
%% absent in element with ref.
element_forbidden_properties(El,S) ->
    element_forbidden_properties(El#xmlElement.attributes,El,S).
element_forbidden_properties([#xmlAttribute{name=nillable,value=V}|Atts],El,S) ->
    element_forbidden_properties(Atts,El,acc_errs(S,{error_path(El,schema),?MODULE,{forbidden_property,nillable,V}}));
element_forbidden_properties([#xmlAttribute{name=default,value=V}|Atts],El,S) ->
    element_forbidden_properties(Atts,El,acc_errs(S,{error_path(El,schema),?MODULE,{forbidden_property,default,V}}));
element_forbidden_properties([#xmlAttribute{name=fixed,value=V}|Atts],El,S) ->
    element_forbidden_properties(Atts,El,acc_errs(S,{error_path(El,schema),?MODULE,{forbidden_property,fixed,V}}));
element_forbidden_properties([#xmlAttribute{name=form,value=V}|Atts],El,S) ->
    element_forbidden_properties(Atts,El,acc_errs(S,{error_path(El,schema),?MODULE,{forbidden_property,form,V}}));
element_forbidden_properties([#xmlAttribute{name=block,value=V}|Atts],El,S) ->
    element_forbidden_properties(Atts,El,acc_errs(S,{error_path(El,schema),?MODULE,{forbidden_property,block,V}}));
element_forbidden_properties([#xmlAttribute{name=type,value=V}|Atts],El,S) ->
    element_forbidden_properties(Atts,El,acc_errs(S,{error_path(El,schema),?MODULE,{forbidden_property,type,V}}));
element_forbidden_properties([#xmlAttribute{}|Atts],El,S) ->
    element_forbidden_properties(Atts,El,S);
element_forbidden_properties([],_,S) ->
    S.

%% 3.3.3 bullet 2.2
%% complexType, simpleType, key, keyref and unique must be absent in
%% element with ref.
element_forbidden_content([],S) ->
    S;
element_forbidden_content([El=#xmlElement{}|Els],S) ->
    case kind(El) of
	K when K==complexType;K==simpleType;K==key;K==keyref;K==unique ->
	    acc_errs(S,{error_path(El,schema),?MODULE,{element_content_must_not_contain,K,El}});
	annotation ->
	    element_forbidden_content(Els,S);
	Other ->
	    acc_errs(S,{error_path(El,schema),?MODULE,{illegal_element_content,Other}})
    end;
element_forbidden_content([T=#xmlText{}|Rest],S) ->
    case is_whitespace(T) of
	true ->
	    element_forbidden_content(Rest,S);
	_ ->
	    acc_errs(S,{error_path(T,schema),?MODULE,{illegal_element_content,T}})
    end.
	
c_t_properties(El,CT,S) ->
    c_t_properties(El#xmlElement.attributes,El,CT,S).
c_t_properties([#xmlAttribute{name=final,value=V}|Rest],El,CT,S) ->
    FinalValues = split_by_whitespace(V,[]),
    case legal_final_values(complexType,FinalValues) of
	{error,Reason} ->
	    Err = {error_path(El,schema),?MODULE,{illegal_final_values,Reason}},
	    c_t_properties(Rest,El,CT,acc_errs(S,Err));
	_ ->
	    c_t_properties(Rest,El,
			   CT#schema_complex_type{final=FinalValues},S)
    end;
c_t_properties([#xmlAttribute{name=block,value=V}|Rest],El,CT,S) ->
    BlockValues = split_by_whitespace(V,[]),
    case legal_block_values(complexType,BlockValues) of
	{error,Reason} ->
	    Err = {error_path(El,schema),?MODULE,
		   {illegal_block_values,Reason}},
	    c_t_properties(Rest,El,CT,acc_errs(S,Err));
	_ ->
	    c_t_properties(Rest,El,CT#schema_complex_type{block=BlockValues},S)
    end;
c_t_properties([#xmlAttribute{name=abstract,value=V}|Rest],El,CT,S) ->
    case boolean_to_atom(V) of
	error ->
	    Err = {error_path(El,schema),?MODULE,
		   {illegal_abstract_value,V}},
	    c_t_properties(Rest,El,CT,acc_errs(S,Err));
	V_atom ->
	    c_t_properties(Rest,El,CT#schema_complex_type{abstract=V_atom},S)
    end;
c_t_properties([_H|T],El,CT,S) ->
    c_t_properties(T,El,CT,S);
c_t_properties([],_,CT,S) ->
    {CT,S}.


legal_block_values(_,['#all']) ->
    true;
legal_block_values(element,BlockValues) ->
    list_members(BlockValues,[extension,restriction,substitution]);
legal_block_values(complexType,BlockValues) ->
    list_members(BlockValues,[extension,restriction]).
 
legal_final_values(_,['#all']) ->
    true;
legal_final_values(_,FinalValues) ->
    list_members(FinalValues,[extension,restriction]).
	
boolean_to_atom(B) when B=="1";B=="true" ->
    true;
boolean_to_atom(B) when B=="0";B=="false" ->
    false;
boolean_to_atom(_) ->
    error.


count_num_el(S=#xsd_state{num_el=N}) ->
    S#xsd_state{num_el=N+1}.
set_num_el(S=#xsd_state{},I) when is_integer(I) ->
    S#xsd_state{num_el=I};
set_num_el(S=#xsd_state{},#xsd_state{num_el=I}) ->
    S#xsd_state{num_el=I}.


occurance(El=#xmlElement{attributes=Atts},{Min,Max},S) ->
    AttVal=fun(#xmlAttribute{value=V},Sin) -> 
		   case catch mk_int_or_atom(V) of
		       {'EXIT',_} ->
			   Err = {error_path(El,schema),?MODULE,
				  {illegal_occurance_value,V}},
			   {V,acc_errs(Sin,Err)};
		       IAV -> {IAV,Sin}
		   end;
	      (V1,Sin) -> {V1,Sin}
	   end,
    {MinVal,S2} = AttVal(keyNsearch(minOccurs,#xmlAttribute.name,
				    Atts,Min),S),
    {MaxVal,S3} = AttVal(keyNsearch(maxOccurs,#xmlAttribute.name,
				    Atts,Max),S2),
    {{MinVal,MaxVal},S3}.

mk_int_or_atom(V="unbounded") ->
    list_to_atom(V);
mk_int_or_atom(V) when is_list(V) ->
    list_to_integer(V);
mk_int_or_atom(V) -> 
    V.

%% E is a complexType, possible kind of content is A)simpleContent, B)
%% complexContent or C) one or zero of 1)group,2)all,3)choice or
%% 4)sequence, followed by any number of attribute or attributeGroup
%% and finally one optional anyAttribute
mixed(E=#xmlElement{content=C},S) ->
    case {get_attribute_value(mixed,E,undefined),
	  [Y||Y=#xmlElement{}<-C,kind(Y)==simpleContent]} of
	{_,[_SCE]} ->
	    {false,S}; %% mixed is false in simpleContent
	{undefined,_} ->
	    case [X||X=#xmlElement{}<-C,
		     kind(X)==complexContent] of
		[E2] -> 
%%		    {get_attribute_value(mixed,E2,false),S};
		    mixed(E2,S);
		_ -> 
		    {false,S}
	    end;
	{M,_} when M=="1";M=="true" -> {true,S};
	{M,_} when M=="0";M=="false" -> {false,S};
	{M,_} -> 
	    Err = {error_path(E,schema),?MODULE,{invalid_mixed_value,M}},
	    {false,acc_errs(S,Err)}
    end.

mixify(false,CM) ->
    CM;
mixify(true,CM) ->
    mixify2(CM,[optional_text]).
mixify2([],Acc) ->
    reverse(Acc);
mixify2([H|T],Acc) ->
    mixify2(T,[optional_text,H|Acc]).

complexity([]) ->
    undefined;
complexity([#xmlText{}|T]) ->
    complexity(T);
complexity([#xmlComment{}|T]) ->
    complexity(T);
complexity([H|T]) ->
    case kind(H) of
	simpleContent ->
	    simple;
	complexContent ->
	    complex;
	_ ->
	    complexity(T)
    end.

%% Validation takes care of the following:
%% 1)	a) Check that targetNamespace attribute in schema matches
%% 	namespace URI if the element.
%% 	b) If schema don't have a targetNamespace the instance element
%% 	must not be namespace-qualified

%% 2)	a) Examine type of the element according to the schema and block
%%           attributes in the element decl.
%% 	b) Default values and other infoset contributions are applied.

%% 3)	Check the immediate attributes and contents of the element
%%         comparing these against the attributes and contents
%%         permitted.
%% 	a) simple type: 
%% 	   -verify there are no attributes or elements.
%% 	   -verify character content matches rules for type.
%% 	b) complex type:
%% 	   -verify attributes present and values ok.
%% 	   -check subelements according to content model.
%% validate_xml/2
validate_xml(El = #xmlElement{name=Name},
	     S=#xsd_state{table=Tab,schemaLocations=SchemaLocations}) ->
    ElQName = {_,_,Namespace} = mk_EII_QName(Name,El,S),
    SchemaCM = get_schema_cm(Tab,Namespace),
    case [X||X={element,{QName,Occ}} <- SchemaCM#schema.content,
	     cmp_name(ElQName,QName,S),
	     at_least_one(Occ)] of
	[Obj] ->
	    {Object,S2} = load_object(Obj,S),
	    validate_xml(El,Object,S2);
	_ ->
	    %% In case the namespace in El is not processed even
	    %% though it is present.
	    case is_already_processed(Namespace,S) of
		true -> %% nothing more to do
		    {error,{error_path(El,Name),?MODULE,
			    {element_not_in_schema,[Name,ElQName,SchemaCM]}}};
		_ ->
		    case keysearch(if_atom_to_list(Namespace),1,SchemaLocations) of
			{value,{_,Location}} ->
			    %% namespace present by schemaLocation
			    %% attribute in instance.
			    S1 = prefix_namespace_2global(Namespace,El#xmlElement.namespace,S),
			    S2 = save_namespace_definition(Namespace,S1),
			    S3 = process_external_schema(Location,S2#xsd_state{targetNamespace=Namespace}),
			    validate_xml(El,S3);
			_ -> %% namespace not imported in schema or instance.
			    {error,{error_path(El,Name),?MODULE,
				    {element_not_in_schema,[Name,ElQName,SchemaCM]}}}
		    end
	    end
    end.
%% validate_xml/3
validate_xml(XMLEl=#xmlElement{},SEl=#schema_element{},S) ->
    %% check that targetNamespace of schema matches URI of the element.
    case check_target_namespace(XMLEl,S) of
	ok ->
	    %% Extract the schemaLocation links in the instance,
	    %% examine type of the element according to the schema and
	    %% the block attributes in the element declaration
	    S2 = schemaLocations(XMLEl,S),
	    ?debug("schemaLocations: ~p~n",[S2#xsd_state.schemaLocations]),
	    #schema_element{name=_Name,type=_Type,block=Bl} = SEl,
	    Block = blocking(Bl,S2#xsd_state.blockDefault), %% complex types, elements
	    Ret = check_element_type([XMLEl],SEl,[],Block,S2,[]),
	    case Ret of
		{ValXML,UnvalRest,S3} ->
%%		    S4 = schema_concistence_checks(S3),
		    {ValXML,UnvalRest,S3};
		_ ->
		    Ret
	    end;
	_ ->
	    Err = {error_path(XMLEl,XMLEl#xmlElement.name),?MODULE,
		   {target_namespace_missmatch}},
	    {XMLEl,[],acc_errs(S,Err)}
    end.

%% check_element_type/3
%% examine type according to schema including info of block
%% attributes. If complex type do test recursively
%% 2 often
check_element_type(XML=[XMLTxt=#xmlText{}|Rest],CM=[CMEl|CMRest],Env,
		   Block,S,Checked) ->
    %% XMLTxt is the first part of the elements content,
    %% CMEl is the allowed type according to the schema
    case is_whitespace(XMLTxt) of
	true -> %% Ignore XMLEl
	    check_element_type(Rest,CM,Env,Block,S,[XMLTxt|Checked]);
	_ -> %% CMEl allows optional_text or is an absent optional element
	    {ResolvedT,S2} = resolve(CMEl,S),
	    case check_text_type(XML,ResolvedT,S2) of
		{error,Reason} ->
		    case is_optional(CMEl,S) of
			true ->
			    check_element_type(XML,CMRest,Env,Block,S,Checked);
			_ ->
			    check_element_type(Rest,CM,Env,Block,
					       acc_errs(S,Reason),Checked)
		    end;
		{Ret,Rest2,S3} ->
		    check_element_type(Rest2,CMRest,Env,Block,S3,reverse(Ret,Checked))
	    end
    end;
%% If CMEl is a sequence more than the first element of the XML list
%% may match.
check_element_type(XML=[#xmlElement{}|_],[{sequence,{CM,Occ}}|_CMRest],
		   Env,_Block,S,Checked) ->
    ?debug("calling sequence/6~n",[]),
    check_sequence(XML,CM,Occ,Env,set_num_el(S,0),Checked);
check_element_type(XML=[#xmlElement{}|_],[{choice,{CM,Occ}}|_CMRest],
		   Env,_Block,S,Checked) ->
    ?debug("calling choice/6~n",[]),
    check_choice(XML,CM,Occ,Env,set_num_el(S,0),Checked);
check_element_type(XML=[#xmlElement{}|_],[{all,{CM,Occ}}|_CMRest],
		   Env,_Block,S,Checked) ->
    ?debug("calling choice/6~n",[]),
    check_all(XML,CM,Occ,Env,set_num_el(S,0),Checked,XML); %%LTH
%% 3 often. CMEL may be ((simpleType | complexType)?, (unique | key | keyref)*))
check_element_type(XML=[XMLEl=#xmlElement{}|_],[CMEl|CMRest],Env,
		   Block,S,Checked) ->
    %% Three possible releations between XMLEl - CMEl:
    %% (1) XMLEl matches CMEl.
    %% (2) XMLEl don't matches CMEl and CMEl is optional.
    %% (3) XMLEl don't matches CMEl, CMEl mandatory, - error.
    %% On the other side may CMEl also match more elements in
    %% Rest. This should come down to 2) next function call.

    {ResolvedT,S2} = resolve(CMEl,S),
    case check_element_type(XML,ResolvedT,Env,Block,S2,[]) of
	{error,Reason} -> % 3
	    check_element_type(tl(XML),CMRest,Env,Block,
			       acc_errs(S,Reason),[XMLEl|Checked]);
	{[],_,_} -> % 2
	    check_element_type(XML,CMRest,Env,Block,S,Checked);
	{XMLEl2,RestXML,S3} -> % 1 This return value does not conform to the others
	    check_element_type(RestXML,[decrease_occurance(CMEl)|CMRest],Env,
			       Block,S3,XMLEl2++Checked)
    end;
check_element_type([],[],_Env,_Block,S,Checked) ->
    {Checked,[],S};
check_element_type([],[CMEl|CMRest],Env,Block,S,Checked) ->
    case is_optional(CMEl,S) of
	true ->
	    check_element_type([],CMRest,Env,Block,S,Checked);
	_ ->
	    Err = {error_path(Checked,undefined),?MODULE,
		   {missing_mandatory_element,CMEl}},
	    {Checked,[],acc_errs(S,Err)}
    end;
check_element_type(_XML=[],
		   #schema_complex_type{name=_Name,base_type=BT,
					complexity=simple,
					content=_C} = CT,
		   _Env,_Block,S,Checked) ->

    %% maybe check attributes here as well.
    {ResolvedType,_} = resolve({simple_or_complex_Type,BT},S),
    case ResolvedType of
	#schema_simple_type{} ->
	    {NewVal,S2} = check_type(ResolvedType,[],unapplied,S),
	    {NewVal,[],S2};
	{simpleType,_} ->
	    {NewVal,S2} = check_type(ResolvedType,[],unapplied,S),
	    {NewVal,[],S2};
	_ ->
	    {error,{error_path(Checked,undefined),?MODULE,
		    {empty_content_not_allowed,CT}}}
    end;
check_element_type([],#schema_complex_type{name=_Name,block=_Bl,content=C},
		   _Env,_Block,S,Checked) ->
    %% This type must have an empty content to be valid
    case allow_empty_content(C) of
	true -> {[],[],S};
	false ->
	    {error,{error_path(Checked,undefined),?MODULE,
		    {empty_content_not_allowed,C}}}
    end;
check_element_type(C, {anyType, _}, _Env, _Block, S, _Checked) ->
    %% permitt anything
    {lists:reverse(C), [], S};

check_element_type(XML=[#xmlText{}|_],Type=#schema_simple_type{},
		    _Env,_Block,S,_Checked) ->
    check_text_type(XML,Type,S);
check_element_type(XML=[#xmlText{}|_],Type={simpleType,_NameNS},
		    _Env,_Block,S,_Checked) ->
    check_text_type(XML,Type,S);

check_element_type(XML=[#xmlText{}|_],
		   #schema_complex_type{name=_Name,base_type=BT,
					complexity=simple,
					content=_C},Env,Block,S,Checked) ->

    %% maybe check attributes here as well.
    {ResolvedType,_} = resolve({simple_or_complex_Type,BT},S),
    check_element_type(XML,ResolvedType,Env,Block,S,Checked);

%% single schema object
check_element_type(XML=[_H|_],
		   #schema_complex_type{name=Name,block=Bl,content=C},
		   Env,_Block,S,Checked) ->
    EnvName = case Name of
		  {LN,_Scope,_NS} -> LN;
		  _ -> anonymous
	      end,
    Block = blocking(Bl,S#xsd_state.blockDefault),
    check_element_type(XML,C,[EnvName|Env],Block,name_scope(Name,S),Checked);

%% 1
check_element_type(XML=[XMLEl=#xmlElement{name=Name}|RestXML],
		   CMEl=#schema_element{name=CMName,type=Type},
		   Env,Block,S,Checked) ->
    ElName = mk_EII_QName(Name,XMLEl,S#xsd_state{scope=element(2,CMName)}),
    {Min,Max} = CMEl#schema_element.occurance,
    case cmp_name(ElName,CMName,S) of %% substitutionGroup
	true when S#xsd_state.num_el =< Max ->
	    S1 = id_constraints(CMEl,XMLEl,S),
	    %% If CMEl element has a substitutionGroup we have to
	    %% switch to the rigth element and type here.
	    {CMEl2,Type2,S2} =
		if 
		    ElName =:= CMName ->
			{CMEl,Type,S1};
		    true ->
			case resolve({element,ElName},S1) of
			    {SESub=#schema_element{type=SubType},Ssub} ->
				{SESub,SubType,Ssub};
			    {_,Ssub} ->
				{CMEl,Type,Ssub}
			end
		end,
			
	    {ResolvedType,S3} = resolve(Type2,XMLEl,S2), 
	    %% What's the value of Resolve?: It must be a simpleType,
	    %% complexType or an identity-constraint object
	    XsiFactors  = xsi_factors(CMEl2),
	    {XMLEl2,S4} = check_attributes(XMLEl,ResolvedType,
					   XsiFactors,S3),
	    S5 = check_abstract(ElName,XMLEl,CMEl,S4),
	    S6 = check_form(ElName,Name,XMLEl,
			    actual_form_value(CMEl#schema_element.form,
					      S5#xsd_state.elementFormDefault),
			    S5), 
	    %Step into content of XML element.
	    {Content,_,S7} =
		case
		    check_element_type(XMLEl2#xmlElement.content,
				       ResolvedType,Env,
				       Block,S6,Checked) of
		    {error,Reason} ->
			{XMLEl2#xmlElement.content,[],acc_errs(S6,Reason)};
		    Result ={_,[],_} -> Result;
		    {_,UnexpectedRest,_} ->
			Err = {error_path(XMLEl,Name),?MODULE,
			       {unexpected_rest,UnexpectedRest}},
			{XMLEl2#xmlElement.content,[],
			 acc_errs(S6,Err)}
		end,
	    {[XMLEl2#xmlElement{content=reverse(Content)}],
	     RestXML,
	     set_scope(S5#xsd_state.scope,set_num_el(S7,S6))};
	true ->
	    {error,{error_path(XMLEl, Name), ?MODULE,
		    {element_not_suitable_with_schema, ElName, S}}};
	_ when S#xsd_state.num_el >= Min -> 
	    %% it may be a match error or an optional element not
	    %% present
	    {[], XML, S#xsd_state{num_el=0}}; 
	_ -> 
	    {error,{error_path(XMLEl,Name),?MODULE,
		    {element_not_suitable_with_schema,ElName,CMName,CMEl,S}}}
    end;
check_element_type(XML,#schema_group{content=[CM]},Env,Block,S,Checked) ->
    %% content may contain one of all | choice | sequence or empty
    check_element_type(XML,CM,Env,Block,S,Checked);
check_element_type(XML,#schema_group{content=[]},_Env,_Block,_S,_Checked) ->
    {error,{error_path(XML,undefined),?MODULE,{no_element_expected_in_group,XML}}};
check_element_type(XML=[#xmlElement{content=_Content}|_Rest],
		   {sequence,{Els,Occ}},Env,_Block,S,Checked) ->
    ?debug("calling sequence/6~n",[]),
    case check_sequence(XML,Els,Occ,Env,S#xsd_state{num_el=0},Checked) of
	Err = {error,_} ->
	    Err;
	{ValidContent,Rest2,S2} -> 
	    %% The sequence may consume more than one element
	    %%{ValidContent,Rest,acc_errs(S2,{sequence_unexpected_rest_objects,UnexpRest})}
	    {ValidContent,Rest2,S2}
    end;
check_element_type(XML=[#xmlElement{}|_Rest],
		   {choice,{Els,Occ}},Env,_Block,S,Checked) ->
    ?debug("calling choice/6~n",[]),

    case check_choice(XML,Els,Occ,Env,S#xsd_state{num_el=0},Checked) of
	Err = {error,_} ->
	    Err;
	{ValidContent,Rest2,S2} ->
	    %% The choice may consume more than one element
	    {ValidContent,Rest2,S2}
    end;
check_element_type(XML=[E=#xmlElement{name=Name}|Rest],
		   Any={any,{Namespace,_Occ={Min,_},ProcessorContents}},Env,
		   _Block,S,_Checked) ->
    ?debug("check any: {any,{~p,~p,~p}}~n",[Namespace,_Occ,ProcessorContents]),
    %% ProcessorContents any of lax | strict | skip
    %% lax: may validate if schema is found
    %% strict: must validate
    ElName = mk_EII_QName(Name,E,S),
    case cmp_any_namespace(ElName,Namespace,S) of
	true ->
	    case ProcessorContents of
		skip ->
		    {[E],Rest,S};
		lax ->
		    {[E],Rest,S};
%%		strict when Namespace==['##local'] ->
		strict ->
		    case member(absent,Namespace) of
			true ->
			    %% unqualified well-formed xml is required. The
			    %% xml is well-formed, check that it is
			    %% unqualified.
			    Traverse =
				fun(#xmlElement{nsinfo=[],
						attributes=Atts,
						content=C},
				    Sin,Fun) ->
					Sin2 = Fun(Atts,Sin,Fun),
					Fun(C,Sin2,Fun);
				   (#xmlAttribute{namespace=[]},Sin,_Fun) ->
					Sin;
				   (#xmlText{},Sin,_Fun) -> Sin;
				   ([H|T],Sin,Fun) ->
					Sin2 = Fun(H,Sin,Fun),
					Fun(T,Sin2,Fun);
				   ([],Sin,_Fun) ->
					Sin;
				   (El,Sin,_Fun) -> 
					Err = {error_path(E,Name),?MODULE,
					       {illegal_component_in_any,El}},
					acc_errs(Sin,Err)
				end,
			    S2 = Traverse(E,S,Traverse),
			    {[E],Rest,S2};
		       _ ->
			    {Result,S2}=check_any(E,Any,Env,S),
			    {[Result],Rest,S2}
		    end
	    end;
	false when S#xsd_state.num_el >= Min ->
	    {[],XML,S};
	_ ->
	    {error,{error_path(E,Name),?MODULE,{element_bad_match,E,Any,Env}}}
    end;
check_element_type([],CM,_Env,_Block,S,Checked) ->
    %% #schema_complex_type, any, #schema_group, anyType and lists are
    %% catched above.
    case CM of
	#schema_simple_type{} ->
	    {NewVal,S2} = check_type(CM,[],unapplied,S),
	    {NewVal,[],S2};
	{simpleType,_} ->
	    {NewVal,S2} = check_type(CM,[],unapplied,S),
	    {NewVal,[],S2};
	_ ->
	    {error,{error_path(Checked,undefined),?MODULE,
		    {empty_content_not_allowed,CM}}}
    end;
check_element_type([C = #xmlComment{} |Rest],CM,Env,Block,S,Checked) ->
     check_element_type(Rest,CM,Env,Block,S,[C |Checked]);
check_element_type(XML,CM,_Env,_Block,S,_Checked) ->
    {error,{error_path(XML,undefined),?MODULE,{match_failure,XML,CM,S}}}.

%% single xml content object and single schema object
check_text_type(XML=[#xmlText{}|_],optional_text,S) ->
%    {XMLTxt,optional_text};
    {XMLText,Rest} = split_xmlText(XML),
    {XMLText,Rest,S};
check_text_type(XML=[Txt=#xmlText{}|_],Type={simpleType,_},S) ->
    {XMLText,Rest} = split_xmlText(XML),
    {NewVal,S2}=check_type(Type,flatten([X||#xmlText{value=X}<-XMLText]),unapplied,S),
    {[Txt#xmlText{value=NewVal}],Rest,S2};
check_text_type(XML=[Txt=#xmlText{}|_],Type=#schema_simple_type{},S) ->
    {XMLText,Rest} = split_xmlText(XML),
    {NewVal,S2}=check_type(Type,flatten([X||#xmlText{value=X}<-XMLText]),unapplied,S),
    {[Txt#xmlText{value=NewVal}],Rest,S2};
check_text_type([XMLTxt=#xmlText{}|_],CMEl,_S) ->
    {error,{error_path(XMLTxt,undefined),?MODULE,
	    {cannot_contain_text,XMLTxt,CMEl}}}.

split_xmlText(XML) ->
    splitwith(fun(#xmlText{}) -> true;(#xmlComment{}) -> true;(_) -> false end,XML).

%% Sequence
check_sequence([T=#xmlText{}|Rest],Els,Occ,Env,S,Checked) ->
    check_sequence(Rest,Els,Occ,Env,S,[T|Checked]);
check_sequence(Seq=[_InstEl=#xmlElement{}|_],[El|Els],Occ={_Min,_Max},Env,S,Checked) ->
    %% El any of (element | group | choice | sequence | any)*

    {ResolvedT,S2} = resolve(El,S),
    case check_element_type(Seq,ResolvedT,Env,[],count_num_el(S2),[]) of
	{[],_,S3} -> %% An optional element not present or maybe content == [].
	    case is_optional(El,S3) of
		true ->
		    check_sequence(Seq,Els,Occ,Env,set_num_el(S3,0),Checked);
		_ ->
		    {error,{error_path(Checked,undefined),?MODULE,
			    {missing_mandatory_elements,El}}}
	    end;
	Err={error,_Reason} ->
	    case {is_optional(El,S),S#xsd_state.num_el,get_occur(El)} of
		{true,_,_} ->
		    check_sequence(Seq,Els,Occ,Env,set_num_el(S,0),Checked);
		{_,N,{_Min2,Max}} when N>=Max ->
		    check_sequence(Seq,Els,Occ,Env,set_num_el(S,0),Checked);
		_ ->
		    Err
	    end;
%% 	{error,_Reason} when Min==0 -> %% optional element
%% 	    {[],Seq,S}; %% {Checked,Seq,S}
%% 	{error,_Reason} when S#xsd_state.num_el >= Max ->
%% 	    %% This failure because of number limit
%% 	    {Checked,Seq,S};
%% 	Err = {error,_Reason} ->
%% 	    %% Even though this match failed
%%	    Err;
	{Ret,UnValRest,S3} ->
	    %% must also take care of more elements of same name
	    %% decrease occurance in El for the optional measurements
	    %% when Seq is empty.
	    check_sequence(UnValRest,[decrease_occurance(El)|Els],Occ,Env,
			   count_num_el(set_num_el(S3,S2)),
			   Ret++Checked)
    end;
check_sequence([C = #xmlComment{} |Rest], Els, Occ, Env, S, Checked) ->
    check_sequence(Rest,Els,Occ,Env,S,[C |Checked]);
check_sequence(Rest,[],_Occ,_Env,S,Checked) ->
    {Checked,Rest,set_num_el(S,0)};
check_sequence([],Els,_Occ,_Env,S,Checked) ->
    case [X||X={_,Y={_,_}} <- Els,optional(Y)==false] of
	[] ->
	    {Checked,[],set_num_el(S,0)};
	MandatoryEls ->
	    {error,{error_path(Checked,undefined),?MODULE,
		    {missing_mandatory_elements,MandatoryEls}}}
    end.
%%check_sequence(Seq,[],_Occ,_Env,_S,_Checked) ->
    %%{error,{unmatched_elements,Seq}}.


%% Choice one alternative must occur unless all alternatives are
%% optional or the entire choice is optional.
check_choice([T=#xmlText{}|Rest],Els,Occ,Env,S,Checked) ->
    case is_whitespace(T) of
	true ->
	    check_choice(Rest,Els,Occ,Env,S,[T|Checked]);
	_ ->
	    {error,{error_path(T,undefined),?MODULE,
		    {choice_missmatch,T,Els}}}
    end;
check_choice(Ch=[#xmlElement{}|_],[El|Els],Occ,Env,S,Checked) ->
    {ResolvedT,S2} = resolve(El,S),
    case check_element_type(Ch,ResolvedT,Env,[],count_num_el(S2),[]) of
	{[],_,_S3} -> %% not matched optional element
	    check_choice(Ch,Els,Occ,Env,S2,Checked);
	{error,_Reason} -> %% This may happen but not for the
	    %% last alternative element unless the
	    %% entire choice is optional. So, just
	    %% continue.
	    case [X||X=#xmlElement{}<-Checked] of
		[] ->
		    check_choice(Ch,Els,Occ,Env,S2,Checked);
		_ ->
		    {Checked,Ch,set_num_el(S,0)}
	    end;
	{Result,UnValRest,S3} -> %% in this case only more elements of
                                 %% El may be allowed
	    check_choice(UnValRest,[El],Occ,Env,
			 count_num_el(set_num_el(S3,S)),Result++Checked)
    end;
check_choice([],_,_,_,S,Checked) ->
    {Checked,[],set_num_el(S,0)};
check_choice(XML,[],{0,_},_,S,Checked) ->
    %% Choice is optional
    {Checked,XML,set_num_el(S,0)};
check_choice(XML,[],_,_,S,Checked) ->
    %% Choice has already matched something, the rest is for somthing
    %% else to match.
    case S#xsd_state.num_el > 0 of
	true ->
	    {Checked,XML,set_num_el(S,0)};
	_ ->
	    {error,{error_path(XML,undefined),?MODULE,
		    {no_element_matching_choice,XML}}}
    end.

check_all([T=#xmlText{}|RestXML],CM,Occ,Env,S,Checked,XML) ->
    case is_whitespace(T) of
	true ->
	    check_all(RestXML,CM,Occ,Env,S,[T|Checked],XML);
	_ ->
	    {error,{error_path(T,undefined),?MODULE,{all_missmatch,T,CM}}}
    end;
check_all(XML=[E=#xmlElement{name=Name}|RestXML],CM,Occ,Env,S,
	  Checked,PrevXML) ->
    ElName = mk_EII_QName(Name,E,S),
    case search_delete_all_el(ElName,CM,S) of
	{CMEl={element,_},RestCM} ->
	    {ResolvedT,S2} = resolve(CMEl,S),
	    case check_element_type(XML,ResolvedT,Env,[],S2,[]) of
		{[],_,_S3} ->
		    Err = {error_path(E,Name),?MODULE,
			   {validation_error_all,ElName,CM}},
		    check_all(RestXML,CM,Occ,Env,acc_errs(S,Err),
			      Checked,PrevXML);
		{error,_} when element(1,Occ)==0 ->
		    {[],PrevXML,S};
		{error,Reason} ->
		    check_all(RestXML,RestCM,Occ,Env,
			      acc_errs(S,Reason),[E|Checked],PrevXML);
		{Result,UnValRest,S3} ->
		    check_all(UnValRest,RestCM,Occ,Env,
			      S3#xsd_state{scope=S#xsd_state.scope},
			      Result++Checked,PrevXML)
	    end;
	_  when element(1,Occ) == 0 ->
	    {[],PrevXML,S};
	_ ->
	    Err = {error_path(E,Name),?MODULE,
		   {element_not_in_all,ElName,E,CM}},
	    check_all(RestXML,CM,Occ,Env,acc_errs(S,Err),[E|Checked],PrevXML)
    end;
check_all([C=#xmlComment{} |RestXML], CM, Occ, Env, S, Checked, XML) ->
    check_all(RestXML, CM, Occ, Env, S, [C |Checked], XML);
check_all(XML,[],_,_,S,Checked,_) ->
    {Checked,XML,S};
check_all([],CM,_Occ,_,S,Checked,_PrevXML) ->
    case [X||X={_,Y={_,_}} <- CM,optional(Y)==false] of
	[] ->
	    {Checked,[],set_num_el(S,0)};
	MandatoryEls ->
	    {error,{error_path(Checked,undefined),?MODULE,
		    {missing_mandatory_elements_in_all,MandatoryEls}}}
    end.

check_any(E,Any,_Env,S) ->
    case catch validate_xml(E,S#xsd_state{scope=[]}) of
	{[Result],[],S2} ->
	    {Result,S2#xsd_state{scope=S#xsd_state.scope}};
	{Result,[],S2} ->
	    {Result,S2#xsd_state{scope=S#xsd_state.scope}};
	{_,_Unvalidated,S2} ->
	    Err = {error_path(E,undefined),?MODULE,{failed_validating,E,Any}},
	    {E,acc_errs(S2#xsd_state{scope=S#xsd_state.scope},Err)};
	{error,Reason} -> 
	    {E,acc_errs(S,Reason)};
	{'EXIT',Reason} ->
%%	    {E,acc_errs(S,format_error({internal_error,Reason},E,Any,Env))}
	    Err = {error_path(E,undefined),?MODULE,{internal_error,Reason}},
	    {E,acc_errs(S,Err)}
    end.

check_target_namespace(XMLEl,S) ->
    case {S#xsd_state.targetNamespace,XMLEl#xmlElement.nsinfo} of
	{undefined,[]} ->
	    ok;
	{URI,{Prefix,_}} ->
	    NS = XMLEl#xmlElement.namespace,
	    case namespace(Prefix,NS,NS#xmlNamespace.default) of
		URI ->
		    ok;
		_ ->
		    failed
	    end;
	{URI,_} ->
	    case (XMLEl#xmlElement.namespace)#xmlNamespace.default of
		URI ->
		    ok;
		_ ->
		    failed
	    end
    end.

schemaLocations(El=#xmlElement{attributes=Atts},S) ->
    Pred = fun(#xmlAttribute{name=schemaLocation}) -> false;
	      (#xmlAttribute{nsinfo={_,"schemaLocation"}}) -> false;
	      (_) -> true
	   end,
    case lists:dropwhile(Pred,Atts) of
	[] ->
	    S;
	[#xmlAttribute{value=Paths}|_] ->
	    case string:tokens(Paths," \n\t\r") of
		L when length(L) > 0 ->
		    case length(L) rem 2 of
			0 ->
			    PairList = 
				fun([],_Fun) ->
					[];
				   ([SLNS,SLLoc|Rest],Fun) ->
					[{SLNS,SLLoc}|Fun(Rest,Fun)]
				end,
			    S#xsd_state{schemaLocations=PairList(L,PairList)};
			_ ->
			    Err = {error_path(El,El#xmlElement.name),?MODULE,
				   {schemaLocation_list_failure,Paths}},
			    acc_errs(S,Err)
		    end;
		_ ->
		    S
	    end;
	_ ->
	    S
    end.

blocking([],BlockDefault) ->
    BlockDefault;
blocking(Block,_) ->
    Block.

allow_empty_content([]) ->
    true;
allow_empty_content([{restriction,{_BT,_CM=[]}}]) ->
    true;
allow_empty_content([{extension,{_BT,_CM=[]}}]) ->
    true;
allow_empty_content([{_,{_,{0,_}}}|Rest]) ->
    allow_empty_content(Rest);
allow_empty_content([{_,{Content,_}}|Rest]) ->
     case allow_empty_content(Content) of
	 true ->
	     allow_empty_content(Rest);
	 _ -> false
     end;
allow_empty_content(_) ->
    false.

empty_xml_content([]) ->
    true;
empty_xml_content([H|T]) ->
    case is_whitespace(H) of
	true ->
	    empty_xml_content(T);
	_ ->
	    false
    end;
empty_xml_content(_) ->
    false.

xsi_factors(#schema_element{nillable=N}) ->
    [{nillable,N}].
check_xsi_factors({nil,_,?XSD_INSTANCE_NAMESPACE},
		  #xmlAttribute{value="true"},XsiFactors,XMLEl,S) ->
    case key1search(nillable,XsiFactors,false) of
	{_,true} ->
	    case empty_xml_content(XMLEl#xmlElement.content) of
		true ->
		    S;
		_ ->
		    Err = {error_path(XMLEl,XMLEl#xmlElement.name),?MODULE,
			   {element_content_not_nil,XMLEl}},
		    acc_errs(S,Err)
	    end;
	_ ->
	    S
    end;
check_xsi_factors(_,_,_,_,S) ->
    S.

check_attributes(XMLEl=#xmlElement{attributes=Atts},
		 #schema_complex_type{name=Name,attributes=SchemaAtts},
		 XsiFactors,S) ->
    %% For each att in Atts check that it is allowed, and has right type.
    %% For each att in CT that is required check that it exists. Apply
    %% none present atts that have default values.
    OldScope = S#xsd_state.scope,
    SchemaAtts2 = resolve_attributeGroups(SchemaAtts,XMLEl,S),
    {XMLEl2,S2}=check_attributes(Atts,SchemaAtts2,XMLEl,XsiFactors,
				 name_scope(Name,S),[]),
    {XMLEl2,S2#xsd_state{scope=OldScope}};
check_attributes(XMLEl=#xmlElement{attributes=[]},_,_,S) ->
    {XMLEl,S};
check_attributes(XMLEl=#xmlElement{name=N,attributes=Atts},_,XsiFactors,S) ->
    Fun =
	fun(AttX,S_in) -> 
		case reserved_attribute(AttX,XMLEl#xmlElement.namespace) of
		    true ->
			AttQName = 
			    mk_EII_QName(AttX#xmlAttribute.name,XMLEl,S_in),
			check_xsi_factors(AttQName,AttX,XsiFactors,XMLEl,S_in);
		    _ ->
			Err = {error_path(XMLEl,N),?MODULE,
			       {attribute_in_simpleType,XMLEl,AttX}},
			acc_errs(S_in,Err)
		end
	end,
    {XMLEl,foldl(Fun,S,Atts)}.

check_attributes([],[SA|SchemaAtts],XMLEl,XsiFactors,S,CheckedAtts) ->
    case resolve(SA,S) of
	{#schema_attribute{name=Name,use=Use,default=Def,fixed=Fix},S2} ->
	    case {Use,Def,Fix} of
		{required,_,_} ->
		    Err = {error_path(XMLEl,XMLEl#xmlElement.name),?MODULE,
			   {required_attribute_missed,XMLEl,Name}},
		    check_attributes([],SchemaAtts,XMLEl,XsiFactors,
				     acc_errs(S2,Err),CheckedAtts);
		{optional,undefined,undefined} ->
		    check_attributes([],SchemaAtts,XMLEl,XsiFactors,
				     S2,CheckedAtts);
		{optional,Default,undefined} ->
		    NewAtt = create_attribute(Name,Default),
		    check_attributes([],SchemaAtts,XMLEl,XsiFactors,S2,
				     [NewAtt|CheckedAtts]);
		{optional,undefined,Fix} ->
		    NewAtt = create_attribute(Name,Def),
		    check_attributes([],SchemaAtts,XMLEl,XsiFactors,S2,
				     [NewAtt|CheckedAtts]);
		{optional,Default,Fix} ->
		    Err = {error_path(XMLEl,XMLEl#xmlElement.name),?MODULE,
			   {default_and_fixed_attributes_mutual_exclusive,
			    Name,Default,Fix}},
		    check_attributes([],SchemaAtts,XMLEl,XsiFactors,
				     acc_errs(S2,Err),CheckedAtts);
		_ ->
		    check_attributes([],SchemaAtts,XMLEl,XsiFactors,
				     S2,CheckedAtts)
	    end;
	{{anyAttribute,{_Namespaces,_PC}},S2} ->
	    check_attributes([],SchemaAtts,XMLEl,XsiFactors,
			     S2,CheckedAtts);	    
	Err ->
	    ErrMsg={error_path(XMLEl,XMLEl#xmlElement.name),?MODULE,
		    {schema_error,unexpected_object,SA,Err}},
	    check_attributes([],SchemaAtts,XMLEl,XsiFactors,
			     acc_errs(S,ErrMsg),CheckedAtts)
    end;
check_attributes([],[],XMLEl,_XsiFactors,S,CheckedAtts) ->
    {XMLEl#xmlElement{attributes=reverse(CheckedAtts)},S};
check_attributes([Att|Atts],SchemaAtts,XMLEl,XsiFactors,
		 S,CheckedAtts) ->
%%    AttQName = mk_EII_QName(Att#xmlAttribute.name,XMLEl,S),
    {IsQ,AttQName} = mk_EII_Att_QName(Att#xmlAttribute.name,XMLEl,S),
    case search_attribute(IsQ,AttQName,SchemaAtts) of
	{AttObj={attribute,_},SchemaAtts2} ->
	    {SA,S2} = load_object(AttObj,S),
	    #schema_attribute{type=[AttType]} = SA, 
	    {Val,S4} = check_type(AttType,
				  Att#xmlAttribute.value, unapplied,S2),
	    check_attributes(Atts,SchemaAtts2,XMLEl,XsiFactors,S4,
			     [Att#xmlAttribute{value=Val}|CheckedAtts]);
	{undefined,SchemaAtts2} ->
	    %% check for reserved attributes or anyAttribute
	    case reserved_attribute(Att,XMLEl#xmlElement.namespace) of
		true ->
		    S2 = check_xsi_factors(AttQName,Att,XsiFactors,XMLEl,S),
		    check_attributes(Atts,SchemaAtts2,XMLEl,XsiFactors,
				     S2,[Att|CheckedAtts]);
		_ ->
		    case check_anyAttribute(Att,SchemaAtts2,XMLEl,S) of
			{error,Reason} ->
			    check_attributes(Atts,SchemaAtts2,XMLEl,XsiFactors,
					     acc_errs(S,Reason),CheckedAtts);
			{Att2,S2} ->
			    check_attributes(Atts,SchemaAtts2,XMLEl,XsiFactors,
					     S2,[Att2|CheckedAtts])
		    end
	    end;
	Other ->
	    Err = {[],?MODULE,{internal_error,Other}},
	    check_attributes(Atts,SchemaAtts,XMLEl,XsiFactors,
			     acc_errs(S,Err),CheckedAtts)
    end.

check_anyAttribute(Att,SchemaAtts,El=#xmlElement{name=Name,namespace=NS},S) ->
    case [Any||Any={anyAttribute,_}<-SchemaAtts] of
	[] ->
	    {error,{error_path(El,Name),?MODULE,
		    {attribute_not_defined_in_schema,
		     Att#xmlAttribute.name}}};
	[{_,{Namespace,PC}}|_] ->
	    case check_anyAttribute_namespace(Namespace,NS) of
		ok ->
		    check_anyAttribute2(Namespace,PC,Att,NS,S);
		_ ->
		    {error,{error_path(El,Name),?MODULE,
			    {disallowed_namespace,Namespace,
			     NS,Att#xmlAttribute.name}}}
	    end
    end.
check_anyAttribute2(_,PC,Att,_,S) when PC==skip;PC==lax ->
    {Att,S};
check_anyAttribute2(_Namespace,_,Att,_NS,S) ->
    %% PC == strict
    {Att,S}.

check_anyAttribute_namespace(['##any'|_],_NS) ->
    ok;
check_anyAttribute_namespace([absent],_NS) ->
    ok;
check_anyAttribute_namespace([NS|_],NS) ->
    ok;
check_anyAttribute_namespace([{'not',NS}|_],NS) ->
    false;
check_anyAttribute_namespace([_H|T],NS) ->
    check_anyAttribute_namespace2(T,NS).
check_anyAttribute_namespace2([NS|_],NS) ->
    ok;
check_anyAttribute_namespace2([_H|T],NS) ->
    check_anyAttribute_namespace2(T,NS);
check_anyAttribute_namespace2([],_NS) ->
    false.
    
resolve_attributeGroups(SchemaAtts,El,S) ->
    resolve_attributeGroups(SchemaAtts,El,S,[],[]).
resolve_attributeGroups([AG={attributeGroup,_}|SchemaAtts],El,S,Parents,Acc) ->
    case resolve(AG,S) of
	{#schema_attribute_group{name=Name,content=AGC},_S2} ->
	    case {member(Name,Parents),S#xsd_state.redefine} of
		{true,false} ->
		    Err = {error_path(El,El#xmlElement.name),?MODULE,
			   {cirkular_attributeGroup_reference,Name}},
		    resolve_attributeGroups(SchemaAtts,El,acc_errs(S,Err),
					    Parents,Acc);
		{true,_} ->
		    resolve_attributeGroups(SchemaAtts,El,S,Parents,Acc);
		_  ->
		    resolve_attributeGroups(AGC++[marker|SchemaAtts],
					    El,S,[Name|Parents],Acc)
	    end;
	Err ->
	    ErrMsg={error_path(El,El#xmlElement.name),?MODULE,
		    {schema_error,unexpected_object,AG,Err}},
	    resolve_attributeGroups(SchemaAtts,El,acc_errs(S,ErrMsg),
				    Parents,Acc)
    end;
resolve_attributeGroups([marker|T],El,S,[_P|Ps],Acc) ->
    resolve_attributeGroups(T,El,S,Ps,Acc);
resolve_attributeGroups([H|T],El,S,Parents,Acc) ->
    resolve_attributeGroups(T,El,S,Parents,[H|Acc]);
resolve_attributeGroups([],_,_,_,Acc) ->
    Acc.

check_type(Type=#schema_simple_type{},Value,FacetS,S) ->
    check_simpleType(Type,Value,FacetS,S);
check_type({simpleType,{anySimpleType,_}},Value, _FacetS,S) ->
    {Value,S};
check_type({union,Types},Value,_FacetS,S) ->
    check_union_types(Types,Value,S);
check_type(ST={simpleType,QName={Name,_Scope,_NS}},Value, FacetS,S) ->
    case is_builtin_simple_type(QName) of
	true ->
	    {ConstrainedValue,S2} = 
		constrained(QName,default_facets(FacetS,Name),Value,S),
	    case xmerl_xsd_type:check_simpleType(Name,ConstrainedValue,S2) of
		{ok,_} when Name=='IDREF';Name=='IDREFS' ->
		    %% do something more
		    {ConstrainedValue,S2};
		{ok,_} ->
		    {ConstrainedValue,S2};
		{error,Reason} ->
		    ?debug("Error validating type: ~p~nwith value: ~p~n",[Name,Value]),
		    {Value,acc_errs(S2,Reason)}
	    end;
	_ ->
	    case resolve(ST,S) of
		{[],S2} ->
		    Err = {[],?MODULE,{could_not_resolve_type,ST}},
		    {Value,acc_errs(S2,Err)};
		{RefedST,S2} ->
		    check_type(RefedST,Value, unapplied,S2)
	    end
    end;
check_type(Type,Value, _FacetS,S) ->
    Err = {[],?MODULE,{could_not_check_value_for_type,Type}},
    ?debug("ERROR: not implemented: ~p~nfor value: ~p~n",[Type,Value]),
    {Value,acc_errs(S,Err)}.

check_simpleType(#schema_simple_type{base_type=BT,final=_Final,
				     facets=Facets,content=Type},
		 Value,FacetS,S) ->
    case {BT,Type} of
	{{_ST,_,_},_} ->
	    case is_builtin_simple_type(BT) of
		true ->
		    {ConstrainedValue,S2} = 
			constrained(BT,merge_facets(default_facets(FacetS,BT),Facets),Value,S),
		    {_,_S3} = check_type({simpleType,BT},ConstrainedValue,applied,S2);
		_ ->
		    case resolve({simpleType,BT},S) of
			{BaseST=#schema_simple_type{facets=Facets2},_} ->
			    check_simpleType(BaseST#schema_simple_type{facets=Facets++Facets2},Value,unapplied,S);
			_ ->
			    Err = {[],?MODULE,{unknown_simpleType,BT}},
			    {Value,acc_errs(S,Err)}
		    end
	    end;
	{_,[CT]} ->
	    {_,_S2} = check_type(CT,Value,unapplied,S)
    end.

check_union_types(Types,Value,S) ->
    check_union_types(Types,Types,Value,S).
check_union_types([],UT,Value,S) ->
    acc_errs(S,{[],?MODULE,{value_not_valid,Value,UT}});
check_union_types([T|Ts],UT,Value,S = #xsd_state{errors=Errs}) ->
    case check_type(T,Value,unapplied,S) of
	{Val,S2=#xsd_state{errors=Errs}} ->
	    {Val,S2};
	{_,_} ->
	    check_union_types(Ts,UT,Value,S)
    end.

reserved_attribute({RA,_,?XSD_INSTANCE_NAMESPACE},_)
  when RA==type;RA==nil;RA==schemaLocation;RA==noNamespaceSchemaLocation ->
    true;
reserved_attribute(#xmlAttribute{name=Name},#xmlNamespace{nodes=NSNodes}) ->
    NameStr = if
		  is_atom(Name) -> atom_to_list(Name);
		  true -> Name
	      end,
    case string:tokens(NameStr,":") of
	["xmlns"|_] ->
	    true;
	[Prefix,InstAtt] when InstAtt=="type";
			      InstAtt=="nil";
			      InstAtt=="schemaLocation";
			      InstAtt=="noNamespaceSchemaLocation" ->
	    case keyNsearch(?XSD_INSTANCE_NAMESPACE,2,NSNodes,[]) of
		{Prefix,_} ->
		    true;
		_ ->
		    false
	    end;
	_ ->
	    false
    end;
reserved_attribute(_,_) ->
    false.


default_facets(applied,_) ->
    [];
default_facets(_,Type) ->
    default_facets(Type).
default_facets({Name,_,_}) when is_list(Name) ->
    %% Type already proven to be a built in simple type
    default_facets(list_to_atom(Name));
default_facets({Name,_,_}) ->
    default_facets(Name);
default_facets(TypeName) ->
    case is_xsd_string(TypeName) of
	false ->
	    [{whiteSpace,"collapse"}];
	_ ->
	    []
    end.

merge_facets([],DefinedF) ->
    DefinedF;
merge_facets([F={Name,_}|Rest],DefinedF) ->
    %% At this moment only F has the allowed value
    merge_facets(Rest,keyreplace(Name,1,DefinedF,F)).
    
constrained({T,_,_},Facets,Value,S) ->
    FacetFuns = [facet_fun(T,F)||F<-Facets],
    constrained2(FacetFuns,Value,S).
constrained2([],Value,S) ->
    {Value,S};
constrained2([Facet|RestFacets],Value,S) ->
    case Facet(Value) of
	{error,Reason} ->
	    constrained2(RestFacets,Value,acc_errs(S,Reason));
	{ok,NewValue} ->
	    constrained2(RestFacets,NewValue,S)
    end.

id_constraints(CMEl,XMLEl,S) ->
    S1 = check_uniqueness(CMEl#schema_element.uniqueness,
			XMLEl,S),
    S2 = check_keys([X||{key,X}<-CMEl#schema_element.key],XMLEl,S1),
    prepare_keyrefs([X||{keyref,X}<-CMEl#schema_element.key],XMLEl,S2).

check_abstract(ElName,El,#schema_element{name=ElName,abstract=true},S) ->
    acc_errs(S,{error_path(El,El#xmlElement.name),?MODULE,
		{abstract_element_instance,ElName}});
check_abstract(ElName,_El,#schema_element{name=ElName},S) ->
    S;
check_abstract(ElName,El,#schema_element{},S) ->
    {XMLEl,_S2} = load_object({element,ElName},S),
    check_abstract(ElName,El,XMLEl,S).

%% Check of form compliance.
%% Globally declared elements may be qualified even though
%% elementformdefault = "unqualified".
%% If ActualFormValue = "qualified" locally defined names must be
%% explicitly or implicitly qualified.
%% check_form({LocalName,Scope,Namespace},LocalName,
%% InstanceNamespace,ActualFormDefault,S) -> NewS
check_form({LocalName,_,Namespace},LocalName,
	   El=#xmlElement{name=Name,namespace=NS},qualified,S) ->
    case NS#xmlNamespace.default of
	Namespace ->
	    S;
	_ ->
	    acc_errs(S,{error_path(El,Name),?MODULE,
			{qualified_name_required,LocalName}})
    end;
check_form({LocalName,_,_},LocalName,_El,_ActualFormDefault,S) ->
    S;
check_form({_LocalName,[],_},_QualifiedName,_El,_ActualFormDefault,S) ->
    S;
check_form({_LocalName,_,_},QualifiedName,El,unqualified,S) ->
    acc_errs(S,{error_path(El,El#xmlElement.name),?MODULE,
		{unqualified_name_required,QualifiedName}});
check_form({_LocalName,_,_},_QualifiedName,_El,_ActualFormDefault,S) ->
    S.

actual_form_value(undefined,GlobalForm) ->
    GlobalForm;
actual_form_value(LocalForm,_) ->
    LocalForm.


check_uniqueness(undefined,_,S) ->
    S;
check_uniqueness(Unique,XMLEl,S) ->
    case Unique of
	[{unique,#id_constraint{selector={selector,SelectorPath},
				fields=Fields}}] ->
	    TargetNodeSet = target_node_set(SelectorPath,XMLEl,S),
	    case qualified_node_set(Fields,TargetNodeSet,XMLEl,S) of
		{[],S1} -> S1;
		{[_E],S1} -> S1;
		{L,S1} when is_list(L) ->
		    key_sequence_uniqueness(L,XMLEl,S1)
	    end;
	_ -> S
    end.
    
target_node_set(SelectorPath,XMLEl,S) ->
    xmerl_xpath:string(SelectorPath,XMLEl,
		       [{namespace,S#xsd_state.namespace_nodes}]).

qualified_node_set(Fields,Set,El,S) ->
    qualified_node_set([X||{field,X} <- Fields],Set,El,S,[]).

qualified_node_set([],_Set,_El,S,Acc) ->
    {Acc,S};
qualified_node_set(_,[],_El,S,Acc) ->
    {Acc,S};
qualified_node_set(Paths,[QN|QNs],El,S,Acc) ->
    Fun = fun(P,Sx) -> 
		  case apply_field(P,QN,Sx) of
		      L when length(L) =< 1 -> % Part1:3.11.4.3
			  {L,Sx};
		      Err ->
			  RetErr =
			      {error_path(El,El#xmlElement.name),?MODULE,
			       {illegal_key_sequence_value,Err}},
			  {[],acc_errs(Sx,RetErr)}
		  end
	  end,
    {KeySequence,S2} = mapfoldl(Fun,S,Paths),
    case flatten(KeySequence) of
	[] ->
	    qualified_node_set(Paths,QNs,El,S2,Acc);
	KS ->
	    qualified_node_set(Paths,QNs,El,S2,[KS|Acc])
    end.

apply_field(F,El,S) ->
    %% xmerl_xpath:string returns a list
    xmerl_xpath:string(F,El,[{namespace,S#xsd_state.namespace_nodes}]).

check_keys([],_XMLEl,S) ->
    S;
check_keys([Key=#id_constraint{selector={selector,SelectorPath},
			       fields=Fields}|Keys],XMLEl,S) ->
    TargetNodeSet = target_node_set(SelectorPath,XMLEl,S),
    S3=
	case qualified_node_set(Fields,TargetNodeSet,XMLEl,S) of
	    {L,S1} when length(L)==length(TargetNodeSet) -> 
		%% Part1: 3.11.4.4.2.1
		S2 = key_sequence_uniqueness(L,XMLEl,S1),
		_ = save_key(Key#id_constraint{key_sequence=L},S2),
		S2;
	    {Err,S1} ->
		acc_errs(S1,{error_path(XMLEl,XMLEl#xmlElement.name),?MODULE,
			     {qualified_node_set_not_correct_for_key,Err}})
	end,
    check_keys(Keys,XMLEl,S3).

%% A reference to a key may occur in another environment than the key
%% was defined. Thus the key must be referenced after the whole
%% document has been processed. At this moment save the info about the
%% keyref and compare it with the key later.
prepare_keyrefs([],_XMLEl,S) ->
    S;
prepare_keyrefs([KeyRef=#id_constraint{selector={selector,SelectorPath},
				      fields=Fields}|Rest],XMLEl,S) ->
    TargetNodeSet = target_node_set(SelectorPath,XMLEl,S),
    {L,S1} = qualified_node_set(Fields,TargetNodeSet,XMLEl,S),
    save_keyref(KeyRef#id_constraint{key_sequence=L},S1),
    prepare_keyrefs(Rest,XMLEl,S1).



%% key_sequence_uniqueness(KeySequence,XMLElement,State)
%% Each element in KeySequence has same length and is a list of one or
%% more elements. key_sequence_uniqueness/2 checks that no two
%% elements has equal values. If it detects two (or more) elements
%% that have equal first subelements it must continue comparing the
%% other subelements of those elements. It returns the state with all
%% detected errors saved.
key_sequence_uniqueness([],_,S) ->
    S;
key_sequence_uniqueness([_H],_,S) ->
    S;
key_sequence_uniqueness([KS=[F1|FRest]|KSs],El,S) ->
    case is_key_sequence_equal(F1,KSs) of
	{true,TailOfEquals} ->
	    S1 =
		case k_s_u(FRest,TailOfEquals,S) of
		    true ->
			acc_errs(S,{error_path(El,El#xmlElement.name),?MODULE,
				    {key_value_not_unique,KS}});
		    _ ->
			S
		end,
	    key_sequence_uniqueness(KSs,El,S1);
	false ->
	    key_sequence_uniqueness(KSs,El,S)
    end.

k_s_u([],_,_) ->
    true;
k_s_u([F|Fs],KSs,S) ->
    case is_key_sequence_equal(F,KSs) of
	{true,TailOfEquals} ->
	    k_s_u(Fs,TailOfEquals,S);
	_ ->
	    false
    end.

is_key_sequence_equal(F,KSs) ->
    is_key_sequence_equal(F,KSs,[]).
is_key_sequence_equal(_F,[],[]) ->
    false;
is_key_sequence_equal(_F,[],Acc) ->
    {true,reverse(Acc)};
is_key_sequence_equal(F,[[F1|TlF1]|Rest],Acc) ->
    case is_key_el_equal(F,F1) of
	true ->
	    is_key_sequence_equal(F,Rest,[TlF1|Acc]);
	false ->
	    is_key_sequence_equal(F,Rest,Acc)
    end.

%% This test must be more elaborated considering the equal facet
is_key_el_equal(#xmlElement{content=C1},#xmlElement{content=C2}) ->
    %% content must be empty or text since elements must be of
    %% simpleType
    is_equal_content(C1,C2);
is_key_el_equal(#xmlAttribute{value=V1},#xmlAttribute{value=V1}) ->
    true;
is_key_el_equal(_,_) ->
    false.

is_equal_content([T1|Rest1],[T2|Rest2]) 
  when is_record(T1,xmlText),is_record(T2,xmlText) -> 
    case is_whitespace(T1) of
	true ->
	    case is_whitespace(T2) of
		true ->
		    is_equal_content(Rest1,Rest2);
		_ ->
		    is_equal_content(Rest1,[T2|Rest2])
	    end;
	_ ->
	    case T1#xmlText.value==T2#xmlText.value of
		true ->
		    is_equal_content(Rest1,Rest2);
		_ ->
		    false
	    end
    end;
is_equal_content([],[]) ->
    true;
is_equal_content(_,_) ->
    false.

schema_concistence_checks(S) ->
    S2 = check_keyrefs(S),
    S3 = check_references(S2),
    S4 = check_substitutionGroups(S3#xsd_state.substitutionGroups,S3),
    S5 = check_cyclic_defs(S4),
    reset_state(S5).

reset_state(S) ->
    S#xsd_state{keyrefs=[],
		'IDs'=[],
		unchecked_references=[],
		substitutionGroups=[],
		derived_types=[],
		circularity_stack=[],
		circularity_disallowed=[]}.

check_keyrefs(S) ->
    KeyRefs = S#xsd_state.keyrefs,
    %% check that a key exists with same name as each keyref
    KeyExist =
	fun({keyref,Name,Refer},S_in) ->
		case load_key(Refer,S_in) of
		    Key=#id_constraint{} ->
			check_keyref_cardinality(Name,
						 load_keyref(Name,S_in),
						 Key,S_in);
%			S_in;
		    _ ->
			acc_errs(S_in,{[],?MODULE,
				       {keyref_missed_matching_key,Refer}})
		end;
	   (Other,S_in) ->
		acc_errs(S_in,{[],?MODULE,
			       {keyref_unexpected_object,Other}})
	end,
    foldl(KeyExist, S, KeyRefs).
check_keyref_cardinality(_,KR=#id_constraint{category=keyref,fields=KeyRefFs},
			 K=#id_constraint{fields=KeyFs},S) ->
    case length(KeyRefFs) == length(KeyFs) of
	true ->
	    S;
	_ ->
	    acc_errs(S,{[],?MODULE,
			{cardinality_of_fields_not_equal,KR,K}})
    end;
check_keyref_cardinality(Name,_,_,S) ->
    acc_errs(S,{[],?MODULE,{could_not_load_keyref,Name}}).
    
check_references(S) when is_record(S,xsd_state) ->    
    check_references(S#xsd_state.unchecked_references,S).
check_references([],S) ->
    S;
check_references([H|T],S) ->
    check_references(T,check_reference(H,S)).
check_reference(Ref={attribute,_},S) ->
    case load_object(Ref,S) of
	{#schema_attribute{},S2} ->
	    S2;
	_ ->
	    acc_errs(S,{[],?MODULE,{reference_undeclared,attribute,Ref}})
    end;
check_reference(Ref={element,_},S) ->
    case load_object(Ref,S) of
	{#schema_element{},S2} ->
	    S2;
	_ ->
	    acc_errs(S,{[],?MODULE,{reference_undeclared,element,Ref}})
    end;
check_reference(Ref={attributeGroup,_},S) ->
    case load_object(Ref,S) of
	{#schema_attribute_group{},S2} ->
	    S2;
	_ ->
	    acc_errs(S,{[],?MODULE,{reference_undeclared,attributeGroup,Ref}})
    end;
check_reference(Ref={group,_},S) ->
    case load_object(Ref,S) of
	{#schema_group{},S2} -> S2;
	_ -> acc_errs(S,{[],?MODULE,{reference_undeclared,group,Ref}})
    end;
check_reference(Ref={simpleType,_},S) ->
    case load_object(Ref,S) of
	{#schema_simple_type{},S2} -> S2;
	_ -> acc_errs(S,{[],?MODULE,{reference_undeclared,simpleType,Ref}})
    end;
check_reference(Ref={complexType,_},S) ->
    case load_object(Ref,S) of
	{#schema_complex_type{},S2} -> S2;
	_ -> acc_errs(S,{[],?MODULE,{reference_undeclared,complexType,Ref}})
    end;
check_reference({simple_or_complex_Type,Ref},S=#xsd_state{errors=Errs}) ->
    %% complex or simple type
    case check_reference({complexType,Ref},S) of
	S2=#xsd_state{errors=Errs} -> S2;
	_ -> check_reference({simpleType,Ref},S)
    end;
check_reference(Ref,S) ->
    acc_errs(S,{[],?MODULE,{internal_error,unknown_reference,Ref}}).
    
%% Substitution groups should be checked for cirkular references
%% (invalid), that reference structure and type structure are
%% concistent.
check_substitutionGroups([],S) ->
    S;
check_substitutionGroups(SGs,S) ->
    S2  = check_substGr_acyclic(SGs,S),
    S3 = check_substGr_type_structure(SGs,S2),
    save_substitutionGroup(SGs,S3).
check_substGr_acyclic(SGs,S) ->
    Set = sofs:family(SGs),
    case catch sofs:family_to_digraph(Set, [acyclic]) of
	{'EXIT',{cyclic,_}} ->
	    acc_errs(S,{[],?MODULE,{cyclic_substitutionGroup,SGs}});
	DG ->
	    digraph:delete(DG),
	    S
    end.
check_substGr_type_structure([SG|SGs],S) ->
    check_substGr_type_structure(SGs,check_substGr_type_structure2(SG,S));
check_substGr_type_structure([],S) ->
    S.
check_substGr_type_structure2({Head,SGMembers},S) ->
    TypeCheck =
	fun(SG,S_in) ->
		case catch cmp_substGr_types(Head,SG,S_in) of
		    {'EXIT',_} -> 
			acc_errs(S_in,{[],?MODULE,
				       {substitutionGroup_error,Head,SG}});
		    S_out -> S_out
		end
	end,
    foldl(TypeCheck,S,SGMembers).
cmp_substGr_types(Head,SG,S) ->
    {HeadElement,S2} = load_object({element,Head},S),
    {MemberElement,S3} = load_object({element,SG},S2),
    case catch derived_or_equal(MemberElement#schema_element.type,
				HeadElement#schema_element.type,
				[],S3) of
	S4=#xsd_state{} ->
	    S4;
	_ ->
	    acc_errs(S3,{[],?MODULE,{internal_error,derived_or_equal,
				     MemberElement#schema_element.type,
				     HeadElement#schema_element.type}})
    end.
check_cyclic_defs(S=#xsd_state{circularity_disallowed=CA}) ->
    Set = sofs:relation_to_family(sofs:relation(CA)),
    case catch sofs:family_to_digraph(Set, [acyclic]) of
	{'EXIT',{cyclic,_}} ->
	    acc_errs(S,{[],?MODULE,{cyclic_definition,CA}});
	DG ->
	    digraph:delete(DG),
	    S
    end.



derived_or_equal(Type,Type,_Block,S) ->
    S;
derived_or_equal([MemberTypeRef],[HeadTypeRef],Block,S) ->
    %% HeadType has to be a 
    {HeadType,_} = resolve(HeadTypeRef,S),
    {MemberType,_} = resolve(MemberTypeRef,S),
    derived_or_equal_types(MemberType,HeadType,schema,Block,S).
derived_or_equal_types(MemT,{anyType,_},Env,Block,S) ->
    case MemT of
	#schema_simple_type{content=Cntnt} ->	    
	    is_derivation_blocked(Env,Block,Cntnt,S);
	#schema_complex_type{content=Cntnt} ->
	    is_derivation_blocked(Env,Block,Cntnt,S);
	_ -> S
    end;
derived_or_equal_types(MemT=#schema_simple_type{name=Mem,base_type=MemBase},
		       #schema_simple_type{name=Head},Env,Block,S)
  when Mem==Head;MemBase==Head ->
    is_derivation_blocked(Env,Block,MemT#schema_simple_type.content,S);
derived_or_equal_types({simpleType,Name},
		       {simpleType,Name},_Env,_Block,S) ->
    S;
derived_or_equal_types(#schema_simple_type{base_type=Name,content=Content},
		       {simpleType,Name},Env,Block,S) ->
    is_derivation_blocked(Env,Block,Content,S);
derived_or_equal_types(#schema_simple_type{content=[{LoU,[Content]}]},
		       SimpleType,Env,Block,S) when LoU==list;LoU==union ->
    {NewMemType,S2}=resolve(Content,S),
    derived_or_equal_types(NewMemType,SimpleType,Env,Block,S2);
derived_or_equal_types(MemT=#schema_complex_type{name=Mem,base_type=MemBase},
		       #schema_complex_type{name=Head},Env,Block,S)
  when Mem==Head;MemBase==Head ->
    is_derivation_blocked(Env,Block,MemT#schema_complex_type.content,S);
derived_or_equal_types(MemT,HeadT,_Env,_Block,S) ->
    acc_errs(S,{[],?MODULE,{type_of_element_not_derived,MemT,HeadT}}).
    
is_derivation_blocked(schema,_,_,S) ->
    S;
is_derivation_blocked(instance,['#all'],Derivation,S) ->
    acc_errs(S,{derivation_blocked,'#all',Derivation});
is_derivation_blocked(instance,[],_,S) ->
    S;
is_derivation_blocked(instance,Block,C=[{Derivation,_}],S) ->
    case member(Derivation,Block) of
	true ->
	    acc_errs(S,{[],?MODULE,{derivation_blocked,Derivation,C}});
	_ ->
	    S
    end;
is_derivation_blocked(instance,_Block,_,S) ->
    S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_attribute(QName,Value) ->
    {Name,_Scope,NSName} = QName,
    #xmlAttribute{name=Name,namespace={Name,NSName},value=Value}.

%% mk_name(L), L must be a list in reversed order
mk_name(L) ->
    mk_name(L,[]).
mk_name([],_Acc) ->
    [];
mk_name([H],[]) ->
    H;
mk_name([H],Acc) ->
    list_to_atom(lists:concat([H,'_'|Acc]));
mk_name([H|T],[]) ->
    mk_name(T,[H]);
mk_name([H1|T],Acc) ->
    mk_name(T,[H1,'_'|Acc]).

cmp_name({LName,Scope,NS},{LName,Scope,NS},_S) ->
    true;
%% substitutionGroup allows different names
cmp_name(XMLName={_,Scope,NS},CMName={_,Scope,NS},S) ->
    {El,_S2} = load_object({element,XMLName},S),
    cmp_SG_name(El,CMName,S);
cmp_name(_,_,_) ->
    false.

cmp_SG_name(#schema_element{substitutionGroup=Name},Name,_S) ->
    true;
cmp_SG_name(#schema_element{substitutionGroup=SGName},CMName,S) ->
    cmp_name(SGName,CMName,S);
cmp_SG_name(_,_,_) ->
    false.

%% Namespace: [{not,NS} | NS]
%% 
cmp_any_namespace({_,_,EIINS},Namespace,_S) ->
    case member(EIINS,Namespace) of
	true ->
	    true;
	_ -> 
	    case keysearch(EIINS,2,Namespace) of
		{value,{'not',EIINS}} ->
		    false;
		_ ->
		    true
	    end
    end.

at_least_one({_Min,Max}) when Max > 0 ->
    true;
at_least_one(_) ->
    false.

is_optional({element,{_,{0,_}}},_S) ->
    true;
is_optional({any,{_,{0,_},_}},_S) ->
    true;
is_optional({MG,{_CM,{0,_}}},_S) 
  when MG =:= all; MG =:= sequence; MG =:= choice  ->
    true;
is_optional({MG,{CM,_Occ}},S) 
  when MG =:= all; MG =:= sequence; MG =:= choice  ->
    case member(false,[is_optional(Y,S)||Y<-CM]) of
	true ->
	    false;
	_ -> true
    end;
is_optional({group,{_,{0,_}}},_S) ->
    true;
is_optional(G={group,_},S) ->
    case resolve(G,S) of
	{#schema_group{content=[]},_} ->
	    true;
	{#schema_group{content=[CM]},_} ->
	    is_optional(CM,S)
    end;
is_optional(_,_) ->
    false.
    
    

acc_errs(S=#xsd_state{errors=Errs},ErrMsg) ->
    S#xsd_state{errors=[ErrMsg|Errs]}.

%% invoked with an element/XML-node and a name of the 
error_path([H|_T],Top) when H==#xmlElement{};H==#xmlText{} ->
    error_path(H,Top);
error_path([_H|T],Top) ->
    error_path(T,Top);
error_path(#xmlElement{parents=Ps,pos=Pos},Top) ->
    error_path(Ps,Pos,Top);
error_path(#xmlAttribute{parents=Ps,pos=Pos},Top) ->
    error_path(Ps,Pos,Top);
error_path(#xmlText{parents=Ps,pos=Pos},Top) ->
    error_path(Ps,Pos,Top);
error_path(_,_) ->
    [].
error_path([],Pos,Top) when is_integer(Pos) ->
    mk_xpath_path([{Top,Pos}]);
error_path([],_,Top) ->
    Top;
error_path(Nodes,_,_) ->
    mk_xpath_path(Nodes).

mk_xpath_path(Nodes) ->
    Slash =
	fun([H1,H2|T],Fun,Acc) -> Fun([H2|T],Fun,["/",H1|Acc]); 
	   ([H1],_,Acc) -> [H1|Acc];
	   ([],_,Acc) -> Acc
	end,
    flatten(Slash([lists:concat([A,"[",B,"]"])||{A,B}<-Nodes],Slash,[])).

resolve(XSDType,InstanceEl,S) ->
    explicit_type(XSDType,InstanceEl,S).

resolve([H],S) ->
    resolve(H,S);
resolve(Any={any,_},S) ->
    {Any,S};
resolve(Any={anyAttribute,_},S) ->
    {Any,S};
resolve(Any={anyType,_},S) ->
    {Any,S};
resolve(Seq={sequence,_},S) ->
    {Seq,S};
resolve(Choice={choice,_},S) ->
    {Choice,S};
resolve({simple_or_complex_Type,QN},S) ->
%%    case load_object({simpleType,QN},S) of
    case resolve({simpleType,QN},S) of
	Res={#schema_simple_type{},_S1} ->
	    Res;
	{[],_S} ->
	    case load_object({complexType,QN},S) of
		{[],_} ->
		    ?debug("could not load object ~p~n",
			   [{simple_or_complex_Type,QN}]),
		    {[],S};
		T ->
		    T
	    end;
	T ->
	    T
    end;
resolve({complexType,{anyType,_,_}},S) ->
    {{anyType,[]},S};
resolve({simpleType,{anyType,_,_}},S) ->
    {{anyType,[]},S};
resolve(ST={simpleType,NameNS={_,_,_}},S) ->
    case load_object(ST,S) of
	{[],_S} -> case is_builtin_simple_type(NameNS) of
		  true ->
		      {ST,S};
		  _ ->
		      {[],S}
	      end;
	Obj ->
	    %resolve(Obj,S)
	    Obj
    end;
resolve({substitutionGroup,QName},S) ->
    %% This shall resolve to the type of the element QName
    case load_object({element,QName},S) of
	Ret = {[],_S} -> Ret;
	{#schema_element{type=[Type]},S2} ->
	    case Type of
		{simple_or_complex_Type,_} ->
		    resolve(Type,S2);
		_ ->
		    {Type,S2}
	    end;
	{#schema_element{type=Type},S2} ->
	    {Type,S2}
    end;
resolve({extension,{BaseType,CM}},S) ->
    case is_builtin_simple_type(BaseType) of
	true ->
	    {{simpleType,BaseType},S};
	_ ->
	    case resolve({simple_or_complex_Type,BaseType},S) of
		{ST = #schema_simple_type{},_} ->
		    {ST,S}; %% any attributes in CM are already
                            %% propagated to the outer complex type.
		{CT = #schema_complex_type{content=C},_} ->
		    {NewC,S2} = extend_type(C,CM,S),
		    {CT#schema_complex_type{content=NewC},S2};
		T -> T
	    end
    end;
resolve({restriction,{BaseType,CM}},S) ->
    case is_builtin_simple_type(BaseType) of
	true ->
	    {{simpleType,BaseType},S};
	_ ->
	    case resolve({simple_or_complex_Type,BaseType},S) of
		{ST = #schema_simple_type{content=C},_} ->
		    {NewContent,S2} = restrict_simple_type(C,CM,BaseType,S),
		    {ST#schema_simple_type{content=NewContent},S2};
		%% the outer complex type.
		{CT = #schema_complex_type{content=C},_} ->
		    {NewContent,S2} = restrict_type(C,CM,BaseType,S),
		    {CT#schema_complex_type{content=NewContent},S2};
		T -> T
	    end
    end;
resolve(optional_text,S) ->
    {optional_text,S};
resolve(E,S) ->
    ?debug("resolve(~p, S)~n",[E]),
    load_object(E,S).

%% explicit_type checks whether the instance element is of an explicit
%% type pointed out by xsi:type. A type refernced by xsi:type must be
%% the same as, or derived from the instance element's type. Concluded
%% from 3.4.6 section "Schema Component Constraint: Type Derivation OK
%% (Complex)".
explicit_type(XSDType,InstanceEl=#xmlElement{namespace=NS,attributes=Atts},S) ->
    case get_instance_type(NS,Atts) of
	false ->
	    resolve(XSDType,S);
	{ok,Name} ->
	    %% Create a {name,scope,namespace}, what is scope?
	    %% assume scope always is at top for the referenced type.
	    QName = mk_EII_QName(Name,InstanceEl,S#xsd_state{scope=[]}),
	    %% The type referenced by "xsi:type" attribute must be a
	    %% legal substitution for InstanceEl: "xsi:type" is the
	    %% same as or a derivation from InstanceEl's type.

	    {XsiType,S2} = resolve({simple_or_complex_Type,QName},S),
	    {_Blocks,S3} = legal_substitution(InstanceEl,XsiType,S2),
%% 	    {ResXSDType,S4} = resolve(XSDType,S3),
	    {XsiType,S3}
%% 	    merge_derived_types(ResXSDType,XsiType,Blocks,xsitype,S4)
    end.

get_instance_type(#xmlNamespace{nodes=Nodes},Atts) ->
    case keyNsearch(?XSD_INSTANCE_NAMESPACE,2,Nodes,[]) of
	{Prefix,_} ->
	    TypeAtt = list_to_atom(Prefix++":type"),
	    case keyNsearch(TypeAtt,#xmlAttribute.name,Atts,[]) of
		#xmlAttribute{value=Value} ->
		    {ok,Value};
		_ -> false
	    end;
	_ ->
	    false
    end.

merge_derived_types(Type1,Type2,Mode,S) ->
    merge_derived_types(Type1,Type2,[],Mode,S).
merge_derived_types(Type,Type,_Blocks,_Mode,S) ->
    {Type,S};
merge_derived_types(XSDType,InstType,Blocks,Mode,S) ->
    case catch merge_derived_types2(XSDType,InstType,Blocks,Mode,S) of
	{'EXIT',Reason} ->
	    {InstType,acc_errs(S,{[],?MODULE,{internal_error,merge_derived_types,Reason}})};
	{error,S2} ->
	    {InstType,S2};
	{MergedType,S2} ->
	    _ = save_merged_type(MergedType,S2),
	    {MergedType,S2}
    end.

merge_derived_types2(XSDType=#schema_complex_type{},
		    InstType=#schema_complex_type{},Blocks,Mode,S) ->
    %% InstType is the type of the instance element that may reference
    %% a type that is an extension/restriction of the XSDType. 
    %% Alternatively XSDType is the base type and InstType the derived
    %% type or XSDType is the original type that is redefined into
    %% InstType.
    %% 
    %% complexType can turn into: 
    %%        simpleContent | complexContent
    %% simpleContent -> restriction
    %% complexContent -> restriction | extension
    %% of course also one of:
    %% ((group | all | choice | sequence)?,
    %%	((attribute | attributeGroup)*,anyAttribute?))))
    %% but then it shouldn't be any difference between XSDType
    %% and InstType
   case InstType#schema_complex_type.content of
       [{extension,{BaseTypeName,CM}}] ->
	   {ExtendedAtts,S2} = 
	       extend_attributes(XSDType#schema_complex_type.attributes,
				 InstType#schema_complex_type.attributes,
				 BaseTypeName,CM,Mode,
				 allowed_derivation(extension,Blocks,S)),
	   case compare_base_types(BaseTypeName,XSDType,S2) of
	       ok ->
		   {NewContent,S3} =
		       extend_type(XSDType#schema_complex_type.content,CM,S2),
		   {InstType#schema_complex_type{attributes=ExtendedAtts,
						 content=NewContent},S3};
	       Err ->
		   {error,acc_errs(S2,Err)}
	   end;
       [{restriction,{BaseTypeName,CM}}] ->
	   {RestrictedAtts,S2} =
	       restrict_attributes(XSDType#schema_complex_type.attributes,
				   InstType#schema_complex_type.attributes,
				   allowed_derivation(restriction,Blocks,S)),
	   case compare_base_types(BaseTypeName,XSDType,S2) of
	       ok ->
		   {NewContent,S3}=
		       case InstType#schema_complex_type.complexity of
			   simple ->
			       restrict_simple_type(XSDType#schema_complex_type.content,CM,BaseTypeName,S2);
			   _ ->
			       restrict_type(XSDType#schema_complex_type.content,CM,BaseTypeName,S2)
		       end,
		   {InstType#schema_complex_type{attributes=RestrictedAtts,
						 content=NewContent},S3};
	       Err ->
		   {error,acc_errs(S,Err)}
	   end;
       Other ->
	   {error,acc_errs(S,{[],?MODULE,{unexpected_type,Other}})}
   end;
merge_derived_types2(XSDType=#schema_simple_type{},
		    InstType=#schema_simple_type{},Blocks,_Mode,S) ->
    case InstType#schema_simple_type.content of
       [{restriction,{BaseTypeName,CM}}] ->
	    case compare_base_types(BaseTypeName,XSDType,S) of
		ok ->
		    
		    {NewContent,S2}=
			restrict_simple_type(XSDType#schema_simple_type.content,CM,
				      BaseTypeName,S),
		    {InstType#schema_simple_type{content=NewContent},
		     allowed_derivation(restriction,Blocks,S2)};
		Err ->
		    {error,allowed_derivation(restriction,Blocks,
					      acc_errs(S,Err))}
	    end;
	Other ->
	    {error,acc_errs(S,{unexpected_type,Other})}
    end;
merge_derived_types2(XSDType=#schema_simple_type{content=XSDContent},
		     InstType=#schema_complex_type{},Blocks,_Mode,S) ->
    %% This is the way to add attributes to a simpleType
    case InstType#schema_complex_type.content of
	[{extension,{BaseTypeName,CM}}] ->
	    case compare_base_types(BaseTypeName,XSDType,S) of
		ok ->
		    {NewContent,S2} = 
			if CM==[] -> {XSDContent,S};
			   true -> extend_type(XSDContent,CM,S)
			end,
		    {InstType#schema_complex_type{content=NewContent},
		     allowed_derivation(extension,Blocks,S2)};
		Err ->
		    {error,allowed_derivation(extension,Blocks,
					      acc_errs(S,Err))}
	    end;
	[{restriction,{BaseTypeName,_CM}}]
	when InstType#schema_complex_type.complexity == simple ->
	    case compare_base_types(BaseTypeName,XSDType,S) of
		ok ->
		    {InstType,
		     allowed_derivation(restriction,Blocks,S)};
		Err ->
		    {error,allowed_derivation(extension,Blocks,
					      acc_errs(S,Err))}
	    end;
	Other ->
	    {error,acc_errs(S,{[],?MODULE,{unexpected_type,Other}})}
    end;
merge_derived_types2(_XSDType={simpleType,BuiltInType},
		     InstType=#schema_complex_type{content=Content},
		     Blocks,_Mode,S) ->
    case Content of
	[{extension,{BuiltInType,CM}}] ->
	    {NewContent,S2} = extend_type([],CM,S),
	    {InstType#schema_complex_type{base_type=BuiltInType,
					  content=NewContent},
	     allowed_derivation(extension,Blocks,S2)};
	[{restriction,{BuiltInType,CM}}] ->
	    {NewContent,S2} = restrict_simple_type([],CM,BuiltInType,S),
	    {InstType#schema_complex_type{base_type=BuiltInType,
					  content=NewContent},
	     allowed_derivation(restriction,Blocks,S2)};
	Other ->
	    {error,acc_errs(S,{[],?MODULE,{unexpected_content,Other,InstType}})}
    end;
merge_derived_types2(_XSDType={anyType,_},InstType,Blocks,_Mode,S) ->
    case type_content(InstType) of
	[{restriction,{_BaseTypeName,CM}}] ->
	    {set_type_content(InstType,CM),
	     allowed_derivation(restriction,Blocks,S)};
	Other ->
	    {error,acc_errs(S,{[],?MODULE,{unexpected_content,Other,InstType}})}
    end;
merge_derived_types2({simpleType,BuiltInType},
		     InstType=#schema_simple_type{content=Content},
		     Blocks,_Mode,S) ->
    case Content of
	[{restriction,{BuiltInType,CM}}] ->
	    {InstType#schema_simple_type{base_type=BuiltInType,
					 content=CM},
	     allowed_derivation(restriction,Blocks,S)};
	Other ->
	    {error,acc_errs(S,{[],?MODULE,{unexpected_content,Other,InstType}})}
    end;    
merge_derived_types2(XSDType,InstType,Blocks,Mode,S) ->
    case {variety_type(XSDType,S),variety_type(InstType,S)} of
	{XSDType,InstType} ->
	    {error,acc_errs(S,{[],?MODULE,{unexpected_type,XSDType,InstType}})};
	{_XSDType2,InstType2} ->
	    case allowed_derivation(substitution,Blocks,S) of
		S ->
		    merge_derived_types2(XSDType,InstType2,Blocks,Mode,S);
		S2 ->
		    {error,S2}
	    end
    end.

variety_type(#schema_simple_type{variety=list,content=[{list,[Type]}]},S) ->
    {VarietyType,_}=resolve(Type,S),
    VarietyType;
variety_type(#schema_simple_type{variety=union,content=[{union,Types}]},S) ->
    [T||{T,_}<-[resolve(VarietyType,S)||VarietyType<-Types]];
variety_type(Type,_S) ->
    Type.

allowed_derivation(_Derivation,_Blocks,S) ->
%%     case {member(Derivation,Blocks),member('#all',Blocks)} of
%% 	{true,_} ->
%% 	    acc_errs(S,{[],?MODULE,{derivation_blocked,Blocks,Derivation}});
%% 	{_,true} ->
%% 	    acc_errs(S,{[],?MODULE,{derivation_blocked,'#all',Derivation}});
%% 	_ ->
%% 	    S
%%     end.
    S.

%% El is the instance element that has the xsi:type attribute with
%% XsiType.
legal_substitution(El=#xmlElement{name=ElName},XsiType,S) ->
    %% See 3.3.6, Substitution Group OK (Transitive)
    %% For ok one of following: 1) same type in El as XsiType, 2)
    %% XsiType is a restriction/extension where El's type is the
    %% base, 3) XsiType is a member in the substitutionGroup of
    %% ElName.
    QName = mk_EII_QName(ElName,El,S),
    {HeadElement,_} = load_object({element,QName},S),

    legal_substitution2(HeadElement,XsiType,S).
legal_substitution2(#schema_element{type=Type,block=Bl},XsiType,S) ->
    {HeadType,_}=resolve(Type,S),
    Block = blocking(Bl,S#xsd_state.blockDefault),
    S2 = derived_or_equal_types(XsiType,HeadType,instance,Block,S),
    {Block,S2}.

compare_base_types(QName,#schema_complex_type{name=QName},_S) ->
    ok;
compare_base_types(QName1,#schema_complex_type{name=QName2},_S) ->
    {[],?MODULE,{names_not_equal,QName1,QName2}};
compare_base_types(QName,#schema_simple_type{name=QName},_S) ->
    ok;
compare_base_types(QName1,#schema_simple_type{name=QName2},_S) ->
    {[],?MODULE,{names_not_equal,QName1,QName2}}.
%%compare_base_types(QName1,Other,_S) ->
%%    {[],?MODULE,{miss_match_base_types,QName1,Other}}.

extend_type(Base,Extension,S) ->
    extend_type(Base,Extension,[],S).
%% Content may be (attribute | attributeGroup)*, anyAttribute? if
%% it is of simpleContent or:
%% (group | all | choice | sequence)?,((attribute | attributeGroup)*,
%%		anyAttribute?) if it is of complexContent
extend_type([],[],Acc,S) ->
    {reverse(Acc),S};
extend_type([BaseCM|BaseRest],Ext=[{SeqCho,{Extension,Occ}}|ExtRest],Acc,S)
  when SeqCho == sequence; SeqCho == choice ->
    case BaseCM of
	{SeqCho,{BC,_Occ}} ->
	    extend_type(BaseRest,ExtRest,[{SeqCho,{BC++Extension,Occ}}|Acc],S);
	G = {group,{_Ref,_Occ}} ->
	    {ResG,S2} = resolve(G,S),
	    case ResG of
	       #schema_group{content=GC} ->
		    case keysearch(SeqCho,1,GC) of
			{value,SCC} ->
			    extend_type([SCC|BaseRest],Ext,Acc,S);
			_ ->
			    S3 = acc_errs(S2,{[],?MODULE,{illegal_content_in_extension,Ext}}),
			    {reverse(Acc),S3}
		    end;
		_ ->
		    S3 = acc_errs(S2,{[],?MODULE,{illegal_content_in_extension,ResG}}),
		    {reverse(Acc),S3}
	    end;
	 _ ->
	    %% BaseCM may be a group that has a sequence
	    extend_type([BaseCM|BaseRest],ExtRest,[{SeqCho,{Extension,Occ}}|Acc],S)
    end;
extend_type(BaseCM,ExtCM,Acc,S) when is_list(BaseCM),is_list(ExtCM) ->
    extend_type([],[],reverse(ExtCM)++reverse(BaseCM)++Acc,S).

restrict_type(Content,CM,BaseTypeName,S)  ->
    restrict_type(Content,CM,BaseTypeName,[],S).
%% Restriction may appear within a 1) simpleType, 2) simpleContent or
%% 3) complexContent construct.
%% The possible content of restriction in different contexts are:
%% 1) (simpleType?, (Any facet)*)
%% 2) (simpleType?, (Any facet)*),((attribute | attributeGroup)*, anyAttribute?)
%% 3) (group | all | choice | sequence)?,
%%	   ((attribute | attributeGroup)*, anyAttribute?)
%% A restriction of a simpleType narrows the possible values of the
%% base type by applying facets.
%% A restriction of a complexType (simpleContent / complexContent) must
%% enumerate all elements, including the preserved ones of the base type.
%% Attributes don't have to be enumerated.
restrict_type([],[],_TypeName,Acc,S) ->
    {reverse(Acc),S};
restrict_type([{restriction,{_Type,CM1}}],[],_TypeName,Acc,S) ->
    {CM1++reverse(Acc),S};
restrict_type([{extension,{_Type,CM1}}],[],_TypeName,Acc,S) ->
    {CM1++reverse(Acc),S};
restrict_type(BaseRest,[ST={simpleType,_Name}|RestrRest],TypeName,Acc,S) ->
    %% context 1 or 2
    restrict_type(BaseRest,RestrRest,TypeName,[ST|Acc],S);
restrict_type([BaseCM|BaseRest],[{SeqCho,{CM,Occ}}|RestrRest],TypeName,Acc,S) 
  when SeqCho == sequence; SeqCho == choice ->
    %% context 3
    case BaseCM of
	{SeqCho,{BCM,_}} ->
	    case check_element_presence(CM,BCM) of
		{error,Reason} ->
		    {reverse(Acc),acc_errs(S,Reason)};
		ok ->
		    restrict_type(BaseRest,RestrRest,TypeName,
				  [{SeqCho,{CM,Occ}}|Acc],S)
	    end;
	Other ->
	    {reverse(Acc),acc_errs(S,{[],?MODULE,{SeqCho,expected,Other,found}})}
    end;
restrict_type(BaseRest,[Facet={F,_Val}|RestrRest],TypeName,Acc,S) ->
    case is_facet(F) of
	true ->
	    restrict_type(BaseRest,RestrRest,TypeName,[Facet|Acc],S);
	_ ->
	    {reverse(Acc),acc_errs(S,{[],?MODULE,{does_not_support,Facet,in_restriction}})}
    end.

restrict_simple_type([{restriction,{_Type,BaseCM}}],RestrCM,_TypeName,S) ->
    restrict_simple_type(BaseCM,RestrCM,_TypeName,S);
restrict_simple_type(CM=[{extension,{_Type,_BaseCM}}],_RestrCM,TypeName,S) ->
    {[],acc_errs(S,{[],?MODULE,{illegal_content_simple_type,CM,TypeName}})};
restrict_simple_type(BaseCM,RestrCM,TypeName,S) ->
    %% all restrictions in base comes first, then check that no one of
    %% the facets in the restriction attempts to redefine a fixed
    %% facet in the base. Add the facets of the restriction.
    {Acc,S2} =
	case BaseCM of
	    [] -> {[],S};
	    _ ->
		restrict_simple_type([],BaseCM,TypeName,S)
	end,
    %% Acc = reverse(BaseCM),
    Fun = fun(X={simpleType,_},{Acc_in,S_in})->
  		  {[X|Acc_in],S_in};
	     (X={LU,_},{Acc_in,S_in}) when LU==list;LU==union ->
		  {[X|Acc_in],S_in};
 	     (X={F,_},{Acc_in,S_in})->
%%    Fun = fun(X={F,_},{Acc_in,S_in})->
		  case is_facet(F) of
		      true ->
			  {[X|Acc_in],S_in};
		      _ ->
			  {Acc_in,acc_errs(S_in,{[],?MODULE,{illegal_in_restriction_of_simpleType,X}})}
		  end;
	     (X,{Acc_in,S_in}) ->
		  {Acc_in,acc_errs(S_in,{[],?MODULE,{illegal_in_restriction_of_simpleType,X}})}
	  end,
    foldl(Fun,{Acc,S2},RestrCM).

check_element_presence([],_BCM) ->
    ok;
check_element_presence([{element,{Name,_}}|CM],BCM) ->
    case check_element_presence2(Name,BCM) of
	{ok,BCM2} ->
	    check_element_presence(CM,BCM2);
	_ ->
	    {error,{[],?MODULE,{element,Name,not_present_in_restriction}}}
    end;
check_element_presence([_C|CM],BCM) ->
    check_element_presence(CM,BCM).

check_element_presence2(Name,BCM) -> 
    check_element_presence2(Name,BCM,[]).
check_element_presence2({LocalName,_,NS},[{element,{{LocalName,_,NS},_}}|BCM],Acc) ->
    {ok,reverse(Acc)++BCM};
check_element_presence2(Name,[E|BCM],Acc) ->
    check_element_presence2(Name,BCM,[E|Acc]);
check_element_presence2(_Name,[],_Acc) ->
    error.

%% A check of the extended attribute should take place here.
%% 
extend_attributes(BaseAtts,[EA={attribute,Name}|ExtAtts],
		  BaseTypeName,CM,Mode,S) ->
    NewAtts=key_replace_or_insert(Name,2,BaseAtts,EA),
    extend_attributes(NewAtts,ExtAtts,BaseTypeName,CM,Mode,S);
%% Extension of wildcards should be handled as described in chapter
%% 3.4.2 and subsection "Complex Type Definition with simple content
%% Schema Component".
extend_attributes(BaseAtts,[LocalWC={anyAttribute,_NS_PC}|ExtAtts],
		  BaseTypeName,CM,deduce,S) ->
    {CompleteWC,S2} = complete_wildcard(LocalWC,CM,S),
    BaseWC = base_wildcard(BaseAtts),
    {NewWC,S4} =
    case BaseWC of
	[] -> {CompleteWC,S2};
	_ ->
	    if CompleteWC==LocalWC -> {BaseWC,S2};
	       true -> 
		    {NS,S3} = attribute_wildcard_union(wc_ns(CompleteWC),
						       wc_ns(BaseWC),S2),
		    PC = wc_pc(CompleteWC),
		    {[{anyAttribute,{NS,PC}}],S3}
	    end
    end,
    NewBaseAtts = keyreplace(anyAttribute,1,BaseAtts,NewWC),
    extend_attributes(NewBaseAtts,ExtAtts,BaseTypeName,CM,deduce,S4);
extend_attributes(Atts,[],_,_,_Mode,S) ->
    {reverse(Atts),S}.
%% A check of the restricted attribute should take place here.
restrict_attributes(BaseAtts,[RA|RAtts],S) ->
%% NewAtts = keyreplace(Name,2,BaseAtts,EA),
    {NewAtts,S2} = restrict_attribute_replace(BaseAtts,RA,S),
    restrict_attributes(NewAtts,RAtts,S2);
restrict_attributes(Atts,[],S) ->
    {reverse(Atts),S}.
restrict_attribute_replace(BaseAtts,EA={attribute,Name},S) ->
    {keyreplace(Name,2,BaseAtts,EA),S};
restrict_attribute_replace(BaseAtts,EA={anyAttribute,{NS,_}},S) ->
    case key1search(anyAttribute,BaseAtts,false) of
	false ->
	    {BaseAtts,acc_errs(S,{invalid_derivation,EA,BaseAtts})};
	{_,{BaseNS,_}} ->
	    S2 = wildcard_subset(BaseNS,NS,S),
	    {keyreplace(anyAttribute,1,BaseAtts,EA),S2}
    end.
%% 3.10.6 Constraints on Wildcard Schema Components
%% Schema Component Constraint: Wildcard Subset
%% bullet 1:
wildcard_subset(['##any'],_NS,S) ->
    S;
%% bullet 2:
wildcard_subset([{'not',NS}],[{'not',NS}],S) ->
    S;
%% bullet 3:
%% if NS has a number of namespaces all of them must be in BaseNS,
%% if BaseNS has {not,Namespaces} neither of Namespaces must be in NS
wildcard_subset(_,[],S) ->
    S;
wildcard_subset(BaseNS,NS,S) when is_list(BaseNS),is_list(NS) ->
    case [X||X<-NS,member(X,BaseNS)] of
	NS ->
	    S;
	_ ->
	    acc_errs(S,{[],?MODULE,{wildcard_namespace,NS,
			not_subset_of_base_namespace,BaseNS}})
    end;
wildcard_subset(BaseNS=[{'not',BNS}],NS,S) when is_list(NS) ->
    case [X||X<-BNS,member(X,NS)] of
	[] ->
	    S;
	_ ->
	    acc_errs(S,{[],?MODULE,{wildcard_namespace,NS,
			not_subset_of_base_namespace,BaseNS}})
    end;
wildcard_subset(BaseNS,NS,S) ->
    acc_errs(S,{[],?MODULE,{wildcard_namespace,NS,
		not_subset_of_base_namespace,BaseNS}}).

base_wildcard(BaseAtts) ->
    key1search(anyAttribute,BaseAtts,[]).
    
complete_wildcard(LocalWC,CM,S) ->
    case keysearch(attributeGroup,1,CM) of
	{value,AttG={_,_Name}} ->
	    case resolve(AttG,S) of
		{#schema_attribute_group{content=Atts},_S} ->
		    case keysearch(anyAttribute,1,Atts) of
			{value,AA} ->
			    {PC,S2} =
				attribute_wildcard_intersection(wc_ns(LocalWC),
								wc_ns(AA),S),
			    {{anyAttribute,{wc_pc(LocalWC),PC}},S2};
			_ -> {LocalWC,S}
		    end;
		_ -> {LocalWC,S}
	    end;
	_ -> {LocalWC,S}
    end.
					   
wc_ns({anyAttribute,{NS,_}})->
    NS;
wc_ns(_) ->
    [].
wc_pc({anyAttribute,{_,PC}})->
    PC;
wc_pc(_) ->
    strict.

%% Union of wildcard namespace:
%% 3.10.6 Constraints on Wildcard Schema Components
%% Schema Component Constraint: Attribute Wildcard Union
%% bullet 1
attribute_wildcard_union(NS,NS,S) ->
    {NS,S};
%% bullet 2
attribute_wildcard_union(NS1,NS2,S) when NS1==['##any'];NS2==['##any'] ->
    {['##any'],S};
attribute_wildcard_union(NS1,NS2,S) ->
    case {keysearch('not',1,NS1),keysearch('not',1,NS2)} of
	{false,false} -> %% bullet 3
	    {NS1 ++ [X||X<-NS2,member(X,NS1)==false],S};
	{{value,{_,Set1}},{value,{_,Set2}}} -> %% bullet 4 or 1
	    case {lists:sort(Set1),lists:sort(Set2)} of
		{L,L} ->    {[{'not',L}],S};
		_ ->	    {[{'not',[absent]}],S}
	    end;
	_ -> %% either is a {not,NS}
	    case toggle_ns(NS1,NS2) of
		{_O1=[absent],NS3} -> %% bullet 6
		    case member(absent,NS3)of
			true -> {['##any'],S};
			_ ->    {[{'not',[absent]}],S}
		    end;
		{O1=[O1Name],NS4} -> %% bullet 5
		    case member(O1Name,NS4) of
			true ->
			    case member(absent,NS4) of
				true -> {['##any'],S}; %% 5.1
			        _ ->    {[{'not',[absent]}],S} %% 5.2
			    end;
			_ ->
			    case member(absent,NS4) of
				true ->
				    %% not expressible 5.3
				    Err = {[],?MODULE,{wildcard_namespace_union_not_expressible,NS1,NS2}},
				    {[],acc_errs(S,Err)};
				_ -> {[{'not',O1}],S} %% 5.4
			    end
		    end
	    end
    end.

%% Schema Component Constraint: Attribute Wildcard Intersection
%% bullet 1
attribute_wildcard_intersection(O1,O1,S) -> {O1,S};
%% bullet 2
attribute_wildcard_intersection(['##any'],O2,S) -> {O2,S};
attribute_wildcard_intersection(O1,['##any'],S) -> {O1,S};
%% bullet 6
attribute_wildcard_intersection([{'not',[absent]}],O2=[{'not',_}],S) -> {O2,S};
attribute_wildcard_intersection(O1=[{'not',_}],[{'not',[absent]}],S) -> {O1,S};
%% bullet 5
attribute_wildcard_intersection([{'not',NS1}],[{'not',NS2}],S) -> 
    case [X||X<-NS1,member(X,NS2)] of
	[] -> {[],acc_errs(S,{[],?MODULE,{wildcard_namespace_intersection_not_expressible,NS1,NS2}})};
	NS3 -> {[{'not',NS3}],S}
    end;
%% bullet 3
attribute_wildcard_intersection([{'not',NS}],O2,S) ->
    {lists:delete(absent,[X||X<-O2,member(X,NS)==false]),S};
attribute_wildcard_intersection(O1,[{'not',NS}],S) ->
    {lists:delete(absent,[X||X<-O1,member(X,NS)==false]),S};
%% bullet 4
attribute_wildcard_intersection(O1,O2,S) ->
    case [X||X<-O1,member(X,O2)] of
	[] ->
	    {[absent],S};
	L ->{L,S}
    end.

toggle_ns(NS1,NS2=[{'not',_}]) ->
    {NS2,NS1};
toggle_ns(NS1,NS2) ->
    {NS1,NS2}.

    
deduce_derived_types([DT|DTs],S) ->
    deduce_derived_types(DTs,deduce_derived_type(DT,S,[]));
deduce_derived_types([],S) ->
    S.

%% deduce_derived_type
deduce_derived_type(DT={_Kind,TName},S,RefChain) ->
    %% check circular references
    case keymember(TName,2,RefChain) of
	true -> 
	    acc_errs(S,{[],?MODULE,{circular_reference_of_type,TName}});
	_ ->
	    deduce_derived_type2(DT,S,[DT|RefChain])
    end.
deduce_derived_type2(DT,S,RefChain) ->
    {DerivedType,_} = resolve(DT,S),
    case is_unmerged_type(DerivedType) of
	true ->
	    BaseTypeRef = get_base_type(DerivedType),
	    {BaseType,_} = resolve({simple_or_complex_Type,BaseTypeRef},S),
	    BaseTypeKind =
		fun(#schema_complex_type{}) -> complexType;
		   (_) -> simpleType
		end (BaseType),
	    case is_unmerged_type(BaseType) of
		true ->
		    %% both derived and not deduced
		    S2 = deduce_derived_type({BaseTypeKind,BaseTypeRef},S,RefChain),
		    case S2#xsd_state.errors==S#xsd_state.errors of
			true -> deduce_derived_type2(DT,S2,RefChain);
			_ -> S2
		    end;
		_ ->
		    {_,S2} = merge_derived_types(BaseType,DerivedType,deduce,S),
		    S2
	    end;
	_ ->
	    S
    end.
is_unmerged_type(Type) ->
    case type_content(Type) of
	[{restriction,_}] -> true;
	[{extension,_}] -> true;
	_ -> false
    end.
type_content(#schema_simple_type{content=C}) ->
    C;
type_content(#schema_complex_type{content=C}) ->
    C;
type_content(_) ->
    [].

set_type_content(Type=#schema_simple_type{},CM) ->
    Type#schema_simple_type{content=CM};
set_type_content(Type=#schema_complex_type{},CM) ->
    Type#schema_complex_type{content=CM}.

get_base_type(#schema_simple_type{base_type=BT}) ->
    BT;
get_base_type(#schema_complex_type{base_type=BT}) ->
    BT.

in_scope({Local,_Scope,_NS},S) ->
    in_scope(Local,S);
in_scope(Name,S=#xsd_state{scope=Scope}) when is_atom(Name) ->
    S#xsd_state{scope=[Name|Scope]};
in_scope(Name,S=#xsd_state{scope=Scope})  when is_list(Name) ->
    S#xsd_state{scope=[atom_if_shortasciilist(Name)|Scope]}.

out_scope({Local,_,_},S) ->
    out_scope(atom_if_shortasciilist(Local),S);
out_scope(Name,S=#xsd_state{scope=[Name|Rest]}) ->
    S#xsd_state{scope=Rest};
out_scope(_Name,S) ->
    S.
    
name_scope({'_xmerl_no_name_',Scope,_NS},S) ->
    S#xsd_state{scope=Scope};
name_scope({Name,Scope,_NS},S) ->
    S#xsd_state{scope=[Name|Scope]}.

reset_scope(S) ->
    S#xsd_state{scope=[]}.

set_scope(Scope,S) when is_list(Scope) ->
     S#xsd_state{scope=Scope};
set_scope(_,S) ->
    S.

is_global_env([_Env]) ->
    true;
is_global_env(_) ->
    false.

kind(#xmlElement{name=Name},S) ->
    LocalName=local_name(Name),
    is_a(LocalName,S).

kind(#xmlElement{name=Name}) ->
    LocalName=local_name(Name),
    element(1,is_a(LocalName,dummy)).

is_a(element,S) -> {element,S};
is_a(annotation,S) -> {annotation,S};
is_a(simpleType,S) -> {simpleType,S};
is_a(complexType,S) -> {complexType,S};
is_a(simpleContent,S) -> {simpleContent,S};
is_a(complexContent,S) -> {complexContent,S};
is_a(include,S) -> {include,S};
is_a(import,S) -> {import,S};
is_a(redefine,S) -> {redefine,S};
is_a(unique,S) -> {unique,S};
is_a(key,S) -> {key,S};
is_a(keyref,S) -> {keyref,S};
is_a(attribute,S) -> {attribute,S};
is_a(attributeGroup,S) -> {attributeGroup,S};
is_a(group,S) -> {group,S};
is_a(all,S) -> {all,S};
is_a(sequence,S) -> {sequence,S};
is_a(choice,S) -> {choice,S};
is_a(any,S) -> {any,S};
is_a(anyAttribute,S) -> {anyAttribute,S};
is_a(selector,S) -> {selector,S};
is_a(field,S) -> {field,S};
is_a(notation,S) -> {notation,S};
is_a(appinfo,S) -> {appinfo,S};
is_a(documentation,S) -> {documentation,S};
is_a(restriction,S) -> {restriction,S};
is_a(extension,S) -> {extension,S};
is_a(list,S) -> {list,S};
is_a(union,S) -> {union,S};
is_a(schema,S) -> {schema,S};
is_a(minExclusive,S) -> {minExclusive,S};
is_a(minInclusive,S) -> {minInclusive,S};
is_a(maxExclusive,S) -> {maxExclusive,S};
is_a(maxInclusive,S) -> {maxInclusive,S};
is_a(totalDigits,S) -> {totalDigits,S};
is_a(fractionDigits,S) -> {fractionDigits,S};
is_a(length,S) -> {length,S};
is_a(minLength,S) -> {minLength,S};
is_a(maxLength,S) -> {maxLength,S};
is_a(enumeration,S) -> {enumeration,S};
is_a(whiteSpace,S) -> {whiteSpace,S};
is_a(pattern,S) -> {pattern,S};
is_a(Name,S) when is_record(S,xsd_state) ->
    {Name,acc_errs(S,{[],?MODULE,{unknown_content,Name}})};
is_a(Name,_) ->
    exit({error,{internal_error,not_implemented,Name}}).




%% namespace/2 -> [token()]
%% token() -> {not,namespace_name()} | namespace_name()
%% ((##any | ##other) | List of (anyURI | (##targetNamespace | ##local)) )  : ##any
%% The result will be:
%% NSList ::= ['##any'] | [{'not',[TNS]}] | NSURIs
%% TNS    ::= URI | absent
%% NSURIs ::= (URI | absent) +
%% URI    ::= atomified URI-string
wildcard_namespace(E,S) ->
    AttVal = get_attribute_value(namespace,E,"##any"),
    ListOfVals = namestring2namelist(AttVal),
    Pred = fun('##other') ->
		   case S#xsd_state.targetNamespace of
		       undefined -> {'not',[absent]};
		       TN -> {'not',TN}
		   end;
	      ('##targetNamespace') ->
		   case S#xsd_state.targetNamespace of
		       undefined -> absent;
		       TN -> TN
		   end;
	      ('##local') -> absent;%%'##local'; %% any well-formed xml that
                                          %% is not qualified.
	      (X) -> X
	   end,
     [X||X <- map(Pred,ListOfVals),X=/=[]].

processor_contents(Any) ->  
    case get_attribute_value(processContents,Any,strict) of
	V when is_list(V) -> list_to_atom(V);
	A -> A
    end.

base_type(E) ->
    get_attribute_value(base,E,[]).
base_type_type(Env) ->
    case member(simpleType,Env) of
	true -> simpleType;
	_ -> simple_or_complex_Type
    end.

attribute_ref(A) ->
    get_attribute_value(ref,A,[]).

particle_ref(El) ->
    get_attribute_value(ref,El,[]).

attributeGroup_ref(El) ->
    get_attribute_value(ref,El,[]).

get_value(El) ->
    get_attribute_value(value,El,undefined).

get_attribute_value(Key,#xmlElement{attributes=Atts},Default) ->
    case keyNsearch(Key,#xmlAttribute.name,Atts,Default) of
	#xmlAttribute{value=V} ->
	    V;
	_ -> Default
    end.

%% qualify_NCName/2 returns a qualified name, QName, that has
%% information of the name attribute and namespace of the XSD object.
%% The object E has a name attribute with a NCName. The Namespace 
%% part of the QName is from the targetNamespace attribute of the
%% schema or the empty list.
qualify_NCName(E=#xmlElement{},S) ->
    case get_local_name(E) of
	[] -> no_name;
	LocalName ->
	    Namespace =
		case S#xsd_state.targetNamespace of
		    undefined ->
			[]; %%?XSD_NAMESPACE;
		    TNS ->
			TNS
		end,
	    {atom_if_shortasciilist(LocalName),S#xsd_state.scope,Namespace}
    end.


get_local_name(#xmlElement{attributes=Atts}) ->
    case keyNsearch(name,#xmlAttribute.name,Atts,[]) of
	#xmlAttribute{value=V} ->
	    V;
	Default -> Default
    end.

local_name(Name) when is_atom(Name) ->
    local_name(atom_to_list(Name));
local_name(Name) when is_list(Name) ->
    case splitwith(fun($:) -> false;(_)->true end,Name) of
	{_,":"++LocalName} -> list_to_atom(LocalName);
	_ ->
	    list_to_atom(Name)
    end.

%% transforms "a B c" to [a,'B',c]
namestring2namelist(Str) ->
    split_by_whitespace(Str,[]).
split_by_whitespace(Str,Acc) when is_list(Str),length(Str) > 0 ->
    F = fun($ ) -> 
		false;
	   (_) ->
		true
	end,
    {Str1,Rest} = splitwith(F,Str),
    split_by_whitespace(string:strip(Rest),[list_to_atom(Str1)|Acc]);
split_by_whitespace(_,Acc) ->
    reverse(Acc).

%% get_QName(Name,S) where Name is a QName in string format, or where
%% a QName is expected according to schema specification. If the name
%% is unqualified it is qualified with the targetNamespace of the schema
%% or with the empty list.
get_QName(Name,NS,S) when is_atom(Name) ->
    get_QName(atom_to_list(Name),NS,S);
get_QName(Name,NS,#xsd_state{scope=Scope}) ->
    qualified_name(Name,NS,NS#xmlNamespace.default,Scope).

qualified_name(Name,NS,Default,Scope) ->
    case splitwith(fun($:) -> false;(_)->true end,Name) of
	{GlobalName,":"++LocalName} -> {atom_if_shortasciilist(LocalName),Scope,
					namespace(GlobalName,NS,Default)};
	_ ->
	    {atom_if_shortasciilist(Name),Scope,Default}
    end.

atom_if_shortasciilist(N) when is_list(N) ->
    case catch list_to_atom(N) of
	{'EXIT',_Reason} ->
	    %% Reason may be system_limit if N is very long, it may be
	    %% badarg ifN is a list of UTF characters.
	    N;
	AN -> AN
    end;
atom_if_shortasciilist(N) ->
    N.

namespace("xml",_,_) -> 'http://www.w3.org/XML/1998/namespace';
namespace(Prefix,NS,Default) ->
    case key1search(Prefix,NS#xmlNamespace.nodes,Default) of
	{Prefix,Namespace} ->
	    Namespace;
	Namespace -> Namespace
    end.


%% mk_EII_QName/2
%% makes a name with qualified info out of an Element Information Item
%% A) If name is qualified get namespace matching prefix.
%% B) If not qualified search parents for a namespace:
%% 1) use default namespace if defined, else.
%% 2) if a parent is qualified use that namespace or
%% 3) no namespace is applied
mk_EII_QName(Name,#xmlElement{name=Me,namespace=NS,parents=P},S) 
  when is_list(Name) ->
    mk_EII_QName(list_to_atom(Name),
		 #xmlElement{name=Me,namespace=NS,parents=P},S);
mk_EII_QName(Name,#xmlElement{name=Me,namespace=NS,parents=P},S) ->
    Scope = S#xsd_state.scope,
    NameStr = atom_to_list(Name),
    case string:tokens(NameStr,":") of
	["xmlns",PrefixDef] -> %% special case
	    {'xmlns',Scope,namespace(PrefixDef,NS,[])};
	[Prefix,LocalName] -> %% A
	    {list_to_atom(LocalName),Scope,namespace(Prefix,NS,[])};
	[_LocalName] -> %% B
	    {Name,Scope,mk_EII_namespace([{Me,0}|P],NS,S)}
    end.
mk_EII_namespace([],#xmlNamespace{default=DefaultNS},_S) ->
    DefaultNS;
%%mk_EII_namespace([{PName,_}|GrandPs],NS=#xmlNamespace{default=[]},S) ->
mk_EII_namespace([{PName,_}|GrandPs],NS,S) ->
    NameStr = atom_to_list(PName),
    case string:tokens(NameStr,":") of
	[Prefix,_LocalName] ->
	    namespace(Prefix,NS,[]);
	[_LocalName] ->
	    mk_EII_namespace(GrandPs,NS,S)
    end;
mk_EII_namespace(_,NS,_S) ->
    NS#xmlNamespace.default.

mk_EII_Att_QName(AttName,XMLEl,S) when is_list(AttName) ->
    mk_EII_Att_QName(list_to_atom(AttName),XMLEl,S);
mk_EII_Att_QName(AttName,XMLEl,S) ->
    NameStr = atom_to_list(AttName),
    {member($:,NameStr),mk_EII_QName(AttName,XMLEl,S)}.
	
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% table access functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_tables(S=#xsd_state{table=undefined}) ->
    Tid=ets:new(xmerl_schema_tab,[]),
    S#xsd_state{table=Tid};
create_tables(S) ->
    S.

delete_table(#xsd_state{table=Tab}) ->
    catch ets:delete(Tab).

%% @hidden
print_table(#xsd_state{table=Tab}) ->
    case catch ets:tab2list(Tab) of
	Res when is_list(Res) ->
	    Res;
	{'EXIT',Reason} ->
	    {error,{?MODULE,[],Reason}}
    end;
print_table(_) ->
    ok.

%save_object({name,_},S) ->
%    %% already saved.
%    S;
%% only simpleType asn complexType are temporary saved with
%% three-tuple key. They are loaded and merged in redefine/2.
save_object({Kind,Obj},S=#xsd_state{redefine=true}) 
  when  Kind == simpleType; Kind == complexType ->
    save_in_table({Kind,redefine,object_name(Obj)},Obj,S);
save_object({Kind,Obj},S=#xsd_state{redefine=true}) 
  when Kind == group; Kind == attributeGroup ->
    save_in_table({Kind,object_name(Obj)},Obj,S);
save_object({Kind,Obj},S) when Kind == simpleType; Kind == complexType ->
    save_unique_type({Kind,object_name(Obj)},Obj,S);
save_object({Kind,Obj},S) 
  when Kind == attributeGroup; Kind == group ->
    save_uniquely({Kind,object_name(Obj)},Obj,S);
save_object({Kind,Obj},S) ->
    save_in_table({Kind,object_name(Obj)},Obj,S).

save_unique_type(Key={_,Name},Obj,S) ->
    case resolve({simple_or_complex_Type,Name},S) of
	{#schema_simple_type{},_} ->
	    acc_errs(S,{[],?MODULE,{type_not_uniquely_defined_in_schema,Name}});
	{#schema_complex_type{},_} ->
	    acc_errs(S,{[],?MODULE,{type_not_uniquely_defined_in_schema,Name}});
	_ ->
	    save_in_table(Key,Obj,S)
    end.

save_uniquely(Key,Obj,S) ->
    case load_object(Key,S) of
	{[],_} ->
	    save_in_table(Key,Obj,S);
	_ ->
	    acc_errs(S,{[],?MODULE,{not_uniquely_defined_in_schema,Key}})
    end.



save_schema_element(CM,S=#xsd_state{elementFormDefault = EFD,
				    attributeFormDefault = AFD,
				    targetNamespace = TN,
				    finalDefault = FD,
				    blockDefault = BD}) ->
    ElementList = [X||X = {element,_} <- CM],
%%    OtherGlobalEls = other_global_elements(S,ElementList),
    Schema = get_schema_cm(S#xsd_state.table,TN),
    Schema2 =
    case Schema == #schema{} of
	true ->
	    Schema#schema{elementFormDefault = EFD,
			  attributeFormDefault = AFD,
			  targetNamespace = TN,
			  blockDefault = BD,
			  finalDefault = FD,
			  content = ElementList};
	_ ->
	    Content = Schema#schema.content,
	    Schema#schema{content=[X||X<-Content,member(X,ElementList)==false]++ElementList}
    end,
    TN2 = case TN of
	      undefined -> [];
	      _ -> TN
	  end,
    _ = save_in_table({schema,TN2},Schema2,S),
    save_to_file(S).

%% other_global_elements(S,ElementList) ->
%%     Schema = get_schema_cm(S#xsd_state.table,S#xsd_state.targetNamespace),
%%     [X||X<-Schema#schema.content,
%% 	member(X,ElementList) == false].

%% other_global_elements(#xsd_state{schema_name=SchemaName,
%% 				 table = Tab,
%% 				 global_element_source=GES},ElementList) ->
%%     case [X||{Y,X}<-GES,Y==SchemaName] of
%% 	[] ->
%% 	    [];
%% 	L -> %% All other schemas included in redefine
%% 	    NameList = [X||{element,{X,_}}<-ElementList],
%% 	    Contents = 
%% 		flatten([X||#schema{content=X}<-[get_schema_cm(Tab,Y)||Y<-L]]),
%% 	    SortFun =
%% 		fun({_,{A,_}},{_,{B,_}}) when A =< B ->
%% 			true;
%% 		   (_,_) -> false end,
%% 	    [X||X={element,{Y,_}}<-lists:sort(SortFun,Contents),member(Y,NameList)==false]
%%     end.

save_to_file(S=#xsd_state{tab2file=true},FileName) ->
    save_to_file(S#xsd_state{tab2file=FileName});
save_to_file(_,_) ->
    ok.

save_to_file(S=#xsd_state{tab2file=TF}) ->
    case TF of
	true ->
	    {ok,IO}=file:open(filename:rootname(S#xsd_state.schema_name)++".tab",
			      [write]),
	    io:format(IO,"~p~n",[catch ets:tab2list(S#xsd_state.table)]),
	    ok = file:close(IO);
	false ->
	    ok;
	IOFile ->
	    {ok,IO}=file:open(IOFile,[write]),
	    io:format(IO,"~p~n",[catch ets:tab2list(S#xsd_state.table)]),
	    ok = file:close(IO)
    end.

save_merged_type(Type=#schema_simple_type{},S) ->
    resave_object({simpleType,Type},S);
save_merged_type(Type=#schema_complex_type{},S) ->
    resave_object({complexType,Type},S).
resave_object({Kind,Obj},S) ->
    save_in_table({Kind,object_name(Obj)},Obj,S).

save_in_table(Name,ElDef,S=#xsd_state{table=Tab}) ->
    catch ets:insert(Tab,{Name,ElDef}),
    S.

save_idc(key,IDConstr,S) ->
    save_key(IDConstr,S);
save_idc(keyref,IDConstr,S) ->
    save_keyref(IDConstr,S);
save_idc(unique,IDConstr,S) ->
    save_unique(IDConstr,S).

save_key(Key,S) ->
    _ = save_object({key,Key},S),
    S.

save_keyref(KeyRef=#id_constraint{category=keyref},S) ->
    S1 = add_keyref(KeyRef,S),
    _ = save_object({keyref,KeyRef},S1),
    S1;
save_keyref(_,S) ->
    S.

save_unique(Unique,S) ->
    _ = save_object({unique,Unique},S),
    S.

save_substitutionGroup([],S) ->
    S;
save_substitutionGroup([{Head,Members}|SGs],S) ->
    %% save {head,[members]}
    _ = save_in_table({substitutionGroup,Head},Members,S),
    %% save {member,head}, an element can only be a member in one
    %% substitutionGroup
    lists:foreach(fun(X)->save_in_table({substitutionGroup_member,X},Head,S) end,Members),
    save_substitutionGroup(SGs,S).
substitutionGroup_member(ElName,S) ->
    case load_object({substitutionGroup_member,ElName},S) of
	{[],_} ->
	    false;
	{Res,_} ->
	    Res
    end.
%% substitutionGroup_head(Head,S) ->
%%     case load_object({substitutionGroup,Head},S) of
%% 	{[],_} ->
%% 	    false;
%% 	{Res,_} ->
%% 	    Res
%%     end.

add_keyref(#id_constraint{name=Name,refer=Refer},
	   S=#xsd_state{keyrefs=KeyRefs}) ->
    S#xsd_state{keyrefs=add_once({keyref,Name,Refer},KeyRefs)}.


load_redefine_object({Kind,Name},S) ->
    load_object({Kind,redefine,Name},S).

load_object({element,{QN,Occ={Min,_}}},S) when is_integer(Min) ->
    case load_object({element,QN},S) of
	{SE=#schema_element{},S1} -> {SE#schema_element{occurance=Occ},S1};
	Other -> Other
    end;
load_object({group,{QN,_Occ={Min,_}}},S) when is_integer(Min) ->
    load_object({group,QN},S);
load_object(Key,S=#xsd_state{table=Tab}) ->
    case ets:lookup(Tab,Key) of
	[{Key,Value}] ->
	    {Value,S};
	[] -> 
	    case ets:lookup(Tab,global_def(Key)) of
		[{_,Value}] -> {Value,global_scope(S)};
		Other -> {Other,S}
	    end;
	Other ->
	    {Other,S}
    end.


load_keyref(Name,S) ->
    case load_object({keyref,Name},S) of
	{KeyRef=#id_constraint{},_} -> KeyRef;
	_ ->
	    []
    end.
load_key(Name,S) ->
    case load_object({key,Name},S) of
	{Key=#id_constraint{},_} -> Key;
	_ ->
	    case load_object({unique,Name},S) of
		{Key=#id_constraint{},_} -> Key;
		_  ->
		    []
	    end
    end.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END table access functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_ID(ID,S) ->
    case member(ID,S#xsd_state.'IDs') of
	true ->
	    acc_errs(S,{'ID_name_not_unique',ID});
	_ ->
	    S#xsd_state{'IDs'=[ID|S#xsd_state.'IDs']}
    end.
check_and_save_ID(ID,S) ->
    case xmerl_xsd_type:check_simpleType('ID',ID,S) of
	{ok,ID} ->
	    save_ID(ID,S);
	_ ->
	    acc_errs(S,{illegal_ID_value,ID})
    end.

insert_substitutionGroup(#schema_element{substitutionGroup=undefined},S) ->
    S;
insert_substitutionGroup(#schema_element{name=Name,
					 substitutionGroup=SG},
			 S=#xsd_state{substitutionGroups=SGregister}) ->
    case key1search(SG,SGregister,[]) of
	{_,SGList} ->
	    S#xsd_state{substitutionGroups=
			keyreplace(SG,1,SGregister,{SG,[Name|SGList]})};
	_ -> 
	    S#xsd_state{substitutionGroups=[{SG,[Name]}|SGregister]}
    end.


global_scope(S=#xsd_state{}) ->
    S#xsd_state{scope=[]}.

global_def({Kind,{Local,_,NS}}) 
  when Kind==simpleType; Kind==complexType; Kind==group;
       Kind==attributeGroup; Kind==element; Kind==attribute;
       Kind==substitutionGroup;Kind==substitutionGroup_member->
    {Kind,{Local,[],NS}};
global_def(D) -> D.


get_schema_cm(Tab,undefined) ->
    get_schema_cm(Tab,[]);
get_schema_cm(Tab,[]) ->
    get_schema_cm1(Tab,[]);
get_schema_cm(Tab,Namespace) ->
    NoNamespaceC=get_no_namespace_content(Tab),
    Schema = get_schema_cm1(Tab,Namespace),
    NSC = Schema#schema.content,
    Schema#schema{content=NSC++[X||X<-NoNamespaceC,member(X,NSC)==false]}.
get_schema_cm1(Tab,Namespace) ->
    case catch ets:lookup(Tab,{schema,Namespace}) of
	[{_,H}] ->
	    H;
	_ ->
	    #schema{}
    end.
get_no_namespace_content(Tab) ->
    case get_schema_cm1(Tab,[]) of
	#schema{content=C} ->
	    C;
	_ -> []
    end.


%% is_simple_type(Type,S) when is_atom(Type) ->
%%     is_simple_type(atom_to_list(Type),S);
is_simple_type({LName,Scope,NS},S) when is_atom(LName) ->
    is_simple_type({atom_to_list(LName),Scope,NS},S);
is_simple_type(QName={_,_,_},S) ->
    case is_builtin_simple_type(QName) of
	true ->
	    true;
	_ -> 
	    is_derived_simple_type(QName,S)
    end.


is_derived_simple_type(QName,S) ->
%%    case resolve({simple_or_complex_Type,QName},S) of
    case resolve({simpleType,QName},S) of
	{#schema_simple_type{},_} -> true;
	_ -> false
    end.



object_name(#schema_element{name=N}) ->
    N;
object_name(#schema_simple_type{name=N}) ->
    N;
object_name(#schema_complex_type{name=N}) ->
    N;
object_name(#schema_attribute{name=N}) ->
    N;
object_name(#schema_attribute_group{name=N}) ->
    N;
object_name(#schema_group{name=N}) ->
    N;
object_name(#id_constraint{name=N}) ->
    N.


is_whitespace(#xmlText{value=V}) ->
    case [X|| X <- V, whitespace(X) == false] of
	[] ->
	    true;
	_ -> false
    end;
is_whitespace(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fetch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch(URI,S) ->
    Split = filename:split(URI),
    Filename = fun([])->[];(X)->lists:last(X) end (Split),
    Fullname = 
	case Split of %% how about Windows systems?
	    ["file:"|Name]-> %% absolute path, see RFC2396 sect 3
		%% file:/dtd_name 
		filename:join(["/"|Name]);
	    ["/"|Rest] when Rest /= [] ->
		%% absolute path name
		URI;
	    ["http:"|_Rest] ->
		{http,URI};
	    [] -> %% empty systemliteral
		[];
	    _ ->
		case S#xsd_state.external_xsd_base of
		    true ->
			filename:join(S#xsd_state.xsd_base, URI);
		    false ->
			filename:join(S#xsd_state.xsd_base, filename:basename(URI))
		end
	end,
    Path = path_locate(S#xsd_state.fetch_path, Filename, Fullname),
    ?dbg("fetch(~p) -> {file, ~p}.~n", [URI, Path]),
    {ok, Path, S}.

path_locate(_, _, {http,_}=URI) ->
    URI;
path_locate(_, _, []) ->
    [];
path_locate([Dir|Dirs], FN, FullName) ->
    F = filename:join(Dir, FN),
    case file:read_file_info(F) of
	{ok, #file_info{type = regular}} ->
	    {file,F};
	_ ->
	    path_locate(Dirs, FN, FullName)
    end;
path_locate([], _FN, FullName) ->
    {file,FullName}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_error(Errs) ->
    {error,reverse(Errs)}.

return_schema_error(Errs) ->
    {error,{schema_failure,reverse(Errs)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% general helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if_atom_to_list(A) when is_atom(A) ->
    atom_to_list(A);
if_atom_to_list(L) ->
    L.
if_list_to_atom(L) when is_list(L) ->
    list_to_atom(L);
if_list_to_atom(A) ->
    A.

list_members(Members,CompleteList) ->
    case [X||X<-Members,member(X,CompleteList)==false] of
	[] ->
	    true;
	L ->
	    {error,L}
    end.

whitespace(X) when ?whitespace(X) ->
    true;
whitespace(_) ->
    false.

key1search(Key,List,Default) ->
    case keysearch(Key,1,List) of
	{value,V} -> V;
	_ -> Default
    end.

keyNsearch(Key,N,L,Default) ->
    case keysearch(Key,N,L) of
	{value,V} -> V;
	_ -> Default
    end.

key_replace_or_insert(Key,N,List,Tuple) ->
    case keyreplace(Key,N,List,Tuple) of
	List ->
	    [Tuple|List];
	NewList ->
	    NewList
    end.

keysearch_delete(Key,N,List,Default) ->
    case keysearch(Key,N,List) of
	{value,Res} ->
	    {Res,keydelete(Key,N,List)};
	_ ->
	    {Default,List}
    end.

search_delete_all_el(ElName,ElList,S) ->
    case search_delete_all_el2(ElName,ElList,[]) of
	false ->
	    case substitutionGroup_member(ElName,S) of
		false ->
		    false;
		Head  ->
		    case search_delete_all_el(Head,ElList,S) of
			{_,Rest} ->
			    {Name,_,NS} = ElName,
			    {{element,{Name,[],NS}},Rest};
			_ ->
			    false
		    end
	    end;
	Res ->
	    Res
    end.
search_delete_all_el2(_ElName,[],_NoMatch) ->
    false;
%% name must match defined (local scope) and referenced (global scope)
%% elements.
search_delete_all_el2({Name,Scope,NS},
		     [El={element,{{Name,ScopeCM,NS},_}}|Rest],
		     NoMatch) 
  when Scope == ScopeCM; ScopeCM == [] ->
    {El,reverse(NoMatch)++Rest};
search_delete_all_el2(ElName,[H|T],NoMatch) ->
    search_delete_all_el2(ElName,T,[H|NoMatch]).

%% Search attribute should not consider the scope. All attributes
%% allowed in this scope are in SchemaAttList.
search_attribute(true,{Name,_,Namespace},SchemaAtts) ->
    case [A||A={_,{N,_,NS}}<-SchemaAtts,N==Name,NS==Namespace] of
	[] ->
	    {undefined,SchemaAtts};
	[Attr] ->
	    {Attr,lists:delete(Attr,SchemaAtts)}
    end;
search_attribute(_,{Name,_,_},SchemaAtts) ->
    case [A||A={_,{N,_,_}}<-SchemaAtts,N==Name] of
	[] ->
	    {undefined,SchemaAtts};
	[Attr] ->
	    {Attr,lists:delete(Attr,SchemaAtts)}
    end.

error_msg(Format,Args) ->
    error_logger:error_msg(Format,Args).


add_once(El,L) ->
    case member(El,L) of
	true ->
	    L;
	_ ->
	    [El|L]
    end.

add_key_once(Key,N,El,L) ->
    case keymember(Key,N,L) of
	true ->
	    L;
	_ ->
	    [El|L]
    end.

%% shema_el_pathname({Type,_},Env) ->
%%     mk_path(reverse([Type|Env])).
%% xml_el_pathname(#xmlElement{name=Name,parents=Parents,pos=Pos}) ->
%%     {element,mk_xml_path(Parents,Name,Pos)};
%% xml_el_pathname(#xmlAttribute{name=Name,parents=Parents,pos=Pos}) ->
%%     {attribute,mk_xml_path(Parents,Name,Pos)};
%% xml_el_pathname(#xmlText{parents=Parents,pos=Pos}) ->
%%     {text,mk_xml_path(Parents,text,Pos)}.

%% mk_path([]) ->
%%     [];
%% mk_path(L) when is_list(L) ->
%%     "/"++filename:join(L).

%% mk_xml_path(Parents,Type,Pos) ->
%% %%    ?dbg("mk_xml_path: Parents = ~p~n",[Parents]),
%%     {filename:join([[io_lib:format("/~w(~w)",[X,Y])||{X,Y}<-Parents],Type]),Pos}.

%% @spec format_error(Errors) -> Result
%%       Errors     = tuple() | [tuple()]
%%       Result       = string() | [string()]
%% @doc Formats error descriptions to human readable strings.
format_error(L) when is_list(L) -> 
    [format_error(X)||X<-L];
format_error({unexpected_rest,UR}) ->
    io_lib:format("XML: The following content of an element didn't validate by the provided schema, ~n~p.",[UR]);
format_error({unvalidated_rest,UR}) ->
    io_lib:format("XML: The following content of an element didn't validate by the provided schema, ~n~p.",[UR]);
format_error({no_schemas_provided}) ->
    "Schema: Validator found no schema. A schema must be provided for validation.";
format_error({internal_error,Reason}) ->
    io_lib:format("An error occured that was unforeseen, due to ~p.",[Reason]);
format_error({internal_error,Reason,Info}) ->
    io_lib:format("An error occured that was unforeseen, due to ~p: ~p.",[Reason,Info]);
format_error({internal_error,Function,Info1,Info2}) ->
    io_lib:format("An internal error occured in function ~p with args: ~p,~p.",[Function,Info1,Info2]);
format_error({illegal_content,Reason,Kind}) ->
    io_lib:format("Schema: The schema violates the content model allowed for schemas.~nReason: ~p,~nkind of schema element: ~p.",[Reason,Kind]);
format_error({no_match,Kind}) ->
    io_lib:format("Schema: The schema violates the content model allowed for schemas.~nKind of schema element: ~p.",[Kind]);
format_error({bad_match,S4SC,CM}) ->
    io_lib:format("Schema: The schema missed mandatory elements ~p in ~p.",[S4SC,CM]);
format_error({unmatched_mandatory_object,SequenceEl1,SequenceEl2}) ->
    io_lib:format("Schema: The schema should have had an ~p object after the ~p, but it was missing.",[SequenceEl2,SequenceEl1]);
format_error({parsing_external_schema_failed,File,Reason}) ->
    io_lib:format("Schema: Parsing the referenced external schema ~p, failed due to ~p.",[File,Reason]);
format_error({fetch_fun_failed,Other}) ->
    io_lib:format("Schema: Fetching this kind of external schema is not supported ~p.",
		  [Other]);
format_error({element_not_in_schema,[EIIName,_ElQName,_CM]}) ->
    io_lib:format("XML: The XML element ~p are not present in the schema.",
		  [EIIName]);
format_error({missing_mandatory_element,CMEl}) ->
    io_lib:format("XML: The XML file missed mandatory element(s) ~p defined in schema.",[CMEl]);
format_error({empty_content_not_allowed,C}) ->
    io_lib:format("XML: The XML file missed mandatory element(s): ~p defined in schema.",[C]);
format_error({element_not_suitable_with_schema,ElName,_S}) ->
    io_lib:format("XML: The XML element: ~p violates the schema, probably to many of same element.",[ElName]);
format_error({element_not_suitable_with_schema,ElName,CMName,_CMEl,_S}) ->
    io_lib:format("XML: The XML element: ~p violates the schema. Schema expected element ~p.",[ElName,CMName]);
format_error({no_element_expected_in_group,XML}) ->
    io_lib:format("XML: The XML element(s) ~p violates the schema. No element was expected.",[XML]);
format_error({element_bad_match,E,Any,_Env}) ->
    io_lib:format("XML: XML element ~p didn't match into the namespace of schema type any ~p.",[E,Any]);
format_error({match_failure,_XML,_CM,_S}) ->
    "XML: A combination of XML element(s) and schema definitions that is not known has occured. The implementation doesn't support this structure.";
format_error({cannot_contain_text,_XMLTxt,CMEl}) ->
    io_lib:format("XML: The schema structure: ~p doesn't allow text",[CMEl]);
format_error({missing_mandatory_elements,MandatoryEls}) ->
    io_lib:format("XML: A schema sequence has mandatory elements ~p, that were unmatched.",[MandatoryEls]);
format_error({choice_missmatch,T,Els}) ->
    io_lib:format("XML: A schema choice structure with the alternatives: ~p doesn't allow the text: ~p.",[Els,T]);
format_error({no_element_matching_choice,XML}) ->
    io_lib:format("XML: The choice at location: ~p had no alternative that matched the XML structure(s): ~p.",[error_path(XML,undefined),XML]);
format_error({all_missmatch,T,CM}) ->
    io_lib:format("XML: The schema expected one of: ~p, but the XML content was text: ~p at the location: ~p.",[CM,T,error_path(T,undefined)]);
format_error({element_not_in_all,ElName,E,_CM}) ->
    io_lib:format("XML: The element ~p at location ~p in the XML file was not allowed according to the schema.",[ElName,error_path(E,undefined)]);
format_error({missing_mandatory_elements_in_all,MandatoryEls}) ->
    io_lib:format("XML: The schema elements ~p were missed in the XML file.",[MandatoryEls]);
format_error({failed_validating,E,Any}) ->
    io_lib:format("XML: The element ~p at location ~p failed validation. It should hav been matched by an any schema element ~p",[E#xmlElement.name,error_path(E,undefined),Any]);
format_error({schemaLocation_list_failure,Paths}) ->
    io_lib:format("XML: schemaLocation values consists of one or more pairs of URI references, separated by white space. The first is a namespace name the second a reference to a schema: ~p.",[Paths]);
format_error({element_content_not_nil,XMLEl}) ->
    io_lib:format("XML: The element ~p at position ~p has content of text/elements despite the nillable attribute was true.",[XMLEl#xmlElement.name,error_path(XMLEl,undefined)]);
format_error({attribute_in_simpleType,El,Att}) ->
    io_lib:format("XML: The element ~p at location ~p must not have attributes like: ~p since it according to the schema has simpleType type.",[El#xmlElement.name,error_path(El,undefined),Att]);
format_error({required_attribute_missed,El,Name}) ->
    io_lib:format("XML: The schema required an attribute ~p in element at location ~p that was missing.",[Name,error_path(El,undefined)]);
format_error({default_and_fixed_attributes_mutual_exclusive,
	      Name,Default,Fix}) ->
    io_lib:format("Schema: It is an error in the schema to assign values for both default and fix for an attribute. Attribute: ~p, default: ~p, fix: ~p.",[Name,Default,Fix]);
format_error({schema_error,unexpected_object,_SA,_Err}) ->
    "Schema: An unforeseen error case occured, maybee due to an unimplemented feature.";
format_error({attribute_not_defined_in_schema,Name}) ->
    io_lib:format("XML: The attribute ~p is not defined in the provided schema.",[Name]);
format_error({disallowed_namespace,Namespace,NS,Name}) ->
    io_lib:format("XML: The attribute ~p is not valid because the namespace ~p is forbidden by ~p",[Name,NS,Namespace]);
format_error({cirkular_attributeGroup_reference,Name}) ->
    io_lib:format("Schema: Cirkular references to attribute groups are forbidden. One was detected including ~p.",[Name]);
format_error({could_not_resolve_type,ST}) ->
    io_lib:format("Schema: The simpleType ~p could not be found among the types defined by the provided schema.",[ST]);
format_error({could_not_check_value_for_type,Type}) ->
    io_lib:format("XML: Checking value for type ~p is not implemented.",[Type]);
format_error({unknown_simpleType,BT}) ->
    io_lib:format("Schema: The simpleType ~p could not be found among the types defined by the provided schema",[BT]);
format_error({abstract_element_instance,ElName}) ->
    io_lib:format("XML: Elements defined as abstract in the schema must not be instantiated in XML: ~p.",[ElName]);
format_error({qualified_name_required,LocalName}) ->
    io_lib:format("XML: Element name ~p in XML instance is not qualified, though the schema requires that.",[LocalName]);
format_error({unqualified_name_required,QualifiedName}) ->
    io_lib:format("XML: Element name ~p in XML instance must be unqualified, according to schema.",[QualifiedName]);
format_error({illegal_key_sequence_value,Err}) ->
    io_lib:format("XML: The 'key-sequence', (se XML-spec 3.11.4), must be a node with at most one member: ~p",[Err]);
format_error({qualified_node_set_not_correct_for_key,_Err}) ->
    "Schema: The 'target node set' and 'qualified node set' (se XML-spec 3.11.4.2.1) must be equal.";
format_error({key_value_not_unique,KS}) ->
    io_lib:format("Schema: Key values must be unique within the schema. This is not ~p,",[KS]);
format_error({keyref_missed_matching_key,Refer}) ->
    io_lib:format("Schema: This keyref had no matching key ~p.",[Refer]);
format_error({keyref_unexpected_object,_Other}) ->
    "Schema: An unforeseen error case occured, unknown failure cause.";
format_error({cardinality_of_fields_not_equal,KR,K}) ->
    io_lib:format("Schema: keyref and the corresponding key must have same cardinality of their fields. Missmatch in this case keyref: ~p, key: ~p.",[KR,K]);
format_error({could_not_load_keyref,Name}) ->
    io_lib:format("Schema: The schema didn't define a keyref with the name ~p.",[Name]);
format_error({reference_undeclared,Kind,Ref}) ->
    io_lib:format("Schema: The schema didn't define an ~p with the name ~p.",[Kind,Ref]);
format_error({cyclic_substitutionGroup,SGs}) ->
    io_lib:format("Schema: cyclic substitutionGroup was detected, substitutionGroup structure is ~p.",[SGs]);
format_error({substitutionGroup_error,Head,SG}) ->
    io_lib:format("Schema: Either of substitutionGroup members ~p or ~p is not defined in the provided schema.",[Head,SG]);
format_error({cyclic_definition,CA}) ->
    io_lib:format("Schema: A forbidden cicular definition was detected ~p.",[CA]);
format_error({type_of_element_not_derived,MemT,HeadT}) ->
    io_lib:format("Schema: Type in substitutionGroup members should be simpleType or complexType. In this case ~p and ~p were found.",[MemT, HeadT]);
format_error({derivation_blocked,BlockTag,Derivation}) ->
    io_lib:format("Derivation by ~p is blocked by the blocking tag ~p.",[Derivation,BlockTag]);
format_error({names_not_equal,QName1,QName2}) ->
    io_lib:format("The type ~p seems to be derived from another type than the base type ~p",[QName2,QName1]);
%% format_error({miss_match_base_types,QName1,QName2}) ->
%%     io_lib:format("Types and/or names of base type ~p and derived type ~p doesn't fit.",[QName1,QName2]);
format_error({illegal_content_in_extension,Ext}) ->
    io_lib:format("The extension content ~p didn't match the content model of the provided schema.",[Ext]);
format_error({SeqCho,expected,Other,found}) 
  when SeqCho == sequence;SeqCho == choice ->
    io_lib:format("Schema: The restriction content ~p didn't match the content model of the provided schema, ~p was expected.",[SeqCho,Other]);
format_error({does_not_support,F,in_restriction}) ->
    io_lib:format("Schema: The structure ~p is not supported in the implementation.",[F]);
format_error({illegal_content_simple_type,CM,TypeName}) ->
    io_lib:format("Schema: ~p content is not allowed in a simpleType, as in ~p.",[CM,TypeName]);
format_error({illegal_in_restriction_of_simpleType,X}) ->
    io_lib:format("Schema: The ~p content is illegal in a simpleType.",[X]);
format_error({element,Name,not_present_in_restriction}) ->
    io_lib:format("Schema: In a restriction all element names of the restriction must be one of the elements of the base type. ~p is not.",[Name]);
format_error({invalid_derivation,EA,BaseAtts}) ->
    io_lib:format("Schema: An anyAttribute ~p in a restricted derived type must be present among the base type attributes ~p.",[EA,BaseAtts]);
format_error({wildcard_namespace,NS,not_subset_of_base_namespace,BaseNS}) ->
    io_lib:format("Schema: See XML spec. section 3.10.6. This wildcard namespace ~p is not allowed by the base namespace restrictions ~p.",[NS,BaseNS]);
format_error({wildcard_namespace_union_not_expressible,NS1,NS2}) ->
    io_lib:format("Schema: See XML spec. section 3.10.6. The union of namespaces ~p and ~p is not expressible.",[NS1,NS2]);
format_error({wildcard_namespace_intersection_not_expressible,NS1,NS2}) ->
    io_lib:format("Schema: See XML spec. section 3.10.6. The intersection of namespaces ~p and ~p is not expressible.",[NS1,NS2]);
format_error({circular_reference_of_type,TName}) ->
    io_lib:format("Schema: An illicit circular reference involving simple/complex type ~p has been detected.",[TName]);
format_error({type_not_uniquely_defined_in_schema,Name}) ->
    io_lib:format("Schema: See XML spec. section 3.4.1. Type names whether simple or complex must be unique within the schema. ~p is not.",[Name]);
format_error({not_uniquely_defined_in_schema,Key}) ->
    io_lib:format("Schema: All schema objects of the same kind identified by name must be unique within the schema. ~p is not.",[Key]);
format_error({illegal_ID_value,ID}) ->
    io_lib:format("The ID value ~p is not allowed as an ID value.",[ID]);
format_error({incomplete_file,_FileName,_Other}) ->
    "Schema: The file containing a schema state must be produced by xmerl_xsd:state2file/[1,2].";
format_error({unexpected_content_in_any,A}) ->
    io_lib:format("Schema: The any type is considered to have no content besides annotation. ~p was found.",[A]);
format_error({erroneous_content_in_identity_constraint,IDC,Err}) ->
    io_lib:format("Schema: An ~p identity constraint must have one selector and one or more field in content. This case ~p",[IDC,Err]);
format_error({missing_xpath_attribute,IDCContent}) ->
    io_lib:format("Schema: A ~p in a identity constraint must have a xpath attribute.",[IDCContent]);
format_error({content_in_anyAttribute,Err}) ->
    io_lib:format("Schema: ~p is not allowed in anyAttribute. Content cannot be anything else than annotation.",[Err]);
format_error({content_in_simpleContent,Err}) ->
    io_lib:format("Schema: Content of simpleContent can only be an optional annotation and one of restriction or extension. In this case ~p.",[Err]);
format_error({complexContent_content_failure,Err}) ->
    io_lib:format("Schema: Besides an optional annotation complexContent should have one of restriction or extension. In this case ~p.",[Err]);
format_error({union_member_type_not_simpleType,IllegalType}) ->
    io_lib:format("Schema: ~p is not allowed in a union. Content must be any nymber of simpleType.",[IllegalType]);
format_error({missing_base_type,restriction,_Other}) ->
    "Schema: A restriction must have a base type, either assigned by the 'base' attribute or as a simpleType defined in content.";
format_error({content_failure_expected_restriction_or_extension,Kind,_}) ->
    io_lib:format("Schema: A ~p had no restriction or extension in content.",[Kind]);
format_error({content_failure_only_one_restriction_or_extension_allowed,Kind,_}) ->
    io_lib:format("Schema: A ~p can only have one of restriction or extension in content.",[Kind]);
format_error({mandatory_component_missing,S4SCMRest,Kind}) ->
    io_lib:format("Schema: After matching a ~p the schema should have had content ~p.",[Kind,S4SCMRest]);
format_error(Err) ->
    io_lib:format("~p~n",[Err]).
    
%% format_error(ErrMsg,E,SchemaE,Env) ->
%%     ?debug("format_error: ~p~n",[ErrMsg]),
%%     {ErrMsg,format_error2(E,SchemaE,Env)}.
%% format_error2(E,SchemaE,Env) ->
%%     {shema_el_pathname(SchemaE,Env),
%%      xml_el_pathname(E)}.

default_namespace_by_convention() ->
    [{xml,'http://www.w3.org/XML/1998/namespace'}].
