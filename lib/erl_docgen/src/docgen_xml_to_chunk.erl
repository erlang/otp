%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : docgen_xml_to_chunk
%%
%% Created : 1 Nov 2018 by Kenneth Lundin <uabkeld@elxa31hr002>
%%
%% Does translation of Erlang XML docs to EEP-48 doc chunks.
%%----------------------------------------------------------------------
-module(docgen_xml_to_chunk).
-export([main/1]).

-include_lib("kernel/include/eep48.hrl").

main([FromBeam, _Escript, ToChunk]) ->
    %% The given module is not documented, generate a hidden beam chunk file
    Name = filename:basename(filename:rootname(FromBeam)) ++ ".erl",

    EmptyDocs = #docs_v1{ anno = erl_anno:set_file(Name, erl_anno:new(0)),
                          module_doc = hidden, docs = []},
    ok = file:write_file(ToChunk, term_to_binary(EmptyDocs,[compressed])),
    ok;
main([FromXML, FromBeam, _Escript, ToChunk]) ->
    _ = erlang:process_flag(max_heap_size,20 * 1000 * 1000),
    case docs(FromXML, FromBeam) of
        {error, Reason} ->
            io:format("Failed to create chunks: ~p~n",[Reason]),
            erlang:halt(1);
        {docs_v1,_,_,_,_,#{ source := S },[]} when
              %% This is a list of all modules that do are known not have any functions
              S =/= "../xml/gen_fsm.xml",
              S =/= "../xml/shell_default.xml",
              S =/= "../xml/user.xml",
              S =/= "../xml/wxClipboardTextEvent.xml",
              S =/= "../xml/wxDisplayChangedEvent.xml",
              S =/= "../xml/wxGBSizerItem.xml",
              S =/= "../xml/wxGraphicsBrush.xml",
              S =/= "../xml/wxGraphicsFont.xml",
              S =/= "../xml/wxGraphicsPen.xml",
              S =/= "../xml/wxInitDialogEvent.xml",
              S =/= "../xml/wxMaximizeEvent.xml",
              S =/= "../xml/wxMouseCaptureLostEvent.xml",
              S =/= "../xml/wxPaintEvent.xml",
              S =/= "../xml/wxPreviewCanvas.xml",
              S =/= "../xml/wxSysColourChangedEvent.xml",
              S =/= "../xml/wxTaskBarIconEvent.xml",
              S =/= "../xml/wxWindowCreateEvent.xml",
              S =/= "../xml/wxWindowDestroyEvent.xml",
              S =/= "../xml/wxDataObject.xml"
              ->
            io:format("Failed to create chunks: no functions found ~s~n",[S]),
            erlang:halt(1),
            ok;
        Docs ->
            ok = file:write_file(ToChunk, term_to_binary(Docs,[compressed]))
    end.

%% Error handling
%%----------------------------------------------------------------------

-define(error(Reason),
	throw({dom_error, Reason})).

%%----------------------------------------------------------------------

%%======================================================================
%% Records
%%======================================================================

%%----------------------------------------------------------------------
%% State record for the validator
%%----------------------------------------------------------------------
-record(state, {
	  tags=[],         %% Tag stack
	  cno=[],          %% Current node number
	  namespaces = [], %% NameSpace stack
	  dom=[]           %% DOM structure
	 }).

%%======================================================================
%% External functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function: initial_state() -> Result
%% Parameters:
%% Result:
%% Description:
%%----------------------------------------------------------------------
initial_state() ->
    #state{}.

%%----------------------------------------------------------------------
%% Function: get_dom(State) -> Result
%% Parameters:
%% Result:
%% Description:
%%----------------------------------------------------------------------
get_dom(#state{dom=Dom}) ->
    Dom.

%%----------------------------------------------------------------------
%% Function: event(Event, LineNo, State) -> Result
%% Parameters:
%% Result:
%% Description:
%%----------------------------------------------------------------------
event(Event, _LineNo, State) ->
    build_dom(Event, State).


%%======================================================================
%% Internal functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function  : build_dom(Event, State) -> Result
%% Parameters: Event = term()
%%             State = #xmerl_sax_simple_dom_state{}
%% Result    : #xmerl_sax_simple_dom_state{} |
%% Description:
%%----------------------------------------------------------------------

%% Document
%%----------------------------------------------------------------------
build_dom(startDocument, State) ->
    State#state{dom=[startDocument]};
build_dom(endDocument,
	  #state{dom=[{Tag, Attributes, Content} |D]} = State) ->
    case D of
	[startDocument] ->
	    State#state{dom=[{Tag, Attributes,
                              lists:reverse(Content)}]};
	[Decl, startDocument] ->
	    State#state{dom=[Decl, {Tag, Attributes,
                                    lists:reverse(Content)}]};
	_ ->
            %% endDocument is also sent by the parser when a fault occur to tell
            %% the event receiver that no more input will be sent
	    State
    end;

%% Element
%%----------------------------------------------------------------------
build_dom({startElement, _Uri, LocalName, _QName, Attributes},
	  #state{tags=T, dom=D} = State) ->

    A = parse_attributes(LocalName, Attributes),
    CName = list_to_atom(LocalName),

    State#state{tags=[CName |T],
                dom=[{CName,
                      lists:reverse(A),
                      []
                     } | D]};
build_dom({endElement, _Uri, LocalName, _QName},
	  #state{tags=[_ |T],
                 dom=[{CName, CAttributes, CContent},
                      {PName, PAttributes, PContent} = _Parent | D]} = State) ->
    case list_to_atom(LocalName) of
	CName ->
            SectionDepth = length([E || E <- T, E =:= section]),
            MappedCName =
                case CName of
                    title ->
                        lists:nth(SectionDepth+1,[h1,h2,h3]);
                    section when SectionDepth > 0 ->
                        p;
                    CName -> CName
                end,

            State#state{tags=T,
                        dom=[{PName, PAttributes,
                              [{MappedCName, CAttributes,
                                lists:reverse(CContent)}
                               |PContent]
                             } | D]};
        _ ->
            ?error("Got end of element: " ++ LocalName ++ " but expected: " ++
                       CName)
    end;

%% Text
%%----------------------------------------------------------------------
build_dom({characters, String},
	  #state{dom=[{Name, Attributes, Content}| D]} = State) ->
    HtmlEnts = [{"&nbsp;",[160]},
                {"&times;",[215]},
                {"&plusmn;",[177]},
                {"&ouml;","ö"},
                {"&auml;","ä"},
                {"&aring;","å"}
               ],

    NoHtmlEnt =
        lists:foldl(
          fun({Pat,Sub},Str) ->
                  re:replace(Str,Pat,Sub,[global,unicode])
          end,String,HtmlEnts),

    case re:run(NoHtmlEnt,"&[a-z]*;",[{capture,first,binary},unicode]) of
        nomatch -> ok;
        {match,[<<"&lt;">>]} -> ok;
        {match,[<<"&gt;">>]} -> ok;
        Else -> throw({found_illigal_thing,Else,String})
    end,
    NewContent =
        [unicode:characters_to_binary(NoHtmlEnt,utf8)| Content],
    State#state{dom=[{Name, Attributes, NewContent} | D]};

build_dom({ignorableWhitespace, String},
          #state{dom=[{Name,_,_} = _E|_]} = State) ->
    case lists:member(Name,
                      [p,pre,input,code,quote,warning,
                       note,dont,do,c,i,em,strong,
                       seealso,tag,item]) of
        true ->
%            io:format("Keep ign white: ~p ~p~n",[String, _E]),
            build_dom({characters, String}, State);
        false ->
            State
    end;

build_dom({startEntity, SysId}, State) ->
    io:format("startEntity:~p~n",[SysId]),
    State;

%% Default
%%----------------------------------------------------------------------
build_dom(_E, State) ->
    State.

%%----------------------------------------------------------------------
%% Function  : parse_attributes(ElName, Attributes) -> Result
%% Parameters:
%% Result    :
%% Description:
%%----------------------------------------------------------------------
parse_attributes(ElName, Attributes) ->
    parse_attributes(ElName, Attributes, 1, []).

parse_attributes(_, [], _, Acc) ->
    Acc;
parse_attributes(ElName, [{_Uri, _Prefix, LocalName, AttrValue} |As], N, Acc) ->
    parse_attributes(ElName, As, N+1, [{list_to_atom(LocalName), AttrValue} |Acc]).

docs(OTPXml, FromBEAM)->
    case xmerl_sax_parser:file(OTPXml,
                               [skip_external_dtd,
                                {event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok,Tree,_} ->
            {ok, {Module, Chunks}} = beam_lib:chunks(FromBEAM,[exports,abstract_code]),
            Dom = get_dom(Tree),
            NewDom = transform(Dom,[]),
            Chunk = to_chunk(NewDom, OTPXml, Module, proplists:get_value(abstract_code, Chunks)),
            verify_chunk(Module,proplists:get_value(exports, Chunks), Chunk),
            Chunk;
        Else ->
            {error,Else}
    end.

verify_chunk(M, Exports, #docs_v1{ docs = Docs } = Doc) ->

    %% Make sure that each documented function actually is exported
    Exported = [begin
                    FA = {F,A},
                    {M,F,A,lists:member(FA,Exports)}
                end || {{function,F,A},_,_,_,_} <- Docs],
    lists:foreach(fun({_M,_F,_A,true}) ->
                          ok
                  end,Exported),

    try
        shell_docs:validate(Doc)
    catch Err ->
            throw({maps:get(<<"en">>,Doc#docs_v1.module_doc), Err})
    end.

%% skip <erlref> but transform and keep its content
transform([{erlref,_Attr,Content}|T],Acc) ->
    Module = [Mod || Mod = {module,_,_} <- Content],
    NewContent = Content -- Module,
    [{module,SinceAttr,[Mname]}] = Module,
    Since = case proplists:get_value(since,SinceAttr) of
                undefined -> [];
                [] -> [];
                Vsn -> [{since,Vsn}]
            end,
    transform([{module,[{name,Mname}|Since],NewContent}|T],Acc);

%% skip <header> and all of its content
transform([{header,_Attr,_Content}|T],Acc) ->
    transform(T,Acc);
transform([{section,Attr,Content}|T],Acc) ->
    transform(T,[{section,Attr,transform(Content,[])}|Acc]);

%% transform <list><item> to <ul><li> or <ol><li> depending on type attribute
transform([{list,Attr,Content}|T],Acc) ->
    transform([transform_list(Attr,Content)|T],Acc);

%% transform <taglist>(tag,item+)+ to <dl>(dt,item+)+
transform([{taglist,Attr,Content}|T],Acc) ->
    transform([transform_taglist(Attr,Content)|T],Acc);

%% remove <anno> as it is only used to validate specs vs xml src
transform([{anno,[],Content}|T],Acc) ->
    transform([Content|T],Acc);

%% transform <c> to <code>
transform([{c,[],Content}|T],Acc) ->
    transform(T, [{code,[],transform(Content,[])}|Acc]);

%% transform <code> to <pre><code>
transform([{code,Attr,Content}|T],Acc) ->
    transform(T, [{pre,[],[{code,Attr,transform(Content,[])}]}|Acc]);
%% transform <pre> to <pre><code>
transform([{pre,Attr,Content}|T],Acc) ->
    transform(T, [{pre,[],[{code,Attr,transform(Content,[])}]}|Acc]);

%% transform <funcs> with <func> as children
transform([{funcs,_Attr,Content}|T],Acc) ->
    Fns = {functions,[],transform_funcs(Content, [])},
    transform(T,[Fns|Acc]);
%% transform <datatypes> with <datatype> as children
transform([{datatypes,_Attr,Content}|T],Acc) ->
    Dts = transform(Content, []),
    transform(T,[{datatypes,[],Dts}|Acc]);
transform([{datatype,_Attr,Content}|T],Acc) ->
    transform(T,transform_datatype(Content, []) ++ Acc);
%% Ignore <datatype_title>
transform([{datatype_title,_Attr,_Content}|T],Acc) ->
    transform(T,Acc);
%% transform <desc>Content</desc> to Content
transform([{desc,_Attr,Content}|T],Acc) ->
    transform(T,[transform(Content,[])|Acc]);
transform([{strong,Attr,Content}|T],Acc) ->
    transform([{em,Attr,Content}|T],Acc);
%% transform <marker id="name"/>  to <a id="name"/>....
transform([{marker,Attr,Content}|T],Acc) ->
    transform(T,[{a,Attr,transform(Content,[])}|Acc]);
%% transform <url href="external URL"> Content</url> to <a href....
transform([{url,Attr,Content}|T],Acc) ->
    transform(T,[{a,Attr,transform(Content,[])}|Acc]);
%% transform note/warning/do/don't to <p class="thing">
transform([{What,[],Content}|T],Acc)
  when What =:= note; What =:= warning; What =:= do; What =:= dont ->
    WhatP = {p,[{class,atom_to_list(What)}], transform(Content,[])},
    transform(T,[WhatP|Acc]);

transform([{type,_,[]}|_] = Dom,Acc) ->
    %% Types are laid out sequentially in the source xml so we need to
    %% parse them like that here too.
    case transform_types(Dom,[]) of
        {[],T} ->
            transform(T,Acc);
        {Types,T} ->
            %% We sort the types here because in the source xml
            %% the description and the declaration do not have
            %% to be next to each other. But we want to have that
            %% for the doc chunks.
            NameSort = fun({li,A,_},{li,B,_}) ->
                               NameA = proplists:get_value(name,A),
                               NameB = proplists:get_value(name,B),
                               if NameA == NameB ->
                                       length(A) =< length(B);
                                  true ->
                                       NameA < NameB
                               end
                       end,
            transform(T,[{ul,[{class,"types"}],lists:sort(NameSort,Types)}|Acc])
    end;
transform([{type_desc,Attr,_Content}|T],Acc) ->
    %% We skip any type_desc with the variable attribute
    true = proplists:is_defined(variable, Attr),
    transform(T,Acc);
transform([{type,[],Content}|T],Acc) ->
    transform(T,[{ul,[{class,"types"}],transform(Content,[])}|Acc]);
transform([{v,[],Content}|T],Acc) ->
    transform(T, [{li,[{class,"type"}],transform(Content,[])}|Acc]);
transform([{d,[],Content}|T],Acc) ->
    transform(T, [{li,[{class,"description"}],transform(Content,[])}|Acc]);

transform([Tag = {seealso,_Attr,_Content}|T],Acc) ->
    transform([transform_seealso(Tag)|T],Acc);

transform([{term,Attr,[]}|T],Acc) ->
    transform([list_to_binary(proplists:get_value(id,Attr))|T],Acc);

transform([{fsummary,_,_}|T],Acc) ->
    %% We skip fsummary as it many times is just a duplicate of the
    %% first line of the docs.
    transform(T,Acc);

transform([{input,_,Content}|T],Acc) ->
    %% Just remove input as it is not used by anything
    transform(T,[transform(Content,[])|Acc]);

%% Tag and Attr is used as is but Content is transformed
transform([{Tag,Attr,Content}|T],Acc) ->
    transform(T,[{Tag,Attr,transform(Content,[])}|Acc]);
transform([Binary|T],Acc) ->
    transform(T,[Binary|Acc]);
transform([],Acc) ->
    lists:flatten(lists:reverse(Acc)).

transform_list([{type,"ordered"}],Content) ->
    {ol,[],[{li,A2,C2}||{item,A2,C2}<-Content]};
transform_list(_,Content) ->
    {ul,[],[{li,A2,C2}||{item,A2,C2}<-Content]}.

transform_types([{type,Attr,[]}|T],Acc) ->
    case proplists:is_defined(name,Attr) of
        true ->
            transform_types(T, [{li,Attr,[]}|Acc]);
        false ->
            true = proplists:is_defined(variable, Attr),
            transform_types(T, Acc)
    end;
transform_types([{type_desc,Attr,Content}|T],Acc) ->
    case proplists:is_defined(name,Attr) of
        true ->
            TypeDesc = transform(Content,[]),
            transform_types(T, [{li,Attr ++ [{class,"description"}],TypeDesc}|Acc]);
        false ->
            true = proplists:is_defined(variable, Attr),
            transform_types(T, Acc)
    end;
transform_types([{type,_,_}|_T],_Acc) ->
    throw(mixed_type_declarations);
transform_types(Dom,Acc) ->
    {lists:reverse(Acc),Dom}.

transform_taglist(Attr,Content) ->
    Items =
        lists:map(fun({tag,A,C}) ->
                          {dt,A,C};
                     ({item,A,C}) ->
                          {dd,A,C}
                  end, Content),
    {dl,Attr,Items}.

%% if we have {func,[],[{name,...},{name,....},...]}
%% we convert it to one {func,[],[{name,...}] per arity lowest first.
transform_funcs([Func|T],Acc) ->
    transform_funcs(T,func2func(Func) ++ Acc);
transform_funcs([],Acc) ->
    lists:reverse(Acc).

func2func({func,Attr,Contents}) ->

    ContentsNoName = [NC||NC <- Contents, element(1,NC) /= name],

    EditLink =
        case proplists:get_value(ghlink,Attr) of
            undefined ->
                #{};
            GhLink ->
                #{ edit_url =>
                       iolist_to_binary(["https://github.com/erlang/otp/edit/",GhLink]) }
        end,

    VerifyNameList =
        fun(NameList, Test) ->
                %% Assert that we don't mix ways to write <name>
                [begin
                     ok = Test(C),
                     {proplists:get_value(name,T),proplists:get_value(arity,T)}
                 end || {name,T,C} <- NameList]
        end,

    NameList = [Name || {name,_,_} = Name <- Contents],

    %% "Since" is hard to accurately as there can be multiple <name> per <func> and they
    %% can refer to the same or other arities. This should be improved in the future but
    %% for now we set since to a comma separated list of all since attributes.
    SinceMD =
        case [proplists:get_value(since, SinceAttr) ||
                 {name,SinceAttr,_} <- NameList, proplists:get_value(since, SinceAttr) =/= []] of
            [] -> EditLink;
            Sinces ->
                EditLink#{ since => unicode:characters_to_binary(
                                      lists:join(",",lists:usort(Sinces))) }
        end,

    Functions =
        case NameList of
            [{name,_,[]}|_] ->
                %% Spec style function docs
                TagsToFA =
                    fun(Tags) ->
                            {proplists:get_value(name,Tags),
                             proplists:get_value(arity,Tags)}
                    end,

                _ = VerifyNameList(NameList,fun([]) -> ok end),

                FAs = [TagsToFA(FAttr) || {name,FAttr,[]} <- NameList ],
                FAClauses = lists:usort([{TagsToFA(FAttr),proplists:get_value(clause_i,FAttr)}
                                         || {name,FAttr,[]} <- NameList ]),
                Signature = [iolist_to_binary([F,"/",A]) || {F,A} <- FAs],
                lists:map(
                  fun({F,A}) ->
                          Specs = [{func_to_atom(CF),list_to_integer(CA),C}
                                   || {{CF,CA},C} <- FAClauses,
                                      F =:= CF, A =:= CA],
                          {function,[{name,F},{arity,list_to_integer(A)},
                                     {signature,Signature},
                                     {meta,SinceMD#{ signature => Specs }}],
                           ContentsNoName}
                  end, lists:usort(FAs));
            NameList ->
                %% Manual style function docs
                FAs = lists:flatten([func_to_tuple(NameString) || {name, _Attr, NameString} <- NameList]),

                _ = VerifyNameList(NameList,fun([_|_]) -> ok end),

                Signature = [strip_tags(NameString) || {name, _Attr, NameString} <- NameList],
                [{function,[{name,F},{arity,A},
                            {signature,Signature},
                            {meta,SinceMD}],ContentsNoName}
                 || {F,A} <- lists:usort(FAs)]
        end,
    transform(Functions,[]).

func_to_tuple(Chars) ->
    try
        [Name,Args] = string:split(strip_tags(Chars),"("),
        Arities = parse_args(unicode:characters_to_list(Args)),
        [{unicode:characters_to_list(Name),Arity} || Arity <- Arities]
    catch E:R:ST ->
            io:format("Failed to parse: ~p~n",[Chars]),
            erlang:raise(E,R,ST)
    end.

%% This function parses a documentation <name> attribute to figure
%% out the arities if that function. Example:
%%    "start([go,Mode] [,Extra])" returns [1, 2].
%%
%% This assumes that when a single <name> describes many arities
%% the arities are listed with [, syntax.
parse_args(")" ++ _) ->
    [0];
parse_args(Args) ->
    parse_args(unicode:characters_to_list(Args),1,[]).
parse_args([$[,$,|T],Arity,[]) ->
    parse_args(T,Arity,[$[]) ++ parse_args(T,Arity+1,[]);
parse_args([$,|T],Arity,[]) ->
    parse_args(T,Arity+1,[]);
parse_args([Open|T],Arity,Stack)
  when Open =:= $[; Open =:= ${; Open =:= $( ->
    parse_args(T,Arity,[Open|Stack]);
parse_args([$]|T],Arity,[$[|Stack]) ->
    parse_args(T,Arity,Stack);
parse_args([$}|T],Arity,[${|Stack]) ->
    parse_args(T,Arity,Stack);
parse_args([$)|T],Arity,[$(|Stack]) ->
    parse_args(T,Arity,Stack);
parse_args([$)|_T],Arity,[]) ->
    [Arity];
parse_args([_H|T],Arity,Stack) ->
    parse_args(T,Arity,Stack).

strip_tags([{_Tag,_Attr,Content}|T]) ->
    [Content | strip_tags(T)];
strip_tags([H|T]) when not is_tuple(H) ->
    [H | strip_tags(T)];
strip_tags([]) ->
    [].

transform_datatype(Dom,_Acc) ->
    ContentsNoName = transform([NC||NC <- Dom, element(1,NC) /= name],[]),
    [case N of
          {name,NameAttr,[]} ->
              {datatype,NameAttr,ContentsNoName};
          {name,[],Content} ->
              [{Name,Arity}] = func_to_tuple(Content),
              Signature = strip_tags(Content),
              {datatype,[{name,Name},{n_vars,integer_to_list(Arity)},
                         {signature,Signature}],ContentsNoName}
      end || N = {name,_,_} <- Dom].

transform_seealso({seealso,Attr,_Content}) ->
    {a, Attr, _Content}.

to_chunk(Dom, Source, Module, AST) ->
    [{module,MAttr,Mcontent}] = Dom,

    ModuleDocs = lists:flatmap(
                   fun({Tag,_,Content}) when Tag =:= description;
                                             Tag =:= section ->
                           Content;
                      ({_,_,_}) ->
                           []
                   end, Mcontent),

    TypeMeta = add_types(AST, maps:from_list([{source,Source}|MAttr])),

    TypeMap = maps:get(types, TypeMeta, []),

    Anno = erl_anno:set_file(atom_to_list(Module)++".erl",erl_anno:new(0)),

    Types = lists:flatten([Types || {datatypes,[],Types} <- Mcontent]),

    TypeEntries =
        lists:map(
          fun({datatype,Attr,Descr}) ->
                  TypeName = func_to_atom(proplists:get_value(name,Attr)),
                  TypeArity = case proplists:get_value(n_vars,Attr) of
                                  undefined ->
                                      find_type_arity(TypeName, TypeMap);
                                  Arity ->
                                      list_to_integer(Arity)
                              end,
                  TypeArgs = lists:join(",",[lists:concat(["Arg",I]) || I <- lists:seq(1,TypeArity)]),
                  PlaceholderSig = io_lib:format("-type ~p(~s) :: term().",[TypeName,TypeArgs]),
                  TypeSignature = proplists:get_value(
                                    signature,Attr,[iolist_to_binary(PlaceholderSig)]),
                  MetaSig =
                      case maps:get({TypeName, TypeArity}, TypeMap, undefined) of
                          undefined ->
                              #{};
                          Sig ->
                              #{ signature => [Sig] }
                      end,
                  docs_v1_entry(type, Anno, TypeName, TypeArity, TypeSignature, MetaSig, Descr)
          end, Types),

    Functions = lists:flatten([Functions || {functions,[],Functions} <- Mcontent]),

    FuncEntrys =
        lists:flatmap(
          fun({function,Attr,Fdoc}) ->
                  case func_to_atom(proplists:get_value(name,Attr)) of
                      callback ->
                          [];
                      Name ->
                          Arity = proplists:get_value(arity,Attr),
                          Signature = proplists:get_value(signature,Attr),
                          FMeta = proplists:get_value(meta,Attr),
                          MetaWSpec = add_spec(AST,FMeta),
                          [docs_v1_entry(function, Anno, Name, Arity, Signature, MetaWSpec, Fdoc)]
                  end
          end, Functions),

    docs_v1(ModuleDocs, Anno, TypeMeta, FuncEntrys ++ TypeEntries).

docs_v1(DocContents, Anno, Metadata, Docs) ->
    #docs_v1{ anno = Anno,
              module_doc = #{<<"en">> => shell_docs:normalize(DocContents)},
              metadata = maps:merge(Metadata, (#docs_v1{})#docs_v1.metadata),
              docs = Docs }.

docs_v1_entry(Kind, Anno, Name, Arity, Signature, Metadata, DocContents) ->
    AnnoWLine =
        case Metadata of
            #{ signature := [Sig|_] } ->
                SigAnno = element(2, Sig),
                erl_anno:set_line(erl_anno:line(SigAnno), Anno);
            _NoSignature ->
                Anno
        end,
    {{Kind, Name, Arity}, AnnoWLine, lists:flatten(Signature),
     #{ <<"en">> => shell_docs:normalize(DocContents)}, Metadata}.

%% A special list_to_atom that handles
%%  'and'
%%  Destroy
%%  'begin'
func_to_atom(List) ->
    case erl_scan:string(List) of
        {ok,[{atom,_,Fn}],_} -> Fn;
        {ok,[{var,_,Fn}],_} -> Fn;
        {ok,[{Fn,_}],_} -> Fn;
        {ok,[{var,_,_},{':',_},_],_} ->
            callback
    end.

-define(IS_TYPE(TO),(TO =:= type orelse TO =:= opaque)).

add_spec(no_abstract_code, Meta) ->
    Meta;
add_spec({raw_abstract_v1, AST}, Meta = #{ signature := Specs } ) ->
    Meta#{ signature := add_spec_clauses(AST, merge_clauses(Specs,#{})) };
add_spec(_, Meta) ->
    Meta.

add_types(no_abstract_code, Meta) ->
    Meta;
add_types({raw_abstract_v1, AST}, Meta) ->
    Meta#{ types =>
               maps:from_list(
                 [{{Name,length(Args)},T} || T = {attribute,_,TO,{Name, _, Args}} <- AST,
                                             ?IS_TYPE(TO)]) }.

add_spec_clauses(AST, [{{F,A},Clauses}|T]) ->
    [filter_clauses(find_spec(AST,F,A),Clauses) | add_spec_clauses(AST,T)];
add_spec_clauses(_AST, []) ->
    [].

filter_clauses(Spec,[undefined]) ->
    Spec;
filter_clauses({attribute,Ln,spec,{FA,Clauses}},ClauseIds) ->
    {_,FilteredClauses} =
        lists:foldl(
          fun({TO,_,_,_} = C,{Cnt,Acc}) when ?IS_TYPE(TO) ->
                  case lists:member(integer_to_list(Cnt),ClauseIds) of
                      true ->
                          {Cnt+1,[C | Acc]};
                      false ->
                          {Cnt+1,Acc}
                  end
          end, {1, []}, Clauses),
    {attribute,Ln,spec,{FA,lists:reverse(FilteredClauses)}}.

merge_clauses([{F,A,Clause}|T],Acc) ->
    merge_clauses(T,Acc#{ {F,A} => [Clause | maps:get({F,A},Acc,[])]});
merge_clauses([],Acc) ->
    maps:to_list(Acc).

find_type_arity(Name, [{{Name,_},{attribute,_,TO,{Name,_,Args}}}|_T]) when ?IS_TYPE(TO) ->
    length(Args);
find_type_arity(Name, [_|T]) ->
    find_type_arity(Name,T);
find_type_arity(Name, Map) when is_map(Map) ->
    find_type_arity(Name, maps:to_list(Map)).

find_spec(AST, Func, Arity) ->
    Specs = lists:filter(fun({attribute,_,spec,{{F,A},_}}) ->
                                 F =:= Func andalso A =:= Arity;
                            ({attribute,_,spec,{{_,F,A},_}}) ->
                                 F =:= Func andalso A =:= Arity;
                            (_) ->
                                 false
                         end, AST),
    case Specs of
        [S] ->
            S;
        [] ->
            io:format("Could not find spec for ~p/~p~n",[Func,Arity]),
            exit(1)
    end.
