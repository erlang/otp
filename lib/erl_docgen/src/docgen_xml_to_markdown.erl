%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2023. All Rights Reserved.
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
-module(docgen_xml_to_markdown).
-export([main/1, convert_application/1]).
-include_lib("kernel/include/eep48.hrl").

main([Application, FromXML, ToMarkdown]) ->
    io:format("Converting: ~ts (~ts)~n",[FromXML, ToMarkdown]),
    _ = erlang:process_flag(max_heap_size,20 * 1000 * 1000),
    case docs(Application, FromXML) of
        {error, Reason} ->
            io:format("Failed to create chunks: ~p~n",[Reason]),
            throw({error, Reason});
        {skip, erlref} ->
            skip;
        {skip, What} ->
            io:format("Skipping ~p: ~ts~n",[What, FromXML]),
            skip;
        EEP48 ->
            %% [io:format("~tp~n",[EEP48]) || filename:rootname(filename:basename(FromXML)) =:= "erl_nif"],
            Markdown = unicode:characters_to_binary(eep48_to_markdown:render_docs(shell_docs:normalize(EEP48))),
            %% io:format("~ts~n",[Markdown]),
            ok = filelib:ensure_dir(ToMarkdown),
            ok = file:write_file(ToMarkdown, unicode:characters_to_binary([header(),Markdown]))
    end.

convert_application(system) ->
    SrcDir = filename:join([code:root_dir(),"system","doc","xml"]),
    DstDir = filename:join([code:root_dir(),"system","doc"]),
    case xmerl_sax_parser:file(filename:join(SrcDir, "book.xml"),
                               [skip_external_dtd,
                                {event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok, Tree, _} ->
            [{book, _, C}] = get_dom(Tree),
            {parts, _, Includes} = lists:keyfind(parts, 1, C),
            [convert_xml_include(
               system, filename:dirname(filename:join([SrcDir,Part])),
               filename:join(DstDir, filename:basename(filename:dirname(filename:join([SrcDir,Part])))),
               filename:join([SrcDir,Part]))
             || {include, [{href, Part}],[]} <- Includes];
        Error ->
            Error
    end;
convert_application(App) ->
    SrcDir = filename:join([code:lib_dir(App),"doc","xml"]),
    DstDir = filename:join([code:lib_dir(App),"doc"]),
    RefDir = filename:join([code:lib_dir(App),"doc","references"]),
    GuideDir = filename:join([code:lib_dir(App),"doc","guides"]),
    InternalDir = filename:join([code:lib_dir(App),"doc","internal_docs"]),
    case xmerl_sax_parser:file(filename:join(SrcDir, "book.xml"),
                               [skip_external_dtd,
                                {event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok, Tree, _} ->
            [{book, _, C}] = get_dom(Tree),
            Guides =
                case lists:keyfind(parts, 1, C) of
                    {parts, _, [{include, [{href, Part}],[]}]} ->
                        convert_xml_include(App, SrcDir, GuideDir, filename:join([SrcDir,Part]));
                    false ->
                        {undefined, []}
                end,
            {applications, _, [{include, [{href, Applications}],[]}]}  = lists:keyfind(applications, 1, C),
            Apps = convert_xml_include(App, SrcDir, RefDir, filename:join([SrcDir,Applications])),
            {releasenotes, _, [{include, [{href, ReleaseNotes}],[]}]}  = lists:keyfind(releasenotes, 1, C),
            ok = main([atom_to_list(App), filename:join(SrcDir,ReleaseNotes),
                       filename:join(DstDir, filename:rootname(ReleaseNotes) ++ ".md")]),
            Internals =
                case lists:keyfind(internals, 1, C) of
                    {internals, _, [{include, [{href, Internal}],[]}]} ->
                        convert_xml_include(App, SrcDir, InternalDir, filename:join([SrcDir,Internal]));
                    false ->
                        {undefined,[]}
                end,
            LocalConfig = filename:join(DstDir,"docs.exs"),
            ok = file:write_file(
                   LocalConfig,
                   ["[ extras: \n",
                    "## The order of these items determine\n",
                    "## how they are listed in the docs\n",
                    to_group(element(2,Guides) ++
                                 element(2,Apps) ++
                                 element(2, Internals)),"]"]),
            os:cmd("elixir -e 'File.write(\""++LocalConfig++"\", Code.format_file!(\""++LocalConfig++"\"))'"),
            ok;
        Error ->
            Error
    end.

convert_xml_include(App, SrcDir, DstDir, IncludeXML) ->
    case xmerl_sax_parser:file(IncludeXML,
                               [skip_external_dtd,
                                {event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok,Tree,_} ->
            [{_, _, C}] = get_dom(Tree),
            {header, _, Header} = lists:keyfind(header, 1, C),
            {h1, _, Title} = lists:keyfind(h1, 1, Header),
            {Title,
             lists:flatmap(
               fun({include,[{href,Path}],_}) ->
                       Dst = filename:join(DstDir, string:lowercase(filename:rootname(Path)) ++ ".md"),
                       case main([atom_to_list(App), filename:join(SrcDir,Path), Dst]) of
                           skip ->
                               [];
                           ok ->
                               [Dst]
                       end;
                  ({description, _, Content}) when App =:= system ->
                       EEP48 = transform(Content, []),
                       Markdown = unicode:characters_to_binary(
                                    ["# Introduction\n\n", eep48_to_markdown:render_docs(shell_docs:normalize(EEP48))]),
                       Dst = filename:join(DstDir, string:lowercase(filename:basename(DstDir)) ++ ".md"),
                       ok = file:write_file(Dst, Markdown),
                       [Dst];
                  ({Tag, _, _}) when Tag =:= header;
                                     Tag =:= description ->
                       []
               end, C)};
        Else ->
            {error,Else}
    end.

to_group(Paths) ->
    ["[", [["\"", remove_cwd(Path) ,"\","] || Path <- Paths], "], "].


remove_cwd(Path) ->
    {ok, Cwd} = file:get_cwd(),
    remove_cwd(Cwd ++ "/", Path).
remove_cwd([H|T],[H|P]) ->
    remove_cwd(T,P);
remove_cwd(_, P) ->
    case filename:split(P) of
        ["lib",_app, "doc" | Path] ->
            filename:join(Path);
        ["erts", "doc" | Path] ->
            filename:join(Path)
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
                        lists:nth(SectionDepth+1,[h1,h2,h3,h4,h5,h6]);
                    section when SectionDepth > 0 ->
                        'div';
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
    State#state{dom=[{Name, Attributes, [unicode:characters_to_binary(String,utf8)| Content]} | D]};
build_dom(startCDATA, State) ->
    State#state{ tags = [startCDATA | State#state.tags ] };
build_dom(endCDATA, #state{ tags = [ CData | T ] } = State) ->
    CData = startCDATA,
    State#state{ tags = T };

build_dom({ignorableWhitespace, String},
          #state{dom=[{Name,_,_} = _E|_]} = State) ->
    case lists:member(Name,
                      [p,pre,input,code,quote,warning,
                       note,change,dont,do,c,b,i,em,strong,
                       seemfa,seeerl,seetype,seeapp,
                       seecom,seecref,seefile,seeguide,
                       tag,item]) of
        true ->
%            io:format("Keep ign white: ~p ~p~n",[String, _E]),
            build_dom({characters, String}, State);
        false ->
            State
    end;

build_dom({startEntity, _SysId}, State) ->
    %% io:format("startEntity:~p~n",[_SysId]),
    State;

%% Default
%%----------------------------------------------------------------------
build_dom(_E, State) ->
    %% io:format("IgnoredEvent: ~p~n",[_E]),
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

docs(Application, OTPXml)->
    DtdFiles =
        [begin
             To = filename:join(filename:dirname(OTPXml),filename:basename(From)),
             {ok, _} = file:copy(From, To),
             To
         end || From <- filelib:wildcard(filename:join([code:priv_dir(erl_docgen),"dtd","*.dtd"]))
                    ++ filelib:wildcard(filename:join([code:priv_dir(erl_docgen),"dtd_html_entities","*.ent"]))],
    case xmerl_sax_parser:file(OTPXml,
                               [{event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok,Tree,_} ->
            [ok = file:delete(Dtd) || Dtd <- DtdFiles],
            put(application, Application),
            Dom = get_dom(Tree),
            case lists:member(
                   element(1, hd(Dom)),
                   [chapter,
                    cref,
                    comref, fileref, appref]) of
                true ->
                    transform(Dom, []);
                false ->
                    {skip,element(1, hd(Dom))}
            end;
        Else ->
            {error,Else}
    end.

%% skip <chapter> but transform and keep its content
transform([{TopTag,[],Content}], []) when TopTag =:= chapter;
                                          TopTag =:= cref;
                                          TopTag =:= comref;
                                          TopTag =:= fileref;
                                          TopTag =:= appref ->
    put(toptag, TopTag),
    transform(Content, []);

%% skip <header> and all of its content
transform([{header,_Attr,Content}|T],Acc) ->
    case get(toptag) of
        chapter ->
            {file, _, Filename} = lists:keyfind(file, 1, Content),
            put(module, Filename),
            {h1, _, Title} = lists:keyfind(h1, 1, Content),
            transform(T,[{h1,[],Title}|Acc]);
        _ ->
            transform(T,Acc)
    end;

transform([{app, [], Content}|T], Acc) ->
    put(module, Content ++ "_app"),
    transform(T, [{h1,[],Content}|Acc]);
transform([{appsummary, [], Content}|T], Acc) ->
    transform(T, [{p,[],transform(Content,[])}|Acc]);

transform([{com, [], Content}|T], Acc) ->
    put(module, Content ++ "_cmd"),
    transform(T, [{h1,[],Content}|Acc]);
transform([{comsummary, [], Content}|T], Acc) ->
    transform(T, [{p,[],transform(Content,[])}|Acc]);

transform([{lib, [], Content}|T], Acc) ->
    put(module, Content),
    transform(T, [{h1,[],Content}|Acc]);
transform([{libsummary, [], Content}|T], Acc) ->
    transform(T, [{p,[],transform(Content,[])}|Acc]);
transform([{file, [], Content}|T], Acc) ->
    put(module, Content),
    transform(T, [{h1,[],Content}|Acc]);
transform([{filesummary, [], Content}|T], Acc) ->
    transform(T, [{p,[],transform(Content,[])}|Acc]);

transform([{section,_,Content}|T],Acc) ->
    transform(T,[transform(Content,[])|Acc]);
transform([{description,_,Content}|T],Acc) ->
    transform(T,[transform(Content,[]), {h2,[],[<<"Description">>]} | Acc]);

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
    transform(T, [{pre,[],[{code,a2b(Attr),transform(Content,[])}]}|Acc]);
%% transform <pre> to <pre><code>
transform([{pre,Attr,Content}|T],Acc) ->
    transform(T, [{pre,[],[{code,Attr,transform(Content,[])}]}|Acc]);

%% transform <funcs> with <func> as children
transform([{funcs,_Attr,Content}|T],Acc) ->
    true = lists:member(get(toptag),[comref,cref]),
    transform(T, [transform(Content, [])|Acc]);
transform([{func,_, Content}|T], Acc) ->
    Type = get(toptag),
    Name =
        lists:foldl(
          fun({name, _, [{ret, _, _},{nametext,_,NameText}]}, FoldAcc) when Type =:= cref ->
                  [FunctionName | _Rest] = string:split(NameText,"("),
                  [[{h2,[],[unicode:characters_to_binary(io_lib:format("~ts()", [FunctionName]))]
                    }] | FoldAcc ];
             %% ({name, _, NameText}, FoldAcc) when Type =:= cref ->
             %%      [[{h2,[],NameText}] | FoldAcc];
             ({name, _, NameText}, FoldAcc) when Type =:= comref ->
                  [{h2,[],NameText} | FoldAcc];
             (E, FoldAcc) when element(1,E) =/= name ->
                  FoldAcc
          end, [], Content),
    Prototype =
        lists:foldl(
          fun({name, _, [{ret, _, Ret},{nametext,_,NameText}]}, FoldAcc) ->
                  [[{pre,[],
                     [{code,[{type,<<"c">>}],
                       [unicode:characters_to_binary(string:trim([Ret, " ", string:trim(NameText),";"]))]
                      }]
                    }] | FoldAcc ];
             ({name, _, _NameText}, FoldAcc) ->
                  FoldAcc;
             (E, FoldAcc) when element(1,E) =/= name ->
                  FoldAcc
          end, [], Content),
    {desc, _, Desc} = lists:keyfind(desc, 1, Content),
    transform(T, [transform(Desc,[]),
                  lists:flatten(lists:reverse(Prototype)),
                  lists:flatten(lists:reverse(Name))] ++ Acc);

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
%% transform <marker id="name"/>  to <a id="name"/>....
transform([{marker,Attrs,Content}|T],Acc) ->
    transform(T,[{a,a2b(Attrs),transform(Content,[])}|Acc]);
%% transform <url href="external URL"> Content</url> to <a href....
transform([{url,Attrs,Content}|T],Acc) ->
    transform(T,[{a,a2b(Attrs),transform(Content,[])}|Acc]);
%% transform note/change/warning/do/don't to <p class="thing">
transform([{What,[],Content}|T],Acc)
  when What =:= note; What =:= change; What =:= warning; What =:= do; What =:= dont ->
    WhatP = {'div',[{class,atom_to_binary(What)}], transform(Content,[])},
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
            transform(T,[{ul,[{class,<<"types">>}],lists:sort(NameSort,Types)}|Acc])
    end;
transform([{type_desc,Attr,_Content}|T],Acc) ->
    %% We skip any type_desc with the variable attribute
    true = proplists:is_defined(variable, Attr),
    transform(T,Acc);
transform([{type,[],Content}|T],Acc) ->
    transform(T,[{ul,[{class,<<"types">>}],transform(Content,[])}|Acc]);
transform([{v,[],Content}|T],Acc) ->
    transform(T, [{li,[{class,<<"type">>}],transform(Content,[])}|Acc]);
transform([{d,[],Content}|T],Acc) ->
    transform(T, [{li,[{class,<<"description">>}],transform(Content,[])}|Acc]);

transform([Elem = {See,_Attr,_Content}|T],Acc)
  when See =:= seemfa; See =:= seeerl; See =:= seetype; See =:= seeapp;
       See =:= seecom; See =:= seecref; See =:= seefile; See =:= seeguide ->
    transform([transform_see(Elem)|T],Acc);

transform([{term,Attr,[]}|T],Acc) ->
    transform([list_to_binary(proplists:get_value(id,Attr))|T],Acc);

transform([{fsummary,_,_}|T],Acc) ->
    %% We skip fsummary as it many times is just a duplicate of the
    %% first line of the docs.
    transform(T,Acc);

transform([{input,_,Content}|T],Acc) ->
    %% Just remove input as it is not used by anything
    transform(T,[transform(Content,[])|Acc]);

transform([{p,Attr,Content}|T],Acc) ->
    transform(T,[{p,a2b(Attr),transform(Content,[])}|Acc]);
transform([{'div',Attr,Content}|T],Acc) ->
    transform(T,[{'div',a2b(Attr),transform(Content,[])}|Acc]);
transform([{row,Attr,Content}|T],Acc) ->
    transform(T,[{tr,a2b(Attr),transform(Content,[])}|Acc]);
transform([{cell,Attr,Content}|T],Acc) ->
    transform(T,[{td,a2b(Attr),transform(Content,[])}|Acc]);
transform([{tcaption,Attr,Content}|T],Acc) ->
    transform(T,[{caption,a2b(Attr),transform(Content,[])}|Acc]);

transform([{image,Attr,Content}|T],Acc) ->
    transform(T,[{img,a2b(Attr),transform(Content,[])}|Acc]);
transform([{icaption,Attr,Content}|T],Acc) ->
    transform(T,[{caption,a2b(Attr),transform(Content,[])}|Acc]);

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
            transform_types(T, [{li,a2b(Attr),[]}|Acc]);
        false ->
            true = proplists:is_defined(variable, Attr),
            transform_types(T, Acc)
    end;
transform_types([{type_desc,Attr,Content}|T],Acc) ->
    case proplists:is_defined(name,Attr) of
        true ->
            TypeDesc = transform(Content,[]),
            transform_types(T, [{li,a2b(Attr) ++ [{class,<<"description">>}],TypeDesc}|Acc]);
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
        lists:map(fun({tag,_A,_C}=Tag) ->
                          transform_tag(Tag);
                     ({item,A,C}) ->
                          {dd,A,C}
                  end, Content),
    %% io:format("Items: ~p~n",[Items]),
    {dl,Attr,Items}.

transform_tag({tag, Attr0, C}) ->
    Attr1 = lists:map(fun({since,Vsn}) ->
                              {since,
                               unicode:characters_to_binary(Vsn)};
                         (A) ->
                              A
                      end,
                      Attr0),
    {dt,Attr1,C}.


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

transform_see({See,[{marker,Marker}],Content}) ->
    AbsMarker =
        case string:split(Marker, "#") of
            [AppFile] -> marker_defaults(AppFile);
            [AppFile, Anchor] -> [marker_defaults(AppFile), "#", Anchor]
        end,

    {a, [{href,iolist_to_binary(AbsMarker)},
         {rel,<<"https://erlang.org/doc/link/",(atom_to_binary(See))/binary>>}], Content}.

marker_defaults("") ->
    [get(application), ":", filename:rootname(unicode:characters_to_list(get(module)))];
marker_defaults(AppFile) ->
    case string:split(AppFile, ":") of
        [File] -> [get(application), ":", File];
        [App, File] -> [App, ":", File]
    end.

a2b(Attrs) ->
    [{Tag,unicode:characters_to_binary(Value)} || {Tag,Value} <- Attrs].

header() ->
    """
<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->

""".
