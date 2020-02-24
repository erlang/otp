%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(shell_docs).

-include("eep48.hrl").

-export([render/2, render/3, render/4]).
-export([render_type/2, render_type/3, render_type/4]).

%% Used by chunks.escript in erl_docgen
-export([validate/1, normalize/1]).

%% Convinience functions
-export([get_doc/1, get_doc/3, get_type_doc/3]).

-record(config, { docs,
                  io_opts = io:getopts(),
                  io_columns = element(2,io:columns())
                }).

-define(ALL_ELEMENTS,[a,p,h1,h2,h3,i,br,em,pre,code,ul,ol,li,dl,dt,dd]).
%% inline elements are:
-define(INLINE,[i,br,em,code,a]).
-define(IS_INLINE(ELEM),(((ELEM) =:= a) orelse ((ELEM) =:= code)
                         orelse ((ELEM) =:= i) orelse ((ELEM) =:= br)
                         orelse ((ELEM) =:= em))).
%% non-inline elements are:
-define(BLOCK,[p,pre,ul,ol,li,dl,dt,dd,h1,h2,h3]).
-define(IS_BLOCK(ELEM),not ?IS_INLINE(ELEM)).
-define(IS_PRE(ELEM),(((ELEM) =:= pre))).

-type chunk_element_type() :: a | p | i | br | em | pre | code | ul |
                              ol | li | dl | dt | dd.
-type chunk_element_attr() :: {atom(),unicode:chardata()}.
-type chunk_element_attrs() :: [chunk_element_attr()].
-type chunk_element() :: {chunk_element_type(),chunk_element_attrs(),
                          chunk_elements()} | binary().
-type chunk_elements() :: [chunk_element()].
-type docs_v1() :: #docs_v1{}.


-spec validate(Module) -> ok when
      Module :: module() | docs_v1().
%% Simple validation of erlang doc chunk. Check that all tags are supported and
%% that the signature is correct.
validate(Module) when is_atom(Module) ->
    {ok, Doc} = code:get_doc(Module),
    validate(Doc);
validate(#docs_v1{ module_doc = MDocs, docs = AllDocs }) ->

    %% Check some macro in-variants
    AE = lists:sort(?ALL_ELEMENTS),
    AE = lists:sort(?INLINE ++ ?BLOCK),
    true = lists:all(fun(Elem) -> ?IS_INLINE(Elem) end, ?INLINE),
    true = lists:all(fun(Elem) -> ?IS_BLOCK(Elem) end, ?BLOCK),

    _ = maps:map(fun(_Key,MDoc) -> validate(MDoc) end, MDocs),
    lists:map(fun({_,_Anno, Sig, Docs, _Meta}) ->
                      case lists:all(fun erlang:is_binary/1, Sig) of
                          false -> throw({invalid_signature,Sig});
                          true -> ok
                      end,
                      maps:map(fun(_Key,Doc) -> validate(Doc) end, Docs)
              end, AllDocs);
validate([H|T]) when is_tuple(H) ->
    _ = validate(H),
    validate(T);
validate({Tag,Attr,Content}) ->
    case lists:member(Tag,?ALL_ELEMENTS) of
        false ->
            throw({invalid_tag,Tag});
        true ->
            ok
    end,
    true = is_list(Attr),
    validate(Content);
validate([Chars | T]) when is_binary(Chars) ->
    validate(T);
validate([]) ->
    ok.

%% Follows algorithm described here:
%% * https://medium.com/@patrickbrosset/when-does-white-space-matter-in-html-b90e8a7cdd33
%% which in turn follows this:
%% * https://www.w3.org/TR/css-text-3/#white-space-processing
-spec normalize(Docs) -> NormalizedDocs when
      Docs :: chunk_elements(),
      NormalizedDocs :: chunk_elements().
normalize(Docs) ->
    Trimmed = normalize_trim(Docs,true),
    normalize_space(Trimmed).

normalize_trim(Bin,true) when is_binary(Bin) ->
    %% Remove any whitespace (except \n) before or after a newline
    NoSpace = re:replace(Bin,"[^\\S\n]*\n+[^\\S\n]*","\n",[global]),
    %% Replace any tabs with space
    NoTab = re:replace(NoSpace,"\t"," ",[global]),
    %% Replace any newlines with space
    NoNewLine = re:replace(NoTab,"\\v"," ",[global]),
    %% Replace any sequences of \s with a single " "
    re:replace(NoNewLine,"\\s+"," ",[global,{return,binary}]);
normalize_trim(Bin,false) when is_binary(Bin) ->
    Bin;
normalize_trim([{pre,Attr,Content}|T],Trim) ->
    [{pre,Attr,normalize_trim(Content,false)} | normalize_trim(T,Trim)];
normalize_trim([{Tag,Attr,Content}|T],Trim) ->
    [{Tag,Attr,normalize_trim(Content,Trim)} | normalize_trim(T,Trim)];
normalize_trim([<<>>|T],Trim) ->
    normalize_trim(T,Trim);
normalize_trim([B1,B2|T],Trim) when is_binary(B1),is_binary(B2) ->
    normalize_trim([<<B1/binary,B2/binary>> | T],Trim);
normalize_trim([H|T],Trim) ->
    [normalize_trim(H,Trim) | normalize_trim(T,Trim)];
normalize_trim([],_Trim) ->
    [].

%% We want to remove any duplicate spaces, even if they
%% cross into other inline elements.
%% For non-inline elements we just need to make sure that any
%% leading or trailing spaces are stripped.
normalize_space([{Pre,Attr,Content}|T]) when ?IS_PRE(Pre) ->
    [{Pre,Attr,trim_first_and_last(Content,$\n)} | normalize_space(T)];
normalize_space([{Block,Attr,Content}|T]) when ?IS_BLOCK(Block) ->
    [{Block,Attr,trim_first_and_last(trim_inline(Content),$ )} | normalize_space(T)];
normalize_space([E|T]) ->
    [E|normalize_space(T)];
normalize_space([]) ->
    [].

trim_inline(Content) ->
    {NewContent,_} = trim_inline(Content,false),
    NewContent.
trim_inline([Bin|T],false) when is_binary(Bin) ->
    LastElem = binary:at(Bin,byte_size(Bin)-1),
    {NewT, NewState} = trim_inline(T,LastElem =:= $ ),
    {[Bin | NewT],NewState};
trim_inline([<<" ">>|T],true) ->
    trim_inline(T,false);
trim_inline([<<" ",Bin/binary>>|T],true) when is_binary(Bin) ->
    trim_inline([Bin | T],false);
trim_inline([Bin|T],true) when is_binary(Bin) ->
    trim_inline([Bin|T],false);
trim_inline([{Elem,Attr,Content}|T],TrimSpace) when ?IS_INLINE(Elem) ->
    {NewContent,ContentTrimSpace} = trim_inline(Content,TrimSpace),
    {NewT,TTrimSpace} = trim_inline(T,ContentTrimSpace),
    {[{Elem,Attr,NewContent} | NewT], TTrimSpace};
trim_inline([{Elem1,_A1,_C1} = B1,<<" ">>,{Elem2,_A2,_C2} = B2|T],TrimSpace)
  when ?IS_BLOCK(Elem1),?IS_BLOCK(Elem2) ->
    trim_inline([B1,B2|T],TrimSpace);
trim_inline([{Elem,_Attr,_Content} = Block|T],_TrimSpace) when ?IS_BLOCK(Elem) ->
    [NewBlock] = normalize_space([Block]),
    {NewT,TTrimSpace} = trim_inline(T,false),
    {[NewBlock | NewT], TTrimSpace};
trim_inline([],TrimSpace) ->
    {[],TrimSpace}.


%% This function removes the first and last What from the content.
%% This is complicated by the fact that the first or last element
%% may not have any binary, or have the binary deeply nested within.
trim_first_and_last(Content, What) when What < 256 ->
    {NewContent,_State} = trim_last(trim_first(Content,What),What),
    NewContent.

trim_first(Content,What) ->
    {NewContent,_State} = trim_first(Content,false,What),
    NewContent.
trim_first([Bin|T],false,What) when is_binary(Bin) ->
    case Bin of
        <<What>> ->
            {T,true};
        <<What,NewBin/binary>> ->
            {[NewBin|T],true};
        Bin ->
            {[Bin|T],true}
    end;
trim_first([{Elem,Attr,Content} = Tag|T],false,What) ->
    case trim_first(Content,false,What) of
        {NewContent,true} ->
            {[{Elem,Attr,NewContent}|T],true};
        {Content,false} ->
            {NewT,NewState} = trim_first(T,false,What),
            {[Tag | NewT],NewState}
    end;
trim_first([],false,_What) ->
    {[],false}.

trim_last([Bin | T],What) when is_binary(Bin) ->
    case trim_last(T,What) of
        {NewT,true} ->
            {[Bin | NewT],true};
        {T,false} ->
            PreSz = byte_size(Bin)-1,
            case Bin of
                <<What>> -> {T,true};
                <<NewBin:PreSz/binary,What>> ->
                    {[NewBin|T],true};
                Bin ->
                    {[Bin|T],true}
            end
    end;
trim_last([{Elem,Attr,Content} = Tag|T],What) ->
    case trim_last(T,What) of
        {NewT,true} ->
            {[Tag | NewT],true};
        {T,false} ->
            {NewContent,NewState} = trim_last(Content,What),
            {[{Elem,Attr,NewContent}|T],NewState}
    end;
trim_last([],_What) ->
    {[],false}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API function for dealing with the function documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_doc(Module :: module()) -> chunk_elements().
get_doc(Module) ->
    {ok, #docs_v1{ module_doc = ModuleDoc } } = code:get_doc(Module),
    get_local_doc(Module, ModuleDoc).

-spec get_doc(Module :: module(), Function, Arity) ->
          [{{Function,Arity}, Anno, Signature, chunk_elements(), Metadata}] when
      Function :: function(),
      Arity :: arity(),
      Anno :: erl_anno:anno(),
      Signature :: [binary()],
      Metadata :: #{}.
get_doc(Module, Function, Arity) ->
    {ok, #docs_v1{ docs = Docs } } = code:get_doc(Module),
    FnFunctions =
        lists:filter(fun({{function, F, A},_Anno,_Sig,_Doc,_Meta}) ->
                             F =:= Function andalso A =:= Arity;
                        (_) ->
                             false
                     end, Docs),

    [{F,A,S,get_local_doc({F,A},D),M} || {F,A,S,D,M} <- FnFunctions].

-spec render(Module :: module(), Docs :: docs_v1()) -> unicode:chardata().
render(Module, #docs_v1{ module_doc = ModuleDoc, metadata = MD } = D) ->
    render_docs([["\t",atom_to_binary(Module)]],
                get_local_doc(Module, ModuleDoc), MD, D).

-spec render(Module :: module(), Function :: function(), Docs :: docs_v1()) ->
          unicode:chardata() | {error,function_missing}.
render(_Module, Function, #docs_v1{ docs = Docs } = D) ->
    render_function(
        lists:filter(fun({{function, F, _},_Anno,_Sig,_Doc,_Meta}) ->
                             F =:= Function;
                        (_) ->
                             false
                     end, Docs), D).
-spec render(Module :: module(), Function :: function(), Arity :: arity(),
             Docs :: docs_v1()) -> unicode:chardata() | {error,function_missing}.
render(_Module, Function, Arity, #docs_v1{ docs = Docs } = D) ->
    render_function(
      lists:filter(fun({{function, F, A},_Anno,_Sig,_Doc,_Meta}) ->
                           F =:= Function andalso A =:= Arity;
                        (_) ->
                             false
                   end, Docs), D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API function for dealing with the type documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_type_doc(Module :: module(), Type :: atom(), Arity :: arity()) ->
          [{{Type,Arity}, Anno, Signature, chunk_elements(), Metadata}] when
      Type :: atom(),
      Arity :: arity(),
      Anno :: erl_anno:anno(),
      Signature :: [binary()],
      Metadata :: #{}.
get_type_doc(Module, Type, Arity) ->
    {ok, #docs_v1{ docs = Docs } } = code:get_doc(Module),
    FnFunctions =
        lists:filter(fun({{type, T, A},_Anno,_Sig,_Doc,_Meta}) ->
                             T =:= Type andalso A =:= Arity;
                        (_) ->
                             false
                     end, Docs),
    [{F,A,S,get_local_doc(F, D),M} || {F,A,S,D,M} <- FnFunctions].

-spec render_type(Module :: module(), Docs :: docs_v1()) -> unicode:chardata().
render_type(Module, #docs_v1{ docs = Docs } = D) ->
    render_type_signatures(Module,
      lists:filter(fun({{type, _, _},_Anno,_Sig,_Doc,_Meta}) ->
                             true;
                        (_) ->
                             false
                     end, Docs), D).

-spec render_type(Module :: module(), Type :: atom(), Docs :: docs_v1()) ->
          unicode:chardata() | {error,type_missing}.
render_type(_Module, Type, #docs_v1{ docs = Docs } = D) ->
    render_type_docs(
      lists:filter(fun({{type, T, _},_Anno,_Sig,_Doc,_Meta}) ->
                             T =:= Type;
                        (_) ->
                             false
                     end, Docs), D).

-spec render_type(Module :: module(), Type :: atom(), Arity :: arity(),
                  Docs :: docs_v1()) -> unicode:chardata() | {error,type_missing}.
render_type(_Module, Type, Arity, #docs_v1{ docs = Docs } = D) ->
    render_type_docs(
      lists:filter(fun({{type, T, A},_Anno,_Sig,_Doc,_Meta}) ->
                           T =:= Type andalso A =:= Arity;
                        (_) ->
                             false
                   end, Docs), D).


%% Get the docs in the correct locale if it exists.
get_local_doc(MissingMod, Docs) when is_atom(MissingMod) ->
    get_local_doc(atom_to_binary(MissingMod), Docs);
get_local_doc({F,A}, Docs) ->
    get_local_doc(unicode:characters_to_binary(io_lib:format("~tp/~p",[F,A])), Docs);
get_local_doc(_Missing, #{ <<"en">> := Docs }) ->
    %% English if it exists
    normalize(Docs);
get_local_doc(_Missing, ModuleDoc) when map_size(ModuleDoc) > 0 ->
    %% Otherwise take first alternative found
    normalize(maps:get(hd(maps:keys(ModuleDoc)), ModuleDoc));
get_local_doc(Missing, hidden) ->
    [{p,[],[<<"The documentation for ">>,Missing,
            <<" is hidden. This probably means that it is internal "
              "and not to be used by other applications.">>]}];
get_local_doc(Missing, None) when None =:= none; None =:= #{} ->
    [{p,[],[<<"There is no documentation for ">>,Missing]}].

%%% Functions for rendering reference documentation
render_function([], _D) ->
    {error,function_missing};
render_function(FDocs, D) ->
    [render_docs(render_signature(Func), get_local_doc({F,A},Doc), Meta, D)
     || {{_,F,A},_Anno,_Sig,Doc,Meta} = Func <- lists:sort(FDocs)].

%% Render the signature of either function or a type, or anything else really.
render_signature({{_Type,_F,_A},_Anno,_Sig,_Docs,#{ signature := Specs }}) ->
    [erl_pp:attribute(Spec,[{encoding,utf8}]) || Spec <- Specs];
render_signature({{_Type,_F,_A},_Anno,Sigs,_Docs,_Meta}) ->
    [Sig || Sig <- Sigs].

render_since(#{ since := Vsn }) ->
    ["\n\nSince: ",Vsn];
render_since(_) ->
    [].

render_docs(Headers, DocContents, MD, D = #config{}) ->
    init_ansi(D),
    try
        {Doc,_} = render_docs(DocContents,[],0,2,D),
        [sansi(bold),
         [io_lib:format("~n~ts",[Header]) || Header <- Headers],
         ransi(bold),
         render_since(MD),
         io_lib:format("~n~n~ts",[Doc])]
    after
        clean_ansi()
    end;
render_docs(Headers, DocContents, MD, D) ->
    render_docs(Headers, DocContents, MD, #config{ docs = D }).

%%% Functions for rendering type documentation
render_type_signatures(Module, Types, D = #config{}) ->
    init_ansi(D),
    try
        [sansi(bold),"\t",atom_to_list(Module),ransi(bold),"\n\n",
         [render_signature(Type) || Type <- Types ]]
    after
        clean_ansi()
    end;
render_type_signatures(Module, Types, D) ->
    render_type_signatures(Module, Types, #config{ docs = D }).

render_type_docs([], _D) ->
    {error,type_missing};
render_type_docs(Types, #config{} = D) when is_list(Types) ->
    [render_type_docs(Type, D) || Type <- Types];
render_type_docs({{_,F,A},_,_Sig,Docs,Meta} = Type, #config{} = D) ->
    render_docs(render_signature(Type), get_local_doc({F,A},Docs), Meta, D);
render_type_docs(Docs, D) ->
    render_type_docs(Docs, #config{ docs = D }).

%%% General rendering functions
render_docs(Elems,State,Pos,Ind,D) when is_list(Elems) ->
    lists:mapfoldl(fun(Elem,P) ->
%                           io:format("Elem: ~p (~p) (~p,~p)~n",[Elem,State,P,Ind]),
                           render_docs(Elem,State,P,Ind,D)
                   end,Pos,Elems);
render_docs(Elem,State,Pos,Ind,D) ->
    render_element(Elem,State,Pos,Ind,D).


%%% The function is the main element rendering function
%%%
%%% Elem: The current element to process
%%% Stack: A stack of element names to see where we are in the dom
%%% Pos: The current print position on the current line
%%% Ind: How much the text should be indented after a newline
%%% Config: The renderer's configuration
%%%
%%% Each element is responsible for putting new lines AFTER itself
%%% The indents are done either by render_words when a newline happens
%%% or when a new element is to be rendered and Pos < Ind.
%%%
%%% Any block elements (i.e. p, ul, li etc) are responsible for trimming
%%% extra new lines. eg. <ul><li><p>content</p></li></ul> should only
%%% have two newlines at the end.
-spec render_element(Elem :: chunk_element(),
                     Stack :: [chunk_element_type()],
                     Pos :: non_neg_integer(),
                     Indent :: non_neg_integer(),
                     Config :: #config{}) ->
          {unicode:chardata(), Pos :: non_neg_integer()}.

render_element({IgnoreMe,_,Content}, State, Pos, Ind,D)
  when IgnoreMe =:= a; IgnoreMe =:= anno ->
    render_docs(Content, State, Pos, Ind,D);

%% Catch h1, h2 and h3 before the padding is done as there reset padding
render_element({h1,_,Content},State,0 = Pos,_Ind,D) ->
    trimnlnl(render_element({code,[],[{em,[],Content}]}, State, Pos, 0, D));
render_element({h2,_,Content},State,0 = Pos,_Ind,D) ->
    trimnlnl(render_element({em,[],Content}, State, Pos, 0, D));
render_element({h3,_,Content},State,Pos,_Ind,D) when Pos =< 2 ->
    trimnlnl(render_element({code,[],Content}, State, Pos, 2, D));

render_element({p,_Attr,_Content} = E,State,Pos,Ind,D) when Pos > Ind ->
    {Docs,NewPos} = render_element(E,State,0,Ind,D),
    {["\n",Docs],NewPos};
render_element({p,[{class,What}],Content},State,Pos,Ind,D) ->
    {Docs,_} = render_docs(Content, [p|State], 0, Ind+2, D),
    trimnlnl([pad(Ind - Pos),string:titlecase(What),":\n",Docs]);
render_element({p,_,Content},State,Pos,Ind,D) ->
    trimnlnl(render_docs(Content, [p|State], Pos, Ind,D));

render_element(Elem,State,Pos,Ind,D) when Pos < Ind ->
%    io:format("Pad: ~p~n",[Ind - Pos]),
    {Docs,NewPos} = render_element(Elem,State,Ind,Ind,D),

    {[pad(Ind - Pos), Docs],NewPos};

render_element({code,_,Content},[pre|_]  = State,Pos,Ind,D) ->
    %% When code is within a pre we don't emit any underline
    render_docs(Content, [code|State], Pos, Ind,D);
render_element({code,_,Content},State,Pos,Ind,D) ->
    Underline = sansi(underline),
    {Docs, NewPos} = render_docs(Content, [code|State], Pos, Ind,D),
    {[Underline,Docs,ransi(underline)], NewPos};

render_element({i,_,Content},State,Pos,Ind,D) ->
    %% Just ignore i as ansi does not have cursive style
    render_docs(Content, State, Pos, Ind,D);

render_element({br,[],[]},_State,_Pos,_Ind,_D) ->
    nl("");

render_element({em,_,Content},State,Pos,Ind,D) ->
    Bold = sansi(bold),
    {Docs, NewPos} = render_docs(Content, State, Pos, Ind,D),
    {[Bold,Docs,ransi(bold)], NewPos};

render_element({pre,_,Content},State,Pos,Ind,D) ->
    %% For pre we make sure to respect the newlines in pre
    trimnlnl(render_docs(Content, [pre|State], Pos, Ind+2, D));

render_element({ul,[{class,"types"}],Content},State,_Pos,Ind,D) ->
    {Docs, _} = render_docs(Content, [types|State], 0, Ind+2, D),
    trimnlnl(["Types:\n", Docs]);
render_element({li,Attr,Content},[types|_] = State,Pos,Ind,C) ->
    Doc =
        case {proplists:get_value(name, Attr),proplists:get_value(class, Attr)} of
            {undefined,Class} when Class =:= undefined; Class =:= "type" ->
                %% Inline html for types
                render_docs(Content,[type|State],Pos,Ind,C);
            {_,"description"} ->
                %% Inline html for type descriptions
                render_docs(Content,[type|State],Pos,Ind+2,C);
            {Name,_} ->
                %% Try to render from type metadata
                case render_type_signature(list_to_atom(Name),C) of
                    undefined when Content =:= [] ->
                        %% Failed and no content, emit place-holder
                        {["-type ",Name,"() :: term()."],0};
                    undefined ->
                        %% Failed with metadata, render the content
                        render_docs(Content,[type|State],Pos,Ind,C);
                    Type ->
                        %% Emit the erl_pp typespec
                        {Type,0}
                end
        end,
    trimnl(Doc);
render_element({ul,[],Content},State,Pos,Ind,D) ->
    render_docs(Content, [l|State], Pos, Ind,D);
render_element({ol,[],Content},State,Pos,Ind,D) ->
    %% For now ul and ol does the same thing
    render_docs(Content, [l|State], Pos, Ind,D);
render_element({li,[],Content},[l | _] = State, Pos, Ind,D) ->
    Bullet = get_bullet(State, proplists:get_value(encoding, D#config.io_opts)),
    BulletLen = string:length(Bullet),
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + BulletLen,Ind + BulletLen, D),
    trimnlnl([Bullet,Docs]);

render_element({dl,_,Content},State,Pos,Ind,D) ->
    render_docs(Content, [dl|State], Pos, Ind,D);
render_element({dt,_,Content},[dl | _] = State,Pos,Ind,D) ->
    Underline = sansi(underline),
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos, Ind, D),
    {[Underline,Docs,ransi(underline),":","\n"], 0};
render_element({dd,_,Content},[dl | _] = State,Pos,Ind,D) ->
    trimnlnl(render_docs(Content, [li | State], Pos, Ind + 2, D));

render_element(B, State, Pos, Ind,#config{ io_columns = Cols }) when is_binary(B) ->
    case lists:member(pre,State) of
        true ->
            Pre = string:replace(B,"\n",["\n",pad(Ind)],all),
            {Pre, Pos + lastline(Pre)};
        _ ->
            render_words(split_to_words(B),State,Pos,Ind,[[]],Cols)
    end;

render_element({Tag,Attr,Content}, State, Pos, Ind,D) ->
    throw({unhandled,{Tag,Attr,Content,Pos,Ind}}),
    render_docs(Content, State, Pos, Ind,D).

render_words(Words,[_,types|State],Pos,Ind,Acc,Cols) ->
    %% When we render words and are in the types->type state we indent
    %% the extra lines two additional spaces to make it look nice
    render_words(Words,State,Pos,Ind+2,Acc,Cols);
render_words([Word|T],State,Pos,Ind,Acc,Cols) when is_binary(Word) ->
    WordLength = string:length(Word),
    NewPos = WordLength + Pos,
    if
        NewPos > (Cols - 10 - Ind) ->
            %% Word does not fit, time to add a newline and also pad to Indent level
            render_words(T,State,WordLength+Ind+1,Ind,[[[pad(Ind), Word]]|Acc],Cols);
         true ->
            %% Word does fit on line
            [Line | LineAcc] = Acc,
            %% Add + 1 to length for space
            NewPosSpc = NewPos+1,
            render_words(T,State,NewPosSpc,Ind,[[Word|Line]|LineAcc],Cols)
    end;
render_words([],_State,Pos,_Ind,Acc,_Cols) ->
    Lines = lists:join(
              $\n,lists:map(fun(RevLine) ->
                                    Line = lists:reverse(RevLine),
                                    lists:join($ ,Line)
                            end,lists:reverse(Acc))),
    {iolist_to_binary(Lines), Pos}.

render_type_signature(Name, #config{ docs = #docs_v1{ metadata = #{ types := AllTypes }}}) ->
    case [Type || Type = {TName,_} <- maps:keys(AllTypes), TName =:= Name] of
        [] ->
            undefined;
        Types ->
            [erl_pp:attribute(maps:get(Type, AllTypes)) || Type <- Types]
    end.

%% Pad N spaces, disabling any ansi formatting while doing so
pad(N) ->
    Pad = lists:duplicate(N," "),
    case ansi() of
        undefined ->
            Pad;
        Ansi ->
            ["\033[0m",Pad,Ansi]
    end.

get_bullet(_State,latin1) ->
    <<" * ">>;
get_bullet(State,unicode) ->
    %% Fancy bullet point logic!
    lists:nth(length([l || l <- State]),
              [<<" • "/utf8>>,<<" ￮ "/utf8>>,
               <<" ◼ "/utf8>>,<<" ◻ "/utf8>>]).

% Look for the length of the last line of a string
lastline(Str) ->
    LastStr = case string:find(Str,"\n",trailing) of
                  nomatch ->
                      Str;
                  Match ->
                      tl(string:next_codepoint(Match))
              end,
    string:length(LastStr).

split_to_words(B) ->
    binary:split(B,[<<" ">>],[global]).

%% These functions make sure that we trim extra newlines added
%% by the renderer. For example if we do <li><p></p></li>
%% that would add 4 \n at after the last </li>. This is trimmed
%% here to only be 2 \n
trimnlnl({Chars, _Pos}) ->
    nl(nl(string:trim(Chars, trailing, "\n")));
trimnlnl(Chars) ->
    nl(nl(string:trim(Chars, trailing, "\n"))).
trimnl({Chars, _Pos}) ->
    nl(string:trim(Chars, trailing, "\n")).
nl({Chars, _Pos}) ->
    nl(Chars);
nl(Chars) ->
    {[Chars,"\n"],0}.

%% We keep the current ansi state in the pdict so that we know
%% what to disable and enable when doing padding
init_ansi(#config{ io_opts = Opts }) ->
    %% We use this as our heuristic to see if we should print ansi or not
    case {application:get_env(kernel, shell_docs_ansi),
          proplists:is_defined(echo, Opts) andalso
          proplists:is_defined(expand_fun, Opts),
          os:type()} of
        {{ok,false}, _, _} ->
            put(ansi, noansi);
        {{ok,true}, _, _} ->
            put(ansi, []);
        {_, _, {win32,_}} ->
            put(ansi, noansi);
        {_, true,_} ->
            put(ansi, []);
        {_, false,_} ->
            put(ansi, noansi)
    end.

clean_ansi() ->
    case get(ansi) of
        [] -> erase(ansi);
        noansi -> erase(ansi)
    end,
    ok.

%% Set ansi
sansi(Type) -> sansi(Type, get(ansi)).
sansi(_Type, noansi) ->
    [];
sansi(Type, Curr) ->
    put(ansi,[Type | Curr]),
    ansi(get(ansi)).

%% Clear ansi
ransi(Type) -> ransi(Type, get(ansi)).
ransi(_Type, noansi) ->
    [];
ransi(Type, Curr) ->
    put(ansi,proplists:delete(Type,Curr)),
    case ansi(get(ansi)) of
        undefined ->
            "\033[0m";
        Ansi ->
            Ansi
    end.

ansi() -> ansi(get(ansi)).
ansi(noansi) -> undefined;
ansi(Curr) ->
    case lists:usort(Curr) of
        [] ->
            undefined;
        [bold] ->
            "\033[;1m";
        [underline] ->
            "\033[;;4m";
        [bold,underline] ->
            "\033[;1;4m"
    end.
