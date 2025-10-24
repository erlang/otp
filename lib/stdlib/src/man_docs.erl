%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
-module(man_docs).
-moduledoc false.

-include_lib("kernel/include/eep48.hrl").

-export([module_to_manpage/3, module_to_manpage/4, markdown_to_manpage/3]).

%% Formats a module documentation as a roff man page.
%% Fetches the documentation for a module with `code:get_doc/1`
-spec module_to_manpage(Module, Path, Section) -> unicode:chardata() when
        Module :: module(),
        Path :: string(),
        Section :: string().
module_to_manpage(Module, Path, Section) when is_atom(Module) ->
    case code:get_doc(Module) of
        {ok, Docs} ->
                module_to_manpage(Module, Path, Docs, Section);
        _Error ->
            ~""
    end.
-spec module_to_manpage(Module, Path, Docs, Section) -> unicode:chardata() when
        Module :: module(),
        Path :: string(),
        Docs :: #docs_v1{},
        Section :: string().
module_to_manpage(_Module, _Path, #docs_v1{module_doc = None}, _Section) when None =:= none; None =:= hidden ->
    ~"";
module_to_manpage(Module, Path, #docs_v1{module_doc = #{~"en" := ModuleDoc}, docs = AllDocs}, Section)
    when is_atom(Module) ->
    PreludeNDescription = if is_binary(ModuleDoc) -> markdown_to_manpage(ModuleDoc, Path, Section);
        true -> markdown_to_manpage1(ModuleDoc, Path, Section)
    end,

    Types = [Doc || {{type,_,_},_,_,_,_}=Doc <- AllDocs],
    TypesSection = format_section("DATA TYPES", Types, Module, AllDocs),
    Callbacks = [Doc || {{callback,_,_},_,_,_,_}=Doc <- AllDocs],
    CallbacksSection = format_section("CALLBACKS", Callbacks, Module, AllDocs),
    Functions = [Doc || {{function,_,_},_,_,_,_}=Doc <- AllDocs],
    FunctionsSection = format_section("FUNCTIONS", Functions, Module, AllDocs),

    iolist_to_binary([PreludeNDescription, TypesSection, FunctionsSection, CallbacksSection]).

%% Formats markdown as a roff man page.
-spec markdown_to_manpage(binary(), file:filename(), string()) -> binary().
markdown_to_manpage(Markdown, Path, Section) ->
        markdown_to_manpage1(shell_docs_markdown:parse_md(Markdown), Path, Section).
markdown_to_manpage1(MarkdownChunks, Path, Section) ->
    Path1 = filename:absname(Path),
    App = case filename:split(string:prefix(Path1, os:getenv("ERL_TOP"))) of
        ["/", "lib", AppStr | _] ->
            list_to_atom(AppStr);
        ["lib", AppStr | _] ->
            list_to_atom(AppStr);
        ["/", "erts" | _] ->
            list_to_atom("erts");
        ["nomatch"] ->
            error("ERL_TOP environment variable doesn't match the PATH " ++ Path)
    end,
    Version = case application:load(App) of
        ok -> {ok,Vsn} = application:get_key(App, vsn), Vsn;
        {error, {"no such file or directory","erts.app"}} -> erlang:system_info(version);
        {error, {already_loaded, App}} -> {ok,Vsn} = application:get_key(App, vsn), Vsn
    end,

    Extension = filename:extension(Path),
    FileName = list_to_binary(filename:rootname(filename:basename(Path), Extension)),
    Name = get_name(MarkdownChunks, FileName),
    Prelude = io_lib:format(".TH ~s ~s \"~s ~s\" \"Ericsson AB\" \"Erlang Module Definition\"\n",
                            [Name, Section, atom_to_binary(App), Version]),
    I = conv(MarkdownChunks, Name),
    iolist_to_binary([Prelude|I]).

get_name([{h1,_,[Name]}|_], _) when is_binary(Name) ->
    Name;
get_name(_, Default) when is_binary(Default) ->
    Default.

conv([{h1,_,[Name]},
      {p,_,[ShortDesc0]},
      {h2,_,[~"Synopsis"]},
      Synopsis0,
      {h2,_,[~"Description"]}|T],_) when is_binary(ShortDesc0) ->
    ShortDesc = string:trim(ShortDesc0, trailing, [$., $\s]),
    Synopsis = strip_formatting(Synopsis0),
    [~".SH NAME\n",
     Name,~B" \- ",ShortDesc,$\n,
     ~".SH SYNOPSIS\n",
     Synopsis,$\n,
     ~".SH DESCRIPTION\n"|format(T)];
conv([{h1,_,[Name]},
      {p,_,[ShortDesc0]},
      {h2,_,[~"Description"]}|T],_) when is_binary(ShortDesc0) ->
    ShortDesc = string:trim(ShortDesc0, trailing, [$., $\s]),
    [~".SH NAME\n",
     Name,~B" \- ",ShortDesc,$\n,
     ~".SH DESCRIPTION\n"|format(T)];
conv([{h1,_,[Head]}|T],_) ->
    Name = ~".SH NAME\n",
    Desc = ~".SH DESCRIPTION\n",
    [Name,Head,$\n,Desc|format(T)];
conv([{p,_,_}=ShortDesc0|T], Head) ->
    Name = ~".SH NAME\n",
    Desc = ~".SH DESCRIPTION\n",
    [~".PP\n"|ShortDesc] = format_one(ShortDesc0),
    [Name,Head,~B" \- ",ShortDesc,$\n,Desc|format(T)].

escape(Text) when is_list(Text) ->
    escape(iolist_to_binary(Text));
escape(Text) when is_binary(Text) ->
    binary:replace(Text, <<$\\>>, ~"\\\\", [global]).

format(Is) ->
    [[format_one(I),$\n] || I <- Is].

format_one({blockquote,_,[{h4,_,Head0},{p,_,Text}|Ps]}) ->
    Head = string:uppercase(<<(string:trim(Head0))/binary,": ">>),
    L = [{p,[],[{em,[],Head}|Text]}|Ps],
    format(L);
format_one({blockquote,_,Qs0}) ->
    [".RS 4\n", format(Qs0), ".RE\n"];
format_one({h1,_,Hs}) ->
    [~'.SH "',Hs,~'"\n'];
format_one({h2,_,Hs}) ->
    [~'.SS "',Hs,~'"\n'];
format_one({h3,item,H}) ->
    [~"\\fB",format_p_item(H),"\\fR"];
format_one({h3,_,[Hs]}) when is_binary(Hs) ->
    [~'.PP\n\\fB',Hs,~'\\fR\n'];
format_one({h3,_,Hs}) when is_list(Hs) ->
    [~'.PP\n',[format_one({h3,item,Hi})||Hi<-Hs],~"\n"];
format_one({h4,_,Hs}) ->
    format_one({h3,[],Hs});
format_one({h5,_,Hs}) ->
    format_one({h3,[],Hs});
format_one({p,_,Ps}) ->
    format_p(Ps);
format_one({pre,_,Ps}) ->
    format_pre(Ps);
format_one({ol,_,Ol}) ->
    format_ol(Ol);
format_one({ul,_,Ul}) ->
    format_ul(Ul);
format_one({a,_,[{code,_,Text}]}) ->
    [~B"\fI",format_p_item(Text),~B"\fR"];
format_one({a,_,Text}) ->
    [~B"\fI",format_p_item(Text),~B"\fR"];
format_one({code,_,Text}) ->
    [~B"\fI",format_p_item(Text),~B"\fR"];
format_one({strong,_,Text}) ->
    [~B"\fB", format_p_item(Text), ~B"\fR"];
format_one({em,_,Text}) ->
    [~B"\fB",format_p_item(Text),~B"\fR"];
format_one({i,_,Text}) ->
    [~B"\fI",format_p_item(Text),~B"\fR"];
format_one({dl,_,Content}) ->
    format_dl(Content);
format_one([Text]) when is_binary(Text) ->
    format_one({p,[],[Text]});
format_one(Text) when is_binary(Text) ->
    format_one({p,[],Text}).

format_dl(Is) ->
    [~".RS 4\n", [format_dl_item(I) || I <- Is], ~".RE\n"].
format_dl_item({dt,_,Content}) ->
    [~".TP\n", "\\fB", format_p_item(Content), "\\fR", $\n];
format_dl_item({dd,_,Content}) ->
    [format_dd_item(Content), $\n].
format_dd_item([{ul,_,_}=UL|Rest]) ->
    [format([UL]), format_dd_item(Rest)];
format_dd_item([{p,_,Content}|Rest]) ->
    [format_p(Content)|format_dd_item(Rest)];
format_dd_item([TextItem|Rest]) ->
    [format_p_item(TextItem),format_dd_item(Rest)];
format_dd_item([]) -> [].
format_p(Text) when is_binary(Text) ->
    format_p([Text]);
format_p(Is0) ->
    Text0 = iolist_to_binary([format_p_item(I) || I <- Is0]),
    Text = string:trim(Text0, leading),
    [~".PP\n",Text,$\n].

format_p_item({Fi,_,Text}) when Fi =:= code; Fi =:= i; Fi =:= a ->
    [~B"\fI",format_p_item(Text),~B"\fR"];
format_p_item({Fb,_,Text}) when Fb =:= em; Fb =:= strong ->
    [~B"\fB",format_p_item(Text),~B"\fR"];
format_p_item([H|T]) ->
    [format_p_item(H)|format_p_item(T)];
format_p_item([]) ->
    [];
format_p_item(Text) when is_binary(Text) ->
    escape_backslashes(Text).

format_pre(Ps0) ->
    Ps = [format_pre_item(P) || P <- Ps0],
    [~".IP\n.nf\n",Ps,$\n,~".fi\n"].

format_pre_item({code,[{class,<<"table">>}],Text}) ->
    to_tbl(parse(extract(iolist_to_binary(Text))));
format_pre_item({code,_,Text}) ->
    escape_backslashes(Text);
format_pre_item(Text) ->
    escape_backslashes(Text).

format_ol(OL) ->
    [".nr li 0 1\n"|[format_ol_item(LI) || LI <- OL]].

format_ol_item({li,_,Ps0}) ->
    B = ~"""
        .sp
        .RS 4
        .ie n \{\
        \h'-04'\n+[li].\h'+03'\c
        .\}
        .el \{\
        .sp -1
        .IP "\n+[li]." 4
        .\}
        """,
    [B,format(Ps0),~".RE\n"].

format_ul(UL) ->
    [format_ul_item(I) || I <- UL].

format_ul_item({li,_,Ps0}) ->
    case Ps0 of
        [{p,_,[Head,<<" - ",Text0/binary>>|Items]}|T] ->
            Text = string:trim(Text0, leading),
            [strip_formatting(Head),$\n,
             ~".RS 2\n",
             Text,$\n,
             [format_p_item(I) || I <- Items],
             $\n,
             format(T),$\n,
             ~".RE\n"
            ];
        _ ->
            B = ~"""

                 .sp
                 .RS 4
                 .ie n \{\
                 \h'-04'\(bu\h'+03'\c
                 .\}
                 .el \{\
                 .sp -1
                 .IP \(bu 2.3
                 .\}
                 """,
            case Ps0 of
                [Text|_] when is_binary(Text);
                              element(1,Text) =:= code;
                              element(1,Text) =:= a ->
                    [B,format_p(Ps0),~".RE\n"];
                _ -> [B,format(Ps0),~".RE\n"]
            end
    end.

strip_formatting({_,_,[_|_]=L}) ->
    strip_formatting(L);
strip_formatting([H|T]) ->
    [strip_formatting(H),strip_formatting(T)];
strip_formatting([]) ->
    [];
strip_formatting(<<".",_/binary>> = Bin) ->
    [~B"\&",Bin];
strip_formatting(Bin) when is_binary(Bin) ->
    Bin.

escape_backslashes(Text) when is_list(Text) ->
    escape_backslashes(iolist_to_binary(Text));
escape_backslashes(Text) when is_binary(Text) ->
    binary:replace(Text, ~B"\", ~B"\\", [global]).

%% Extracts all Markdown tables from a binary text.
%% Returns a list of binaries, where each binary is a complete table.
extract(Text) ->
    %% This regex finds the header, the separator line, and all data rows.
    Regex = "^\\s*\\|.+\\|\\s*\\n\\s*\\|[-:|\\s]+\\|\\s*\\n(?:\\s*\\|.+\\|\\s*\\n?)+",
    
    %% Scan for all occurrences and extract the matched strings.
    case re:run(Text, Regex, [{capture, all, binary}, global, multiline]) of
        {match, [[Captured]|_]} -> Captured;
        nomatch -> []
    end.

%% Parses a single Markdown table string into a list of rows (list of cells).
%% It automatically discodes the separator line.
parse([]) -> [];
parse(TableString) ->
    Lines = binary:split(TableString, <<"\n">>, [global, trim]),
    
    %% Map over each line, parsing it into a list of cells.
    ParsedRows = [parse_row(Line) || Line <- Lines],
    
    %% The second line is the separator (|---|-|); we remove it.
    [Header | DataRows] = ParsedRows,
    [Header | tl(DataRows)].

%% Converts a parsed table (list of lists of binaries) into tbl format.
to_tbl([]) -> <<>>;
to_tbl([[] | _]) -> <<>>;
to_tbl([Header | _] = ParsedData) ->
    NumCols = length(Header),
    
    % 1. Define global options for the table.
    Options = <<"allbox;">>,

    % 2. Generate format lines. We'll default to a centered (c) header
    %    and left-aligned (l) data columns.
    HeaderFmt = iolist_to_binary(lists:join(<<" ">>, lists:duplicate(NumCols, <<"c">>))),
    DataFmt = iolist_to_binary([lists:join(<<" ">>, lists:duplicate(NumCols, <<"l">>)), <<".">>]),

    % 3. Convert each row of cells into a single tab-separated binary.
    FormattedRows = [iolist_to_binary(lists:join(<<"\t">>, Row)) || Row <- ParsedData],
    
    % 4. Assemble all parts into the final tbl block.
    Parts = [
        <<".TS\n">>,
        Options, <<"\n">>,
        HeaderFmt, <<"\n">>,
        DataFmt, <<"\n">>,
        iolist_to_binary(lists:join(<<"\n">>, FormattedRows)),
        <<"\n.TE">>
    ],
    iolist_to_binary(Parts).

%% Parses a single row string into a list of binary cells.
parse_row(Line) ->
    %% 1. Remove surrounding whitespace from the line.
    TrimmedLine = string:trim(Line),
    
    %% 2. Remove the leading and trailing pipes.
    NoOuterPipes = strip_outer_pipes(TrimmedLine),
    
    %% 3. Split the row by the pipe delimiter.
    Cells = binary:split(NoOuterPipes, <<"|">>, [global]),
    
    %% 4. Trim whitespace from each individual cell.
    [format_cell(string:trim(Cell)) || Cell <- Cells].

format_cell(B) ->
    re:replace(B,"`(.+)`",<<"\\\\fI\\g{1}\\\\fR">>, [{return, binary},global]).

%% Helper to safely remove the first and last characters if they are pipes.
strip_outer_pipes(Bin) ->
    string:trim(Bin, both, "|").


format_section(Title, Docs, Module, AllDocs) ->
    case Docs of
        [] -> [];
        _ ->
            SortedDocs = lists:sort(fun({MFA1,_,_,_,_}, {MFA2,_,_,_,_}) -> MFA1 =< MFA2 end, Docs),
            [".SH ", Title, "\n.LP\n" |
                lists:flatmap(fun(Doc) ->
                            format_function([Doc], Module, AllDocs)
                            end, SortedDocs)]
    end.
    
format_function(FDocs, Module, AllDocs) ->
    GlobalSpecs = shell_docs:extract_type_specs(Module),
    Grouping = %% Groups functions using the 'equiv' meta attribute
        lists:foldr(
            fun({_Group,_Anno,_Sig,_Doc,#{ equiv := Group }} = Func, Acc) ->
                    case lists:keytake(Group, 1, Acc) of
                        false -> [{Group, [Func], format_signature(Func, GlobalSpecs)} | Acc];
                        {value, {Group, Members, Sigs}, NewAcc} ->
                            [{Group, [Func | Members], format_signature(Func, GlobalSpecs) ++ Sigs} | NewAcc]
                    end;
                ({Group, _Anno, _Sig, _Doc, _Meta} = Func, Acc) ->
                    [{Group, [Func], format_signature(Func, GlobalSpecs)} | Acc]
            end, [],
            lists:sort(fun(A, B) -> element(1, A) =< element(1, B) end, FDocs)),
    lists:map(
        fun({Group, [{{Type,_,_},_,_,_,_}|_]=Members, Signatures}) ->
            DocContents = case lists:search(fun({_,_,_,Doc,_}) -> Doc =/= #{} end, Members) of
                            {value, {_,_,_,Doc,_Meta}} -> Doc;
                            false ->
                                case lists:keyfind(Group, 1, AllDocs) of
                                        false -> none;
                                        {_,_,_,Doc,_} -> Doc
                                    end
                        end,
                        
            case DocContents of
                #{}=M when map_size(M) =:= 0 -> ~"";
                ~"" when Type =/= type -> ~"";
                none when Type =/= type -> ~"";
                hidden -> ~"";
                _ -> FormattedDocs = case DocContents of
                        #{~"en" := MarkdownText} when is_binary(MarkdownText) -> format(shell_docs_markdown:parse_md(MarkdownText));
                        #{~"en" := MarkdownContent1} -> format(MarkdownContent1);
                        none -> ~""
                    end,
                    [".B\n", Signatures, "\n.br\n.RS\n",FormattedDocs,"\n.RE\n"]
            end
        end, Grouping).

format_signature({{Type,F,A},_Anno,Sigs,_Docs,Meta}, Specs) ->
    case maps:find({F, A}, maps:get(Type, Specs, #{})) of
        {ok, Spec} ->
            [format_ast(Spec) |format_meta(Meta)];
        _Error ->
            lists:map(fun(Sig) -> ["\\fB", escape(Sig), "\\fR", "\n"] end, Sigs) ++ [format_meta(Meta)]
    end.

format_ast(AST) ->
    PPSpec = erl_pp:attribute(AST,[{encoding,unicode}]),
    Spec = case AST of
                {_Attribute, _Line, opaque, _} -> hd(string:split(PPSpec,"::"));
                _ -> PPSpec
            end,
    BinSpec = unicode:characters_to_binary(string:trim(Spec, trailing, "\n")),

    BinSpec2 = re:replace(BinSpec, "-((type)|(spec)|(callback)) ", ""),
    BinSpec3 = string:replace(escape(BinSpec2),"\n","\\fR\n\\fB",all),
    ["\\fB", BinSpec3, "\\fR", "\n"].

format_meta(#{ deprecated := Depr } = M) ->
    [~"\n.RS\n.LP\nDeprecated: ",
     unicode:characters_to_binary(Depr), ~"\n.RE\n" | format_meta(maps:remove(deprecated, M))];
format_meta(_) -> [].
