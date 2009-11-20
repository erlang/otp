%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_html_util).

-export([attribute_cdata_to_html/1,
	 element_cdata_to_html/1,
	 pcdata_to_html/1, pcdata_to_html/2]).
-export([copy_pics/3]).
-export([extract_header_data/2, all_header_data/1]).
-export([make_uri/1,
	 make_anchor_href/1, make_anchor_href_short/3,
	 make_anchor_name_short/2,
	 make_funcdef_short/2]).
-export([erl_include/2, code_include/2, erl_eval/1]).
-export([number/3, count_sections/1]).
-export([format_toc/1]).
-export([html_latin1_sort_order/1]).

%%--Handle CDATA and PCDATA---------------------------------------------

%% NB: Functions for transforming sgmls/XMerL data output to html.
%%     Do not use these for included text files (cf code_include and
%%     erl_include).

attribute_cdata_to_html(Data) ->
    data2html(Data, false).

element_cdata_to_html(Data) ->
    data2html(Data, false).

pcdata_to_html(Data) ->
    data2html(Data, true).

pcdata_to_html(Data, RmSp) ->
    data2html(Data, RmSp).

%% PCDATA, CDATA: Replace entities, and optionally delete
%% leading and multiple spaces. CDATA never contains entities to
%% replace.

%% data2html(Cs, RmSpace)
data2html([246| Cs], RmSp) ->
    [$&, $#, $2, $4, $6, $;| data2html(Cs, RmSp)];
data2html([$>| Cs], RmSp) ->
    [$&, $#, $6, $2, $;| data2html(Cs, RmSp)];
data2html([$<| Cs], RmSp) ->
    [$&, $#, $6, $0, $;| data2html(Cs, RmSp)];
data2html([$&| Cs], RmSp) ->
    [$&, $#, $3, $8, $;| data2html(Cs, RmSp)];
data2html([$\"| Cs], RmSp) ->
    [$&, $#, $3, $4, $;| data2html(Cs, RmSp)];
data2html([$\n| Cs], RmSp) ->
    data2html(Cs, RmSp);
data2html([$\\, $n| Cs], false) ->
    [$\n| data2html(Cs, false)];
data2html([$\\, $n| Cs], true) ->
    [$\n| data2html(delete_leading_space(Cs), true)];
data2html([$ , $ | Cs], true) ->		% delete multiple space
    [$ | data2html(delete_leading_space(Cs), true)];
data2html([$\\, $|| Cs0], RmSp) ->
    {Ent, Cs1} = collect_entity(Cs0),
    [entity_to_html(Ent)|  data2html(Cs1, RmSp)];
data2html([$\\, $0, $1, $2| Cs], RmSp) ->
    data2html(Cs, RmSp);
data2html([$\\, $\\, $n| Cs], RmSp) ->
    [$\\, $n| data2html(Cs, RmSp)];
data2html([$\\, O1, O2, O3| Cs], RmSp)
  when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    case octal2dec(O1, O2, O3) of
	173 ->					% soft hyphen
	    data2html(Cs, RmSp);
	C when C > 31, C < 256 ->
	    Ent = io_lib:format("&#~w;", [C]),
	    [Ent|  data2html(Cs, RmSp)];
	C ->
	    [C| data2html(Cs, RmSp)]
    end;
data2html([$\\, $\\| Cs], RmSp) ->
    [$\\| data2html(Cs, RmSp)];
data2html([C| Cs], RmSp) ->
    [C| data2html(Cs, RmSp)];
data2html([], _) ->
    [].

delete_leading_space([$ | Cs]) ->
    delete_leading_space(Cs);
delete_leading_space(Cs) ->
    Cs.

collect_entity(Data) ->
    collect_entity(Data, []).

collect_entity([$\\, $|| Cs], Rs) ->
    {lists:reverse(Rs), Cs};
collect_entity([C| Cs], Rs) ->
    collect_entity(Cs, [C| Rs]);
collect_entity([], Rs) ->
    {[], lists:reverse(Rs)}.

entity_to_html("&") -> "&#38;";
entity_to_html("\"") -> "&#34;";
entity_to_html("<") -> "&#60;";
entity_to_html(">") -> "&#62;";
entity_to_html([$\\, O1, O2, O3])
  when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    case octal2dec(O1, O2, O3) of
	173 ->					% soft hyphen
	    "";
	Value ->
	    io_lib:format("&#~w;", [Value])
    end;
entity_to_html(Other) ->
    docb_html_util_iso:entity_to_html(Other).

octal2dec(O1, O2, O3) ->
    (O1*8+O2)*8+O3-73*$0.

%%--Copy images---------------------------------------------------------

copy_pics(Src, Dest, Opts) ->
    Dir = code:lib_dir(docbuilder),
    InFile = filename:join([Dir, "etc", Src]),
    OutFile = docb_util:outfile(Dest, "", Opts),

    case filelib:last_modified(OutFile) of
	0 -> % File doesn't exist
	    file:copy(InFile, OutFile);

	OutMod2 ->
	    InMod1s = calendar:datetime_to_gregorian_seconds(
			filelib:last_modified(InFile)),
	    OutMod2s = calendar:datetime_to_gregorian_seconds(OutMod2),
	    if
		InMod1s > OutMod2s -> % InFile is newer than OutFile
		    file:copy(InFile, OutFile);
		true ->
		    ok
	    end
    end.

%%--Resolve header data-------------------------------------------------

extract_header_data(Key, {header, [], List}) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, [], []}} ->
	    "";
	{value, {Key, [], [{pcdata, [], Value}]}} ->
	    pcdata_to_html(Value);
	false ->
	    ""
    end.

all_header_data(Header) ->
    all_header_data(Header,
		    [title, prepared, responsible, docno, approved,
		     checked, date, rev, file]).

all_header_data(_Header, []) ->
    [];
all_header_data(Header, [Key| Rest]) ->
    [extract_header_data(Key, Header) | all_header_data(Header, Rest)].

%%--Resolve hypertext references----------------------------------------

%% URI regular expression (RFC 2396):
%%    "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
%% We split it in five parts:
%% scheme:	"^(([^:/?#]+):)?"	(includes trailing `:')
%% authority:	"^(//([^/?#]*))?"	(includes leading `//')
%% path:	"^([^?#]*)"
%% query:	"^(\\?([^#]*))?"	(includes leading `?')
%% fragment:	"^(#(.*))?"		(includes leading `#')

make_uri(Cs) ->
    lists:flatmap(
      fun({path, S}) ->
	      case regexp:match(S, "\.xml?\$") of
		  {match, _, _} ->
		      {ok, NS, _} = regexp:sub(S, "\.xml?\$", ".html"),
		      NS;
		  _  ->
		      S
	      end;
	 ({_, S}) ->
	      S
      end,
      split_uri(Cs)).

split_uri(URI) ->
    split_uri(URI, [{scheme, "^(([^:/?#]+):)?"},
		    {authority, "^(//([^/?#]*))?"},
		    {path, "^([^?#]*)"},
		    {'query', "^(\\?([^#]*))?"},
		    {fragment, "^(#(.*))?"}]).

split_uri("", [{Tag, _R}| T]) ->
    [{Tag, ""}| split_uri("", T)];
split_uri(Cs0, [{Tag, R}| T]) ->
    {match, 1, N} = regexp:match(Cs0, R),
    Cs1 = string:substr(Cs0, 1, N),
    Cs2 = strip_and_escape_uri_component(Tag, Cs1),
    [{Tag, Cs2}| split_uri(string:substr(Cs0, N+1), T)];
split_uri(_, []) ->
    [].

strip_and_escape_uri_component(authority, "//" ++ Cs) ->
    "//" ++ escape_uri(string:strip(Cs));
strip_and_escape_uri_component(path, Cs) ->
    escape_uri(string:strip(Cs));
strip_and_escape_uri_component('query', "?" ++ Cs) ->
    "?" ++ escape_uri(string:strip(Cs));
strip_and_escape_uri_component(fragment, "#" ++ Cs) ->
    "#" ++ escape_uri(string:strip(Cs));
strip_and_escape_uri_component(_, "") ->
    "";
strip_and_escape_uri_component(_, Cs) ->
    escape_uri(string:strip(Cs)).

escape_uri([C|Cs]) when C =< 32;
                        C == $<; C == $<; C == $#; C == $%; C == $";
                        C == $?;
			C == ${; C == $}; C ==$|; C == $\\; C == $^;
			C == $[; C == $]; C ==$';
			C >= 127 ->
    [$%, mk_hex(C div 16), mk_hex(C rem 16)| escape_uri(Cs)];
escape_uri([C|Cs]) ->
    [C|escape_uri(Cs)];
escape_uri([]) ->
    [].

mk_hex(C) when C<10 ->
    C + $0;
mk_hex(C) ->
    C - 10 + $a.

make_anchor_href(HRef) ->
    case regexp:split(HRef, "#") of
	{ok, [HRef]} ->
	    %% No `#' in HRef, i.e. only path
	    make_anchor_href(HRef, "");
	 {ok, [Path, Fragment]}->
	    make_anchor_href(Path, Fragment)
    end.

make_anchor_href(Path0, Frag0) ->
    Frag1 = string:strip(Frag0),
    Path1 = case Path0 of
		"" ->
		    "";
		_  ->
		    case regexp:match(Path0, "\.xml?\$") of
			nomatch ->
			    Path0 ++ ".html";
			_ ->
			    {ok, NewPath, _} = regexp:sub(Path0,
							  "\.xml?\$",
							  ".html"),
			    NewPath
		    end
	    end,
    case Frag1 of
	"" ->
	    attribute_cdata_to_html(Path1);
	_ ->
	    attribute_cdata_to_html(Path1) ++ 
		"#" ++
		attribute_cdata_to_html([case Ch of $/ -> $-; _ -> Ch end||
					    Ch <-Frag1])
    end.

make_anchor_href_short(Path, Frag, RefType) ->
    ShortFrag = make_funcdef_short(Frag, RefType,"-"),
    make_anchor_href(Path, ShortFrag).

make_anchor_name_short(FuncName0, RefType) ->
    FuncName1 = make_funcdef_short(FuncName0, RefType,"-"),
    attribute_cdata_to_html(FuncName1).

make_funcdef_short(FuncDef0, RefType) ->
    make_funcdef_short(FuncDef0, RefType, "/").

make_funcdef_short(FuncDef0, RefType,Delimiter) ->
    FuncDef1 = docb_util:trim(FuncDef0),
    Any0 = case lists:member(RefType, [cref, erlref]) of
	       true ->
		   case catch docb_util:fknidx(FuncDef1, Delimiter) of
		       {'EXIT', _} ->
			   false;
		       Any1  ->
			   Any1
		   end;
	       false ->
		   false
	   end,
    case Any0 of
	false ->
	    case string:tokens(FuncDef1, " ") of
		[Any2| _] ->
		    Any2;
		_ ->
		    FuncDef1
	    end;
	_ ->
	    Any0
    end.

%%--Include tags--------------------------------------------------------

%% Only used in report DTD
erl_include(File, Tag) ->
    case docb_main:include_file(File, Tag) of
	{ok, Cs} ->
	    {drop, "\n<pre>\n" ++ text_to_html(Cs) ++ "\n</pre>\n"};
	error ->
	    {drop, ""}
    end.

code_include(File, Tag) ->
    case docb_main:include(File, Tag, Tag) of
	{ok, Cs} ->
	    {ok,text_to_html(Cs)};
	error ->
	    {error, {codeinclude,File}}
    end.

erl_eval(Expr) ->
    Cs = docb_main:eval_str(Expr),
    {drop, "\n<pre>\n" ++ text_to_html(Cs) ++ "\n</pre>\n"}.

%% Only replaces certain characters. Spaces and new lines etc are kept.
%% Used for plain text (e.g. inclusions of code).
text_to_html([$>| Cs]) ->
    [$&, $#, $6, $2, $;| text_to_html(Cs)];
text_to_html([$<| Cs]) ->
    [$&, $#, $6, $0, $;| text_to_html(Cs)];
text_to_html([$&| Cs]) ->
    [$&, $#, $3, $8, $;| text_to_html(Cs)];
text_to_html([$\"| Cs]) ->
    [$&, $#, $3, $4, $;| text_to_html(Cs)];
text_to_html([C| Cs]) ->
    [C| text_to_html(Cs)];
text_to_html([]) ->
    [].

%%--Number sections-----------------------------------------------------

number({Tag, Attrs, More}, none, File) ->
    {Tag, Attrs, do_number(More, [1], File)};
number({Tag, Attrs, More}, Prefix, File) ->
    {Tag, Attrs, do_number(More, [list_to_integer(Prefix)], File)}.

do_number([], _, _) ->
    [];
do_number([{header, Attrs, More}| Rest], NN, File) ->
    [{header, Attrs, More}| do_number(Rest, NN, File)];
do_number([{section, Attrs, More}| Rest], [N| NN], File) ->
    [{section, Attrs, do_number(More, [1, N| NN], File)}|
     do_number(Rest, [N+1| NN], File)];
do_number([{title, _, [{pcdata, _, Title}]}| More], [N| NN], File) ->
    Format = make_format(length(NN)),
    Number = lists:flatten(io_lib:format(Format, lists:reverse(NN))),
    [{marker, [{"ID", "CDATA", Number}], []},
     {title, [{"NUMBER", "CDATA", Number},
	      {"FILE", "CDATA", File}],
      [{pcdata, [], Title}]}| do_number(More, [N| NN], File)];
do_number([{pcdata, Attrs, More}| Rest], NN, File) ->
    [{pcdata, Attrs, More}| do_number(Rest, NN, File)];
do_number([{Tag, Attrs, More}| Rest], NN, File) ->
    [{Tag, Attrs, do_number(More, NN, File)}|do_number(Rest, NN, File)].

make_format(1) ->
    "~w";
make_format(N) ->
    "~w." ++ make_format(N-1).

count_sections([section| Rest]) ->
    1 + count_sections(Rest);
count_sections([_| Rest]) ->
    count_sections(Rest);
count_sections([]) ->
    0.

%%--Make a ToC----------------------------------------------------------

format_toc(Toc) ->
    lists:map(fun({Number, Title}) ->
		      [Number, " <a href = \"#", Number,
		       "\">", Title, "</a><br/>\n"]
	      end, Toc).

%%--Convert HTML ISO Latin 1 characters to ordinary characters----------

%% To be used for sorting.  Cs must be flat.
html_latin1_sort_order(Cs) ->
    hlso(Cs).

hlso([]) ->
    [];
hlso([$&, $#, C2, C1, C0, $;| Cs])
  when $0 =< C2, C2 =< $9, $0 =< C1, C1 =< $9, $0 =< C0, C0 =< $9 ->
    C = ((C2-$0)*10 + (C1-$0))*10 + C0-$0,
    hlso0(C, Cs);
hlso([$&, $#, C1, C0, $;| Cs])
  when $0 =< C1, C1 =< $9, $0 =< C0, C0 =< $9 ->
    C = (C1-$0)*10 + C0-$0,
    hlso0(C, Cs);
hlso([C| Cs]) ->
    [C| hlso(Cs)].

hlso0(C, Cs) when 0 =< C, C =< 159  ->
    [C| hlso(Cs)];
hlso0(160, Cs) ->  %% no-break space
    hlso(Cs);					% Remove it.
hlso0(161, Cs) ->  %% inverted exclamation mark
    [$? |hlso(Cs)];
hlso0(162, Cs) ->  %% cent sign
    [$$|hlso(Cs)];
hlso0(163, Cs) ->  %% pound sterling sign
    [$$|hlso(Cs)];
hlso0(164, Cs) ->  %% general currency sign
    [$$|hlso(Cs)];
hlso0(165, Cs) ->  %% yen sign
    [$$|hlso(Cs)];
hlso0(166, Cs) ->  %% broken (vertical) bar
    [$| |hlso(Cs)];
hlso0(167, Cs) ->  %% section sign
    [$$|hlso(Cs)];
hlso0(168, Cs) ->  %% umlaut (dieresis)
    [$: |hlso(Cs)];
hlso0(169, Cs) ->  %% copyright sign
    [$c |hlso(Cs)];
hlso0(170, Cs) ->  %% ordinal indicator, feminine
    [$f |hlso(Cs)];
hlso0(171, Cs) ->  %% angle quotation mark, left
    [$" |hlso(Cs)];
hlso0(172, Cs) ->  %% not sign
    [$- |hlso(Cs)];
hlso0(173, Cs) ->  %% soft hyphen
    [$- |hlso(Cs)];
hlso0(174, Cs) ->  %% registered sign
    [$r |hlso(Cs)];
hlso0(175, Cs) ->  %% macron
    [$- |hlso(Cs)];
hlso0(176, Cs) ->  %% degree sign
    [$d |hlso(Cs)];
hlso0(177, Cs) ->  %% plus-or-minus sign
    [$+ |hlso(Cs)];
hlso0(178, Cs) ->  %% superscript two
    [$2 |hlso(Cs)];
hlso0(179, Cs) ->  %% superscript three
    [$3 |hlso(Cs)];
hlso0(180, Cs) ->  %% acute accent
    [$' |hlso(Cs)];
hlso0(181, Cs) ->  %% micro sign
    [$' |hlso(Cs)];
hlso0(182, Cs) ->  %% pilcrow (paragraph sign)
    [$$|hlso(Cs)];
hlso0(183, Cs) ->  %% middle dot
    [$. |hlso(Cs)];
hlso0(184, Cs) ->  %% cedilla
    [$c |hlso(Cs)];
hlso0(185, Cs) ->  %% superscript one
    [$1 |hlso(Cs)];
hlso0(186, Cs) ->  %% ordinal indicator, masculine
    [$m |hlso(Cs)];
hlso0(187, Cs) ->  %% angle quotation mark, right
    [$" |hlso(Cs)];
hlso0(188, Cs) ->  %% fraction one-quarter
    [$4 |hlso(Cs)];
hlso0(189, Cs) ->  %% fraction one-half
    [$2 |hlso(Cs)];
hlso0(190, Cs) ->  %% fraction three-quarters
    [$3 |hlso(Cs)];
hlso0(191, Cs) ->  %% inverted question mark
    [$? |hlso(Cs)];

hlso0(C, Cs) when 192 =< C, C =< 198 -> %% capital A
    [$A |hlso(Cs)];
hlso0(199, Cs) ->  %% capital C, cedilla 
    [$C |hlso(Cs)];
hlso0(C, Cs) when 200 =< C, C =< 203 ->  %% capital E
    [$E |hlso(Cs)];
hlso0(C, Cs) when 204 =< C, C =< 207 -> %% capital I
    [$I |hlso(Cs)];
hlso0(208, Cs) ->  %% capital Eth, Icelandic
    [$D |hlso(Cs)];
hlso0(209, Cs) ->  %% capital N, tilde
    [$N |hlso(Cs)];
hlso0(C, Cs) when 210 =< C, C =< 214 -> %% capital O
    [$O |hlso(Cs)];
hlso0(215, Cs) ->  %% multiply sign
    [$x |hlso(Cs)];
hlso0(216, Cs) ->  %% capital O, slash
    [$O |hlso(Cs)];
hlso0(C, Cs) when 217 =< C, C =< 220 ->  %% capital U
    [$U |hlso(Cs)];
hlso0(221, Cs) ->  %% capital Y, acute accent
    [$Y |hlso(Cs)];
hlso0(222, Cs) ->  %% capital THORN, Icelandic
    [$T |hlso(Cs)];
hlso0(223, Cs) ->  %% small sharp s, German (sz
    [$s |hlso(Cs)];
hlso0(C, Cs) when 224 =< C, C =< 230->  %% small a
    [$a |hlso(Cs)];
hlso0(231, Cs) ->  %% small c, cedilla
    [$c |hlso(Cs)];
hlso0(C, Cs) when 232 =< C, C =< 235 ->  %% small e
    [$e |hlso(Cs)];
hlso0(C, Cs) when 236 =< C, C =< 239 ->  %% small i
    [$i |hlso(Cs)];
hlso0(240, Cs) ->  %% small eth, Icelandic
    [$d |hlso(Cs)];
hlso0(241, Cs) ->  %% small n, tilde
    [$n |hlso(Cs)];
hlso0(C, Cs) when 242 =< C, C =< 246 ->  %% small o
    [$o |hlso(Cs)];
hlso0(247, Cs) ->  %% divide sign
    [$/ |hlso(Cs)];
hlso0(248, Cs) ->  %% small o, slash
    [$o |hlso(Cs)];
hlso0(C, Cs) when 249 =< C, C =< 252 ->  %% small u
    [$u |hlso(Cs)];
hlso0(253, Cs) ->  %% small y, acute accent
    [$y |hlso(Cs)];
hlso0(254, Cs) ->  %% small thorn, Icelandic
    [$t |hlso(Cs)];
hlso0(255, Cs) ->  %% small y, dieresis or umlaut
    [$y |hlso(Cs)].
