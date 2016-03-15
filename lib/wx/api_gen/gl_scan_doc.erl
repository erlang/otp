%%-*-erlang-*-
%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(gl_scan_doc).

-export([file/2]).
-export([event/3]).

-compile(export_all).

-define(error(Reason),
	throw({gl_output_error, Reason})).

-record(state, {type=undefined, gen_output=false, str=[]}).

file(FileName, Options0) ->
    {Options2, State} = parse_options(Options0),
    Options = [skip_external_dtd, {event_fun, fun event/3}, {event_state, State}|Options2],
    case xmerl_sax_parser:file(FileName, Options) of
	{ok, #state{str=Str}, _} -> lists:reverse(Str);
	Else -> Else
    end.

event(Event, _LineNo, State) ->
    gen_output(Event, State).

parse_options(Options) ->
    parse_options(Options, #state{}).

parse_options(Options, State) ->
    {Options, State}.

%% Element
%%----------------------------------------------------------------------
gen_output({characters, String0}, #state{gen_output=true, type=Type, str=Str} = State) ->
    case fix_str(strip_white_space(String0)) of
	[] -> State;
	String ->
	    Add = case Type of
		      mi -> case hd(Str) of
				"/" -> String;
				"*" -> String;
				[215] -> String;
				"+" -> String;
				"-" -> String;
				"=" -> String;
				{fenced,_,_} -> String;
				_ ->
				    [$ |String]
			    end;
		      constant -> {constant, String};
		      emphasis -> {emphasis, String};
		      function -> {function, String};
		      reffunc  -> {reffunc,  String};
		      purpose  -> {purpose,  String};
		      parameter -> {parameter, String};
		      equation -> {equation, String};
		      _        -> String
		  end,
	    State#state{str=[Add|Str]}
    end;

gen_output({startElement, _Uri, "function", _QName, _Attributes}, #state{gen_output=true} = State) ->
    State#state{type=function};
gen_output({endElement, _Uri, "function", _QName}, #state{gen_output=true} = State) ->
    State#state{type=undefined};

gen_output({startElement, _Uri, "constant", _QName, _Attributes}, #state{gen_output=true} = State) ->
    State#state{type=constant};
gen_output({endElement, _Uri, "constant", _QName}, #state{gen_output=true} = State) ->
    State#state{type=undefined};

gen_output({startElement, _Uri, "parameter", _QName, _Attributes}, #state{gen_output=true} = State) ->
    State#state{type=parameter};
gen_output({endElement, _Uri, "parameter", _QName}, #state{gen_output=true} = State) ->
    State#state{type=undefined};

gen_output({startElement, _Uri, "emphasis", _QName, _Attributes}, #state{gen_output=true} = State) ->
    State#state{type=emphasis};
gen_output({endElement, _Uri, "emphasis", _QName}, #state{gen_output=true} = State) ->
    State#state{type=undefined};

gen_output(startDocument, State) ->
    State;
gen_output(endDocument, State) ->
    State;

gen_output({startElement, _Uri, "refentrytitle", _QName, _Attributes}, #state{gen_output=true} = State) ->
    State#state{type=reffunc};
gen_output({endElement, _Uri, "refentrytitle", _QName}, #state{gen_output=true} = State) ->
    State#state{type=undefined};

gen_output({startElement, _Uri, "refpurpose", _QName, _Attributes}, State) ->
    State#state{gen_output=true, type=purpose};
gen_output({endElement, _Uri, "refpurpose", _QName}, State) ->
    State#state{gen_output=false, type=undefined};

gen_output({startElement, _Uri, "refsect1", _QName, [{_, _, "id", "description"}]}, State) ->
    State#state{gen_output=true};
gen_output({endElement, _Uri, "refsect1", _QName}, State) ->
    State#state{gen_output=false};

gen_output({startElement, _Uri, "title", _QName, _Attributes}, #state{gen_output=true} = State) ->
    State#state{gen_output=skip};
gen_output({endElement, _Uri, "title", _QName}, #state{gen_output=skip} = State) ->
    State#state{gen_output=true};

%% gen_output({startElement, _Uri, "inlineequation", _QName, _Attributes}, #state{gen_output=true} = State) ->
%%     State#state{type=equation};
%% gen_output({endElement, _Uri, "inlineequation", _QName}, #state{gen_output=skip} = State) ->
%%     State#state{type=undefined};
%% gen_output({startElement, _Uri, "informalequation", _QName, _Attributes}, #state{gen_output=true} = State) ->
%%     State#state{type=equation};
%% gen_output({endElement, _Uri, "informalequation", _QName}, #state{gen_output=skip} = State) ->
%%     State#state{type=undefined};
gen_output({startElement, _Uri, "mfenced", _QName, Attributes}, #state{gen_output=true, str=Str} = State) ->
    Fenc = get_fenc(Attributes, "(", ")"),
    State#state{str=[Fenc|Str]};
gen_output({endElement, _Uri, "mfenced", _QName}, #state{gen_output=true, str=Str0} = State) ->
    Check = fun(What) -> case What of {fenced,_,_} ->false; _ -> true end end,
    {Fenced, [{fenced, Open, Close}|Str]} = lists:splitwith(Check, Str0),
    State#state{str=[{fenced, Open, Close, lists:reverse(Fenced)}| Str]};

%% gen_output({startElement, _Uri, "tgroup", _QName, Attributes}, #state{gen_output=true, str=Str} = State) ->
%%     {_, _, _, ValueStr} = lists:keyfind(3, "cols", Attributes),
%%     ColSz = list_to_integer(ValueStr),
%%     State#state{str=[{tgroup, ColSz}|Str]};
%% gen_output({endElement, _Uri, "tgroup", _QName}, #state{gen_output=true, str=Str0} = State) ->
%%     Check = fun(What) -> case What of {tgroup, _} ->false; _ -> true end end,
%%     {Rows, [{tgroup, ColSize}|Str]} = lists:splitwith(Check, Str0),
%%     State#state{str=[{tgroup, ColSize, lists:reverse(Rows)}| Str]};

%% gen_output({endElement, _Uri, "row", _QName}, #state{gen_output=true, str=Str} = State) ->
%%     State#state{type=undefined, str=[break|Str]};
gen_output({startElement, _Uri, "informaltable", _QName, _Attributes}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["<table>"|Str]};
gen_output({endElement, _Uri, "informaltable", _QName}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["</table>"|Str]};
gen_output({startElement, _Uri, "table", _QName, _Attributes}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["<table>"|Str]};
gen_output({endElement, _Uri, "table", _QName}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["</table>"|Str]};

gen_output({startElement, _Uri, "tbody", _QName, _Attributes}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["<tbody>"|Str]};
gen_output({endElement, _Uri, "tbody", _QName}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["</tbody>"|Str]};

gen_output({startElement, _Uri, "thead", _QName, _Attributes}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["<tbody>"|Str]};
gen_output({endElement, _Uri, "thead", _QName}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["</tbody>"|Str]};
gen_output({startElement, _Uri, "row", _QName, _Attributes}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["<tr>"|Str]};
gen_output({endElement, _Uri, "row", _QName}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["</tr>"|Str]};
gen_output({startElement, _Uri, "entry", _QName, _Attributes}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["<td>"|Str]};
gen_output({endElement, _Uri, "entry", _QName}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=["</td>"|Str]};

gen_output({startElement, _Uri, "mi", _QName, _Attributes}, #state{gen_output=true} = State) ->
    State#state{type=mi};
gen_output({endElement, _Uri, "mi", _QName}, #state{gen_output=true} = State) ->
    State#state{type=undefined};
gen_output({startElement, _Uri, "mn", _QName, _Attributes}, #state{gen_output=true} = State) ->
    State#state{type=mi};
gen_output({endElement, _Uri, "mn", _QName}, #state{gen_output=true} = State) ->
    State#state{type=undefined};



gen_output({startElement, _Uri, "varlistentry", _QName, _Attributes}, #state{gen_output=true, str=Str} = State) ->
    State#state{str=[listentry|Str]};

gen_output({endElement, _Uri, "mfrac", _QName}, #state{gen_output=true, str=[A0,B|Str]} = State) ->
    A = case A0 of
	    [$ |A1] -> A1;
	    A1 -> A1
	end,
    State#state{str=[A,"/",B|Str]};

gen_output({startElement, _Uri, "para", _QName, _Attributes}, #state{gen_output=true, str=Str} = State) ->
    case Str of
	[para|_] -> State;
	[": "|_] -> State;
	[Const, listentry | Rest] ->
	    State#state{str=[": ",Const,para|Rest]};
	[Mod, Const, listentry | Rest] ->
	    State#state{str=[": ",Mod,Const,para|Rest]};
	_ ->
	    State#state{str=[para|Str]}
    end;

%% gen_output({startElement, _Uri, What, _QName, _Attributes}, State) ->
%%     io:format("Skipped ~s~n",[What]),
%%     State;

gen_output(_E, State) ->
    State.

%%----------------------------------------------------------------------
%% Function  : parse_attributes(Attributes) -> Result
%% Parameters:
%% Result    :
%% Description:
%%----------------------------------------------------------------------
print_attributes([]) ->
    ok;
print_attributes([{_Uri, _Prefix, LocalName, AttrValue} |Attributes]) ->
    io:format(" ~s=\"~s\"",[LocalName, AttrValue]),
    print_attributes(Attributes).

get_fenc([{_Uri, _Prefix, "open", AttrValue} |Attributes], _Open, Close)
  when AttrValue /= "" ->
    get_fenc(Attributes, fenc(AttrValue), Close);
get_fenc([{_Uri, _Prefix, "close", AttrValue} |Attributes], Open, _Close)
  when AttrValue /= "" ->
    get_fenc(Attributes, Open, fenc(AttrValue));
get_fenc([_|Attributes], Open, Close) ->
    get_fenc(Attributes, Open, Close);
get_fenc([], Open, Close) ->
    {fenced,Open,Close}.

fenc("&DoubleVerticalBar;") -> "||";
fenc("&VerticalBar;") -> "|";
fenc("&LeftCeiling;") -> "|";
fenc("&RightCeiling;") -> "|";
fenc("&lceil;") -> "|";
fenc("&rceil;") -> "|";
fenc("&LeftFloor;") -> "|";
fenc("&RightFloor;") -> "|";
fenc("&lfloor;") -> "|";
fenc("&rfloor;") -> "|";
fenc("[") -> "[";
fenc("]") -> "]";
fenc(Else = [_]) -> Else.

strip_white_space([$ | Str = [$  |_]]) ->
    strip_white_space(Str);
strip_white_space([$\n| Str]) ->
    strip_white_space([$ |Str]);
strip_white_space([$\t| Str]) ->
    strip_white_space([$ |Str]);
strip_white_space([$\\| Str]) ->
    strip_white_space(Str);

strip_white_space([Char|Str]) ->
    [Char|strip_white_space(Str)];
strip_white_space([]) -> [].

fix_str([$<|Str]) ->
    [$&,$l,$t,$;|fix_str(Str)];
fix_str([$>|Str]) ->
    [$&,$g,$t,$;|fix_str(Str)];
fix_str("&times;"++Str) ->
    [215|fix_str(Str)];
%% fix_str([215|Str]) ->
%%     [$*|fix_str(Str)];
fix_str("&Prime;"++Str) ->
    [$"|fix_str(Str)];
fix_str("&CenterDot;"++Str) ->
    [$.|fix_str(Str)];
fix_str("&af;"++Str) ->
    fix_str(Str);
fix_str("&it;"++Str) ->
    [$ |fix_str(Str)];
fix_str("&nbsp;"++Str) ->
    [$ |fix_str(Str)];
fix_str([$&|Str]) ->
    [$&,$a,$m,$p,$; |fix_str(Str)];
%% fix_str([C|Str]) when C > 255 ->
%%     fix_str(Str);
fix_str([C|Str]) ->
    [C|fix_str(Str)];
fix_str([]) -> [].
