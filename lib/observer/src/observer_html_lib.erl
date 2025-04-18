%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2003-2025. All Rights Reserved.
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
-module(observer_html_lib).
-moduledoc false.

%% 
%% This module implements the HTML generation for the crashdump
%% viewer. No logic or states are kept by this module.
%%

-export([plain_page/2,
	 expandable_term/4,
	 warning/2]).

-include("crashdump_viewer.hrl").
-include("observer_defs.hrl").

%%%-----------------------------------------------------------------
%%% Display the given information as is, no heading
%%% Empty body if no info exists.
warning(Info, Colors0) ->
    Colors = convert(Colors0),
    header(body(warning_body(Info), Colors)).

warning_body(Info) ->
    [warn(Info)].

%%%-----------------------------------------------------------------
%%% Display the given information as is, no heading
%%% Empty body if no info exists.
plain_page(Info, Colors0) ->
    Colors = convert(Colors0),
    header(body(plain_body(Info), Colors)).

plain_body(Info) ->
    [pre(href_proc_port(lists:flatten(Info)))].

%%%-----------------------------------------------------------------
%%% Expanded memory
expandable_term(Heading,Expanded,Tab, Colors0) ->
    Colors = convert(Colors0),
    header(Heading,
           body(expandable_term_body(Heading,Expanded,Tab,Colors),
                Colors)).
    
expandable_term_body(Heading,[],_Tab, _) ->
    [case Heading of
	 "MsgQueue" -> "No messages were found";
	 "Message Queue" -> "No messages were found";
	 "StackDump"  -> "No stack dump was found";
	 "Dictionary" -> "No dictionary was found";
	 "ProcState"  -> "Information could not be retrieved,"
			     " system messages may not be handled by this process.";
         "SaslLog"    -> "No log entry was found";
         "Persistent Terms" -> "No persistent terms were found"
     end];
expandable_term_body(Heading,Expanded,Tab, Colors) ->
    Attr = "BORDER=0 CELLPADDING=0 CELLSPACING=1 WIDTH=100%",
    [case Heading of
	 "MsgQueue" ->
	     table(Attr,
		   [tr(
		      [th("WIDTH=70%","Message"),
		       th("WIDTH=30%","SeqTraceToken")]) |
		    element(1, lists:mapfoldl(fun(Msg, Even) ->
						      {msgq_table(Tab, Msg, Even, Colors),
						       not Even}
					      end,
					      true, Expanded))]);
	 "Message Queue" ->
	     table(Attr,
		   [tr(
		      [th("WIDTH=10%","Id"),
		       th("WIDTH=90%","Message")]) |
		    element(1, lists:mapfoldl(fun(Msg, {Even,N}) ->
						      {msgq_table(Tab, Msg, N, Even, Colors),
						       {not Even, N+1}}
					      end,
					      {true,1}, Expanded))]);
	 "StackDump" ->
	     table(Attr,
		   [tr(
		      [th("WIDTH=20%","Label"),
		       th("WIDTH=80%","Term")]) |
		    element(1, lists:mapfoldl(fun(Entry, Even) ->
						      {stackdump_table(Tab, Entry, Even, Colors),
						       not Even}
					      end, true, Expanded))]);
	 "ProcState" ->
	     table(Attr,
		   [tr(
		      [th("WIDTH=20%","Label"),
		       th("WIDTH=80%","Information")]) |
		    element(1, lists:mapfoldl(fun(Entry, Even) ->
						      {proc_state(Tab, Entry,Even, Colors),
						       not Even}
					      end, true, Expanded))]);         
     "SaslLog"  ->
             table(Attr,
                   [tr("BGCOLOR=white",[td("ALIGN=left", pre(href_proc_port(Expanded)))])]) ;
	 _ ->
	     table(Attr,
		   [tr(
		      [th("WIDTH=30%","Key"),
		       th("WIDTH=70%","Value")]) |
		    element(1, lists:mapfoldl(fun(Entry, Even) ->
						      {dict_table(Tab, Entry, Even, Colors),
						       not Even}
					      end, true, Expanded))])
     end].

msgq_table(Tab,{Msg0,Token0}, Even, Colors) ->
    Token = case Token0 of
		[] -> "";
		_ -> io_lib:fwrite("~w",[Token0])
	    end,
    Msg = all_or_expand(Tab,Msg0),
    tr(color(Even, Colors),[td(pre(Msg)), td(Token)]).

msgq_table(Tab,Msg0, Id, Even, Colors) ->
    Msg = all_or_expand(Tab,Msg0),
    tr(color(Even, Colors),[td(integer_to_list(Id)), td(pre(Msg))]).

stackdump_table(Tab,{Label0,Term0},Even, Colors) ->
    Label = io_lib:format("~ts",[Label0]),
    Term = case string:split(Label, ":") of
               [_Addr, "S" ++ _] ->
                   %% Return address or catch tag. It is known to be a
                   %% flat list, shortish, possibly containing characters
                   %% greater than 255.
                   href_proc_port(Term0);
               _ ->
                   %% Any term is possible, including huge ones.
                   all_or_expand(Tab,Term0)
           end,
    tr(color(Even, Colors), [td("VALIGN=center",pre(Label)), td(pre(Term))]).

dict_table(Tab,{Key0,Value0}, Even, Colors) ->
    Key = all_or_expand(Tab,Key0),
    Value = all_or_expand(Tab,Value0),
    tr(color(Even, Colors), [td("VALIGN=center",pre(Key)), td(pre(Value))]).

proc_state(Tab,{Key0,Value0}, Even, Colors) ->
    Key = lists:flatten(io_lib:format("~ts",[Key0])),
    Value = all_or_expand(Tab,Value0),
    tr(color(Even, Colors), [td("VALIGN=center",Key), td(pre(Value))]).

all_or_expand(Tab,Term) ->
    Preview = io_lib:format("~tP",[Term,8]),
    Check = io_lib:format("~tP",[Term,100]),
    Exp = Preview=/=Check,
    all_or_expand(Tab,Term,Preview,Exp).
all_or_expand(_Tab,Term,Str,false)
  when not is_binary(Term) ->
    href_proc_port(lists:flatten(Str));
all_or_expand(Tab,Term,Preview,true)
  when not is_binary(Term) ->
    Key = {Key1,Key2,Key3} = {erlang:unique_integer([positive]),1,2},
    ets:insert(Tab,{Key,Term}),
    [href_proc_port(lists:flatten(Preview), false), $\n,
     href("TARGET=\"expanded\"",
	  ["#Term?key1="++integer_to_list(Key1)++
	       "&key2="++integer_to_list(Key2)++
	       "&key3="++integer_to_list(Key3)],
	  "Click to expand above term")];
all_or_expand(Tab,Bin,_PreviewStr,_Expand)
  when is_binary(Bin) ->
    OBSBin = observer_lib:make_obsbin(Bin,Tab),
    Term = io_lib:format("~tp", [OBSBin]),
    href_proc_port(lists:flatten(Term), true).

color(true, #colors{even=Even}) -> "BGCOLOR="++Even;
color(false,#colors{odd=Odd})   -> "BGCOLOR="++Odd.

%%%-----------------------------------------------------------------
%%% Internal library
start_html() ->
    "<HTML>\n".
stop_html() ->
    "</HTML>".
start_html_body(#colors{even=Even, fg=Fg}) ->
    "<BODY BGCOLOR=" ++ Even ++ ">\n <FONT COLOR=" ++ Fg ++ ">\n".
stop_html_body() ->
    "</FONT> </BODY>\n".

header(Body) ->
    header("","",Body).
header(Title,Body) ->
    header(Title,"",Body).
header(Title,JavaScript,Body) ->
    [%only_http_header(),
     html_header(Title,JavaScript,Body)].

html_header(Title,JavaScript,Body) ->    
    [start_html(),
     only_html_header(Title,JavaScript),
     Body,
     stop_html()].

only_html_header(Title,JavaScript) ->
    ["<HEAD>\n",
     "<TITLE>", Title,  "</TITLE>\n",
     JavaScript,
     "</HEAD>\n"].

body(Text, Colors) ->
    [start_html_body(Colors),
     Text,
     stop_html_body()].

start_table(Args) ->
    ["<TABLE ", Args, ">\n"].
stop_table() ->
    "</TABLE>\n".

table(Args,Text) ->
    [start_table(Args), Text, stop_table()].
tr(Text) ->
    ["<TR>\n", Text, "\n</TR>\n"].
tr(Args,Text) ->
    ["<TR ", Args, ">\n", Text, "\n</TR>\n"].
th(Args,Text) ->
    ["<TH ", Args, ">\n", Text, "\n</TH>\n"].
td(Text) ->
    ["<TD>", Text, "</TD>"].
td(Args,Text) ->
    ["<TD ", Args, ">", Text, "</TD>"].

start_pre() ->
    "<PRE>".
stop_pre() ->
    "</PRE>".
pre(Text) ->
    [start_pre(),Text,stop_pre()].
href(Link,Text) ->
    ["<A HREF=\"",Link,"\">",Text,"</A>"].
href(Args,Link,Text) ->
    ["<A HREF=\"",Link,"\" ",Args,">",Text,"</A>"].
font(Args,Text) ->
    ["<FONT ",Args,">\n",Text,"\n</FONT>\n"].
p(Text) ->
    ["<P>",Text,"</P>\n"].
br() ->
    "<BR>\n".


%% In all the following, "<" is changed to "&lt;" and ">" is changed to "&gt;"
href_proc_port(Text) ->
    href_proc_port(Text,true).
href_proc_port(Text,LinkToBin) ->
    href_proc_port(Text,[],LinkToBin).
href_proc_port("#Ref<"++T,Acc,LTB) ->
    %% No links to refs
    href_proc_port(T,["#Ref&lt;"|Acc],LTB);
href_proc_port("#Fun<"++T,Acc,LTB) ->
    %% No links to funs
    href_proc_port(T,["#Fun&lt;"|Acc],LTB);
href_proc_port("#Port<"++T,Acc,LTB) ->
    {Port0,Rest} = split($>,T),
    Port = "#Port&lt;"++Port0 ++ "&gt;",
    href_proc_port(Rest,[href(Port,Port)|Acc],LTB);
href_proc_port("<<"++T,Acc,LTB) ->
    %% No links to binaries
    href_proc_port(T,["&lt;&lt;"|Acc],LTB);
href_proc_port("<"++([C|_]=T),Acc,LTB) when $0 =< C, C =< $9 ->
    %% Pid
    {Pid0,Rest} = split($>,T),
    Pid = "&lt;" ++ Pid0 ++ "&gt",
    href_proc_port(Rest,[href(Pid,Pid)|Acc],LTB);
href_proc_port("['#CDVBin'"++T,Acc,LTB) ->
    %% Binary written by crashdump_viewer:parse_heap_term(...)
    href_proc_bin(cdv, T, Acc, LTB);
href_proc_port("['#OBSBin'"++T,Acc,LTB) ->
    %% Binary written by crashdump_viewer:parse_heap_term(...)
    href_proc_bin(obs, T, Acc, LTB);
href_proc_port("['#CDVPort'"++T,Acc,LTB) ->
    %% Port written by crashdump_viewer:parse_term(...)
    {Port0,Rest} = split($],T),
    PortStr=
	case string:lexemes(Port0,",.|") of
	    [X,Y] ->
		Port = "#Port&lt;"++X++"."++Y++"&gt;",
		href(Port,Port);
	Ns ->
		"#Port&lt;" ++ lists:join($.,Ns) ++"...&gt;"
    end,
    href_proc_port(Rest,[PortStr|Acc],LTB);
href_proc_port("['#CDVPid'"++T,Acc,LTB) ->
    %% Pid written by crashdump_viewer:parse_term(...)
    {Pid0,Rest} = split($],T),
    PidStr =
	case string:lexemes(Pid0,",.|") of
	    [X,Y,Z] ->
		Pid = "&lt;"++X++"."++Y++"."++Z++"&gt;",
		href(Pid,Pid);
	    Ns ->
		"&lt;" ++ lists:join($.,Ns) ++ "...&gt;"
	end,
    href_proc_port(Rest,[PidStr|Acc],LTB);
href_proc_port("'#CDVIncompleteHeap'"++T,Acc,LTB)->
    %% The heap is incomplete! Written by crashdump_viewer:deref_pts(...)
    IH = lists:reverse(
	   lists:flatten(
	     "<FONT COLOR=\"#FF0000\">...(Incomplete Heap)</FONT>")),
    href_proc_port(T,IH++Acc,LTB);
href_proc_port("'#CDVTruncatedBinary'"++T,Acc,LTB)->
    %% A binary which is truncated! Written by 
    %% crashdump_viewer:parse_heap_term(...)
    IH = lists:reverse(
	   lists:flatten(
	     "<FONT COLOR=\"#FF0000\">&lt;&lt;...(Truncated Binary)&gt;&gt;"
	     "</FONT>")),
    href_proc_port(T,IH++Acc,LTB);
href_proc_port("'#CDVNonexistingBinary'"++T,Acc,LTB)->
    %% A binary which could not be found in the dump! Written by 
    %% crashdump_viewer:parse_heap_term(...)
    IH = lists:reverse(
	   lists:flatten(
	     "<FONT COLOR=\"#FF0000\">&lt;&lt;...(Nonexisting Binary)&gt;&gt;"
	     "</FONT>")),
    href_proc_port(T,IH++Acc,LTB);
href_proc_port("<"++T,Acc,LTB) ->
    href_proc_port(T,["&lt;"|Acc],LTB);
href_proc_port(">"++T,Acc,LTB) ->
    href_proc_port(T,["&gt;"|Acc],LTB);
href_proc_port([H|T],Acc,LTB) ->
    href_proc_port(T,[H|Acc],LTB);
href_proc_port([],Acc,_) ->
    lists:reverse(Acc).

href_proc_bin(From, T, Acc, LTB) ->
    {OffsetSizePos,Rest} = split($],T),
    BinStr =
	case string:lexemes(OffsetSizePos,",.| \n") of
	    [Offset,SizeStr,Pos] when From =:= cdv ->
                Size = list_to_integer(SizeStr),
                PreviewSize = min(Size,10),
		Id = {list_to_integer(Offset),PreviewSize,list_to_integer(Pos)},
                case crashdump_viewer:expand_binary(Id) of
                    {ok, '#CDVTruncatedBinary'} ->
                          lists:flatten(
                            "<FONT COLOR=\"#FF0000\">"
                            "&lt;&lt;...(Truncated Binary)&gt;&gt;"
                            "</FONT>");
                    {ok, PreviewBin} ->
                        PreviewStr = preview_string(Size, PreviewBin),
                        if LTB ->
                                href("TARGET=\"expanded\"",
                                     ["#Binary?offset="++Offset++
                                          "&size="++SizeStr++
                                          "&pos="++Pos],
                                     PreviewStr);
                           true ->
                                PreviewStr
                        end
                end;
	    [PreviewIntStr,PreviewBitSizeStr,SizeStr,Md5] when From =:= obs ->
		Size = list_to_integer(SizeStr),
                PreviewInt = list_to_integer(PreviewIntStr),
                PreviewBitSize = list_to_integer(PreviewBitSizeStr),
		PreviewStr = preview_string(Size,<<PreviewInt:PreviewBitSize>>),
		if LTB ->
			href("TARGET=\"expanded\"",
			     ["#OBSBinary?key1="++PreviewIntStr++
				  "&key2="++SizeStr++
				  "&key3="++Md5],
			     PreviewStr);
		   true ->
			PreviewStr
		end;
	    _ ->
		"&lt;&lt; ... &gt;&gt;"
	end,
    href_proc_port(Rest,[BinStr|Acc],LTB).

preview_string(Size, PreviewBin) when Size > 10 ->
    ["&lt;&lt;",
     remove_lgt(io_lib:format("~tp",[PreviewBin])),
     "...(",
     observer_lib:to_str({bytes,Size}),
     ")",
     "&gt;&gt"];
preview_string(_, PreviewBin) ->
    ["&lt;&lt;",
     remove_lgt(io_lib:format("~tp",[PreviewBin])),
     "&gt;&gt"].

remove_lgt(Deep) ->
    remove_lgt_1(lists:flatten(Deep)).

remove_lgt_1([$<,$<|Rest]) ->
    [$>,$>|BinStr] = lists:reverse(Rest),
    replace_lgt(lists:reverse(BinStr));
remove_lgt_1(TruncBin) ->
    TruncBin.

replace_lgt([$<|R]) ->
    ["&lt;"|replace_lgt(R)];
replace_lgt([$>|R]) ->
    ["&gt;"|replace_lgt(R)];
replace_lgt([L=[_|_]|R]) ->
    [replace_lgt(L)|replace_lgt(R)];
replace_lgt([A|R]) ->
    [A|replace_lgt(R)];
replace_lgt([]) -> [].

split(Char,Str) ->
    split(Char,Str,[]).
split(Char,[Char|Str],Acc) -> % match Char
    {lists:reverse(Acc),Str};
split(Char,[H|T],Acc) ->
    split(Char,T,[H|Acc]).

warn([]) ->
    [];
warn(Warning) ->
    font("COLOR=\"#FF0000\"",p([Warning,br(),br()])).

convert(#colors{fg={FR,FB,FG}, even={ER,EB,EG}, odd={OR,OG,OB}}) ->
    #colors{fg   = io_lib:format("\"#~2.16.0B~2.16.0B~2.16.0B\"", [FR,FB,FG]),
            even = io_lib:format("\"#~2.16.0B~2.16.0B~2.16.0B\"", [ER,EB,EG]),
            odd  = io_lib:format("\"#~2.16.0B~2.16.0B~2.16.0B\"", [OR,OG,OB])}.
