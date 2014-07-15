%%%-------------------------------------------------------------------
%%% File    : motorcycles2html.erl
%%% Author  : Bertil Karlsson <bertil@localhost.localdomain>
%%% Description : 
%%%
%%% Created :  2 Sep 2004 by Bertil Karlsson <bertil@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(motorcycles2html).

-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xs, 
	[ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).

-export([process_xml/1,process_to_file/2,process_to_file/1]).

process_xml(Doc) ->
    template(Doc).

process_to_file(FileName) ->
    process_to_file(FileName,'motorcycles.xml').

process_to_file(FileName,XMLDoc) ->
    case file:open(FileName,[write]) of
	{ok,IOF} ->
	    {XMLContent,_} = xmerl_scan:file(XMLDoc),
	    TransformedXML=process_xml(XMLContent),
	    io:format(IOF,"~s",[TransformedXML]),
	    file:close(IOF);
	{error,Reason} ->
	    io:format("could not open file due to ~p.~n",[Reason])
    end.

%%% templates
template(E = #xmlElement{name='motorcycles'}) ->
    [    "<head>\n<title>motorcycles</title>\n</head>\n",
         "<body>\n",
	 "<h1>Used Motorcycles</h1>\n",
	 "<ul>\n",
	 remove_duplicates(value_of(select("bike/name/manufacturer",E))),
	 "\n</ul>\n",
	 sort_by_manufacturer(xslapply(fun template/1, E)),
         "</body>\n",
	 "</html>\n"];
template(E = #xmlElement{name='bike'}) ->
    {value_of(select("name/manufacturer",E)),["<dt>",xslapply(fun template/1,select("name",E)),"</dt>",
    "<dd><ul>\n",
    "<li style=\"color:green\">Manufacturing year: ",xslapply(fun template/1,select("@year",E)),"</li>\n",
    "<li style=\"color:red\">Color: ",xslapply(fun template/1,select("@color",E)),"</li>\n",
    "<li style=\"color:blue\">Shape : ",xslapply(fun template/1,select("@condition",E)),"</li>\n",
    "</ul></dd>\n"]};
template(E) -> built_in_rules(fun template/1, E).


%%%%%%%%%%% helper routines  

%% sorts on the bike name element, unwraps the bike information and
%% inserts a line feed and indentation on each bike element.
sort_by_manufacturer(L) ->
    Tuples=[X1||X1={_,_} <- L],
    SortedTS = lists:keysort(1,Tuples),
    InsertRefName_UnWrap=
	fun([{[Name],V}|Rest],Name,F)->
		[V|F(Rest,Name,F)];
	   ([{[Name],V}|Rest],_PreviousName,F) ->
		[["<a name=\"",Name,"\"></>"],V|F(Rest,Name,F)];
	   ([],_,_) -> []
	end,
    SortedRefed=InsertRefName_UnWrap(SortedTS,no_name,InsertRefName_UnWrap),
%    SortedTs=[Y||{X,Y}<-lists:keysort(1,Tuples)],
    WS = "\n    ",
    Fun=fun([H|T],Acc,F)->
		F(T,[H,WS|Acc],F);
	   ([],Acc,_F)->
		lists:reverse([WS|Acc])
	end,
    if length(SortedRefed) > 0 ->
	    Fun(SortedRefed,[],Fun);
       true -> []
    end.

    
%% removes all but the first of an element in L and inserts a html
%% reference for each list element.
remove_duplicates(L) ->
    remove_duplicates(L,[]).

remove_duplicates([],Acc) -> 
    make_ref(lists:sort(lists:reverse(Acc)));
remove_duplicates([A|L],Acc) -> 
    case lists:delete(A,L) of
	L ->
	    remove_duplicates(L,[A|Acc]);
	L1 -> 
	    remove_duplicates([A|L1],[Acc])
    end.

make_ref([]) -> [];
make_ref([H]) when is_atom(H) ->
    "<ul><a href=\"#"++atom_to_list(H)++"\">"++atom_to_list(H)++"</a></ul>";
make_ref([H]) when is_list(H) ->
    "<ul><a href=\"#"++H++"\">\s"++H++"</a></ul>";
make_ref([H|T]) when is_atom(H) ->
    ["<ul><a href=\"#"++atom_to_list(H)++"\">\s"++atom_to_list(H)++",\n</a></ul>"
     |make_ref(T)];
make_ref([H|T]) when is_list(H) ->
    ["<ul><a href=\"#"++H++"\">\s"++H++",\n</a></ul>"|make_ref(T)].
