%%%-------------------------------------------------------------------
%%% File    : xpath_abbrev.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 17 Jan 2006 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(xpath_abbrev).

-export([test/0, check_node_set/2, ticket_6873/0, ticket_7496/0, functions/0]).
-export([namespaces/0]).

-include("test_server.hrl").
-include_lib("xmerl/include/xmerl.hrl").

test() ->
    ?line {E,_} = xmerl_scan:file("xpath.xml"),

    ?line Res1 = xmerl_xpath:string("blipp",E),
    ?line ok = check_node_set("blipp",Res1),
    ?line Res2 = xmerl_xpath:string("*",E),
    ?line ok = check_node_set("*",Res2),
    ?line Res3 = xmerl_xpath:string("blipp/blupp/plopp/text()",E),
    ?line ok = check_node_set("blipp/blupp/plopp/text()",Res3),
    ?line Res4 = xmerl_xpath:string("blipp/blupp/@att2",E),
    ?line ok = check_node_set("blipp/blupp/@att2",Res4),
    ?line Res5 = xmerl_xpath:string("blipp/@*",E),
    ?line ok = check_node_set("blipp/@*",Res5),
    ?line Res6 = xmerl_xpath:string("blipp[2]",E),
    ?line ok = check_node_set("blipp[2]",Res6),
    ?line Res7 = xmerl_xpath:string("blipp[last()]",E),
    ?line ok = check_node_set("blipp[last()]",Res7),
    ?line Res8 = xmerl_xpath:string("*/blupp",E),
    ?line ok = check_node_set("*/blupp",Res8),
    ?line Res9 = xmerl_xpath:string("/myBS_model/blipp[3]/blupp[2]",E),
    ?line ok = check_node_set("/myBS_model/blipp[3]/blupp[2]",Res9),
    ?line Res10 = xmerl_xpath:string("blipp//plopp",E),
    ?line ok = check_node_set("blipp//plopp",Res10),
    ?line Res11 = xmerl_xpath:string("//plopp",E),
    ?line ok = check_node_set("//plopp",Res11),
    ?line Res12 = xmerl_xpath:string("//blupp/plopp",E),
    ?line ok = check_node_set("//blupp/plopp",Res12),
    ?line Res13 = xmerl_xpath:string(".",E),
    ?line ok = check_node_set(".",Res13),
    ?line Res14 = xmerl_xpath:string(".//blipp2",E),
    ?line ok = check_node_set(".//blipp2",Res14),
    ?line Res15 = xmerl_xpath:string(".//blipp2/blupp/plopp/..",E),
    ?line ok = check_node_set(".//blipp2/blupp/plopp/..",Res15),
    ?line Res16 = xmerl_xpath:string(".//blipp[2]/blupp/plopp/../@att2",E),
    ?line ok = check_node_set(".//blipp[2]/blupp/plopp/../@att2",Res16),
    ?line Res17 = xmerl_xpath:string(".//blipp/blupp/plopp[2]/../@att2",E),
    ?line ok = check_node_set(".//blipp/blupp/plopp[2]/../@att2",Res17),
    ?line Res18 = xmerl_xpath:string("blipp[@id='name2']",E),
    ?line ok = check_node_set("blipp[@id='name2']",Res18),
    ?line Res19 = xmerl_xpath:string("blipp[@id='name2'][3]",E),
    ?line ok = check_node_set("blipp[@id='name2'][3]",Res19),
    ?line Res20 = xmerl_xpath:string("//blupp[plopp=\"here are some more text\"]",E),
    ?line ok = check_node_set("//blupp[plopp=\"here are some more text\"]",Res20),
    ?line Res21 = xmerl_xpath:string("//blupp[plopp]",E),
    ?line ok = check_node_set("//blupp[plopp]",Res21),
    ?line Res22 = xmerl_xpath:string("blipp[@id and @test]",E),
    ?line ok = check_node_set("blipp[@id and @test]",Res22).

check_node_set("blipp",[E1,E2,E3]) -> 
    ?line ok = xml_element_name(E1,blipp),
    ?line ok = xml_element_name(E2,blipp),
    ?line ok = xml_element_name(E3,blipp),
    ok;
check_node_set("*",[E1,E2,E3,E4]) ->
    ?line ok = xml_element_name(E1,blipp),
    ?line ok = xml_element_name(E2,blipp),
    ?line ok = xml_element_name(E3,blipp),
    ?line ok = xml_element_name(E4,blipp2),
    ok;
check_node_set("blipp/blupp/plopp/text()",[T1,T2]) ->
    ?line #xmlText{value="here are some text"} = T1,
    ?line #xmlText{value="here are some more text"} = T2,
    ok;
check_node_set("blipp/blupp/@att2",[A1,A2]) ->
    ?line #xmlAttribute{name=att2} = A1,
    ?line #xmlAttribute{name=att2} = A2,
    ok;
check_node_set("blipp/@*",[A1,A2,A3,A4]) ->
    ?line #xmlAttribute{} = A1,
    ?line #xmlAttribute{} = A2,
    ?line #xmlAttribute{} = A3,
    ?line #xmlAttribute{} = A4,
    ok;
check_node_set("blipp[2]",[E]) ->
    ?line #xmlElement{name=blipp,
		      attributes=[#xmlAttribute{name=id,value="name2"}]} = E,
    ok;
check_node_set("blipp[last()]",[E]) ->
    ?line #xmlElement{name=blipp,
		      attributes=[#xmlAttribute{name=id,value="name3"}|_]} = E,
    ok;
check_node_set("*/blupp",[E1,E2,E3,E4,E5,E6]) ->
    ?line ok = xml_element_name(E1,blupp),
    ?line ok = xml_element_name(E2,blupp),
    ?line ok = xml_element_name(E3,blupp),
    ?line ok = xml_element_name(E4,blupp),
    ?line ok = xml_element_name(E5,blupp),
    ?line ok = xml_element_name(E6,blupp),
    ok;
check_node_set("/myBS_model/blipp[3]/blupp[2]",[E]) ->
    ?line #xmlElement{name=blupp,
		      attributes=[#xmlAttribute{name=att,value="bluppc2"}]}=E,
    ok;
check_node_set("blipp//plopp",[#xmlElement{name=plopp},#xmlElement{name=plopp}]) -> 
    ok;
check_node_set("//plopp",[E1,E2,E3]) ->
    ?line ok = xml_element_name(E1,plopp),
    ?line ok = xml_element_name(E2,plopp),
    ?line ok = xml_element_name(E3,plopp),
    ok;
check_node_set("//blupp/plopp",[E1,E2,E3]) ->
    ?line ok = xml_element_name(E1,plopp),
    ?line ok = xml_element_name(E2,plopp),
    ?line ok = xml_element_name(E3,plopp),
    ok;
check_node_set(".",[#xmlElement{name=myBS_model}]) ->
    ok;
check_node_set(".//blipp2",[#xmlElement{name=blipp2}]) ->
    ok;
check_node_set(".//blipp2/blupp/plopp/..",[#xmlElement{name=blupp}]) ->
    ok;
check_node_set(".//blipp[2]/blupp/plopp/../@att2",[#xmlAttribute{name=att2,value="bluppb"}]) ->
    ok;
check_node_set(".//blipp/blupp/plopp[2]/../@att2",[#xmlAttribute{name=att2,value="bluppc"}]) ->
    ok;
check_node_set("blipp[@id='name2']",[E]) ->
    ?line #xmlElement{name=blipp,
		      attributes=[#xmlAttribute{name=id,value="name2"}]}=E,
    ok;
check_node_set("blipp[@id='name2'][3]",[]) ->
    ok;
check_node_set("//blupp[plopp=\"here are some more text\"]",[E]) ->
    ?line #xmlElement{name=blupp,
		      content=[_T,#xmlElement{name=plopp,content=C}|_]} = E,
    ?line true = lists:keymember("here are some more text",#xmlText.value,C),
    ok;
check_node_set("//blupp[plopp]",[E1,E2,E3]) ->
    ?line #xmlElement{name=blupp,
		      content=C1} = E1,
    ?line true = lists:keymember(plopp,#xmlElement.name,C1),
    ?line #xmlElement{name=blupp,
		      content=C2} = E2,
    ?line true = lists:keymember(plopp,#xmlElement.name,C2),
    ?line #xmlElement{name=blupp,
		      content=C3} = E3,
    ?line true = lists:keymember(plopp,#xmlElement.name,C3),
    ok;
check_node_set("blipp[@id and @test]",[E]) ->
    ?line #xmlElement{name=blipp,
		      attributes=Atts} = E,
    ?line true = lists:keymember(id,#xmlAttribute.name,Atts),
    ?line true = lists:keymember(test,#xmlAttribute.name,Atts),
    ok;
check_node_set(Pattern,NodeSet) ->
    io:format("Pattern: ~p~nNodeSet: ~p~n",[Pattern,NodeSet]),
    error.

xml_element_name(E,N) ->
    ?line #xmlElement{name=N} = E,
    ok.

ticket_6873() ->
    ?line [#xmlElement{}] = xmerl_xpath:string("//foo[contains(@bar, 'oe')]",element(1,xmerl_scan:string("<foo bar=\"Joe\" />"))),
    ok.

ticket_7496() ->
    Test = fun(Doc, XPath, Exp) ->
		   Result = xmerl_xpath:string(XPath, Doc),
		   ?line Exp = [Name || #xmlElement{name = Name} <- Result],
		   ok
	   end,
    ?line {Doc1,_} = xmerl_scan:string("<a><b/> <c/> <d/> <e/></a>"),
    ?line ok = Test(Doc1, "//b/following::*", [c, d, e]),
    ?line ok = Test(Doc1,"//b/following::*[1]", [c]),
    ?line ok = Test(Doc1,"//b/following::*[position()=1]", [c]),
    ?line ok = Test(Doc1,"//b/following::*[3]", [e]),
    ?line ok = Test(Doc1,"//b/following::*[position()=3]", [e]),
    ?line ok = Test(Doc1,"//e/preceding::*", [b, c, d]),
    ?line ok = Test(Doc1,"//e/preceding::*[1]", [d]),
    ?line ok = Test(Doc1,"//e/preceding::*[position()=1]", [d]),
    ?line ok = Test(Doc1,"//e/preceding::*[3]", [b]),
    ?line ok = Test(Doc1,"//e/preceding::*[position()=3]", [b]),
    ?line ok = Test(Doc1,"//b/following::*[position() mod 2=0]", [d]),
    ?line ok = Test(Doc1,"//b/self::*", [b]),

    ?line {Doc2,_} = xmerl_scan:string("<a><b/> <c><d/></c> <e/> <f><g/></f> <h/> <i><j/></i> <k/></a>"),
    ?line ok = Test(Doc2,"//g/preceding::*", [b, c, d, e]),
    ?line ok = Test(Doc2, "//g/following::*", [h, i, j, k]),
    ?line ok = Test(Doc2,"//g/ancestor::*", [a, f]),
    ?line ok = Test(Doc2,"//g/ancestor::*[1]", [f]),
    ?line ok = Test(Doc2,"//g/ancestor::*[2]", [a]),
    ?line ok = Test(Doc2,"//g/ancestor-or-self::*", [a, f, g]),
    ?line ok = Test(Doc2,"//g/ancestor-or-self::*[1]", [g]),
    ?line ok = Test(Doc2,"//g/ancestor-or-self::*[2]", [f]),
    ?line ok = Test(Doc2,"//g/ancestor-or-self::*[3]", [a]),
    ?line ok = Test(Doc2,"/descendant::*", [a, b, c, d, e, f, g, h, i, j, k]),
    ?line ok = Test(Doc2,"//f/preceding-sibling::*", [b, c, e]),
    ?line ok = Test(Doc2,"//f/following-sibling::*", [h, i, k]),
    ?line ok = Test(Doc2,"//f/self::*", [f]),
    ?line ok = Test(Doc2,"//f/ancestor::*", [a]),
    ?line ok = Test(Doc2,"//f/descendant::*", [g]),
    ?line ok = Test(Doc2,"//f/preceding::*", [b, c, d, e]),
    ?line ok = Test(Doc2,"//f/following::*", [h, i, j, k]),
    ?line ok = Test(Doc2,"//text()[1]/following-sibling::*", [c, e, f, h, i, k]),
    
    ?line {Doc3,_} = xmerl_scan:file("documentRoot.xml"),
    ?line ok = Test(Doc3,"//child",[child,child,child]),
    ?line ok = Test(Doc3,"//child[@name='beta']",[child]),
    ?line [{xmlAttribute,id,[],[],[],_,1,[],"2",false}] =
	xmerl_xpath:string("/documentRoot/parent/child[@name='beta']/@id",Doc3),
    ?line ok = Test(Doc3,"/documentRoot/parent/child|/documentRoot/parent/pet",
		    [child,child,child,pet,pet]),
    ?line ok = Test(Doc3,"//*[starts-with(local-name(),'p')]",
		    [parent,pet,pet]).
    

functions() ->
    Test = fun(Doc, XPath, Exp) ->
		   Result = xmerl_xpath:string(XPath, Doc),
		   ?line Exp = [begin
				    case Obj of
					#xmlElement{name = EName} ->
					    EName;
					#xmlAttribute{name = AName} ->
					    AName;
					#xmlText{value=Text} ->
					    Text
				    end
				end|| Obj <- Result],
		   ok
	   end,
    Foo = 
	"<foo>"
	"  <bar>"
	"    <name>Xml</name>"
	"    <value>1</value>"
	"  </bar>"
	"  <bar>"
	"    <name>Xpath</name>"
	"    <value>2</value>"
	"  </bar>"
	"  <bar>"
	"    <name>Erlang</name>"
	"    <value>3</value>"
	"  </bar>"
	"</foo>",
    {Doc,_} = xmerl_scan:string(Foo),
    ?line ok = Test(Doc,"/foo/bar[name = 'Xml']/value/text()",["1"]),
    ?line ok = Test(Doc,"/foo/bar/node()/text()",
		    ["Xml","1","Xpath","2","Erlang","3"]),
    ?line ok = Test(Doc,"/foo/bar[contains(name, 'path')]",[bar]),
    ?line ok = Test(Doc,"/foo/bar[starts-with(name, 'X')]",[bar,bar]),
    ?line ok = Test(Doc,"/foo/bar[value = string(1)]/value/text()",["1"]),
    

    {Doc2,_}= xmerl_scan:file("purchaseOrder.xml"),
    ?line ok = Test(Doc2,"//*[starts-with(local-name(),'c')]",
		    ['apo:comment',city,city,comment]),
    ?line ok = Test(Doc2,"//*[starts-with(name(),'c')]",
		    [city,city,comment]),
    ?line ok = Test(Doc2,"//*[starts-with(name(),'{http://www.example.com/PO1')]",
		    ['apo:purchaseOrder','apo:comment']).


namespaces() ->
    {Doc,_} = xmerl_scan:file("purchaseOrder.xml", [{namespace_conformant, true}]),

    %% Element name using regular namespace and context namespace declaration.
    ?line [#xmlElement{nsinfo = {_, "purchaseOrder"}}] =
        xmerl_xpath:string("/apo:purchaseOrder", Doc),
    ?line [#xmlElement{nsinfo = {_, "purchaseOrder"}}] =
        xmerl_xpath:string("/t:purchaseOrder", Doc, [{namespace, [{"t", "http://www.example.com/PO1"}]}]),

    %% Wildcard element name using regular namespace and context namespace declaration.
    ?line [#xmlElement{nsinfo = {_, "comment"}}] =
        xmerl_xpath:string("./apo:*", Doc),
    ?line [#xmlElement{nsinfo = {_, "comment"}}] =
        xmerl_xpath:string("./t:*", Doc, [{namespace, [{"t", "http://www.example.com/PO1"}]}]),

    %% Attribute name using regular namespace and context namespace declaration.
    ?line [#xmlAttribute{nsinfo = {_, "type"}}, #xmlAttribute{nsinfo = {_, "type"}}] =
        xmerl_xpath:string("//@xsi:type", Doc),
    ?line [#xmlAttribute{nsinfo = {_, "type"}}, #xmlAttribute{nsinfo = {_, "type"}}] =
        xmerl_xpath:string("//@t:type", Doc, [{namespace, [{"t", "http://www.w3.org/2001/XMLSchema-instance"}]}]),

    %% Wildcard attribute name using regular namespace and context namespace declaration.
    ?line [#xmlAttribute{nsinfo = {_, "type"}}, #xmlAttribute{nsinfo = {_, "type"}}] =
        xmerl_xpath:string("//@xsi:*", Doc),
    ?line [#xmlAttribute{nsinfo = {_, "type"}}, #xmlAttribute{nsinfo = {_, "type"}}] =
        xmerl_xpath:string("//@t:*", Doc, [{namespace, [{"t", "http://www.w3.org/2001/XMLSchema-instance"}]}]),

    ok.
