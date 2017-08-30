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

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

test() ->
    {E,_} = xmerl_scan:file("xpath.xml"),

    Res1 = xmerl_xpath:string("blipp",E),
    ok = check_node_set("blipp",Res1),
    Res2 = xmerl_xpath:string("*",E),
    ok = check_node_set("*",Res2),
    Res3 = xmerl_xpath:string("blipp/blupp/plopp/text()",E),
    ok = check_node_set("blipp/blupp/plopp/text()",Res3),
    Res4 = xmerl_xpath:string("blipp/blupp/@att2",E),
    ok = check_node_set("blipp/blupp/@att2",Res4),
    Res5 = xmerl_xpath:string("blipp/@*",E),
    ok = check_node_set("blipp/@*",Res5),
    Res6 = xmerl_xpath:string("blipp[2]",E),
    ok = check_node_set("blipp[2]",Res6),
    Res7 = xmerl_xpath:string("blipp[last()]",E),
    ok = check_node_set("blipp[last()]",Res7),
    Res8 = xmerl_xpath:string("*/blupp",E),
    ok = check_node_set("*/blupp",Res8),
    Res9 = xmerl_xpath:string("/myBS_model/blipp[3]/blupp[2]",E),
    ok = check_node_set("/myBS_model/blipp[3]/blupp[2]",Res9),
    Res10 = xmerl_xpath:string("blipp//plopp",E),
    ok = check_node_set("blipp//plopp",Res10),
    Res11 = xmerl_xpath:string("//plopp",E),
    ok = check_node_set("//plopp",Res11),
    Res12 = xmerl_xpath:string("//blupp/plopp",E),
    ok = check_node_set("//blupp/plopp",Res12),
    Res13 = xmerl_xpath:string(".",E),
    ok = check_node_set(".",Res13),
    Res14 = xmerl_xpath:string(".//blipp2",E),
    ok = check_node_set(".//blipp2",Res14),
    Res15 = xmerl_xpath:string(".//blipp2/blupp/plopp/..",E),
    ok = check_node_set(".//blipp2/blupp/plopp/..",Res15),
    Res16 = xmerl_xpath:string(".//blipp[2]/blupp/plopp/../@att2",E),
    ok = check_node_set(".//blipp[2]/blupp/plopp/../@att2",Res16),
    Res17 = xmerl_xpath:string(".//blipp/blupp/plopp[2]/../@att2",E),
    ok = check_node_set(".//blipp/blupp/plopp[2]/../@att2",Res17),
    Res18 = xmerl_xpath:string("blipp[@id='name2']",E),
    ok = check_node_set("blipp[@id='name2']",Res18),
    Res19 = xmerl_xpath:string("blipp[@id='name2'][3]",E),
    ok = check_node_set("blipp[@id='name2'][3]",Res19),
    Res20 = xmerl_xpath:string("//blupp[plopp=\"here are some more text\"]",E),
    ok = check_node_set("//blupp[plopp=\"here are some more text\"]",Res20),
    Res21 = xmerl_xpath:string("//blupp[plopp]",E),
    ok = check_node_set("//blupp[plopp]",Res21),
    Res22 = xmerl_xpath:string("blipp[@id and @test]",E),
    ok = check_node_set("blipp[@id and @test]",Res22).

check_node_set("blipp",[E1,E2,E3]) -> 
    ok = xml_element_name(E1,blipp),
    ok = xml_element_name(E2,blipp),
    ok = xml_element_name(E3,blipp),
    ok;
check_node_set("*",[E1,E2,E3,E4]) ->
    ok = xml_element_name(E1,blipp),
    ok = xml_element_name(E2,blipp),
    ok = xml_element_name(E3,blipp),
    ok = xml_element_name(E4,blipp2),
    ok;
check_node_set("blipp/blupp/plopp/text()",[T1,T2]) ->
    #xmlText{value="here are some text"} = T1,
    #xmlText{value="here are some more text"} = T2,
    ok;
check_node_set("blipp/blupp/@att2",[A1,A2]) ->
    #xmlAttribute{name=att2} = A1,
    #xmlAttribute{name=att2} = A2,
    ok;
check_node_set("blipp/@*",[A1,A2,A3,A4]) ->
    #xmlAttribute{} = A1,
    #xmlAttribute{} = A2,
    #xmlAttribute{} = A3,
    #xmlAttribute{} = A4,
    ok;
check_node_set("blipp[2]",[E]) ->
    #xmlElement{name=blipp,
                attributes=[#xmlAttribute{name=id,value="name2"}]} = E,
    ok;
check_node_set("blipp[last()]",[E]) ->
    #xmlElement{name=blipp,
                attributes=[#xmlAttribute{name=id,value="name3"}|_]} = E,
    ok;
check_node_set("*/blupp",[E1,E2,E3,E4,E5,E6]) ->
    ok = xml_element_name(E1,blupp),
    ok = xml_element_name(E2,blupp),
    ok = xml_element_name(E3,blupp),
    ok = xml_element_name(E4,blupp),
    ok = xml_element_name(E5,blupp),
    ok = xml_element_name(E6,blupp),
    ok;
check_node_set("/myBS_model/blipp[3]/blupp[2]",[E]) ->
    #xmlElement{name=blupp,
                attributes=[#xmlAttribute{name=att,value="bluppc2"}]}=E,
    ok;
check_node_set("blipp//plopp",[#xmlElement{name=plopp},#xmlElement{name=plopp}]) -> 
    ok;
check_node_set("//plopp",[E1,E2,E3]) ->
    ok = xml_element_name(E1,plopp),
    ok = xml_element_name(E2,plopp),
    ok = xml_element_name(E3,plopp),
    ok;
check_node_set("//blupp/plopp",[E1,E2,E3]) ->
    ok = xml_element_name(E1,plopp),
    ok = xml_element_name(E2,plopp),
    ok = xml_element_name(E3,plopp),
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
    #xmlElement{name=blipp,
                attributes=[#xmlAttribute{name=id,value="name2"}]}=E,
    ok;
check_node_set("blipp[@id='name2'][3]",[]) ->
    ok;
check_node_set("//blupp[plopp=\"here are some more text\"]",[E]) ->
    #xmlElement{name=blupp,
                content=[_T,#xmlElement{name=plopp,content=C}|_]} = E,
    true = lists:keymember("here are some more text",#xmlText.value,C),
    ok;
check_node_set("//blupp[plopp]",[E1,E2,E3]) ->
    #xmlElement{name=blupp,
                content=C1} = E1,
    true = lists:keymember(plopp,#xmlElement.name,C1),
    #xmlElement{name=blupp,
                content=C2} = E2,
    true = lists:keymember(plopp,#xmlElement.name,C2),
    #xmlElement{name=blupp,
                content=C3} = E3,
    true = lists:keymember(plopp,#xmlElement.name,C3),
    ok;
check_node_set("blipp[@id and @test]",[E]) ->
    #xmlElement{name=blipp,
                attributes=Atts} = E,
    true = lists:keymember(id,#xmlAttribute.name,Atts),
    true = lists:keymember(test,#xmlAttribute.name,Atts),
    ok;
check_node_set(Pattern,NodeSet) ->
    io:format("Pattern: ~p~nNodeSet: ~p~n",[Pattern,NodeSet]),
    error.

xml_element_name(E,N) ->
    #xmlElement{name=N} = E,
    ok.

ticket_6873() ->
    [#xmlElement{}] = xmerl_xpath:string("//foo[contains(@bar, 'oe')]",element(1,xmerl_scan:string("<foo bar=\"Joe\" />"))),
    ok.

ticket_7496() ->
    Test = fun(Doc, XPath, Exp) ->
                   Result = xmerl_xpath:string(XPath, Doc),
                   Exp = [Name || #xmlElement{name = Name} <- Result],
                   ok
           end,
    {Doc1,_} = xmerl_scan:string("<a><b/> <c/> <d/> <e/></a>"),
    ok = Test(Doc1, "//b/following::*", [c, d, e]),
    ok = Test(Doc1,"//b/following::*[1]", [c]),
    ok = Test(Doc1,"//b/following::*[position()=1]", [c]),
    ok = Test(Doc1,"//b/following::*[3]", [e]),
    ok = Test(Doc1,"//b/following::*[position()=3]", [e]),
    ok = Test(Doc1,"//e/preceding::*", [b, c, d]),
    ok = Test(Doc1,"//e/preceding::*[1]", [d]),
    ok = Test(Doc1,"//e/preceding::*[position()=1]", [d]),
    ok = Test(Doc1,"//e/preceding::*[3]", [b]),
    ok = Test(Doc1,"//e/preceding::*[position()=3]", [b]),
    ok = Test(Doc1,"//b/following::*[position() mod 2=0]", [d]),
    ok = Test(Doc1,"//b/self::*", [b]),

    {Doc2,_} = xmerl_scan:string("<a><b/> <c><d/></c> <e/> <f><g/></f> <h/> <i><j/></i> <k/></a>"),
    ok = Test(Doc2,"//g/preceding::*", [b, c, d, e]),
    ok = Test(Doc2, "//g/following::*", [h, i, j, k]),
    ok = Test(Doc2,"//g/ancestor::*", [a, f]),
    ok = Test(Doc2,"//g/ancestor::*[1]", [f]),
    ok = Test(Doc2,"//g/ancestor::*[2]", [a]),
    ok = Test(Doc2,"//g/ancestor-or-self::*", [a, f, g]),
    ok = Test(Doc2,"//g/ancestor-or-self::*[1]", [g]),
    ok = Test(Doc2,"//g/ancestor-or-self::*[2]", [f]),
    ok = Test(Doc2,"//g/ancestor-or-self::*[3]", [a]),
    ok = Test(Doc2,"/descendant::*", [a, b, c, d, e, f, g, h, i, j, k]),
    ok = Test(Doc2,"//f/preceding-sibling::*", [b, c, e]),
    ok = Test(Doc2,"//f/following-sibling::*", [h, i, k]),
    ok = Test(Doc2,"//f/self::*", [f]),
    ok = Test(Doc2,"//f/ancestor::*", [a]),
    ok = Test(Doc2,"//f/descendant::*", [g]),
    ok = Test(Doc2,"//f/preceding::*", [b, c, d, e]),
    ok = Test(Doc2,"//f/following::*", [h, i, j, k]),
    ok = Test(Doc2,"//text()[1]/following-sibling::*", [c, e, f, h, i, k]),

    {Doc3,_} = xmerl_scan:file("documentRoot.xml"),
    ok = Test(Doc3,"//child",[child,child,child]),
    ok = Test(Doc3,"//child[@name='beta']",[child]),
    [{xmlAttribute,id,[],[],[],_,1,[],"2",false}] =
    xmerl_xpath:string("/documentRoot/parent/child[@name='beta']/@id",Doc3),
    ok = Test(Doc3,"/documentRoot/parent/child|/documentRoot/parent/pet",
              [child,child,child,pet,pet]),
    ok = Test(Doc3,"//*[starts-with(local-name(),'p')]",
              [parent,pet,pet]).


functions() ->
    Test = fun(Doc, XPath, Exp) ->
                   Result = xmerl_xpath:string(XPath, Doc),
                   Exp = [begin
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
    ok = Test(Doc,"/foo/bar[name = 'Xml']/value/text()",["1"]),
    ok = Test(Doc,"/foo/bar/node()/text()",
              ["Xml","1","Xpath","2","Erlang","3"]),
    ok = Test(Doc,"/foo/bar[contains(name, 'path')]",[bar]),
    ok = Test(Doc,"/foo/bar[starts-with(name, 'X')]",[bar,bar]),
    ok = Test(Doc,"/foo/bar[value = string(1)]/value/text()",["1"]),


    {Doc2,_}= xmerl_scan:file("purchaseOrder.xml"),
    ok = Test(Doc2,"//*[starts-with(local-name(),'c')]",
              ['apo:comment',city,city,comment]),
    ok = Test(Doc2,"//*[starts-with(name(),'c')]",
              [city,city,comment]),
    ok = Test(Doc2,"//*[starts-with(name(),'{http://www.example.com/PO1')]",
              ['apo:purchaseOrder','apo:comment']).


namespaces() ->
    {Doc,_} = xmerl_scan:file("purchaseOrder.xml", [{namespace_conformant, true}]),

    %% Element name using regular namespace and context namespace declaration.
    [#xmlElement{nsinfo = {_, "purchaseOrder"}}] =
    xmerl_xpath:string("/apo:purchaseOrder", Doc),
    [#xmlElement{nsinfo = {_, "purchaseOrder"}}] =
    xmerl_xpath:string("/t:purchaseOrder", Doc, [{namespace, [{"t", "http://www.example.com/PO1"}]}]),

    %% Wildcard element name using regular namespace and context namespace declaration.
    [#xmlElement{nsinfo = {_, "comment"}}] =
    xmerl_xpath:string("./apo:*", Doc),
    [#xmlElement{nsinfo = {_, "comment"}}] =
    xmerl_xpath:string("./t:*", Doc, [{namespace, [{"t", "http://www.example.com/PO1"}]}]),

    %% Attribute name using regular namespace and context namespace declaration.
    [#xmlAttribute{nsinfo = {_, "type"}}, #xmlAttribute{nsinfo = {_, "type"}}] =
    xmerl_xpath:string("//@xsi:type", Doc),
    [#xmlAttribute{nsinfo = {_, "type"}}, #xmlAttribute{nsinfo = {_, "type"}}] =
    xmerl_xpath:string("//@t:type", Doc, [{namespace, [{"t", "http://www.w3.org/2001/XMLSchema-instance"}]}]),

    %% Wildcard attribute name using regular namespace and context namespace declaration.
    [#xmlAttribute{nsinfo = {_, "type"}}, #xmlAttribute{nsinfo = {_, "type"}}] =
    xmerl_xpath:string("//@xsi:*", Doc),
    [#xmlAttribute{nsinfo = {_, "type"}}, #xmlAttribute{nsinfo = {_, "type"}}] =
    xmerl_xpath:string("//@t:*", Doc, [{namespace, [{"t", "http://www.w3.org/2001/XMLSchema-instance"}]}]),
    ok.
