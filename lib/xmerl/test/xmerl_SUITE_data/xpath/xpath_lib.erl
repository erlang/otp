%%%-------------------------------------------------------------------
%%% File    : xpath_lib.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 13 Jan 2006 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(xpath_lib).

-export([test/0,check_node_set/2,ticket_6873/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

test() ->
    {E,_} = xmerl_scan:file("myBS_model.xml"),
    Res1 = xmerl_xpath:string("blipp",E),
    ok = check_node_set("blipp",Res1),
    Res2 = xmerl_xpath:string("//blipp",E),
    ok = check_node_set("//blipp",Res2),
    Res3 = xmerl_xpath:string("/myBS_model/blipp",E),
    ok = check_node_set("/myBS_model/blipp",Res3),
    Res4 = xmerl_xpath:string("blipp[@id=\"name1\"]",E),
    ok = check_node_set("blipp[@id=\"name1\"]",Res4),
    Res5 = xmerl_xpath:string("//blipp[@id=\"name1\"]",E),
    ok = check_node_set("//blipp[@id=\"name1\"]",Res5),
    Res6 = xmerl_xpath:string("/myBS_model/blipp[@id=\"name1\"]",E),
    ok = check_node_set("/myBS_model/blipp[@id=\"name1\"]",Res6).


check_node_set("blipp",[H1,H2]) ->
    #xmlElement{name = blipp} = H1,
    #xmlElement{name = blipp} = H2,
    ok;
check_node_set("//blipp",[H1,H2]) ->
    #xmlElement{name = blipp} = H1,
    #xmlElement{name = blipp} = H2,
    ok;
check_node_set("/myBS_model/blipp",[H1,H2]) ->
    #xmlElement{name = blipp} = H1,
    #xmlElement{name = blipp} = H2,
    ok;
check_node_set("blipp[@id=\"name1\"]",[H]) ->
    H#xmlElement{attributes=#xmlAttribute{name=id,value="name1"}},
    ok;
check_node_set("//blipp[@id=\"name1\"]",[H]) ->
    H#xmlElement{attributes=#xmlAttribute{name=id,value="name1"}},
    ok;
check_node_set("/myBS_model/blipp[@id=\"name1\"]",[H]) ->
    H#xmlElement{attributes=#xmlAttribute{name=id,value="name1"}},
    ok.

ticket_6873() ->
    GetId = fun(Atts) ->
                    case lists:keysearch(id,#xmlAttribute.name,Atts) of
                        {value,#xmlAttribute{value=AttV}} -> AttV;
                        _ -> novalue
                    end
            end,

    Test = fun(Doc, XPath, Exp) ->
                   Result = xmerl_xpath:string(XPath, Doc),
                   Exp = [begin
                              case Obj of
                                  #xmlElement{name = EName,attributes=Atts} ->
                                      {EName,GetId(Atts)};
                                  #xmlAttribute{name = AName} ->
                                      AName;
                                  #xmlText{value=Text} ->
                                      Text
                              end
                          end|| Obj <- Result],
                   ok
           end,

    Doc1 = get_doc("e1074"),
    ok = Test(Doc1,"/*",[{root,"1"}]),
    ok = Test(Doc1,"/root",[{root,"1"}]),
    ok = Test(Doc1,"/root/*",[{elem1,"2"},{elem1,"8"},{e,"12"}]),
    ok = Test(Doc1,"/root/e",[{e,"12"}]),
    ok = Test(Doc1,"//e",[{e,"12"},{e,"4"},{e,"6"},{e,"10"},{e,"11"}]),
    ok = Test(Doc1,"//*[name() != 'e']",
              [{root,"1"},{elem1,"2"},{elem1,"8"},{elem2,"3"},
               {elem3,"5"},{elem3,"7"},{elem2,"9"}]),
    ok = Test(Doc1,"//elem1/e",[{e,"10"},{e,"11"}]),
    ok = Test(Doc1,"//elem1//e",[{e,"4"},{e,"6"},{e,"10"},{e,"11"}]),
    ok = Test(Doc1,"//*[*]",
              [{root,"1"},{elem1,"2"},{elem1,"8"},
               {elem2,"3"},{elem3,"5"}]),
    ok = Test(Doc1,"//*[not(*)]",
              [{e,"12"},{e,"4"},{elem3,"7"},{e,"6"},
               {elem2,"9"},{e,"10"},{e,"11"}]),
    %% contents would be empty in the above expression
    [#xmlElement{content=[]}|_] = xmerl_xpath:string("//*[not(*)]",Doc1),
    ok = Test(Doc1,"//*[e]",[{root,"1"},{elem1,"8"},{elem2,"3"},{elem3,"5"}]),
    ok = Test(Doc1,"//*[count(e)>1]",[{elem1,"8"}]),
    ok = Test(Doc1,"//*[not(e) and name() != 'e']",
              [{elem1,"2"},{elem3,"7"},{elem2,"9"}]),
    ok = Test(Doc1,"/*/*/*/e",[{e,"4"}]),
    ok = Test(Doc1,"//*[starts-with(name(), 'el')]",
              [{elem1,"2"},{elem1,"8"},{elem2,"3"},
               {elem3,"5"},{elem3,"7"},{elem2,"9"}]),
    ok = Test(Doc1,"//*[contains(name(), 'lem1')]",
              [{elem1,"2"},{elem1,"8"}]),
    ok = Test(Doc1,"/*/e | //elem2/e",[{e,"4"},{e,"12"}]),

    io:format("Tested ~p~n",[e1074]),

    Doc2 = get_doc("e1075"),
    ok = Test(Doc2,"/*/*[1]",[{elem1,"2"}]),
    ok = Test(Doc2,"/root/elem1[2]",[{elem1,"8"}]),

    %% Get all first-born e elements in the document; that is, for all
    %% e elements with e element siblings, include only the first
    %% sibling.Note that //e[1] does not return the first e element in
    %% the document because the [1] predicate applies to e, which
    %% represents the set of e elements under one element and not to
    %% //e, which represents the set of e elements in the document.
    %%    ok = Test(Doc2,"//e[1]",[{e,"4"},{e,"6"},{e,"10"},{e,"12"}]),

    %% The following expression retrieves the first e element in the
    %% document:
    %%    ok = Test(Doc2,"(//e)[1]",[{e,4}]),

    %% For all e elements with e element siblings, include only the
    %% first 3 siblings
    %% ok = Test(Doc2,"//e[position() <= 3]",[{e,"4"},{e,"6"},{e,"10"},{e,"11"},{e,"12"}]),

    %% Get all last-born e elements in the document; that is, for all
    %% e elements with e element siblings, include only the last
    %% sibling
    %% ok = Test(Doc2,"//e[last()]",[{e,"4"},{e,"6"},{e,"11"},{e,"12"}]),

    %% Get the last e element in the document
    %% ok = Test(Doc2,"(//e)[last()]", [{e,"12"}]),

    io:format("Tested ~p~n",[e1075]),

    Doc3 = get_doc("e1076"),
    ok = Test(Doc3,"//*[.='cat']",[{elem1,"2"},{elem3,"6"}]),
    ok = Test(Doc3,"//*[.='dog']",[]),
    ok = Test(Doc3,"//*[contains(.,'cat')]", [{elem1,"2"},{elem1,"4"},{elem3,"6"}]),
    ok = Test(Doc3,"//elem3[contains(.,'cat')]",[{elem3,"6"}]),
    ok = Test(Doc3,"//*[contains(child::text(),'cat')]",[{elem1,"2"},{elem1,"4"},{elem3,"6"}]),
    ok = Test(Doc3,"//*[count(*)=0 and contains(.,'cat')]",[{elem1,"2"},{elem3,"6"}]),
    ok = Test(Doc3,"//*[contains(translate(.,'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'),'CAT')]",[{elem1,"2"},{elem1,"4"},{elem1,"7"},{elem3,"6"}]),

    io:format("Tested ~p~n",[e1076]),

    Doc4 = get_doc("e1078"),
    ok = Test(Doc4,"//*[@pet='cat']",[{elem1,"2"}]),
    ok = Test(Doc4,"//*[@pet='dog']",[{elem1,"7"}]),
    ok = Test(Doc4,"//*[contains(@pet,'dog')]",
              [{elem1,"3"},{elem1,"7"}]),
    ok = Test(Doc4,"//*[@age]",[{elem1,"3"},{elem3,"6"}]),
    ok = Test(Doc4,"//elem1[@age]",[{elem1,"3"}]),
    ok = Test(Doc4,"//*[@pet and @age]",[{elem1,"3"}]),
    ok = Test(Doc4,"//*[contains(translate(@pet,'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'),'CAT')]",[{elem1,"2"},{elem1,"4"}]),

    io:format("Tested ~p~n",[e1078]),

    Doc5 = get_doc("e1077"),
    ok = Test(Doc5,"id('3')",[{e,"3"}]),

    %% Get all e elements directly under element id 3
    %%    ok = Test(Doc5,"id('two')/e",[{e,"3"},{e,"4"},{e,"6"}]),
    ok = Test(Doc5,"id('two 3 seven the fifth')",[{e,"seven"},{e,"3"},{e,"two"}]),
    ok = Test(Doc5,"id('100')",[]),

    io:format("Tested ~p~n",[e1077]),
    ok.


get_doc(Name) ->
    {Doc,_} = xmerl_scan:file(Name++".xml"),
    Doc.
