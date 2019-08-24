%%%-------------------------------------------------------------------
%%% File    : xpath_text.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 14 Dec 2004 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(xpath_text).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xs, 
        [ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).


one() ->
    {A,_}=xmerl_scan:file('motorcycles.xml'),
    [["Suzuki","Yamaha"]] = template(A),
    ok.

%%% templates, test of OTP-5268
template(E = #xmlElement{name='motorcycles'}) ->
    [value_of(select("bike/name/manufacturer/text()",E))];
template(E) -> built_in_rules(fun template/1, E).

