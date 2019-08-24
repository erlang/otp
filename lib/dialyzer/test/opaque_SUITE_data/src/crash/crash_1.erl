%%%-------------------------------------------------------------------
%%% From : Fredrik Thulin <ft@it.su.se>
%%%
%%% A module with an erroneous record field declaration which mixes up
%%% structured and opaque terms and causes a crash in dialyzer.
%%%
%%% In addition, it revealed that the compiler produced extraneous
%%% warnings about unused record definitions when in fact they are
%%% needed for type declarations. This is now fixed.
%%%-------------------------------------------------------------------
-module(crash_1).

-export([add/3, empty/0]).

%%--------------------------------------------------------------------

-record(sipurl,  {proto = "sip" :: string(), host :: string()}).
-record(keylist, {list = [] :: [_]}).
-type sip_headers() :: #keylist{}.
-record(request, {uri :: #sipurl{}, header :: sip_headers()}).
-type sip_request() :: #request{}.

%%--------------------------------------------------------------------

-record(target, {branch	:: string(), request :: sip_request()}).
-opaque target() :: #target{}.

-record(targetlist, {list :: target()}).  % XXX: THIS ONE SHOULD READ [target()]
-opaque targetlist() :: #targetlist{}.

%%====================================================================

add(Branch, #request{} = Request, #targetlist{list = L} = TargetList) ->
    case get_using_branch(Branch, TargetList) of
	none ->
	    NewTarget = #target{branch = Branch, request = Request},
	    #targetlist{list = L ++ [NewTarget]};
	#target{} ->
	    TargetList
    end.

-spec empty() -> targetlist().

empty() ->
    #targetlist{list = []}.

get_using_branch(Branch, #targetlist{list = L}) when is_list(Branch) ->
    get_using_branch2(Branch, L).

get_using_branch2(_Branch, []) ->
    none;
get_using_branch2(Branch, [#target{branch=Branch}=H | _T]) ->
    H;
get_using_branch2(Branch, [#target{} | T]) ->
    get_using_branch2(Branch, T).
