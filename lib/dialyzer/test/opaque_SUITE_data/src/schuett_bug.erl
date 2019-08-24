%%---------------------------------------------------------------------------
%% From: Thorsten Schuett <schuett@zib.de>
%% Date: 7 July 2010
%%
%% When I run dialyzer of R14A on the attached code, it complains about
%% the new_neighborhood/1 function:
%% nodelist.erl:12: Invalid type specification for function
%% nodelist:new_neighborhood/1. The success typing is (_) -> {[any(),...]}
%%
%% However, when I change the type nodelist() from opaque to non-opaque
%% (see comment), dialyzer accepts the code. The types seem to be correct.
%% The problem seems to be with nested opaque types.
%%---------------------------------------------------------------------------

-module(schuett_bug).

-export([new_neighborhood/1]).

-export_type([nodelist/0, neighborhood/0]).

-type node_type() :: 'node_type'.

-opaque nodelist() :: [node_type(),...]. % change to -type
-opaque neighborhood() :: {nodelist()}.

-spec new_neighborhood(Node::node_type()) -> neighborhood().
new_neighborhood(Node) ->
    {[Node]}.
