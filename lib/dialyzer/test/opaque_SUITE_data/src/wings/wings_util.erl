%% This software is copyrighted by Bjorn Gustavsson, and other parties.
%% The following terms apply to all files associated with the software unless
%% explicitly disclaimed in individual files.
%%
%% The authors hereby grant permission to use, copy, modify, distribute,
%% and license this software and its documentation for any purpose, provided
%% that existing copyright notices are retained in all copies and that this
%% notice is included verbatim in any distributions. No written agreement,
%% license, or royalty fee is required for any of the authorized uses.
%% Modifications to this software may be copyrighted by their authors
%% and need not follow the licensing terms described here, provided that
%% the new terms are clearly indicated on the first page of each file where
%% they apply.
%%
%% IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
%% FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
%% ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
%% DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%
%% THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
%% IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
%% NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
%% MODIFICATIONS.
%%
%% GOVERNMENT USE: If you are acquiring this software on behalf of the
%% U.S. government, the Government shall have only "Restricted Rights"
%% in the software and related documentation as defined in the Federal
%% Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
%% are acquiring the software on behalf of the Department of Defense, the
%% software shall be classified as "Commercial Computer Software" and the
%% Government shall have only "Restricted Rights" as defined in Clause
%% 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
%% authors grant the U.S. Government and others acting in its behalf
%% permission to use and distribute the software in accordance with the
%% terms specified in this license.


%%
%%  wings_util.erl --
%%
%%     Various utility functions that not obviously fit somewhere else.
%%

-module(wings_util).

-export([gb_trees_smallest_key/1, gb_trees_largest_key/1,
	 gb_trees_map/2, rel2fam/1]).

-include("wings.hrl").

rel2fam(Rel) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(Rel))).

%% a definition that does not violate the opacity of gb_trees:tree()
gb_trees_smallest_key(Tree) ->
    {Key, _V} = gb_trees:smallest(Tree),
    Key.

%% a definition that violates the opacity of gb_trees:tree()
gb_trees_largest_key({_, Tree}) ->
    largest_key1(Tree).

largest_key1({Key, _Value, _Smaller, nil}) ->
    Key;
largest_key1({_Key, _Value, _Smaller, Larger}) ->
    largest_key1(Larger).

gb_trees_map(F, {Size,Tree}) ->
    {Size,gb_trees_map_1(F, Tree)}.

gb_trees_map_1(_, nil) -> nil;
gb_trees_map_1(F, {K,V,Smaller,Larger}) ->
    {K,F(K, V),
     gb_trees_map_1(F, Smaller),
     gb_trees_map_1(F, Larger)}.
