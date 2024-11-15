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
%%  wings_shape.erl --
%%
%%     Utilities for shape records.
%%

-module(wings_shape).

-export([insert/3]).

-include("wings.hrl").

%%%
%%% Exported functions.
%%%

%% new(We, Suffix, St0) -> St.
%%  Suffix = cut | clone | copy | extract | sep
%%
%%  Create a new object based on an old object. The name
%%  will be created from the old name (with digits and known
%%  suffixes stripped) with the given Suffix and a number
%%  appended.
insert(#we{name=OldName}=We0, Suffix, #st{shapes=Shapes0,onext=Oid}=St) ->
    Name = new_name(OldName, Suffix, Oid),
    We = We0#we{id=Oid,name=Name},
    Shapes = gb_trees:insert(Oid, We, Shapes0),
    St#st{shapes=Shapes,onext=Oid+1}.

%%%
%%% Local functions follow.
%%%

new_name(OldName, Suffix0, Id) ->
    Suffix = suffix(Suffix0),
    Base = base(lists:reverse(OldName)),
    lists:reverse(Base, "_" ++ Suffix ++ integer_to_list(Id)).

%% Note: Filename suffixes are intentionally not translated.
%% If we are to translate them in the future, base/1 below
%% must be updated to strip suffixes (both for the current language
%% and for English).

suffix(cut) -> "cut";
suffix(clone) -> "clone";
suffix(copy) -> "copy";
suffix(extract) -> "extract";
suffix(mirror) -> "mirror";
suffix(sep) -> "sep".

%% base_1(ReversedName) -> ReversedBaseName
%%  Given an object name, strip digits and known suffixes to
%%  create a base name. Returns the unchanged name if
%%  no known suffix could be stripped.

base(OldName) ->
    case base_1(OldName) of
	error -> OldName;
	Base -> Base
    end.

base_1([H|T]) when $0 =< H, H =< $9 -> base_1(T);
base_1("tuc_"++Base) -> Base;			%"_cut"
base_1("enolc_"++Base) -> Base;			%"_clone"
base_1("ypoc_"++Base) -> Base;			%"_copy"
base_1("tcartxe_"++Base) -> Base;		%"_extract"
base_1("rorrim_"++Base) -> Base;		%"_mirror"
base_1("pes_"++Base) -> Base;			%"_sep"
base_1(_Base) -> error.
