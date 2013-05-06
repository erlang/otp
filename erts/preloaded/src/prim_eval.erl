%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(prim_eval).

%% This module is simply a stub which abstract code gets included in the result
%% of compilation of prim_eval.S, to keep Dialyzer happy.

-export(['receive'/2]).

-spec 'receive'(fun((term()) -> nomatch | T), timeout()) -> T.
'receive'(_, _) ->
    erlang:nif_error(stub).
