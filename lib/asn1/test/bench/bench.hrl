%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
%%
-define(rep5(X), X, X, X, X, X).
-define(rep10(X), ?rep5(X), ?rep5(X)).
-define(rep20(X), ?rep10(X), ?rep10(X)).
-define(rep40(X), ?rep20(X), ?rep20(X)).
-define(rep80(X), ?rep40(X), ?rep40(X)).
