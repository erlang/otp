%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-type rgba_float() :: {float(), float(), float(), float()}.
-type rgba_byte() :: {byte(), byte(), byte(), byte()}.
-type rgb() :: {byte(), byte(), byte()}.

-record(image_object, {
	type,
	points = [],
	span,
	internals,
	intervals,
	color}). % RGBA in float values

-record(image, {
	width,
	height,
	objects = [],
	background = {1.0,1.0,1.0,1.0},
	image}).

-define(debug, void).

-ifdef(debug).
-define(dbg(X), io:format("DEBUG: ~p:~p~n",[?MODULE, X])).
-else.
-define(dbg(X), void).
-endif.

