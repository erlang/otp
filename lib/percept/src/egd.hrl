%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
-define(dbg(X), ok).
-endif.
