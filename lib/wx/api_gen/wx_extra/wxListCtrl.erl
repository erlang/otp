%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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
<<EXPORT:SortItems sortItems/2 SortItems:EXPORT>>

<<SortItems
%% @spec (This::wxListCtrl(), SortCallBack::function()) -> boolean()
%% @doc Sort the items in the list control<br />
%%   <pre>SortCalBack(Item1,Item2) -> integer()</pre>
%%  <br /> SortCallBack receives the client data associated with two items
%%         to compare, and should return 0 if the items are equal, a negative
%%         value if the first item is less than the second one and a positive
%%         value if the first item is greater than the second one.
%%  <br /> NOTE: The callback may not call other processes.
sortItems(#wx_ref{type=ThisT,ref=ThisRef}, SortCallBack)
  when is_function(SortCallBack, 2) ->
	?CLASS(ThisT,wxListCtrl),
	Sort = fun([Item1,Item2]) ->
			Result = SortCallBack(Item1,Item2),
			<<Result:32/?UI>>
		end,
	SortId = wxe_util:get_cbId(Sort),
	wxe_util:call(~s, <<ThisRef:32/?UI,SortId:32/?UI>>).
SortItems>>
