%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2024 Vance Shipley <vances@sigscale.org>
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%%
%%
-module(test_exclusive_decode_rest).
-export([test/0]).

test() ->
	{ok, CDR} = 'SwCDR':encode('SwCDR', {origSvcCallRecord, orig_cdr()}),
	Bin = <<CDR/binary, CDR/binary>>,
	{ok, {origSvcCallRecord, _}, CDR} = 'SwCDR':decode_cdr(Bin).
	
orig_cdr() ->
	{'OrigSvcCallRecord', 1, 2, 3, <<145,65,97,85,21,50,244>>,
			<<145,65,97,85,85,118,248>>, <<145,65,97,85,85,118,248>>,
			<<2,0,2,4,1,0,0,5>>, <<2,0,3,2,4,5>>, 35000, 0, 0, 0, 3600,
			3600, 3600, 3600, 48, 48, 64, 64, 20, 20, 12, 12, 20, 20,
			4, 4, 1, 1, 3600, 3600, asn1_NOVALUE}.

