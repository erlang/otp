-module(many_live_xregs).
-export([many_args/100]).

many_args(
    X00, X01, X02, X03, X04, X05, X06, X07, X08, X09,
    X10, X11, X12, X13, X14, X15, X16, X17, X18, X19,
    X20, X21, X22, X23, X24, X25, X26, X27, X28, X29,
    X30, X31, X32, X33, X34, X35, X36, X37, X38, X39,
    X40, X41, X42, X43, X44, X45, X46, X47, X48, X49,
    X50, X51, X52, X53, X54, X55, X56, X57, X58, X59,
    X60, X61, X62, X63, X64, X65, X66, X67, X68, X69,
    X70, X71, X72, X73, X74, X75, X76, X77, X78, X79,
    X80, X81, X82, X83, X84, X85, X86, X87, X88, X89,
    X90, X91, X92, X93, X94, X95, X96, X97, X98, X99
) ->
    Acc = X01,
    case X00 of
        0 -> X01;
        _ ->
            SumX02toX99 = lists:sum([
                          X02, X03, X04, X05, X06, X07, X08, X09,
                X10, X11, X12, X13, X14, X15, X16, X17, X18, X19,
                X20, X21, X22, X23, X24, X25, X26, X27, X28, X29,
                X30, X31, X32, X33, X34, X35, X36, X37, X38, X39,
                X40, X41, X42, X43, X44, X45, X46, X47, X48, X49,
                X50, X51, X52, X53, X54, X55, X56, X57, X58, X59,
                X60, X61, X62, X63, X64, X65, X66, X67, X68, X69,
                X70, X71, X72, X73, X74, X75, X76, X77, X78, X79,
                X80, X81, X82, X83, X84, X85, X86, X87, X88, X89,
                X90, X91, X92, X93, X94, X95, X96, X97, X98, X99
            ]),
            many_args(
                X00-1,
                Acc + SumX02toX99,
                          X02, X03, X04, X05, X06, X07, X08, X09,
                X10, X11, X12, X13, X14, X15, X16, X17, X18, X19,
                X20, X21, X22, X23, X24, X25, X26, X27, X28, X29,
                X30, X31, X32, X33, X34, X35, X36, X37, X38, X39,
                X40, X41, X42, X43, X44, X45, X46, X47, X48, X49,
                X50, X51, X52, X53, X54, X55, X56, X57, X58, X59,
                X60, X61, X62, X63, X64, X65, X66, X67, X68, X69,
                X70, X71, X72, X73, X74, X75, X76, X77, X78, X79,
                X80, X81, X82, X83, X84, X85, X86, X87, X88, X89,
                X90, X91, X92, X93, X94, X95, X96, X97, X98, X99)
    end.

%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
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
