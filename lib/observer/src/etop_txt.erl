%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
-module(etop_txt).
-moduledoc false.
-author('siri@erix.ericsson.se').

%%-compile(export_all).
-export([init/1,stop/1]).
-export([do_update/4, newline/2]).

-include("etop.hrl").
-include("etop_defs.hrl").

-define(DEFAULT_WIDTH, 89).
-define(ERASE_ALL, "\e[;H\e[2J").

-define(SYSFORM(IsTerminal, ShellMode),
        " ~-72w~10s" ++ newline(IsTerminal, ShellMode) ++
            " Load:  cpu  ~8w               Memory:  total    ~8w    binary   ~8w" ++
            newline(IsTerminal, ShellMode) ++
            "        procs~8w                        processes~8w    code     ~8w" ++
            newline(IsTerminal, ShellMode) ++
            "        runq ~8w                        atom     ~8w    ets      ~8w" ++
            newline(IsTerminal, ShellMode)).

-record(field_widths, {cols      :: pos_integer(),
                       used_cols :: pos_integer(),
                       init_func :: pos_integer(),
                       reds      :: pos_integer(),
                       mem       :: pos_integer(),
                       msgq      :: pos_integer(),
                       curr_func :: pos_integer()}).

-import(etop,[loadinfo/2,meminfo/2]).

stop(Pid) -> Pid ! stop.

init(Config) ->
    loop(#etop_info{},Config).

loop(Prev,Config) ->
    try do_update(Prev,Config) of
        Info ->
            receive
                stop -> stopped;
                {dump,Fd} -> do_update(Fd,Info,Prev,Config), loop(Info,Config);
                {config,_,Config1} -> loop(Info,Config1)
            after Config#opts.intv -> loop(Info,Config)
            end
    catch error : badarg : ST ->
            receive
                %% Accumulator tab could have been already deleted if stop is requested
                %% Ignore this if we're already stopping
                stop -> stopped
            after 0 -> erlang:raise(error, badarg, ST)
            end
    end.

do_update(Prev,Config) ->
    Info = etop:update(Config),
    do_update(standard_io,Info,Prev,Config).

do_update(Fd,Info,Prev,Config) ->
    {Cpu,NProcs,RQ,Clock} = loadinfo(Info,Prev),
    FieldWidths = calc_field_widths(Info#etop_info.procinfo),

    IsTerminal = proplists:is_defined(terminal, io:getopts(Fd)),
    ShellMode = Config#opts.shell_mode,

    case IsTerminal of
        true ->
            io:fwrite(Fd, ?ERASE_ALL ++ newline(IsTerminal, ShellMode), []);
        false ->
            io:fwrite(Fd, newline(IsTerminal, ShellMode), [])
    end,
    writedoubleline(Fd, FieldWidths, IsTerminal, ShellMode),
    case Info#etop_info.memi of
	undefined ->
	    io:fwrite(Fd, " ~-72w~10s" ++ newline(IsTerminal, ShellMode) ++
		      " Load:  cpu  ~8w" ++ newline(IsTerminal, ShellMode) ++
		      "        procs~8w" ++ newline(IsTerminal, ShellMode) ++
		      "        runq ~8w" ++ newline(IsTerminal, ShellMode),
		      [Config#opts.node,Clock,
		       Cpu,NProcs,RQ]);
	Memi ->
	    [Tot,Procs,Atom,Bin,Code,Ets] = 
		meminfo(Memi, [total,processes,atom,binary,code,ets]),
	    io:fwrite(Fd, ?SYSFORM(IsTerminal, ShellMode),
		      [Config#opts.node,Clock,
		       Cpu,Tot,Bin,
		       NProcs,Procs,Code,
		       RQ,Atom,Ets])
    end,
    io:fwrite(Fd, newline(IsTerminal, ShellMode), []),
    writepinfo_header(Fd, FieldWidths, IsTerminal, ShellMode),
    writesingleline(Fd, FieldWidths, IsTerminal, ShellMode),
    writepinfo(Fd, Info#etop_info.procinfo, modifier(Fd), FieldWidths, IsTerminal, ShellMode),
    writedoubleline(Fd, FieldWidths, IsTerminal, ShellMode),
    case {IsTerminal, ShellMode} of
        {true, raw} ->
            io:fwrite(Fd, newline(IsTerminal, ShellMode) ++ "Press 'q' to stop etop." ++
                          newline(IsTerminal, ShellMode), []);
        {true, _} ->
            io:fwrite(Fd, newline(IsTerminal, ShellMode) ++ "Type Ctrl+G, then enter 'i' to interrupt etop." ++
                          newline(IsTerminal, ShellMode), []);
        {false, _} ->
            ok
    end,
    Info.


calc_field_widths(ProcInfoL) ->
    Cols = case io:columns() of
               {ok, IoCols}     -> max(IoCols, ?DEFAULT_WIDTH);
               {error, enotsup} -> ?DEFAULT_WIDTH
           end,

    %% There is a priority with the field calculation since it's done
    %% sequential with all fields deducted with their default values from
    %% start. If any field eventually exceed the column budget, the rest of the
    %% columns will get their default widths.

    %% Calculate columns left. NOTE: The extra spaces between reds/mem/msgq.
    %% Subsequent calculations adds default width then deducts calculated width.
    %% See proc_format/2 for fields format.
    %% Columns: pid, init_func, time, reds, mem, msgq, curr_func.
    ColsLeft0 = Cols - 15 - 20 - 8 - 8 - 1 - 8 - 1 - 8 - 20,

    RedsWidth = get_width(reds, ProcInfoL, ColsLeft0),

    ColsLeft1 = ColsLeft0 + 8 - RedsWidth,

    MemWidth = get_width(mem, ProcInfoL, ColsLeft1),

    ColsLeft2 = ColsLeft1 + 8 - MemWidth,

    MsgQWidth= get_width(msgq, ProcInfoL, ColsLeft2),

    ColsLeft3 = ColsLeft2 + 8 - MsgQWidth,

    %% Use the rest for initial and current function fields
    if ColsLeft3 > 0 ->
           %% compensate field start width for rounding
           FieldSize = 19 + round((ColsLeft3 - 1) / 2),
           InitFuncWidth = FieldSize,
           CurrFuncWidth = FieldSize;
       true ->
           InitFuncWidth = 20,
           CurrFuncWidth = 20
    end,

    %% Extra space between reds/mem, extra space due to rounding
    UsedCols =
        15 + InitFuncWidth + 8 + RedsWidth + 1 + MemWidth + 1
        + MsgQWidth + CurrFuncWidth + 1,

    #field_widths{cols = Cols,
                  used_cols = UsedCols,
                  init_func = InitFuncWidth,
                  reds = RedsWidth,
                  mem = MemWidth,
                  msgq = MsgQWidth,
                  curr_func = CurrFuncWidth}.


get_width(reds, ProcInfoL, ColsLeft) ->
    get_width(4, ProcInfoL, ColsLeft);
get_width(mem, ProcInfoL, ColsLeft) ->
    get_width(3, ProcInfoL, ColsLeft);
get_width(msgq, ProcInfoL, ColsLeft) ->
    get_width(8, ProcInfoL, ColsLeft);

%% Get the maximum width of the field at place N in #sysinfo{}.
%% Calculate the maximum width by taking the maximum width (by taking common
%% logarithm of largest number) then check if is larger than 8 and that it
%% fits the column budget, if so return that value; otherwise return 8.
get_width(N, ProcInfoL, ColsLeft) ->
    MaxNum = lists:foldr(fun(Info, Acc)
                               when element(N, Info) > Acc ->
                                 element(N, Info);
                            (_, Acc) ->
                                 Acc
                         end,
                         0,
                         ProcInfoL),

    MaxWidth =
        if MaxNum > 0 -> round(math:log10(MaxNum)) + 1;
           true       -> 1  %% logarithm defined in R_n, n > 0.
        end,

    if MaxWidth > 8 andalso ColsLeft - MaxWidth > 0 ->
           MaxWidth;
       true ->
           8
    end.


writepinfo_header(Fd, #field_widths{init_func = InitFunc, reds = Reds,
                                    mem = Mem, msgq = MsgQ}, IsTerminal, ShellMode) ->
    %% Add spaces between variable width columns.
    Header =
        "Pid            Name or Initial Func"
        ++ lists:duplicate(max(InitFunc - 16, 4), $\s) ++
        "Time"
        ++ lists:duplicate(max(Reds - 4, 4), $\s) ++
        "Reds"
        ++ lists:duplicate(max(Mem - 5, 3), $\s) ++
        "Memory"
        ++ lists:duplicate(max(MsgQ - 3, 5), $\s) ++
        "MsgQ Current Function" ++ newline(IsTerminal, ShellMode),

    io:fwrite(Fd, Header, []).

writesingleline(Fd, FieldWidths, IsTerminal, ShellMode) ->
    writedupline(Fd, $-, FieldWidths, IsTerminal, ShellMode).
writedoubleline(Fd, FieldWidths, IsTerminal, ShellMode) ->
    writedupline(Fd, $=, FieldWidths, IsTerminal, ShellMode).

writedupline(Fd, Char, #field_widths{used_cols = UsedCols}, IsTerminal, ShellMode) ->
    Line = lists:duplicate(UsedCols, Char) ++ newline(IsTerminal, ShellMode),
    io:fwrite(Fd, Line, []).

writepinfo(Fd,[#etop_proc_info{pid=Pid,
			       mem=Mem,
			       reds=Reds,
			       name=Name,
			       runtime=Time,
			       cf=MFA,
			       mq=MQ}
	       |T],
           Modifier, FieldWidths, IsTerminal, ShellMode) ->
    io:fwrite(Fd,proc_format(Modifier, FieldWidths, IsTerminal, ShellMode),
              [Pid,to_string(Name,Modifier),Time,Reds,Mem,MQ,
               to_string(MFA,Modifier)]),
    writepinfo(Fd,T,Modifier, FieldWidths, IsTerminal, ShellMode);
writepinfo(_Fd,[],_,_,_,_) ->
    ok.

proc_format(Modifier, #field_widths{init_func = InitFunc, reds = Reds,
                                    mem = Mem, msgq = MsgQ,
                                    curr_func = CurrFunc}, IsTerminal, ShellMode) ->
    "~-15w"
    "~-" ++ i2l(InitFunc) ++ Modifier ++ "s"
    "~8w"
    "~" ++ i2l(Reds) ++ "w "
    "~" ++ i2l(Mem) ++"w "
    "~" ++ i2l(MsgQ) ++ "w "
    "~-" ++ i2l(CurrFunc) ++ Modifier ++ "s" ++ newline(IsTerminal, ShellMode).

to_string(Other,_Modifier) when is_binary(Other) ->
    Other;
to_string({M,F,A},Modifier) ->
    io_lib:format("~w:~"++Modifier++"w/~w",[M,F,A]);
to_string(Other,Modifier) ->
    io_lib:format("~"++Modifier++"w",[Other]).

i2l(I) -> integer_to_list(I).

modifier(Device) ->
    case encoding(Device) of
        latin1 -> "";
        _ -> "t"
    end.

encoding(Device) ->
    case io:getopts(Device) of
        List when is_list(List) ->
            proplists:get_value(encoding,List,latin1);
        _ ->
            latin1
    end.

newline(false, _) ->
    "~n";
newline(true, raw) ->
    "\r\n";
newline(true, _) ->
    "~n".

