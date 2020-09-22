#!/usr/bin/env escript

-mode(compile).

main([VGFile,PerfFile]) ->
    {ok, Perf} = file:read_file(PerfFile),
    {ok, VG} = file:read_file(VGFile),
    file:write_file(VGFile,update_vg(VG, parse_perf(Perf))).

parse_perf(Perf) ->
    %% Example: 0x409b1c0 84 $global::arith_compare_shared
    lists:foldl(
      fun(<<>>, Acc) ->
              Acc;
         (Line, Acc) ->
              [<<"0x",Base/binary>>, Size, Name] = string:split(Line," ",all),
              Acc#{ binary_to_integer(Base, 16) =>
                        {binary_to_integer(Size, 16), Name}}
      end,#{},string:split(Perf,"\n",all)).


update_vg(VG, Perf) ->
    %% Check if regular log file
    case re:run(VG,"(?:by|at) 0x([0-9A-F]+): (\\?\\?\\?)",[global]) of
        {match, Matches} ->
            lists:foldl(
              fun(Match, File) ->
                      [_,Base, Replace] = Match,
                      case find_replacement(binary_to_integer(binary:part(VG,Base),16), Perf) of
                          undefined ->
                              File;
                          Replacement ->
                              replace(File,Replace,Replacement)
                      end
              end, VG,
              %% Run replacements in reverse in order to not invalidate
              %% the positions as we update the contents.
              lists:reverse(Matches));
        _ ->
            %% Check if xml log file
            case re:run(VG,"(<ip>0x([0-9A-F]+)</ip>)",[global]) of
                {match, Matches} ->
                    lists:foldl(
                      fun(Match, File) ->
                              [_,Replace,Base] = Match,
                              case find_replacement(binary_to_integer(binary:part(VG,Base),16), Perf) of
                                  undefined ->
                                      File;
                                  Replacement ->
                                      Xml = ["<ip>0x",binary:part(VG,Base),"</ip>\n"
                                             "      <obj>JIT code</obj>\n"
                                             "      <fn>",Replacement,"</fn>\n"
                                             "      <dir></dir>\n"
                                             "      <file></file>\n"
                                             "      <line></line>"],
                                      replace(File,Replace,Xml)
                              end
                      end, VG,
                      %% Run replacements in reverse in order to not invalidate
                      %% the positions as we update the contents.
                      lists:reverse(Matches));
                _ ->
                    VG
            end
    end.

find_replacement(Addr, Perf) when is_map(Perf) ->
    find_replacement(Addr, maps:iterator(Perf));
find_replacement(Addr, Iter) ->
    case maps:next(Iter) of
        {Base,{Size,Str},Next} ->
            if Base =< Addr andalso Addr < Base + Size ->
                    [Str,"+",integer_to_list(Addr - Base, 16)];
               true ->
                    find_replacement(Addr,Next)
            end;
        none ->
            undefined
    end.

replace(Bin,{Start,Len},What) ->
    [string:slice(Bin,0,Start),
     What,
     string:slice(Bin,Start+Len)].
