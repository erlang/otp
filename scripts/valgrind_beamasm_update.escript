#!/usr/bin/env escript

-mode(compile).

main([VGFile,PerfFile]) ->
    ets:new(perf, [ordered_set, {keypos,1}, named_table]),
    {ok, Perf} = file:read_file(PerfFile),
    {ok, VGIo} = file:open(VGFile, [read,binary]),
    parse_perf(Perf),
    case update_vg(VGIo) of
        {ok, Out} ->
            file:write_file(VGFile, Out);
        {error, Error} ->
            io:format(standard_error, "Error ~p", [Error]),
            exit(1)
    end.

parse_perf(Perf) ->
    %% Example: 0x409b1c0 84 $global::arith_compare_shared
    lists:foreach(
      fun(<<>>) ->
              ok;
         (Line) ->
              [<<"0x",Base/binary>>, Size, Name] = string:split(Line," ",all),
              Start = binary_to_integer(Base, 16),
              End = Start + binary_to_integer(Size, 16),
              ets:insert(perf, [{Start, End, Name}])
      end,string:split(Perf,"\n",all)).

update_vg(VGIo) ->
    {ok, RegularPattern} = re:compile("(?:by|at) 0x([0-9A-F]+): (\\?\\?\\?)"),
    {ok, XmlPattern} = re:compile("(<ip>0x([0-9A-F]+)</ip>)"),
    update_vg(VGIo, RegularPattern, XmlPattern, [], #{}).

update_vg(VGIo, RegularPattern, XmlPattern, Acc, AddrCache) ->
    case io:get_line(VGIo, "") of
        eof ->
            {ok, lists:reverse(Acc)};
        {error, _} = Error ->
            Error;
        Line ->
            {Line1, AddrCache1} = update_vg0(Line, RegularPattern, XmlPattern, AddrCache),
            update_vg(VGIo, RegularPattern, XmlPattern, [Line1|Acc], AddrCache1)
    end.

update_vg0(Line, RegularPattern, XmlPattern, AddrCache) ->
    %% Check if regular log file
    case re:run(Line,RegularPattern,[global]) of
        {match, Matches} ->
            lists:foldl(
              fun(Match, {File, Cache}) ->
                      [_,Base, Replace] = Match,
                      case find_replacement_cached(binary_to_integer(binary:part(Line,Base),16), Cache) of
                          {undefined, Cache1} ->
                              {File, Cache1};
                          {Replacement, Cache1} ->
                              {replace(File,Replace,Replacement), Cache1}
                      end
              end, {Line, AddrCache},
              %% Run replacements in reverse in order to not invalidate
              %% the positions as we update the contents.
              lists:reverse(Matches));
        _ ->
            %% Check if xml log file
            case re:run(Line,XmlPattern,[global]) of
                {match, Matches} ->
                    lists:foldl(
                      fun(Match, {File, Cache}) ->
                              [_,Replace,Base] = Match,
                              case find_replacement_cached(binary_to_integer(binary:part(Line,Base),16), Cache) of
                                  {undefined, Cache1} ->
                                      {File, Cache1};
                                  {Replacement, Cache1} ->
                                      Xml = ["<ip>0x",binary:part(Line,Base),"</ip>\n"
                                             "      <obj>JIT code</obj>\n"
                                             "      <fn>",Replacement,"</fn>\n"
                                             "      <dir></dir>\n"
                                             "      <file></file>\n"
                                             "      <line></line>"],
                                      {replace(File,Replace,Xml), Cache1}
                              end
                      end, {Line, AddrCache},
                      %% Run replacements in reverse in order to not invalidate
                      %% the positions as we update the contents.
                      lists:reverse(Matches));
                _ ->
                    {Line, AddrCache}
            end
    end.

find_replacement_cached(Addr, Cache) ->
    case Cache of
        #{Addr := Result} ->
            {Result, Cache};
        _ ->
            Result = find_replacement(Addr),
            {Result, Cache#{Addr => Result}}
    end.


find_replacement(Addr) ->
    MatchSpec = [{{'$1','$2','$3'},
      [{'andalso',{'>=', Addr,'$1'},
                {'=<', Addr,'$2'}}],
      [{{'$1','$3'}}]}],
    case ets:select(perf, MatchSpec, 1) of
        [] ->
            undefined;
        '$end_of_table' ->
            undefined;
        {[{Start, Name}], _} ->
            [Name,"+",integer_to_list(Addr - Start, 16)]
    end.

replace(Bin,{Start,Len},What) ->
    [string:slice(Bin,0,Start),
     What,
     string:slice(Bin,Start+Len)].
