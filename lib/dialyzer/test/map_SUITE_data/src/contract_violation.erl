-module(contract_violation).

-export([entry/1, beam_disasm_lines/2]).

%%-----------------------------------------------------------------------

-type lines() :: #{non_neg_integer() => {string(), non_neg_integer()}}.

entry(Bin) ->
    I = 42,
    case beam_disasm_lines(Bin, ':-)') of
	#{I := Loc} -> {good, Loc};
	_ -> bad
    end.

-spec beam_disasm_lines(binary() | none, module()) -> lines().

beam_disasm_lines(none, _) -> #{};
beam_disasm_lines(<<NumLines:32, LineBin:NumLines/binary, FileBin/binary>>,
		  _Module) ->
    Lines = binary_to_term(LineBin),
    Files = binary_to_term(FileBin),
    lines_collect_items(Lines, Files, #{}).

lines_collect_items([], _, Acc) -> Acc;
lines_collect_items([{FileNo, LineNo}|Rest], Files, Acc) ->
    #{FileNo := File} = Files,
    lines_collect_items(
      Rest, Files, Acc#{map_size(Acc)+1 => {location, File, LineNo}}).
