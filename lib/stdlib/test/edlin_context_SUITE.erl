%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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
-module(edlin_context_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1]).

-export([get_context/1]).

suite() ->
    [{timetrap,{minutes,1}}].
all() ->
    [get_context].
groups() ->
    [].
init_per_suite(Config) ->
    Config.
end_per_suite(_Config) ->
    ok.

get_context(_Config) ->
    {term, [], {atom, "h"}} = edlin_context:get_context(lists:reverse("h")),
    {term} = edlin_context:get_context(lists:reverse("h(file")),
    {term} = edlin_context:get_context(lists:reverse("h(file,open")),
    {term, [{call, "h(file,open)"}], {atom, "h"}} = edlin_context:get_context(lists:reverse("h(file,open), h")),
    {term} = edlin_context:get_context(lists:reverse("h(file,open), h(file")),
    {term} = edlin_context:get_context(lists:reverse("h(file,open), h(file,open")),
    {term, [{call, "h(file,open)"}], {call, "h(file,open)"}} = edlin_context:get_context(lists:reverse("h(file,open), h(file,open)")),
    {function, "file"} = edlin_context:get_context(lists:reverse("file:")),
    {function, "file"} = edlin_context:get_context(lists:reverse("file:open")),
    {function, "file", "open", [], [], []} = edlin_context:get_context(lists:reverse("file:open(")),
    {string} = edlin_context:get_context(lists:reverse("file:open(\"")),
    {string} = edlin_context:get_context(lists:reverse("file:open(\"/")),
    {string} = edlin_context:get_context(lists:reverse("file:open(\"Word")),
    {function, "file", "open", [], {string, "\"\""}, []} = edlin_context:get_context(lists:reverse("file:open(\"\"")),
    {function, "file", "open", [{string, "\"\""}], [], []} = edlin_context:get_context(lists:reverse("file:open(\"\",")),
    {function, "file", "open", [{string, "\"\""}], [], [{list, [], []}]} = edlin_context:get_context(lists:reverse("file:open(\"\",[")),
    {function, "file", "open", [{string, "\"\""}], [], [{tuple, [], []}]} = edlin_context:get_context(lists:reverse("file:open(\"\",{")),
    {function, "file", "open", [{string, "\"\""}], [], [{list, [], []},{tuple, [], []}]} = edlin_context:get_context(lists:reverse("file:open(\"\",[{")),
    {function, "file", "open", [{string, "\"\""}], [], [{list, [], {atom, "atom"}}]} = edlin_context:get_context(lists:reverse("file:open(\"\",[atom")),
    {function, "file", "open", [{string, "\"\""}], [], [{tuple, [], {atom, "atom"}}]} = edlin_context:get_context(lists:reverse("file:open(\"\",{atom")),
    {function, "file", "open", [{string, "\"\""}], [], [{list, [], []},{tuple, [], {atom, "atom"}}]} = edlin_context:get_context(lists:reverse("file:open(\"\",[{atom")),
    {function, "file", "open", [{string, "\"\""}], [], [{list, [], []},{tuple, [{atom, "atom"}], []}]} = edlin_context:get_context(lists:reverse("file:open(\"\",[{atom,")),
    {function, "file", "open", [{string, "\"\""}], [], [{map, ["atom"], "atom", [], []}]} = edlin_context:get_context(lists:reverse("file:open(\"\",#{ atom =>")),
    {term, [], {atom, "list"}} = edlin_context:get_context(lists:reverse("#{list")),
    {term, [], {atom, "list"}} = edlin_context:get_context(lists:reverse("{list")),
    {term, [], {atom, "list"}} = edlin_context:get_context(lists:reverse("[list")),
    {map, "M", []} = edlin_context:get_context(lists:reverse("M#{")),
    {map, "M", []} = edlin_context:get_context(lists:reverse("M#{key")),
    {map, "M", ["key"]} = edlin_context:get_context(lists:reverse("M#{key=>")),
    {map, "M", ["key"]} = edlin_context:get_context(lists:reverse("M#{key:=")), %% map value
    {map, "M", ["key"]} = edlin_context:get_context(lists:reverse("M#{key=>0")),
    {map, "M", ["key"]} = edlin_context:get_context(lists:reverse("M#{key=>0,")),
    {map, "M", ["key", "key2"]} = edlin_context:get_context(lists:reverse("M#{key=>0,key2=>")),
    {map_or_record} = edlin_context:get_context(lists:reverse("#")),
    {record, "record", [], [], [], [], []} = edlin_context:get_context(lists:reverse("#record{")),
    {record, "record", [], [], [], [], []} = edlin_context:get_context(lists:reverse("#record.")),
    {record, "record", [], [], [], {atom, "field"}, []} = edlin_context:get_context(lists:reverse("#record{field")),
    {record, "record", ["field"], "field", [], [], []} = edlin_context:get_context(lists:reverse("#record{field=>")),
    {record, "record", ["field"], "field", [], [], []} = edlin_context:get_context(lists:reverse("#record{field:=")), %% record_field value
    {record, "record", ["field"], [], [{integer, "0"}], [], []} = edlin_context:get_context(lists:reverse("R#record{field=>0,")),
    {record, "record", ["field", "field2"], "field2", [{integer,"0"}], [], [{list, [], []},{tuple, [{atom, "atom"}], []}]} = edlin_context:get_context(lists:reverse("R#record{field=>0,field2=>[{atom,")),
    {term,[],{atom,"fun"}} = edlin_context:get_context(lists:reverse("fun")),
    {term,[],{atom,"fun"}} = edlin_context:get_context(lists:reverse("fun ")),
    {fun_} = edlin_context:get_context(lists:reverse("fun m")),
    {fun_, "m"} = edlin_context:get_context(lists:reverse("fun m:")),
    {fun_, "m"} = edlin_context:get_context(lists:reverse("fun m:f")),
    {fun_, "m", "f"} = edlin_context:get_context(lists:reverse("fun m:f/")),
    {fun_, "m", "f"} = edlin_context:get_context(lists:reverse("fun m:f/1")), 
    {fun_, "m", "f"} = edlin_context:get_context(lists:reverse("fun m:f/1 ")), 
    {term,[{fun_,"fun m:f/1"}],[]} = edlin_context:get_context(lists:reverse("fun m:f/1 ,")), 
    {function,"user_defined","my_fun",
        [{keyword,"receive X -> X end"}],
        [],[]} = edlin_context:get_context(lists:reverse("my_fun(receive X -> X end, ")),
    {function,"user_defined","my_fun",
        [{keyword,"maybe X -> X end"}],
        [],[]} = edlin_context:get_context(lists:reverse("my_fun(maybe X -> X end, ")),
    {function,"user_defined","my_fun",
        [{keyword,"try a end"}],
        [],[]} = edlin_context:get_context(lists:reverse("my_fun(try a end, ")),
    {function,"user_defined","my_fun",
        [{keyword,"catch X -> X end"}],
        [],[]}= edlin_context:get_context(lists:reverse("my_fun(catch X -> X end, ")),
    {function,"user_defined","my_fun",
        [{keyword,"try a catch _:_ -> b end"}],
        [],[]} = edlin_context:get_context(lists:reverse("my_fun(try a catch _:_ -> b end, ")),
    {function,"user_defined","my_fun",
        [{keyword,"begin X end"}],
        [],[]}= edlin_context:get_context(lists:reverse("my_fun(begin X end, ")),
    {function,"user_defined","my_fun",
        [{keyword,"if X -> X end"}],
        [],[]} = edlin_context:get_context(lists:reverse("my_fun(if X -> X end, ")),
    {function,"user_defined","my_fun",
        [{keyword,"case X of _ -> X end"}],
        [],[]} = edlin_context:get_context(lists:reverse("my_fun(case X of _ -> X end, ")),
    {binding} = edlin_context:get_context(lists:reverse("fun() -> X")),
    {term,[],{atom,"x"}} = edlin_context:get_context(lists:reverse("fun() -> x")),
    {term} = edlin_context:get_context(lists:reverse("fun() ->")),
    {macro} = edlin_context:get_context(lists:reverse("?")), %% not supported in edlin_expand
    % unknown map
    {term,[{operation,"one = a"},{operation,"two = b"}],[]} = edlin_context:get_context(lists:reverse("#{ one = a, two = b, ")),
    {term,[{atom,"a"},{atom,"b"}],[]} = edlin_context:get_context(lists:reverse("#{ one := a, two := b, ")),
    {term,[{atom,"a"},{atom,"b"}],[]} = edlin_context:get_context(lists:reverse("#{ one => a, two => b, ")),
    {term,[{operation,"A = a"},{operation,"B = b"}],[]} = edlin_context:get_context(lists:reverse("A = a, B = b, ")),
    {term,[{operation,"one = a"}],[]} = edlin_context:get_context(lists:reverse("#{ one = a, two = ")),
    {term,[{atom,"a"}],[]} = edlin_context:get_context(lists:reverse("#{ one := a, two := ")),
    {term,[{atom,"a"}],[]} = edlin_context:get_context(lists:reverse("#{ one => a, two => ")),
    {term,[{operation,"A = a"}],[]} = edlin_context:get_context(lists:reverse("A = a, B = ")),
    {term,[],{operation,"A = a"}} = edlin_context:get_context(lists:reverse("A = a")),
    {'end'} = edlin_context:get_context(lists:reverse("a.")),
    {record,"record",[],[],[],[],[]} = edlin_context:get_context(lists:reverse("#record.")),
    {record,"record",[],[],[],[],[]} = edlin_context:get_context(lists:reverse("{#record.")),
    {record,"record",[],[],[],{atom,"a"},[]} = edlin_context:get_context(lists:reverse("#record.a")),
    {record,"record",[],[],[],{atom,"a"},[]} = edlin_context:get_context(lists:reverse("{#record.a")),
    {term,[],{record,"#record{}"}} = edlin_context:get_context(lists:reverse("#record{}")),
    {term,[],{map,"#{ a => b}"}} = edlin_context:get_context(lists:reverse("#{ a => b}")),
    {term,[{atom,"a"}],{atom,"tuple"}} = edlin_context:get_context(lists:reverse("{a, tuple")),
    {term,[],{tuple,"{a, tuple}"}} = edlin_context:get_context(lists:reverse("{a, tuple}")),
    {term,[],{call,"lists:my_fun()"}} = edlin_context:get_context(lists:reverse("lists:my_fun()")),
    {term} = edlin_context:get_context(lists:reverse("(")),
    {term,[],{parenthesis,"()"}} = edlin_context:get_context(lists:reverse("()")),
    {new_fun,"()"} = edlin_context:get_context(lists:reverse("fun()")),
    %% 256 $]
    {term,[],{list,"[]"}} = edlin_context:get_context(lists:reverse("[]")),
    {term,[{atom,"a"}],{atom,"b"}} = edlin_context:get_context(lists:reverse("fun() when a, b")),
    {term,[{atom,"a"}],{atom,"b"}} = edlin_context:get_context(lists:reverse("fun() -> a, b")),
    {term,[{atom,"a"},{atom,"b"}],[]} = edlin_context:get_context(lists:reverse("fun() -> a, b, ")),
    {term, [], {pid, "<1.0.1>"}} = edlin_context:get_context(lists:reverse("<1.0.1>")),
    {term, [], {funref, "#Fun<erl_eval.0.1>"}} = edlin_context:get_context(lists:reverse("#Fun<erl_eval.0.1>")),
    {term, [], {ref, "#Ref<1.0.1>"}} = edlin_context:get_context(lists:reverse("#Ref<1.0.1>")),
    %{term, [], {port, "#Port<1.0>"}} = edlin_context:get_context(lists:reverse("#Port<1.0>")),
    {term, [], {binary, "<<0>>"}} = edlin_context:get_context(lists:reverse("<<0>>")),
    {term,[],{keyword,"fun (X) -> X end"}} = edlin_context:get_context(lists:reverse("fun (X) -> X end")),
    {term,[],{keyword,"fun(X) -> X end"}} = edlin_context:get_context(lists:reverse("fun(X) -> X end")), %% should be fun_ too
    {term,[],{keyword,"receive X -> X end"}} = edlin_context:get_context(lists:reverse("receive X -> X end")),
    {error, _} = edlin_context:get_context(lists:reverse("no_keyword -> X end")),
    {term} = edlin_context:get_context(lists:reverse("@")), %% No valid argument 305
    {term,[],{char,"$@"}} = edlin_context:get_context(lists:reverse("$@")),
    {term,[],{char,"$ "}} = edlin_context:get_context(lists:reverse("$ ")),
    {term,[],{float,"1.0"}} = edlin_context:get_context(lists:reverse("1.0")),
    {term,[],{integer,"10#10"}} = edlin_context:get_context(lists:reverse("10#10")),
    {term,[],{integer,"1"}} = edlin_context:get_context(lists:reverse("1")),
    {binding} = edlin_context:get_context(lists:reverse("{X")),
    {term,[{var, "X"}], []} = edlin_context:get_context(lists:reverse("{X, ")),
    {error,_} = edlin_context:get_context(lists:reverse("<abc)")),
    {error,_} = edlin_context:get_context(lists:reverse("<abc]")),
    {error,_} = edlin_context:get_context(lists:reverse("<abc}")),
    {term} = edlin_context:get_context(lists:reverse("(abc>")),
    {term} = edlin_context:get_context(lists:reverse("\"\\\"\"")), %% odd quotes "\""
    {error, _} = edlin_context:get_context(lists:reverse("{\"\", $\"}")), %% odd quotes
    {term} = edlin_context:get_context(lists:reverse("receive X -> ")),
    %% read operator and argument order validity
    {error,_} = edlin_context:get_context(lists:reverse("foo bar")), %% TODO what should be returned here, illegal
    {term,[],{operation,"\" \" \" \""}} = edlin_context:get_context(lists:reverse("\" \" \" \"")), %% " " " "
    {term,[],{operation,"1 + 2"}} = edlin_context:get_context(lists:reverse("1+2")),
    {term,[],{operation,"1 andalso 2"}} = edlin_context:get_context(lists:reverse("1 andalso 2")),
    {term,[],{operation,"1 and 2"}} = edlin_context:get_context(lists:reverse("1 and 2")),
    {term,[],{operation,"1 orelse 2"}} = edlin_context:get_context(lists:reverse("1 orelse 2")),
    {term,[],{operation,"1 or 2"}} = edlin_context:get_context(lists:reverse("1 or 2")),
    {term,[],{operation,"1 or 2"}} = edlin_context:get_context(lists:reverse("1 or 2")),
    {term,[],{operation,"1 =/= 2"}} = edlin_context:get_context(lists:reverse("1 =/= 2")),
    {term,[],{operation,"1 =:= 2"}} = edlin_context:get_context(lists:reverse("1 =:= 2")),
    {term,[],{operation,"1 <=> 2"}} = edlin_context:get_context(lists:reverse("1 <=> 2")),
    {term,[],{operation,"<<1>> > <<2>>"}} = edlin_context:get_context(lists:reverse("<<1>>><<2>>")),
    %{term,[],{operation,"<<1>> > <<2>>"}} = edlin_context:get_context(lists:reverse("<<1>> > <<2>>")),
    {error,_} = edlin_context:get_context(lists:reverse("1 + + 2")),
    {term,[],{integer,"2"}} = edlin_context:get_context(lists:reverse("1 -> 2")),
    {term,[],{integer,"2"}} = edlin_context:get_context(lists:reverse("receive X -> 2")),
    {term} = edlin_context:get_context(lists:reverse("receive X ->")),
    {term,[{integer,"2"}],{operation,"1 + 3"}} = edlin_context:get_context(lists:reverse("receive X -> 2, 1+3")),
    {term,[],{integer,"-1"}} = edlin_context:get_context(lists:reverse("-1")),
    {term,[],{float,"-1.2"}} = edlin_context:get_context(lists:reverse("-1.2")),
    ok.