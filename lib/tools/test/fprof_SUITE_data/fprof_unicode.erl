-module(fprof_unicode).

-export([t/0, 'кирилли́ческий атом'/0, annan/0, c_break/1,
        'кирилли́ческий атомB'/1]).

t() ->
    _Atom = 'кирилли́ческий атом',
    'кирилли́ческий атом'().

'кирилли́ческий атом'() ->
    'кирилли́ческий атом'('кирилли́ческий атом').

'кирилли́ческий атом'(_Atom) ->
    self() ! 'кирилли́ческий атом',
    G = fun (X) ->
                catch foo:bar()
        end,
    G("кирилли́ческий атом"), % line 17
    Pid = spawn_link(fun() -> waiting() end),
    true = register('кирилли́ческий атом', Pid),
    F = fun() -> 'кирилли́ческий атом' end,
    F().

annan() ->
    foo.

waiting() ->
    receive
        X -> X
    end.

c_break(_B) ->
    true.

'кирилли́ческий атомB'(_B) ->
    true.
