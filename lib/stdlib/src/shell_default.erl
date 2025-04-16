%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

%% This is just a empty template which calls routines in the module c
%% to do all the work!

-module(shell_default).
-moduledoc """
Customizing the Erlang environment.

The functions in this module are called when no module name is specified in a
shell command.

Consider the following shell dialog:

```erlang
1> lists:reverse("abc").
"cba"
2> c(foo).
{ok, foo}
```

In command one, module `m:lists` is called. In command two, no module name is
specified. The shell searches module `user_default` followed by module
`shell_default` for function `c/1`.

`shell_default` is intended for "system wide" customizations to the shell.
`user_default` is intended for "local" or individual user customizations.

## Hint

To add your own commands to the shell, create a module called `user_default` and
add the commands you want. Then add the following line as the _first_ line in
your `.erlang` file in your home directory.

```text
code:load_abs("$PATH/user_default").
```

`$PATH` is the directory where your `user_default` module can be found.
""".

-export([help/0,lc/1,c/1,c/2,c/3,nc/1,nl/1,l/1,i/0,pid/3,i/3,m/0,m/1,lm/0,mm/0,
         memory/0,memory/1,uptime/0,
         erlangrc/1,bi/1, regs/0, flush/0,pwd/0,ls/0,ls/1,cd/1,
         y/1, y/2,
         xm/1, bt/1, q/0,
         h/1, h/2, h/3, ht/1, ht/2, ht/3, hcb/1, hcb/2, hcb/3,
         ni/0, nregs/0]).

-export([ih/0,iv/0,im/0,ii/1,ii/2,iq/1,ini/1,ini/2,inq/1,ib/2,ib/3,
         ir/2,ir/3,ibd/2,ibe/2,iba/3,ibc/3,
         ic/0,ir/1,ir/0,il/0,ipb/0,ipb/1,iaa/1,iaa/2,ist/1,ia/1,ia/2,ia/3,
         ia/4,ip/0]).
-export(['$handle_undefined_function'/2]).
-import(io, [format/1]).

-doc "Print the help for all shell commands.".
-spec help() -> true.
help() ->
    shell:help(),
    c:help(),
    format(~"** commands in module i (interpreter interface) **\n"),
    format(~"ih()       -- print help for the i module\n"),
    %% format("** private commands ** \n"),
    %% format("myfunc()   -- does my operation ...\n"),
    true.

%% These are in alphabetic order. It would be nice if they
%% were to *stay* so!

-doc false.
bi(I)           -> c:bi(I).
-doc false.
bt(Pid)         -> c:bt(Pid).
-doc false.
c(File)         -> c:c(File).
-doc false.
c(File, Opt)    -> c:c(File, Opt).
-doc false.
c(File, Opt, Filter) -> c:c(File, Opt, Filter).
-doc false.
cd(D)           -> c:cd(D).
-doc false.
erlangrc(X)     -> c:erlangrc(X).
-doc false.
flush()         -> c:flush().
-doc false.
h(M)            -> c:h(M).
-doc false.
h(M,F)          -> c:h(M,F).
-doc false.
h(M,F,A)        -> c:h(M,F,A).
-doc false.
ht(M)           -> c:ht(M).
-doc false.
ht(M,F)         -> c:ht(M,F).
-doc false.
ht(M,F,A)       -> c:ht(M,F,A).
-doc false.
hcb(M)          -> c:hcb(M).
-doc false.
hcb(M,F)        -> c:hcb(M,F).
-doc false.
hcb(M,F,A)      -> c:hcb(M,F,A).
-doc false.
i()             -> c:i().
-doc false.
i(X,Y,Z)        -> c:i(X,Y,Z).
-doc false.
l(Mod)          -> c:l(Mod).
-doc false.
lc(X)           -> c:lc(X).
-doc false.
ls()            -> c:ls().
-doc false.
ls(S)           -> c:ls(S).
-doc false.
m()             -> c:m().
-doc false.
m(Mod)          -> c:m(Mod).
-doc false.
lm()            -> c:lm().
-doc false.
mm()            -> c:mm().
-doc false.
memory()        -> c:memory().
-doc false.
memory(Type)    -> c:memory(Type).
-doc false.
nc(X)           -> c:nc(X).
-doc false.
ni()            -> c:ni().
-doc false.
nl(Mod)         -> c:nl(Mod).
-doc false.
nregs()         -> c:nregs().
-doc false.
pid(X,Y,Z)      -> c:pid(X,Y,Z).
-doc false.
pwd()           -> c:pwd().
-doc false.
q()             -> c:q().
-doc false.
regs()          -> c:regs().
-doc false.
uptime()        -> c:uptime().
-doc false.
xm(Mod)         -> c:xm(Mod).
-doc false.
y(File)         -> c:y(File).
-doc false.
y(File, Opts)   -> c:y(File, Opts).

-doc false.
iaa(Flag)       -> calli(iaa, [Flag]).
-doc false.
iaa(Flag,Fnk)   -> calli(iaa, [Flag,Fnk]).
-doc false.
ist(Flag)       -> calli(ist, [Flag]).
-doc false.
ia(Pid)         -> calli(ia, [Pid]).
-doc false.
ia(X,Y,Z)       -> calli(ia, [X,Y,Z]).
-doc false.
ia(Pid,Fnk)     -> calli(ia, [Pid,Fnk]).
-doc false.
ia(X,Y,Z,Fnk)   -> calli(ia, [X,Y,Z,Fnk]).
-doc false.
ib(Mod,Line)    -> calli(ib, [Mod,Line]).
-doc false.
ib(Mod,Fnk,Arity) -> calli(ib, [Mod,Fnk,Arity]).
-doc false.
ibd(Mod,Line)   -> calli(ibd, [Mod,Line]).
-doc false.
ibe(Mod,Line)   -> calli(ibe, [Mod,Line]).
-doc false.
iba(M,L,Action) -> calli(iba, [M,L,Action]).
-doc false.
ibc(M,L,Cond)   -> calli(ibc, [M,L,Cond]).
-doc false.
ic()            -> calli(ic, []).
-doc false.
ih()            -> calli(help, []).
-doc false.
ii(Mod)         -> calli(ii, [Mod]).
-doc false.
ii(Mod,Op)      -> calli(ii, [Mod,Op]).
-doc false.
il()            -> calli(il, []).
-doc false.
im()            -> calli(im, []).
-doc false.
ini(Mod)        -> calli(ini, [Mod]).
-doc false.
ini(Mod,Op)     -> calli(ini, [Mod,Op]).
-doc false.
inq(Mod)        -> calli(inq, [Mod]).
-doc false.
ip()            -> calli(ip, []).
-doc false.
ipb()           -> calli(ipb, []).
-doc false.
ipb(Mod)        -> calli(ipb, [Mod]).
-doc false.
iq(Mod)         -> calli(iq, [Mod]).
-doc false.
ir(Mod,Line)    -> calli(ir, [Mod,Line]).
-doc false.
ir(Mod,Fnk,Arity) -> calli(ir, [Mod,Fnk,Arity]).
-doc false.
ir(Mod)         -> calli(ir, [Mod]).
-doc false.
ir()            -> calli(ir, []).
-doc false.
iv()            -> calli(iv, []).

calli(F, Args) ->
    c:appcall(debugger, i, F, Args).

-doc false.
'$handle_undefined_function'(Func, Args) ->
    case shell:get_function(Func, length(Args)) of
       undefined ->
           error_handler:raise_undef_exception(?MODULE, Func, Args);
       Fun when is_function(Fun, length(Args)) ->
           apply(Fun, Args)
    end.
