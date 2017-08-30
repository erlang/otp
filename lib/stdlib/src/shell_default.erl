%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

-export([help/0,lc/1,c/1,c/2,nc/1,nl/1,l/1,i/0,pid/3,i/3,m/0,m/1,
         memory/0,memory/1,uptime/0,
	 erlangrc/1,bi/1, regs/0, flush/0,pwd/0,ls/0,ls/1,cd/1, 
         y/1, y/2,
	 xm/1, bt/1, q/0,
	 ni/0, nregs/0]).

-export([ih/0,iv/0,im/0,ii/1,ii/2,iq/1,ini/1,ini/2,inq/1,ib/2,ib/3,
	 ir/2,ir/3,ibd/2,ibe/2,iba/3,ibc/3,
	 ic/0,ir/1,ir/0,il/0,ipb/0,ipb/1,iaa/1,iaa/2,ist/1,ia/1,ia/2,ia/3,
	 ia/4,ip/0]).

-import(io, [format/1]).

help() ->
    format("** shell internal commands **~n"),
    format("b()        -- display all variable bindings\n"),
    format("e(N)       -- repeat the expression in query <N>\n"),
    format("f()        -- forget all variable bindings\n"),
    format("f(X)       -- forget the binding of variable X\n"),
    format("h()        -- history\n"),    
    format("history(N) -- set how many previous commands to keep\n"),
    format("results(N) -- set how many previous command results to keep\n"),
    format("catch_exception(B) -- how exceptions are handled\n"),
    format("v(N)       -- use the value of query <N>\n"),
    format("rd(R,D)    -- define a record\n"),
    format("rf()       -- remove all record information\n"),
    format("rf(R)      -- remove record information about R\n"),
    format("rl()       -- display all record information\n"),
    format("rl(R)      -- display record information about R\n"),
    format("rp(Term)   -- display Term using the shell's record information\n"),
    format("rr(File)   -- read record information from File (wildcards allowed)\n"),
    format("rr(F,R)    -- read selected record information from file(s)\n"),
    format("rr(F,R,O)  -- read selected record information with options\n"),
    format("** commands in module c **\n"),
    c:help(),
    format("** commands in module i (interpreter interface) **\n"),
    format("ih()       -- print help for the i module\n"),
    %% format("** private commands ** \n"),
    %% format("myfunc()   -- does my operation ...\n"),
    true.

%% these are in alphabetic order it would be nice if they
%% were to *stay* so!

bi(I) 		-> c:bi(I).
bt(Pid)		-> c:bt(Pid).
c(File) 	-> c:c(File).
c(File, Opt)    -> c:c(File, Opt).
cd(D)           -> c:cd(D).
erlangrc(X) 	-> c:erlangrc(X).
flush()         -> c:flush().
i() 		-> c:i().
i(X,Y,Z) 	-> c:i(X,Y,Z).
l(Mod)       	-> c:l(Mod).
lc(X)  		-> c:lc(X).
ls()            -> c:ls().
ls(S)           -> c:ls(S).
m() 		-> c:m().
m(Mod) 		-> c:m(Mod).
memory()        -> c:memory().
memory(Type)    -> c:memory(Type).
nc(X)     	-> c:nc(X).
ni()            -> c:ni().
nl(Mod) 	-> c:nl(Mod).
nregs()         -> c:nregs().
pid(X,Y,Z) 	-> c:pid(X,Y,Z).
pwd()           -> c:pwd().
q()		-> c:q().
regs()          -> c:regs().
uptime()        -> c:uptime().
xm(Mod)         -> c:xm(Mod).
y(File)         -> c:y(File).
y(File, Opts)   -> c:y(File, Opts).

iaa(Flag)       -> calli(iaa, [Flag]).
iaa(Flag,Fnk)   -> calli(iaa, [Flag,Fnk]).
ist(Flag)       -> calli(ist, [Flag]).
ia(Pid)         -> calli(ia, [Pid]).
ia(X,Y,Z)       -> calli(ia, [X,Y,Z]).
ia(Pid,Fnk)     -> calli(ia, [Pid,Fnk]).
ia(X,Y,Z,Fnk)   -> calli(ia, [X,Y,Z,Fnk]).
ib(Mod,Line)    -> calli(ib, [Mod,Line]).
ib(Mod,Fnk,Arity) -> calli(ib, [Mod,Fnk,Arity]).
ibd(Mod,Line)   -> calli(ibd, [Mod,Line]).
ibe(Mod,Line)   -> calli(ibe, [Mod,Line]).
iba(M,L,Action) -> calli(iba, [M,L,Action]).
ibc(M,L,Cond)   -> calli(ibc, [M,L,Cond]).
ic()            -> calli(ic, []).
ih()            -> calli(help, []).
ii(Mod)         -> calli(ii, [Mod]).
ii(Mod,Op)      -> calli(ii, [Mod,Op]).
il()            -> calli(il, []).
im()            -> calli(im, []).
ini(Mod)        -> calli(ini, [Mod]).
ini(Mod,Op)     -> calli(ini, [Mod,Op]).
inq(Mod)        -> calli(inq, [Mod]).
ip()            -> calli(ip, []).
ipb()           -> calli(ipb, []).
ipb(Mod)        -> calli(ipb, [Mod]).
iq(Mod)         -> calli(iq, [Mod]).
ir(Mod,Line)    -> calli(ir, [Mod,Line]).
ir(Mod,Fnk,Arity) -> calli(ir, [Mod,Fnk,Arity]).
ir(Mod)         -> calli(ir, [Mod]).
ir()            -> calli(ir, []).
iv()            -> calli(iv, []).

calli(F, Args) ->
    c:appcall(debugger, i, F, Args).
