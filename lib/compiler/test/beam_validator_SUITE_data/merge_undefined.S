{module, merge_undefined}.  %% version = 0

{exports, [{uninitialized,2},{undecided,2}]}.

{attributes, []}.

{labels, 15}.

{function, uninitialized, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,uninitialized},2}.
  {label,2}.
    {test,is_atom,{f,1},[{x,0}]}.
    {select_val,{x,0},{f,1},{list,[{atom,gurka},{f,3},{atom,delete},{f,4}]}}.
  {label,3}.
    {allocate_heap,2,6,2}.
    {test,is_eq_exact,{f,5},[{x,0},{atom,ok}]}.
    %% This is unreachable since {x,0} is known not to be 'ok'. We should not
    %% fail with "uninitialized y registers" on erlang:exit/1
    {move,{atom,nisse},{x,0}}.
    {call_ext,1,{extfunc,erlang,exit,1}}.
  {label,4}.
    {allocate_heap,2,6,2}.
    {move,{x,1},{y,0}}.
    {put_list,{integer,112},nil,{x,0}}.
    {put_list,{integer,126},{x,0},{x,0}}.
    {put_list,{y,0},nil,{x,1}}.
    {'%live',2}.
    {call_ext,2,{extfunc,io,format,2}}.
    {test,is_ne_exact,{f,6},[{x,0},{atom,ok}]}.
  {label,5}.
    %% The Y registers are not initialized here.
    {move,{atom,logReader},{x,1}}.
    {move,{atom,console},{x,0}}.
    {call_ext,2,{extfunc,debug,filter,2}}.
    {test_heap,14,1}.
    {put_list,{atom,logReader},nil,{x,1}}.
    {put_list,{atom,console},{x,1},{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{atom,debug}}.
    {put,{atom,filter}}.
    {put,{x,1}}.
    {put_tuple,2,{x,1}}.
    {put,{x,2}}.
    {put,{x,0}}.
    {put_tuple,2,{x,0}}.
    {put,{atom,badmatch}}.
    {put,{x,1}}.
    {'%live',1}.
    {call_ext,1,{extfunc,erlang,exit,1}}.
  {label,6}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,14},1}.

{function, undecided, 2, 8}.
  {label,7}.
    {func_info,{atom,t},{atom,undecided},2}.
  {label,8}.
    {test,is_atom,{f,7},[{x,0}]}.
    {select_val,{x,0},{f,1},{list,[{atom,gurka},{f,9},{atom,delete},{f,10}]}}.
  {label,9}.
    {allocate_heap,2,6,2}.
    {test,is_eq_exact,{f,11},[{x,0},{atom,ok}]}.
    %% This is unreachable since {x,0} is known not to be 'ok'. We should not
    %% fail with "uninitialized y registers" on erlang:exit/1
    {move,{atom,nisse},{x,0}}.
    {call_ext,1,{extfunc,erlang,exit,1}}.
  {label,10}.
    {allocate_heap,1,6,2}.
    {move,{x,1},{y,0}}.
    {put_list,{integer,112},nil,{x,0}}.
    {put_list,{integer,126},{x,0},{x,0}}.
    {put_list,{y,0},nil,{x,1}}.
    {'%live',2}.
    {call_ext,2,{extfunc,io,format,2}}.
    {test,is_ne_exact,{f,12},[{x,0},{atom,ok}]}.
  {label,11}.
    %% The number of allocated Y registers are in conflict here.
    {move,{atom,logReader},{x,1}}.
    {move,{atom,console},{x,0}}.
    {call_ext,2,{extfunc,debug,filter,2}}.
    {test_heap,14,1}.
    {put_list,{atom,logReader},nil,{x,1}}.
    {put_list,{atom,console},{x,1},{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{atom,debug}}.
    {put,{atom,filter}}.
    {put,{x,1}}.
    {put_tuple,2,{x,1}}.
    {put,{x,2}}.
    {put,{x,0}}.
    {put_tuple,2,{x,0}}.
    {put,{atom,badmatch}}.
    {put,{x,1}}.
    {'%live',1}.
    {call_ext,1,{extfunc,erlang,exit,1}}.
  {label,12}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,8},1}.

{function, foo, 1, 14}.
  {label,13}.
    {func_info,{atom,t},{atom,foo},1}.
  {label,14}.
    {move,{atom,ok},{x,0}}.
    return.
