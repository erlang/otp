{module, merge_undefined}.  %% version = 0

{exports, [{bar,2},{foo,1},{handle_call,2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 15}.


{function, handle_call, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,handle_call},2}.
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
    {allocate_heap,1,6,2}.
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
    {call_last,1,{f,8},1}.


{function, foo, 1, 8}.
  {label,7}.
    {func_info,{atom,t},{atom,foo},1}.
  {label,8}.
    {move,{atom,ok},{x,0}}.
    return.


{function, bar, 2, 10}.
  {label,9}.
    {func_info,{atom,t},{atom,bar},2}.
  {label,10}.
    {move,{atom,ok},{x,0}}.
    return.


{function, module_info, 0, 12}.
  {label,11}.
    {func_info,{atom,t},{atom,module_info},0}.
  {label,12}.
    {move,{atom,t},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 14}.
  {label,13}.
    {func_info,{atom,t},{atom,module_info},1}.
  {label,14}.
    {move,{x,0},{x,1}}.
    {move,{atom,t},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
