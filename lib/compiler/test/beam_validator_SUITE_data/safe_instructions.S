{module, safe_instructions}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{send_request,1}]}.

{attributes, []}.

{labels, 18}.


{function, send_request, 1, 2}.
  {label,1}.
    {line,[{location,"t.erl",6}]}.
    {func_info,{atom,t},{atom,send_request},1}.
  {label,2}.
    {allocate,3,1}.
    {init,{y,0}}.
    {move,{x,0},{y,1}}.
    {'try',{y,2},{f,7}}.
    {move,nil,{x,0}}.
    {line,[{location,"t.erl",8}]}.
    {call,1,{f,9}}.
    {move,{x,0},{y,0}}.
    {'try',{y,1},{f,3}}.
    {line,[{location,"t.erl",9}]}.
    {call,0,{f,13}}.
    {'%',{type_info,{x,0},{atom,ok}}}.
    {try_end,{y,1}}.
    {jump,{f,4}}.
  {label,3}.
    {try_case,{y,1}}.
  {label,4}.
    {test,is_eq_exact,{f,5},[{y,0},{atom,ok}]}.
    {move,{atom,ok},{x,0}}.
    {jump,{f,6}}.
  {label,5}.
    {move,{y,0},{x,0}}.
  {label,6}.
    {try_end,{y,2}}.
    {deallocate,3}.
    return.
  {label,7}.
    {try_case,{y,2}}.
    {test_heap,2,0}.
    {put_list,{y,1},nil,{x,1}}.
    {move,{literal,"error: sending file to ~s"},{x,0}}.
    {line,[{location,"t.erl",16}]}.
    {call_ext,2,{extfunc,lager,debug,2}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,3}.
    return.


{function, ftp_cmds, 1, 9}.
  {label,8}.
    {line,[{location,"t.erl",21}]}.
    {func_info,{atom,t},{atom,ftp_cmds},1}.
  {label,9}.
    {test,is_nonempty_list,{f,11},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {line,[{location,"t.erl",23}]}.
    {call_fun,0}.
    {test,is_eq_exact,{f,10},[{x,0},{atom,ok}]}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,9},1}.
  {label,10}.
    {deallocate,1}.
    return.
  {label,11}.
    {test,is_nil,{f,8},[{x,0}]}.
    {move,{atom,ok},{x,0}}.
    return.


{function, ftp_close, 0, 13}.
  {label,12}.
    {line,[{location,"t.erl",29}]}.
    {func_info,{atom,t},{atom,ftp_close},0}.
  {label,13}.
    {move,{atom,ok},{x,0}}.
    return.


{function, module_info, 0, 15}.
  {label,14}.
    {line,[]}.
    {func_info,{atom,t},{atom,module_info},0}.
  {label,15}.
    {move,{atom,t},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 17}.
  {label,16}.
    {line,[]}.
    {func_info,{atom,t},{atom,module_info},1}.
  {label,17}.
    {move,{x,0},{x,1}}.
    {move,{atom,t},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
