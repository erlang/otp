{module, bad_dsetel}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{t,1}]}.

{attributes, []}.

{labels, 8}.


{function, t, 1, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,t},1}.
  {label,2}.
    {test,is_tuple,{f,3},[{x,0}]}.
    {test,test_arity,{f,3},[{x,0},7]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,3},[{x,1},{atom,r}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,2}}.
    {move,{integer,3},{x,0}}.
    {call_ext,3,{extfunc,erlang,setelement,3}}.
    {test_heap,6,1}.
    {put_list,{integer,99},nil,{x,1}}.
    {put_list,{integer,98},{x,1},{x,1}}.
    {put_list,{integer,97},{x,1},{x,1}}.
    {set_tuple_element,{x,1},{x,0},1}.
    {'%live',1}.
    {deallocate,0}.
    return.
  {label,3}.
    {test_heap,3,0}.
    {put_tuple,2,{x,0}}.
    {put,{atom,badrecord}}.
    {put,{atom,r}}.
    {'%live',1}.
    {call_ext_only,1,{extfunc,erlang,error,1}}.


{function, module_info, 0, 5}.
  {label,4}.
    {func_info,{atom,t},{atom,module_info},0}.
  {label,5}.
    {move,{atom,t},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 7}.
  {label,6}.
    {func_info,{atom,t},{atom,module_info},1}.
  {label,7}.
    {move,{x,0},{x,1}}.
    {move,{atom,t},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
