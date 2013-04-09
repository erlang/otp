{module, yes_14}.  %% version = 0

{exports, [{f,2},{module_info,0},{module_info,1},{yes_14,0}]}.

{attributes, []}.

{labels, 12}.


{function, yes_14, 0, 2}.
  {label,1}.
    {func_info,{atom,yes_14},{atom,yes_14},0}.
  {label,2}.
    {move,{atom,ok},{x,0}}.
    return.


{function, f, 2, 4}.
  {label,3}.
    {func_info,{atom,yes_14},{atom,f},2}.
  {label,4}.
    {allocate_heap,2,3,2}.
    {move,{x,0},{y,1}}.
    {put_tuple,2,{y,0}}.
    {put,{atom,data}}.
    {put,{x,1}}.
    {call_ext,0,{extfunc,erlang,make_ref,0}}. % Ref in [x0]
    {test_heap,4,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,request}}.
    {put,{x,0}}.
    {put,{y,0}}.
    {move,{x,0},{y,0}}. % Ref in [x0,y0]
    {move,{y,1},{x,0}}. % Ref in [y0]
    {kill,{y,1}}.
    send.
    {move,{y,0},{x,0}}. % Ref in [x0,y0]
    {move,{x,0},{y,1}}. % Ref in [x0,y0,y1]
  {label,5}.
    {loop_rec,{f,7},{x,0}}. % Ref in [y0,y1]
    {test,is_tuple,{f,6},[{x,0}]}.
    {test,test_arity,{f,6},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {test,is_eq_exact,{f,6},[{x,1},{atom,reply}]}.
    {test,is_eq_exact,{f,6},[{x,2},{y,1}]}.
    remove_message.
    {move,{atom,ok},{x,0}}.
    {deallocate,2}.
    return.
  {label,6}.
    {loop_rec_end,{f,5}}.
  {label,7}.
    {wait,{f,5}}.


{function, module_info, 0, 9}.
  {label,8}.
    {func_info,{atom,yes_14},{atom,module_info},0}.
  {label,9}.
    {move,{atom,yes_14},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 11}.
  {label,10}.
    {func_info,{atom,yes_14},{atom,module_info},1}.
  {label,11}.
    {move,{x,0},{x,1}}.
    {move,{atom,yes_14},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
