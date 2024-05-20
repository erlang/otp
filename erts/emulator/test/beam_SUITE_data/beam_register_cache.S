{module, beam_register_cache}.  %% version = 0

{exports, [{beam_register_cache,0},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 17}.


{function, beam_register_cache, 0, 2}.
  {label,1}.
    {line,[{location,"beam_register_cache.erl",4}]}.
    {func_info,{atom,beam_register_cache},{atom,beam_register_cache},0}.
  {label,2}.
    {allocate,0,0}.
    {move,{literal,{a,b,c,d,e}},{x,0}}.
    {call,1,{f,5}}. % test/1
    {test,is_eq_exact,{f,3},[{x,0},{atom,d}]}. %% 'b' on error
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,3}.
    {badmatch,{x,0}}.


{function, test, 1, 5}.
  {label,4}.
    {line,[{location,"beam_register_cache.erl",8}]}.
    {func_info,{atom,beam_register_cache},{atom,test},1}.
  {label,5}.
    {test,is_tuple,{f,4},[{x,0}]}.
    {test,test_arity,{f,4},[{x,0},5]}.
    {allocate,5,1}.
    {get_tuple_element,{x,0},0,{y,4}}.
    {get_tuple_element,{x,0},1,{y,3}}.
    {get_tuple_element,{x,0},2,{y,2}}.
    {get_tuple_element,{x,0},3,{y,1}}.
    {get_tuple_element,{x,0},4,{y,0}}.
    {test,is_eq_exact,{f,6},[{y,3},{atom,b}]}.
    {move,{y,1},{y,3}}. %% i_move_trim blew up the register cache here...
    {trim,2,3}.         %%
    {move,{y,1},{x,0}}. %% ... mixing up {y,1} from {y,3} loaded in is_eq_exact
    {deallocate,3}.
    return.
  {label,6}.
    {test_heap,2,0}.
    {put_list,{y,4},nil,{x,0}}.
    {deallocate,5}.
    return.


{function, module_info, 0, 14}.
  {label,13}.
    {line,[]}.
    {func_info,{atom,beam_register_cache},{atom,module_info},0}.
  {label,14}.
    {move,{atom,beam_register_cache},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 16}.
  {label,15}.
    {line,[]}.
    {func_info,{atom,beam_register_cache},{atom,module_info},1}.
  {label,16}.
    {move,{x,0},{x,1}}.
    {move,{atom,beam_register_cache},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
