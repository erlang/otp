%%
%% Compiled from get_two_tuple_elements.erl like this:
%%
%%     erlc +no_ssa_opt_sink get_two_tuple_elements.erl
%%
%% We have kept this .S file to ensure that the test continues to
%% be effective even if the compiler's code optimizations or code
%% generation will change in the future.
%%
%% In particular, we want to ensure that the following instruction
%% sequence in xx/2 is tested:
%%
%%     {get_tuple_element,{x,1},0,{x,0}}.
%%     {get_tuple_element,{x,1},1,{x,1}}.
%%     {get_tuple_element,{x,1},2,{x,1}}.
%%

{module, get_two_tuple_elements}.  %% version = 0

{exports, [{get_two_tuple_elements,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 14}.


{function, get_two_tuple_elements, 0, 2}.
  {label,1}.
    {line,[{location,"get_two_tuple_elements.erl",4}]}.
    {func_info,{atom,get_two_tuple_elements},{atom,get_two_tuple_elements},0}.
  {label,2}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{atom,x},{x,0}}.
    {line,[{location,"get_two_tuple_elements.erl",5}]}.
    {call,1,{f,9}}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[x]},
                                   none,
                                   {t_integer,{1,2}},
                                   none,none}}]}}.
    {move,{x,0},{y,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"get_two_tuple_elements.erl",5}]}.
    {call,1,{f,9}}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[x]},
                                   none,
                                   {t_integer,{1,2}},
                                   none,none}}]}}.
    {move,{x,0},{y,0}}.
    {move,{integer,2},{x,0}}.
    {line,[{location,"get_two_tuple_elements.erl",5}]}.
    {call,1,{f,9}}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[x]},
                                   none,
                                   {t_integer,{1,2}},
                                   none,none}}]}}.
    {test_heap,7,1}.
    {put_tuple2,{x,0},{list,[{atom,x},{atom,y},{x,0}]}}.
    {put_tuple2,{x,1},{list,[{y,0},{x,0}]}}.
    {move,{y,1},{x,0}}.
    {trim,2,0}.
    {line,[{location,"get_two_tuple_elements.erl",5}]}.
    {call,2,{f,5}}.
    {'%',{var_info,{x,0},[{type,{t_number,any}}]}}.
    {test,is_eq_exact,{f,3},[{x,0},{integer,3}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,3}.
    {line,[{location,"get_two_tuple_elements.erl",5}]}.
    {badmatch,{x,0}}.


{function, xx, 2, 5}.
  {label,4}.
    {line,[{location,"get_two_tuple_elements.erl",8}]}.
    {func_info,{atom,get_two_tuple_elements},{atom,xx},2}.
  {label,5}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[x]},
                                   none,
                                   {t_integer,{1,2}},
                                   none,none}}]}}.
    {'%',
     {var_info,
      {x,1},
      [{type,
        {t_tuple,2,true,
         #{1 => {t_union,{t_atom,[x]},none,{t_integer,{1,2}},none,none},
           2 =>
            {t_tuple,3,true,
             #{1 => {t_atom,[x]},
               2 => {t_atom,[y]},
               3 =>
                {t_union,
                 {t_atom,[x]},
                 none,
                 {t_integer,{1,2}},
                 none,none}}}}}}]}}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,1}}.
    {get_tuple_element,{x,1},2,{x,1}}.
    {move,nil,{x,2}}.
    {call_only,3,{f,7}}.


{function, yy, 3, 7}.
  {label,6}.
    {line,[{location,"get_two_tuple_elements.erl",17}]}.
    {func_info,{atom,get_two_tuple_elements},{atom,yy},3}.
  {label,7}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[x]},
                                   none,
                                   {t_integer,{1,2}},
                                   none,none}}]}}.
    {'%',{var_info,{x,1},
                   [{type,{t_union,{t_atom,[x]},
                                   none,
                                   {t_integer,{1,2}},
                                   none,none}}]}}.
    {'%',{var_info,{x,2},[{type,nil}]}}.
    {line,[{location,"get_two_tuple_elements.erl",18}]}.
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
    return.


{function, id, 1, 9}.
  {label,8}.
    {line,[{location,"get_two_tuple_elements.erl",20}]}.
    {func_info,{atom,get_two_tuple_elements},{atom,id},1}.
  {label,9}.
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,[x]},
                                   none,
                                   {t_integer,{1,2}},
                                   none,none}}]}}.
    return.


{function, module_info, 0, 11}.
  {label,10}.
    {line,[]}.
    {func_info,{atom,get_two_tuple_elements},{atom,module_info},0}.
  {label,11}.
    {move,{atom,get_two_tuple_elements},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 13}.
  {label,12}.
    {line,[]}.
    {func_info,{atom,get_two_tuple_elements},{atom,module_info},1}.
  {label,13}.
    {move,{x,0},{x,1}}.
    {move,{atom,get_two_tuple_elements},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
