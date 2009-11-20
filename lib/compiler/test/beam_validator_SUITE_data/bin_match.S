{module, bin_match}.  %% version = 0

{exports, [{t,1}]}.

{attributes, []}.

{labels, 8}.


{function, t, 1, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,t},1}.
  {label,2}.
%%    {test,bs_start_match,{f,1},[{x,0}]}.
    {bs_save,0}.
    {test,bs_get_integer,
          {f,3},
          [{integer,8},1,{field_flags,[aligned,unsigned,big]},{x,1}]}.
    {test,bs_get_integer,
          {f,3},
          [{integer,8},1,{field_flags,[aligned,unsigned,big]},{x,2}]}.
    {test,bs_test_tail,{f,3},[0]}.
    {test_heap,3,3}.
    {put_tuple,2,{x,0}}.
    {put,{x,1}}.
    {put,{x,2}}.
    {'%live',1}.
    return.
  {label,3}.
    {bs_restore,0}.
    {test,bs_get_integer,
          {f,1},
          [{integer,32},1,{field_flags,[aligned,unsigned,big]},{x,1}]}.
    {test,bs_test_tail,{f,1},[0]}.
    {move,{x,1},{x,0}}.
    return.

{function, x, 1, 5}.
  {label,4}.
    {func_info,{atom,t},{atom,x},1}.
  {label,5}.
    {test,bs_start_match,{f,4},[{x,0}]}.
    {bs_save,0}.
    {test,bs_get_integer,
          {f,6},
          [{integer,8},1,{field_flags,[aligned,unsigned,big]},{x,1}]}.
    {test,bs_get_integer,
          {f,6},
          [{integer,8},1,{field_flags,[aligned,unsigned,big]},{x,2}]}.
    {test,bs_test_tail,{f,6},[0]}.
    {test_heap,3,3}.
    {put_tuple,2,{x,0}}.
    {put,{x,1}}.
    {put,{x,2}}.
    {'%live',1}.
    return.
  {label,6}.
    {bs_restore,1}.
    {test,bs_get_integer,
          {f,4},
          [{integer,32},1,{field_flags,[aligned,unsigned,big]},{x,1}]}.
    {test,bs_test_tail,{f,4},[0]}.
    {move,{x,1},{x,0}}.
    return.
