{module, guard_SUITE_tuple_size}.  %% version = 0

{exports, [{t,1}]}.

{attributes, []}.

{labels, 5}.


{function, t, 1, 2}.
  {label,1}.
    {func_info,{atom,guard_SUITE_tuple_size},{atom,t},1}.
  {label,2}.
    {bif,tuple_size,{f,4},[{x,0}],{x,1}}.
    {test,is_eq_exact,{f,4},[{x,1},{integer,4}]}.
    {test,is_tuple,{f,3},[{x,0}]}.
    {test,test_arity,{f,3},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,5}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {get_tuple_element,{x,0},3,{x,4}}.
    {gc_bif,'+',{f,0},6,[{x,1},{x,2}],{x,0}}.
    {gc_bif,'+',{f,0},6,[{x,0},{x,3}],{x,0}}.
    {gc_bif,'+',{f,0},6,[{x,0},{x,4}],{x,0}}.
    {gc_bif,'+',{f,0},6,[{x,0},{x,5}],{x,0}}.
    return.
  {label,3}.
    {badmatch,{x,0}}.
  {label,4}.
    {jump,{f,1}}.
