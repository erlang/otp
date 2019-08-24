{module, map_field_lists}.  %% version = 0

{exports, [{x,1},{y,1}]}.

{attributes, []}.

{labels, 5}.


{function, x, 1, 2}.
  {label,1}.
    {line,[{location,"map_field_lists.erl",4}]}.
    {func_info,{atom,map_field_lists},{atom,x},1}.
  {label,2}.
    {test,is_map,{f,1},[{x,0}]}.
    {test,has_map_fields,{f,1},{x,0},{list,[{atom,a},{atom,a}]}}.
    {move,{atom,ok},{x,0}}.
    return.


{function, y, 1, 4}.
  {label,3}.
    {line,[{location,"map_field_lists.erl",7}]}.
    {func_info,{atom,map_field_lists},{atom,y},1}.
  {label,4}.
    {test,is_map,{f,3},[{x,0}]}.
    {test,has_map_fields,{f,3},{x,0},{list,[]}}.
    {move,{atom,ok},{x,0}}.
    return.
