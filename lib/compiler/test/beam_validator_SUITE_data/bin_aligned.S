{module, t}.  %% version = 0

{exports, [{decode,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, decode, 1, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,decode},1}.
  {label,2}.
    {move,{integer,1},{x,1}}.
    {bif,size,{f,0},[{x,0}],{x,2}}.
    {bs_add,{f,0},[{x,1},{x,2},1],{x,1}}.
    {bs_init2,{f,0},{x,1},0,1,{field_flags,[]},{x,1}}.
    {bs_put_integer,{f,0},
                    {integer,3},
                    1,
                    {field_flags,[aligned,unsigned,big]},
                    {integer,0}}.
    {bs_put_binary,{f,0},{atom,all},8,{field_flags,[unsigned,big]},{x,0}}.
    {bs_put_integer,{f,0},
                    {integer,5},
                    1,
                    {field_flags,[unsigned,big,aligned]},
                    {integer,0}}.
    {move,{x,1},{x,0}}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,module_info},0}.
  {label,4}.
    {move,{atom,t},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {func_info,{atom,t},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,t},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
