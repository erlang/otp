{module, otp_24_code_gh_8238}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{otp_24_code_gh_8238,0}]}.

{attributes, []}.

{labels, 7}.


{function, otp_24_code_gh_8238, 0, 2}.
  {label,1}.
    {line,[{location,"otp_24_code_gh_8238.erl",4}]}.
    {func_info,{atom,otp_24_code_gh_8238},{atom,otp_24_code_gh_8238},0}.
  {label,2}.
    {allocate,0,0}.
    {move,{integer,1000},{x,0}}.
    {line,[{location,"otp_24_code_gh_8238.erl",5}]}.
    {call_ext,1,{extfunc,erlang,integer_to_binary,1}}.
    {line,[{location,"otp_24_code_gh_8238.erl",6}]}.
    {gc_bif,byte_size,{f,0},1,[{x,0}],{x,1}}.
    {bs_add,{f,0},[{x,1},{integer,9},1],{x,1}}.
    {bs_init2,{f,0},{x,1},2,2,{field_flags,[]},{x,1}}.
    {bs_put_integer,{f,0},
                    {integer,72},
                    1,
                    {field_flags,[unsigned,big]},
                    {integer,1281499675772873685536}}.
    {bs_put_binary,{f,0},{atom,all},8,{field_flags,[unsigned,big]},{x,0}}.
    {put_list,{x,1},nil,{x,1}}.
    {move,{literal,"~p\n"},{x,0}}.
    {call_ext_last,2,{extfunc,io,format,2},0}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,otp_24_code_gh_8238},{atom,module_info},0}.
  {label,4}.
    {move,{atom,otp_24_code_gh_8238},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,otp_24_code_gh_8238},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,otp_24_code_gh_8238},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
