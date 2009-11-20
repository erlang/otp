{module, illegal_instruction}.  %% version = 0

{exports, []}.

{attributes, []}.

{labels, 7}.


{function, t, 1, 2}.
  {label,1}.
    {func_info,{atom,illegal_instruction},{atom,t},1}.
  {label,2}.
    {my_illegal_instruction,{x,0}}.
    return.


{function, x, 1, 4}.
  {label,3}.
    bad_func_info.
  {label,4}.
    {my_illegal_instruction,{x,0}}.
    return.

{function, y, 0, 17}.
	