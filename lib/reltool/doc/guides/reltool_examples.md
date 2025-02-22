<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Examples

## Start and stop windows and servers

The main process in Reltool is the server. It can be used as it is or be used
via the GUI frontend process. When the GUI is started, a server process will
automatically be started. The GUI process is started with `reltool:start/0`,
`reltool:start/1` or `reltool:start_link/1`. The pid of its server can be
obtained with [`reltool:get_server/1`](`reltool:start_link/1`)

```erlang
Erlang/OTP 20 [erts-9.0] [source-c13b302] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10]
[hipe] [kernel-poll:false]
Eshell V9.0  (abort with ^G)
1>
1> {ok, Win} = reltool:start([]).
{ok,<0.36.01>}
2> {ok, Server} = reltool:get_server(Win).
{ok,<0.37.01>}
3> reltool:get_config(Server).
{ok,{sys,[]}}
4>
4> {ok, Server2} = reltool:start_server([]).
{ok,<0.6535.01>}
5> reltool:get_config(Server2).
{ok,{sys,[]}}
6> reltool:stop(Server2).
ok
```

## Inspecting the configuration

```erlang
Erlang/OTP 20 [erts-9.0] [source-c13b302] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10]
[hipe] [kernel-poll:false]
Eshell V9.0  (abort with ^G)
1>
1> Config = {sys, [{escript, "examples/display_args", [{incl_cond, include}]},
		   {app, inets, [{incl_cond, include}]},
		   {app, mnesia, [{incl_cond, exclude}]},
		   {app, ssl, [{incl_cond, exclude}]},
		   {app, runtime_tools, [{incl_cond, exclude}]},
		   {app, syntax_tools, [{incl_cond, exclude}]}]}.
{sys,[{escript,"examples/display_args",[{incl_cond,include}]},
      {app,inets,[{incl_cond,include}]},
      {app,mnesia,[{incl_cond,exclude}]},
      {app,ssl,[{incl_cond,exclude}]},
      {app,runtime_tools,[{incl_cond,exclude}]},
      {app,syntax_tools,[{incl_cond,exclude}]}]}
2>
2> {ok, Server} = reltool:start_server([Config]).
{ok,<0.66.0>}
3>
3> reltool:get_config(Server).
{ok,{sys,[{escript,"/usr/local/lib/erlang/lib/reltool-0.7.3/examples/display_args",
                   [{incl_cond,include}]},
          {app,inets,[{incl_cond,include}]},
          {app,mnesia,[{incl_cond,exclude}]},
          {app,runtime_tools,[{incl_cond,exclude}]},
          {app,ssl,[{incl_cond,exclude}]},
          {app,syntax_tools,[{incl_cond,exclude}]}]}}
4>
4> reltool:get_config(Server, false, false).
{ok,{sys,[{escript,"/usr/local/lib/erlang/lib/reltool-0.7.3/examples/display_args",
                   [{incl_cond,include}]},
          {app,inets,[{incl_cond,include}]},
          {app,mnesia,[{incl_cond,exclude}]},
          {app,runtime_tools,[{incl_cond,exclude}]},
          {app,ssl,[{incl_cond,exclude}]},
          {app,syntax_tools,[{incl_cond,exclude}]}]}}
5>
5> reltool:get_config(Server, true, false).
{ok,{sys,[{root_dir,"/usr/local/lib/erlang"},
          {lib_dirs,[]},
          {escript,"/usr/local/lib/erlang/lib/reltool-0.7.3/examples/display_args",
                   [{incl_cond,include}]},
          {mod_cond,all},
          {incl_cond,derived},
          {app,inets,
               [{incl_cond,include},{vsn,undefined},{lib_dir,undefined}]},
          {app,mnesia,[{incl_cond,exclude}]},
          {app,runtime_tools,[{incl_cond,exclude}]},
          {app,ssl,[{incl_cond,exclude}]},
          {app,syntax_tools,[{incl_cond,exclude}]},
          {boot_rel,"start_clean"},
          {rel,"start_clean","1.0",[]},
          {rel,"start_sasl","1.0",[sasl]},
          {emu_name,"beam"},
          {relocatable,true},
          {profile,development},
          {incl_sys_filters,[".*"]},
          {excl_sys_filters,[]},
          {incl_app_filters,[".*"]},
          {excl_app_filters,[]},
          {rel_app_type,...},
          {...}|...]}}
6>
6> reltool:get_config(Server, true, true).
{ok,{sys,[{root_dir,"/usr/local/lib/erlang"},
          {lib_dirs,[]},
          {escript,"/usr/local/lib/erlang/lib/reltool-0.7.3/examples/display_args",
                   [{incl_cond,include}]},
          {mod_cond,all},
          {incl_cond,derived},
          {erts,[{app,erts,
                      [{vsn,"10.0"},
                       {lib_dir,"/usr/local/lib/erlang/lib/erts-10.0"},
                       {mod,erl_prim_loader,[]},
                       {mod,erl_tracer,[]},
                       {mod,erlang,[]},
                       {mod,erts_code_purger,[]},
                       {mod,erts_dirty_process_signal_handler,[]},
                       {mod,erts_internal,[]},
                       {mod,erts_literal_area_collector,[]},
                       {mod,init,[]},
                       {mod,erl_init,...},
                       {mod,...},
                       {...}|...]}]},
          {app,compiler,
               [{vsn,"7.0.4"},
                {lib_dir,"/usr/local/lib/erlang/lib/compiler-7.0.4"},
                {mod,beam_a,[]},
                {mod,beam_asm,[]},
                {mod,beam_block,[]},
                {mod,beam_bs,[]},
                {mod,beam_bsm,[]},
                {mod,beam_clean,[]},
                {mod,beam_dead,[]},
                {mod,beam_dict,[]},
                {mod,beam_disasm,[]},
                {mod,beam_except,[]},
                {mod,beam_flatten,...},
                {mod,...},
                {...}|...]},
          {app,crypto,
               [{vsn,"3.7.4"},
                {lib_dir,"/usr/local/lib/erlang/lib/crypto-3.7.4"},
                {mod,crypto,[]},
                {mod,crypto_ec_curves,[]}]},
          {app,hipe,
               [{vsn,"3.15.4"},
                {lib_dir,"/usr/local/lib/erlang/lib/hipe-3.15.4"},
                {mod,cerl_cconv,[]},
                {mod,cerl_closurean,[]},
                {mod,cerl_hipeify,[]},
                {mod,cerl_lib,[]},
                {mod,cerl_messagean,[]},
                {mod,cerl_pmatch,[]},
                {mod,cerl_prettypr,[]},
                {mod,cerl_to_icode,[]},
                {mod,cerl_typean,...},
                {mod,...},
                {...}|...]},
          {app,inets,
               [{incl_cond,include},
                {vsn,"6.3.9"},
                {lib_dir,"/usr/local/lib/erlang/lib/inets-6.3.9"},
                {mod,ftp,[]},
                {mod,ftp_progress,[]},
                {mod,ftp_response,[]},
                {mod,ftp_sup,[]},
                {mod,http_chunk,[]},
                {mod,http_request,[]},
                {mod,http_response,...},
                {mod,...},
                {...}|...]},
          {app,kernel,
               [{vsn,"5.2"},
                {lib_dir,"/usr/local/lib/erlang/lib/kernel-5.2"},
                {mod,application,[]},
                {mod,application_controller,[]},
                {mod,application_master,[]},
                {mod,application_starter,[]},
                {mod,auth,[]},
                {mod,code,[]},
                {mod,code_server,...},
                {mod,...},
                {...}|...]},
          {app,mnesia,[{incl_cond,exclude}]},
          {app,runtime_tools,[{incl_cond,exclude}]},
          {app,sasl,
               [{vsn,"3.0.3"},
                {lib_dir,"/usr/local/lib/erlang/lib/sasl-3.0.3"},
                {mod,alarm_handler,[]},
                {mod,erlsrv,[]},
                {mod,format_lib_supp,[]},
                {mod,misc_supp,...},
                {mod,...},
                {...}|...]},
          {app,ssl,[{incl_cond,exclude}]},
          {app,stdlib,
               [{vsn,"3.3"},
                {lib_dir,"/usr/local/lib/erlang/lib/stdlib-3.3"},
                {mod,array,[]},
                {mod,base64,...},
                {mod,...},
                {...}|...]},
          {app,syntax_tools,[{incl_cond,exclude}]},
          {app,tools,
               [{vsn,"2.9.1"},{lib_dir,[...]},{mod,...},{...}|...]},
          {boot_rel,"start_clean"},
          {rel,"start_clean","1.0",[]},
          {rel,"start_sasl","1.0",[...]},
          {emu_name,"beam"},
          {relocatable,true},
          {profile,...},
          {...}|...]}}
7>
7> reltool:get_config([{sys, [{profile, embedded}]}], true, false).
{ok,{sys,[{root_dir,"/usr/local/lib/erlang"},
          {lib_dirs,[]},
          {mod_cond,all},
          {incl_cond,derived},
          {boot_rel,"start_clean"},
          {rel,"start_clean","1.0",[]},
          {rel,"start_sasl","1.0",[sasl]},
          {emu_name,"beam"},
          {relocatable,true},
          {profile,embedded},
          {incl_sys_filters,["^bin","^erts","^lib","^releases"]},
          {excl_sys_filters,["^bin/(erlc|dialyzer|typer)(|\\.exe)$",
                             "^erts.*/bin/(erlc|dialyzer|typer)(|\\.exe)$",
                             "^erts.*/bin/.*(debug|pdb)"]},
          {incl_app_filters,["^ebin","^include","^priv"]},
          {excl_app_filters,[]},
          {rel_app_type,permanent},
          {embedded_app_type,load},
          {app_file,keep},
          {debug_info,keep}]}}
8>
8> reltool:get_config([{sys, [{profile, standalone}]}], true, false).
{ok,{sys,[{root_dir,"/usr/local/lib/erlang"},
          {lib_dirs,[]},
          {mod_cond,all},
          {incl_cond,derived},
          {boot_rel,"start_clean"},
          {rel,"start_clean","1.0",[]},
          {rel,"start_sasl","1.0",[sasl]},
          {emu_name,"beam"},
          {relocatable,true},
          {profile,standalone},
          {incl_sys_filters,["^bin/(erl|epmd)(|\\.exe|\\.ini)$",
                             "^bin/start(|_clean).boot$","^erts.*/bin","^lib$"]},
          {excl_sys_filters,["^erts.*/bin/(erlc|dialyzer|typer)(|\\.exe)$",
                             "^erts.*/bin/(start|escript|to_erl|run_erl)(|\\.exe)$",
                             "^erts.*/bin/.*(debug|pdb)"]},
          {incl_app_filters,["^ebin","^priv"]},
          {excl_app_filters,["^ebin/.*\\.appup$"]},
          {rel_app_type,permanent},
          {app_file,keep},
          {debug_info,keep}]}}
```

## Generate release and script files

```erlang
Erlang/OTP 20 [erts-10.0] [source-c13b302] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10]
[hipe] [kernel-poll:false]
Eshell V10.0  (abort with ^G)
1>
1> {ok, Server} = reltool:start_server([{config,
                                         {sys,
                                          [{boot_rel, "NAME"},
                                           {rel, "NAME", "VSN",
                                            [sasl],
                                            [{load_dot_erlang, false}]}]}}]).
{ok,<0.1288.0>}
2>
2> reltool:get_config(Server).
{ok,{sys,[{boot_rel,"NAME"},
          {rel,"NAME","VSN",[sasl]}]}}
3>
3> reltool:get_rel(Server, "NAME").
{ok,{release,{"NAME","VSN"},
             {erts,"10.0"},
             [{kernel,"5.2"},{stdlib,"3.3"},{sasl,"3.0.3"}]}}
4>
4> reltool:get_script(Server, "NAME").
{ok,{script,{"NAME","VSN"},
            [{preLoaded,[erl_prim_loader,erl_tracer,erlang,
                         erts_code_purger,erts_dirty_process_signal_handler,
                         erts_internal,erts_literal_area_collector,init,erl_init,
                         prim_eval,prim_file,prim_inet,zlib]},
             {progress,preloaded},
             {path,["$ROOT/lib/kernel-5.2/ebin",
                    "$ROOT/lib/stdlib-3.3/ebin"]},
             {primLoad,[error_handler]},
             {kernel_load_completed},
             {progress,kernel_load_completed},
             {path,["$ROOT/lib/kernel-5.2/ebin"]},
             {primLoad,[application,application_controller,
                        application_master,application_starter,auth,code,
                        code_server,disk_log,disk_log_1,disk_log_server,
                        disk_log_sup,dist_ac,dist_util,erl_boot_server|...]},
             {path,["$ROOT/lib/stdlib-3.3/ebin"]},
             {primLoad,[array,base64,beam_lib,binary,c,calendar,dets,
                        dets_server,dets_sup,dets_utils,dets_v9,dict|...]},
             {path,["$ROOT/lib/sasl-3.0.3/ebin"]},
             {primLoad,[alarm_handler,erlsrv,format_lib_supp,misc_supp,
                        rb,rb_format_supp,release_handler,release_handler_1,sasl,
                        sasl_report|...]},
             {progress,modules_loaded},
             {path,["$ROOT/lib/kernel-5.2/ebin",
                    "$ROOT/lib/stdlib-3.3/ebin","$ROOT/lib/sasl-3.0.3/ebin"]},
             {kernelProcess,heart,{heart,start,[]}},
             {kernelProcess,error_logger,{error_logger,start_link,[]}},
             {kernelProcess,application_controller,
                            {application_controller,start,[{...}]}},
             {progress,init_kernel_started},
             {apply,{application,load,[...]}},
             {apply,{application,load,...}},
             {progress,applications_loaded},
             {apply,{...}},
             {apply,...},
             {...}|...]}}
5>
5> reltool:stop(Server).
ok
```

## Create a target system

```erlang
Erlang/OTP 20 [erts-10.0] [source-c13b302] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10]
[hipe] [kernel-poll:false]
Eshell V10.0  (abort with ^G)
1>
1> Config = {sys, [{escript, "examples/display_args", [{incl_cond, include}]},
		   {app, inets, [{incl_cond, include}]},
		   {app, mnesia, [{incl_cond, exclude}]},
		   {app, ssl, [{incl_cond, exclude}]},
		   {app, runtime_tools, [{incl_cond, exclude}]},
		   {app, syntax_tools, [{incl_cond, exclude}]}]}.
{sys,[{escript,"examples/display_args",[{incl_cond,include}]},
      {app,inets,[{incl_cond,include}]},
      {app,mnesia,[{incl_cond,exclude}]},
      {app,ssl,[{incl_cond,exclude}]},
      {app,runtime_tools,[{incl_cond,exclude}]},
      {app,syntax_tools,[{incl_cond,exclude}]}]}
2>
2> {ok, Spec} = reltool:get_target_spec([Config]).
{ok,[{create_dir,"releases",
         [{write_file,"start_erl.data","10.0 1.0\n"},
          {create_dir,"1.0",
              [{write_file,"start_clean.rel",
                   [37,37,32,114,101,108,32,103,101,110,101,114,97,116|...]},
               {write_file,"start_clean.script",
                   [37,37,32,115,99,114,105,112,116,32,103,101,110|...]},
               {write_file,"start_clean.boot",
                   <<131,104,3,119,6,115,99,114,105,112,116,104,...>>},
               {write_file,"start_sasl.rel",
                   [37,37,32,114,101,108,32,103,101,110,101|...]},
               {write_file,"start_sasl.script",
                   [37,37,32,115,99,114,105,112,116,32|...]},
               {write_file,"start_sasl.boot",
                   <<131,104,3,119,6,115,99,114,105,...>>}]}]},
     {create_dir,"bin",
         [{copy_file,"display_args.escript",
              "/usr/local/lib/erlang/lib/reltool-0.7.3/examples/display_args"},
          {copy_file,"display_args","erts-10.0/bin/escript"},
          {copy_file,"start","erts-10.0/bin/start"},
          {copy_file,"ct_run","erts-10.0/bin/ct_run"},
          {copy_file,"dialyzer","erts-10.0/bin/dialyzer"},
          {copy_file,"run_erl","erts-10.0/bin/run_erl"},
          {copy_file,"erl","erts-10.0/bin/dyn_erl"},
          {copy_file,"to_erl","erts-10.0/bin/to_erl"},
          {copy_file,"epmd","erts-10.0/bin/epmd"},
          {copy_file,"erlc","erts-10.0/bin/erlc"},
          {copy_file,"typer","erts-10.0/bin/typer"},
          {copy_file,"escript","erts-10.0/bin/escript"},
          {write_file,"start_clean.boot",<<131,104,3,119,6,115,...>>},
          {write_file,"start_sasl.boot",<<131,104,3,119,6,...>>},
          {write_file,"start.boot",<<131,104,3,119,...>>}]},
     {copy_file,"Install"},
     {create_dir,"misc",
         [{copy_file,"format_man_pages"}]},
     {create_dir,"usr",
         [{create_dir,"lib",
              [{copy_file,"liberl_interface_st.a"},
               {copy_file,"libic.a"},
               {copy_file,"liberl_interface.a"},
               {copy_file,"libei_st.a"},
               {copy_file,"libei.a"}]},
          {create_dir,"include",
              [{copy_file,"driver_int.h"},
               {copy_file,"ei_connect.h"},
               {copy_file,"ei.h"},
               {copy_file,"erl_nif_api_funcs.h"},
               {copy_file,"erl_fixed_size_int_types.h"},
               {copy_file,"erl_int_sizes_config.h"},
               {copy_file,"erl_interface.h"},
               {copy_file,"eicode.h"},
               {copy_file,"erl_driver.h"},
               {copy_file,"erlang.idl"},
               {copy_file,[...]},
               {copy_file,...},
               {...}]}]},
     {create_dir,"erts-10.0",
         [{create_dir,"bin",
              [{copy_file,"start"},
               {copy_file,"ct_run"},
               {copy_file,"erlexec"},
               {copy_file,"dialyzer"},
               {copy_file,"beam.smp"},
               {copy_file,"run_erl"},
               {copy_file,"erl","erts-10.0/bin/dyn_erl"},
               {copy_file,"to_erl"},
               {copy_file,"epmd"},
               {copy_file,"erl_child_setup"},
               {copy_file,"heart"},
               {copy_file,[...]},
               {copy_file,...},
               {...}|...]},
          {create_dir,"lib",
              [{create_dir,"internal",
                   [{copy_file,"liberts_internal.a"},
                    {copy_file,"liberts_internal_r.a"},
                    {copy_file,"libethread.a"},
                    {copy_file,"README"}]},
               ]},
          {create_dir,"src",[{copy_file,"setuid_socket_wrap.c"}]},
          {create_dir,"doc",[]},
          {create_dir,"man",[]},
          {create_dir,"include",
              [{create_dir,"internal",
                   [{create_dir,"i386",[{...}|...]},
                    {copy_file,"erl_errno.h"},
                    {copy_file,[...]},
                    {copy_file,...},
                    {...}|...]},
               {copy_file,"driver_int.h"},
               {copy_file,"erl_nif_api_funcs.h"},
               {copy_file,"erl_fixed_size_int_types.h"},
               {copy_file,"erl_int_sizes_config.h"},
               {copy_file,[...]},
               {copy_file,...},
               {...}]}]},
     {create_dir,"lib",
         [{create_dir,"compiler-7.0.4",
              [{create_dir,"src",
                   [{copy_file,"beam_flatten.erl"},
                    {copy_file,[...]},
                    {copy_file,...},
                    {...}|...]},
               {create_dir,"ebin",
                   [{copy_file,[...]},{copy_file,...},{...}|...]}]},
          {create_dir,"crypto-3.7.4",
              [{create_dir,"src",[{copy_file,[...]},{copy_file,...}]},
               {create_dir,"ebin",[{copy_file,...},{...}|...]}]},
          {create_dir,"crypto-3.7.4",
              [{create_dir,"priv",
                   [{create_dir,"lib",[{copy_file,[...]},{copy_file,...}]},
                    {create_dir,"obj",[{copy_file,...},{...}|...]}]}]},
          {create_dir,"erts-10.0",
              [{create_dir,"src",[{...}|...]},
               {create_dir,"ebin",[...]}]},
          {create_dir,"hipe-3.15.4",
              [{create_dir,"flow",[...]},
               {copy_file,[...]},
               {create_dir,...},
               {...}|...]},
          {create_dir,"inets-6.3.9",
              [{create_dir,[...],...},{create_dir,...},{...}]},
          {create_dir,"inets-6.3.9",
              [{create_dir,"priv",[{create_dir,[...],...}]},
               {create_dir,"include",[{copy_file,...},{...}]}]},
          {create_dir,"kernel-5.2",[{...}|...]},
          {create_dir,"kernel-5.2",
              [{create_dir,"include",[{...}|...]}]},
          {create_dir,[...],...},
          {create_dir,...},
          {create_dir,"stdlib-3.3",[{create_dir,...}]},
          ...]}]}
3>
3> TargetDir = "/tmp/my_target_dir".
"/tmp/my_target_dir"
4>
4> reltool:eval_target_spec(Spec, code:root_dir(), TargetDir).
{error,"/tmp/my_target_dir: no such file or directory"}
5>
5> file:make_dir(TargetDir).
ok
6>
6> reltool:eval_target_spec(Spec, code:root_dir(), TargetDir).
ok
7>
7> file:list_dir(TargetDir).
{ok,["bin","Install","lib","misc","usr","erts-10.0",
     "releases"]}
8>
8> file:list_dir(filename:join([TargetDir,"lib"])).
{ok,["tools-2.9.1","inets-6.3.9",
     "kernel-5.2","sasl-3.0.3",
     "crypto-3.7.4","erts-10.0",
     "stdlib-3.3","compiler-7.0.4"]}
9>
9> file:make_dir("/tmp/yet_another_target_dir").
ok
10>
10> reltool:create_target([Config], "/tmp/yet_another_target_dir").
ok
11>
11> file:list_dir("/tmp/yet_another_target_dir").
{ok,["bin","Install","lib","misc","usr","erts-10.0",
     "releases"]}
```
