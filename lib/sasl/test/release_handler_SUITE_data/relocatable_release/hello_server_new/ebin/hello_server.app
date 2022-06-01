{application, hello_server,
              [{description, "Simple server that sends back hej"},
               {vsn, "B"},
               {modules, [app_callback_module, hello_server]},
               {registered, [hello_server]},
               {applications, [kernel, stdlib, sasl]},
               {mod, {app_callback_module,[]}}]}.
