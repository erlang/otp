{application,dummy,
             [{description,"a dummy app"},
              {vsn,"0.1"},
              {registered,[dummy_app]},
              {mod,{dummy_app,[]}},
              {applications,[kernel,stdlib,sasl]},
              {modules,[dummy_app,dummy_server,dummy_sup,dummy_sup_2]}]}.