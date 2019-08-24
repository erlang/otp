{application, uds_dist,
   [{description, "SSL socket version 2"},
    {vsn, "1.0"},
    {modules, [uds_server]},
    {registered, [uds_server]},
    {applications, [kernel, stdlib]},
    {env, []}]}.
