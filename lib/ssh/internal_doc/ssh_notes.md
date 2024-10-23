# SSH supervision tree (server side update >= OTP-28)
```mermaid
---
title: SSH supervision tree
---
flowchart RL
    d_sup --> sup[["ssh_sup<br />(ssh_app.erl)<br />[o4o]"]]
    c_sup --> sup

    subgraph client
    connection_sup --> c_sup[["sshc_sup<br />(ssh_app.erl)<br />[o4o]<br />auto_shutdown=never"]]
    subgraph connection_c
    connection_handler["ssh_connection_handler<br />SIGNIFICANT"] --> connection_sup[["ssh_connection_sup<br />[o4a]<br />auto_shutdown=any_significant"]]
    channel_sup[["ssh_channel_sup<br />[o4o]"]] --> connection_sup
    sftp["ssh_sftp"] --> channel_sup
    tcpip_forward_acceptor_sup[["ssh_tcpip_forward_acceptor_sup<br />[o4o]"]] --> connection_sup
    ssh_tcpip_forward_acceptor["ssh_tcpip_forward_acceptor"] --> tcpip_forward_acceptor_sup
    end
    end

    subgraph server
    lsocket_sup[["ssh_lsocket_sup<br />[simple_one_for_one]"]] --> d_sup
    ssh_lsocket_provider --> lsocket_sup
    system_sup_s --> d_sup[["sshd_sup<br />(ssh_app.erl)<br />[o4o]"]]
    acceptor_sup --> system_sup_s[["ssh_system_sup<br />[o4o]<br />auto_shutdown=all_significant"]]
    acceptor["ssh_acceptor"] --> acceptor_sup[["ssh_acceptor_sup<br />[o4o?]<br />SIGNIFICANT"]]

    connection_sup_s --> system_sup_s

    subgraph connection_s
    connection_handler_s["ssh_connection_handler<br />SIGNIFICANT"] --> connection_sup_s[["ssh_connection_sup<br />[o4a]<br />auto_shutdown=any_significant<br />SIGNIFICANT"]]
    channel_sup_s[["ssh_channel_sup<br />[o4o]"]] --> connection_sup_s
    tcpip_forward_acceptor_sup_s[["ssh_tcpip_forward_acceptor_sup<br />[o4o]"]] --> connection_sup_s
    ssh_tcpip_forward_acceptor_s["ssh_tcpip_forward_acceptor"] --> tcpip_forward_acceptor_sup_s
    sftd1["ssh_sftpd"] --> channel_sup_s
    end
    end
```

# SSH supervision tree (client side update since ssh-5.2.3, ssh-5.1.4.3, ssh-4.15.3.7)
```mermaid
---
title: SSH supervision tree
---
flowchart RL
    d_sup --> sup[["ssh_sup<br />(ssh_app.erl)<br />[o4o]"]]
    c_sup --> sup

    subgraph client
    connection_sup --> c_sup[["sshc_sup<br />(ssh_app.erl)<br />[o4o]<br />auto_shutdown=never"]]
    subgraph connection_c
    connection_handler["ssh_connection_handler<br />SIGNIFICANT"] --> connection_sup[["ssh_connection_sup<br />[o4a]<br />auto_shutdown=any_significant"]]
    channel_sup[["ssh_channel_sup<br />[o4o]"]] --> connection_sup
    sftp["ssh_sftp"] --> channel_sup
    tcpip_forward_acceptor_sup[["ssh_tcpip_forward_acceptor_sup<br />[o4o]"]] --> connection_sup
    ssh_tcpip_forward_acceptor["ssh_tcpip_forward_acceptor"] --> tcpip_forward_acceptor_sup  
    end
    end

    subgraph server
    system_sup_s --> d_sup[["sshd_sup<br />(ssh_app.erl)<br />[o4o]"]]
    acceptor_sup --> system_sup_s[["ssh_system_sup<br />[o4o]<br />auto_shutdown=all_significant"]]
    acceptor["ssh_acceptor"] --> acceptor_sup[["ssh_acceptor_sup<br />[o4o]<br />SIGNIFICANT"]]

    connection_sup_s --> system_sup_s

    subgraph connection_s
    connection_handler_s["ssh_connection_handler<br />SIGNIFICANT"] --> connection_sup_s[["ssh_connection_sup<br />[o4a]<br />auto_shutdown=any_significant<br />SIGNIFICANT"]]
    channel_sup_s[["ssh_channel_sup<br />[o4o]"]] --> connection_sup_s
    tcpip_forward_acceptor_sup_s[["ssh_tcpip_forward_acceptor_sup<br />[o4o]"]] --> connection_sup_s
    ssh_tcpip_forward_acceptor_s["ssh_tcpip_forward_acceptor"] --> tcpip_forward_acceptor_sup_s
    sftd1["ssh_sftpd"] --> channel_sup_s
    end
    end
```

# SSH supervision tree (OTP >= 24)
```mermaid
---
title: SSH supervision tree (OTP >= 24)
---
flowchart RL
    d_sup --> sup[["ssh_sup<br />(ssh_app.erl)<br />[o4o]"]]
    c_sup --> sup

    subgraph client
    system_sup --> c_sup[["sshc_sup<br />(ssh_app.erl)<br />[o4o]<br />auto_shutdown=never"]]
    subgraph connection_c
    subsystem_sup --> system_sup[["ssh_system_sup<br />[o4o]<br />auto_shutdown=all_significant"]]
    connection_handler["ssh_connection_handler<br />SIGNIFICANT"] --> subsystem_sup[["ssh_subsystem_sup<br />[o4a]<br />auto_shutdown=any_significant<br />SIGNIFICANT"]]
    channel_sup[["ssh_channel_sup<br />[o4o]"]] --> subsystem_sup
    sftp["ssh_sftp"] --> channel_sup
    ssh_tcpip_forward_client --> channel_sup
    tcpip_forward_acceptor_sup[["ssh_tcpip_forward_acceptor_sup<br />[o4o]"]] --> subsystem_sup
    end
    end

    subgraph server
    system_sup_s --> d_sup[["sshd_sup<br />(ssh_app.erl)<br />[o4o]"]]
    acceptor_sup --> system_sup_s[["ssh_system_sup<br />[o4o]<br />auto_shutdown=all_significant"]]
    acceptor["ssh_acceptor"] --> acceptor_sup[["ssh_acceptor_sup<br />[o4o]<br />SIGNIFICANT"]]
    acceptor_worker["acceptor<br />(parallel_login)"] o-. link .-o acceptor
    subsystem_sup_s --> system_sup_s

    subgraph connection_s
    connection_handler_s["ssh_connection_handler<br />SIGNIFICANT"] --> subsystem_sup_s[["ssh_subsystem_sup<br />[o4a]<br />auto_shutdown=any_significant<br />SIGNIFICANT"]]
    channel_sup_s[["ssh_channel_sup<br />[o4o]"]] --> subsystem_sup_s
    tcpip_forward_acceptor_sup_s[["ssh_tcpip_forward_acceptor_sup<br />[o4o]"]] --> subsystem_sup_s
    ssh_tcpip_forward_acceptor_s["ssh_tcpip_forward_acceptor"] --> tcpip_forward_acceptor_sup_s
    sftd1["ssh_sftpd"] --> channel_sup_s
    ssh_tcpip_forward_srv --> channel_sup_s
    custom_cli --> channel_sup_s
    custom_subsystem --> channel_sup_s
    end
    end
```
# SSH supervision tree (OTP-23) ??
# SSH supervision tree (OTP-22)
```mermaid
---
title: SSH supervision tree (OTP-22)
---
flowchart RL
    d_sup --> sup[["ssh_sup<br />(ssh_app.erl)<br />[o4o]"]]
    c_sup --> sup

    subgraph client
    connection_handler["ssh_connection_handler<br />SIGNIFICANT?"] --> c_sup
    end

    subgraph server
    system_sup_s --> d_sup[["sshd_sup<br />(ssh_app.erl)<br />[o4o]"]]
    acceptor_sup --> system_sup_s[["ssh_system_sup<br />[o4o]<br />auto_shutdown=all_significant"]]
    acceptor["ssh_acceptor"] --> acceptor_sup[["ssh_acceptor_sup<br />[o4o]<br />SIGNIFICANT"]]

    subsystem_sup_s --> system_sup_s

    subgraph connection_s
    connection_handler_s["ssh_connection_handler<br />SIGNIFICANT"] --> subsystem_sup_s[["ssh_subsystem_sup<br />[o4a]<br />auto_shutdown=any_significant<br />SIGNIFICANT"]]
    channel_sup_s[["ssh_channel_sup<br />[o4o]"]] --> subsystem_sup_s
    tcpip_forward_acceptor_sup_s[["ssh_tcpip_forward_acceptor_sup<br />[o4o]"]] --> subsystem_sup_s
    sftd1["ssh_sftpd"] --> channel_sup_s
    end
end
```
