{config, "../test_server/ts.config"}.
{config, "../test_server/ts.unix.config"}.

{cases,"../kernel_test", inet_SUITE,[t_gethostbyaddr,t_gethostbyname,
	t_gethostbyaddr_v6,t_gethostbyname_v6,t_gethostnative,getifaddrs]}.
{cases,"../kernel_test", inet_res_SUITE,[gethostbyaddr,gethostbyname,
	gethostbyaddr_v6,gethostbyname_v6,basic]}.
{cases,"../kernel_test", gen_tcp_echo_SUITE,[active_echo]}.
{cases,"../kernel_test", heart_SUITE,[reboot]}.
